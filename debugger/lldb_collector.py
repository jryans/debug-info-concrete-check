# See also https://lldb.llvm.org/use/python-reference.html

import os
import platform
import subprocess
import sys
import threading

try:
    # Try to import if we're inside LLDB or path is already set
    import lldb
except ImportError:
    command = (
        ["xcrun", "lldb", "-P"] if platform.system() == "Darwin" else ["lldb", "-P"]
    )
    lldb_python_path = subprocess.check_output(command).decode("utf-8").strip()
    if os.path.exists(lldb_python_path) and not sys.path.__contains__(lldb_python_path):
        sys.path.append(lldb_python_path)
    try:
        import lldb
    except ImportError:
        print("Error: Couldn't locate the `lldb` module")
        sys.exit(1)

trace_file = sys.stdout


def trace_write(message):
    trace_file.write(message)


def trace_print(message):
    print(message, file=trace_file)


class StoredFrame:
    def __init__(self, frame):
        # Store key frame properties for comparison
        # The LLDB frame object is a live view, not fixed data
        self.module = frame.module
        self.pc = frame.pc

        # Other properties
        self.is_inlined = frame.is_inlined
        self.addr = frame.addr

    def __eq__(self, other):
        return (self.module, self.pc) == (other.module, other.pc)

    def __hash__(self):
        return hash((self.module, self.pc))

    def __repr__(self):
        return repr(self.__dict__)


class TraceCollector:
    def __init__(self, debugger):
        self.debugger = debugger
        self.step_limit = None

        # This list of frames is ordered with the oldest frame first
        # Order is opposite of LLDB's frame order
        self.frames_seen = []

    @staticmethod
    def get_first_non_inlined_frame(frame):
        while frame.is_inlined and frame.parent:
            frame = frame.parent
        return frame

    def run_command(self, command):
        interpreter = self.debugger.GetCommandInterpreter()
        result = lldb.SBCommandReturnObject()
        interpreter.HandleCommand(command, result)
        return result

    def run_command_and_print_output(self, command):
        result = self.run_command(command)
        if result.Succeeded():
            print(result.GetOutput())
        else:
            print(f"`{command}` failed: {result.GetError()}")

    def run_command_and_trace_output(self, command):
        result = self.run_command(command)
        if result.Succeeded():
            trace_print(result.GetOutput())
        else:
            trace_print(f"`{command}` failed: {result.GetError()}")

    def add_symbols(self, dwarf_path):
        self.run_command_and_print_output(f"target symbols add {dwarf_path}")

    def trace_current_frame_info(self):
        self.run_command_and_trace_output("frame info")

    def trace_current_frame_variables(self):
        self.run_command_and_trace_output("frame variable -D 0 -d no-dynamic-values")

    def trace_current_frame_details(self):
        self.trace_current_frame_info()
        self.trace_current_frame_variables()

    def trace_frame(self, frame: StoredFrame, indent_steps=0):
        for _ in range(indent_steps):
            trace_write("  ")
        # Roughly matching LLDB's default frame format but without args
        # 0x000000010011dc57 git`main(argc=4, argv=0x00007ff7bfefea78) at common-main.c:31:2
        # `frame.addr` covers the bulk of this, but includes extra lines we
        # don't need when inlining is involved.
        frame_addr_first_line = repr(frame.addr).splitlines()[0]
        trace_write(f"0x{frame.pc:016x} {frame_addr_first_line}")
        trace_write("\n")

    def update_frames_and_trace_tree_changes(self, thread):
        # Track different types of modifications
        added = False
        changed = False
        removed = False

        # Walk through all thread frames, starting with the oldest frame first
        # LLDB sorts frames with the newest frame first
        # We store them in `frames_seen` with the oldest frame first
        thread_num_frames = thread.num_frames
        seen_num_frames = len(self.frames_seen)
        for thread_fid in reversed(range(thread_num_frames)):
            seen_fid = thread_num_frames - 1 - thread_fid
            thread_frame = StoredFrame(thread.frame[thread_fid])
            # trace_print(f"Thread FID: {thread_fid}, Seen FID: {seen_fid}")
            # trace_print(thread.frame[thread_fid])

            if seen_fid < seen_num_frames:
                # Existing frame index, need to compare past frame
                seen_frame = self.frames_seen[seen_fid]
                # trace_print(f"Seen: {seen_frame}")
                # trace_print(f"Thre: {thread_frame}")
                if seen_frame != thread_frame:
                    self.trace_frame(thread_frame, seen_fid)
                    self.frames_seen[seen_fid] = thread_frame
                    changed = True
            else:
                # New frame index with no past frame to compare to
                self.trace_frame(thread_frame, seen_fid)
                self.frames_seen.append(thread_frame)
                added = True

        # Remove any extra frames from seen list
        if seen_num_frames > thread_num_frames:
            del self.frames_seen[thread_num_frames:]
            removed = True

        return {"added": added, "changed": changed, "removed": removed}

    def on_main_function_entry(self, frame):
        thread = frame.thread
        print("Main function entered")

        # TODO: Remove this default
        if self.step_limit is None:
            self.step_limit = 20
        print(f"Tracing {self.step_limit} steps")

        steps = 0
        tree_mods = self.update_frames_and_trace_tree_changes(thread)
        # self.trace_current_frame_info()
        if tree_mods["added"] or tree_mods["changed"]:
            self.trace_current_frame_variables()
        steps += 1
        while steps < self.step_limit:
            self.debugger.SetAsync(False)
            thread.StepInto()
            self.debugger.SetAsync(True)
            tree_mods = self.update_frames_and_trace_tree_changes(thread)
            # self.trace_current_frame_info()
            if tree_mods["added"] or tree_mods["changed"]:
                self.trace_current_frame_variables()
            steps += 1

        print(f"Reached end of tracing steps, continuing...")
        thread.process.Continue()

    def on_included_function_entry(self, frame):
        thread = frame.thread
        num_frames_when_hit = thread.num_frames
        print(f"Frames when hit: {num_frames_when_hit}")

        non_inlined_frame = TraceCollector.get_first_non_inlined_frame(frame)
        print(f"First non-inlined frame index: {non_inlined_frame.idx}")

        num_frames_when_hit -= non_inlined_frame.idx
        print(f"Frame when hit after inlining adjustment: {num_frames_when_hit}")

        self.trace_current_frame_details()
        while thread.num_frames >= num_frames_when_hit:
            self.debugger.SetAsync(False)
            thread.StepOver()
            self.debugger.SetAsync(True)
            self.trace_current_frame_details()

        print("Outside analysis window, continuing...")
        thread.process.Continue()


# Launches LLDB for tracing from an external Python environment
def trace(binary, dwarf_path, program_args, functions, steps, get_out_path, trace):
    global trace_file
    trace_file = trace

    # Create debugger and linked trace collector
    debugger = lldb.SBDebugger.Create()
    collector = TraceCollector(debugger)

    # Disable color output to avoid terminal escape codes in trace files
    debugger.SetUseColor(False)

    # Create target and add symbols
    target = debugger.CreateTarget(binary)
    collector.add_symbols(dwarf_path)

    # Check whether all functions are being analysed or only a specific list
    all_functions = functions is None

    # When analysing all functions, start from program's main
    if all_functions:
        functions = ["main"]

    # Break on entry to each analysed function
    for function in functions:
        print(f"Function: {function}")
        bp = target.BreakpointCreateByName(function)
        assert bp.enabled
        # Each function name might result in multiple locations
        assert bp.num_locations >= 1
        print(f"Locations: {bp.num_locations}")

    # Apply step limit if any
    collector.step_limit = steps

    # Listen for debugger events from separate thread
    process = None
    listener = debugger.GetListener()

    def event_listener():
        event = lldb.SBEvent()
        active = True
        while active:
            listener.WaitForEvent(lldb.UINT32_MAX, event)
            # print("Received event")
            # print(f"Type: {event.GetType()}")
            # stream = lldb.SBStream()
            # event.GetDescription(stream)
            # print(f"Desc: {stream.GetData()}")

            if not lldb.SBProcess.EventIsProcessEvent(event):
                continue

            state = lldb.SBProcess.GetStateFromEvent(event)
            # print(f"State: {state}")

            if (
                state == lldb.eStateInvalid
                or state == lldb.eStateUnloaded
                or state == lldb.eStateCrashed
                or state == lldb.eStateDetached
                or state == lldb.eStateExited
            ):
                active = False

            if state != lldb.eStateStopped:
                continue

            # print(process)
            for thread in process:
                # print(thread)
                stop_reason = thread.GetStopReason()
                if stop_reason != lldb.eStopReasonBreakpoint:
                    continue
                # print("Stop reason: breakpoint")
                if all_functions:
                    collector.on_main_function_entry(thread.frame[0])
                else:
                    collector.on_included_function_entry(thread.frame[0])

            if process.is_stopped:
                process.Continue()

    # event_listener_thread = threading.Thread(target=event_listener)
    # event_listener_thread.start()

    # Clear stdio files manually, as process launch below does not do so
    stdout_path = get_out_path("stdout")
    stderr_path = get_out_path("stderr")
    os.remove(stdout_path)
    os.remove(stderr_path)

    # Create target and process
    error = lldb.SBError()
    process = target.Launch(
        listener,  # listener
        program_args,
        None,  # envp
        None,  # stdin_path
        stdout_path,
        stderr_path,
        None,  # working_directory
        lldb.eLaunchFlagDisableASLR,  # launch_flags
        False,  # stop_at_entry
        error,
    )
    assert error.success

    # Wait for thread
    event_listener()
    # event_listener_thread.join()

    # Tracing complete
    process.Destroy()
