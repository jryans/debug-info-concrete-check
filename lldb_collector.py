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

# Global print to trace function
trace_print = None


class Tracer:
    def __init__(self, debugger):
        self.debugger = debugger

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

    def print_current_frame_details(self):
        self.run_command_and_trace_output("frame info")
        self.run_command_and_trace_output("frame variable -D 0")

    def on_main_function_entry(self, frame):
        self.print_current_frame_details()
        thread = frame.thread
        thread.process.Continue()

    def on_included_function_entry(self, frame):
        thread = frame.thread
        num_frames_when_hit = thread.num_frames
        print(f"Frames when hit: {num_frames_when_hit}")

        non_inlined_frame = Tracer.get_first_non_inlined_frame(frame)
        print(f"First non-inlined frame index: {non_inlined_frame.idx}")

        num_frames_when_hit -= non_inlined_frame.idx
        print(f"Frame when hit after inlining adjustment: {num_frames_when_hit}")

        self.print_current_frame_details()
        while thread.num_frames >= num_frames_when_hit:
            self.debugger.SetAsync(False)
            thread.StepOver()
            self.debugger.SetAsync(True)
            self.print_current_frame_details()

        print("Outside analysis window, continuing...")
        thread.process.Continue()


# Launches LLDB for tracing from an external Python environment
def trace(binary, dwarf_path, program_args, functions, get_out_path, print_func):
    global trace_print
    trace_print = print_func

    # Create debugger and linked tracer
    debugger = lldb.SBDebugger.Create()
    tracer = Tracer(debugger)

    # Disable color output to avoid terminal escape codes in trace files
    debugger.SetUseColor(False)

    # Create target and add symbols
    target = debugger.CreateTarget(binary)
    tracer.add_symbols(dwarf_path)

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
                    tracer.on_main_function_entry(thread.frame[0])
                else:
                    tracer.on_included_function_entry(thread.frame[0])

            if process.is_stopped:
                process.Continue()

    event_listener_thread = threading.Thread(target=event_listener)
    event_listener_thread.start()

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
    event_listener_thread.join()

    # Tracing complete
    process.Destroy()
