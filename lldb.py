# This file is _imported into_ LLDB's Python environment
# See also https://lldb.llvm.org/use/python-reference.html

import lldb


def get_first_non_inlined_frame(frame):
    while frame.is_inlined and frame.parent:
        frame = frame.parent
    return frame


def run_command_and_print_output(debugger, command):
    interpreter = debugger.GetCommandInterpreter()
    result = lldb.SBCommandReturnObject()
    interpreter.HandleCommand(command, result)
    if result.Succeeded():
        print(result.GetOutput())
    else:
        print(f"`{command}` failed: {result.GetError()}")


def print_frame_details(frame):
    debugger = frame.thread.process.target.debugger
    run_command_and_print_output(debugger, "frame info")
    run_command_and_print_output(debugger, "frame variable -D 0")


def on_breakpoint(frame, bp_loc, internal_dict):
    thread = frame.thread
    num_frames_when_hit = thread.num_frames
    print(f"Frames when hit: {num_frames_when_hit}")

    non_inlined_frame = get_first_non_inlined_frame(frame)
    print(f"First non-inlined frame index: {non_inlined_frame.idx}")

    num_frames_when_hit -= non_inlined_frame.idx
    print(f"Frame when hit after inlining adjustment: {num_frames_when_hit}")

    debugger = thread.process.target.debugger
    print_frame_details(frame)
    while thread.num_frames >= num_frames_when_hit:
        debugger.SetAsync(False)
        thread.StepOver()
        debugger.SetAsync(True)
        print_frame_details(thread.frame[0])

    print("Outside analysis window, continuing...")
    thread.process.Continue()
