#!/usr/bin/python3
# Requires Python installation with access to LLDB modules

import argparse
import os
import platform

import lldb_collector


def get_dwarf_path(binary):
    dwarf_path = binary
    if platform.system() == "Darwin":
        # TODO: Ask Spotlight to find debug info by UUID...?
        dwarf_path = f"{binary}.dwarf"
    return dwarf_path


# Extracted from https://github.com/cristianassaiante/incomplete-debuginfo
def get_variables_from_trace(trace):
    output = {}

    current_line = None
    for line in trace.split("\n"):
        line = line.strip()

        if (
            "lldb" in line
            or "Current" in line
            or "Breakpoint" in line
            or "Process" in line
            or "Command" in line
            or len(line.split()) == 0
        ):
            continue
        if line.startswith("[") or line.startswith("*"):
            continue

        # Remove metadata shown by some LLDB versions that confuses the
        # following steps
        line = line.replace(" [opt]", "")

        line_no = line.split()[-1]
        if ":" in line_no:
            line_no = line_no.split(":")[-2]
            current_line = line_no
            output[current_line] = {
                "available": [],
                "optimized_out": [],
                "not_available": [],
            }
            continue

        if current_line and " = " in line:
            var_name = line.split(" = ")[0]
            if var_name.startswith("("):
                var_name = var_name.split(")")[1].rstrip().lstrip()
            value = line.split(" = ")[-1].rstrip().lstrip()
            if "optimized out" in value:
                output[current_line]["optimized_out"].append(var_name)
            elif "not available" in value:
                output[current_line]["not_available"].append(var_name)
            else:
                output[current_line]["available"].append(var_name)

    for line in output:
        output[line]["optimized_out"] = list(set(output[line]["optimized_out"]))
        output[line]["available"] = list(set(output[line]["available"]))

        for var in output[line]["available"]:
            if var in output[line]["optimized_out"]:
                output[line]["optimized_out"].remove(var)
            if var in output[line]["not_available"]:
                output[line]["not_available"].remove(var)

    return output


def trace(binary, program_args, functions):
    # TODO: Support all executed functions
    assert len(functions) >= 1

    out_dir = "concrete-trace"
    os.makedirs(out_dir, exist_ok=True)

    def get_out_path(file):
        return os.path.join(out_dir, file)

    dwarf_path = get_dwarf_path(binary)

    with open(get_out_path("trace"), "w") as trace_file:

        def trace_print(message):
            print(message, file=trace_file)

        lldb_collector.trace(
            binary, dwarf_path, program_args, functions, get_out_path, trace_print
        )


def main(args):
    trace(args.binary, args.args, args.include_function)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(prog="collect-debug-info-trace")

    parser.add_argument("binary", help="Program binary")
    parser.add_argument("args", nargs="*", help="Program arguments")

    parser.add_argument(
        "--include-function",
        action="append",
        help="Include only specific named function "
        "(exact match, can be specified multiple times)",
    )

    main(parser.parse_args())
