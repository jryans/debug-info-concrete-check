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


def trace(binary, program_args, functions, steps):
    out_dir = "concrete-trace"
    os.makedirs(out_dir, exist_ok=True)

    def get_out_path(file):
        return os.path.join(out_dir, file)

    dwarf_path = get_dwarf_path(binary)

    with open(get_out_path("trace"), "w") as trace_file:
        lldb_collector.trace(
            binary,
            dwarf_path,
            program_args,
            functions,
            steps,
            get_out_path,
            trace_file,
        )


def main(args):
    trace(args.binary, args.args, args.include_function, args.steps)


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

    parser.add_argument(
        "--steps",
        type=int,
        help="Limit tracing to specific number of steps through the program",
    )

    main(parser.parse_args())
