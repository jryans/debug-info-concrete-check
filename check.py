#!/usr/bin/python3
# Requires Python installation with access to LLDB modules

import os
import argparse

import tracer


def main(args):
    os.makedirs("concrete-trace", exist_ok=True)

    before_trace = tracer.trace(args.before_binary, args.args, args.include_function)

    print(before_trace)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(prog="check-concrete-debug-info")

    parser.add_argument("before_binary", help="Program binary before optimisation")
    parser.add_argument("after_binary", help="Program binary after optimisation")

    parser.add_argument("args", nargs="*", help="Program arguments")

    parser.add_argument(
        "--include-function",
        action="append",
        help="Include only specific named function "
        "(exact match, can be specified multiple times)",
    )

    main(parser.parse_args())
