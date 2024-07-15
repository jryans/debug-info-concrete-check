#!/usr/bin/env python3

import argparse

import tracer


def main(args):
    before_lines = tracer.get_lines(args.before_binary)

    print(before_lines)


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
