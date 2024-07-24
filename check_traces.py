#!/usr/bin/python3
# Requires Python installation with access to LLDB modules

import argparse


if __name__ == "__main__":
    parser = argparse.ArgumentParser(prog="check-debug-info-traces")

    parser.add_argument("before_binary", help="Program binary before optimisation")
    parser.add_argument("after_binary", help="Program binary after optimisation")

    # TODO: Check things!
