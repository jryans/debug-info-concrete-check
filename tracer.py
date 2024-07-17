# Extracted from https://github.com/cristianassaiante/incomplete-debuginfo

import os
import random
import tempfile
import platform
import os.path

from run import *
from log import *

# Arguments: binary
DWARF = "/Users/jryans/Projects/LLVM/llvm/builds/release-clang-lldb/bin/llvm-dwarfdump --debug-line {0}"

# Arguments: script_path, binary, program_args
LLDB = "lldb -s {0} -- {1} {2}"

# Arguments: dwarf_path, tracer_dir, function_scripts
LLDB_SCRIPT_TEMPLATE = """
target symbols add {0}

command script import {1}/lldb.py

{2}

process launch -o ./concrete-trace/stdout -e ./concrete-trace/stderr
quit
"""

# Arguments: function
LLDB_FUNCTION_TEMPLATE = """
break set -n {0}
break command add -F lldb.on_breakpoint
"""


def get_dwarf_path(binary):
    dwarf_path = binary
    if platform.system() == "Darwin":
        # TODO: Ask Spotlight to find debug info by UUID...?
        dwarf_path = f"{binary}.dwarf"
    return dwarf_path


def get_lines(binary):
    lines = set()
    dwarf_path = get_dwarf_path(binary)
    output = run_cmd(DWARF.format(dwarf_path), get_output=True)
    for line in output.split("\n"):
        if len(line.strip().split()) < 2:
            continue
        if "is_stmt" not in line.strip().split()[-1].strip():
            continue
        line = line.strip().split()[1].strip()
        if line.isnumeric():
            lines.add(int(line))
    if 0 in lines:
        lines.remove(0)
    return list(lines)


def run_dbg(binary, program_args, dbg_script):
    output = ""
    cmd_template = LLDB

    with tempfile.TemporaryDirectory() as tmp_dir:
        script_file = str(random.randint(0, 2**32)) + ".dbg"
        script_path = os.path.join(tmp_dir, script_file)
        with open(script_path, "w") as f:
            f.write(dbg_script)

        # print(f"## Script\n\n{dbg_script}")

        # print(f"Binary: {binary}")
        # print(f"Script path: {script_path}")

        cmd = cmd_template.format(script_path, binary, " ".join(program_args))

        # print(f"Command: {cmd}")

        output = run_cmd(cmd, get_output=True)

    return output


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

    script_template = LLDB_SCRIPT_TEMPLATE

    dwarf_path = get_dwarf_path(binary)
    tracer_dir = os.path.dirname(__file__)

    function_scripts = [LLDB_FUNCTION_TEMPLATE.format(f) for f in functions]

    dbg_script = script_template.format(
        dwarf_path, tracer_dir, "".join(function_scripts)
    )

    return run_dbg(binary, program_args, dbg_script)
