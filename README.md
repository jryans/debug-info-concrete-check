## Summary

This repo includes various debug info concrete consistency checking tools
developed as part of our [OOPSLA 2026 paper][paper]:

Debugging Debugging Information Using Dynamic Call Trees

by J. Ryan Stinnett and Stephen Kell.

## Tools

### `binary-instrumentation`

The `binary-instrumentation` project builds
an `LD_PRELOAD`-able library
making use of the [QBDI binary instrumentation framework][QBDI].
During execution of the program under analysis,
an execution trace that tracks function calls and returns
is recorded.
Debug info (e.g. DWARF) is used by the library
to map program addresses to function names and source coordinates.

### `compare-traces`

The `compare-traces` project builds
a CLI tool that reads in two call tree traces
(in the format produced by the `binary-instrumentation` project)
and compares them using a tree diffing approach.
Various categories of trace divergences can be detected
using tree pattern matching.

The intended use of this tool (as seen in our paper)
is comparing the apparent execution of a program
across different compilers / compiler options
(versions, families, optimisation levels).
In today's compilers,
optimisation often
removes or corrupts debug info,
leading to various call tree divergences when comparing traces.

## Usage

The artifact for our paper contains various experiments and surrounding scripts
that make use of these projects. You may wish to start there when trying out
these tools.

[paper]:
  TODO
[artifact]:
  https://github.com/jryans/debug-info-concrete-check
[QBDI]:
  https://qbdi.quarkslab.com/
