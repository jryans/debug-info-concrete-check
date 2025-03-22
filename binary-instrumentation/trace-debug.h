#ifndef TRACE_DEBUG_H
#define TRACE_DEBUG_H

#include "llvm/Support/Format.h"
#include "llvm/Support/raw_ostream.h"

#include "QBDI.h"

void traceRegisters(llvm::raw_fd_ostream &trace, QBDI::GPRState *gprState) {
  trace << "rsp:         " << llvm::format_hex(gprState->rsp, 18) << "\n";
  trace << "*(rsp):      "
        << llvm::format_hex(*((QBDI::rword *)gprState->rsp + 0), 18) << "\n";
  trace << "*(rsp + 8):  "
        << llvm::format_hex(*((QBDI::rword *)gprState->rsp + 8), 18) << "\n";
  trace << "*(rsp + 16): "
        << llvm::format_hex(*((QBDI::rword *)gprState->rsp + 16), 18) << "\n";
}

void traceMemoryMaps(llvm::raw_fd_ostream &trace,
                     const std::vector<QBDI::MemoryMap> &memoryMaps) {
  for (const auto &mm : memoryMaps) {
    const auto &name = mm.name.empty() ? "<unnamed>" : mm.name;
    trace << name << ": ";
    (mm.permission & QBDI::PF_READ) ? trace << "r" : trace << "_";
    (mm.permission & QBDI::PF_WRITE) ? trace << "w" : trace << "_";
    (mm.permission & QBDI::PF_EXEC) ? trace << "x" : trace << "_";
    trace << " [" << llvm::format_hex(mm.range.start(), 18) << ", "
          << llvm::format_hex(mm.range.end(), 18) << ")\n";
  }
}

#endif // TRACE_DEBUG_H
