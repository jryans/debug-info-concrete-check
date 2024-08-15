#include <cstddef>
#include <cstdlib>
#include <memory>
#include <system_error>
#include <utility>

#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Twine.h"
#include "llvm/DebugInfo/DIContext.h"
#include "llvm/DebugInfo/DWARF/DWARFContext.h"
#include "llvm/DebugInfo/DWARF/DWARFDie.h"
#include "llvm/Object/Binary.h"
#include "llvm/Object/ObjectFile.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/ErrorOr.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

#include "QBDIPreload.h"

using namespace llvm;

namespace {

void *mainFunc;

bool verbose = false;

std::unique_ptr<raw_fd_ostream> trace;

std::unique_ptr<MemoryBuffer> dwarfBuffer;
std::unique_ptr<object::Binary> dwarfBinary;
std::unique_ptr<DWARFContext> dwarfCtx;

size_t stackDepth = 0;
bool stackDepthChanged = true;
QBDI::rword lastCallReturnTarget = 0;

void pushStackFrame() {
  ++stackDepth;
  stackDepthChanged = true;
}

void popStackFrame() {
  if (stackDepth) {
    --stackDepth;
    // We skip the post-return event, as this is not expected to map
    // to the same source location across versions.
    // stackDepthChanged = true;
  } else {
    *trace << "🔔 Ignoring return, stack depth would have wrapped around\n";
  }
}

void printStackDepth() {
  // *trace << format("%4lu", stackDepth) << ": ";
  // Print indentation to represent current stack depth
  for (size_t i = 0; i < stackDepth; ++i)
    *trace << "  ";
}

QBDI::VMAction onInstruction(QBDI::VMInstanceRef vm, QBDI::GPRState *gprState,
                             QBDI::FPRState *fprState, void *data) {
  // TODO: Defer analysis when stack depth unchanged
  QBDI::AnalysisType analysisType = QBDI::ANALYSIS_INSTRUCTION;
  if (verbose)
    analysisType |= QBDI::ANALYSIS_DISASSEMBLY;
  const QBDI::InstAnalysis *instAnalysis = vm->getInstAnalysis(analysisType);
  const auto &address = instAnalysis->address;

  // Print address and disassembly in verbose mode for trace debugging
  if (verbose) {
    // 16 hex digits for 64-bit address plus 2 character prefix
    *trace << format_hex(address, 18) << "\n";
    *trace << instAnalysis->disassembly << "\n";
  }

  // Look for function name and line info related to this address
  const auto lineInfo = dwarfCtx->getLineInfoForAddress(
      {address}, {DILineInfoSpecifier::FileLineInfoKind::RawValue,
                  DILineInfoSpecifier::FunctionNameKind::ShortName});

  // If last instrumented instruction was a call, check whether this next
  // instrumented instruction was the call's return target. If yes, it means we
  // just returned from uninstrumented code and need to adjust stack depth.
  // TODO: May need to track the full stack of addresses in case uninstrumented
  // code can call back into instrumented code.
  if (lastCallReturnTarget) {
    if (lastCallReturnTarget == address) {
      lastCallReturnTarget = 0;
      popStackFrame();
    }
  }

  const bool stackDepthWillChange =
      instAnalysis->isCall || instAnalysis->isReturn;

  // By default, only log to trace before and after stack depth changes.
  // This covers both sides of calls and returns... or it used to!
  // For returns, we skip the post-return event, as this is not expected to map
  // to the same source location across versions.
  if (stackDepthWillChange || stackDepthChanged || verbose) {
    printStackDepth();

    // Print function name and line info
    if (lineInfo) {
      const auto functionOffset = address - *lineInfo.StartAddress;
      *trace << lineInfo.FunctionName;
      if (verbose)
        *trace << " + " << format_hex(functionOffset, 6);
      *trace << " at " << lineInfo.FileName << ":" << lineInfo.Line << ":"
             << lineInfo.Column << "\n";
    } else {
      if (instAnalysis->isBranch)
        *trace << "Jump to external code\n";
      else
        *trace << "🔔 No info for this address\n";
    }
  }

  // Reset did change tracker
  stackDepthChanged = false;

  // Update stack depth for next instruction after calls and returns
  if (instAnalysis->isCall) {
    pushStackFrame();
    // Track address to return to so we can compare to stack value on next
    // instrumented instruction to check whether we followed the call. Compute
    // this manually since this callback sees the pre-instruction register
    // state.
    lastCallReturnTarget = address + instAnalysis->instSize;
  } else if (instAnalysis->isReturn) {
    // Clear return target when we see an instrumented return
    lastCallReturnTarget = 0;
    // If we're returning from `main`, no need to update stack frames
    if (!lineInfo || lineInfo.FunctionName != "main")
      popStackFrame();
  }

  // Add extra line break in verbose mode for readability
  if (verbose)
    *trace << "\n";

  return QBDI::CONTINUE;
}

bool loadDWARFDebugInfo(const Twine &dwarfPath) {
  bool result = true;

  // Create memory buffer for DWARF file
  ErrorOr<std::unique_ptr<MemoryBuffer>> dwarfBufferMaybe =
      MemoryBuffer::getFileOrSTDIN(dwarfPath);
  if (dwarfBufferMaybe.getError()) {
    errs() << "Error: Unable to open DWARF debug info file\n";
    return false;
  }
  dwarfBuffer = std::move(dwarfBufferMaybe.get());

  // Parse object file containing DWARF debug info
  Expected<std::unique_ptr<object::Binary>> dwarfBinaryMaybe =
      object::createBinary(*dwarfBuffer);
  if (dwarfBinaryMaybe.takeError()) {
    errs() << "Error: Unable to parse object file "
              "containing DWARF debug info\n";
    return false;
  }
  dwarfBinary = std::move(dwarfBinaryMaybe.get());
  if (!isa<object::ObjectFile>(dwarfBinary)) {
    errs() << "Error: Unexpected object file type "
              "containing DWARF debug info\n";
    return false;
  }
  const auto &dwarfObjectFile = cast<object::ObjectFile>(*dwarfBinary);

  // Load debug info into DWARF context
  auto errorHandler = [&](Error error) {
    result = false;
    handleAllErrors(std::move(error), [](ErrorInfoBase &errorInfo) {
      errs() << "Error: " << errorInfo.message() << "\n";
    });
  };
  dwarfCtx = DWARFContext::create(
      dwarfObjectFile, DWARFContext::ProcessDebugRelocations::Process, nullptr,
      "", errorHandler);

  return result;
}

} // namespace

extern "C" {

QBDIPRELOAD_INIT;

int qbdipreload_on_start(void *main) {
  mainFunc = main;
  // Leave this as "not handled" so the default handler will still run,
  // which hooks the `main` function.
  return QBDIPRELOAD_NOT_HANDLED;
}

int qbdipreload_on_premain(void *gprCtx, void *fpuCtx) {
  return QBDIPRELOAD_NOT_HANDLED;
}

int qbdipreload_on_main(int argc, char **argv) {
  if (std::getenv("CON_TRACE_VERBOSE"))
    verbose = true;

  StringRef execPath(argv[0]);
  if (!loadDWARFDebugInfo(execPath + ".dwarf"))
    return QBDIPRELOAD_ERR_STARTUP_FAILED;

  // Leave this as "not handled" so the default handler will still run,
  // which starts up the QBDI instrumentation VM.
  return QBDIPRELOAD_NOT_HANDLED;
}

int qbdipreload_on_run(QBDI::VMInstanceRef vm, QBDI::rword start,
                       QBDI::rword stop) {
  std::error_code error;
  auto traceName = verbose ? "trace-verbose" : "trace";
  trace = std::make_unique<raw_fd_ostream>(traceName, error);
  if (error) {
    errs() << "Error: Unable to open trace file\n";
    return QBDIPRELOAD_ERR_STARTUP_FAILED;
  }

  // Reset instrumented range to include only the main program module.
  // This ensures we avoid the dynamic loader and libraries.
  // The dynamic loader seems to confuse stack depth detection.
  vm->removeAllInstrumentedRanges();
  vm->addInstrumentedModuleFromAddr((QBDI::rword)mainFunc);

  vm->addCodeCB(QBDI::PREINST, onInstruction, nullptr);
  vm->run(start, stop);

  return QBDIPRELOAD_NO_ERROR;
}

int qbdipreload_on_exit(int status) { return QBDIPRELOAD_NO_ERROR; }
}
