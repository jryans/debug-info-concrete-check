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

static bool verbose = false;

static std::unique_ptr<raw_fd_ostream> trace;

static std::unique_ptr<MemoryBuffer> dwarfBuffer;
static std::unique_ptr<object::Binary> dwarfBinary;
static std::unique_ptr<DWARFContext> dwarfCtx;

static size_t stackDepth = 0;
static bool lastInstWasCall = false;

static QBDI::VMAction onInstruction(QBDI::VMInstanceRef vm,
                                    QBDI::GPRState *gprState,
                                    QBDI::FPRState *fprState, void *data) {
  QBDI::AnalysisType analysisType = QBDI::ANALYSIS_INSTRUCTION;
  if (verbose)
    analysisType |= QBDI::ANALYSIS_DISASSEMBLY;
  const QBDI::InstAnalysis *instAnalysis = vm->getInstAnalysis(analysisType);
  const auto &address = instAnalysis->address;

  // Look for function name and line info related to this address
  const auto lineInfo = dwarfCtx->getLineInfoForAddress(
      {address}, {DILineInfoSpecifier::FileLineInfoKind::RawValue,
                  DILineInfoSpecifier::FunctionNameKind::ShortName});

  // If last instrumented instruction was a call, and then we encountered an
  // instruction without info, revert the stack depth.
  if (lastInstWasCall) {
    lastInstWasCall = false;
    if (!lineInfo)
      --stackDepth;
  }

  // Print indentation to represent current stack depth
  for (size_t i = 0; i < stackDepth; ++i)
    *trace << "  ";

  // 16 hex digits for 64-bit address plus 2 character prefix
  if (verbose)
    *trace << format_hex(address, 18) << " ";

  // Print function name and line info
  if (lineInfo) {
    const auto functionOffset = address - *lineInfo.StartAddress;
    *trace << lineInfo.FunctionName;
    if (verbose)
      *trace << " + " << format_hex(functionOffset, 6);
    *trace << " at " << lineInfo.FileName << ":" << lineInfo.Line << ":"
           << lineInfo.Column << "\n";
  } else {
    *trace << "🔔 No info for this address\n";
  }

  // Include disassembly for trace debugging
  if (verbose)
    *trace << instAnalysis->disassembly << "\n";

  // Update stack depth for next instruction after calls and returns
  if (instAnalysis->isCall) {
    ++stackDepth;
    lastInstWasCall = true;
  } else if (instAnalysis->isReturn) {
    if (stackDepth)
      --stackDepth;
    else
      *trace << "🔔 Ignoring return, stack depth would have wrapped around\n";
  }

  return QBDI::CONTINUE;
}

static bool loadDWARFDebugInfo(const Twine &dwarfPath) {
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

extern "C" {

QBDIPRELOAD_INIT;

int qbdipreload_on_start(void *main) { return QBDIPRELOAD_NOT_HANDLED; }

int qbdipreload_on_premain(void *gprCtx, void *fpuCtx) {
  return QBDIPRELOAD_NOT_HANDLED;
}

int qbdipreload_on_main(int argc, char **argv) {
  if (std::getenv("CON_TRACE_VERBOSE"))
    verbose = true;

  StringRef execPath(argv[0]);
  if (!loadDWARFDebugInfo(execPath + ".dwarf"))
    return QBDIPRELOAD_ERR_STARTUP_FAILED;

  // Leave this as "not handled" so the default handler will still run, which
  // starts up the QBDI instrumentation VM.
  return QBDIPRELOAD_NOT_HANDLED;
}

int qbdipreload_on_run(QBDI::VMInstanceRef vm, QBDI::rword start,
                       QBDI::rword stop) {
  std::error_code error;
  trace = std::make_unique<raw_fd_ostream>("concrete-trace/trace", error);
  if (error) {
    errs() << "Error: Unable to open trace file\n";
    return QBDIPRELOAD_ERR_STARTUP_FAILED;
  }

  vm->addCodeCB(QBDI::PREINST, onInstruction, nullptr);
  vm->run(start, stop);

  return QBDIPRELOAD_NO_ERROR;
}

int qbdipreload_on_exit(int status) { return QBDIPRELOAD_NO_ERROR; }
}
