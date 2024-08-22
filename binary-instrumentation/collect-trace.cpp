#include <cassert>
#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <memory>
#include <optional>
#include <system_error>
#include <utility>
#include <vector>

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Twine.h"
#include "llvm/DebugInfo/DIContext.h"
#include "llvm/DebugInfo/DWARF/DWARFCompileUnit.h"
#include "llvm/DebugInfo/DWARF/DWARFContext.h"
#include "llvm/DebugInfo/DWARF/DWARFDie.h"
#include "llvm/Object/Binary.h"
#include "llvm/Object/ObjectFile.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/ErrorOr.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

#include "QBDIPreload.h"

using namespace llvm;

namespace {

void *mainFunc;

bool verbose = false;
bool printSource = false;
bool printLocation = true;

std::unique_ptr<raw_fd_ostream> trace;

std::unique_ptr<MemoryBuffer> dwarfBuffer;
std::unique_ptr<object::Binary> dwarfBinary;
std::unique_ptr<DWARFContext> dwarfCtx;

// Newest frames are at the end of the stack vector
std::vector<DWARFDie> stack;
bool stackDepthChanged = true;

// Initialised to `true` to cover the first instrumented instruction in `main`
bool lastInstWasCall = true;
QBDI::rword lastCallReturnTarget = 0;

SmallVector<DWARFDie, 4> lastInlinedChain;

void pushStackFrame(const DWARFDie &entry) {
  stack.push_back(entry);
  if (verbose)
    *trace << "push\n";
  // Only non-inlined entries notify stack depth changes for event printing
  // Inlined chain processing handles printing all inlined entries when needed
  if (!entry || entry.isSubprogramDIE())
    stackDepthChanged = true;
}

void popStackFrame() {
  if (!stack.empty()) {
    stack.pop_back();
    if (verbose)
      *trace << "pop\n";
    // We skip the "return to" event, as this is not expected to map
    // to the same source location across versions.
    // stackDepthChanged = true;
  } else {
    *trace << "🔔 Ignoring return, stack depth would have wrapped around\n";
  }
}

void printStackDepth() {
  // *trace << format("%4lu", stackDepth) << ": ";
  // Print indentation to represent current stack depth
  for (size_t i = 1, e = stack.size(); i < e; ++i)
    *trace << "  ";
}

void getInlinedChain(const QBDI::rword &address,
                     SmallVectorImpl<DWARFDie> &inlinedChain) {
  DWARFCompileUnit *compileUnit =
      dwarfCtx->getCompileUnitForCodeAddress(address);
  if (!compileUnit)
    return;
  // Stores inlined chain with newest frames at the beginning
  compileUnit->getInlinedChainForAddress(address, inlinedChain);
  // Reverse chain so that order matches stack vector
  // Newest frames are now at the end of the vector
  std::reverse(inlinedChain.begin(), inlinedChain.end());
}

enum struct EventType {
  CallFrom,
  CallTo,
  ReturnFrom,
  Verbose,
};

inline raw_ostream &operator<<(raw_ostream &trace, const EventType &type) {
  switch (type) {
  case EventType::CallFrom:
    trace << "CF";
    break;
  case EventType::CallTo:
    trace << "CT";
    break;
  case EventType::ReturnFrom:
    trace << "RF";
    break;
  case EventType::Verbose:
    trace << "V";
    break;
  default:
    llvm_unreachable("Unexpected EventType");
  }
  return trace;
}

enum struct EventSource {
  Stack,
  InlinedChain,
  Verbose,
};

void printEventFromLineInfo(
    const DILineInfo &lineInfo, const EventType &type,
    const EventSource &source,
    const std::optional<QBDI::rword> &address = std::nullopt,
    const std::optional<bool> &isBranch = std::nullopt);

void printEventFromLineInfo(const DILineInfo &lineInfo, const EventType &type,
                            const EventSource &source,
                            const std::optional<QBDI::rword> &address,
                            const std::optional<bool> &isBranch) {
  printStackDepth();
  if (printSource && source == EventSource::InlinedChain) {
    *trace << "I";
  }
  *trace << type;
  *trace << ": ";
  if (lineInfo) {
    *trace << lineInfo.FunctionName;
    if (address && verbose) {
      const auto functionOffset = *address - *lineInfo.StartAddress;
      *trace << " + " << format_hex(functionOffset, 6);
    }
    if (printLocation)
      *trace << " at " << lineInfo.FileName << ":" << lineInfo.Line << ":"
             << lineInfo.Column;
  } else {
    if (isBranch && *isBranch)
      *trace << "Jump to external code";
    else
      *trace << "🔔 No info for this address";
  }
  *trace << "\n";
}

void printCallFromEventForInlinedEntry(const DWARFDie &entry) {
  assert(entry.getTag() == dwarf::Tag::DW_TAG_inlined_subroutine);

  DILineInfo lineInfo;
  // Function where simulated call occurred comes from the parent entry
  DWARFDie parent = entry.getParent();
  // Skip over any lexical blocks in the parent chain
  while (parent.isValid() && !parent.isSubroutineDIE()) {
    parent = parent.getParent();
  }
  assert(parent.isValid());
  lineInfo.FunctionName = parent.getShortName();

  // Extracted from `DWARFContext::getInliningInfoForAddress`
  uint32_t callFile = 0, callLine = 0, callColumn = 0, callDisc = 0;
  entry.getCallerFrame(callFile, callLine, callColumn, callDisc);
  auto *unit = entry.getDwarfUnit();
  const auto *lineTable = dwarfCtx->getLineTableForUnit(unit);
  std::string fileName;
  lineTable->getFileNameByIndex(callFile, unit->getCompilationDir(),
                                DILineInfoSpecifier::FileLineInfoKind::RawValue,
                                fileName);
  lineInfo.FileName = fileName;
  lineInfo.Line = callLine;
  lineInfo.Column = callColumn;

  printEventFromLineInfo(lineInfo, EventType::CallFrom,
                         EventSource::InlinedChain);
}

void printCallToEventForInlinedEntry(const DWARFDie &entry) {
  assert(entry.getTag() == dwarf::Tag::DW_TAG_inlined_subroutine);

  DILineInfo lineInfo;
  lineInfo.FunctionName = entry.getShortName();

  // Use decl. file and line from abstract origin entry
  lineInfo.FileName =
      entry.getDeclFile(DILineInfoSpecifier::FileLineInfoKind::RawValue);
  lineInfo.Line = entry.getDeclLine();
  lineInfo.Column = 0;

  printEventFromLineInfo(lineInfo, EventType::CallTo,
                         EventSource::InlinedChain);
}

void printReturnFromEventForInlinedEntry(const DWARFDie &entry) {
  assert(entry.getTag() == dwarf::Tag::DW_TAG_inlined_subroutine);

  DILineInfo lineInfo;
  lineInfo.FunctionName = entry.getShortName();

  // Use decl. file from abstract origin entry
  lineInfo.FileName =
      entry.getDeclFile(DILineInfoSpecifier::FileLineInfoKind::RawValue);
  // Function epilogue location not generally available when inlined
  // TODO: Look potential out-of-line copy, borrow its location
  lineInfo.Line = 0;
  lineInfo.Column = 0;

  printEventFromLineInfo(lineInfo, EventType::ReturnFrom,
                         EventSource::InlinedChain);
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
  // JRS: Remove this and use only the inlined chain...?
  const auto lineInfo = dwarfCtx->getLineInfoForAddress(
      {address}, {DILineInfoSpecifier::FileLineInfoKind::RawValue,
                  DILineInfoSpecifier::FunctionNameKind::ShortName});

  // Get the inlined chain for this address
  SmallVector<DWARFDie, 4> inlinedChain;
  getInlinedChain(address, inlinedChain);

  // If last instruction was a call, push a new stack frame using the debug
  // entry inside the callee.
  if (lastInstWasCall) {
    lastInstWasCall = false;
    // If the inlined chain is empty (implying no debug info for this address),
    // we still push an (empty) entry to record the stack depth change.
    if (!inlinedChain.empty())
      pushStackFrame(inlinedChain.back());
    else
      pushStackFrame(DWARFDie());
  }

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

  // In verbose mode, log this instruction, but only if other blocks will not
  if (verbose && !stackDepthWillChange && !stackDepthChanged) {
    printEventFromLineInfo(lineInfo, EventType::Verbose, EventSource::Verbose,
                           address, instAnalysis->isBranch);
  }

  // Log to trace after stack depth changed (by previous instruction)
  if (stackDepthChanged) {
    printEventFromLineInfo(lineInfo, EventType::CallTo, EventSource::Stack,
                           address, instAnalysis->isBranch);
  }

  // Reset did change tracker
  stackDepthChanged = false;

  // Compare inlined chain for this address to last chain
  // TODO: Adjust this logic for multiple active inlined chains
  if (verbose) {
    *trace << "Inlined chain:\n";
    for (const auto &entry : inlinedChain) {
      *trace << entry.getShortName() << "\n";
    }
  }
  if (!inlinedChain.empty() && lastInlinedChain != inlinedChain) {
    // Inlined chain has changed since last instruction
    // Adjust stack frames to match change in inline details
    // JRS: There's some partial overlap with the non-inlined push / pops...
    if (verbose)
      *trace << "Inlined chain changed!\n";
    // Both the stack and inlined chain vectors are sorted
    // with newest frames at the end of the vector.
    const size_t oldChainSize = lastInlinedChain.size();
    const size_t newChainSize = inlinedChain.size();
    if (verbose) {
      *trace << "Old chain: " << oldChainSize << "\n";
      *trace << "New chain: " << newChainSize << "\n";
    }

    assert(!stack.empty());
    // Align inlined chain by finding the oldest chain link in the stack
    // TODO: Limit search by only looking as far back as old chain size
    const auto &oldestChainLink = inlinedChain[0];
    if (verbose) {
      *trace << "Aligning inlined chain\n";
      *trace << "Oldest chain link: " << oldestChainLink.getShortName() << "\n";
    }
    size_t stackIdxOldestChainLink = SIZE_T_MAX;
    for (size_t i = stack.size(); i--;) {
      if (verbose)
        *trace << "stack[" << i << "]: " << stack[i].getShortName() << "\n";
      if (stack[i] != oldestChainLink)
        continue;
      stackIdxOldestChainLink = i;
      break;
    }
    // As long as the stack is not empty,
    // we should always find at least one chain link in the stack
    assert(stackIdxOldestChainLink != SIZE_T_MAX);

    // Pop any stack frames not found in the new inlined chain
    size_t chainIdxNewestMatchingStack = SIZE_T_MAX;
    size_t stackItemsToCheck = stack.size() - stackIdxOldestChainLink;
    if (verbose)
      *trace << "Popping any frames not found\n";
    for (size_t i = stackItemsToCheck; i--;) {
      if (verbose) {
        *trace << "stack[" << stackIdxOldestChainLink + i << "]: ";
        *trace << stack[stackIdxOldestChainLink + i].getShortName() << "\n";
        *trace << "inlinedChain[" << i << "]: ";
        if (i < newChainSize)
          *trace << inlinedChain[i].getShortName() << "\n";
        else
          *trace << "past end of chain\n";
      }
      if (i < newChainSize &&
          stack[stackIdxOldestChainLink + i] == inlinedChain[i]) {
        chainIdxNewestMatchingStack = i;
        break;
      }
      printReturnFromEventForInlinedEntry(stack.back());
      popStackFrame();
    }
    // From the alignment block above,
    // we know there must be at least one chain link in the stack
    assert(chainIdxNewestMatchingStack != SIZE_T_MAX);

    // Push any new frames beyond what is already in the stack
    if (verbose)
      *trace << "Pushing any new frames\n";
    for (size_t i = chainIdxNewestMatchingStack + 1; i < newChainSize; ++i) {
      // Print call frame info _before_ pushing, since simulated call would
      // have occurred in frame before the one being pushed
      const auto &entry = inlinedChain[i];
      printCallFromEventForInlinedEntry(entry);
      pushStackFrame(entry);
      printCallToEventForInlinedEntry(entry);
    }

    // Store chain to check for changes with next instruction
    lastInlinedChain = inlinedChain;
  }

  // Log to trace before stack depth changes (by this instruction)
  if (stackDepthWillChange) {
    EventType type =
        instAnalysis->isCall ? EventType::CallFrom : EventType::ReturnFrom;
    printEventFromLineInfo(lineInfo, type, EventSource::Stack, address,
                           instAnalysis->isBranch);
  }

  // Update stack depth for next instruction after calls and returns
  if (instAnalysis->isCall) {
    // Will push stack frame on next instruction
    // (so that we push the debug entry for the callee)
    lastInstWasCall = true;
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
  if (std::getenv("CON_TRACE_SOURCE"))
    printSource = true;
  if (std::getenv("CON_TRACE_LOCATION") &&
      !std::strcmp(std::getenv("CON_TRACE_LOCATION"), "0"))
    printLocation = false;

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
