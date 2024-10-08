#include <cassert>
#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <functional>
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
bool printReturnFromLocation = true;
bool includeExternalLibrary = true;
bool includeInternalFunction = true;

std::unique_ptr<raw_fd_ostream> trace;

std::unique_ptr<MemoryBuffer> dwarfBuffer;
std::unique_ptr<object::Binary> dwarfBinary;
std::unique_ptr<DWARFContext> dwarfCtx;

// Initialised to `true` to cover the first instrumented instruction in `main`
bool lastInstWasCallLike = true;
QBDI::rword lastCallReturnTarget = 0;

SmallVector<DWARFDie, 4> inlinedChain;
SmallVector<DWARFDie, 4> lastInlinedChain;

std::optional<std::function<void()>> queuedEvent;

struct StackFrame {
  StackFrame(DWARFDie entry) : entry(entry) {}

  DWARFDie entry;

  // Marks frames that do not exist on the "native" stack in memory.
  // During a tail call, the caller's native stack frame becomes the callee's.
  // In our view of the stack, we retain the caller's frame, mark it
  // artificial (since it no longer exists natively), and push an additional
  // frame for the callee.
  // When a return instruction is encountered, it will pop the current frame and
  // any adjacent artificial frames (as we must assume they are now gone).
  bool isArtificial = false;
};

// Newest frames are at the end of the stack vector
std::vector<StackFrame> stack;
bool stackDepthChanged = true;

// Defaults to current depth, but non-current depth can also be passed in
void printStackDepth(const std::optional<size_t> &depth = std::nullopt);

void printStackDepth(const std::optional<size_t> &depth) {
  const size_t stackDepth = depth.value_or(stack.size());
  // *trace << format("%4lu", stackDepth) << ": ";
  // Print indentation to represent current stack depth
  for (size_t i = 1; i < stackDepth; ++i)
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

DWARFDie getCallSiteEntry(const DWARFDie &entry, const QBDI::rword &address) {
  if (!entry.hasChildren())
    return DWARFDie();
  for (const auto &callSite : entry.children()) {
    if (callSite.getTag() != dwarf::Tag::DW_TAG_call_site)
      continue;
    if (const auto attrValue = callSite.find(dwarf::DW_AT_call_pc)) {
      if (attrValue->getAsAddress() == address)
        return callSite;
    }
  }
  return DWARFDie();
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

bool isFunctionPrintable() {
  if (includeInternalFunction)
    return true;
  // Filter only applies to subprogram (non-inlined) functions
  if (inlinedChain.size() != 1)
    return true;
  // Look for a possible subprogram (non-inlined function) entry
  DWARFDie entry = inlinedChain.back();
  // Skip over any lexical blocks in the parent chain
  while (entry.isValid() && !entry.isSubroutineDIE()) {
    entry = entry.getParent();
  }
  assert(entry.isValid() && entry.isSubprogramDIE());
  // Check `external` attribute on subprogram entry
  if (const auto attrValue = entry.find(dwarf::DW_AT_external)) {
    return attrValue->getRawUValue();
  }
  // Assume internal if no attribute found
  return false;
}

bool isLocationPrintable(const EventType &type) {
  if (!printLocation)
    return false;
  if (type == EventType::ReturnFrom && !printReturnFromLocation)
    return false;
  return true;
}

void printEventFromLineInfo(
    const DILineInfo &lineInfo, const EventType &type,
    const EventSource &source,
    const std::optional<QBDI::rword> &address = std::nullopt,
    const std::optional<bool> &isBranch = std::nullopt,
    const std::optional<size_t> &depth = std::nullopt);

void printEventFromLineInfo(const DILineInfo &lineInfo, const EventType &type,
                            const EventSource &source,
                            const std::optional<QBDI::rword> &address,
                            const std::optional<bool> &isBranch,
                            const std::optional<size_t> &depth) {
  if (!isFunctionPrintable())
    return;
  printStackDepth(depth);
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
    *trace << " at " << lineInfo.FileName;
    if (isLocationPrintable(type))
      *trace << ":" << lineInfo.Line << ":" << lineInfo.Column;
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

void traceRegisters(QBDI::GPRState *gprState) {
  *trace << "rsp:         " << format_hex(gprState->rsp, 18) << "\n";
  *trace << "*(rsp):      "
         << format_hex(*((QBDI::rword *)gprState->rsp + 0), 18) << "\n";
  *trace << "*(rsp + 8):  "
         << format_hex(*((QBDI::rword *)gprState->rsp + 8), 18) << "\n";
  *trace << "*(rsp + 16): "
         << format_hex(*((QBDI::rword *)gprState->rsp + 16), 18) << "\n";
}

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

  // Also return from any artificial frames from past tail calls
  if (!stack.empty() && stack.back().isArtificial) {
    if (verbose)
      *trace << "Returning from artificial frame\n";

    const auto &entry = stack.back().entry;

    DILineInfo lineInfo;
    lineInfo.FunctionName = entry.getShortName();

    lineInfo.FileName =
        entry.getDeclFile(DILineInfoSpecifier::FileLineInfoKind::RawValue);
    // Source coordinates not generally available when leaving these
    // artificial frames
    lineInfo.Line = 0;
    lineInfo.Column = 0;

    printEventFromLineInfo(lineInfo, EventType::ReturnFrom, EventSource::Stack);
    popStackFrame();
  }
}

QBDI::VMAction beforeInstruction(QBDI::VMInstanceRef vm,
                                 QBDI::GPRState *gprState,
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

  // If we're jumping to external code, we may have a queued "call to" event
  // that needs to be checked against our filters before printing
  const bool isBranchToExternal = !lineInfo && instAnalysis->isBranch;
  if (isBranchToExternal && !includeExternalLibrary) {
    queuedEvent = std::nullopt;
  }

  // Get the inlined chain for this address
  inlinedChain.clear();
  getInlinedChain(address, inlinedChain);

  // If there's an event queued, print that now
  if (queuedEvent) {
    (*queuedEvent)();
    queuedEvent = std::nullopt;
  }

  // If last instruction was a call, push a new stack frame using the debug
  // entry inside the callee.
  if (lastInstWasCallLike) {
    lastInstWasCallLike = false;

    // TODO: Push stack frame using address called
    // (to properly report indirect calls to external code)

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
      if (verbose)
        *trace << "Call return target matches\n";
      lastCallReturnTarget = 0;
      popStackFrame();
    }
  }

  // Examine branches in case they are actually tail calls
  bool isTailCall = false;
  if (instAnalysis->isBranch && !inlinedChain.empty()) {
    const auto callSite = getCallSiteEntry(inlinedChain.back(), address);
    if (const auto attrValue = callSite.find(dwarf::DW_AT_call_tail_call)) {
      // Branch is a tail call
      isTailCall = true;
      if (verbose)
        *trace << "Branch is a tail call\n";
      // Current frame becomes artificial after tail call
      stack.back().isArtificial = true;
    }
  }

  const bool isCallLike = instAnalysis->isCall || isTailCall;
  // Our view of stack depth changes even for tail calls.
  // We preserve tail caller frames as artificial and
  // push additional frames for the tail callee.
  const bool stackDepthWillChange = isCallLike || instAnalysis->isReturn;

  // In verbose mode, log this instruction, but only if other blocks will not
  if (verbose && !stackDepthWillChange && !stackDepthChanged) {
    printEventFromLineInfo(lineInfo, EventType::Verbose, EventSource::Verbose,
                           address, instAnalysis->isBranch);
  }

  // Log to trace after stack depth changed (by previous instruction)
  if (stackDepthChanged && (!isBranchToExternal || includeExternalLibrary)) {
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
      const auto &entry = stack[i].entry;
      if (verbose)
        *trace << "stack[" << i << "]: " << entry.getShortName() << "\n";
      if (entry != oldestChainLink)
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
      const auto &entry = stack[stackIdxOldestChainLink + i].entry;
      if (verbose) {
        *trace << "stack[" << stackIdxOldestChainLink + i << "]: ";
        *trace << entry.getShortName() << "\n";
        *trace << "inlinedChain[" << i << "]: ";
        if (i < newChainSize)
          *trace << inlinedChain[i].getShortName() << "\n";
        else
          *trace << "past end of chain\n";
      }
      if (i < newChainSize && entry == inlinedChain[i]) {
        chainIdxNewestMatchingStack = i;
        break;
      }
      printReturnFromEventForInlinedEntry(stack.back().entry);
      // JRS: Not sure yet if inline-pop should also pop artificial frames...
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
    const size_t depth = stack.size();
    if (isCallLike) {
      // Queue for (potential) future printing based on next instruction
      queuedEvent = [=]() {
        // Will have access to the _next_ instruction's inlined chain when run
        printEventFromLineInfo(lineInfo, EventType::CallFrom,
                               EventSource::Stack, address,
                               instAnalysis->isBranch, depth);
      };
    } else {
      // Print immediately
      printEventFromLineInfo(lineInfo, EventType::ReturnFrom,
                             EventSource::Stack, address,
                             instAnalysis->isBranch, depth);
    }
  }

  // Update stack depth for next instruction after calls and returns
  if (isCallLike) {
    // Will push stack frame on next instruction
    // (so that we push the debug entry for the callee)
    lastInstWasCallLike = true;
    // See related code in post-instruction hook below which will capture the
    // call return target
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

QBDI::VMAction afterInstruction(QBDI::VMInstanceRef vm,
                                QBDI::GPRState *gprState,
                                QBDI::FPRState *fprState, void *data) {
  if (lastInstWasCallLike) {
    // We will track the address to return to so we can compare to stack value
    // on next instrumented instruction to check whether we followed the call.
    // To do this for both regular and tail calls, we need to capture the
    // post-instruction value at `rsp` after the call-like instruction.
    lastCallReturnTarget = *((QBDI::rword *)gprState->rsp);

    if (verbose) {
      *trace << "Last inst. was call-like\n";
      *trace << "Call return target: " << format_hex(lastCallReturnTarget, 18)
             << "\n";
      // traceRegisters(gprState);
    }
  }

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
  if (std::getenv("CON_TRACE_RF_LOCATION") &&
      !std::strcmp(std::getenv("CON_TRACE_RF_LOCATION"), "0"))
    printReturnFromLocation = false;
  if (std::getenv("CON_TRACE_EXTERNAL_LIBRARY") &&
      !std::strcmp(std::getenv("CON_TRACE_EXTERNAL_LIBRARY"), "0"))
    includeExternalLibrary = false;
  if (std::getenv("CON_TRACE_INTERNAL_FUNCTION") &&
      !std::strcmp(std::getenv("CON_TRACE_INTERNAL_FUNCTION"), "0"))
    includeInternalFunction = false;

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

  vm->addCodeCB(QBDI::PREINST, beforeInstruction, nullptr);
  vm->addCodeCB(QBDI::POSTINST, afterInstruction, nullptr);
  vm->run(start, stop);

  return QBDIPRELOAD_NO_ERROR;
}

int qbdipreload_on_exit(int status) { return QBDIPRELOAD_NO_ERROR; }
}
