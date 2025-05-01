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

#include <sys/types.h>
#include <unistd.h>

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Twine.h"
#include "llvm/DebugInfo/DIContext.h"
#include "llvm/DebugInfo/DWARF/DWARFCompileUnit.h"
#include "llvm/DebugInfo/DWARF/DWARFContext.h"
#include "llvm/DebugInfo/DWARF/DWARFDie.h"
#include "llvm/Object/Binary.h"
#include "llvm/Object/ELFObjectFile.h"
#include "llvm/Object/MachO.h"
#include "llvm/Object/ObjectFile.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/ErrorOr.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"

#include "QBDIPreload.h"

#include "elf.h"
#include "macho.h"
#include "trace-debug.h"

using namespace llvm;

namespace {

QBDI::rword mainFunc;

bool append = false;
bool verbose = false;
bool printSource = false;
bool printLocation = true;
bool printReturnFromLocation = true;
bool includeExternalLibrary = true;
bool includeInternalFunction = true;

std::unique_ptr<raw_fd_ostream> trace;

std::unique_ptr<MemoryBuffer> execBuffer;
std::unique_ptr<object::Binary> execBinary;

std::unique_ptr<MemoryBuffer> dwarfBuffer;
std::unique_ptr<object::Binary> dwarfBinary;
std::unique_ptr<DWARFContext> dwarfCtx;

std::vector<QBDI::MemoryMap> currentModuleMemoryMaps;

// State prefixed with `prev` is passed from previous instruction
// to hooks for next instruction.
// State prefixed with `curr` is passed from pre-instruction hook
// to post-instruction hook.

bool currInstIsCall = false;
bool currInstIsTailCall = false;
bool currInstIsBranch = false;
bool currInstIsReturn = false;
QBDI::rword prevCallReturnTarget = 0;

SmallVector<DWARFDie, 4> inlinedChain;
SmallVector<DWARFDie, 4> prevInlinedChain;

// We don't commit to "call from" / "call to" events until we have analysis
// for the next instruction, so we queue them here for potential printing.
std::optional<std::function<void()>> queuedCallFromEvent;
std::optional<std::function<void()>> queuedCallToEvent;

void dropQueue() {
  // TODO: Dropped events should still be printed in verbose mode
  queuedCallFromEvent = std::nullopt;
  queuedCallToEvent = std::nullopt;
}

void printQueue() {
  if (queuedCallFromEvent) {
    (*queuedCallFromEvent)();
    queuedCallFromEvent = std::nullopt;
  }
  if (queuedCallToEvent) {
    (*queuedCallToEvent)();
    queuedCallToEvent = std::nullopt;
  }
}

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

  // The first address we encountered when calling into this frame.
  QBDI::rword callToAddress;
};

// Newest frames are at the end of the stack vector
std::vector<StackFrame> stack;
bool stackDepthChanged = false;

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

DWARFDie getCallSiteEntry(const DWARFDie &entry, const QBDI::rword &address,
                          const uint32_t &instSize) {
  if (!entry.hasChildren())
    return DWARFDie();
  for (const auto &callSite : entry.children()) {
    const auto tag = callSite.getTag();
    // Clang uses `DW_TAG_call_site` (DWARF 5) even when emitting DWARF 4
    // GCC uses `DW_TAG_GNU_call_site` with DWARF 4 in non-strict mode
    if (tag != dwarf::Tag::DW_TAG_call_site &&
        tag != dwarf::Tag::DW_TAG_GNU_call_site)
      continue;
    // Clang
    if (const auto attrValue = callSite.find(dwarf::DW_AT_call_pc)) {
      if (attrValue->getAsAddress() == address)
        return callSite;
    }
    // GCC includes `DW_AT_low_pc`, but it points at the instruction _after_
    if (const auto attrValue = callSite.find(dwarf::DW_AT_low_pc)) {
      if (attrValue->getAsAddress() == (address + instSize))
        return callSite;
    }
  }
  return DWARFDie();
}

bool isAddressInCurrentModule(QBDI::rword address) {
  for (const auto &mm : currentModuleMemoryMaps) {
    if (mm.range.contains(address))
      return true;
  }
  return false;
}

enum struct EventType {
  CallFrom,
  CallTo,
  ReturnFrom,
  Verbose,
};

inline raw_ostream &operator<<(raw_ostream &stream, const EventType &type) {
  switch (type) {
  case EventType::CallFrom:
    stream << "CF";
    break;
  case EventType::CallTo:
    stream << "CT";
    break;
  case EventType::ReturnFrom:
    stream << "RF";
    break;
  case EventType::Verbose:
    stream << "V";
    break;
  default:
    llvm_unreachable("Unexpected EventType");
  }
  return stream;
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

std::optional<StringRef> findDynamicFunctionName(QBDI::rword address) {
  const auto &execObjectFile = cast<object::ObjectFile>(*execBinary);
  // Cached here via `static`
  static const bool isELF = execObjectFile.isELF();
  static const bool isMachO = execObjectFile.isMachO();
  if (isELF)
    return findDynamicFunctionNameELF(
        address, cast<object::ELFObjectFileBase>(execObjectFile));
  if (isMachO)
    return findDynamicFunctionNameMachO(
        address, cast<object::MachOObjectFile>(execObjectFile));
  return std::nullopt;
}

void printEventFromLineInfo(
    const DILineInfo &lineInfo, const EventType &type,
    const EventSource &source,
    const std::optional<QBDI::rword> &address = std::nullopt,
    const std::optional<QBDI::VMInstanceRef> &vm = std::nullopt,
    const std::optional<size_t> &depth = std::nullopt,
    const std::optional<bool> &tailCallWithoutInfo = std::nullopt);

void printEventFromLineInfo(const DILineInfo &lineInfo, const EventType &type,
                            const EventSource &source,
                            const std::optional<QBDI::rword> &address,
                            const std::optional<QBDI::VMInstanceRef> &vm,
                            const std::optional<size_t> &depth,
                            const std::optional<bool> &tailCallWithoutInfo) {
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
    if (address && vm && (*vm)->getCachedInstAnalysis(*address) &&
        (*vm)->getCachedInstAnalysis(*address)->isBranch) {
      *trace << "Jump to external code";
      const std::optional<StringRef> symbolName =
          findDynamicFunctionName(*address);
      if (symbolName)
        *trace << " for " << symbolName;
    } else if (address && !isAddressInCurrentModule(*address)) {
      *trace << "External code";
    } else {
      *trace << "No info for this address";
    }
  }
  if (tailCallWithoutInfo && *tailCallWithoutInfo)
    *trace << " (TCWI)";
  *trace << "\n";
  // Flush after each event to avoid trace corruption due to context switching
  // caused by threads, signals, etc.
  trace->flush();
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

void pushStackFrame(const DWARFDie &entry) {
  stack.push_back(entry);
  if (verbose)
    *trace << "push\n";
  // Only non-inlined entries notify stack depth changes for event printing
  // Inlined chain processing handles printing all inlined entries when needed
  if (!entry || entry.isSubprogramDIE())
    stackDepthChanged = true;
}

void popStackFrame(const QBDI::VMInstanceRef &vm) {
  if (!stack.empty()) {
    stack.pop_back();
    if (verbose)
      *trace << "pop\n";
    // We skip the "return to" event, as this is not expected to map
    // to the same source location across versions.
    // stackDepthChanged = true;
  } else {
    *trace << "Ignoring return, stack depth would have wrapped around\n";
  }

  // Also return from any artificial frames from past tail calls
  if (!stack.empty() && stack.back().isArtificial) {
    if (verbose)
      *trace << "Returning from artificial frame\n";

    const auto &frame = stack.back();
    const auto &entry = frame.entry;

    bool frameIsBranchToExternal = false;
    DILineInfo lineInfo;
    if (entry.isValid()) {
      lineInfo.FunctionName = entry.getShortName();
      lineInfo.FileName =
          entry.getDeclFile(DILineInfoSpecifier::FileLineInfoKind::RawValue);
      // Source coordinates not generally available when leaving these
      // artificial frames
      lineInfo.Line = 0;
      lineInfo.Column = 0;
    } else if (vm->getCachedInstAnalysis(frame.callToAddress) &&
               vm->getCachedInstAnalysis(frame.callToAddress)->isBranch) {
      // Frame appears to represent a branch to external code
      frameIsBranchToExternal = true;
    }

    if (includeExternalLibrary || !frameIsBranchToExternal)
      printEventFromLineInfo(lineInfo, EventType::ReturnFrom,
                             EventSource::Stack, frame.callToAddress, vm);
    popStackFrame(vm);
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
  const auto &instSize = instAnalysis->instSize;

  // Save bits of current instruction analysis that may be of use
  // in the post-instruction hook as well
  // JRS: We can still access instruction analysis for the current instruction
  // in the post-instruction hook. Should we do that instead of shared state...?
  currInstIsCall = instAnalysis->isCall;
  // Tail call status discovered in several places across both hooks
  currInstIsTailCall = false;
  currInstIsBranch = instAnalysis->isBranch;
  currInstIsReturn = instAnalysis->isReturn;

  // Print address and disassembly in verbose mode for trace debugging
  if (verbose) {
    // 16 hex digits for 64-bit address plus 2 character prefix
    *trace << format_hex(address, 18) << "\n";
    *trace << instAnalysis->disassembly << "\n";
  }

  // Our view of stack depth changes even for tail calls.
  // We preserve tail caller frames as artificial and
  // push additional frames for the tail callee.
  const bool stackDepthMayChange = instAnalysis->affectControlFlow;

  // Look for function name and line info related to this address
  // JRS: Remove this and use only the inlined chain...?
  DILineInfo lineInfo;
  if (verbose || stackDepthMayChange) {
    lineInfo = dwarfCtx->getLineInfoForAddress(
        {address}, {DILineInfoSpecifier::FileLineInfoKind::RawValue,
                    DILineInfoSpecifier::FunctionNameKind::ShortName});
  }

  // If we're jumping to external code, we may have queued events
  // that need to be checked against our filters before printing
  // TODO: Check that we're actually in the jump table rather than assuming
  // that's what happens for all branches without line info.
  const bool isBranchToExternal = currInstIsBranch && !lineInfo;
  if (isBranchToExternal && !includeExternalLibrary) {
    dropQueue();
  }

  // If there are queued events, print them now
  printQueue();

  // Get the inlined chain for this address
  inlinedChain.clear();
  getInlinedChain(address, inlinedChain);

  // If previous instrumented instruction was a call, check whether this next
  // instrumented instruction was the call's return target. If yes, it means we
  // just returned from uninstrumented code and need to adjust stack depth.
  // TODO: May need to track the full stack of addresses in case uninstrumented
  // code can call back into instrumented code.
  if (prevCallReturnTarget) {
    if (prevCallReturnTarget == address) {
      if (verbose)
        *trace << "Call return target matches\n";
      prevCallReturnTarget = 0;
      popStackFrame(vm);
    }
  }

  // Compare inlined chain for this address to previous chain
  // TODO: Adjust this logic for multiple active inlined chains
  if (verbose) {
    *trace << "Inlined chain:\n";
    for (const auto &entry : inlinedChain) {
      *trace << entry.getShortName() << "\n";
    }
  }
  if (!inlinedChain.empty() && prevInlinedChain != inlinedChain) {
    // Inlined chain has changed since previous instruction
    // Adjust stack frames to match change in inline details
    // JRS: There's some partial overlap with the non-inlined push / pops...
    if (verbose)
      *trace << "Inlined chain changed!\n";
    // Both the stack and inlined chain vectors are sorted
    // with newest frames at the end of the vector.
    const size_t oldChainSize = prevInlinedChain.size();
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
    size_t stackIdxOldestChainLink = SIZE_MAX;
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
    assert(stackIdxOldestChainLink != SIZE_MAX);

    // Pop any stack frames not found in the new inlined chain
    size_t chainIdxNewestMatchingStack = SIZE_MAX;
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
      popStackFrame(vm);
    }
    // From the alignment block above,
    // we know there must be at least one chain link in the stack
    assert(chainIdxNewestMatchingStack != SIZE_MAX);

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
    prevInlinedChain = inlinedChain;
  }

  // Examine branches in case they are actually tail calls
  if (currInstIsBranch && !inlinedChain.empty()) {
    const auto callSite =
        getCallSiteEntry(inlinedChain.back(), address, instSize);
    const bool attrFound = callSite.find(dwarf::DW_AT_call_tail_call) ||
                           callSite.find(dwarf::DW_AT_GNU_tail_call);
    if (attrFound) {
      // Stack marked as artificial in post-instruction hook
      currInstIsTailCall = true;
    }
  }

  // Log to trace before stack depth changes (by this instruction)
  if (stackDepthMayChange) {
    const size_t depth = stack.size();
    if (currInstIsCall || currInstIsBranch) {
      // Queue for (potential) future printing based on next instruction.
      // We may drop the call based on filtering rules (e.g. external code).
      // We don't know for certain whether a branch is or is not a tail call
      // here, as it might be treated as tail call by moving to external code.
      queuedCallFromEvent = [=]() {
        // Will have access to the _next_ instruction's inlined chain when run,
        // which is checked by `isFunctionPrintable` to filter internal function
        // events if needed
        printEventFromLineInfo(lineInfo, EventType::CallFrom,
                               EventSource::Stack, address, vm, depth);
      };
    } else {
      // Print immediately
      printEventFromLineInfo(lineInfo, EventType::ReturnFrom,
                             EventSource::Stack, address, vm, depth);
    }
  }

  // In verbose mode, log current instruction, but only if other blocks will not
  if (verbose && !stackDepthMayChange) {
    printEventFromLineInfo(lineInfo, EventType::Verbose, EventSource::Verbose,
                           address, vm);
  }

  return QBDI::CONTINUE;
}

QBDI::VMAction afterInstruction(QBDI::VMInstanceRef vm,
                                QBDI::GPRState *gprState,
                                QBDI::FPRState *fprState, void *data) {
  const auto &nextAddress = gprState->rip;

  // If the currently analysed instruction is a call or branch,
  // use the next instruction's address to check if we moved to external code
  bool nextInstInCurrentModule = true;
  if (currInstIsCall || currInstIsBranch) {
    // Check whether we moved to external code
    nextInstInCurrentModule = isAddressInCurrentModule(nextAddress);
    if (!nextInstInCurrentModule) {
      if (verbose) {
        *trace << "Inst. moved to external code: "
               << format_hex(gprState->rip, 18) << "\n";
      }
    }
  }

  // If the currently analysed instruction is a branch and moved to external,
  // treat this as a tail call
  if (currInstIsBranch && !nextInstInCurrentModule)
    currInstIsTailCall = true;

  // Get the inlined chain for the next instruction.
  // Note that is currently also (somewhat awkwardly) saved to shared state as
  // well because the internal function filter depends on it.
  // TODO: Remove this dependency
  if (currInstIsCall || currInstIsBranch) {
    inlinedChain.clear();
    getInlinedChain(nextAddress, inlinedChain);
  }

  bool tailCallWithoutInfo = false;
  // If the currently analysed instruction is a branch and moved to
  // different function, treat this as a tail call.
  // TODO: Consider how to safely detect this with inlining enabled
  if (currInstIsBranch && !currInstIsTailCall && prevInlinedChain.size() == 1 &&
      inlinedChain.size() == 1 && prevInlinedChain != inlinedChain) {
    if (verbose)
      *trace << "Branch changed functions, assuming tail call without info\n";
    tailCallWithoutInfo = true;
    currInstIsTailCall = true;
  }

  // If we detected a tail call via any means, log and mark the stack
  if (currInstIsTailCall) {
    if (verbose)
      *trace << "Branch is a tail call\n";
    // Current frame becomes artificial after tail call
    stack.back().isArtificial = true;
  }

  // We now know (as best we can) if a branch is or is not a tail call,
  // so we drop the queued call event if it is not
  if (currInstIsBranch && !currInstIsTailCall)
    dropQueue();

  // If the currently analysed instruction is call-like (call or tail call),
  // push a new stack frame using the debug entry inside the callee
  if (currInstIsCall || currInstIsTailCall) {
    // If the inlined chain is empty (implying no debug info for this address),
    // we still push an (empty) entry to record the stack depth change.
    if (!inlinedChain.empty())
      pushStackFrame(inlinedChain.back());
    else
      pushStackFrame(DWARFDie());
    // Capture the first address encountered in the callee's frame
    stack.back().callToAddress = nextAddress;

    // We will track the address to return to so we can compare to stack value
    // on next instrumented instruction to check whether we followed the call.
    // To do this for both regular and tail calls, we need to capture the
    // post-instruction value at `rsp` after the call-like instruction.
    prevCallReturnTarget = *((QBDI::rword *)gprState->rsp);

    if (verbose) {
      *trace << "Inst. was call-like\n";
      *trace << "Call return target: " << format_hex(prevCallReturnTarget, 18)
             << "\n";
    }
  }

  // Log next instruction after stack depth changed
  if (stackDepthChanged) {
    // Look for function name and line info related to this address
    // JRS: Remove this and use only the inlined chain...?
    const auto nextLineInfo = dwarfCtx->getLineInfoForAddress(
        {nextAddress}, {DILineInfoSpecifier::FileLineInfoKind::RawValue,
                        DILineInfoSpecifier::FunctionNameKind::ShortName});
    const size_t depth = stack.size();
    // Queue for (potential) future printing based on next instruction
    queuedCallToEvent = [=]() {
      printEventFromLineInfo(nextLineInfo, EventType::CallTo,
                             EventSource::Stack, nextAddress, vm, depth,
                             tailCallWithoutInfo);
    };
  }

  // Reset did change tracker
  stackDepthChanged = false;

  // If we moved to external code and external libraries are disabled,
  // drop any queued call events we may have.
  if (!nextInstInCurrentModule && !includeExternalLibrary)
    dropQueue();

  // If the currently analysed instruction is a return,
  // pop the current stack frame (along with any artificial frames)
  if (currInstIsReturn) {
    // Clear return target when we see an instrumented return
    prevCallReturnTarget = 0;
    popStackFrame(vm);
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

bool loadExecutable(const Twine &execPath) {
  // Create memory buffer for executable file
  ErrorOr<std::unique_ptr<MemoryBuffer>> execBufferMaybe =
      MemoryBuffer::getFileOrSTDIN(execPath);
  if (execBufferMaybe.getError()) {
    errs() << "Error: Unable to open executable file\n";
    return false;
  }
  execBuffer = std::move(execBufferMaybe.get());

  // Parse object file containing executable
  Expected<std::unique_ptr<object::Binary>> execBinaryMaybe =
      object::createBinary(*execBuffer);
  if (execBinaryMaybe.takeError()) {
    errs() << "Error: Unable to parse object file "
              "containing executable\n";
    return false;
  }
  execBinary = std::move(execBinaryMaybe.get());
  if (!isa<object::ObjectFile>(execBinary)) {
    errs() << "Error: Unexpected object file type "
              "containing executable\n";
    return false;
  }

  if (execBinary->isELF()) {
    // Used by `findDynamicFunctionNameELF`
    // Adding additional targets here may work, but currently untested
    LLVMInitializeX86TargetInfo();
    LLVMInitializeX86TargetMC();
  }

  return true;
}

} // namespace

extern "C" {

QBDIPRELOAD_INIT;

int qbdipreload_on_start(void *main) {
  mainFunc = (QBDI::rword)main;
  // Leave this as "not handled" so the default handler will still run,
  // which hooks the `main` function.
  return QBDIPRELOAD_NOT_HANDLED;
}

int qbdipreload_on_premain(void *gprCtx, void *fpuCtx) {
  return QBDIPRELOAD_NOT_HANDLED;
}

int qbdipreload_on_main(int argc, char **argv) {
  if (std::getenv("CON_TRACE_APPEND"))
    append = true;
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
  if (!loadExecutable(execPath))
    return QBDIPRELOAD_ERR_STARTUP_FAILED;
  if (!loadDWARFDebugInfo(execPath + ".dwarf"))
    return QBDIPRELOAD_ERR_STARTUP_FAILED;

  // Leave this as "not handled" so the default handler will still run,
  // which starts up the QBDI instrumentation VM.
  return QBDIPRELOAD_NOT_HANDLED;
}

int qbdipreload_on_run(QBDI::VMInstanceRef vm, QBDI::rword start,
                       QBDI::rword stop) {
  // Use process IDs in trace file name in case of process forks
  auto pid = getpid();
  auto ppid = getppid();
  auto traceBase = verbose ? "trace-verbose" : "trace";
  auto traceName = formatv("{0}-{1}-{2}", traceBase, ppid, pid).str();
  std::error_code error;
  trace = std::make_unique<raw_fd_ostream>(
      traceName, error, append ? sys::fs::OF_Append : sys::fs::OF_None);
  if (error) {
    errs() << "Error: Unable to open trace file\n";
    return QBDIPRELOAD_ERR_STARTUP_FAILED;
  }

  // Save current module memory maps to check for moves into external code
  const auto processMemoryMaps = QBDI::getCurrentProcessMaps();
  std::string currentModuleName;
  for (const auto &mm : processMemoryMaps) {
    if (!mm.range.contains(mainFunc))
      continue;
    if (mm.name.empty()) {
      errs() << "Error: Current module has no name\n";
      return QBDIPRELOAD_ERR_STARTUP_FAILED;
    }
    currentModuleName = mm.name;
  }
  for (const auto &mm : processMemoryMaps) {
    if (!(mm.permission & QBDI::PF_EXEC))
      continue;
    if (mm.name != currentModuleName)
      continue;
    currentModuleMemoryMaps.push_back(mm);
  }

  // Reset instrumented range to include only the main program module.
  // This ensures we avoid the dynamic loader and libraries.
  // The dynamic loader seems to confuse stack depth detection.
  vm->removeAllInstrumentedRanges();
  vm->addInstrumentedModuleFromAddr(mainFunc);

  vm->addCodeCB(QBDI::PREINST, beforeInstruction, nullptr);
  vm->addCodeCB(QBDI::POSTINST, afterInstruction, nullptr);

  // Initialise stack with frame for `main`
  getInlinedChain(mainFunc, inlinedChain);
  assert(!inlinedChain.empty());
  pushStackFrame(inlinedChain.back());

  // Log initial instruction
  const auto lineInfo = dwarfCtx->getLineInfoForAddress(
      {mainFunc}, {DILineInfoSpecifier::FileLineInfoKind::RawValue,
                   DILineInfoSpecifier::FunctionNameKind::ShortName});
  printEventFromLineInfo(lineInfo, EventType::CallTo, EventSource::Stack,
                         mainFunc);
  stackDepthChanged = false;

  vm->run(start, stop);

  return QBDIPRELOAD_NO_ERROR;
}

int qbdipreload_on_exit(int status) { return QBDIPRELOAD_NO_ERROR; }
}
