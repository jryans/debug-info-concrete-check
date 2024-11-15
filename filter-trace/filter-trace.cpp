#include <cstdlib>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <system_error>
#include <utility>

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorOr.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

namespace {

cl::OptionCategory filterCategory("Filter options");

cl::opt<std::string> traceFile(cl::Positional, cl::Required,
                               cl::desc("<trace>"), cl::cat(filterCategory));

cl::opt<std::string>
    moduleFile(cl::Positional, cl::Required,
               cl::desc("<module (.bc/.ll) with function attrs>"),
               cl::cat(filterCategory));

cl::opt<bool> listFunctions(
    "list-functions",
    cl::desc("Print all functions that would be filtered and exit"),
    cl::cat(filterCategory));

cl::opt<bool> includeFiltered(
    "include-filtered",
    cl::desc("Include (annotated) lines that (would be) filtered"),
    cl::cat(filterCategory));

} // namespace

int main(int argc, char **argv) {
  InitLLVM x(argc, argv);

  cl::HideUnrelatedOptions(filterCategory);
  cl::ParseCommandLineOptions(argc, argv, "Filter trace\n");

  // Load LLVM module
  LLVMContext context;
  SMDiagnostic error;
  std::unique_ptr<Module> module = parseIRFile(moduleFile, error, context);
  if (!module) {
    outs() << "Unable to load module file `" << moduleFile
           << "`: " << error.getMessage() << "\n";
    return EXIT_FAILURE;
  }

  // Gather any functions that (at most) only read memory
  // TODO: Track file name as well
  std::set<std::string> onlyReadMemoryFunctions;
  for (const auto &f : module->functions()) {
    if (f.onlyReadsMemory()) {
      StringRef name = f.getName();
      // Use subprogram name if it exists to deduplicate macro instantiations
      const auto *sp = f.getSubprogram();
      if (sp)
        name = sp->getName();
      onlyReadMemoryFunctions.insert(name.str());
    }
  }

  if (listFunctions) {
    for (const auto &functionName : onlyReadMemoryFunctions)
      outs() << functionName << "\n";
    return EXIT_SUCCESS;
  }

  // Read trace into memory
  ErrorOr<std::unique_ptr<MemoryBuffer>> traceOrErr =
      MemoryBuffer::getFileOrSTDIN(traceFile, /*isText=*/true);
  if (std::error_code error = traceOrErr.getError()) {
    outs() << "Unable to load trace file `" << traceFile
           << "`: " << error.message() << "\n";
    return EXIT_FAILURE;
  }

  // Filter trace line by line
  SmallVector<StringRef> traceQueue;
  std::optional<StringRef> filteringUntil;
  const auto &trace = *traceOrErr;
  for (const auto line : split(trace->getBuffer(), "\n")) {
    // If we're filtering, look for matching "return from"
    if (filteringUntil) {
      if (line.contains("RF:") && line.contains(*filteringUntil)) {
        // Matching "return from", filtering ends here
        filteringUntil = std::nullopt;
      }
      // Filtering active, skip this line
      if (includeFiltered)
        outs() << "FILT: " << line << "\n";
      continue;
    }

    // Buffer "call from" events until we check the following "call to"
    if (line.contains("CF:")) {
      traceQueue.push_back(line);
      continue;
    }
    if (line.contains("CT:")) {
      const auto trimmedLine = line.trim();
      const auto functionName = trimmedLine.split(" ").second.split(" ").first;
      if (onlyReadMemoryFunctions.count(functionName.str())) {
        // Save function name, filter all events until matching "return from"
        filteringUntil = functionName;
        // Filtering active, skip queue lines
        if (includeFiltered) {
          for (const auto queuedLine : traceQueue) {
            outs() << "FILT: " << queuedLine << "\n";
          }
        }
        traceQueue.clear();
        // Filtering active, skip this line
        if (includeFiltered)
          outs() << "FILT: " << line << "\n";
        continue;
      }
    }

    // Print queued events if any
    if (!traceQueue.empty()) {
      for (const auto queuedLine : traceQueue) {
        outs() << queuedLine << "\n";
      }
      traceQueue.clear();
    }

    // Current line is safe
    outs() << line << "\n";
  }

  return EXIT_SUCCESS;
}
