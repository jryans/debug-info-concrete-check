#include <cstdlib>
#include <memory>
#include <set>
#include <string>
#include <utility>

#include "llvm/ADT/StringRef.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
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
      // outs() << name << "\n";
    }
  }
  outs() << "Count: " << onlyReadMemoryFunctions.size() << "\n";

  return EXIT_SUCCESS;
}
