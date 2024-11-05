#include <cstdlib>

#include "llvm/Support/raw_ostream.h"

using namespace llvm;

int main(int argc, char **argv) {
  if (argc != 3) {
    outs() << "Expected trace file and bitcode file\n";
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
