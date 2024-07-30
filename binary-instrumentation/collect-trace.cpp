#include <memory>
#include <system_error>

#include "llvm/Support/Format.h"
#include "llvm/Support/raw_ostream.h"

#include "QBDIPreload.h"

static std::unique_ptr<llvm::raw_fd_ostream> trace;

static QBDI::VMAction onInstruction(QBDI::VMInstanceRef vm,
                                    QBDI::GPRState *gprState,
                                    QBDI::FPRState *fprState, void *data) {
  const QBDI::InstAnalysis *instAnalysis = vm->getInstAnalysis();

  // 16 hex digits for 64-bit address plus 2 character prefix
  *trace << llvm::format_hex(instAnalysis->address, 18) << ": "
         << instAnalysis->disassembly << "\n";

  return QBDI::CONTINUE;
}

extern "C" {

QBDIPRELOAD_INIT;

int qbdipreload_on_start(void *main) { return QBDIPRELOAD_NOT_HANDLED; }

int qbdipreload_on_premain(void *gprCtx, void *fpuCtx) {
  return QBDIPRELOAD_NOT_HANDLED;
}

int qbdipreload_on_main(int argc, char **argv) {
  return QBDIPRELOAD_NOT_HANDLED;
}

int qbdipreload_on_run(QBDI::VMInstanceRef vm, QBDI::rword start,
                       QBDI::rword stop) {
  std::error_code error;
  trace = std::make_unique<llvm::raw_fd_ostream>("concrete-trace/trace", error);
  if (error)
    return QBDIPRELOAD_ERR_STARTUP_FAILED;

  vm->addCodeCB(QBDI::PREINST, onInstruction, nullptr);
  vm->run(start, stop);

  return QBDIPRELOAD_NO_ERROR;
}

int qbdipreload_on_exit(int status) { return QBDIPRELOAD_NO_ERROR; }
}
