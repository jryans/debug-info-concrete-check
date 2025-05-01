#ifndef ELF_H
#define ELF_H

#include <cstdint>
#include <optional>

#include "llvm/ADT/StringRef.h"
#include "llvm/Object/ELFObjectFile.h"

std::optional<llvm::StringRef>
findDynamicFunctionNameELF(uint64_t address,
                           const llvm::object::ELFObjectFileBase &objFile);

#endif // ELF_H
