#ifndef MACHO_H
#define MACHO_H

#include <cstdint>
#include <optional>

#include "llvm/ADT/StringRef.h"
#include "llvm/BinaryFormat/MachO.h"
#include "llvm/Object/MachO.h"

// Borrowed from LLVM's `MachODump.cpp`
std::optional<llvm::StringRef>
findIndirectSymbolNameMacho(uint64_t address,
                            const llvm::object::MachOObjectFile &objFile);

#endif // MACHO_H
