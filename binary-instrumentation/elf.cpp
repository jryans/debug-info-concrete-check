#include "elf.h"

#include <cstdint>
#include <map>
#include <optional>

#include "llvm/ADT/StringRef.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/Object/ELFObjectFile.h"
#include "llvm/Object/ObjectFile.h"

using namespace llvm;
using namespace llvm::object;

std::optional<StringRef>
findDynamicFunctionNameELF(uint64_t address, const ELFObjectFileBase &objFile) {
  // Cached here via `static` as there's only one object file currently
  static bool initDone = false;
  static std::map<uint64_t, StringRef> pltAddressToSymbolName;

  if (!initDone) {
    const auto pltEntries = objFile.getPltEntries();
    for (const auto pltEntry : pltEntries) {
      if (pltEntry.Symbol) {
        SymbolRef symbol(*pltEntry.Symbol, &objFile);
        if (Expected<StringRef> nameOrErr = symbol.getName()) {
          if (!nameOrErr->empty())
            pltAddressToSymbolName[pltEntry.Address] = *nameOrErr;
        }
      }
    }
    initDone = true;
  }

  const auto symbolNameLookup = pltAddressToSymbolName.find(address);
  if (symbolNameLookup != pltAddressToSymbolName.end())
    return symbolNameLookup->second;

  return std::nullopt;
}
