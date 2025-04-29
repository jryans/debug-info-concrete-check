#include "macho.h"

#include <cstdint>
#include <optional>

#include "llvm/ADT/StringRef.h"
#include "llvm/BinaryFormat/MachO.h"
#include "llvm/Object/MachO.h"

using namespace llvm;
using namespace llvm::object;

// Borrowed from LLVM's `MachODump.cpp`
std::optional<StringRef>
findDynamicFunctionNameMachO(uint64_t address, const MachOObjectFile &objFile) {
  MachO::dysymtab_command dysymtab = objFile.getDysymtabLoadCommand();
  MachO::symtab_command symtab = objFile.getSymtabLoadCommand();
  for (const auto &load : objFile.load_commands()) {
    if (load.C.cmd == MachO::LC_SEGMENT_64) {
      MachO::segment_command_64 seg = objFile.getSegment64LoadCommand(load);
      for (unsigned J = 0; J < seg.nsects; ++J) {
        MachO::section_64 sec = objFile.getSection64(load, J);
        uint32_t sectionType = sec.flags & MachO::SECTION_TYPE;
        if ((sectionType == MachO::S_NON_LAZY_SYMBOL_POINTERS ||
             sectionType == MachO::S_LAZY_SYMBOL_POINTERS ||
             sectionType == MachO::S_LAZY_DYLIB_SYMBOL_POINTERS ||
             sectionType == MachO::S_THREAD_LOCAL_VARIABLE_POINTERS ||
             sectionType == MachO::S_SYMBOL_STUBS) &&
            address >= sec.addr && address < sec.addr + sec.size) {
          uint32_t stride;
          if (sectionType == MachO::S_SYMBOL_STUBS)
            stride = sec.reserved2;
          else
            stride = 8;
          if (stride == 0)
            return std::nullopt;
          uint32_t index = sec.reserved1 + (address - sec.addr) / stride;
          if (index < dysymtab.nindirectsyms) {
            uint32_t indirectSymbol =
                objFile.getIndirectSymbolTableEntry(dysymtab, index);
            if (indirectSymbol < symtab.nsyms) {
              symbol_iterator sym = objFile.getSymbolByIndex(indirectSymbol);
              Expected<StringRef> name = sym->getName();
              if (name)
                return *name;
              return std::nullopt;
            }
          }
        }
      }
    } else if (load.C.cmd == MachO::LC_SEGMENT) {
      MachO::segment_command seg = objFile.getSegmentLoadCommand(load);
      for (unsigned J = 0; J < seg.nsects; ++J) {
        MachO::section sec = objFile.getSection(load, J);
        uint32_t sectionType = sec.flags & MachO::SECTION_TYPE;
        if ((sectionType == MachO::S_NON_LAZY_SYMBOL_POINTERS ||
             sectionType == MachO::S_LAZY_SYMBOL_POINTERS ||
             sectionType == MachO::S_LAZY_DYLIB_SYMBOL_POINTERS ||
             sectionType == MachO::S_THREAD_LOCAL_VARIABLE_POINTERS ||
             sectionType == MachO::S_SYMBOL_STUBS) &&
            address >= sec.addr && address < sec.addr + sec.size) {
          uint32_t stride;
          if (sectionType == MachO::S_SYMBOL_STUBS)
            stride = sec.reserved2;
          else
            stride = 4;
          if (stride == 0)
            return std::nullopt;
          uint32_t index = sec.reserved1 + (address - sec.addr) / stride;
          if (index < dysymtab.nindirectsyms) {
            uint32_t indirectSymbol =
                objFile.getIndirectSymbolTableEntry(dysymtab, index);
            if (indirectSymbol < symtab.nsyms) {
              symbol_iterator sym = objFile.getSymbolByIndex(indirectSymbol);
              Expected<StringRef> name = sym->getName();
              if (name)
                return *name;
              return std::nullopt;
            }
          }
        }
      }
    }
  }
  return std::nullopt;
}
