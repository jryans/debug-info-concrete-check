cmake_minimum_required(VERSION 3.25)

project(binary-instrumentation)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../cmake")

# Assertions

# With non-debug builds, CMake automatically defines NDEBUG.
# We want to preserve assertions in all builds, so we explicitly undefine it.
if(NOT CMAKE_BUILD_TYPE STREQUAL "Debug")
  add_compile_options(-UNDEBUG)
endif()

# collect-trace-preload

find_package(QBDI REQUIRED)
find_package(QBDIPreload REQUIRED)

add_library(collect-trace-preload SHARED
  collect-trace.cpp
  elf.cpp
  macho.cpp
)

target_link_libraries(collect-trace-preload PRIVATE
  QBDI::QBDI
  QBDIPreload::QBDIPreload
)

set_target_properties(collect-trace-preload PROPERTIES
  CXX_STANDARD 17
  CXX_STANDARD_REQUIRED ON
)

# LLVM

find_package(ZLIB REQUIRED)
find_package(zstd REQUIRED)

include(FindLLVM)

set(LLVM_VARS
  LLVM_PACKAGE_VERSION
  LLVM_VERSION_MAJOR
  LLVM_VERSION_MINOR
  LLVM_VERSION_PATCH
  LLVM_ENABLE_EH
  LLVM_ENABLE_RTTI
  LLVM_DEFINITIONS
  LLVM_INCLUDE_DIRS
  LLVM_LIBRARY_DIRS
  TARGET_TRIPLE
)

foreach(vname ${LLVM_VARS})
  message(STATUS "${vname}: \"${${vname}}\"")
  if (NOT (DEFINED "${vname}"))
    message(FATAL_ERROR "${vname} was not defined")
  endif()
endforeach()

target_include_directories(collect-trace-preload PRIVATE
  ${LLVM_INCLUDE_DIRS}
)

set(LLVM_COMPONENTS
  DebugInfoDWARF
  Object
  Support
  X86
)
get_llvm_libs(LLVM_LIBS ${LLVM_COMPONENTS})
target_link_libraries(collect-trace-preload PRIVATE
  ${ZLIB_LIBRARY}
  zstd::libzstd_shared
  ${LLVM_LIBS}
)

# LLVM is usually built without exceptions and RTTI, so we need to do the same
if(NOT LLVM_ENABLE_EH)
  target_compile_options(collect-trace-preload PRIVATE
    -fno-exceptions
  )
endif()
if(NOT LLVM_ENABLE_RTTI)
  target_compile_options(collect-trace-preload PRIVATE
    -fno-rtti
  )
endif()
