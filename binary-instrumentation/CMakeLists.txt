cmake_minimum_required(VERSION 3.25)

project(debug-info-concrete-check)

find_package(QBDI REQUIRED)
find_package(QBDIPreload REQUIRED)

add_library(collect-trace-preload SHARED
  collect-trace.cpp
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

include(${CMAKE_SOURCE_DIR}/cmake/find_llvm.cmake)

set(LLVM_VARS
  LLVM_PACKAGE_VERSION
  LLVM_VERSION_MAJOR
  LLVM_VERSION_MINOR
  LLVM_VERSION_PATCH
  LLVM_DEFINITIONS
  LLVM_INCLUDE_DIRS
  LLVM_LIBRARY_DIRS
  TARGET_TRIPLE
)

foreach (vname ${LLVM_VARS})
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
)
get_llvm_libs(LLVM_LIBS ${LLVM_COMPONENTS})
target_link_libraries(collect-trace-preload PRIVATE
  ${ZLIB_LIBRARY}
  zstd::libzstd_shared
  ${LLVM_LIBS}
)

# LLVM is usually built without RTTI, so we need to do the same
target_compile_options(collect-trace-preload PRIVATE
  -fno-rtti
)
