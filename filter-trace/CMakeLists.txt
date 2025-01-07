cmake_minimum_required(VERSION 3.25)

project(filter-trace)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../cmake")

# Assertions

# With non-debug builds, CMake automatically defines NDEBUG.
# We want to preserve assertions in all builds, so we explicitly undefine it.
if(NOT CMAKE_BUILD_TYPE STREQUAL "Debug")
  add_compile_options(-UNDEBUG)
endif()

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

# filter-trace-inlining

add_executable(filter-trace-inlining
  filter-trace-inlining.cpp
)

set_target_properties(filter-trace-inlining PROPERTIES
  CXX_STANDARD 17
  CXX_STANDARD_REQUIRED ON
)

## LLVM for filter-trace-inlining

target_include_directories(filter-trace-inlining PRIVATE
  ${LLVM_INCLUDE_DIRS}
)

set(LLVM_COMPONENTS_INLINING
  Core
  IRReader
  Support
)
get_llvm_libs(LLVM_LIBS_INLINING ${LLVM_COMPONENTS_INLINING})
target_link_libraries(filter-trace-inlining PRIVATE
  ${ZLIB_LIBRARY}
  zstd::libzstd_shared
  ${LLVM_LIBS_INLINING}
)

# LLVM is usually built without exceptions and RTTI, so we need to do the same
if(NOT LLVM_ENABLE_EH)
  target_compile_options(filter-trace-inlining PRIVATE
    -fno-exceptions
  )
endif()
if(NOT LLVM_ENABLE_RTTI)
  target_compile_options(filter-trace-inlining PRIVATE
    -fno-rtti
  )
endif()

# filter-trace-memory-effects

add_executable(filter-trace-memory-effects
  filter-trace-memory-effects.cpp
)

set_target_properties(filter-trace-memory-effects PROPERTIES
  CXX_STANDARD 17
  CXX_STANDARD_REQUIRED ON
)

## LLVM for filter-trace-memory-effects

target_include_directories(filter-trace-memory-effects PRIVATE
  ${LLVM_INCLUDE_DIRS}
)

set(LLVM_COMPONENTS_MEMORY_EFFECTS
  Core
  IRReader
  Support
)
get_llvm_libs(LLVM_LIBS_MEMORY_EFFECTS ${LLVM_COMPONENTS_MEMORY_EFFECTS})
target_link_libraries(filter-trace-memory-effects PRIVATE
  ${ZLIB_LIBRARY}
  zstd::libzstd_shared
  ${LLVM_LIBS_MEMORY_EFFECTS}
)

# LLVM is usually built without exceptions and RTTI, so we need to do the same
if(NOT LLVM_ENABLE_EH)
  target_compile_options(filter-trace-memory-effects PRIVATE
    -fno-exceptions
  )
endif()
if(NOT LLVM_ENABLE_RTTI)
  target_compile_options(filter-trace-memory-effects PRIVATE
    -fno-rtti
  )
endif()
