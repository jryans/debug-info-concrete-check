cmake_minimum_required(VERSION 3.25)

project(debug-info-concrete-check)

find_package(QBDI REQUIRED)
find_package(QBDIPreload REQUIRED)

add_library(
  collect-trace-preload SHARED
  collect-trace.cpp
)

target_link_libraries(
  collect-trace-preload
  QBDI::QBDI_static
  QBDIPreload::QBDIPreload
)

set_target_properties(
  collect-trace-preload
  PROPERTIES
  CXX_STANDARD 17
  CXX_STANDARD_REQUIRED ON
)
