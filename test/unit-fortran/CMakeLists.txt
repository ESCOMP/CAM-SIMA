cmake_minimum_required(VERSION 3.18)

project(CAM-SIMA-core-utils LANGUAGES Fortran)

option(CAM_SIMA_ENABLE_TESTS "Run pFUnit unit tests" OFF)
option(CAM_SIMA_ENABLE_CODE_COVERAGE "Run code coverage tool" OFF)

if(CAM_SIMA_ENABLE_CODE_COVERAGE)
  add_compile_options(-O0 --coverage)
  add_link_options(--coverage)
endif()

add_subdirectory(${CMAKE_SOURCE_DIR}/../../src/core_utils ${CMAKE_BINARY_DIR}/core/)

if(CAM_SIMA_ENABLE_TESTS OR CAM_SIMA_ENABLE_CODE_COVERAGE)
  set(CMAKE_BUILD_TYPE Debug)
  find_package(PFUNIT REQUIRED)
  enable_testing()
  add_subdirectory(src/core_utils)
endif()
