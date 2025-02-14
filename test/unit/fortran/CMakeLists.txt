cmake_minimum_required(VERSION 3.18)

project(CAM-SIMA-core-utils LANGUAGES Fortran)

option(CAM_SIMA_ENABLE_TESTS "Run pFUnit unit tests" OFF)
option(CAM_SIMA_ENABLE_CODE_COVERAGE "Run code coverage tool" OFF)

if(NOT CAM-SIMA-core-utils_IS_TOP_LEVEL)
  message(WARNING "CAM-SIMA-core-utils is not integrated into the CMake build of any top level "
                  "project yet and this CMake is for testing purposes only.  "
                  "Making a change to this project's CMake will not impact the build of "
                  "a parent project at this time.")
endif()

# Overwrite the init flags chosen by CMake
if(${CMAKE_Fortran_COMPILER_ID} MATCHES "GNU")
  add_compile_options("-ffree-line-length-none")
endif()

if(CAM_SIMA_ENABLE_CODE_COVERAGE)
  add_compile_options(-O0 --coverage)
  add_link_options(--coverage)
endif()

add_subdirectory(${CMAKE_SOURCE_DIR}/../../../src/core_utils ${CMAKE_BINARY_DIR}/src)

if(CAM_SIMA_ENABLE_TESTS OR CAM_SIMA_ENABLE_CODE_COVERAGE)
set(CMAKE_BUILD_TYPE Debug)
  find_package(PFUNIT REQUIRED)
  enable_testing()
  add_subdirectory(src/core_utils)
endif()
