cmake_minimum_required(VERSION 3.18)

project(CAM-SIMA-core-utils LANGUAGES Fortran)

option(CAM_SIMA_ENABLE_TESTS "Run pFUnit unit tests" OFF)
option(CAM_SIMA_ENABLE_CODE_COVERAGE "Run code coverage tool" OFF)
option(CAM_SIMA_ENABLE_IO_TESTS "Run IO test integration" OFF)

if(NOT CAM-SIMA-core-utils_IS_TOP_LEVEL)
  message(WARNING "CAM-SIMA-core-utils is not integrated into the CMake build of any top level "
                  "project yet and this CMake is for testing purposes only.  "
                  "Making a change to this project's CMake will not impact the build of "
                  "a parent project at this time.")
endif()

# Overwrite the init flags chosen by CMake
if(${CMAKE_Fortran_COMPILER_ID} MATCHES "GNU")
  add_compile_options(-ffree-line-length-none -std=f2018)
endif()

if(CAM_SIMA_ENABLE_CODE_COVERAGE)
  add_compile_options(-O0 --coverage)
  add_link_options(--coverage)
endif()

add_subdirectory(${CMAKE_SOURCE_DIR}/../../../src/core_utils ${CMAKE_BINARY_DIR}/src)

if(CAM_SIMA_ENABLE_TESTS OR CAM_SIMA_ENABLE_CODE_COVERAGE OR CAM_SIMA_ENABLE_IO_TESTS)
  set(CMAKE_BUILD_TYPE Debug)
  find_package(PFUNIT REQUIRED)
  enable_testing()

  if(CAM_SIMA_ENABLE_TESTS)
    add_subdirectory(src/core_utils)
  endif()
  if(CAM_SIMA_ENABLE_IO_TESTS)
    find_package(MPI REQUIRED COMPONENTS Fortran)
    list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
    find_package(NetCDF REQUIRED COMPONENTS Fortran)
    find_package(PIO REQUIRED COMPONENTS Fortran C)
    
    # add_subdirectory(${CMAKE_SOURCE_DIR}/../../../src/physics/utils ${CMAKE_BINARY_DIR}/phys/utils)
    # target_include_directories(io_reader PRIVATE include)

    add_subdirectory(utils)

    add_subdirectory(src/mock_routines)
  endif()
endif()
