cmake_minimum_required(VERSION 3.21)

project(
  cam-sima-tests
  VERSION 0.0.0
  LANGUAGES Fortran
)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH};${CMAKE_CURRENT_LIST_DIR}/cmake)
set(CMAKE_USER_MAKE_RULES_OVERRIDE ${CMAKE_MODULE_PATH}/SetDefaults.cmake)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})

option(CCPP_ENABLE_MEMCHECK "Enable memory checks in tests" OFF)

set(INSTALL_GTEST OFF CACHE BOOL "" FORCE)
set(BUILD_GMOCK OFF CACHE BOOL "" FORCE)

set(CAM_SIMA_SRC_PATH ${CMAKE_SOURCE_DIR}/../src)
set(CAM_SIMA_TEST_PATH ${CMAKE_SOURCE_DIR})

include(TestUtils)
include(CTest)
enable_testing()

add_subdirectory(unit)