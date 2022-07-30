CMAKE_MINIMUM_REQUIRED(VERSION 3.11)
PROJECT(TestUtils)
ENABLE_LANGUAGE(Fortran)

include(CMakeForceCompiler)

find_package(MPI REQUIRED)
add_definitions(${MPI_Fortran_COMPILE_FLAGS})
include_directories(${MPI_Fortran_INCLUDE_PATH})
link_directories(${MPI_Fortran_LIBRARIES})

#-----------------------------------------------------------------------------
#
# Paths should be relative to CMAKE_SOURCE_DIR (this file's directory)
#
#-----------------------------------------------------------------------------
GET_FILENAME_COMPONENT(TEST_PATH ${CMAKE_CURRENT_SOURCE_DIR} DIRECTORY)
SET(UTILS_PATH ${TEST_PATH}/include)
GET_FILENAME_COMPONENT(ROOT_PATH ${TEST_PATH} DIRECTORY)
SET(SRC_PATH ${ROOT_PATH}/src)
SET(HIST_PATH ${SRC_PATH}/history)
# Find CIME directory
if (EXISTS "${ROOT_PATH}/cime")
  SET(CIME_PATH ${ROOT_PATH}/cime)
else(EXISTS "${ROOT_PATH}/cime")
  GET_FILENAME_COMPONENT(_components ${ROOT_PATH} DIRECTORY)
  GET_FILENAME_COMPONENT(_toplev ${_components} DIRECTORY)
  SET(CIME_PATH ${_toplev}/cime)
endif(EXISTS "${ROOT_PATH}/cime")
# Test copies of CAM and CIME utility files
LIST(APPEND SOURCE_FILES "${UTILS_PATH}/shr_string_mod.F90")
LIST(APPEND SOURCE_FILES "${UTILS_PATH}/shr_infnan_mod.F90")
LIST(APPEND SOURCE_FILES "${UTILS_PATH}/shr_assert_mod.F90")
# Regular CAM and CIME utility files
LIST(APPEND SOURCE_FILES "${CIME_PATH}/src/share/util/shr_kind_mod.F90")
LIST(APPEND SOURCE_FILES "${CIME_PATH}/src/share/util/shr_mpi_mod.F90")
LIST(APPEND SOURCE_FILES "${CIME_PATH}/src/share/util/shr_abort_mod.F90")
LIST(APPEND SOURCE_FILES "${CIME_PATH}/src/share/util/shr_sys_mod.F90")
LIST(APPEND SOURCE_FILES "${CIME_PATH}/src/share/util/shr_timer_mod.F90")
LIST(APPEND SOURCE_FILES "${CIME_PATH}/src/share/util/shr_log_mod.F90")
LIST(APPEND SOURCE_FILES "${CIME_PATH}/src/share/util/shr_strconvert_mod.F90")
LIST(APPEND SOURCE_FILES "${SRC_PATH}/utils/string_utils.F90")
# Utility test modules
LIST(APPEND SOURCE_FILES "${CMAKE_CURRENT_SOURCE_DIR}/string_utils_tests.F90")
# TEST_EXE.F90 is the name of the program source file
SET(TEST_EXE "test_utils")
ADD_EXECUTABLE(${TEST_EXE} ${TEST_EXE}.F90)

#-----------------------------------------------------------------------------
##############################################################################
#
# End of project-specific input
#
##############################################################################
#-----------------------------------------------------------------------------

# Use rpaths on MacOSX
set(CMAKE_MACOSX_RPATH 1)

#-----------------------------------------------------------------------------
# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    #message(STATUS "Setting build type to 'Debug' as none was specified.")
    #set(CMAKE_BUILD_TYPE Debug CACHE STRING "Choose the type of build." FORCE)
    message(STATUS "Setting build type to 'Release' as none was specified.")
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)

    # Set the possible values of build type for cmake-gui
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
                 "MinSizeRel" "RelWithDebInfo")
endif()

ADD_COMPILE_OPTIONS(-O0)

if (${CMAKE_Fortran_COMPILER_ID} MATCHES "GNU")
#  gfortran
#  MESSAGE("gfortran being used.")
  ADD_COMPILE_OPTIONS(-fcheck=all)
  ADD_COMPILE_OPTIONS(-fbacktrace)
  ADD_COMPILE_OPTIONS(-ffpe-trap=zero)
  ADD_COMPILE_OPTIONS(-finit-real=nan)
  ADD_COMPILE_OPTIONS(-ggdb)
  ADD_COMPILE_OPTIONS(-ffree-line-length-none)
  ADD_COMPILE_OPTIONS(-cpp)
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DHAVE_IEEE_ARITHMETIC")
elseif (${CMAKE_Fortran_COMPILER_ID} MATCHES "Intel")
#  ifort
#  MESSAGE("ifort being used.")
  #ADD_COMPILE_OPTIONS(-check all)
  ADD_COMPILE_OPTIONS(-fpe0)
  ADD_COMPILE_OPTIONS(-warn)
  ADD_COMPILE_OPTIONS(-traceback)
  ADD_COMPILE_OPTIONS(-debug extended)
  ADD_COMPILE_OPTIONS(-fpp)
elseif (${CMAKE_Fortran_COMPILER_ID} MATCHES "PGI")
#  pgf90
#  MESSAGE("pgf90 being used.")
  ADD_COMPILE_OPTIONS(-g)
  ADD_COMPILE_OPTIONS(-Mipa=noconst)
  ADD_COMPILE_OPTIONS(-traceback)
  ADD_COMPILE_OPTIONS(-Mfree)
  ADD_COMPILE_OPTIONS(-Mfptrap)
  ADD_COMPILE_OPTIONS(-Mpreprocess)
else (${CMAKE_Fortran_COMPILER_ID} MATCHES "GNU")
  message (FATAL_ERROR "This program has only been compiled with gfortran, pgf90 and ifort. If another compiler is needed, the appropriate flags must be added in ${CMAKE_SOURCE_DIR}/CMakeLists.txt")
endif (${CMAKE_Fortran_COMPILER_ID} MATCHES "GNU")
set (CMAKE_Fortran_FLAGS
  "${CMAKE_Fortran_FLAGS} -I${CIME_PATH}/src/share/include")

#-----------------------------------------------------------------------------
# Set OpenMP flags for C/C++/Fortran
if (OPENMP)
  include(detect_openmp)
  detect_openmp()
  set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
  message(STATUS "Enable OpenMP support for C/C++/Fortran compiler")
else(OPENMP)
  message (STATUS "Disable OpenMP support for C/C++/Fortran compiler")
endif()
TARGET_SOURCES(${TEST_EXE} PUBLIC ${SOURCE_FILES})
TARGET_LINK_LIBRARIES(${TEST_EXE} ${MPI_Fortran_LIBRARIES})

set_target_properties(${TEST_EXE} PROPERTIES
                                  COMPILE_FLAGS "${CMAKE_Fortran_FLAGS}"
                                  LINK_FLAGS "${CMAKE_Fortran_FLAGS}")
