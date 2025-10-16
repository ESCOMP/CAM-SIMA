cmake_minimum_required(VERSION 3.21)

# `mpas_dynamical_core_procedures` has not been integrated into the CMake build of any top level projects yet,
# and this CMakeLists.txt file is currently for unit testing purposes only.
# Therefore, making a change to this CMakeLists.txt file will not impact the build of a parent project at this time.
project(mpas_dynamical_core_procedures
    VERSION
        0.1.0
    DESCRIPTION
        "Standardized computational and utility procedures in MPAS dynamical core"
    LANGUAGES
        Fortran
)

add_library(mpas_dynamical_core_procedures)
target_sources(mpas_dynamical_core_procedures
    PRIVATE
        driver/dyn_mpas_procedures.F90
        dyn_procedures.F90
)
target_include_directories(mpas_dynamical_core_procedures
    INTERFACE
        ${CMAKE_CURRENT_BINARY_DIR}
)
target_compile_options(mpas_dynamical_core_procedures
    PRIVATE
        $<$<AND:$<CONFIG:Debug>,$<Fortran_COMPILER_ID:GNU>>:-fbacktrace -fcheck=all -ffpe-trap=invalid,overflow,zero -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans -std=f2018 -Wall -Wextra -Wpedantic>
        $<$<AND:$<CONFIG:Debug>,$<Fortran_COMPILER_ID:Intel>>:-check all -fp-model=precise -stand f18 -traceback -warn all>
        $<$<AND:$<CONFIG:Debug>,$<Fortran_COMPILER_ID:IntelLLVM>>:-check all -fp-model=precise -stand f18 -traceback -warn all>
)

# Enable testing only when this project is at the top level.
# Otherwise, if this project is under a parent project, respect its choice of whether to enable testing.
if(PROJECT_IS_TOP_LEVEL)
    include(CTest)
    enable_testing()
endif(PROJECT_IS_TOP_LEVEL)

if(BUILD_TESTING)
    add_subdirectory(tests/unit)
endif(BUILD_TESTING)
