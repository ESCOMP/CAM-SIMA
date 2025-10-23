find_package(PFUNIT REQUIRED)

add_pfunit_ctest(test_dyn_mpas_procedures
    TEST_SOURCES
        test_dyn_mpas_procedures.pf
    LINK_LIBRARIES
        mpas_dynamical_core_procedures
)
target_compile_options(test_dyn_mpas_procedures
    PRIVATE
        $<$<AND:$<CONFIG:Debug>,$<Fortran_COMPILER_ID:GNU>>:-fbacktrace -fcheck=all -ffpe-trap=invalid,overflow,zero -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans -std=f2018 -Wall -Wextra -Wpedantic>
        $<$<AND:$<CONFIG:Debug>,$<Fortran_COMPILER_ID:Intel>>:-check all -fp-model=precise -stand f18 -traceback -warn all>
        $<$<AND:$<CONFIG:Debug>,$<Fortran_COMPILER_ID:IntelLLVM>>:-check all -fp-model=precise -stand f18 -traceback -warn all>
)

add_pfunit_ctest(test_dyn_procedures
    TEST_SOURCES
        test_dyn_procedures.pf
    LINK_LIBRARIES
        mpas_dynamical_core_procedures
)
target_compile_options(test_dyn_procedures
    PRIVATE
        $<$<AND:$<CONFIG:Debug>,$<Fortran_COMPILER_ID:GNU>>:-fbacktrace -fcheck=all -ffpe-trap=invalid,overflow,zero -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans -std=f2018 -Wall -Wextra -Wpedantic>
        $<$<AND:$<CONFIG:Debug>,$<Fortran_COMPILER_ID:Intel>>:-check all -fp-model=precise -stand f18 -traceback -warn all>
        $<$<AND:$<CONFIG:Debug>,$<Fortran_COMPILER_ID:IntelLLVM>>:-check all -fp-model=precise -stand f18 -traceback -warn all>
)
