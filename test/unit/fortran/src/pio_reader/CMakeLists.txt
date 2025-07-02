add_pfunit_ctest(pio_tests
                 TEST_SOURCES test_pio_reader.pf
                 LINK_LIBRARIES io_reader
                                cam_pio_utils
                                ccpp_framework
                                MPI::MPI_Fortran
                                PIO::PIO_Fortran
                                PIO::PIO_C
                                NetCDF::NetCDF_C
                                NetCDF::NetCDF_Fortran)
target_include_directories(pio_tests PRIVATE ${CMAKE_BINARY_DIR}/utils
                                             ${CMAKE_BINARY_DIR}/ccpp_framework_stub
                                             ${CMAKE_BINARY_DIR}/src/physics/utils)
