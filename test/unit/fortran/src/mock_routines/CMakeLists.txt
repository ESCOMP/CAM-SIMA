add_pfunit_ctest(pio_tests
                 TEST_SOURCES test_pio_read.pf test_pio_reader.pf
                 LINK_LIBRARIES cam_pio_utils
                                MPI::MPI_Fortran
                                PIO::PIO_Fortran
                                PIO::PIO_C
                                NetCDF::NetCDF_C
                                NetCDF::NetCDF_Fortran)
target_include_directories(pio_tests PRIVATE ${CMAKE_BINARY_DIR}/utils)
