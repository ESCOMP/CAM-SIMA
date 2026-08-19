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
