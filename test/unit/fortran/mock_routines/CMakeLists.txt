add_library(cam_pio_utils cam_pio_utils.F90)
target_link_libraries(cam_pio_utils PUBLIC MPI::MPI_Fortran
                                           PIO::PIO_Fortran
                                           PIO::PIO_C
                                           NetCDF::NetCDF_C
                                           NetCDF::NetCDF_Fortran)
