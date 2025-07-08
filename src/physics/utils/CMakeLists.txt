set(IO_READER_SOURCE pio_reader.F90
	             pio_reader_sub.F90
		     ${CMAKE_CURRENT_SOURCE_DIR}/../ncar_ccpp/phys_utils/ccpp_io_reader.F90
		     ${CMAKE_CURRENT_SOURCE_DIR}/../../../share/src/shr_kind_mod.F90)

# io_reader is not integrated into the CMake build of any top level
# project yet and this CMake is for testing purposes only.
# Making a change to this project's CMake will not impact the build of
# a parent project at this time.
add_library(io_reader ${IO_READER_SOURCE})
target_link_libraries(io_reader PUBLIC
                      MPI::MPI_Fortran
                      MPI::MPI_C
                      NetCDF::NetCDF_C
                      NetCDF::NetCDF_Fortran
                      PIO::PIO_Fortran
                      PIO::PIO_C
                      cam_pio_utils
                      ccpp_framework)
target_include_directories(io_reader PRIVATE ${CMAKE_BINARY_DIR}/ccpp_framework_stub
                                             ${CMAKE_BINARY_DIR}/mock_routines)
