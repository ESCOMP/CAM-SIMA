set(IO_READER_SOURCE pio_reader.F90 pio_reader_sub.F90 ../ncar_ccpp/phys_utils/ccpp_io_reader.F90 ../../../share/src/shr_kind_mod.F90)

# io_reader is not integrated into the CMake build of any top level
# project yet and this CMake is for testing purposes only.
# Making a change to this project's CMake will not impact the build of
# a parent project at this time.
add_library(io_reader ${IO_READER_SOURCE})
target_include_directories(io_reader PUBLIC ${CMAKE_BINARY_DIR}/utils)
target_link_libraries(io_reader PRIVATE PIO::PIO_Fortran cam_pio_utils)
