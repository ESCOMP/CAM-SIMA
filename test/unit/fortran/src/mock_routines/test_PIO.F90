program test_PIO

   use pio,           only: file_desc_t
   use pio,           only: PIO_NOWRITE
   use mpi,           only: mpi_init
   use mpi,           only: mpi_finalize
   use cam_pio_utils, only: cam_pio_openfile

   implicit none

!++++++++++++++++++++++++++++
!Main testing program portion
!++++++++++++++++++++++++++++

   character(len=*), parameter :: fname = &
   '/glade/work/nusbaume/SE_projects/SIMA/CAM-SIMA/src/physics/utils/rrtmgp-data/rrtmgp-gas-sw-g112.nc'

   type(file_desc_t) :: file_handle

   integer :: ierr

   !Initialize MPI:
   call mpi_init(ierr)

   !Open file:
   call cam_pio_openfile(file_handle, fname, PIO_NOWRITE)

   !Write out info from file_handle:
   write(*,*) 'PIO File handle info:', file_handle%fh

   !Finalize MPI:
   call MPI_Finalize(ierr)

end program test_PIO
