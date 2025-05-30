!This file is a "mock" version of "src/utils/cam_pio_utils.F90"
!which can provide certain public procedures of that module
!without needing to build the entire dependency tree
!that the production version currently needs.

module cam_pio_utils

   use pio, only: iosystem_desc_t

   implicit none
   private

   !Publicly-available routines:

   public :: cam_pio_openfile ! Open an existing NetCDF file

   !Private variables:
   type(iosystem_desc_t):: pio_system

   !Logical to check whether or not PIO has been initialized:
   logical :: pio_initialized = .false.

!+++++++++++++++++++++++++++++++
contains
!+++++++++++++++++++++++++++++++

   !===========================================================================
   subroutine cam_pio_openfile(file, fname, mode, log_info)
      use pio, only: pio_openfile, file_desc_t, pio_nowrite
      use pio, only: pio_noerr, pio_iotask_rank
      use pio, only: pio_iotype_netcdf !Always assume serial NetCDF

      !Subroutine input/output arguments:
      type(file_desc_t), intent(inout), target :: file
      character(len=*), intent(in) :: fname
      integer, intent(in) :: mode
      logical, optional, intent(in) :: log_info ! if .false. suppress informational logging

      !Local variables:
      integer :: ierr
      logical :: log_information

      !Check if PIO system used for mock unit test code has been initialized.
      !If not then initialize it now:
      if (.not.pio_initialized) then
         call cam_pio_system_init()
      end if

      if (present(log_info)) then
         log_information = log_info
      else
         log_information = .true.
      end if

      ierr = pio_openfile(pio_system, file, pio_iotype_netcdf, fname, mode)

      if(ierr /= PIO_NOERR) then
         stop 'Failed to open '//trim(fname)//' to read'
      else if(pio_iotask_rank(pio_system) == 0 .and. log_information &
              .and. mode /= pio_nowrite) then
         write(*,*) 'Opened existing file ', trim(fname), file%fh
      end if

   end subroutine cam_pio_openfile
   !===========================================================================

   !===========================================================================
   subroutine cam_pio_system_init()
      !Initialize a PIO system for use in unit testing.

       !MPI routines/variables:
       use mpi, only: MPI_COMM_WORLD
       use mpi, only: mpi_comm_rank, mpi_comm_size

       !PIO routines/variables:
       use pio, only: pio_init
       use pio, only: pio_rearr_subset
       use pio, only: pio_rearr_subset

       !Local variables:
       integer :: mpi_rank, mpi_ntasks
       integer :: ierr

       !PIO-specific local variables:
       integer :: pio_stride
       integer :: pio_num_aggregator
       integer :: pio_base
       integer :: pio_niotasks

       !Set PIO configuration variables (copied from PIO Fortran example in repo):
       pio_stride = 1
       pio_num_aggregator = 0
       pio_base = 1

       !Determine local processor MPI rank:
       call mpi_comm_rank(MPI_COMM_WORLD, mpi_rank, ierr)

       !Determine number of MPI tasks:
       call MPI_Comm_size(MPI_COMM_WORLD, mpi_ntasks, ierr)

       !Assume each MPI task is also an IO task:
       pio_niotasks = mpi_ntasks

       call pio_init(mpi_rank,     & ! MPI rank
            MPI_COMM_WORLD,        & ! MPI communicator
            pio_niotasks,          & ! Number of iotasks (ntasks/stride)
            pio_num_aggregator,    & ! number of aggregators to use
            pio_stride,            & ! stride
            pio_rearr_subset,      & ! do not use any form of rearrangement
            pio_system,            & ! iosystem
            base=pio_base)           ! base (optional argument)


      !Indicate to rest of module that PIO has now been initialized:
      pio_initialized = .true.

   end subroutine cam_pio_system_init

end module cam_pio_utils
