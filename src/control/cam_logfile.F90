module cam_logfile

!-----------------------------------------------------------------------
!
! Purpose: This module is responsible for managing the logical unit
!          of CAM's output log
!
! Author: mvr, Sep 2007
!
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!- use statements ------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!- module boilerplate --------------------------------------------------
!-----------------------------------------------------------------------
   implicit none
   private
   save

!-----------------------------------------------------------------------
! Public interfaces ----------------------------------------------------
!-----------------------------------------------------------------------
   public :: cam_set_log_unit
   public :: cam_logfile_readnl
   public :: cam_log_multiwrite
!-----------------------------------------------------------------------
! Public data ----------------------------------------------------------
!-----------------------------------------------------------------------
   integer, public, parameter :: DEBUGOUT_NONE    = 0
   integer, public, parameter :: DEBUGOUT_INFO    = 1
   integer, public, parameter :: DEBUGOUT_VERBOSE = 2
   integer, public, parameter :: DEBUGOUT_DEBUG   = 3
   integer, public, protected :: debug_output = DEBUGOUT_NONE
   !> \section arg_table_cam_logfile  Argument Table
   !! \htmlinclude cam_logfile.html
   integer, public, protected :: iulog = 6
   logical, public, protected :: log_output = .false.

!-----------------------------------------------------------------------
! Private data ---------------------------------------------------------
!-----------------------------------------------------------------------
   logical :: iulog_set = .false.

   interface cam_log_multiwrite
      module procedure cam_log_multiwrite_ni  ! Multiple integers
      module procedure cam_log_multiwrite_nr8 ! Multiple 8-byte reals
   end interface cam_log_multiwrite

CONTAINS

!-----------------------------------------------------------------------
! Subroutines and functions --------------------------------------------
!-----------------------------------------------------------------------

   subroutine cam_set_log_unit(unit_num)
      use shr_sys_mod, only: shr_sys_flush

      integer, intent(in) :: unit_num

      ! Change iulog to unit_num on this PE or log a warning
      ! The log unit number can be set at most once per run
      if (iulog_set) then
         write(iulog, *) 'cam_set_log_unit: Cannot change log unit during run'
         call shr_sys_flush(iulog)
      else
         iulog = unit_num
         iulog_set = .true.
      end if
   end subroutine cam_set_log_unit

   subroutine cam_logfile_readnl(nlfile)
      use mpi,        only: mpi_integer
      use shr_nl_mod, only: find_group_name => shr_nl_find_group_name
      use spmd_utils, only: mpicom, masterprocid, masterproc

      ! nlfile: filepath for file containing namelist input
      character(len=*), intent(in) :: nlfile

      ! Local variables
      integer                      :: unitn
      integer                      :: ierr

      character(len=*), parameter  :: subname = 'cam_logfile_readnl'

      namelist /cam_logfile_nl/ debug_output
      !------------------------------------------------------------------------

      ! Since cam_set_log_unit is called before spmd_init is called,
      !    set log_output flag here
      log_output = masterproc

      if (masterproc) then
         open(newunit=unitn, file=trim(nlfile), status='old')
         call find_group_name(unitn, 'cam_logfile_nl', status=ierr)
         if (ierr == 0) then
            read(unitn, cam_logfile_nl, iostat=ierr)
            if (ierr /= 0) then
               ! Can't call endrun because of dependency loop
               write(iulog, *) subname, ': ERROR: reading namelist'
            end if
         end if
         close(unitn)

         select case(debug_output)
         case (0)
            write(iulog, *) subname, ': No extra checks or informational output'
         case (1)
            write(iulog, *) subname, ': Output extra informational output'
         case (2)
            write(iulog, *) subname, ': Extra checks plus informational output'
         case (3)
            write(iulog, *) subname,                                          &
                 ': All available checks and informational output'
         case default
            write(iulog, '(3a,i0,a)') subname, ' WARNING: Ignoring invalid ', &
                 'log level, ', debug_output, ' (using 0, no extra output)'
         end select
      end if

      call mpi_bcast(debug_output, 1, mpi_integer, masterprocid, mpicom, ierr)
      if (ierr /= 0) then
         ! Can't call endrun because of dependency loop
         ! But MPI usually crashes in Fortran
         write(iulog, *) subname, ": ERROR: mpi_bcast: debug_output"
      end if

   end subroutine cam_logfile_readnl

   subroutine cam_log_multiwrite_ni(subname, headers, fmt_string, values)
      ! Print out values from every task
      use shr_sys_mod,  only: shr_sys_flush
      use spmd_utils,   only: mpicom, masterprocid, masterproc, npes
      use mpi,          only: mpi_integer

      ! Dummy arguments
      character(len=*), intent(in) :: subname
      character(len=*), intent(in) :: headers
      character(len=*), intent(in) :: fmt_string
      integer,          intent(in) :: values(:)
      ! Local variables
      integer, allocatable         :: global_info(:,:)
      integer                      :: index, fnum
      integer                      :: num_fields
      integer                      :: iret

      num_fields = size(values, 1)

      allocate(global_info(npes, num_fields))
      do index = 1, num_fields
         call MPI_Gather(values(index), 1, MPI_INTEGER, global_info(:,index), &
              1, MPI_INTEGER, masterprocid, mpicom, iret)
      end do
      if (masterproc) then
         write(iulog, '(2a)') trim(subname), trim(headers)
         do index = 1, npes
            write(iulog, fmt_string) subname, index - 1,                      &
                 (global_info(index, fnum), fnum = 1, num_fields)
         end do
         call shr_sys_flush(iulog)
      end if
      deallocate(global_info)
   end subroutine cam_log_multiwrite_ni

   subroutine cam_log_multiwrite_nr8(subname, headers, fmt_string, values)
      ! Print out values from every task
      use iso_fortran_env, only: r8 => REAL64
      use shr_sys_mod,     only: shr_sys_flush
      use spmd_utils,      only: mpicom, masterprocid, masterproc, npes
      use mpi,             only: mpi_real8

      ! Dummy arguments
      character(len=*), intent(in) :: subname
      character(len=*), intent(in) :: headers
      character(len=*), intent(in) :: fmt_string
      real(r8),         intent(in) :: values(:)
      ! Local variables
      real(r8), allocatable        :: global_info(:,:)
      integer                      :: index, fnum
      integer                      :: num_fields
      integer                      :: iret

      num_fields = size(values, 1)

      allocate(global_info(npes, num_fields))
      do index = 1, num_fields
         call MPI_Gather(values(index), 1, MPI_REAL8, global_info(:,index), &
              1, MPI_REAL8, masterprocid, mpicom, iret)
      end do
      if (masterproc) then
         write(iulog, '(2a)') trim(subname), trim(headers)
         do index = 1, npes
            write(iulog, fmt_string) subname, index - 1,                      &
                 (global_info(index, fnum), fnum = 1, num_fields)
         end do
         call shr_sys_flush(iulog)
      end if
      deallocate(global_info)
   end subroutine cam_log_multiwrite_nr8

end module cam_logfile
