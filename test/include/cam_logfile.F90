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
   integer, public, protected :: iulog = 6
   integer, public, parameter :: DEBUGOUT_NONE    = 0
   integer, public, parameter :: DEBUGOUT_INFO    = 1
   integer, public, parameter :: DEBUGOUT_VERBOSE = 2
   integer, public, parameter :: DEBUGOUT_DEBUG   = 3
   integer, public, protected :: debug_output = DEBUGOUT_NONE

!-----------------------------------------------------------------------
! Private data ---------------------------------------------------------
!-----------------------------------------------------------------------
   logical :: iulog_set = .true.

   interface cam_log_multiwrite
      module procedure cam_log_multiwrite_ni ! Multiple integers
   end interface cam_log_multiwrite

CONTAINS

!-----------------------------------------------------------------------
! Subroutines and functions --------------------------------------------
!-----------------------------------------------------------------------

   subroutine cam_set_log_unit(unit_num)

      integer, intent(in) :: unit_num

      ! Change iulog to unit_num on this PE or log a waring
      ! The log unit number can be set at most once per run
      if (iulog_set) then
         write(iulog, *) 'cam_set_log_unit: Cannot change log unit during run'
      else
         iulog = unit_num
         iulog_set = .true.
      end if
   end subroutine cam_set_log_unit

   subroutine cam_logfile_readnl(nlfile)

      ! nlfile: filepath for file containing namelist input
      character(len=*), intent(in) :: nlfile

   end subroutine cam_logfile_readnl

   subroutine cam_log_multiwrite_ni(subname, headers, fmt_string, values)
      ! Print out values from every task
      use spmd_utils,   only: masterproc

      ! Dummy arguments
      character(len=*), intent(in) :: subname
      character(len=*), intent(in) :: headers
      character(len=*), intent(in) :: fmt_string
      integer,          intent(in) :: values(:)
      ! Local variables
      integer                      :: num_fields
      integer                      :: fnum

      num_fields = size(values, 1)

      if (masterproc) then
         write(iulog, '(2a)') trim(subname), trim(headers)
         write(iulog, fmt_string) subname, 0,                                 &
              (values(fnum), fnum = 1, num_fields)
      end if
   end subroutine cam_log_multiwrite_ni

end module cam_logfile
