module time_manager

   ! Provide CAM specific time management.  This is a wrapper layer for the ESMF
   ! time manager utility.
   ! This test version skips any ESMF call

   use shr_string_mod, only: to_upper => shr_string_toUpper
   use shr_kind_mod,   only: r8 => shr_kind_r8, SHR_KIND_CS
   use spmd_utils,     only: masterproc
   use cam_abortutils, only: endrun
   use cam_logfile,    only: iulog

   implicit none
   private
   save

! Public methods

public ::&
   timemgr_init,             &! time manager initialization
   advance_timestep,         &! increment the clocks current time
   get_step_size,            &! return step size in seconds
   get_nstep,                &! return timestep number
   get_curr_date,            &! return date components at end of current timestep
   get_prev_date,            &! return date components at beginning of current timestep
   get_start_date,           &! return components of the start date
   get_ref_date,             &! return components of the reference date
   get_perp_date,            &! return components of the perpetual date, and current time of day
   get_curr_time,            &! return components of elapsed time since reference date at end of current timestep
   get_prev_time,            &! return components of elapsed time since reference date at beg of current timestep
   is_first_step,            &! return true on first step of initial run
   is_first_restart_step      ! return true on first step of restart or branch run

! Private module data

integer, parameter :: uninit_int = -999999999

integer :: dtime = uninit_int               ! timestep in seconds

character(len=32) :: calendar               ! Calendar type
logical :: tm_first_restart_step = .false.  ! true for first step of a restart or branch run
logical :: tm_perp_calendar = .false.       ! true when using perpetual calendar

!=========================================================================================
contains
!=========================================================================================

subroutine timemgr_init( &
   dtime_in, calendar_in, start_ymd, start_tod, ref_ymd, &
   ref_tod, stop_ymd, stop_tod, curr_ymd, curr_tod,      &
   perpetual_run, perpetual_ymd, initial_run)

   ! Initialize the time manager.

   ! Arguments
   integer,          intent(in) :: dtime_in       ! Coupling period (sec)
   character(len=*), intent(IN) :: calendar_in    ! Calendar type
   integer,          intent(IN) :: start_ymd      ! Start date (YYYYMMDD)
   integer,          intent(IN) :: start_tod      ! Start time of day (sec)
   integer,          intent(IN) :: ref_ymd        ! Reference date (YYYYMMDD)
   integer,          intent(IN) :: ref_tod        ! Reference time of day (sec)
   integer,          intent(IN) :: stop_ymd       ! Stop date (YYYYMMDD)
   integer,          intent(IN) :: stop_tod       ! Stop time of day (sec)
   integer,          intent(IN) :: curr_ymd       ! current date (YYYYMMDD)
   integer,          intent(IN) :: curr_tod       ! current time of day (sec)
   logical,          intent(IN) :: perpetual_run  ! If in perpetual mode or not
   integer,          intent(IN) :: perpetual_ymd  ! Perpetual date (YYYYMMDD)
   logical,          intent(in) :: initial_run    ! true => initial (or startup) run


end subroutine timemgr_init
!=========================================================================================

subroutine advance_timestep()

! Increment the timestep number.

! Local variables
   character(len=*), parameter :: sub = 'advance_timestep'
   integer :: rc
!-----------------------------------------------------------------------------------------

   tm_first_restart_step = .false.

end subroutine advance_timestep
!=========================================================================================

integer function get_step_size()

! Return the step size in seconds.

! Local variables
   character(len=*), parameter :: sub = 'get_step_size'
   integer :: rc
!-----------------------------------------------------------------------------------------

   rc = 1800

end function get_step_size
!=========================================================================================

integer function get_nstep()

! Return the timestep number.

! Local variables
   character(len=*), parameter :: sub = 'get_nstep'
   integer :: rc
!-----------------------------------------------------------------------------------------

   get_nstep = 1

end function get_nstep
!=========================================================================================

subroutine get_curr_date(yr, mon, day, tod, offset)

! Return date components valid at end of current timestep with an optional
! offset (positive or negative) in seconds.

! Arguments
   integer, intent(out) ::&
      yr,    &! year
      mon,   &! month
      day,   &! day of month
      tod     ! time of day (seconds past 0Z)

   integer, optional, intent(in) :: offset  ! Offset from current time in seconds.
                                            ! Positive for future times, negative
                                            ! for previous times.

! Local variables
   character(len=*), parameter :: sub = 'get_curr_date'
   integer :: rc
!-----------------------------------------------------------------------------------------

   yr = 101
   mon = 1
   day = 1
   tod = 0

end subroutine get_curr_date
!=========================================================================================

subroutine get_perp_date(yr, mon, day, tod, offset)

! Return time of day valid at end of current timestep and the components
! of the perpetual date (with an optional offset (positive or negative) in seconds.

! Arguments
   integer, intent(out) ::&
      yr,    &! year
      mon,   &! month
      day,   &! day of month
      tod     ! time of day (seconds past 0Z)

   integer, optional, intent(in) :: offset  ! Offset from current time in seconds.
                                            ! Positive for future times, negative
                                            ! for previous times.

! Local variables
   character(len=*), parameter :: sub = 'get_perp_date'
   integer :: rc

   yr = 1
   mon = 1
   day = 1
   tod = 0

end subroutine get_perp_date
!=========================================================================================

subroutine get_prev_date(yr, mon, day, tod)

! Return date components valid at beginning of current timestep.

! Arguments
   integer, intent(out) ::&
      yr,    &! year
      mon,   &! month
      day,   &! day of month
      tod     ! time of day (seconds past 0Z)

   yr = 100
   mon = 12
   day = 31
   tod = 84600

end subroutine get_prev_date
!=========================================================================================

subroutine get_start_date(yr, mon, day, tod)

! Return date components valid at beginning of initial run.

! Arguments
   integer, intent(out) ::&
      yr,    &! year
      mon,   &! month
      day,   &! day of month
      tod     ! time of day (seconds past 0Z)

! Local variables
   character(len=*), parameter :: sub = 'get_start_date'
   integer :: rc
!-----------------------------------------------------------------------------------------

   call get_curr_date(yr, mon, day, tod)

end subroutine get_start_date
!=========================================================================================

subroutine get_ref_date(yr, mon, day, tod)

! Return date components of the reference date.

! Arguments
   integer, intent(out) ::&
      yr,    &! year
      mon,   &! month
      day,   &! day of month
      tod     ! time of day (seconds past 0Z)

! Local variables
   character(len=*), parameter :: sub = 'get_ref_date'
   integer :: rc
!-----------------------------------------------------------------------------------------

   call get_curr_date(yr, mon, day, tod)

end subroutine get_ref_date
!=========================================================================================

subroutine get_curr_time(days, seconds)

! Return time components valid at end of current timestep.
! Current time is the time interval between the current date and the reference date.

! Arguments
   integer, intent(out) ::&
      days,   &! number of whole days in time interval
      seconds  ! remaining seconds in time interval

! Local variables
   character(len=*), parameter :: sub = 'get_curr_time'
   integer :: rc

   days = 0
   seconds = 0

end subroutine get_curr_time
!=========================================================================================

subroutine get_prev_time(days, seconds)

! Return time components valid at beg of current timestep.
! prev time is the time interval between the prev date and the reference date.

! Arguments
   integer, intent(out) ::&
      days,   &! number of whole days in time interval
      seconds  ! remaining seconds in time interval

! Local variables
   character(len=*), parameter :: sub = 'get_prev_time'
   integer :: rc
!-----------------------------------------------------------------------------------------

   days = 0
   seconds = 0

end subroutine get_prev_time

logical function is_first_step()

! Return true on first step of initial run only.

! Local variables
   character(len=*), parameter :: sub = 'is_first_step'
   integer :: rc
!-----------------------------------------------------------------------------------------

   is_first_step = .true.

end function is_first_step
!=========================================================================================

logical function is_first_restart_step()

! Return true on first step of restart run only.

!-----------------------------------------------------------------------------------------

   is_first_restart_step = .false.

end function is_first_restart_step
!=========================================================================================

logical function is_last_step()

! Return true on last timestep.

! Local variables
   character(len=*), parameter :: sub = 'is_last_step'
   integer :: rc
!-----------------------------------------------------------------------------------------

   is_last_step = .false.

end function is_last_step

end module time_manager
