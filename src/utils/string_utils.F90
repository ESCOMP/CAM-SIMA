module string_utils

   use shr_string_mod, only: to_upper => shr_string_toUpper
   use shr_string_mod, only: to_lower => shr_string_toLower
   use cam_logfile,    only: iulog
   use cam_abortutils, only: endrun
   use string_core_utils, only: core_int_date_to_yyyymmdd, core_int_seconds_to_hhmmss
   use string_core_utils, only: stringify=>core_stringify, to_str=>core_to_str

   implicit none
   private

   ! Public interface methods
   public :: strlist_get_ind  ! Gets the index of a given string in a list of strings
   public :: date2yyyymmdd    ! Convert encoded date integer to "yyyy-mm-dd" format
   public :: sec2hms          ! Convert integer seconds past midnight to "hh:mm:ss" format
   public :: to_str           ! Convert integer to left justified string
   public :: to_upper         ! Convert all characters in string to upper case.
   public :: to_lower         ! Convert all characters in string to lower case.
   public :: stringify        ! Convert one or more values of any intrinsic data types to a character string for pretty printing

CONTAINS

   subroutine strlist_get_ind(strlist, str, ind, abort)

      ! Get the index of a given string in a list of strings.  Optional abort argument
      ! allows returning control to caller when the string is not found.  Default
      ! behavior is to call endrun when string is not found.

      ! Arguments
      character(len=*),  intent(in)  :: strlist(:) ! list of strings
      character(len=*),  intent(in)  :: str        ! string to search for
      integer,           intent(out) :: ind        ! index of str in strlist
      logical, optional, intent(in)  :: abort      ! flag controlling abort

      ! Local variables
      integer :: m
      logical :: abort_on_error
      character(len=*), parameter :: sub='strlist_get_ind'

      ! Find string in list
      do m = 1, size(strlist)
         if (str == strlist(m)) then
            ind  = m
            return
         end if
      end do

      ! String not found
      abort_on_error = .true.
      if (present(abort)) abort_on_error = abort

      if (abort_on_error) then
         write(iulog, *) sub//': FATAL: string:', trim(str), ' not found in list:', strlist(:)
         call endrun(sub//': FATAL: string not found')
      end if

      ! error return
      ind = -1

   end subroutine strlist_get_ind

   character(len=10) function date2yyyymmdd (date)

      integer, intent(in) :: date

      if (date < 0) then
         write(iulog,*)'DATE2YYYYMMDD: negative date not allowed'
         call endrun ('DATE2YYYYMMDD: negative date not allowed')
      end if

      date2yyyymmdd = core_int_date_to_yyyymmdd(date)

   end function date2yyyymmdd

   character(len=8) function sec2hms (seconds)

      integer, intent(in) :: seconds

      if (seconds < 0 .or. seconds > 86400) then
         write(iulog,*)'SEC2HMS: bad input seconds:', seconds
         call endrun ('SEC2HMS: bad input seconds: '//stringify((/seconds/)))
      end if

      sec2hms = core_int_seconds_to_hhmmss(seconds)

   end function sec2hms

end module string_utils
