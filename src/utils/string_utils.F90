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
   public :: increment_string ! Increment a string whose ending characters are digits.
   public :: last_non_digit   ! Get position of last non-digit in the input string.
   public :: get_last_significant_char ! Get position of last significant (non-blank, non-null) character in string.

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

   ! Increment a string whose ending characters are digits.
   ! The incremented integer must be in the range [0 - (10**n)-1]
   ! where n is the number of trailing digits.
   ! Return values:
   !
   !  0 success
   ! -1 error: no trailing digits in string
   ! -2 error: incremented integer is out of range
   integer function increment_string(s, inc)
       integer,          intent(in)    :: inc ! value to increment string (may be negative)
       character(len=*), intent(inout) :: s   ! string with trailing digits

       integer :: &
         i, &        ! index
         lstr, &     ! number of significant characters in string
         lnd, &      ! position of last non-digit
         ndigit, &   ! number of trailing digits
         ival, &     ! integer value of trailing digits
         pow, &      ! power of ten
         digit       ! integer value of a single digit

       lstr   = get_last_significant_char(s)
       lnd    = last_non_digit(s)
       ndigit = lstr - lnd

       if(ndigit == 0) then
           increment_string = -1
           return
       end if

       ! Calculate integer corresponding to trailing digits.
       ival = 0
       pow  = 0
       do i = lstr,lnd+1,-1
           digit = ICHAR(s(i:i)) - ICHAR('0')
           ival  = ival + digit * 10**pow
           pow   = pow + 1
       end do

       ! Increment the integer
       ival = ival + inc
       if( ival < 0 .or. ival > 10**ndigit-1 ) then
           increment_string = -2
           return
       end if

       ! Overwrite trailing digits
       pow = ndigit
       do i = lnd+1,lstr
           digit  = MOD( ival,10**pow ) / 10**(pow-1)
           s(i:i) = CHAR( ICHAR('0') + digit )
           pow    = pow - 1
       end do

       increment_string = 0

   end function increment_string

   ! Get position of last non-digit in the input string.
   ! Return values:
   !     > 0  => position of last non-digit
   !     = 0  => token is all digits (or empty)
   integer pure function last_non_digit(s)
       character(len=*), intent(in) :: s
       integer :: n, nn, digit

       n = get_last_significant_char(s)
       if(n == 0) then     ! empty string
           last_non_digit = 0
           return
       end if

       do nn = n,1,-1
           digit = ICHAR(s(nn:nn)) - ICHAR('0')
           if( digit < 0 .or. digit > 9 ) then
               last_non_digit = nn
           return
           end if
       end do

       last_non_digit = 0    ! all characters are digits

   end function last_non_digit

   ! Get position of last significant character in string.
   !   Here significant means non-blank or non-null.
   !   Return values:
   !       > 0  => position of last significant character
   !       = 0  => no significant characters in string
   integer pure function get_last_significant_char(cs)
       character(len=*), intent(in) :: cs       !  Input character string
       integer :: l, n

       l = LEN(cs)
       if( l == 0 ) then
           get_last_significant_char = 0
           return
       end if

       do n = l,1,-1
           if( cs(n:n) /= ' ' .and. cs(n:n) /= CHAR(0) ) then
               exit
           end if
       end do
       get_last_significant_char = n

   end function get_last_significant_char

end module string_utils
