module string_utils

   use shr_string_mod, only: to_upper => shr_string_toUpper
   use shr_string_mod, only: to_lower => shr_string_toLower

   implicit none
   private

   ! Public interface methods

   public :: to_upper         ! Convert character string to upper case
   public :: to_lower         ! Convert character string to lower case
   public :: increment_string ! increments a string
   public :: last_sig_char    ! Position of last significant character in string
   public :: to_str           ! convert integer to left justified string

   ! Private module variables
   integer, parameter :: lower_to_upper = iachar("A") - iachar("a")
   integer, parameter :: upper_to_lower = iachar("a") - iachar("A")

CONTAINS

   integer function increment_string(str, increment)
      !-----------------------------------------------------------------------
      ! 	... Increment a string whose ending characters are digits.
      !           The incremented integer must be in the range [0 - (10**n)-1]
      !           where n is the number of trailing digits.
      !           Return values:
      !
      !            0 success
      !           -1 error: no trailing digits in string
      !           -2 error: incremented integer is out of range
      !-----------------------------------------------------------------------

      !-----------------------------------------------------------------------
      ! 	... Dummy variables
      !-----------------------------------------------------------------------
      character(len=*), intent(inout) :: str       ! string with trailing digits
      ! increment: value to increment string (may be negative)
      integer,          intent(in)    :: increment

      !-----------------------------------------------------------------------
      ! 	... Local variables
      !-----------------------------------------------------------------------
      integer :: ind    ! index
      integer :: lstr   ! number of significant characters in string
      integer :: lnd    ! position of last non-digit
      integer :: ndigit ! number of trailing digits
      integer :: ival   ! integer value of trailing digits
      integer :: exp    ! power of ten related to most sig digit processed
      integer :: digit  ! integer value of a single digit

      lstr   = last_sig_char(str)
      lnd    = last_index(str)
      ndigit = lstr - lnd

      if (ndigit == 0) then
         increment_string = -1
         return
      end if

      !-----------------------------------------------------------------------
      !     	... Calculate integer corresponding to trailing digits.
      !-----------------------------------------------------------------------
      ival = 0
      exp  = 1
      do ind = lstr, lnd+1, -1
         digit = ICHAR(str(ind:ind)) - ICHAR('0')
         ival  = ival + (digit * exp)
         exp   = exp * 10
      end do

      !-----------------------------------------------------------------------
      !     	... Increment the integer and test for range
      !-----------------------------------------------------------------------
      exp = exp - 1
      ival = ival + increment
      if (ival < 0 .or. ival > exp) then
         increment_string = -2
         return
      end if

      !-----------------------------------------------------------------------
      !     	... Record new values
      !-----------------------------------------------------------------------
      do ind = lstr, lnd+1, -1
         str(ind:ind) = ACHAR(MOD(ival, 10) + ICHAR('0'))
         ival = ival / 10
      end do

      increment_string = 0

   end function increment_string

!=========================================================================================

   integer function last_index(cstr)
      !-----------------------------------------------------------------------
      ! 	... Position of last non-digit in the first input token.
      ! 	    Return values:
      !     	    > 0  => position of last non-digit
      !     	    = 0  => token is all digits (or empty)
      !-----------------------------------------------------------------------
      !-----------------------------------------------------------------------
      ! 	... Dummy arguments
      !-----------------------------------------------------------------------
      character(len=*), intent(in) :: cstr    !  Input character string

      !-----------------------------------------------------------------------
      ! 	... Local variables
      !-----------------------------------------------------------------------
      integer :: lsc ! last sig char
      integer :: index
      integer :: digit

      lsc = last_sig_char(cstr)
      if (lsc == 0) then     ! empty string
         last_index = 0
         return
      end if

      do index = lsc, 1, -1
         digit = ICHAR(cstr(index:index)) - ICHAR('0')
         if ((digit < 0) .or. (digit > 9)) then
            last_index = index
            return
         end if
      end do

      last_index = 0    ! all characters are digits

   end function last_index

!=========================================================================================

   integer function last_sig_char(cstr)
      !-----------------------------------------------------------------------
      ! 	... Position of last significant character in string.
      !           Here significant means non-blank or non-null.
      !           Return values:
      !               > 0  => position of last significant character
      !               = 0  => no significant characters in string
      !-----------------------------------------------------------------------
      !-----------------------------------------------------------------------
      ! 	... Dummy arguments
      !-----------------------------------------------------------------------
      character(len=*), intent(in) :: cstr    !  Input character string

      !-----------------------------------------------------------------------
      ! 	... Local variables
      !-----------------------------------------------------------------------
      integer :: slen
      integer :: index

      slen = len_trim(cstr)
      if (slen == 0) then
         last_sig_char = 0
         return
      end if

      do index = slen, 1, -1
         if ( (cstr(index:index) /= ' ') .and.                                &
              (cstr(index:index) /= ACHAR(0))) then
            exit
         end if
      end do
      last_sig_char = index

   end function last_sig_char

!=========================================================================================

character(len=10) function to_str(n)

   ! return default integer as a left justified string

   ! arguments
   integer, intent(in) :: n
   !----------------------------------------------------------------------------

   write(to_str,'(i0)') n

end function to_str

!=========================================================================================

end module string_utils
