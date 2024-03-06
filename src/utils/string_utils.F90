module string_utils

   use shr_string_mod, only: to_upper => shr_string_toUpper
   use shr_string_mod, only: to_lower => shr_string_toLower

   implicit none
   private

   ! Public interface methods

   public :: to_upper         ! Convert character string to upper case
   public :: to_lower         ! Convert character string to lower case
   public :: strlist_get_ind  ! find string in a list of strings and return its index
   public :: increment_string ! increments a string
   public :: last_sig_char    ! Position of last significant character in string
   public :: to_str           ! convert integer to left justified string
   public :: stringify        ! Convert one or more values of any intrinsic data types to a character string for pretty printing

   ! Private module variables
   integer, parameter :: lower_to_upper = iachar("A") - iachar("a")
   integer, parameter :: upper_to_lower = iachar("a") - iachar("A")

CONTAINS

   !=========================================================================================

   subroutine strlist_get_ind(strlist, str, ind, abort)

   ! Get the index of a given string in a list of strings.  Optional abort argument
   ! allows returning control to caller when the string is not found.  Default
   ! behavior is to call endrun when string is not found.

   use cam_logfile,    only: iulog
   use cam_abortutils, only: endrun

   ! Arguments
   character(len=*),  intent(in)  :: strlist(:) ! list of strings
   character(len=*),  intent(in)  :: str        ! string to search for
   integer,           intent(out) :: ind        ! index of str in strlist
   logical, optional, intent(in)  :: abort      ! flag controlling abort

   ! Local variables
   integer :: m
   logical :: abort_on_error
   character(len=*), parameter :: sub='strlist_get_ind'
   !----------------------------------------------------------------------------

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

   !=========================================================================================

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

   !> Convert one or more values of any intrinsic data types to a character string for pretty printing.
   !> If `value` contains more than one element, the elements will be stringified, delimited by `separator`, then concatenated.
   !> If `value` contains exactly one element, the element will be stringified without using `separator`.
   !> If `value` contains zero element or is of unsupported data types, an empty character string is produced.
   !> If `separator` is not supplied, it defaults to `, ` (i.e., a comma and a space).
   !> (KCW, 2024-02-04)
   pure function stringify(value, separator)
      use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64

      class(*), intent(in) :: value(:)
      character(*), optional, intent(in) :: separator
      character(:), allocatable :: stringify

      integer, parameter :: sizelimit = 1024

      character(:), allocatable :: buffer, delimiter, format
      integer :: i, n, offset

      if (present(separator)) then
         delimiter = separator
      else
         delimiter = ', '
      end if

      n = min(size(value), sizelimit)

      if (n == 0) then
         stringify = ''

         return
      end if

      select type (value)
         type is (character(*))
            allocate(character(len(value) * n + len(delimiter) * (n - 1)) :: buffer)

            buffer(:) = ''
            offset = 0

            do i = 1, n
               if (len(delimiter) > 0 .and. i > 1) then
                  buffer(offset + 1:offset + len(delimiter)) = delimiter
                  offset = offset + len(delimiter)
               end if

               if (len_trim(adjustl(value(i))) > 0) then
                  buffer(offset + 1:offset + len_trim(adjustl(value(i)))) = trim(adjustl(value(i)))
                  offset = offset + len_trim(adjustl(value(i)))
               end if
            end do
         type is (integer(int32))
            allocate(character(11 * n + len(delimiter) * (n - 1)) :: buffer)
            allocate(character(17 + len(delimiter) + floor(log10(real(n))) + 1) :: format)

            write(format, '(a, i0, 3a)') '(ss, ', n, '(i0, :, "', delimiter, '"))'
            write(buffer, format) value
         type is (integer(int64))
            allocate(character(20 * n + len(delimiter) * (n - 1)) :: buffer)
            allocate(character(17 + len(delimiter) + floor(log10(real(n))) + 1) :: format)

            write(format, '(a, i0, 3a)') '(ss, ', n, '(i0, :, "', delimiter, '"))'
            write(buffer, format) value
         type is (logical)
            allocate(character(1 * n + len(delimiter) * (n - 1)) :: buffer)
            allocate(character(13 + len(delimiter) + floor(log10(real(n))) + 1) :: format)

            write(format, '(a, i0, 3a)') '(', n, '(l1, :, "', delimiter, '"))'
            write(buffer, format) value
         type is (real(real32))
            allocate(character(13 * n + len(delimiter) * (n - 1)) :: buffer)

            if (maxval(abs(value)) < 1.0e5_real32) then
               allocate(character(20 + len(delimiter) + floor(log10(real(n))) + 1) :: format)
               write(format, '(a, i0, 3a)') '(ss, ', n, '(f13.6, :, "', delimiter, '"))'
            else
               allocate(character(23 + len(delimiter) + floor(log10(real(n))) + 1) :: format)
               write(format, '(a, i0, 3a)') '(ss, ', n, '(es13.6e2, :, "', delimiter, '"))'
            end if

            write(buffer, format) value
         type is (real(real64))
            allocate(character(13 * n + len(delimiter) * (n - 1)) :: buffer)

            if (maxval(abs(value)) < 1.0e5_real64) then
               allocate(character(20 + len(delimiter) + floor(log10(real(n))) + 1) :: format)
               write(format, '(a, i0, 3a)') '(ss, ', n, '(f13.6, :, "', delimiter, '"))'
            else
               allocate(character(23 + len(delimiter) + floor(log10(real(n))) + 1) :: format)
               write(format, '(a, i0, 3a)') '(ss, ', n, '(es13.6e2, :, "', delimiter, '"))'
            end if

            write(buffer, format) value
         class default
            stringify = ''

            return
      end select

      stringify = trim(buffer)
   end function stringify

!=========================================================================================

end module string_utils
