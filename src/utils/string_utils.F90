module string_utils

   use shr_string_mod, only: to_upper => shr_string_toUpper
   use shr_string_mod, only: to_lower => shr_string_toLower
   use cam_logfile,    only: iulog
   use cam_abortutils, only: endrun

   implicit none
   private

   ! Public interface methods

   public :: strlist_get_ind  ! Gets the index of a given string in a list of strings
   public :: date2yyyymmdd    ! convert encoded date integer to "yyyy-mm-dd" format
   public :: sec2hms          ! convert integer seconds past midnight to "hh:mm:ss" format
   public :: increment_string ! increments a string
   public :: last_sig_char    ! Position of last significant character in string
   public :: to_str           ! convert integer to left justified string
   public :: parse_multiplier ! Parse a repeat count and a token from input
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

   character(len=10) function date2yyyymmdd (date)

      ! Input arguments

      integer, intent(in) :: date

      ! Local workspace

      integer :: year    ! year of yyyy-mm-dd
      integer :: month   ! month of yyyy-mm-dd
      integer :: day     ! day of yyyy-mm-dd

      if (date < 0) then
         call endrun ('DATE2YYYYMMDD: negative date not allowed')
      end if

      year  = date / 10000
      month = (date - year*10000) / 100
      day   = date - year*10000 - month*100

      write(date2yyyymmdd,80) year, month, day
   80 format(i4.4,'-',i2.2,'-',i2.2)

   end function date2yyyymmdd

   !=========================================================================================

   character(len=8) function sec2hms (seconds)

      ! Input arguments

      integer, intent(in) :: seconds

      ! Local workspace

      integer :: hours     ! hours of hh:mm:ss
      integer :: minutes   ! minutes of hh:mm:ss
      integer :: secs      ! seconds of hh:mm:ss

      if (seconds < 0 .or. seconds > 86400) then
         write(iulog,*)'SEC2HMS: bad input seconds:', seconds
         call endrun ('SEC2HMS: bad input seconds:')
      end if

      hours   = seconds / 3600
      minutes = (seconds - hours*3600) / 60
      secs    = (seconds - hours*3600 - minutes*60)

      write(sec2hms,80) hours, minutes, secs
   80 format(i2.2,':',i2.2,':',i2.2)

   end function sec2hms

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
      character(len=*), intent(inout) :: str     ! string with trailing digits
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

   !===========================================================================

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

   !===========================================================================

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

   !===========================================================================

   character(len=10) function to_str(n)

      ! return default integer as a left justified string

      ! arguments
      integer, intent(in) :: n
      !----------------------------------------------------------------------------

      write(to_str,'(i0)') n

   end function to_str

   !===========================================================================

   subroutine parse_multiplier(input, multiplier, token, allowed_set, errmsg)
      ! Parse a character string (<input>) to find a token <token>, possibly
      ! multiplied by an integer (<multiplier>).
      ! Return values for <multiplier>:
      !   positive integer: Successful return with <multiplier> and <token>.
      !   zero:             <input> is an empty string
      !   -1:               Error condition (malformed input string)
      ! Return values for <token>
      !   On a successful return, <token> will contain <input> with the
      !      optional multiplier and multiplication symbol removed.
      !   On an error return, <token> will be an empty string
      !
      ! If <allowed_set> is present, then <token> must equal a value in
      !   <allowed_set> (case insensitive)
      ! If <errmsg> is present, it is filled with an error message if <input>
      !   is not an allowed format.
      ! Allowed formats are:
      !   <multiplier>*<token> where <multiplier> is the string representation
      !      a positive integer.
      !   <token> in which case <multiplier> is assumed to be one.
      !

      ! Dummy arguments
      character(len=*),           intent(in)  :: input
      integer,                    intent(out) :: multiplier
      character(len=*),           intent(out) :: token
      character(len=*), optional, intent(in)  :: allowed_set(:)
      character(len=*), optional, intent(out) :: errmsg
      ! Local variables
      integer          :: mult_ind ! Index of multiplication symbol
      integer          :: lind     ! Loop index
      integer          :: alen     ! Number of entries in <allowed_set>
      integer          :: stat     ! Read status
      logical          :: match    ! For matching <allowed_set>
      character(len=8) :: fmt_str  ! Format string

      ! Initialize output
      errmsg = ''
      multiplier = -1
      token = ''
      ! Do we have a multipler?
      mult_ind = index(input, '*')
      if (len_trim(input) == 0) then
         multiplier = 0
      else if (mult_ind <= 0) then
         multiplier = 1
         token = trim(input)
      else
         write(fmt_str, '(a,i0,a)') "(i", mult_ind - 1, ")"
         read(input, fmt_str, iostat=stat) multiplier
         if (stat == 0) then
            token = trim(input(mult_ind+1:))
         else
            if (present(errmsg)) then
               write(errmsg, *) "Invalid multiplier, '",                      &
                    input(1:mult_ind-1), "' in '", trim(input), "'"
            end if
            multiplier = -1
            token = ''
         end if
      end if

      if ((multiplier >= 0) .and. present(allowed_set)) then
         alen = size(allowed_set)
         match = .false.
         do lind = 1, alen
            if (trim(to_lower(token)) == trim(to_lower(allowed_set(lind)))) then
               match = .true.
               exit
            end if
         end do
         if (.not. match) then
            if (present(errmsg)) then
               write(errmsg, *) "Error, token, '", trim(token), "' not in (/"
               lind = len_trim(errmsg) + 1
               do mult_ind = 1, alen
                  if (mult_ind == alen) then
                     fmt_str = "' "
                  else
                     fmt_str = "', "
                  end if
                  write(errmsg(lind:), *) "'", trim(allowed_set(mult_ind)),   &
                       trim(fmt_str)
                  lind = lind + len_trim(allowed_set(mult_ind)) +             &
                       len_trim(fmt_str) + 2
               end do
               write(errmsg(lind:), *) "/)"
            end if
            multiplier = -1
            token = ''
         end if
      end if

   end subroutine parse_multiplier

   !===========================================================================

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
