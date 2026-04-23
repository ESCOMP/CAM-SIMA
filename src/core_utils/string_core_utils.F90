module string_core_utils

    implicit none
    private

    public :: core_to_str                ! Convert integer to left justified string
    public :: core_int_date_to_yyyymmdd  ! Convert encoded date integer to "yyyy-mm-dd" format
    public :: core_int_seconds_to_hhmmss ! Convert integer seconds past midnight to "hh:mm:ss" format
    public :: split                      ! Parse a string into tokens, one at a time
    public :: stringify                  ! Convert one or more values of any intrinsic data types to a character string for pretty printing
    public :: tokenize                   ! Parse a string into tokens
    public :: increment_string           ! Increment a string whose ending characters are digits.
    public :: last_non_digit             ! Get position of last non-digit in the input string.
    public :: get_last_significant_char  ! Get position of last significant (non-blank, non-null) character in string.

    interface tokenize
        module procedure tokenize_into_first_last
        module procedure tokenize_into_tokens_separator
    end interface tokenize

contains

    character(len=10) pure function core_to_str(n)
        ! return default integer as a left justified string

        integer, intent(in) :: n
    
        write(core_to_str,'(i0)') n
    
    end function core_to_str

    character(len=10) pure function core_int_date_to_yyyymmdd (date)
        ! Undefined behavior if date <= 0

        ! Input arguments
        integer, intent(in) :: date

        ! Local variables
        integer :: year    ! year of yyyy-mm-dd
        integer :: month   ! month of yyyy-mm-dd
        integer :: day     ! day of yyyy-mm-dd

        year  = date / 10000
        month = (date - year*10000) / 100
        day   = date - year*10000 - month*100

        write(core_int_date_to_yyyymmdd, '(i4.4,A,i2.2,A,i2.2)') &
                                           year,'-',month,'-',day

    end function core_int_date_to_yyyymmdd

    character(len=8) pure function core_int_seconds_to_hhmmss (seconds)
        ! Undefined behavior if seconds outside [0, 86400]

        ! Input arguments
        integer, intent(in) :: seconds

        ! Local variables
        integer :: hours     ! hours of hh:mm:ss
        integer :: minutes   ! minutes of hh:mm:ss
        integer :: secs      ! seconds of hh:mm:ss

        hours   = seconds / 3600
        minutes = (seconds - hours*3600) / 60
        secs    = (seconds - hours*3600 - minutes*60)

        write(core_int_seconds_to_hhmmss,'(i2.2,A,i2.2,A,i2.2)') &
                                        hours,':',minutes,':',secs

    end function core_int_seconds_to_hhmmss

    !> Parse a string into tokens, one at a time. Each character in `set` is a token delimiter.
    !> If `back` is absent or is present with the value `.false.`, `pos` is assigned the position of the leftmost
    !> token delimiter in `string` whose position is greater than `pos`, or if there is no such character, it
    !> is assigned a value one greater than the length of `string`. This identifies a token with starting
    !> position one greater than the value of `pos` on invocation, and ending position one less than the
    !> value of `pos` on return.
    !> If `back` is present with the value `.true.`, `pos` is assigned the position of the rightmost token delimiter
    !> in `string` whose position is less than `pos`, or if there is no such character, it is assigned the value
    !> zero. This identifies a token with ending position one less than the value of `pos` on invocation, and
    !> starting position one greater than the value of `pos` on return.
    !> This subroutine implements the `split` intrinsic procedure as defined in the Fortran 2023 language standard
    !> (Section 16.9.196). We implement it ourselves because the compiler support may take years to become widespread.
    !> (KCW, 2025-10-29)
    pure subroutine split(string, set, pos, back)
        character(*), intent(in) :: string, set
        integer, intent(inout) :: pos
        logical, optional, intent(in) :: back

        integer :: offset

        if (present(back)) then
            if (back) then
                offset = max(min(pos, len(string) + 1), 1)
                pos = scan(string(1:offset - 1), set, back=.true.)

                return
            end if
        end if

        offset = max(min(pos, len(string)), 0)
        pos = scan(string(offset + 1:), set)

        if (pos == 0) then
            pos = len(string) + 1

            return
        end if

        pos = offset + pos
    end subroutine split

    !> Convert one or more values of any intrinsic data types to a character string for pretty printing.
    !> If `value` contains more than one element, the elements will be stringified, delimited by `separator`, then concatenated.
    !> If `value` contains exactly one element, the element will be stringified without using `separator`.
    !> If `value` contains zero element or is of unsupported data types, an empty character string is produced.
    !> If `separator` is not supplied, it defaults to ", " (i.e., a comma and a space).
    !> (KCW, 2024-02-04)
    pure function stringify(value, separator)
        use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64

        class(*), intent(in) :: value(:)
        character(*), optional, intent(in) :: separator
        character(:), allocatable :: stringify

        integer, parameter :: sizelimit = 1024

        character(:), allocatable :: buffer, delimiter, format
        character(:), allocatable :: value_c(:)
        integer :: i, n, offset, fmt_len

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

                ! Workaround for a bug in GNU Fortran >= 12. This is perhaps the manifestation of GCC Bugzilla Bug 100819.
                ! When a character string array is passed as the actual argument to an unlimited polymorphic dummy argument,
                ! its array index and length parameter are mishandled.
                allocate(character(len(value)) :: value_c(size(value)))

                value_c(:) = value(:)

                do i = 1, n
                    if (len(delimiter) > 0 .and. i > 1) then
                        buffer(offset + 1:offset + len(delimiter)) = delimiter
                        offset = offset + len(delimiter)
                    end if

                    if (len_trim(adjustl(value_c(i))) > 0) then
                        buffer(offset + 1:offset + len_trim(adjustl(value_c(i)))) = trim(adjustl(value_c(i)))
                        offset = offset + len_trim(adjustl(value_c(i)))
                    end if
                end do

                deallocate(value_c)
            type is (integer(int32))
                allocate(character(len=11 * n + len(delimiter) * (n - 1)) :: buffer)
                fmt_len = 17 + len(delimiter) + floor(log10(real(n))) + 1
                allocate(character(fmt_len) :: format)

                write(format, '(a, i0, 3a)') '(ss, ', n, '(i0, :, "', delimiter, '"))'
                write(buffer, format) value
            type is (integer(int64))
                allocate(character(len=20 * n + len(delimiter) * (n - 1)) :: buffer)
                fmt_len = 17 + len(delimiter) + floor(log10(real(n))) + 1
                allocate(character(len=fmt_len) :: format)

                write(format, '(a, i0, 3a)') '(ss, ', n, '(i0, :, "', delimiter, '"))'
                write(buffer, format) value
            type is (logical)
                allocate(character(len=1 * n + len(delimiter) * (n - 1)) :: buffer)
                fmt_len = 13 + len(delimiter) + floor(log10(real(n))) + 1
                allocate(character(len=fmt_len) :: format)

                write(format, '(a, i0, 3a)') '(', n, '(l1, :, "', delimiter, '"))'
                write(buffer, format) value
            type is (real(real32))
                allocate(character(len=13 * n + len(delimiter) * (n - 1)) :: buffer)

                if (maxval(abs(value)) < 1.0e5_real32) then
                    fmt_len = 20 + len(delimiter) + floor(log10(real(n))) + 1
                    allocate(character(len=fmt_len) :: format)
                    write(format, '(a, i0, 3a)') '(ss, ', n, '(f13.6, :, "', delimiter, '"))'
                else
                    fmt_len = 23 + len(delimiter) + floor(log10(real(n))) + 1
                    allocate(character(len=fmt_len) :: format)
                    write(format, '(a, i0, 3a)') '(ss, ', n, '(es13.6e2, :, "', delimiter, '"))'
                end if

                write(buffer, format) value
            type is (real(real64))
                allocate(character(len=13 * n + len(delimiter) * (n - 1)) :: buffer)

                if (maxval(abs(value)) < 1.0e5_real64) then
                    fmt_len = 20 + len(delimiter) + floor(log10(real(n))) + 1
                    allocate(character(len=fmt_len) :: format)
                    write(format, '(a, i0, 3a)') '(ss, ', n, '(f13.6, :, "', delimiter, '"))'
                else
                    fmt_len = 23 + len(delimiter) + floor(log10(real(n))) + 1
                    allocate(character(len=fmt_len) :: format)
                    write(format, '(a, i0, 3a)') '(ss, ', n, '(es13.6e2, :, "', delimiter, '"))'
                end if

                write(buffer, format) value
            class default
                stringify = ''

                return
        end select

        stringify = trim(buffer)
    end function stringify

    !> Parse a string into tokens. Each character in `set` is a token delimiter.
    !> `first` is allocated with the lower bound equal to one and the upper bound equal to the number of tokens in `string`.
    !> Each element is assigned, in array element order, the starting position of each token in `string`, in the order found.
    !> `last` is allocated with the lower bound equal to one and the upper bound equal to the number of tokens in `string`.
    !> Each element is assigned, in array element order, the ending position of each token in `string`, in the order found.
    !> This subroutine implements the `tokenize` intrinsic procedure as defined in the Fortran 2023 language standard
    !> (Section 16.9.210). We implement it ourselves because the compiler support may take years to become widespread.
    !> (KCW, 2025-10-29)
    pure subroutine tokenize_into_first_last(string, set, first, last)
        character(*), intent(in) :: string, set
        integer, allocatable, intent(out) :: first(:), last(:)

        integer :: pos_start(len(string) + 1), pos_end(len(string) + 1)
        integer :: l, n, pos

        l = len(string)
        n = 0
        pos = 0

        do while (pos < l + 1)
            n = n + 1
            pos_start(n) = pos + 1

            call split(string, set, pos)

            pos_end(n) = pos - 1
        end do

        allocate(first(n), last(n))

        first(:) = pos_start(1:n)
        last(:) = pos_end(1:n)
    end subroutine tokenize_into_first_last

    !> Parse a string into tokens. Each character in `set` is a token delimiter.
    !> `tokens` is allocated with the lower bound equal to one and the upper bound equal to the number of tokens in `string`,
    !> and with character length equal to the length of the longest token. It contains the tokens in `string`.
    !> `separator` is allocated with the lower bound equal to one and the upper bound equal to one less than the number of
    !> tokens in `string`, and with character length equal to one. It contains the token delimiters in `string`.
    !> This subroutine implements the `tokenize` intrinsic procedure as defined in the Fortran 2023 language standard
    !> (Section 16.9.210). We implement it ourselves because the compiler support may take years to become widespread.
    !> (KCW, 2025-10-29)
    pure subroutine tokenize_into_tokens_separator(string, set, tokens, separator)
        character(*), intent(in) :: string, set
        character(:), allocatable, intent(out) :: tokens(:)
        character(:), allocatable, optional, intent(out) :: separator(:)

        integer, allocatable :: first(:), last(:)
        integer :: i, n

        call tokenize(string, set, first, last)

        n = size(first)

        allocate(character(maxval(last - first) + 1) :: tokens(n))

        do i = 1, n
            tokens(i) = string(first(i):last(i))
        end do

        if (present(separator)) then
            allocate(character(1) :: separator(n - 1))

            do i = 1, n - 1
                separator(i) = string(last(i) + 1:last(i) + 1)
            end do
        end if
    end subroutine tokenize_into_tokens_separator

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

end module string_core_utils
