module string_core_utils

    implicit none
    private

    public :: core_to_str                ! Convert integer to left justified string
    public :: core_int_date_to_yyyymmdd  ! Convert encoded date integer to "yyyy-mm-dd" format
    public :: core_int_seconds_to_hhmmss ! Convert integer seconds past midnight to "hh:mm:ss" format
    public :: split                      ! Parse a string into tokens, one at a time
    public :: stringify                  ! Convert one or more values of any intrinsic data types to a character string for pretty printing
    public :: tokenize                   ! Parse a string into tokens

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

end module string_core_utils
