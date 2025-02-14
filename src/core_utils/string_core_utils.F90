module string_core_utils

    implicit none
    private

    public :: core_to_str                ! Convert integer to left justified string
    public :: core_int_date_to_yyyymmdd  ! Convert encoded date integer to "yyyy-mm-dd" format
    public :: core_int_seconds_to_hhmmss ! Convert integer seconds past midnight to "hh:mm:ss" format
    public :: core_stringify             ! Convert one or more values of any intrinsic data types to a character string for pretty printing

CONTAINS

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

    !> Convert one or more values of any intrinsic data types to a character string for pretty printing.
    !> If `value` contains more than one element, the elements will be stringified, delimited by `separator`, then concatenated.
    !> If `value` contains exactly one element, the element will be stringified without using `separator`.
    !> If `value` contains zero element or is of unsupported data types, an empty character string is produced.
    !> If `separator` is not supplied, it defaults to `, ` (i.e., a comma and a space).
    !> (KCW, 2024-02-04)
    pure function core_stringify(value, separator)
        use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64

        class(*), intent(in) :: value(:)
        character(*), optional, intent(in) :: separator
        character(:), allocatable :: core_stringify

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
            core_stringify = ''
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
            core_stringify = ''
                return
        end select

        core_stringify = trim(buffer)

    end function core_stringify

end module string_core_utils
