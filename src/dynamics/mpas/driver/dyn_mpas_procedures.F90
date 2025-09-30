! Copyright (C) 2025 University Corporation for Atmospheric Research (UCAR)
! SPDX-License-Identifier: Apache-2.0

!> This module provides standardized procedures (i.e., functions and subroutines) that serve as
!> reusable building blocks for larger and more complex functionalities elsewhere. Specifically,
!> procedures in this module are intended to be used by the MPAS subdriver and therefore are
!> compiled into the MPAS library ahead of CAM-SIMA.
!>
!> Computational procedures implement formulas that are universal in atmospheric sciences. They
!> should be designated as `elemental` where possible to aid compiler optimizations, such as
!> vectorization.
!> Utility procedures implement simple and well-defined operations that can be easily tested.
module dyn_mpas_procedures
    implicit none

    private
    ! Computational procedures.
    ! None in this group for now.
    ! Utility procedures.
    public :: almost_divisible
    public :: almost_equal
    public :: clamp
    public :: index_unique
    public :: stringify

    interface almost_divisible
        module procedure almost_divisible_real32
        module procedure almost_divisible_real64
    end interface almost_divisible

    interface almost_equal
        module procedure almost_equal_real32
        module procedure almost_equal_real64
    end interface almost_equal

    interface clamp
        module procedure clamp_int32
        module procedure clamp_int64
        module procedure clamp_real32
        module procedure clamp_real64
    end interface clamp
contains
    !> Test if `a` is divisible by `b`, where `a` and `b` are both reals.
    !> (KCW, 2024-05-25)
    pure elemental function almost_divisible_real32(a, b) result(almost_divisible)
        use, intrinsic :: iso_fortran_env, only: real32

        real(real32), intent(in) :: a, b
        logical :: almost_divisible

        real(real32) :: error_tolerance

        error_tolerance = epsilon(1.0_real32) * max(abs(a), abs(b))

        if (almost_equal(mod(abs(a), abs(b)), 0.0_real32, absolute_tolerance=error_tolerance) .or. &
            almost_equal(mod(abs(a), abs(b)), abs(b), absolute_tolerance=error_tolerance)) then
            almost_divisible = .true.

            return
        end if

        almost_divisible = .false.
    end function almost_divisible_real32

    !> Test if `a` is divisible by `b`, where `a` and `b` are both reals.
    !> (KCW, 2024-05-25)
    pure elemental function almost_divisible_real64(a, b) result(almost_divisible)
        use, intrinsic :: iso_fortran_env, only: real64

        real(real64), intent(in) :: a, b
        logical :: almost_divisible

        real(real64) :: error_tolerance

        error_tolerance = epsilon(1.0_real64) * max(abs(a), abs(b))

        if (almost_equal(mod(abs(a), abs(b)), 0.0_real64, absolute_tolerance=error_tolerance) .or. &
            almost_equal(mod(abs(a), abs(b)), abs(b), absolute_tolerance=error_tolerance)) then
            almost_divisible = .true.

            return
        end if

        almost_divisible = .false.
    end function almost_divisible_real64

    !> Test `a` and `b` for approximate equality, where `a` and `b` are both reals.
    !> (KCW, 2024-05-25)
    pure elemental function almost_equal_real32(a, b, absolute_tolerance, relative_tolerance) result(almost_equal)
        use, intrinsic :: iso_fortran_env, only: real32

        real(real32), intent(in) :: a, b
        real(real32), optional, intent(in) :: absolute_tolerance, relative_tolerance
        logical :: almost_equal

        real(real32) :: error_tolerance

        if (a /= a .or. b /= b) then
            ! NaN is not equal to anything, including itself.
            almost_equal = .false.

            return
        end if

        if (abs(a) > huge(a) .or. abs(b) > huge(b)) then
            ! Infinities of the same sign are equal to each other.
            ! Infinities of different signs are not equal to each other.
            ! An infinity is not equal to anything that is finite.
            almost_equal = (a == b)

            return
        end if

        if (present(relative_tolerance)) then
            error_tolerance = relative_tolerance * max(abs(a), abs(b))
        else
            error_tolerance = epsilon(1.0_real32) * max(abs(a), abs(b))
        end if

        if (present(absolute_tolerance)) then
            error_tolerance = max(absolute_tolerance, error_tolerance)
        else
            error_tolerance = max(epsilon(1.0_real32), error_tolerance)
        end if

        almost_equal = (abs(a - b) <= error_tolerance)
    end function almost_equal_real32

    !> Test `a` and `b` for approximate equality, where `a` and `b` are both reals.
    !> (KCW, 2024-05-25)
    pure elemental function almost_equal_real64(a, b, absolute_tolerance, relative_tolerance) result(almost_equal)
        use, intrinsic :: iso_fortran_env, only: real64

        real(real64), intent(in) :: a, b
        real(real64), optional, intent(in) :: absolute_tolerance, relative_tolerance
        logical :: almost_equal

        real(real64) :: error_tolerance

        if (a /= a .or. b /= b) then
            ! NaN is not equal to anything, including itself.
            almost_equal = .false.

            return
        end if

        if (abs(a) > huge(a) .or. abs(b) > huge(b)) then
            ! Infinities of the same sign are equal to each other.
            ! Infinities of different signs are not equal to each other.
            ! An infinity is not equal to anything that is finite.
            almost_equal = (a == b)

            return
        end if

        if (present(relative_tolerance)) then
            error_tolerance = relative_tolerance * max(abs(a), abs(b))
        else
            error_tolerance = epsilon(1.0_real64) * max(abs(a), abs(b))
        end if

        if (present(absolute_tolerance)) then
            error_tolerance = max(absolute_tolerance, error_tolerance)
        else
            error_tolerance = max(epsilon(1.0_real64), error_tolerance)
        end if

        almost_equal = (abs(a - b) <= error_tolerance)
    end function almost_equal_real64

    !> Clamp/Limit the value of `x` to the range of [`xmin`, `xmax`], where `x`, `xmin`, and `xmax` are all integers.
    !> No check is performed to ensure `xmin` < `xmax`.
    !> (KCW, 2025-07-16)
    pure elemental function clamp_int32(x, xmin, xmax) result(clamp)
        use, intrinsic :: iso_fortran_env, only: int32

        integer(int32), intent(in) :: x, xmin, xmax
        integer(int32) :: clamp

        clamp = max(min(x, xmax), xmin)
    end function clamp_int32

    !> Clamp/Limit the value of `x` to the range of [`xmin`, `xmax`], where `x`, `xmin`, and `xmax` are all integers.
    !> No check is performed to ensure `xmin` < `xmax`.
    !> (KCW, 2025-07-16)
    pure elemental function clamp_int64(x, xmin, xmax) result(clamp)
        use, intrinsic :: iso_fortran_env, only: int64

        integer(int64), intent(in) :: x, xmin, xmax
        integer(int64) :: clamp

        clamp = max(min(x, xmax), xmin)
    end function clamp_int64

    !> Clamp/Limit the value of `x` to the range of [`xmin`, `xmax`], where `x`, `xmin`, and `xmax` are all reals.
    !> No check is performed to ensure `xmin` < `xmax`.
    !> (KCW, 2025-07-16)
    pure elemental function clamp_real32(x, xmin, xmax) result(clamp)
        use, intrinsic :: iso_fortran_env, only: real32

        real(real32), intent(in) :: x, xmin, xmax
        real(real32) :: clamp

        clamp = max(min(x, xmax), xmin)
    end function clamp_real32

    !> Clamp/Limit the value of `x` to the range of [`xmin`, `xmax`], where `x`, `xmin`, and `xmax` are all reals.
    !> No check is performed to ensure `xmin` < `xmax`.
    !> (KCW, 2025-07-16)
    pure elemental function clamp_real64(x, xmin, xmax) result(clamp)
        use, intrinsic :: iso_fortran_env, only: real64

        real(real64), intent(in) :: x, xmin, xmax
        real(real64) :: clamp

        clamp = max(min(x, xmax), xmin)
    end function clamp_real64

    !> Return the index of unique elements in `array`, which can be any intrinsic data types, as an integer array.
    !> Please note that `array` must only have one dimension, and the unique elements are returned by their first occurrences
    !> in `array`.
    !> If `array` contains zero element or is of unsupported data types, an empty integer array is produced.
    !> For example, `index_unique([1, 2, 3, 1, 2, 3, 4, 5])` returns `[1, 2, 3, 7, 8]`.
    !> (KCW, 2024-03-22)
    pure function index_unique(array)
        use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64

        class(*), intent(in) :: array(:)
        integer, allocatable :: index_unique(:)

        character(:), allocatable :: array_c(:)
        integer :: i, n
        logical :: mask_unique(size(array))

        n = size(array)

        if (n == 0) then
            allocate(index_unique(0))

            return
        end if

        mask_unique = .false.

        select type (array)
            type is (character(*))
                ! Workaround for a bug in GNU Fortran >= 12. This is perhaps the manifestation of GCC Bugzilla Bug 100819.
                ! When a character string array is passed as the actual argument to an unlimited polymorphic dummy argument,
                ! its array index and length parameter are mishandled.
                allocate(character(len(array)) :: array_c(size(array)))

                array_c(:) = array(:)

                do i = 1, n
                    if (.not. any(array_c(i) == array_c .and. mask_unique)) then
                        mask_unique(i) = .true.
                    end if
                end do

                deallocate(array_c)
            type is (integer(int32))
                do i = 1, n
                    if (.not. any(array(i) == array .and. mask_unique)) then
                        mask_unique(i) = .true.
                    end if
                end do
            type is (integer(int64))
                do i = 1, n
                    if (.not. any(array(i) == array .and. mask_unique)) then
                        mask_unique(i) = .true.
                    end if
                end do
            type is (logical)
                do i = 1, n
                    if (.not. any((array(i) .eqv. array) .and. mask_unique)) then
                        mask_unique(i) = .true.
                    end if
                end do
            type is (real(real32))
                do i = 1, n
                    if (.not. any(array(i) == array .and. mask_unique)) then
                        mask_unique(i) = .true.
                    end if
                end do
            type is (real(real64))
                do i = 1, n
                    if (.not. any(array(i) == array .and. mask_unique)) then
                        mask_unique(i) = .true.
                    end if
                end do
            class default
                allocate(index_unique(0))

                return
        end select

        index_unique = pack([(i, i = 1, n)], mask_unique)
    end function index_unique

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
end module dyn_mpas_procedures
