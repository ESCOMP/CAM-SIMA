! Copyright (C) 2024 National Science Foundation-National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!--------------------------------------------------------------------------
!
! Tests of the orbital_data module.
!
!--------------------------------------------------------------------------

! Assert macros
#define ASSERT(x) if (.not.(x)) then; write(*,*) "Assertion failed[", __FILE__, ":", __LINE__, "]: x"; stop 1; endif
#define ASSERT_NEAR( a, b, abs_error ) if( (abs(a - b) >= abs_error) .and. (abs(a - b) /= 0.0) ) then; write(*,*) "Assertion failed[", __FILE__, ":", __LINE__, "]: a, b"; stop 1; endif

program test_orbital_data

  implicit none

  call test_orbital_data_functions()

contains

  subroutine test_orbital_data_functions

    use orbital_data
    use shr_kind_mod, only: R8 => SHR_KIND_R8
    implicit none

    integer, parameter :: NUMBER_OF_COLUMNS = 3
    real(kind=R8) :: calendar_day
    real(kind=R8) :: latitudes(NUMBER_OF_COLUMNS)
    real(kind=R8) :: longitudes(NUMBER_OF_COLUMNS)
    integer :: i

    calendar_day = 10.5_R8
    latitudes = [1.0_R8, 2.0_R8, 3.0_R8]
    longitudes = [4.0_R8, 5.0_R8, 6.0_R8]

    call orbital_data_init(NUMBER_OF_COLUMNS)

    ASSERT(allocated(solar_zenith_angle))
    ASSERT(size(solar_zenith_angle) == NUMBER_OF_COLUMNS)
    ASSERT(all(solar_zenith_angle == -1.0_R8))
    ASSERT(solar_declination == -1.0_R8)
    ASSERT(earth_sun_distance == -1.0_R8)

    call orbital_data_advance(calendar_day, latitudes, longitudes)

    ASSERT(allocated(solar_zenith_angle))
    ASSERT(size(solar_zenith_angle) == NUMBER_OF_COLUMNS)
    ASSERT_NEAR(solar_zenith_angle(1), acos(latitudes(1) + longitudes(1) + calendar_day * 21.0_R8), 1.0E-6_R8)
    ASSERT_NEAR(solar_zenith_angle(2), acos(latitudes(2) + longitudes(2) + calendar_day * 21.0_R8), 1.0E-6_R8)
    ASSERT_NEAR(solar_zenith_angle(3), acos(latitudes(3) + longitudes(3) + calendar_day * 21.0_R8), 1.0E-6_R8)
    ASSERT(solar_declination == calendar_day * 2.0_R8)
    ASSERT(earth_sun_distance == calendar_day * 3.0_R8)

  end subroutine test_orbital_data_functions

end program test_orbital_data