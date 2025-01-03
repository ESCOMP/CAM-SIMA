! Copyright (C) 2024 National Science Foundation-National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!--------------------------------------------------------------------------
!
! Stub modules for the orbital_data module tests.
!
!--------------------------------------------------------------------------

module shr_kind_mod
  implicit none
  integer, parameter :: SHR_KIND_R8 = selected_real_kind(15)
end module shr_kind_mod
module ccpp_kinds
  implicit none
  integer, parameter :: kind_phys = selected_real_kind(15)
end module ccpp_kinds
module cam_abortutils
  implicit none
  public :: check_allocate
contains
  subroutine check_allocate(error_code, subroutine_name, variable_name, file, line)
    integer, intent(in) :: error_code
    character(len=*), intent(in) :: subroutine_name
    character(len=*), intent(in) :: variable_name
    character(len=*), intent(in) :: file
    integer, intent(in) :: line
    if (error_code /= 0) then
      write(*,*) "Allocation of '", variable_name, "' failed with code ", error_code, " in ", subroutine_name, " at ", file, ":", line
      stop 3
    end if  
  end subroutine check_allocate
end module cam_abortutils
module shr_orb_mod
  use shr_kind_mod, only: R8 => SHR_KIND_R8
  implicit none
  real(kind=R8), parameter :: SHR_ORB_UNDEF_REAL = -1.0_R8
  public :: shr_orb_decl, shr_orb_cosz
contains
  subroutine shr_orb_decl(calendar_day, eccen, mvelpp, lambm0, obliqr, solar_declination, earth_sun_distance)
    real(kind=R8), intent(in) :: calendar_day
    real(kind=R8), intent(in) :: eccen
    real(kind=R8), intent(in) :: mvelpp
    real(kind=R8), intent(in) :: lambm0
    real(kind=R8), intent(in) :: obliqr
    real(kind=R8), intent(out) :: solar_declination
    real(kind=R8), intent(out) :: earth_sun_distance
    solar_declination = calendar_day * 2.0
    earth_sun_distance = calendar_day * 3.0
  end subroutine shr_orb_decl
  real(R8) pure function shr_orb_cosz(calendar_day, latitude, longitude, solar_declination)
    real(kind=R8), intent(in) :: calendar_day
    real(kind=R8), intent(in) :: latitude
    real(kind=R8), intent(in) :: longitude
    real(kind=R8), intent(in) :: solar_declination
    shr_orb_cosz = latitude + longitude + solar_declination
  end function shr_orb_cosz
end module shr_orb_mod
module cam_control_mod
  use shr_kind_mod, only: R8 => SHR_KIND_R8
  implicit none
  real(kind=R8) :: eccen = 2.0_R8
  real(kind=R8) :: mvelpp = 3.0_R8
  real(kind=R8) :: lambm0 = 4.0_R8
  real(kind=R8) :: obliqr = 5.0_R8
end module cam_control_mod