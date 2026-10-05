!-----------------------------------------------------------------------
! Mock physconst for unit testing.
! Provides physical constants used by aerosol modules.
!-----------------------------------------------------------------------
module physconst

  use shr_kind_mod, only: r8 => shr_kind_r8

  implicit none

  real(r8), public, parameter :: pi  = 3.14159265358979323846_r8
  real(r8), public, parameter :: rga = 1._r8 / 9.80616_r8
  real(r8), public, parameter :: rhoh2o = 1000._r8
  ! Dry air gas constant (J/kg/K) matching shr_const_rdair:
  real(r8), public, parameter :: rair = 287.04_r8

end module physconst
