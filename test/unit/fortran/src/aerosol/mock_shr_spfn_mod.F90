!-----------------------------------------------------------------------
! Mock shr_spfn_mod for unit testing.
! Provides shr_spfn_erf wrapping the Fortran intrinsic erf.
!-----------------------------------------------------------------------
module shr_spfn_mod

  use shr_kind_mod, only: r8 => shr_kind_r8

  implicit none
  private

  public :: shr_spfn_erf

contains

  elemental real(r8) function shr_spfn_erf(x)
    real(r8), intent(in) :: x
    shr_spfn_erf = erf(x)
  end function shr_spfn_erf

end module shr_spfn_mod
