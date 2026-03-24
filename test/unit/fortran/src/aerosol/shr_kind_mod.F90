!-----------------------------------------------------------------------
! Local shr_kind_mod stub for unit testing.
! Provides the same kind parameters as CESM share/src/shr_kind_mod.F90
! so tests can build without checking out the share submodule.
!-----------------------------------------------------------------------
module shr_kind_mod

  use ISO_FORTRAN_ENV, only: shr_kind_r8 => REAL64

  implicit none
  private

  public :: shr_kind_r8

end module shr_kind_mod
