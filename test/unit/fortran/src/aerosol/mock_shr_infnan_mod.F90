!-----------------------------------------------------------------------
! Mock shr_infnan_mod for unit testing.
! Provides shr_infnan_nan and assignment(=) interface for NaN values.
!-----------------------------------------------------------------------
module shr_infnan_mod

  use shr_kind_mod, only: r8 => shr_kind_r8
  use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan

  implicit none
  private

  ! Type to represent a NaN assignment source
  type, public :: shr_infnan_nan_type
     integer :: unused = 0
  end type shr_infnan_nan_type

  type(shr_infnan_nan_type), public, parameter :: shr_infnan_nan = shr_infnan_nan_type(0)

  public :: assignment(=)

  interface assignment(=)
     module procedure assign_nan_r8
  end interface

contains

  elemental subroutine assign_nan_r8(output, input)
    real(r8),                  intent(out) :: output
    type(shr_infnan_nan_type), intent(in)  :: input
    output = ieee_value(output, ieee_quiet_nan)
  end subroutine assign_nan_r8

end module shr_infnan_mod
