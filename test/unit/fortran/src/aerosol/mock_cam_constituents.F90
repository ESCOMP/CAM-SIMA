!-----------------------------------------------------------------------
! Mock cam_constituents for unit testing.
! Provides const_get_index as a no-op stub (returns idx = -1) and
! const_molec_weight as a no-op stub (returns 0).
! Required by modal_aerosol_state_mod and modal_aerosol_properties_mod
! procedure-level use statements.
!-----------------------------------------------------------------------
module cam_constituents

  use shr_kind_mod, only: r8 => shr_kind_r8

  implicit none
  private

  public :: const_get_index
  public :: const_molec_weight

contains

  subroutine const_get_index(name, idx, abort)
    character(len=*), intent(in) :: name
    integer, intent(out) :: idx
    logical, intent(in), optional :: abort

    idx = -1
  end subroutine const_get_index

  real(r8) function const_molec_weight(const_ind)
    integer, intent(in) :: const_ind

    const_molec_weight = 0._r8
  end function const_molec_weight

end module cam_constituents
