!-----------------------------------------------------------------------
! Mock cam_constituents for unit testing.
! Provides const_get_index as a no-op stub (returns idx = -1).
! Required by modal_aerosol_state_mod procedure-level use statements.
!-----------------------------------------------------------------------
module cam_constituents

  implicit none
  private

  public :: const_get_index

contains

  subroutine const_get_index(name, idx, abort)
    character(len=*), intent(in) :: name
    integer, intent(out) :: idx
    logical, intent(in), optional :: abort

    idx = -1
  end subroutine const_get_index

end module cam_constituents
