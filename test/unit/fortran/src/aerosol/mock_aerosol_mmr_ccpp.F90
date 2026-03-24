!-----------------------------------------------------------------------
! Mock aerosol_mmr_ccpp module for unit testing bulk_aerosol_state.
!
! The real module retrieves aerosol mixing ratios from the CCPP
! constituents array by looking up indices that were resolved during
! initialization. This mock bypasses the index resolution and directly
! returns constituents(:,:,aer_idx) for a given aerosol index.
!
! This means the test constituents array should be set up so that
! constituents(:,:,i) contains the MMR for aerosol i.
!-----------------------------------------------------------------------
module aerosol_mmr_ccpp

  use shr_kind_mod, only: r8 => shr_kind_r8
  use ccpp_kinds,   only: kind_phys

  implicit none
  private

  ! Generic interface matching the real module
  interface rad_cnst_get_aer_mmr
     module procedure rad_cnst_get_aer_mmr_by_idx
  end interface

  public :: rad_cnst_get_aer_mmr

contains

  !-----------------------------------------------------------------------
  ! Mock rad_cnst_get_aer_mmr: directly index into constituents(:,:,aer_idx).
  ! In the real module, aer_idx is used to look up the resolved CCPP index.
  ! Here we short-circuit that and use aer_idx directly as the 3rd dimension.
  !-----------------------------------------------------------------------
  subroutine rad_cnst_get_aer_mmr_by_idx(list_idx, aer_idx, constituents, mmr)
    integer,                        intent(in)  :: list_idx
    integer,                        intent(in)  :: aer_idx
    real(kind_phys), target,        intent(in)  :: constituents(:,:,:)
    real(r8),                       pointer     :: mmr(:,:)

    mmr => constituents(:, :, aer_idx)
  end subroutine rad_cnst_get_aer_mmr_by_idx

end module aerosol_mmr_ccpp
