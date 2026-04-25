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
     module procedure rad_cnst_get_mam_mmr_by_idx
  end interface

  public :: rad_cnst_get_aer_mmr
  public :: rad_cnst_get_mode_num

contains

  !-----------------------------------------------------------------------
  ! Mock rad_cnst_get_aer_mmr (bulk): directly index into constituents(:,:,aer_idx).
  !-----------------------------------------------------------------------
  subroutine rad_cnst_get_aer_mmr_by_idx(list_idx, aer_idx, constituents, mmr)
    integer,                        intent(in)  :: list_idx
    integer,                        intent(in)  :: aer_idx
    real(kind_phys), target,        intent(in)  :: constituents(:,:,:)
    real(r8),                       pointer     :: mmr(:,:)

    mmr => constituents(:, :, aer_idx)
  end subroutine rad_cnst_get_aer_mmr_by_idx

  !-----------------------------------------------------------------------
  ! Mock rad_cnst_get_mam_mmr_by_idx (modal): compute flat index from
  ! mode_idx and spec_idx, return constituents(:,:,flat_idx).
  !
  ! Flat layout: for each mode m, offset(m) = 1 + sum_{i<m}(nspec(i)+1)
  ! Number is at offset(m), species at offset(m)+spec_idx.
  !-----------------------------------------------------------------------
  subroutine rad_cnst_get_mam_mmr_by_idx(list_idx, mode_idx, spec_idx, phase, constituents, mmr)
    use radiative_aerosol, only: mock_nmodes, mock_nspec

    integer,                        intent(in)  :: list_idx
    integer,                        intent(in)  :: mode_idx
    integer,                        intent(in)  :: spec_idx
    character(len=1),               intent(in)  :: phase
    real(kind_phys), target,        intent(in)  :: constituents(:,:,:)
    real(r8),                       pointer     :: mmr(:,:)

    integer :: flat_idx, m

    ! Compute flat offset for this mode
    flat_idx = 1
    do m = 1, mode_idx - 1
       flat_idx = flat_idx + mock_nspec(m) + 1
    end do
    ! Species are after the number slot
    flat_idx = flat_idx + spec_idx

    mmr => constituents(:, :, flat_idx)
  end subroutine rad_cnst_get_mam_mmr_by_idx

  !-----------------------------------------------------------------------
  ! Mock rad_cnst_get_mode_num (modal): return number mixing ratio for mode.
  ! Number is at the start of each mode's block in the flat layout.
  !-----------------------------------------------------------------------
  subroutine rad_cnst_get_mode_num(list_idx, mode_idx, phase, constituents, num)
    use radiative_aerosol, only: mock_nmodes, mock_nspec

    integer,                        intent(in)  :: list_idx
    integer,                        intent(in)  :: mode_idx
    character(len=1),               intent(in)  :: phase
    real(kind_phys), target,        intent(in)  :: constituents(:,:,:)
    real(r8),                       pointer     :: num(:,:)

    integer :: flat_idx, m

    flat_idx = 1
    do m = 1, mode_idx - 1
       flat_idx = flat_idx + mock_nspec(m) + 1
    end do

    num => constituents(:, :, flat_idx)
  end subroutine rad_cnst_get_mode_num

end module aerosol_mmr_ccpp
