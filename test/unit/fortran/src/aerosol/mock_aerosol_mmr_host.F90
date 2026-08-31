!-----------------------------------------------------------------------
! Mock aerosol_mmr_host module for unit testing bulk_aerosol_state.
!
! The real module retrieves aerosol mixing ratios from the CCPP
! constituents array by looking up indices that were resolved during
! initialization. This mock bypasses the index resolution and directly
! returns constituents(:,:,aer_idx) for a given aerosol index.
!
! This means the test constituents array should be set up so that
! constituents(:,:,i) contains the MMR for aerosol i.
!
! Mirrors the real module's host-binding handle surface: states hold an
! opaque aero_host_binding_t built from the constituents array.
!-----------------------------------------------------------------------
module aerosol_mmr_host

  use shr_kind_mod, only: r8 => shr_kind_r8
  use ccpp_kinds,   only: kind_phys

  implicit none
  private

  ! Opaque host-binding handle matching the real module
  type :: aero_host_binding_t
     real(kind_phys), pointer :: constituents(:,:,:) => null()
  end type aero_host_binding_t

  ! Generic interface matching the real module
  interface rad_cnst_get_aer_mmr
     module procedure rad_cnst_get_aer_mmr_by_idx
     module procedure rad_cnst_get_mam_mmr_by_idx
     module procedure rad_cnst_get_aer_mmr_by_idx_host
     module procedure rad_cnst_get_mam_mmr_by_idx_host
  end interface

  interface rad_cnst_get_mode_num
     module procedure rad_cnst_get_mode_num_ccpp
     module procedure rad_cnst_get_mode_num_host
  end interface

  public :: aero_host_binding_t
  public :: aero_host_binding
  public :: rad_cnst_get_aer_mmr
  public :: rad_cnst_get_mode_num
  public :: get_mode_dry_diameter
  public :: get_mode_wet_diameter
  public :: get_mode_aer_water

contains

  !-----------------------------------------------------------------------
  ! Build a host-binding handle from the test constituents array.
  !-----------------------------------------------------------------------
  function aero_host_binding(constituents) result(host)
    real(kind_phys), pointer, intent(in) :: constituents(:,:,:)
    type(aero_host_binding_t) :: host

    host%constituents => constituents
  end function aero_host_binding

  !-----------------------------------------------------------------------
  ! Mode diameter / aerosol water accessors. Mirror the real module by
  ! returning the mock physics_types registry fields; the host handle is
  ! ignored (parity with the real interface). Tests exercising these paths
  ! must allocate and fill dgncur_a / dgncur_awet / qaerwat_aer.
  !-----------------------------------------------------------------------
  subroutine get_mode_dry_diameter(host, dgnum)
    use physics_types, only: dgncur_a
    type(aero_host_binding_t), intent(in) :: host
    real(r8),                  pointer    :: dgnum(:,:,:)

    dgnum => dgncur_a
  end subroutine get_mode_dry_diameter

  subroutine get_mode_wet_diameter(host, dgnumwet)
    use physics_types, only: dgncur_awet
    type(aero_host_binding_t), intent(in) :: host
    real(r8),                  pointer    :: dgnumwet(:,:,:)

    dgnumwet => dgncur_awet
  end subroutine get_mode_wet_diameter

  subroutine get_mode_aer_water(host, qaerwat)
    use physics_types, only: qaerwat_aer
    type(aero_host_binding_t), intent(in) :: host
    real(r8),                  pointer    :: qaerwat(:,:,:)

    qaerwat => qaerwat_aer
  end subroutine get_mode_aer_water

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

  subroutine rad_cnst_get_aer_mmr_by_idx_host(list_idx, aer_idx, host, mmr)
    integer,                   intent(in) :: list_idx
    integer,                   intent(in) :: aer_idx
    type(aero_host_binding_t), intent(in) :: host
    real(r8),                  pointer    :: mmr(:,:)

    call rad_cnst_get_aer_mmr_by_idx(list_idx, aer_idx, host%constituents, mmr)
  end subroutine rad_cnst_get_aer_mmr_by_idx_host

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

  subroutine rad_cnst_get_mam_mmr_by_idx_host(list_idx, mode_idx, spec_idx, phase, host, mmr)
    integer,                   intent(in) :: list_idx
    integer,                   intent(in) :: mode_idx
    integer,                   intent(in) :: spec_idx
    character(len=1),          intent(in) :: phase
    type(aero_host_binding_t), intent(in) :: host
    real(r8),                  pointer    :: mmr(:,:)

    call rad_cnst_get_mam_mmr_by_idx(list_idx, mode_idx, spec_idx, phase, host%constituents, mmr)
  end subroutine rad_cnst_get_mam_mmr_by_idx_host

  !-----------------------------------------------------------------------
  ! Mock rad_cnst_get_mode_num (modal): return number mixing ratio for mode.
  ! Number is at the start of each mode's block in the flat layout.
  !-----------------------------------------------------------------------
  subroutine rad_cnst_get_mode_num_ccpp(list_idx, mode_idx, phase, constituents, num)
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
  end subroutine rad_cnst_get_mode_num_ccpp

  subroutine rad_cnst_get_mode_num_host(list_idx, mode_idx, phase, host, num)
    integer,                   intent(in) :: list_idx
    integer,                   intent(in) :: mode_idx
    character(len=1),          intent(in) :: phase
    type(aero_host_binding_t), intent(in) :: host
    real(r8),                  pointer    :: num(:,:)

    call rad_cnst_get_mode_num_ccpp(list_idx, mode_idx, phase, host%constituents, num)
  end subroutine rad_cnst_get_mode_num_host

end module aerosol_mmr_host
