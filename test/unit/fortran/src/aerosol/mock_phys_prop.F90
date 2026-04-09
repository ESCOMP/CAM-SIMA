module phys_prop
  ! Mock stub for unit tests.
  ! Provides physprop_get_id (returns a deterministic index)
  ! and physprop_get that returns mock optics data when configured.
  !
  ! For tests that need optics data (e.g. insoluble_aerosol_optics),
  ! call setup_mock_physprop_optics() before constructing optics objects.
  use shr_kind_mod, only: r8 => shr_kind_r8

  implicit none
  private

  integer, parameter, public :: ot_length = 32

  public :: physprop_get_id
  public :: physprop_get
  public :: setup_mock_physprop_optics
  public :: cleanup_mock_physprop

  integer, save :: next_id = 0

  ! Mock optics data storage, indexed as (nwavbands, naero).
  ! The id passed to physprop_get is used as the aerosol index.
  integer, save :: mock_nswbands = 0
  integer, save :: mock_nlwbands = 0
  real(r8), allocatable, target, save :: mock_sw_nonhygro_ext(:,:)
  real(r8), allocatable, target, save :: mock_sw_nonhygro_ssa(:,:)
  real(r8), allocatable, target, save :: mock_sw_nonhygro_asm(:,:)
  real(r8), allocatable, target, save :: mock_lw_abs(:,:)

contains

  !-----------------------------------------------------------------------
  ! Configure mock optics tables for physprop_get to return.
  ! Arrays are dimensioned (nbands, naero).
  !-----------------------------------------------------------------------
  subroutine setup_mock_physprop_optics(nswbands, nlwbands, naero, &
       sw_ext, sw_ssa, sw_asm, lw_abs)
    integer, intent(in) :: nswbands, nlwbands, naero
    real(r8), intent(in), optional :: sw_ext(:,:)
    real(r8), intent(in), optional :: sw_ssa(:,:)
    real(r8), intent(in), optional :: sw_asm(:,:)
    real(r8), intent(in), optional :: lw_abs(:,:)

    call cleanup_mock_physprop()
    mock_nswbands = nswbands
    mock_nlwbands = nlwbands

    allocate(mock_sw_nonhygro_ext(nswbands, naero))
    allocate(mock_sw_nonhygro_ssa(nswbands, naero))
    allocate(mock_sw_nonhygro_asm(nswbands, naero))
    allocate(mock_lw_abs(nlwbands, naero))
    mock_sw_nonhygro_ext = 0._r8
    mock_sw_nonhygro_ssa = 0._r8
    mock_sw_nonhygro_asm = 0._r8
    mock_lw_abs = 0._r8

    if (present(sw_ext)) mock_sw_nonhygro_ext(:, 1:naero) = sw_ext
    if (present(sw_ssa)) mock_sw_nonhygro_ssa(:, 1:naero) = sw_ssa
    if (present(sw_asm)) mock_sw_nonhygro_asm(:, 1:naero) = sw_asm
    if (present(lw_abs)) mock_lw_abs(:, 1:naero)           = lw_abs
  end subroutine setup_mock_physprop_optics

  !-----------------------------------------------------------------------
  ! Clean up mock optics data.
  !-----------------------------------------------------------------------
  subroutine cleanup_mock_physprop()
    if (allocated(mock_sw_nonhygro_ext)) deallocate(mock_sw_nonhygro_ext)
    if (allocated(mock_sw_nonhygro_ssa)) deallocate(mock_sw_nonhygro_ssa)
    if (allocated(mock_sw_nonhygro_asm)) deallocate(mock_sw_nonhygro_asm)
    if (allocated(mock_lw_abs))          deallocate(mock_lw_abs)
    mock_nswbands = 0
    mock_nlwbands = 0
  end subroutine cleanup_mock_physprop

  integer function physprop_get_id(filename)
    character(len=*), intent(in) :: filename
    next_id = next_id + 1
    physprop_get_id = next_id
  end function physprop_get_id

  subroutine physprop_get(id, sourcefile, opticstype, &
                          sw_hygro_ext, sw_hygro_ssa, sw_hygro_asm, lw_hygro_abs, &
                          sw_hygro_ext_wtp, sw_hygro_ssa_wtp, sw_hygro_asm_wtp, lw_hygro_abs_wtp, &
                          sw_nonhygro_ext, sw_nonhygro_ssa, sw_nonhygro_asm, &
                          sw_nonhygro_scat, sw_nonhygro_ascat, lw_abs, &
                          refindex_aer_sw, refindex_aer_lw, &
                          r_sw_ext, r_sw_scat, r_sw_ascat, r_lw_abs, mu, &
                          extpsw, abspsw, asmpsw, absplw, refrtabsw, &
                          refitabsw, refrtablw, refitablw, &
                          aername, density_aer, hygro_aer, dryrad_aer, dispersion_aer, &
                          num_to_mass_aer, ncoef, prefr, prefi, sigmag, &
                          dgnum, dgnumlo, dgnumhi, rhcrystal, rhdeliques, &
                          extpsw2, abspsw2, asmpsw2, absplw2, corefrac, nfrac, &
                          wgtpct, bcdust, kap, relh, &
                          nkap, nwtp, nbcdust, nrelh, &
                          sw_hygro_coreshell_ext, sw_hygro_coreshell_ssa, &
                          sw_hygro_coreshell_asm, lw_hygro_coreshell_abs)
    ! No-op stub: all optional arguments are simply ignored.
    ! Tests that need real optics data should mock at the radiative_aerosol level.
    integer, intent(in)  :: id
    character(len=256),       optional, intent(out) :: sourcefile
    character(len=ot_length), optional, intent(out) :: opticstype
    real(r8),          optional, pointer     :: sw_hygro_ext(:,:)
    real(r8),          optional, pointer     :: sw_hygro_ssa(:,:)
    real(r8),          optional, pointer     :: sw_hygro_asm(:,:)
    real(r8),          optional, pointer     :: lw_hygro_abs(:,:)
    real(r8),          optional, pointer     :: sw_hygro_ext_wtp(:,:)
    real(r8),          optional, pointer     :: sw_hygro_ssa_wtp(:,:)
    real(r8),          optional, pointer     :: sw_hygro_asm_wtp(:,:)
    real(r8),          optional, pointer     :: lw_hygro_abs_wtp(:,:)
    real(r8),          optional, pointer     :: sw_nonhygro_ext(:)
    real(r8),          optional, pointer     :: sw_nonhygro_ssa(:)
    real(r8),          optional, pointer     :: sw_nonhygro_asm(:)
    real(r8),          optional, pointer     :: sw_nonhygro_scat(:)
    real(r8),          optional, pointer     :: sw_nonhygro_ascat(:)
    real(r8),          optional, pointer     :: lw_abs(:)
    complex(r8),       optional, pointer     :: refindex_aer_sw(:)
    complex(r8),       optional, pointer     :: refindex_aer_lw(:)
    real(r8),          optional, pointer     :: r_sw_ext(:,:)
    real(r8),          optional, pointer     :: r_sw_scat(:,:)
    real(r8),          optional, pointer     :: r_sw_ascat(:,:)
    real(r8),          optional, pointer     :: r_lw_abs(:,:)
    real(r8),          optional, pointer     :: mu(:)
    real(r8),          optional, pointer     :: extpsw(:,:,:,:)
    real(r8),          optional, pointer     :: abspsw(:,:,:,:)
    real(r8),          optional, pointer     :: asmpsw(:,:,:,:)
    real(r8),          optional, pointer     :: absplw(:,:,:,:)
    real(r8),          optional, pointer     :: refrtabsw(:,:)
    real(r8),          optional, pointer     :: refitabsw(:,:)
    real(r8),          optional, pointer     :: refrtablw(:,:)
    real(r8),          optional, pointer     :: refitablw(:,:)
    character(len=20), optional, intent(out) :: aername
    real(r8),          optional, intent(out) :: density_aer
    real(r8),          optional, intent(out) :: hygro_aer
    real(r8),          optional, intent(out) :: dryrad_aer
    real(r8),          optional, intent(out) :: dispersion_aer
    real(r8),          optional, intent(out) :: num_to_mass_aer
    integer,           optional, intent(out) :: ncoef
    integer,           optional, intent(out) :: prefr
    integer,           optional, intent(out) :: prefi
    real(r8),          optional, intent(out) :: sigmag
    real(r8),          optional, intent(out) :: dgnum
    real(r8),          optional, intent(out) :: dgnumlo
    real(r8),          optional, intent(out) :: dgnumhi
    real(r8),          optional, intent(out) :: rhcrystal
    real(r8),          optional, intent(out) :: rhdeliques
    real(r8),          optional, pointer     :: extpsw2(:,:,:,:)
    real(r8),          optional, pointer     :: abspsw2(:,:,:,:)
    real(r8),          optional, pointer     :: asmpsw2(:,:,:,:)
    real(r8),          optional, pointer     :: absplw2(:,:,:,:)
    real(r8),          optional, pointer     :: corefrac(:)
    integer,           optional, intent(out) :: nfrac
    real(r8),          optional, pointer     :: wgtpct(:)
    real(r8),          optional, pointer     :: bcdust(:)
    real(r8),          optional, pointer     :: kap(:)
    real(r8),          optional, pointer     :: relh(:)
    integer,           optional, intent(out) :: nkap
    integer,           optional, intent(out) :: nwtp
    integer,           optional, intent(out) :: nbcdust
    integer,           optional, intent(out) :: nrelh
    real(r8),          optional, pointer     :: sw_hygro_coreshell_ext(:,:,:,:,:)
    real(r8),          optional, pointer     :: sw_hygro_coreshell_ssa(:,:,:,:,:)
    real(r8),          optional, pointer     :: sw_hygro_coreshell_asm(:,:,:,:,:)
    real(r8),          optional, pointer     :: lw_hygro_coreshell_abs(:,:,:,:,:)

    ! Return mock optics data when available.
    ! The id maps to the aerosol index (1-based).
    if (present(sw_nonhygro_ext)) then
       if (allocated(mock_sw_nonhygro_ext)) then
          sw_nonhygro_ext => mock_sw_nonhygro_ext(:, id)
       else
          nullify(sw_nonhygro_ext)
       end if
    end if
    if (present(sw_nonhygro_ssa)) then
       if (allocated(mock_sw_nonhygro_ssa)) then
          sw_nonhygro_ssa => mock_sw_nonhygro_ssa(:, id)
       else
          nullify(sw_nonhygro_ssa)
       end if
    end if
    if (present(sw_nonhygro_asm)) then
       if (allocated(mock_sw_nonhygro_asm)) then
          sw_nonhygro_asm => mock_sw_nonhygro_asm(:, id)
       else
          nullify(sw_nonhygro_asm)
       end if
    end if
    if (present(lw_abs)) then
       if (allocated(mock_lw_abs)) then
          lw_abs => mock_lw_abs(:, id)
       else
          nullify(lw_abs)
       end if
    end if
  end subroutine physprop_get

end module phys_prop
