!-----------------------------------------------------------------------
! Mock radiative_aerosol module for unit testing aerosol properties.
!
! This mock replaces the real radiative_aerosol facade module. The real
! module delegates to phys_prop (which reads NetCDF
! physprop files via PIO). This mock provides configurable in-memory data
! via setup_mock_rad_aer() / setup_mock_modal_rad_aer(),
! removing all file I/O dependencies.
!
! Supported interfaces (matching the real module's generic interfaces):
!   rad_aer_get_info(list_idx, naero=...)
!   rad_aer_get_info(list_idx, m_idx, mode_type=..., nspec=...)
!   rad_aer_get_info(list_idx, m_idx, s_idx, spec_type=...)
!   rad_aer_get_props(list_idx, aer_idx, density_aer=..., hygro_aer=..., ...)
!   rad_aer_get_props(list_idx, mode_idx, spec_idx, density_aer=..., ...)
!-----------------------------------------------------------------------
module radiative_aerosol

  use shr_kind_mod, only: r8 => shr_kind_r8

  implicit none
  private

  ! Maximum number of mock aerosols
  integer, parameter :: max_mock_aero = 10
  integer, parameter :: max_mock_modes = 10
  integer, parameter :: max_mock_spec = 10

  ! Mock data storage for bulk aerosols (populated by setup_mock_rad_aer)
  integer :: mock_naero = 0
  real(r8) :: mock_density(max_mock_aero) = 0._r8
  real(r8) :: mock_hygro(max_mock_aero) = 0._r8
  real(r8) :: mock_dispersion(max_mock_aero) = 1._r8
  real(r8) :: mock_dryrad(max_mock_aero) = 0._r8
  real(r8) :: mock_num_to_mass(max_mock_aero) = 0._r8
  character(len=20) :: mock_aername(max_mock_aero) = ''
  character(len=32) :: mock_opticstype(max_mock_aero) = ''

  ! Mock optics table storage (non-hygroscopic / insoluble)
  real(r8), allocatable, target :: mock_sw_nonhygro_ext(:,:)  ! (nswbands, max_mock_aero)
  real(r8), allocatable, target :: mock_sw_nonhygro_ssa(:,:)
  real(r8), allocatable, target :: mock_sw_nonhygro_asm(:,:)
  real(r8), allocatable, target :: mock_lw_ext(:,:)           ! (nlwbands, max_mock_aero)

  ! Mock optics table storage (hygroscopic)
  real(r8), allocatable, target :: mock_sw_hygro_ext(:,:,:)   ! (nrh, nswbands, max_mock_aero)
  real(r8), allocatable, target :: mock_sw_hygro_ssa(:,:,:)
  real(r8), allocatable, target :: mock_sw_hygro_asm(:,:,:)
  real(r8), allocatable, target :: mock_lw_hygro_ext(:,:,:)

  ! Mock refractive index storage
  complex(r8), allocatable, target :: mock_refindex_sw(:,:)   ! (nswbands, max_mock_aero)
  complex(r8), allocatable, target :: mock_refindex_lw(:,:)   ! (nlwbands, max_mock_aero)

  ! Mock data storage for modal aerosols (populated by setup_mock_modal_rad_aer)
  integer, public :: mock_nmodes = 0
  integer, public :: mock_nspec(max_mock_modes) = 0
  character(len=32) :: mock_mode_type(max_mock_modes) = ''
  character(len=32) :: mock_num_name(max_mock_modes) = ''
  character(len=32) :: mock_num_name_cw(max_mock_modes) = ''
  real(r8) :: mock_sigmag(max_mock_modes) = 0._r8
  real(r8) :: mock_dgnum(max_mock_modes) = 0._r8
  real(r8) :: mock_dgnumlo(max_mock_modes) = 0._r8
  real(r8) :: mock_dgnumhi(max_mock_modes) = 0._r8
  real(r8) :: mock_rhcrystal(max_mock_modes) = 0._r8
  real(r8) :: mock_rhdeliques(max_mock_modes) = 0._r8
  character(len=32) :: mock_spec_type(max_mock_modes, max_mock_spec) = ''
  character(len=32) :: mock_spec_name(max_mock_modes, max_mock_spec) = ''
  character(len=32) :: mock_spec_name_cw(max_mock_modes, max_mock_spec) = ''
  real(r8) :: mock_spec_density(max_mock_modes, max_mock_spec) = 0._r8
  real(r8) :: mock_spec_hygro(max_mock_modes, max_mock_spec) = 0._r8

  ! Generic interfaces matching the real module
  interface rad_aer_get_info
     module procedure rad_aer_get_info_basic
     module procedure rad_aer_get_info_by_mode
     module procedure rad_aer_get_info_by_mode_spec
  end interface

  interface rad_aer_get_props
     module procedure rad_aer_get_props_by_idx
     module procedure rad_aer_get_mam_props_by_idx
  end interface

  public :: rad_aer_get_info
  public :: rad_aer_get_props
  public :: setup_mock_rad_aer
  public :: setup_mock_modal_rad_aer
  public :: setup_mock_optics_tables
  public :: cleanup_mock_rad_aer
  public :: rad_aer_bulk_physprop_id
  public :: rad_aer_get_mode_props
  public :: rad_aer_mode_physprop_id

contains

  !-----------------------------------------------------------------------
  ! Configure mock data for a set of bulk aerosols.
  ! Call this from your test before constructing bulk_aerosol_properties.
  !-----------------------------------------------------------------------
  subroutine setup_mock_rad_aer(naero, density, hygro, dispersion, aername, opticstype, &
                                 dryrad, num_to_mass)
    integer,          intent(in) :: naero
    real(r8),         intent(in) :: density(:)
    real(r8),         intent(in) :: hygro(:)
    real(r8),         intent(in) :: dispersion(:)
    character(len=*), intent(in) :: aername(:)
    character(len=*), intent(in) :: opticstype(:)
    real(r8),         intent(in), optional :: dryrad(:)
    real(r8),         intent(in), optional :: num_to_mass(:)

    mock_naero = naero
    mock_density(1:naero) = density(1:naero)
    mock_hygro(1:naero) = hygro(1:naero)
    mock_dispersion(1:naero) = dispersion(1:naero)
    mock_aername(1:naero) = aername(1:naero)
    mock_opticstype(1:naero) = opticstype(1:naero)
    if (present(dryrad)) mock_dryrad(1:naero) = dryrad(1:naero)
    if (present(num_to_mass)) mock_num_to_mass(1:naero) = num_to_mass(1:naero)
  end subroutine setup_mock_rad_aer

  !-----------------------------------------------------------------------
  ! Configure mock optics tables for non-hygroscopic aerosols.
  ! Arrays are (nbands, naero).
  !-----------------------------------------------------------------------
  subroutine setup_mock_optics_tables(nswbands, nlwbands, &
       sw_ext, sw_ssa, sw_asm, lw_abs)
    integer, intent(in) :: nswbands, nlwbands
    real(r8), intent(in), optional :: sw_ext(:,:)   ! (nswbands, naero)
    real(r8), intent(in), optional :: sw_ssa(:,:)
    real(r8), intent(in), optional :: sw_asm(:,:)
    real(r8), intent(in), optional :: lw_abs(:,:)   ! (nlwbands, naero)

    if (allocated(mock_sw_nonhygro_ext)) deallocate(mock_sw_nonhygro_ext)
    if (allocated(mock_sw_nonhygro_ssa)) deallocate(mock_sw_nonhygro_ssa)
    if (allocated(mock_sw_nonhygro_asm)) deallocate(mock_sw_nonhygro_asm)
    if (allocated(mock_lw_ext))          deallocate(mock_lw_ext)

    allocate(mock_sw_nonhygro_ext(nswbands, max_mock_aero))
    allocate(mock_sw_nonhygro_ssa(nswbands, max_mock_aero))
    allocate(mock_sw_nonhygro_asm(nswbands, max_mock_aero))
    allocate(mock_lw_ext(nlwbands, max_mock_aero))
    mock_sw_nonhygro_ext = 0._r8
    mock_sw_nonhygro_ssa = 0._r8
    mock_sw_nonhygro_asm = 0._r8
    mock_lw_ext = 0._r8

    if (present(sw_ext)) mock_sw_nonhygro_ext(:, 1:mock_naero) = sw_ext
    if (present(sw_ssa)) mock_sw_nonhygro_ssa(:, 1:mock_naero) = sw_ssa
    if (present(sw_asm)) mock_sw_nonhygro_asm(:, 1:mock_naero) = sw_asm
    if (present(lw_abs)) mock_lw_ext(:, 1:mock_naero)          = lw_abs
  end subroutine setup_mock_optics_tables

  !-----------------------------------------------------------------------
  ! Configure mock data for modal aerosols.
  !-----------------------------------------------------------------------
  subroutine setup_mock_modal_rad_aer(nmodes, nspec, mode_type, &
       num_name, num_name_cw, sigmag, dgnum, dgnumlo, dgnumhi, &
       rhcrystal, rhdeliques, spec_type, spec_name, spec_name_cw, &
       spec_density, spec_hygro)
    integer,          intent(in) :: nmodes
    integer,          intent(in) :: nspec(:)
    character(len=*), intent(in) :: mode_type(:)
    character(len=*), intent(in) :: num_name(:)
    character(len=*), intent(in) :: num_name_cw(:)
    real(r8),         intent(in) :: sigmag(:)
    real(r8),         intent(in) :: dgnum(:)
    real(r8),         intent(in) :: dgnumlo(:)
    real(r8),         intent(in) :: dgnumhi(:)
    real(r8),         intent(in) :: rhcrystal(:)
    real(r8),         intent(in) :: rhdeliques(:)
    character(len=*), intent(in) :: spec_type(:,:)
    character(len=*), intent(in) :: spec_name(:,:)
    character(len=*), intent(in) :: spec_name_cw(:,:)
    real(r8),         intent(in) :: spec_density(:,:)
    real(r8),         intent(in) :: spec_hygro(:,:)

    integer :: m, s

    mock_nmodes = nmodes
    mock_nspec(1:nmodes) = nspec(1:nmodes)
    mock_mode_type(1:nmodes) = mode_type(1:nmodes)
    mock_num_name(1:nmodes) = num_name(1:nmodes)
    mock_num_name_cw(1:nmodes) = num_name_cw(1:nmodes)
    mock_sigmag(1:nmodes) = sigmag(1:nmodes)
    mock_dgnum(1:nmodes) = dgnum(1:nmodes)
    mock_dgnumlo(1:nmodes) = dgnumlo(1:nmodes)
    mock_dgnumhi(1:nmodes) = dgnumhi(1:nmodes)
    mock_rhcrystal(1:nmodes) = rhcrystal(1:nmodes)
    mock_rhdeliques(1:nmodes) = rhdeliques(1:nmodes)
    do m = 1, nmodes
       do s = 1, nspec(m)
          mock_spec_type(m, s) = spec_type(m, s)
          mock_spec_name(m, s) = spec_name(m, s)
          mock_spec_name_cw(m, s) = spec_name_cw(m, s)
          mock_spec_density(m, s) = spec_density(m, s)
          mock_spec_hygro(m, s) = spec_hygro(m, s)
       end do
    end do
  end subroutine setup_mock_modal_rad_aer

  !-----------------------------------------------------------------------
  ! Mock rad_aer_get_info_by_mode: return mode-level info.
  !-----------------------------------------------------------------------
  subroutine rad_aer_get_info_by_mode(list_idx, m_idx, mode_type, num_name, num_name_cw, nspec)
    integer,                     intent(in)  :: list_idx
    integer,                     intent(in)  :: m_idx
    character(len=32), optional, intent(out) :: mode_type
    character(len=32), optional, intent(out) :: num_name
    character(len=32), optional, intent(out) :: num_name_cw
    integer,           optional, intent(out) :: nspec

    if (present(mode_type))   mode_type   = mock_mode_type(m_idx)
    if (present(num_name))    num_name    = mock_num_name(m_idx)
    if (present(num_name_cw)) num_name_cw = mock_num_name_cw(m_idx)
    if (present(nspec))       nspec       = mock_nspec(m_idx)
  end subroutine rad_aer_get_info_by_mode

  !-----------------------------------------------------------------------
  ! Mock rad_aer_get_info_by_mode_spec: return species-level info.
  !-----------------------------------------------------------------------
  subroutine rad_aer_get_info_by_mode_spec(list_idx, m_idx, s_idx, &
       spec_type, spec_name, spec_name_cw)
    integer,                     intent(in)  :: list_idx
    integer,                     intent(in)  :: m_idx
    integer,                     intent(in)  :: s_idx
    character(len=32), optional, intent(out) :: spec_type
    character(len=32), optional, intent(out) :: spec_name
    character(len=32), optional, intent(out) :: spec_name_cw

    if (present(spec_type))    spec_type    = mock_spec_type(m_idx, s_idx)
    if (present(spec_name))    spec_name    = mock_spec_name(m_idx, s_idx)
    if (present(spec_name_cw)) spec_name_cw = mock_spec_name_cw(m_idx, s_idx)
  end subroutine rad_aer_get_info_by_mode_spec

  !-----------------------------------------------------------------------
  ! Mock rad_aer_get_mam_props_by_idx: return per-species modal properties.
  !-----------------------------------------------------------------------
  subroutine rad_aer_get_mam_props_by_idx(list_idx, mode_idx, spec_idx, &
       opticstype, &
       sw_hygro_ext, sw_hygro_ssa, sw_hygro_asm, lw_hygro_ext, &
       sw_nonhygro_ext, sw_nonhygro_ssa, sw_nonhygro_asm, &
       sw_nonhygro_scat, sw_nonhygro_ascat, lw_ext, &
       refindex_aer_sw, refindex_aer_lw, &
       r_sw_ext, r_sw_scat, r_sw_ascat, r_lw_abs, mu, &
       aername, density_aer, hygro_aer, dryrad_aer, dispersion_aer, &
       num_to_mass_aer, spectype)
    use phys_prop, only: ot_length

    integer,                     intent(in)  :: list_idx
    integer,                     intent(in)  :: mode_idx
    integer,                     intent(in)  :: spec_idx
    character(len=ot_length), optional, intent(out) :: opticstype
    real(r8),          optional, pointer     :: sw_hygro_ext(:,:)
    real(r8),          optional, pointer     :: sw_hygro_ssa(:,:)
    real(r8),          optional, pointer     :: sw_hygro_asm(:,:)
    real(r8),          optional, pointer     :: lw_hygro_ext(:,:)
    real(r8),          optional, pointer     :: sw_nonhygro_ext(:)
    real(r8),          optional, pointer     :: sw_nonhygro_ssa(:)
    real(r8),          optional, pointer     :: sw_nonhygro_asm(:)
    real(r8),          optional, pointer     :: sw_nonhygro_scat(:)
    real(r8),          optional, pointer     :: sw_nonhygro_ascat(:)
    real(r8),          optional, pointer     :: lw_ext(:)
    complex(r8),       optional, pointer     :: refindex_aer_sw(:)
    complex(r8),       optional, pointer     :: refindex_aer_lw(:)
    real(r8),          optional, pointer     :: r_sw_ext(:,:)
    real(r8),          optional, pointer     :: r_sw_scat(:,:)
    real(r8),          optional, pointer     :: r_sw_ascat(:,:)
    real(r8),          optional, pointer     :: r_lw_abs(:,:)
    real(r8),          optional, pointer     :: mu(:)
    character(len=20), optional, intent(out) :: aername
    real(r8),          optional, intent(out) :: density_aer
    real(r8),          optional, intent(out) :: hygro_aer
    real(r8),          optional, intent(out) :: dryrad_aer
    real(r8),          optional, intent(out) :: dispersion_aer
    real(r8),          optional, intent(out) :: num_to_mass_aer
    character(len=32), optional, intent(out) :: spectype

    if (present(density_aer)) density_aer = mock_spec_density(mode_idx, spec_idx)
    if (present(hygro_aer))   hygro_aer   = mock_spec_hygro(mode_idx, spec_idx)
    if (present(spectype))    spectype     = mock_spec_type(mode_idx, spec_idx)

    ! Optics pointers not yet mocked for modal - nullify
    if (present(sw_hygro_ext))     nullify(sw_hygro_ext)
    if (present(sw_hygro_ssa))     nullify(sw_hygro_ssa)
    if (present(sw_hygro_asm))     nullify(sw_hygro_asm)
    if (present(lw_hygro_ext))     nullify(lw_hygro_ext)
    if (present(sw_nonhygro_ext))  nullify(sw_nonhygro_ext)
    if (present(sw_nonhygro_ssa))  nullify(sw_nonhygro_ssa)
    if (present(sw_nonhygro_asm))  nullify(sw_nonhygro_asm)
    if (present(sw_nonhygro_scat)) nullify(sw_nonhygro_scat)
    if (present(sw_nonhygro_ascat)) nullify(sw_nonhygro_ascat)
    if (present(lw_ext))           nullify(lw_ext)
    if (present(refindex_aer_sw))  nullify(refindex_aer_sw)
    if (present(refindex_aer_lw))  nullify(refindex_aer_lw)
    if (present(r_sw_ext))         nullify(r_sw_ext)
    if (present(r_sw_scat))        nullify(r_sw_scat)
    if (present(r_sw_ascat))       nullify(r_sw_ascat)
    if (present(r_lw_abs))         nullify(r_lw_abs)
    if (present(mu))               nullify(mu)
  end subroutine rad_aer_get_mam_props_by_idx

  !-----------------------------------------------------------------------
  ! Mock rad_aer_get_mode_props: return mode-level physical properties.
  !-----------------------------------------------------------------------
  subroutine rad_aer_get_mode_props(list_idx, mode_idx, opticstype, &
       extpsw, abspsw, asmpsw, absplw, refrtabsw, &
       refitabsw, refrtablw, refitablw, ncoef, prefr, &
       prefi, sigmag, dgnum, dgnumlo, dgnumhi, &
       rhcrystal, rhdeliques)
    use phys_prop, only: ot_length

    integer,             intent(in)  :: list_idx
    integer,             intent(in)  :: mode_idx
    character(len=ot_length), optional, intent(out) :: opticstype
    real(r8),  optional, pointer     :: extpsw(:,:,:,:)
    real(r8),  optional, pointer     :: abspsw(:,:,:,:)
    real(r8),  optional, pointer     :: asmpsw(:,:,:,:)
    real(r8),  optional, pointer     :: absplw(:,:,:,:)
    real(r8),  optional, pointer     :: refrtabsw(:,:)
    real(r8),  optional, pointer     :: refitabsw(:,:)
    real(r8),  optional, pointer     :: refrtablw(:,:)
    real(r8),  optional, pointer     :: refitablw(:,:)
    integer,   optional, intent(out) :: ncoef
    integer,   optional, intent(out) :: prefr
    integer,   optional, intent(out) :: prefi
    real(r8),  optional, intent(out) :: sigmag
    real(r8),  optional, intent(out) :: dgnum
    real(r8),  optional, intent(out) :: dgnumlo
    real(r8),  optional, intent(out) :: dgnumhi
    real(r8),  optional, intent(out) :: rhcrystal
    real(r8),  optional, intent(out) :: rhdeliques

    if (present(sigmag))      sigmag      = mock_sigmag(mode_idx)
    if (present(dgnum))       dgnum       = mock_dgnum(mode_idx)
    if (present(dgnumlo))     dgnumlo     = mock_dgnumlo(mode_idx)
    if (present(dgnumhi))     dgnumhi     = mock_dgnumhi(mode_idx)
    if (present(rhcrystal))   rhcrystal   = mock_rhcrystal(mode_idx)
    if (present(rhdeliques))  rhdeliques  = mock_rhdeliques(mode_idx)

    ! Pointer/optics arguments not yet mocked - nullify
    if (present(extpsw))    nullify(extpsw)
    if (present(abspsw))    nullify(abspsw)
    if (present(asmpsw))    nullify(asmpsw)
    if (present(absplw))    nullify(absplw)
    if (present(refrtabsw)) nullify(refrtabsw)
    if (present(refitabsw)) nullify(refitabsw)
    if (present(refrtablw)) nullify(refrtablw)
    if (present(refitablw)) nullify(refitablw)
  end subroutine rad_aer_get_mode_props

  !-----------------------------------------------------------------------
  ! Mock rad_aer_mode_physprop_id: return mode_idx as the physprop ID.
  !-----------------------------------------------------------------------
  integer function rad_aer_mode_physprop_id(list_idx, mode_idx)
    integer, intent(in) :: list_idx
    integer, intent(in) :: mode_idx

    rad_aer_mode_physprop_id = mode_idx
  end function rad_aer_mode_physprop_id

  !-----------------------------------------------------------------------
  ! Clean up mock data.
  !-----------------------------------------------------------------------
  subroutine cleanup_mock_rad_aer()
    mock_naero = 0
    mock_nmodes = 0
    mock_nspec(:) = 0
    if (allocated(mock_sw_nonhygro_ext)) deallocate(mock_sw_nonhygro_ext)
    if (allocated(mock_sw_nonhygro_ssa)) deallocate(mock_sw_nonhygro_ssa)
    if (allocated(mock_sw_nonhygro_asm)) deallocate(mock_sw_nonhygro_asm)
    if (allocated(mock_lw_ext))          deallocate(mock_lw_ext)
    if (allocated(mock_sw_hygro_ext))    deallocate(mock_sw_hygro_ext)
    if (allocated(mock_sw_hygro_ssa))    deallocate(mock_sw_hygro_ssa)
    if (allocated(mock_sw_hygro_asm))    deallocate(mock_sw_hygro_asm)
    if (allocated(mock_lw_hygro_ext))    deallocate(mock_lw_hygro_ext)
    if (allocated(mock_refindex_sw))     deallocate(mock_refindex_sw)
    if (allocated(mock_refindex_lw))     deallocate(mock_refindex_lw)
  end subroutine cleanup_mock_rad_aer

  !-----------------------------------------------------------------------
  ! Mock rad_aer_get_info: return number of aerosols.
  !-----------------------------------------------------------------------
  subroutine rad_aer_get_info_basic(list_idx, aernames, naero, nmodes, nbins)
    integer,                     intent(in)  :: list_idx
    character(len=64), optional, intent(out) :: aernames(:)
    integer,           optional, intent(out) :: naero
    integer,           optional, intent(out) :: nmodes
    integer,           optional, intent(out) :: nbins

    integer :: i

    if (present(naero)) naero = mock_naero
    if (present(nmodes)) nmodes = mock_nmodes
    if (present(nbins)) nbins = 0
    if (present(aernames)) then
       do i = 1, mock_naero
          aernames(i) = mock_aername(i)
       end do
    end if
  end subroutine rad_aer_get_info_basic

  !-----------------------------------------------------------------------
  ! Mock rad_aer_get_props: return per-aerosol properties from mock data.
  ! Matches the interface of rad_aer_get_props_by_idx in the real module.
  !-----------------------------------------------------------------------
  subroutine rad_aer_get_props_by_idx(list_idx, &
       aer_idx, opticstype, &
       sw_hygro_ext, sw_hygro_ssa, sw_hygro_asm, lw_hygro_ext, &
       sw_nonhygro_ext, sw_nonhygro_ssa, sw_nonhygro_asm, &
       sw_nonhygro_scat, sw_nonhygro_ascat, lw_ext, &
       refindex_aer_sw, refindex_aer_lw, &
       r_sw_ext, r_sw_scat, r_sw_ascat, r_lw_abs, mu, &
       aername, density_aer, hygro_aer, dryrad_aer, dispersion_aer, num_to_mass_aer)

    integer,                     intent(in)  :: list_idx
    integer,                     intent(in)  :: aer_idx
    character(len=*),  optional, intent(out) :: opticstype
    real(r8),          optional, pointer     :: sw_hygro_ext(:,:)
    real(r8),          optional, pointer     :: sw_hygro_ssa(:,:)
    real(r8),          optional, pointer     :: sw_hygro_asm(:,:)
    real(r8),          optional, pointer     :: lw_hygro_ext(:,:)
    real(r8),          optional, pointer     :: sw_nonhygro_ext(:)
    real(r8),          optional, pointer     :: sw_nonhygro_ssa(:)
    real(r8),          optional, pointer     :: sw_nonhygro_asm(:)
    real(r8),          optional, pointer     :: sw_nonhygro_scat(:)
    real(r8),          optional, pointer     :: sw_nonhygro_ascat(:)
    real(r8),          optional, pointer     :: lw_ext(:)
    complex(r8),       optional, pointer     :: refindex_aer_sw(:)
    complex(r8),       optional, pointer     :: refindex_aer_lw(:)
    real(r8),          optional, pointer     :: r_sw_ext(:,:)
    real(r8),          optional, pointer     :: r_sw_scat(:,:)
    real(r8),          optional, pointer     :: r_sw_ascat(:,:)
    real(r8),          optional, pointer     :: r_lw_abs(:,:)
    real(r8),          optional, pointer     :: mu(:)
    character(len=20), optional, intent(out) :: aername
    real(r8),          optional, intent(out) :: density_aer
    real(r8),          optional, intent(out) :: hygro_aer
    real(r8),          optional, intent(out) :: dryrad_aer
    real(r8),          optional, intent(out) :: dispersion_aer
    real(r8),          optional, intent(out) :: num_to_mass_aer

    if (present(opticstype))     opticstype     = trim(mock_opticstype(aer_idx))
    if (present(aername))        aername        = mock_aername(aer_idx)
    if (present(density_aer))    density_aer    = mock_density(aer_idx)
    if (present(hygro_aer))      hygro_aer      = mock_hygro(aer_idx)
    if (present(dryrad_aer))     dryrad_aer     = mock_dryrad(aer_idx)
    if (present(dispersion_aer)) dispersion_aer = mock_dispersion(aer_idx)
    if (present(num_to_mass_aer)) num_to_mass_aer = mock_num_to_mass(aer_idx)

    ! Non-hygroscopic optics tables
    if (present(sw_nonhygro_ext)) then
       if (allocated(mock_sw_nonhygro_ext)) then
          sw_nonhygro_ext => mock_sw_nonhygro_ext(:, aer_idx)
       else
          nullify(sw_nonhygro_ext)
       end if
    end if
    if (present(sw_nonhygro_ssa)) then
       if (allocated(mock_sw_nonhygro_ssa)) then
          sw_nonhygro_ssa => mock_sw_nonhygro_ssa(:, aer_idx)
       else
          nullify(sw_nonhygro_ssa)
       end if
    end if
    if (present(sw_nonhygro_asm)) then
       if (allocated(mock_sw_nonhygro_asm)) then
          sw_nonhygro_asm => mock_sw_nonhygro_asm(:, aer_idx)
       else
          nullify(sw_nonhygro_asm)
       end if
    end if
    if (present(lw_ext)) then
       if (allocated(mock_lw_ext)) then
          lw_ext => mock_lw_ext(:, aer_idx)
       else
          nullify(lw_ext)
       end if
    end if

    ! Hygroscopic optics tables
    if (present(sw_hygro_ext)) then
       if (allocated(mock_sw_hygro_ext)) then
          sw_hygro_ext => mock_sw_hygro_ext(:, :, aer_idx)
       else
          nullify(sw_hygro_ext)
       end if
    end if
    if (present(sw_hygro_ssa)) then
       if (allocated(mock_sw_hygro_ssa)) then
          sw_hygro_ssa => mock_sw_hygro_ssa(:, :, aer_idx)
       else
          nullify(sw_hygro_ssa)
       end if
    end if
    if (present(sw_hygro_asm)) then
       if (allocated(mock_sw_hygro_asm)) then
          sw_hygro_asm => mock_sw_hygro_asm(:, :, aer_idx)
       else
          nullify(sw_hygro_asm)
       end if
    end if
    if (present(lw_hygro_ext)) then
       if (allocated(mock_lw_hygro_ext)) then
          lw_hygro_ext => mock_lw_hygro_ext(:, :, aer_idx)
       else
          nullify(lw_hygro_ext)
       end if
    end if

    ! Refractive indices
    if (present(refindex_aer_sw)) then
       if (allocated(mock_refindex_sw)) then
          refindex_aer_sw => mock_refindex_sw(:, aer_idx)
       else
          nullify(refindex_aer_sw)
       end if
    end if
    if (present(refindex_aer_lw)) then
       if (allocated(mock_refindex_lw)) then
          refindex_aer_lw => mock_refindex_lw(:, aer_idx)
       else
          nullify(refindex_aer_lw)
       end if
    end if

    ! Volcanic radius tables are not yet mocked, nullify
    if (present(r_sw_ext))  nullify(r_sw_ext)
    if (present(r_sw_scat)) nullify(r_sw_scat)
    if (present(r_sw_ascat)) nullify(r_sw_ascat)
    if (present(r_lw_abs))  nullify(r_lw_abs)
    if (present(mu))        nullify(mu)

    ! Derived non-hygro tables
    if (present(sw_nonhygro_scat))  nullify(sw_nonhygro_scat)
    if (present(sw_nonhygro_ascat)) nullify(sw_nonhygro_ascat)

  end subroutine rad_aer_get_props_by_idx

  !-----------------------------------------------------------------------
  ! Mock rad_aer_bulk_physprop_id: return the aer_idx as the physprop ID.
  ! In the real module this looks up the physprop index from the bin list.
  !-----------------------------------------------------------------------
  integer function rad_aer_bulk_physprop_id(list_idx, aer_idx)
    integer, intent(in) :: list_idx
    integer, intent(in) :: aer_idx

    rad_aer_bulk_physprop_id = aer_idx
  end function rad_aer_bulk_physprop_id

end module radiative_aerosol
