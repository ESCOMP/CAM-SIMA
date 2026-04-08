!-----------------------------------------------------------------------
! Mock radiative_aerosol module for unit testing bulk_aerosol_properties.
!
! This mock replaces the real radiative_aerosol facade module. The real
! module delegates to phys_prop (which reads NetCDF
! physprop files via PIO). This mock provides configurable in-memory data
! via setup_mock_rad_aer(), removing all file I/O dependencies.
!
! Supported interfaces (matching the real module's generic interfaces):
!   rad_aer_get_info(list_idx, naero=...)
!   rad_aer_get_props(list_idx, aer_idx, density_aer=..., hygro_aer=..., ...)
!-----------------------------------------------------------------------
module radiative_aerosol

  use shr_kind_mod, only: r8 => shr_kind_r8

  implicit none
  private

  ! Maximum number of mock aerosols
  integer, parameter :: max_mock_aero = 10

  ! Mock data storage (populated by setup_mock_rad_aer)
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

  ! Generic interfaces matching the real module
  interface rad_aer_get_info
     module procedure rad_aer_get_info_basic
  end interface

  interface rad_aer_get_props
     module procedure rad_aer_get_props_by_idx
  end interface

  public :: rad_aer_get_info
  public :: rad_aer_get_props
  public :: setup_mock_rad_aer
  public :: setup_mock_optics_tables
  public :: cleanup_mock_rad_aer
  public :: rad_aer_bulk_physprop_id

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
  ! Clean up mock data.
  !-----------------------------------------------------------------------
  subroutine cleanup_mock_rad_aer()
    mock_naero = 0
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
    if (present(nmodes)) nmodes = 0
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
