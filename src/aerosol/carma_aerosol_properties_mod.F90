module carma_aerosol_properties_mod
  !-----------------------------------------------------------------------------
  ! Stub module for CARMA aerosol properties not yet implemented in CAM-SIMA.
  ! Exports the constructor interface so aerosol_instances_mod compiles.
  !-----------------------------------------------------------------------------
  use shr_kind_mod, only: r8 => shr_kind_r8
  use cam_abortutils, only: endrun
  use aerosol_properties_mod, only: aerosol_properties, aero_name_len

  implicit none
  private

  public :: carma_aerosol_properties

  type, extends(aerosol_properties) :: carma_aerosol_properties
     private
   contains
     procedure :: number_transported
     procedure :: get
     procedure :: amcube
     procedure :: actfracs
     procedure :: num_names
     procedure :: mmr_names
     procedure :: amb_num_name
     procedure :: amb_mmr_name
     procedure :: species_type
     procedure :: icenuc_updates_num
     procedure :: icenuc_updates_mmr
     procedure :: apply_number_limits
     procedure :: hetfrz_species
     procedure :: physprop_id
     procedure :: soluble
     procedure :: min_mass_mean_rad
     procedure :: bin_name
     procedure :: scav_diam
     procedure :: resuspension_resize
     procedure :: rebin_bulk_fluxes
     procedure :: hydrophilic
     procedure :: model_is
  end type carma_aerosol_properties

  interface carma_aerosol_properties
     procedure :: constructor
  end interface carma_aerosol_properties

contains

  function constructor(list_idx) result(newobj)
    integer, optional, intent(in) :: list_idx
    type(carma_aerosol_properties), pointer :: newobj
    nullify(newobj)
    call endrun('carma_aerosol_properties: not implemented in CAM-SIMA')
  end function constructor

  integer function number_transported(self)
    class(carma_aerosol_properties), intent(in) :: self
    number_transported = -1
    call endrun('carma_aerosol_properties%number_transported: not implemented')
  end function number_transported

  subroutine get(self, bin_ndx, species_ndx, density, hygro, &
       spectype, specname, specmorph, refindex_sw, refindex_lw, num_to_mass_aer, dryrad)
    class(carma_aerosol_properties), intent(in) :: self
    integer, intent(in) :: bin_ndx, species_ndx
    real(r8), optional, intent(out) :: density, hygro
    character(len=*), optional, intent(out) :: spectype, specname, specmorph
    complex(r8), pointer, optional, intent(out) :: refindex_sw(:), refindex_lw(:)
    real(r8), optional, intent(out) :: num_to_mass_aer, dryrad
    call endrun('carma_aerosol_properties%get: not implemented')
  end subroutine get

  pure elemental real(r8) function amcube(self, bin_ndx, volconc, numconc)
    class(carma_aerosol_properties), intent(in) :: self
    integer, intent(in) :: bin_ndx
    real(r8), intent(in) :: volconc, numconc
    amcube = -huge(1._r8)
  end function amcube

  subroutine actfracs(self, bin_ndx, smc, smax, fn, fm)
    class(carma_aerosol_properties), intent(in) :: self
    integer, intent(in) :: bin_ndx
    real(r8), intent(in) :: smc, smax
    real(r8), intent(out) :: fn, fm
    call endrun('carma_aerosol_properties%actfracs: not implemented')
  end subroutine actfracs

  subroutine num_names(self, bin_ndx, name_a, name_c)
    class(carma_aerosol_properties), intent(in) :: self
    integer, intent(in) :: bin_ndx
    character(len=*), intent(out) :: name_a, name_c
    call endrun('carma_aerosol_properties%num_names: not implemented')
  end subroutine num_names

  subroutine mmr_names(self, bin_ndx, species_ndx, name_a, name_c)
    class(carma_aerosol_properties), intent(in) :: self
    integer, intent(in) :: bin_ndx, species_ndx
    character(len=*), intent(out) :: name_a, name_c
    call endrun('carma_aerosol_properties%mmr_names: not implemented')
  end subroutine mmr_names

  subroutine amb_num_name(self, bin_ndx, name)
    class(carma_aerosol_properties), intent(in) :: self
    integer, intent(in) :: bin_ndx
    character(len=*), intent(out) :: name
    call endrun('carma_aerosol_properties%amb_num_name: not implemented')
  end subroutine amb_num_name

  subroutine amb_mmr_name(self, bin_ndx, species_ndx, name)
    class(carma_aerosol_properties), intent(in) :: self
    integer, intent(in) :: bin_ndx, species_ndx
    character(len=*), intent(out) :: name
    call endrun('carma_aerosol_properties%amb_mmr_name: not implemented')
  end subroutine amb_mmr_name

  subroutine species_type(self, bin_ndx, species_ndx, spectype)
    class(carma_aerosol_properties), intent(in) :: self
    integer, intent(in) :: bin_ndx, species_ndx
    character(len=*), intent(out) :: spectype
    call endrun('carma_aerosol_properties%species_type: not implemented')
  end subroutine species_type

  function icenuc_updates_num(self, bin_ndx) result(res)
    class(carma_aerosol_properties), intent(in) :: self
    integer, intent(in) :: bin_ndx
    logical :: res
    res = .false.
  end function icenuc_updates_num

  function icenuc_updates_mmr(self, bin_ndx, species_ndx) result(res)
    class(carma_aerosol_properties), intent(in) :: self
    integer, intent(in) :: bin_ndx, species_ndx
    logical :: res
    res = .false.
  end function icenuc_updates_mmr

  subroutine apply_number_limits(self, naerosol, vaerosol, istart, istop, m)
    class(carma_aerosol_properties), intent(in) :: self
    real(r8), intent(inout) :: naerosol(:)
    real(r8), intent(in) :: vaerosol(:)
    integer, intent(in) :: istart, istop, m
    call endrun('carma_aerosol_properties%apply_number_limits: not implemented')
  end subroutine apply_number_limits

  function hetfrz_species(self, bin_ndx, spc_ndx) result(res)
    class(carma_aerosol_properties), intent(in) :: self
    integer, intent(in) :: bin_ndx, spc_ndx
    logical :: res
    res = .false.
  end function hetfrz_species

  integer function physprop_id(self, bin_ndx)
    class(carma_aerosol_properties), intent(in) :: self
    integer, intent(in) :: bin_ndx
    physprop_id = -1
    call endrun('carma_aerosol_properties%physprop_id: not implemented')
  end function physprop_id

  logical function soluble(self, bin_ndx)
    class(carma_aerosol_properties), intent(in) :: self
    integer, intent(in) :: bin_ndx
    soluble = .false.
    call endrun('carma_aerosol_properties%soluble: not implemented')
  end function soluble

  function min_mass_mean_rad(self, bin_ndx, species_ndx) result(minrad)
    class(carma_aerosol_properties), intent(in) :: self
    integer, intent(in) :: bin_ndx, species_ndx
    real(r8) :: minrad
    minrad = 0._r8
  end function min_mass_mean_rad

  function bin_name(self, bin_ndx) result(name)
    class(carma_aerosol_properties), intent(in) :: self
    integer, intent(in) :: bin_ndx
    character(len=aero_name_len) :: name
    name = ''
    call endrun('carma_aerosol_properties%bin_name: not implemented')
  end function bin_name

  function scav_diam(self, bin_ndx) result(diam)
    class(carma_aerosol_properties), intent(in) :: self
    integer, intent(in) :: bin_ndx
    real(r8) :: diam
    diam = -huge(1._r8)
  end function scav_diam

  subroutine resuspension_resize(self, dcondt)
    class(carma_aerosol_properties), intent(in) :: self
    real(r8), intent(inout) :: dcondt(:)
    call endrun('carma_aerosol_properties%resuspension_resize: not implemented')
  end subroutine resuspension_resize

  subroutine rebin_bulk_fluxes(self, bulk_type, dep_fluxes, diam_edges, bulk_fluxes, &
       error_code, error_string)
    class(carma_aerosol_properties), intent(in) :: self
    character(len=*), intent(in) :: bulk_type
    real(r8), intent(in) :: dep_fluxes(:), diam_edges(:)
    real(r8), intent(out) :: bulk_fluxes(:)
    integer, intent(out) :: error_code
    character(len=*), intent(out) :: error_string
    call endrun('carma_aerosol_properties%rebin_bulk_fluxes: not implemented')
  end subroutine rebin_bulk_fluxes

  logical function hydrophilic(self, bin_ndx)
    class(carma_aerosol_properties), intent(in) :: self
    integer, intent(in) :: bin_ndx
    hydrophilic = .false.
  end function hydrophilic

  pure logical function model_is(self, query)
    class(carma_aerosol_properties), intent(in) :: self
    character(len=*), intent(in) :: query
    model_is = (trim(query) == 'CARMA' .or. trim(query) == 'carma')
  end function model_is

end module carma_aerosol_properties_mod
