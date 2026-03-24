module carma_aerosol_state_mod
  !-----------------------------------------------------------------------------
  ! Stub module for CARMA aerosol state not yet implemented in CAM-SIMA.
  ! Exports the constructor interface so aerosol_instances_mod compiles.
  !-----------------------------------------------------------------------------
  use shr_kind_mod, only: r8 => shr_kind_r8
  use ccpp_kinds, only: kind_phys
  use cam_abortutils, only: endrun
  use aerosol_state_mod, only: aerosol_state, ptr2d_t
  use aerosol_properties_mod, only: aerosol_properties

  implicit none
  private

  public :: carma_aerosol_state

  type, extends(aerosol_state) :: carma_aerosol_state
     private
   contains
     procedure :: get_transported
     procedure :: set_transported
     procedure :: ambient_total_bin_mmr
     procedure :: get_ambient_mmr
     procedure :: get_cldbrne_mmr
     procedure :: get_ambient_num
     procedure :: get_cldbrne_num
     procedure :: get_states
     procedure :: icenuc_size_wght_arr
     procedure :: icenuc_size_wght_val
     procedure :: update_bin
     procedure :: hetfrz_size_wght
     procedure :: hygroscopicity
     procedure :: water_uptake
     procedure :: dry_volume
     procedure :: wet_volume
     procedure :: water_volume
     procedure :: wet_diameter
     procedure :: wgtpct
  end type carma_aerosol_state

  interface carma_aerosol_state
     procedure :: constructor
  end interface carma_aerosol_state

contains

  function constructor(constituents, list_idx) result(newobj)
    real(kind_phys), pointer, intent(in) :: constituents(:,:,:)
    integer, intent(in), optional :: list_idx
    type(carma_aerosol_state), pointer :: newobj
    nullify(newobj)
    call endrun('carma_aerosol_state: not implemented in CAM-SIMA')
  end function constructor

  subroutine set_transported(self, transported_array)
    class(carma_aerosol_state), intent(inout) :: self
    real(r8), intent(in) :: transported_array(:,:,:)
    call endrun('carma_aerosol_state%set_transported: not implemented')
  end subroutine set_transported

  subroutine get_transported(self, transported_array)
    class(carma_aerosol_state), intent(in) :: self
    real(r8), intent(out) :: transported_array(:,:,:)
    call endrun('carma_aerosol_state%get_transported: not implemented')
  end subroutine get_transported

  function ambient_total_bin_mmr(self, aero_props, bin_ndx, col_ndx, lyr_ndx) result(mmr_tot)
    class(carma_aerosol_state), intent(in) :: self
    class(aerosol_properties), intent(in) :: aero_props
    integer, intent(in) :: bin_ndx, col_ndx, lyr_ndx
    real(r8) :: mmr_tot
    mmr_tot = 0._r8
    call endrun('carma_aerosol_state%ambient_total_bin_mmr: not implemented')
  end function ambient_total_bin_mmr

  subroutine get_ambient_mmr(self, species_ndx, bin_ndx, mmr)
    class(carma_aerosol_state), intent(in) :: self
    integer, intent(in) :: species_ndx, bin_ndx
    real(r8), pointer :: mmr(:,:)
    nullify(mmr)
    call endrun('carma_aerosol_state%get_ambient_mmr: not implemented')
  end subroutine get_ambient_mmr

  subroutine get_cldbrne_mmr(self, species_ndx, bin_ndx, mmr)
    class(carma_aerosol_state), intent(in) :: self
    integer, intent(in) :: species_ndx, bin_ndx
    real(r8), pointer :: mmr(:,:)
    nullify(mmr)
    call endrun('carma_aerosol_state%get_cldbrne_mmr: not implemented')
  end subroutine get_cldbrne_mmr

  subroutine get_ambient_num(self, bin_ndx, num)
    class(carma_aerosol_state), intent(in) :: self
    integer, intent(in) :: bin_ndx
    real(r8), pointer :: num(:,:)
    nullify(num)
    call endrun('carma_aerosol_state%get_ambient_num: not implemented')
  end subroutine get_ambient_num

  subroutine get_cldbrne_num(self, bin_ndx, num)
    class(carma_aerosol_state), intent(in) :: self
    integer, intent(in) :: bin_ndx
    real(r8), pointer :: num(:,:)
    nullify(num)
    call endrun('carma_aerosol_state%get_cldbrne_num: not implemented')
  end subroutine get_cldbrne_num

  subroutine get_states(self, aero_props, raer, qqcw)
    class(carma_aerosol_state), intent(in) :: self
    class(aerosol_properties), intent(in) :: aero_props
    type(ptr2d_t), intent(out) :: raer(:), qqcw(:)
    call endrun('carma_aerosol_state%get_states: not implemented')
  end subroutine get_states

  subroutine icenuc_size_wght_arr(self, bin_ndx, ncol, nlev, species_type, use_preexisting_ice, wght)
    class(carma_aerosol_state), intent(in) :: self
    integer, intent(in) :: bin_ndx, ncol, nlev
    character(len=*), intent(in) :: species_type
    logical, intent(in) :: use_preexisting_ice
    real(r8), intent(out) :: wght(:,:)
    call endrun('carma_aerosol_state%icenuc_size_wght_arr: not implemented')
  end subroutine icenuc_size_wght_arr

  subroutine icenuc_size_wght_val(self, bin_ndx, col_ndx, lyr_ndx, species_type, use_preexisting_ice, wght)
    class(carma_aerosol_state), intent(in) :: self
    integer, intent(in) :: bin_ndx, col_ndx, lyr_ndx
    character(len=*), intent(in) :: species_type
    logical, intent(in) :: use_preexisting_ice
    real(r8), intent(out) :: wght
    call endrun('carma_aerosol_state%icenuc_size_wght_val: not implemented')
  end subroutine icenuc_size_wght_val

  subroutine update_bin(self, bin_ndx, col_ndx, lyr_ndx, delmmr_sum, delnum_sum, tnd_ndx, dtime, tend)
    class(carma_aerosol_state), intent(in) :: self
    integer, intent(in) :: bin_ndx, col_ndx, lyr_ndx, tnd_ndx
    real(r8), intent(in) :: delmmr_sum, delnum_sum, dtime
    real(r8), intent(inout) :: tend(:,:,:)
    call endrun('carma_aerosol_state%update_bin: not implemented')
  end subroutine update_bin

  function hetfrz_size_wght(self, bin_ndx, ncol, nlev) result(wght)
    class(carma_aerosol_state), intent(in) :: self
    integer, intent(in) :: bin_ndx, ncol, nlev
    real(r8) :: wght(ncol,nlev)
    call endrun('carma_aerosol_state%hetfrz_size_wght: not implemented')
  end function hetfrz_size_wght

  subroutine hygroscopicity(self, bin_ndx, kappa)
    class(carma_aerosol_state), intent(in) :: self
    integer, intent(in) :: bin_ndx
    real(r8), intent(out) :: kappa(:,:)
    call endrun('carma_aerosol_state%hygroscopicity: not implemented')
  end subroutine hygroscopicity

  subroutine water_uptake(self, aero_props, bin_idx, ncol, nlev, dgnumwet, qaerwat)
    class(carma_aerosol_state), intent(in) :: self
    class(aerosol_properties), intent(in) :: aero_props
    integer, intent(in) :: bin_idx, ncol, nlev
    real(r8), intent(out) :: dgnumwet(ncol,nlev), qaerwat(ncol,nlev)
    call endrun('carma_aerosol_state%water_uptake: not implemented')
  end subroutine water_uptake

  function dry_volume(self, aero_props, bin_idx, ncol, nlev) result(vol)
    class(carma_aerosol_state), intent(in) :: self
    class(aerosol_properties), intent(in) :: aero_props
    integer, intent(in) :: bin_idx, ncol, nlev
    real(r8) :: vol(ncol,nlev)
    vol = -huge(1._r8)
  end function dry_volume

  function wet_volume(self, aero_props, bin_idx, ncol, nlev) result(vol)
    class(carma_aerosol_state), intent(in) :: self
    class(aerosol_properties), intent(in) :: aero_props
    integer, intent(in) :: bin_idx, ncol, nlev
    real(r8) :: vol(ncol,nlev)
    vol = -huge(1._r8)
  end function wet_volume

  function water_volume(self, aero_props, bin_idx, ncol, nlev) result(vol)
    class(carma_aerosol_state), intent(in) :: self
    class(aerosol_properties), intent(in) :: aero_props
    integer, intent(in) :: bin_idx, ncol, nlev
    real(r8) :: vol(ncol,nlev)
    vol = -huge(1._r8)
  end function water_volume

  function wet_diameter(self, bin_idx, ncol, nlev) result(diam)
    class(carma_aerosol_state), intent(in) :: self
    integer, intent(in) :: bin_idx, ncol, nlev
    real(r8) :: diam(ncol,nlev)
    call endrun('carma_aerosol_state%wet_diameter: not implemented')
  end function wet_diameter

  function wgtpct(self, ncol, nlev) result(wtp)
    class(carma_aerosol_state), intent(in) :: self
    integer, intent(in) :: ncol, nlev
    real(r8) :: wtp(ncol,nlev)
    wtp = -huge(1._r8)
  end function wgtpct

end module carma_aerosol_state_mod
