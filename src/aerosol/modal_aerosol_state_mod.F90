module modal_aerosol_state_mod
  use shr_kind_mod, only: r8 => shr_kind_r8
  use ccpp_kinds, only: kind_phys
  use shr_spfn_mod, only: erf => shr_spfn_erf
  use aerosol_state_mod, only: aerosol_state, ptr2d_t
  use radiative_aerosol, only: rad_aer_get_info, rad_aer_get_mode_props
  use aerosol_mmr_ccpp, only: rad_cnst_get_aer_mmr, rad_cnst_get_mode_num
  use aerosol_properties_mod, only: aerosol_properties
  use physconst,  only: rhoh2o
  use cam_abortutils, only: endrun

  implicit none

  private

  public :: modal_aerosol_state

  type, extends(aerosol_state) :: modal_aerosol_state
     private
     real(kind_phys), pointer :: constituents(:,:,:) => null()
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
     procedure :: icenuc_type_wght
     procedure :: update_bin
     procedure :: hetfrz_size_wght
     procedure :: hygroscopicity
     procedure :: water_uptake
     procedure :: dry_volume
     procedure :: wet_volume
     procedure :: water_volume
     procedure :: wet_diameter
     procedure :: convcld_actfrac
     procedure :: wgtpct

     final :: destructor

  end type modal_aerosol_state

  interface modal_aerosol_state
     procedure :: constructor
  end interface modal_aerosol_state

  real(r8), parameter :: rh2odens = 1._r8/rhoh2o

contains

  !------------------------------------------------------------------------------
  !------------------------------------------------------------------------------
  function constructor(constituents, list_idx) result(newobj)
    real(kind_phys), pointer, intent(in) :: constituents(:,:,:)
    integer, intent(in), optional :: list_idx

    type(modal_aerosol_state), pointer :: newobj

    integer :: ierr

    allocate(newobj,stat=ierr)
    if( ierr /= 0 ) then
       nullify(newobj)
       return
    end if

    newobj%constituents => constituents

    if (present(list_idx)) call newobj%set_list_idx(list_idx)

  end function constructor

  !------------------------------------------------------------------------------
  !------------------------------------------------------------------------------
  subroutine destructor(self)
    type(modal_aerosol_state), intent(inout) :: self

    nullify(self%constituents)

  end subroutine destructor

  !------------------------------------------------------------------------------
  ! sets transported components
  ! This aerosol model with the state of the transported aerosol constituents
  ! (mass mixing ratios or number mixing ratios)
  !------------------------------------------------------------------------------
  subroutine set_transported( self, transported_array )
    class(modal_aerosol_state), intent(inout) :: self
    real(r8), intent(in) :: transported_array(:,:,:)
    ! to be implemented later
  end subroutine set_transported

  !------------------------------------------------------------------------------
  ! returns transported components
  ! This returns to current state of the transported aerosol constituents
  ! (mass mixing ratios or number mixing ratios)
  !------------------------------------------------------------------------------
  subroutine get_transported( self, transported_array )
    class(modal_aerosol_state), intent(in) :: self
    real(r8), intent(out) :: transported_array(:,:,:)
    ! to be implemented later
  end subroutine get_transported

  !------------------------------------------------------------------------
  ! Total aerosol mass mixing ratio for a bin in a given grid box location (column and layer)
  !------------------------------------------------------------------------
  function ambient_total_bin_mmr(self, aero_props, bin_ndx, col_ndx, lyr_ndx) result(mmr_tot)
    class(modal_aerosol_state), intent(in) :: self
    class(aerosol_properties), intent(in) :: aero_props
    integer, intent(in) :: bin_ndx      ! bin index
    integer, intent(in) :: col_ndx      ! column index
    integer, intent(in) :: lyr_ndx      ! vertical layer index

    real(r8) :: mmr_tot                 ! mass mixing ratios totaled for all species
    real(r8),pointer :: mmrptr(:,:)
    integer :: spec_ndx

    mmr_tot = 0._r8

    do spec_ndx=1,aero_props%nspecies(bin_ndx)
       call rad_cnst_get_aer_mmr(self%list_idx_, bin_ndx, spec_ndx, 'a', self%constituents, mmrptr)
       mmr_tot = mmr_tot + mmrptr(col_ndx,lyr_ndx)
    end do

  end function ambient_total_bin_mmr

  !------------------------------------------------------------------------------
  ! returns ambient aerosol mass mixing ratio for a given species index and bin index
  !------------------------------------------------------------------------------
  subroutine get_ambient_mmr(self, species_ndx, bin_ndx, mmr)
    class(modal_aerosol_state), intent(in) :: self
    integer, intent(in) :: species_ndx  ! species index
    integer, intent(in) :: bin_ndx      ! bin index
    real(r8), pointer :: mmr(:,:)       ! mass mixing ratios (ncol,nlev)

    call rad_cnst_get_aer_mmr(self%list_idx_, bin_ndx, species_ndx, 'a', self%constituents, mmr)
  end subroutine get_ambient_mmr

  !------------------------------------------------------------------------------
  ! returns cloud-borne aerosol number mixing ratio for a given species index and bin index
  !------------------------------------------------------------------------------
  subroutine get_cldbrne_mmr(self, species_ndx, bin_ndx, mmr)
    class(modal_aerosol_state), intent(in) :: self
    integer, intent(in) :: species_ndx  ! species index
    integer, intent(in) :: bin_ndx      ! bin index
    real(r8), pointer :: mmr(:,:)       ! mass mixing ratios (ncol,nlev)

    call rad_cnst_get_aer_mmr(self%list_idx_, bin_ndx, species_ndx, 'c', self%constituents, mmr)
  end subroutine get_cldbrne_mmr

  !------------------------------------------------------------------------------
  ! returns ambient aerosol number mixing ratio for a given species index and bin index
  !------------------------------------------------------------------------------
  subroutine get_ambient_num(self, bin_ndx, num)
    class(modal_aerosol_state), intent(in) :: self
    integer, intent(in) :: bin_ndx     ! bin index
    real(r8), pointer   :: num(:,:)    ! number densities

    call rad_cnst_get_mode_num(self%list_idx_, bin_ndx, 'a', self%constituents, num)
  end subroutine get_ambient_num

  !------------------------------------------------------------------------------
  ! returns cloud-borne aerosol number mixing ratio for a given species index and bin index
  !------------------------------------------------------------------------------
  subroutine get_cldbrne_num(self, bin_ndx, num)
    class(modal_aerosol_state), intent(in) :: self
    integer, intent(in) :: bin_ndx             ! bin index
    real(r8), pointer :: num(:,:)

    call rad_cnst_get_mode_num(self%list_idx_, bin_ndx, 'c', self%constituents, num)
  end subroutine get_cldbrne_num

  !------------------------------------------------------------------------------
  ! returns interstitial and cloud-borne aerosol states
  !------------------------------------------------------------------------------
  subroutine get_states( self, aero_props, raer, qqcw )
    class(modal_aerosol_state), intent(in) :: self
    class(aerosol_properties), intent(in) :: aero_props
    type(ptr2d_t), intent(out) :: raer(:)
    type(ptr2d_t), intent(out) :: qqcw(:)

    integer :: ibin,ispc, indx

    do ibin = 1, aero_props%nbins()
       indx = aero_props%indexer(ibin, 0)
       call self%get_ambient_num(ibin, raer(indx)%fld)
       call self%get_cldbrne_num(ibin, qqcw(indx)%fld)
       do ispc = 1, aero_props%nspecies(ibin)
          indx = aero_props%indexer(ibin, ispc)
          call self%get_ambient_mmr(species_ndx=ispc, bin_ndx=ibin, mmr=raer(indx)%fld)
          call self%get_cldbrne_mmr(species_ndx=ispc, bin_ndx=ibin, mmr=qqcw(indx)%fld)
       end do
    end do

  end subroutine get_states

  !------------------------------------------------------------------------------
  ! return aerosol bin size weights for a given bin
  !------------------------------------------------------------------------------
  subroutine icenuc_size_wght_arr(self, bin_ndx, ncol, nlev, species_type, use_preexisting_ice, wght)
    use aerosol_properties_mod, only: aero_name_len
    use cam_constituents, only: const_get_index

    class(modal_aerosol_state), intent(in) :: self
    integer, intent(in) :: bin_ndx                ! bin number
    integer, intent(in) :: ncol                ! number of columns
    integer, intent(in) :: nlev                ! number of vertical levels
    character(len=*), intent(in) :: species_type  ! species type
    logical, intent(in) :: use_preexisting_ice ! pre-existing ice flag
    real(r8), intent(out) :: wght(:,:)

    character(len=aero_name_len) :: modetype
    real(r8) :: sigmag_aitken
    integer :: i,k
    integer :: idx_dgnum
    character(len=64) :: cname

    if (self%list_idx_ /= 0) then
       call endrun('modal_aerosol_state::icenuc_size_wght_arr: only valid for climate list (list_idx=0)')
    end if

    call rad_aer_get_info(0, bin_ndx, mode_type=modetype)

    wght = 0._r8

    select case ( trim(species_type) )
    case('dust')
       if (modetype=='coarse' .or. modetype=='coarse_dust') then
          wght(:ncol,:) = 1._r8
       end if
    case('sulfate')
       if (modetype=='aitken') then
          if ( use_preexisting_ice ) then
             wght(:ncol,:) = 1._r8
          else
             ! The CAM DGNUM pbuf field (i,k,<bin_ndx>) is replaced by dgnum_m<bin_ndx> in SIMA.
             ! It should be registered by the aerosol model.
             ! If not found, the model will endrun when calling this subroutine.
             call rad_aer_get_mode_props(0, bin_ndx, sigmag=sigmag_aitken)
             write(cname, '(a,i2.2)') 'dgnum_m', bin_ndx
             call const_get_index(trim(cname), idx_dgnum)
             do k = 1, nlev
                do i = 1, ncol
                   if (self%constituents(i,k,idx_dgnum) > 0._r8) then
                      ! only allow so4 with D>0.1 um in ice nucleation
                      wght(i,k) = max(0._r8,(0.5_r8 - 0.5_r8* &
                           erf(log(0.1e-6_r8 / self%constituents(i,k,idx_dgnum)) / &
                           (2._r8**0.5_r8*log(sigmag_aitken)))  ))
                   end if
                end do
             end do
          endif
       endif
    case('black-c')
       if (modetype=='accum') then
          wght(:ncol,:) = 1._r8
       endif
    case('sulfate_strat')
       if (modetype=='accum' .or. modetype=='coarse' .or. modetype=='coarse_strat') then
          wght(:ncol,:) = 1._r8
       endif
    end select

  end subroutine icenuc_size_wght_arr

  !------------------------------------------------------------------------------
  ! return aerosol bin size weights for a given bin, column and vertical layer
  !------------------------------------------------------------------------------
  subroutine icenuc_size_wght_val(self, bin_ndx, col_ndx, lyr_ndx, species_type, use_preexisting_ice, wght)
    use aerosol_properties_mod, only: aero_name_len
    use cam_constituents, only: const_get_index

    class(modal_aerosol_state), intent(in) :: self
    integer, intent(in) :: bin_ndx                ! bin number
    integer, intent(in) :: col_ndx                ! column index
    integer, intent(in) :: lyr_ndx                ! vertical layer index
    character(len=*), intent(in) :: species_type  ! species type
    logical, intent(in) :: use_preexisting_ice    ! pre-existing ice flag
    real(r8), intent(out) :: wght

    character(len=aero_name_len) :: modetype
    real(r8) :: sigmag_aitken
    integer :: idx_dgnum
    character(len=64) :: cname

    if (self%list_idx_ /= 0) then
       call endrun('modal_aerosol_state::icenuc_size_wght_val: only valid for climate list (list_idx=0)')
    end if

    wght = 0._r8

    call rad_aer_get_info(0, bin_ndx, mode_type=modetype)

    select case ( trim(species_type) )
    case('dust')
       if (modetype=='coarse' .or. modetype=='coarse_dust') then
          wght = 1._r8
       end if
    case('sulfate')
       if (modetype=='aitken') then
          if ( use_preexisting_ice ) then
             wght = 1._r8
          else
             call rad_aer_get_mode_props(0, bin_ndx, sigmag=sigmag_aitken)
             write(cname, '(a,i2.2)') 'dgnum_m', bin_ndx
             call const_get_index(trim(cname), idx_dgnum)

             if (self%constituents(col_ndx, lyr_ndx, idx_dgnum) > 0._r8) then
                ! only allow so4 with D>0.1 um in ice nucleation
                wght = max(0._r8,(0.5_r8 - 0.5_r8* &
                     erf(log(0.1e-6_r8 / self%constituents(col_ndx, lyr_ndx, idx_dgnum)) / &
                     (2._r8**0.5_r8*log(sigmag_aitken)))  ))
             end if
          endif
       endif
    case('black-c')
       if (modetype=='accum') then
          wght = 1._r8
       endif
    case('sulfate_strat')
       if (modetype=='accum' .or. modetype=='coarse' .or. modetype=='coarse_strat') then
          wght = 1._r8
       endif
    end select

  end subroutine icenuc_size_wght_val

  !------------------------------------------------------------------------------
  ! returns aerosol type weights for a given aerosol type and bin
  !------------------------------------------------------------------------------
  subroutine icenuc_type_wght(self, bin_ndx, ncol, nlev, species_type, aero_props, rho, wght, cloud_borne)

    use aerosol_properties_mod, only: aerosol_properties
    use aerosol_properties_mod, only: aero_name_len

    class(modal_aerosol_state), intent(in) :: self
    integer, intent(in) :: bin_ndx                ! bin number
    integer, intent(in) :: ncol                   ! number of columns
    integer, intent(in) :: nlev                   ! number of vertical levels
    character(len=*), intent(in) :: species_type  ! species type
    class(aerosol_properties), intent(in) :: aero_props ! aerosol properties object
    real(r8), intent(in) :: rho(:,:)              ! air density (kg m-3)
    real(r8), intent(out) :: wght(:,:)            ! type weights
    logical, optional, intent(in) :: cloud_borne  ! if TRUE cloud-borne aerosols are used
                                                  ! otherwise ambient aerosols are used

    character(len=aero_name_len) :: modetype

    if (self%list_idx_ /= 0) then
       call endrun('modal_aerosol_state::icenuc_type_wght: only valid for climate list (list_idx=0)')
    end if

    call rad_aer_get_info(0, bin_ndx, mode_type=modetype)

    wght = 0._r8

    if (species_type == 'dust') then
       if (modetype=='coarse_dust') then
          wght(:ncol,:) = 1._r8
       else
          call self%icenuc_type_wght_base(bin_ndx, ncol, nlev, species_type, aero_props, rho, wght, cloud_borne)
       end if
    else if (species_type == 'sulfate_strat') then
       if (modetype=='accum') then
          wght(:ncol,:) = 1._r8
       elseif ( modetype=='coarse' .or. modetype=='coarse_strat') then
          call self%icenuc_type_wght_base(bin_ndx, ncol, nlev, species_type, aero_props, rho, wght, cloud_borne)
       endif
    else
       wght(:ncol,:) = 1._r8
    end if

  end subroutine icenuc_type_wght

  !------------------------------------------------------------------------------
  !------------------------------------------------------------------------------
  subroutine update_bin( self, bin_ndx, col_ndx, lyr_ndx, delmmr_sum, delnum_sum, tnd_ndx, dtime, tend )
    class(modal_aerosol_state), intent(in) :: self
    integer, intent(in) :: bin_ndx                ! bin number
    integer, intent(in) :: col_ndx                ! column index
    integer, intent(in) :: lyr_ndx                ! vertical layer index
    real(r8),intent(in) :: delmmr_sum             ! mass mixing ratio change summed over all species in bin
    real(r8),intent(in) :: delnum_sum             ! number mixing ratio change summed over all species in bin
    integer, intent(in) :: tnd_ndx                ! tendency index
    real(r8),intent(in) :: dtime                  ! time step size (sec)
    real(r8),intent(inout) :: tend(:,:,:)         ! tendency

    real(r8), pointer :: amb_num(:,:)
    real(r8), pointer :: cld_num(:,:)

    call self%get_ambient_num(bin_ndx, amb_num)
    call self%get_cldbrne_num(bin_ndx, cld_num)

    ! if there is no bin mass compute updates/tendencies for bin number
    ! -- apply the total number change to bin number
    if (tnd_ndx>0) then
       tend(col_ndx,lyr_ndx,tnd_ndx) = -delnum_sum/dtime
    else
       amb_num(col_ndx,lyr_ndx) = amb_num(col_ndx,lyr_ndx) - delnum_sum
    end if

    ! apply the total number change to bin number
    cld_num(col_ndx,lyr_ndx) = cld_num(col_ndx,lyr_ndx) + delnum_sum

  end subroutine update_bin

  !------------------------------------------------------------------------------
  ! returns the volume-weighted fractions of aerosol subset `bin_ndx` that can act
  ! as heterogeneous freezing nuclei
  !------------------------------------------------------------------------------
  function hetfrz_size_wght(self, bin_ndx, ncol, nlev) result(wght)
    use aerosol_properties_mod, only: aero_name_len

    class(modal_aerosol_state), intent(in) :: self
    integer, intent(in) :: bin_ndx             ! bin number
    integer, intent(in) :: ncol                ! number of columns
    integer, intent(in) :: nlev                ! number of vertical levels

    real(r8) :: wght(ncol,nlev)

    character(len=aero_name_len) :: modetype

    if (self%list_idx_ /= 0) then
       call endrun('modal_aerosol_state::hetfrz_size_wght: only valid for climate list (list_idx=0)')
    end if

    wght(:,:) = 1._r8

    call rad_aer_get_info(0, bin_ndx, mode_type=modetype)

    if (trim(modetype) == 'aitken') then
       wght(:,:) = 0._r8
    end if

  end function hetfrz_size_wght

  !------------------------------------------------------------------------------
  ! returns hygroscopicity for a given radiation diagnostic list number and
  ! bin number
  !------------------------------------------------------------------------------
  subroutine hygroscopicity(self, bin_ndx, kappa)
    class(modal_aerosol_state), intent(in) :: self
    integer, intent(in) :: bin_ndx         ! bin number
    real(r8), intent(out) :: kappa(:,:)    ! hygroscopicity (ncol,nlev)

    kappa = -huge(1._r8)

  end subroutine hygroscopicity

  !------------------------------------------------------------------------------
  ! returns aerosol wet diameter and aerosol water concentration for a given mode
  !
  ! In CAM, the climate list (list_idx==0) retrieves pre-computed DGNUMWET
  ! and QAERWAT from the physics buffer (third dimension is bin index);
  ! diagnostic lists recompute via modal_aero_calcsize/wateruptake.
  !
  ! CAM-SIMA: for the climate list, DGNUMWET and QAERWAT are retrieved from
  ! non-advected CCPP constituents dgnumwet_m<bin_ndx>, qaerwat_m<bin_ndx).
  ! These must be registered by the CCPPized modal water uptake scheme.
  ! const_get_index will endrun if they are not found.
  ! Diagnostic-list recomputation is not yet available (it requires the eventual
  ! CCPPized schemes)
  !------------------------------------------------------------------------------
  subroutine water_uptake(self, aero_props, bin_idx, ncol, nlev, dgnumwet, qaerwat)
    use cam_constituents, only: const_get_index

    class(modal_aerosol_state), intent(in) :: self
    class(aerosol_properties), intent(in) :: aero_props
    integer, intent(in) :: bin_idx              ! bin number
    integer, intent(in) :: ncol                 ! number of columns
    integer, intent(in) :: nlev                 ! number of levels
    real(r8),intent(out) :: dgnumwet(ncol,nlev) ! aerosol wet diameter (m)
    real(r8),intent(out) :: qaerwat(ncol,nlev)  ! aerosol water concentration (g/g)

    integer :: idx_wet, idx_qaw
    character(len=64) :: cname

    if (self%list_idx_ == 0) then
       ! Climate list: retrieve pre-computed fields from constituents
       ! by the modal water uptake CCPPized scheme.
       write(cname, '(a,i2.2)') 'dgnumwet_m', bin_idx
       call const_get_index(trim(cname), idx_wet)
       dgnumwet(:ncol,:nlev) = self%constituents(:ncol,:nlev,idx_wet)

       write(cname, '(a,i2.2)') 'qaerwat_m', bin_idx
       call const_get_index(trim(cname), idx_qaw)
       qaerwat(:ncol,:nlev) = self%constituents(:ncol,:nlev,idx_qaw)
    else
       ! Diagnostic lists: recomputation requires modal_aero_calcsize and
       ! modal_aero_wateruptake, which are not yet CCPPized.
       call endrun('modal_aerosol_state::water_uptake: diagnostic-list water uptake ' // &
            'not yet available in CAM-SIMA (requires CCPPized modal_aero_calcsize/wateruptake)')
    end if

  end subroutine water_uptake

  !------------------------------------------------------------------------------
  ! aerosol dry volume (m3/kg) for given radiation diagnostic list number and bin number
  !------------------------------------------------------------------------------
  function dry_volume(self, aero_props, bin_idx, ncol, nlev) result(vol)

    class(modal_aerosol_state), intent(in) :: self
    class(aerosol_properties), intent(in) :: aero_props

    integer, intent(in) :: bin_idx   ! bin number
    integer, intent(in) :: ncol      ! number of columns
    integer, intent(in) :: nlev      ! number of levels

    real(r8) :: vol(ncol,nlev)       ! m3/kg

    real(r8), pointer :: mmr(:,:)
    real(r8) :: specdens              ! species density (kg/m3)

    integer :: ispec

    vol(:,:) = 0._r8

    do ispec = 1, aero_props%nspecies(bin_idx)
       call self%get_ambient_mmr(species_ndx=ispec, bin_ndx=bin_idx, mmr=mmr)
       call aero_props%get(bin_idx, ispec, density=specdens)
       vol(:ncol,:) = vol(:ncol,:) + mmr(:ncol,:)/specdens
    end do

  end function dry_volume

  !------------------------------------------------------------------------------
  ! aerosol wet volume (m3/kg) for given radiation diagnostic list number and bin number
  !------------------------------------------------------------------------------
  function wet_volume(self, aero_props, bin_idx, ncol, nlev) result(vol)

    class(modal_aerosol_state), intent(in) :: self
    class(aerosol_properties), intent(in) :: aero_props

    integer, intent(in) :: bin_idx   ! bin number
    integer, intent(in) :: ncol      ! number of columns
    integer, intent(in) :: nlev      ! number of levels

    real(r8) :: vol(ncol,nlev)       ! m3/kg

    real(r8) :: dryvol(ncol,nlev)
    real(r8) :: watervol(ncol,nlev)

    dryvol = self%dry_volume(aero_props, bin_idx, ncol, nlev)
    watervol = self%water_volume(aero_props, bin_idx, ncol, nlev)

    vol = watervol + dryvol

  end function wet_volume

  !------------------------------------------------------------------------------
  ! aerosol water volume (m3/kg) for given radiation diagnostic list number and bin number
  !------------------------------------------------------------------------------
  function water_volume(self, aero_props, bin_idx, ncol, nlev) result(vol)

    class(modal_aerosol_state), intent(in) :: self
    class(aerosol_properties), intent(in) :: aero_props

    integer, intent(in) :: bin_idx   ! bin number
    integer, intent(in) :: ncol      ! number of columns
    integer, intent(in) :: nlev      ! number of levels

    real(r8) :: vol(ncol,nlev)       ! m3/kg

    real(r8) :: dgnumwet(ncol,nlev)
    real(r8) :: qaerwat(ncol,nlev)

    call self%water_uptake(aero_props, bin_idx, ncol, nlev, dgnumwet, qaerwat)

    vol(:ncol,:nlev) = qaerwat(:ncol,:nlev)*rh2odens
    where (vol<0._r8)
       vol = 0._r8
    end where

  end function water_volume

  !------------------------------------------------------------------------------
  ! aerosol wet diameter for a given mode
  !------------------------------------------------------------------------------
  function wet_diameter(self, bin_idx, ncol, nlev) result(diam)
    use cam_constituents, only: const_get_index

    class(modal_aerosol_state), intent(in) :: self
    integer, intent(in) :: bin_idx   ! bin number
    integer, intent(in) :: ncol      ! number of columns
    integer, intent(in) :: nlev      ! number of levels

    real(r8) :: diam(ncol,nlev)

    integer :: idx_wet
    character(len=64) :: cname

    write(cname, '(a,i2.2)') 'dgnumwet_m', bin_idx
    call const_get_index(trim(cname), idx_wet)
    diam(:ncol,:nlev) = self%constituents(:ncol,:nlev,idx_wet)

  end function wet_diameter

  !------------------------------------------------------------------------------
  ! prescribed aerosol activation fraction for convective cloud
  !------------------------------------------------------------------------------
  function convcld_actfrac(self, aero_props, ibin, ispc, ncol, nlev) result(frac)
    use aerosol_properties_mod, only: aero_name_len

    class(modal_aerosol_state), intent(in) :: self
    class(aerosol_properties), intent(in) :: aero_props ! aerosol properties object
    integer, intent(in) :: ibin   ! bin index
    integer, intent(in) :: ispc   ! species index
    integer, intent(in) :: ncol   ! number of columns
    integer, intent(in) :: nlev   ! number of vertical levels

    real(r8) :: frac(ncol,nlev)

    real(r8) :: f_act_conv_coarse(ncol,nlev)
    real(r8) :: f_act_conv_coarse_dust, f_act_conv_coarse_nacl
    real(r8) :: tmpdust, tmpnacl
    real(r8), pointer :: dust_mmr(:,:), nacl_mmr(:,:)
    integer :: dust_ndx, nacl_ndx
    integer :: i,k,l
    character(len=aero_name_len) :: bin_type, spectype

    bin_type = aero_props%bin_name(ibin)

    f_act_conv_coarse(:,:) = 0.60_r8
    f_act_conv_coarse_dust = 0.40_r8
    f_act_conv_coarse_nacl = 0.80_r8
    if (trim(bin_type) == 'coarse') then
       ! find dust and seasalt species indices in the coarse mode
       dust_ndx = -1
       nacl_ndx = -1
       do l = 1, aero_props%nspecies(ibin)
          call aero_props%species_type(ibin, l, spectype)
          if (trim(spectype) == 'dust') dust_ndx = l
          if (trim(spectype) == 'seasalt') nacl_ndx = l
       end do
       if ((dust_ndx > 0) .and. (nacl_ndx > 0)) then
          call self%get_ambient_mmr(species_ndx=dust_ndx, bin_ndx=ibin, mmr=dust_mmr)
          call self%get_ambient_mmr(species_ndx=nacl_ndx, bin_ndx=ibin, mmr=nacl_mmr)
          do k = 1, nlev
             do i = 1, ncol
                tmpdust = max( 0.0_r8, dust_mmr(i,k) )
                tmpnacl = max( 0.0_r8, nacl_mmr(i,k) )
                if ((tmpdust+tmpnacl) > 1.0e-30_r8) then
                   f_act_conv_coarse(i,k) = (f_act_conv_coarse_dust*tmpdust &
                        + f_act_conv_coarse_nacl*tmpnacl)/(tmpdust+tmpnacl)
                end if
             end do
          end do
       end if
    end if

    if (trim(bin_type) == 'primary_carbon') then
       frac = 0.0_r8
    else if ((trim(bin_type) == 'fine_dust') .or. (trim(bin_type) == 'coarse_dust')) then
       frac = 0.4_r8
    else
       frac = 0.8_r8
    end if

    ! set f_act_conv for interstitial (lphase=1) coarse mode species
    ! for the convective in-cloud, we conceptually treat the coarse dust and seasalt
    ! as being externally mixed, and apply f_act_conv = f_act_conv_coarse_dust/nacl to dust/seasalt
    ! number and sulfate are conceptually partitioned to the dust and seasalt
    ! on a mass basis, so the f_act_conv for number and sulfate are
    ! mass-weighted averages of the values used for dust/seasalt
    if (trim(bin_type) == 'coarse') then
       frac = f_act_conv_coarse
       if (ispc>0) then
          call aero_props%species_type(ibin, ispc, spectype)
          if (trim(spectype) == 'dust') then
             frac = f_act_conv_coarse_dust
          else if (trim(spectype) == 'seasalt') then
             frac = f_act_conv_coarse_nacl
          end if
       end if
    end if

  end function convcld_actfrac

  !------------------------------------------------------------------------------
  ! aerosol weight percent of H2SO4/H2O solution
  !------------------------------------------------------------------------------
  function wgtpct(self, ncol, nlev) result(wtp)
    class(modal_aerosol_state), intent(in) :: self
    integer, intent(in) ::  ncol, nlev
    real(r8) :: wtp(ncol,nlev)  ! weight percent of H2SO4/H2O solution for given icol, ilev

    wtp(:,:) = -huge(1._r8)

  end function wgtpct

end module modal_aerosol_state_mod
