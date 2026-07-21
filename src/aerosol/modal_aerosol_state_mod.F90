module modal_aerosol_state_mod
  use shr_kind_mod, only: r8 => shr_kind_r8
  use shr_spfn_mod, only: erf => shr_spfn_erf
  use aerosol_state_mod, only: aerosol_state, ptr2d_t
  use radiative_aerosol, only: rad_aer_get_info, rad_aer_get_mode_props
  use aerosol_mmr_host, only: rad_cnst_get_aer_mmr, rad_cnst_get_mode_num, aero_host_binding_t
  use aerosol_mmr_host, only: get_mode_dry_diameter, get_mode_wet_diameter, get_mode_aer_water
  use aerosol_properties_mod, only: aerosol_properties, aero_name_len
  use physconst,  only: rhoh2o, pi
  use cam_abortutils, only: endrun

  implicit none

  private

  public :: modal_aerosol_state
  public :: modal_aerosol_state_register_water_uptake_diag

  type, extends(aerosol_state) :: modal_aerosol_state
     private
     ! Opaque host-binding handle used to retrieve aerosol fields from
     ! host model data; built by host-side wiring (aerosol_instances_mod).
     ! This keeps model-specific data structures outside of the aerosol interface.
     type(aero_host_binding_t) :: host_
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
     procedure :: aqu_gain_binfraction

     final :: destructor

  end type modal_aerosol_state

  interface modal_aerosol_state
     procedure :: constructor
  end interface modal_aerosol_state

  ! Interface of the diagnostic-list water uptake recompute. The science
  ! (modal_aero_wateruptake_diag) lives with the portable modal aerosol
  ! schemes, which are not part of every build, so it is wired in by host
  ! code at initialization through a procedure pointer rather than
  ! referenced directly; when nothing is registered, diagnostic-list
  ! water uptake aborts as unavailable.
  abstract interface
     subroutine water_uptake_diag_i(aero_props, aero_state, ncol, nlev, top_lev, &
          pi, rhoh2o, t, pmid, h2ommr, cldn, bin_idx, dgnumwet, qaerwat, errmsg, errflg)
       import :: aerosol_properties, aerosol_state, r8
       class(aerosol_properties), intent(in) :: aero_props
       class(aerosol_state),      intent(in) :: aero_state
       integer,          intent(in)  :: ncol          ! number of columns
       integer,          intent(in)  :: nlev          ! number of vertical levels
       integer,          intent(in)  :: top_lev       ! top level for aerosol calculations
       real(r8),         intent(in)  :: pi            ! pi
       real(r8),         intent(in)  :: rhoh2o        ! density of liquid water (kg/m3)
       real(r8),         intent(in)  :: t(:,:)        ! temperature (K)
       real(r8),         intent(in)  :: pmid(:,:)     ! layer pressure (Pa)
       real(r8),         intent(in)  :: h2ommr(:,:)   ! specific humidity (kg/kg)
       real(r8),         intent(in)  :: cldn(:,:)     ! layer cloud fraction (0-1)
       integer,          intent(in)  :: bin_idx       ! mode index of the returned slices
       real(r8),         intent(out) :: dgnumwet(:,:) ! wet number mode diameter of mode bin_idx (m)
       real(r8),         intent(out) :: qaerwat(:,:)  ! aerosol water of mode bin_idx (g/g)
       character(len=*), intent(out) :: errmsg
       integer,          intent(out) :: errflg
     end subroutine water_uptake_diag_i
  end interface

  procedure(water_uptake_diag_i), pointer :: water_uptake_diag_fn => null()

  real(r8), parameter :: rh2odens = 1._r8/rhoh2o

contains

  !------------------------------------------------------------------------------
  ! register the diagnostic-list water uptake implementation (host wiring;
  ! called at initialization from code that has access to the portable
  ! modal aerosol schemes)
  !------------------------------------------------------------------------------
  subroutine modal_aerosol_state_register_water_uptake_diag(fn)
    procedure(water_uptake_diag_i) :: fn

    water_uptake_diag_fn => fn

  end subroutine modal_aerosol_state_register_water_uptake_diag

  !------------------------------------------------------------------------------
  !------------------------------------------------------------------------------
  function constructor(ncol, host, list_idx) result(newobj)
    integer, intent(in) :: ncol
    type(aero_host_binding_t), intent(in) :: host
    integer, intent(in), optional :: list_idx

    type(modal_aerosol_state), pointer :: newobj

    integer :: ierr

    allocate(newobj,stat=ierr)
    if( ierr /= 0 ) then
       nullify(newobj)
       return
    end if

    call newobj%set_ncol(ncol)
    newobj%host_ = host

    if (present(list_idx)) call newobj%set_list_idx(list_idx)

  end function constructor

  !------------------------------------------------------------------------------
  !------------------------------------------------------------------------------
  subroutine destructor(self)
    type(modal_aerosol_state), intent(inout) :: self

    ! disassociate the host binding (data referenced within is not owned here)
    self%host_ = aero_host_binding_t()

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
       call rad_cnst_get_aer_mmr(self%list_idx_, bin_ndx, spec_ndx, 'a', self%host_, mmrptr)
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

    call rad_cnst_get_aer_mmr(self%list_idx_, bin_ndx, species_ndx, 'a', self%host_, mmr)
  end subroutine get_ambient_mmr

  !------------------------------------------------------------------------------
  ! returns cloud-borne aerosol number mixing ratio for a given species index and bin index
  !------------------------------------------------------------------------------
  subroutine get_cldbrne_mmr(self, species_ndx, bin_ndx, mmr)
    class(modal_aerosol_state), intent(in) :: self
    integer, intent(in) :: species_ndx  ! species index
    integer, intent(in) :: bin_ndx      ! bin index
    real(r8), pointer :: mmr(:,:)       ! mass mixing ratios (ncol,nlev)

    call rad_cnst_get_aer_mmr(self%list_idx_, bin_ndx, species_ndx, 'c', self%host_, mmr)
  end subroutine get_cldbrne_mmr

  !------------------------------------------------------------------------------
  ! returns ambient aerosol number mixing ratio for a given species index and bin index
  !------------------------------------------------------------------------------
  subroutine get_ambient_num(self, bin_ndx, num)
    class(modal_aerosol_state), intent(in) :: self
    integer, intent(in) :: bin_ndx     ! bin index
    real(r8), pointer   :: num(:,:)    ! number densities

    call rad_cnst_get_mode_num(self%list_idx_, bin_ndx, 'a', self%host_, num)
  end subroutine get_ambient_num

  !------------------------------------------------------------------------------
  ! returns cloud-borne aerosol number mixing ratio for a given species index and bin index
  !------------------------------------------------------------------------------
  subroutine get_cldbrne_num(self, bin_ndx, num)
    class(modal_aerosol_state), intent(in) :: self
    integer, intent(in) :: bin_ndx             ! bin index
    real(r8), pointer :: num(:,:)

    call rad_cnst_get_mode_num(self%list_idx_, bin_ndx, 'c', self%host_, num)
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

    class(modal_aerosol_state), intent(in) :: self
    integer, intent(in) :: bin_ndx                ! bin number
    integer, intent(in) :: ncol                ! number of columns
    integer, intent(in) :: nlev                ! number of vertical levels
    character(len=*), intent(in) :: species_type  ! species type
    logical, intent(in) :: use_preexisting_ice ! pre-existing ice flag
    real(r8), intent(out) :: wght(:,:)

    character(len=aero_name_len) :: modetype
    real(r8), pointer :: dgnum(:,:,:)    ! mode dry radius
    real(r8) :: sigmag_aitken
    integer :: i,k

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
             call rad_aer_get_mode_props(0, bin_ndx, sigmag=sigmag_aitken)
             call get_mode_dry_diameter(self%host_, dgnum)
             do k = 1,nlev
                do i = 1,ncol
                   if (dgnum(i,k,bin_ndx) > 0._r8) then
                      ! only allow so4 with D>0.1 um in ice nucleation
                      wght(i,k) = max(0._r8,(0.5_r8 - 0.5_r8* &
                           erf(log(0.1e-6_r8/dgnum(i,k,bin_ndx))/ &
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

    class(modal_aerosol_state), intent(in) :: self
    integer, intent(in) :: bin_ndx                ! bin number
    integer, intent(in) :: col_ndx                ! column index
    integer, intent(in) :: lyr_ndx                ! vertical layer index
    character(len=*), intent(in) :: species_type  ! species type
    logical, intent(in) :: use_preexisting_ice    ! pre-existing ice flag
    real(r8), intent(out) :: wght

    character(len=aero_name_len) :: modetype
    real(r8), pointer :: dgnum(:,:,:)    ! mode dry radius
    real(r8) :: sigmag_aitken

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
             call get_mode_dry_diameter(self%host_, dgnum)

             if (dgnum(col_ndx,lyr_ndx,bin_ndx) > 0._r8) then
                ! only allow so4 with D>0.1 um in ice nucleation
                wght = max(0._r8,(0.5_r8 - 0.5_r8* &
                     erf(log(0.1e-6_r8/dgnum(col_ndx,lyr_ndx,bin_ndx))/ &
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
  ! For the climate list (list_idx==0) these were pre-computed by the water
  ! uptake calculation and are retrieved via the aerosol_mmr_host accessors
  ! (DGNUMWET/QAERWAT pbuf fields in CAM; dgncur_awet/qaerwat_aer registry
  ! fields written by the CCPPized wateruptake scheme in CAM-SIMA).
  ! Diagnostic lists are recomputed from the atmospheric state passed in by
  ! the caller, via the registered portable implementation (see the
  ! water_uptake_diag_i interface above).
  !------------------------------------------------------------------------------
  subroutine water_uptake(self, aero_props, bin_idx, ncol, nlev, top_lev, &
                          t, pmid, h2ommr, cldn, dgnumwet, qaerwat)

    class(modal_aerosol_state), intent(in) :: self
    class(aerosol_properties), intent(in) :: aero_props
    integer, intent(in) :: bin_idx              ! bin number
    integer, intent(in) :: ncol                 ! number of columns
    integer, intent(in) :: nlev                 ! number of levels
    integer, intent(in) :: top_lev              ! top level for aerosol calculations
    real(r8),intent(in) :: t(:,:)               ! temperature (K)
    real(r8),intent(in) :: pmid(:,:)            ! layer pressure (Pa)
    real(r8),intent(in) :: h2ommr(:,:)          ! specific humidity (kg/kg)
    real(r8),intent(in) :: cldn(:,:)            ! layer cloud fraction (0-1)
    real(r8),intent(out) :: dgnumwet(ncol,nlev) ! aerosol wet diameter (m)
    real(r8),intent(out) :: qaerwat(ncol,nlev)  ! aerosol water concentration (g/g)

    real(r8), pointer :: dgnumwet_m(:,:,:) ! number mode wet diameter for all modes
    real(r8), pointer :: qaerwat_m(:,:,:)  ! aerosol water (g/g) for all modes

    character(len=512) :: errmsg
    integer            :: errflg

    if (self%list_idx_ == 0) then
       ! water uptake and wet radius for the climate list has already been calculated
       call get_mode_wet_diameter(self%host_, dgnumwet_m)
       call get_mode_aer_water(self%host_, qaerwat_m)

       dgnumwet(:ncol,:nlev) = dgnumwet_m(:ncol,:nlev,bin_idx)
       qaerwat (:ncol,:nlev) =  qaerwat_m(:ncol,:nlev,bin_idx)

    else
       ! If doing a diagnostic calculation then need to calculate the wet radius
       ! and water uptake for the diagnostic modes
       if (.not. associated(water_uptake_diag_fn)) then
          call endrun('modal_aerosol_state::water_uptake: diagnostic-list water uptake ' // &
               'is not available: no implementation registered (the modal aerosol ' // &
               'schemes are not part of this build)')
       end if
       call water_uptake_diag_fn(aero_props, self, ncol, nlev, top_lev, pi, rhoh2o, &
            t, pmid, h2ommr, cldn, bin_idx, dgnumwet, qaerwat, errmsg, errflg)
       if (errflg /= 0) then
          call endrun('modal_aerosol_state::water_uptake: '//trim(errmsg))
       end if
    endif

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
  function wet_volume(self, aero_props, bin_idx, ncol, nlev, top_lev, &
                      t, pmid, h2ommr, cldn) result(vol)

    class(modal_aerosol_state), intent(in) :: self
    class(aerosol_properties), intent(in) :: aero_props

    integer, intent(in) :: bin_idx   ! bin number
    integer, intent(in) :: ncol      ! number of columns
    integer, intent(in) :: nlev      ! number of levels
    integer, intent(in) :: top_lev   ! top level for aerosol calculations
    real(r8),intent(in) :: t(:,:)    ! temperature (K)
    real(r8),intent(in) :: pmid(:,:) ! layer pressure (Pa)
    real(r8),intent(in) :: h2ommr(:,:) ! specific humidity (kg/kg)
    real(r8),intent(in) :: cldn(:,:) ! layer cloud fraction (0-1)

    real(r8) :: vol(ncol,nlev)       ! m3/kg

    real(r8) :: dryvol(ncol,nlev)
    real(r8) :: watervol(ncol,nlev)

    dryvol = self%dry_volume(aero_props, bin_idx, ncol, nlev)
    watervol = self%water_volume(aero_props, bin_idx, ncol, nlev, top_lev, &
                                 t, pmid, h2ommr, cldn)

    vol = watervol + dryvol

  end function wet_volume

  !------------------------------------------------------------------------------
  ! aerosol water volume (m3/kg) for given radiation diagnostic list number and bin number
  !------------------------------------------------------------------------------
  function water_volume(self, aero_props, bin_idx, ncol, nlev, top_lev, &
                        t, pmid, h2ommr, cldn) result(vol)

    class(modal_aerosol_state), intent(in) :: self
    class(aerosol_properties), intent(in) :: aero_props

    integer, intent(in) :: bin_idx   ! bin number
    integer, intent(in) :: ncol      ! number of columns
    integer, intent(in) :: nlev      ! number of levels
    integer, intent(in) :: top_lev   ! top level for aerosol calculations
    real(r8),intent(in) :: t(:,:)    ! temperature (K)
    real(r8),intent(in) :: pmid(:,:) ! layer pressure (Pa)
    real(r8),intent(in) :: h2ommr(:,:) ! specific humidity (kg/kg)
    real(r8),intent(in) :: cldn(:,:) ! layer cloud fraction (0-1)

    real(r8) :: vol(ncol,nlev)       ! m3/kg

    real(r8) :: dgnumwet(ncol,nlev)
    real(r8) :: qaerwat(ncol,nlev)

    call self%water_uptake(aero_props, bin_idx, ncol, nlev, top_lev, &
                           t, pmid, h2ommr, cldn, dgnumwet, qaerwat)

    vol(:ncol,:nlev) = qaerwat(:ncol,:nlev)*rh2odens
    where (vol<0._r8)
       vol = 0._r8
    end where

  end function water_volume

  !------------------------------------------------------------------------------
  ! aerosol wet diameter for a given mode
  !------------------------------------------------------------------------------
  function wet_diameter(self, bin_idx, ncol, nlev) result(diam)
    class(modal_aerosol_state), intent(in) :: self
    integer, intent(in) :: bin_idx   ! bin number
    integer, intent(in) :: ncol      ! number of columns
    integer, intent(in) :: nlev      ! number of levels

    real(r8) :: diam(ncol,nlev)

    real(r8), pointer :: dgnumwet(:,:,:)

    call get_mode_wet_diameter(self%host_, dgnumwet)

    diam(:ncol,:nlev) = dgnumwet(:ncol,:nlev,bin_idx)

  end function wet_diameter

  !------------------------------------------------------------------------------
  ! prescribed aerosol activation fraction for convective cloud
  !------------------------------------------------------------------------------
  function convcld_actfrac(self, aero_props, ibin, ispc, ncol, nlev) result(frac)

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

  !------------------------------------------------------------------------------
  ! aqueous chemistry partitioning -- used in sox_cldaero_update
  !------------------------------------------------------------------------------
  subroutine aqu_gain_binfraction(self, aero_props, type, qcw, delso4_o3rxn, faqgain)

    class(modal_aerosol_state), intent(in) :: self
    class(aerosol_properties), intent(in) :: aero_props ! aerosol properties object
    character(len=*), intent(in) :: type                ! aerosol species type
    real(r8), intent(in) :: qcw(:,:,:)                  ! cloud-borne aerosol volume mixing ratio
    real(r8), intent(in) :: delso4_o3rxn(:,:)           ! sulfate concentration change due to oxidation
    real(r8), intent(out) :: faqgain(:,:,:)             ! fraction gain in each mode / bin

    character(len=aero_name_len) :: modetype, spectype
    integer :: i,k,l,m,n,mm, ncol, nlev, nbins
    integer :: accum_n
    real(r8) :: sumf
    real(r8), allocatable :: qnum_c(:)

    ncol = self%ncol()
    nlev = size(qcw, 2)
    nbins = aero_props%nbins()

    !-------------------------------------------------------------------------
    ! compute factors for partitioning aerosol mass gains among modes.
    ! The factors are proportional to the activated particle MR for each
    ! mode, which is the MR of cloud drops "associated with" the mode
    ! thus we are assuming the cloud drop size is independent of the
    ! associated aerosol mode properties (i.e., drops associated with
    ! Aitken and coarse sea-salt particles are same size)
    !
    ! qnum_c(n) = activated particle number MR for mode n (these are just
    ! used for partitioning among modes, so don't need to divide by cldfrc)
    !-------------------------------------------------------------------------

    accum_n = -1
    do m = 1, nbins
       call rad_aer_get_info(0, m, mode_type=modetype)
       if (modetype=='accum') then
          accum_n = m
       end if
    end do

    allocate(qnum_c(nbins))

    faqgain = 0.0_r8

    lev_loop: do k = 1,nlev
       col_loop: do i = 1,ncol
          do m = 1, nbins
             mm = aero_props%indexer(m,0)
             qnum_c(m) = max( 0.0_r8, qcw(i,k,mm) )
           end do

          ! force qnum_c(n) to be positive for n=modeptr_accum or n=1
          n = accum_n
          if (n <= 0) n = 1
          qnum_c(n) = max( 1.0e-10_r8, qnum_c(n) )

          ! faqgain_so4(n) = fraction of total so4_c gain going to mode n
          ! these are proportional to the activated particle MR for each mode
          sumf = 0.0_r8
          do n = 1, nbins
             do l = 1, aero_props%nspecies(n)
                call  aero_props%get(n,l, spectype=spectype)
                if (trim(spectype) == trim(type)) then
                   faqgain(n,i,k) = qnum_c(n)
                   sumf = sumf + faqgain(n,i,k)
                end if
             end do
          end do

          if (sumf > 0.0_r8) then
             do n = 1, nbins
                faqgain(n,i,k) = faqgain(n,i,k) / sumf
             end do
          end if
          ! at this point (sumf <= 0.0) only when all the faqgain_so4 are zero

       end do col_loop
    end do lev_loop

    deallocate(qnum_c)

  end subroutine aqu_gain_binfraction

end module modal_aerosol_state_mod
