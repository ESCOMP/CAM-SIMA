module aerosol_mmr_host

!------------------------------------------------------------------------------------------------
!
! Host-binding module for aerosol MMR retrieval (CAM-SIMA flavor).
!
! This is the CAM-SIMA side of a per-host module pair: CAM provides a module
! of the same name backed by physics_state and the physics buffer. The pair
! owns the opaque aero_host_binding_t handle; shared aerosol interface code
! stores and passes the handle without referencing host-model data structures
! directly. Here the routines access the CCPP constituents array to return
! mixing ratio pointers.
!
! The pair also provides get_mode_dry_diameter, get_mode_wet_diameter and
! get_mode_aer_water accessors for the climate-list mode diagnostics computed
! by the calcsize/wateruptake calculations.
!
! Ported from the CAM flavor: replaces pbuf/state%q with CCPP constituents.
!
!------------------------------------------------------------------------------------------------

use shr_kind_mod,   only: r8 => shr_kind_r8
use ccpp_kinds,     only: kind_phys

implicit none
private

! Opaque host-binding handle: aggregates the host-model data references
! (for CAM-SIMA: the CCPP constituents array) needed to retrieve aerosol
! fields. Shared aerosol interface code stores and passes this handle
! without referencing host-model data structures directly; only this
! module looks inside it.
type :: aero_host_binding_t
   real(kind_phys), pointer :: constituents(:,:,:) => null()
end type aero_host_binding_t

! define generic interface for MMR retrieval
interface rad_cnst_get_aer_mmr
   module procedure rad_cnst_get_aer_mmr_by_idx
   module procedure rad_cnst_get_mam_mmr_by_idx
   module procedure rad_cnst_get_aer_mmr_by_idx_host
   module procedure rad_cnst_get_mam_mmr_by_idx_host
end interface

! generic interfaces dispatching between the legacy (constituents) variants
! and the host-binding handle variants
interface rad_cnst_get_mode_num
   module procedure rad_cnst_get_mode_num_ccpp
   module procedure rad_cnst_get_mode_num_host
end interface

interface rad_cnst_get_bin_num
   module procedure rad_cnst_get_bin_num_ccpp
   module procedure rad_cnst_get_bin_num_host
end interface

interface rad_cnst_get_bin_mmr_by_idx
   module procedure rad_cnst_get_bin_mmr_by_idx_ccpp
   module procedure rad_cnst_get_bin_mmr_by_idx_host
end interface

! values for constituents with requested value of zero
real(r8), allocatable, target :: zero_cols(:,:)

public :: aero_host_binding_t
public :: aero_host_binding   ! build a handle from host data structures
public :: get_mode_dry_diameter  ! dry number mode diameters of the climate list
public :: get_mode_wet_diameter  ! wet number mode diameters of the climate list
public :: get_mode_aer_water     ! aerosol water of the climate list modes
public :: aerosol_mmr_init    ! allocate zero_cols
public :: get_host_idx
public :: resolve_mode_idx, resolve_bin_idx
public :: resolve_bulk_idx
public :: rad_cnst_get_aer_mmr
public :: rad_cnst_get_mam_mmr_idx
public :: rad_cnst_get_mode_num
public :: rad_cnst_get_mode_num_idx
public :: rad_cnst_get_bin_mmr_by_idx
public :: rad_cnst_get_bin_num
public :: rad_cnst_get_bin_num_idx
public :: rad_cnst_get_carma_mmr_idx
public :: rad_cnst_get_bin_mmr
public :: rad_aer_diag_init
public :: rad_aer_diag_out

!==============================================================================
contains
!==============================================================================

subroutine aerosol_mmr_init()
   use physics_grid,   only: columns_on_task
   use vert_coord,     only: pver
   use shr_kind_mod,   only: shr_kind_cl
   use cam_abortutils, only: check_allocate

   integer                    :: ierr
   character(len=shr_kind_cl) :: errmsg

   ! Allocate zero_cols array (must be called after grid/vert is set up)
   if (.not. allocated(zero_cols)) then
      allocate(zero_cols(columns_on_task, pver), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, 'aerosol_mmr_init', 'zero_cols(columns_on_task, pver)', &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)
      zero_cols = 0._r8
   end if
end subroutine aerosol_mmr_init

!================================================================================================

function aero_host_binding(constituents) result(host)

   ! Build a host-binding handle from CAM-SIMA host data structures.
   ! Called from host-side wiring only (aerosol_instances_mod); the
   ! resulting handle is stored opaquely by the aerosol_state objects.

   real(kind_phys), pointer, intent(in) :: constituents(:,:,:)
   type(aero_host_binding_t) :: host

   host%constituents => constituents

end function aero_host_binding

!================================================================================================

subroutine get_mode_dry_diameter(host, dgnum)

   ! Return the dry number mode diameters (all modes) of the climate list,
   ! computed by the modal_aero_calcsize calculation.
   ! CAM-SIMA: the dgncur_a registry field written by the CCPPized scheme
   ! (the host handle is not needed; kept for interface parity with CAM).

   use physics_types, only: dgncur_a

   type(aero_host_binding_t), intent(in) :: host
   real(r8),                  pointer    :: dgnum(:,:,:)

   dgnum => dgncur_a

end subroutine get_mode_dry_diameter

!================================================================================================

subroutine get_mode_wet_diameter(host, dgnumwet)

   ! Return the wet number mode diameters (all modes) of the climate list,
   ! computed by the modal_aero_wateruptake calculation.
   ! CAM-SIMA: the dgncur_awet registry field written by the CCPPized scheme
   ! (the host handle is not needed; kept for interface parity with CAM).

   use physics_types, only: dgncur_awet

   type(aero_host_binding_t), intent(in) :: host
   real(r8),                  pointer    :: dgnumwet(:,:,:)

   dgnumwet => dgncur_awet

end subroutine get_mode_wet_diameter

!================================================================================================

subroutine get_mode_aer_water(host, qaerwat)

   ! Return the aerosol water (all modes) of the climate list,
   ! computed by the modal_aero_wateruptake calculation.
   ! CAM-SIMA: the qaerwat_aer registry field written by the CCPPized scheme
   ! (the host handle is not needed; kept for interface parity with CAM).

   use physics_types, only: qaerwat_aer

   type(aero_host_binding_t), intent(in) :: host
   real(r8),                  pointer    :: qaerwat(:,:,:)

   qaerwat => qaerwat_aer

end subroutine get_mode_aer_water

!================================================================================================

integer function get_host_idx(source, name, routine)

   ! Get index of name in the CCPP constituents array.
   ! Both 'A' (advected) and 'N' (non-advected) sources resolve through
   ! const_get_index, which searches the unified CCPP constituent table.
   ! 'Z' returns -1 (zero field).

   use cam_constituents, only: const_get_index
   use cam_abortutils,   only: endrun

   character(len=*), intent(in) :: source
   character(len=*), intent(in) :: name
   character(len=*), intent(in) :: routine  ! name of calling routine

   integer :: idx
   !-----------------------------------------------------------------------------

   if (source(1:1) == 'N' .or. source(1:1) == 'A') then
      call const_get_index(trim(name), idx)
      ! const_get_index aborts by default if name is not found
   else if (source(1:1) == 'Z') then
      idx = -1
   else
      call endrun(routine//' ERROR: invalid source for species '//trim(name))
   end if

   get_host_idx = idx

end function get_host_idx

!===========================

subroutine resolve_mode_idx(modes)

   ! Initialize the mode definitions by looking up the relevant indices in the
   ! CCPP constituents array, and getting the physprop IDs

   use shr_kind_mod,   only: shr_kind_cl
   use phys_prop,      only: physprop_get_id
   use cam_abortutils, only: endrun, check_allocate
   use radiative_aerosol_definitions, only: modes_t

   ! Arguments
   type(modes_t), intent(inout) :: modes

   ! Local variables
   integer :: m, ispec, nspec
   integer :: ierr
   character(len=shr_kind_cl) :: errmsg

   character(len=*), parameter :: routine = 'resolve_mode_idx'
   !-----------------------------------------------------------------------------

   do m = 1, modes%nmodes

      ! indices for number mixing ratio components
      modes%comps(m)%idx_num_a = get_host_idx(modes%comps(m)%source_num_a, modes%comps(m)%camname_num_a, routine)
      modes%comps(m)%idx_num_c = get_host_idx(modes%comps(m)%source_num_c, modes%comps(m)%camname_num_c, routine)

      ! allocate memory for species
      nspec = modes%comps(m)%nspec
      allocate( &
         modes%comps(m)%idx_mmr_a(nspec), &
         modes%comps(m)%idx_mmr_c(nspec), &
         modes%comps(m)%idx_props(nspec), &
         stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, routine, 'modes%comps(m)%idx_mmr_a/idx_mmr_c/idx_props(nspec)', &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)

      do ispec = 1, nspec

         ! indices for species mixing ratio components
         modes%comps(m)%idx_mmr_a(ispec) = get_host_idx(modes%comps(m)%source_mmr_a(ispec), &
                                                   modes%comps(m)%camname_mmr_a(ispec), routine)
         modes%comps(m)%idx_mmr_c(ispec) = get_host_idx(modes%comps(m)%source_mmr_c(ispec), &
                                                   modes%comps(m)%camname_mmr_c(ispec), routine)

         ! get physprop ID
         modes%comps(m)%idx_props(ispec) = physprop_get_id(modes%comps(m)%props(ispec))
         if (modes%comps(m)%idx_props(ispec) == -1) then
            call endrun(routine//' : ERROR idx not found for '//trim(modes%comps(m)%props(ispec)))
         end if

      end do

   end do

end subroutine resolve_mode_idx

!===========================

subroutine resolve_bin_idx(bins)

   ! Initialize the bin definitions by looking up the relevant indices in the
   ! CCPP constituents array, and getting the physprop IDs

   use shr_kind_mod,   only: shr_kind_cl
   use phys_prop,      only: physprop_get_id
   use cam_abortutils, only: endrun, check_allocate
   use radiative_aerosol_definitions, only: bins_t

   ! Arguments
   type(bins_t), intent(inout) :: bins

   ! Local variables
   integer :: m, ispec, nspec
   integer :: ierr
   character(len=shr_kind_cl) :: errmsg

   character(len=*), parameter :: routine = 'resolve_bin_idx'
   !-----------------------------------------------------------------------------

   do m = 1, bins%nbins

      ! indices for number mixing ratio components
      bins%comps(m)%idx_num_a = get_host_idx(bins%comps(m)%source_num_a, bins%comps(m)%camname_num_a, routine)
      bins%comps(m)%idx_num_c = get_host_idx(bins%comps(m)%source_num_c, bins%comps(m)%camname_num_c, routine)
      if (bins%comps(m)%source_mass_a /= 'NOTSET' .and. bins%comps(m)%camname_mass_a /= 'NOTSET') then
         bins%comps(m)%idx_mass_a = get_host_idx(bins%comps(m)%source_mass_a, bins%comps(m)%camname_mass_a, routine)
      end if
      if (bins%comps(m)%source_mass_c /= 'NOTSET' .and. bins%comps(m)%camname_mass_c /= 'NOTSET') then
         bins%comps(m)%idx_mass_c = get_host_idx(bins%comps(m)%source_mass_c, bins%comps(m)%camname_mass_c, routine)
      end if

      ! allocate memory for species
      nspec = bins%comps(m)%nspec
      allocate( &
         bins%comps(m)%idx_mmr_a(nspec), &
         bins%comps(m)%idx_mmr_c(nspec), &
         bins%comps(m)%idx_props(nspec), &
         stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, routine, 'bins%comps(m)%idx_mmr_a/idx_mmr_c/idx_props(nspec)', &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)

      do ispec = 1, nspec

         ! indices for species mixing ratio components
         bins%comps(m)%idx_mmr_a(ispec) = get_host_idx(bins%comps(m)%source_mmr_a(ispec), &
                                                   bins%comps(m)%camname_mmr_a(ispec), routine)
         bins%comps(m)%idx_mmr_c(ispec) = get_host_idx(bins%comps(m)%source_mmr_c(ispec), &
                                                   bins%comps(m)%camname_mmr_c(ispec), routine)

         ! get physprop ID
         bins%comps(m)%idx_props(ispec) = physprop_get_id(bins%comps(m)%props(ispec))
         if (bins%comps(m)%idx_props(ispec) == -1) then
            call endrun(routine//' : ERROR idx not found for '//trim(bins%comps(m)%props(ispec)))
         end if

      end do

   end do

end subroutine resolve_bin_idx

!===========================

subroutine resolve_bulk_idx(aerlist)

   ! Resolve host-specific indices for bulk aerosols via CCPP constituents.
   ! Must be called before list_resolve_physprops (which resolves physprop IDs).

   use cam_abortutils, only: endrun
   use radiative_aerosol_definitions, only: aerlist_t

   type(aerlist_t), intent(inout) :: aerlist

   integer :: i
   character(len=*), parameter :: routine = 'resolve_bulk_idx'
   !-----------------------------------------------------------------------------

   do i = 1, aerlist%numaerosols
      aerlist%aer(i)%idx = get_host_idx(aerlist%aer(i)%source, aerlist%aer(i)%camname, routine)
   end do

end subroutine resolve_bulk_idx

!================================================================================================

subroutine rad_cnst_get_aer_mmr_by_idx(list_idx, aer_idx, constituents, mmr)

   ! Return pointer to mass mixing ratio for the bulk aerosol from the specified
   ! climate or diagnostic list, using the CCPP constituents array.

   use string_utils,   only: to_str
   use cam_abortutils, only: endrun
   use radiative_aerosol_definitions, only: N_DIAG, aerlist_t, bulk_aerosol_list

   ! Arguments
   integer,                        intent(in)  :: list_idx    ! index of the climate or a diagnostic list
   integer,                        intent(in)  :: aer_idx
   real(kind_phys), target,        intent(in)  :: constituents(:,:,:)
   real(r8),                       pointer     :: mmr(:,:)

   ! Local variables
   integer :: idx
   character(len=1) :: source
   type(aerlist_t), pointer :: aerlist
   character(len=*), parameter :: subname = 'rad_cnst_get_aer_mmr_by_idx'
   !-----------------------------------------------------------------------------

   if (list_idx >= 0 .and. list_idx <= N_DIAG) then
      aerlist => bulk_aerosol_list(list_idx)
   else
      call endrun(subname//': list_idx out of bounds: list_idx = '//to_str(list_idx))
   end if

   ! Check for valid input aerosol index
   if (aer_idx < 1  .or.  aer_idx > aerlist%numaerosols) then
      call endrun(subname//': aerosol list index out of range: aer_idx = '// &
           to_str(aer_idx)//', numaerosols = '//to_str(aerlist%numaerosols))
   end if

   ! Get data source
   source = aerlist%aer(aer_idx)%source
   idx    = aerlist%aer(aer_idx)%idx
   select case(source)
   case ('A','N')
      mmr => constituents(:,:,idx)
   case ('Z')
      mmr => zero_cols
   case default
      call endrun(subname//': unrecognized source "'//source//'"; must be "A", "N" or "Z"')
   end select

end subroutine rad_cnst_get_aer_mmr_by_idx

!================================================================================================

subroutine rad_cnst_get_aer_mmr_by_idx_host(list_idx, aer_idx, host, mmr)

   ! Host-binding handle variant: unpack the handle and delegate.

   integer,                   intent(in) :: list_idx    ! index of the climate or a diagnostic list
   integer,                   intent(in) :: aer_idx
   type(aero_host_binding_t), intent(in) :: host
   real(r8),                  pointer    :: mmr(:,:)

   call rad_cnst_get_aer_mmr_by_idx(list_idx, aer_idx, host%constituents, mmr)

end subroutine rad_cnst_get_aer_mmr_by_idx_host

!================================================================================================

subroutine rad_cnst_get_mam_mmr_by_idx(list_idx, mode_idx, spec_idx, phase, constituents, mmr)

   ! Return pointer to mass mixing ratio for the modal aerosol specie from the specified
   ! climate or diagnostic list, using the CCPP constituents array.

   use string_utils,   only: to_str
   use cam_abortutils, only: endrun
   use radiative_aerosol_definitions, only: N_DIAG, modelist_t, modal_aerosol_list, modes

   ! Arguments
   integer,                     intent(in) :: list_idx    ! index of the climate or a diagnostic list
   integer,                     intent(in) :: mode_idx    ! mode index
   integer,                     intent(in) :: spec_idx    ! index of specie in the mode
   character(len=1),            intent(in) :: phase       ! 'a' for interstitial, 'c' for cloud borne
   real(kind_phys), target,     intent(in) :: constituents(:,:,:)
   real(r8),                    pointer    :: mmr(:,:)

   ! Local variables
   integer :: m_idx
   integer :: idx
   character(len=1) :: source
   type(modelist_t), pointer :: mlist
   character(len=*), parameter :: subname = 'rad_cnst_get_mam_mmr_by_idx'
   !-----------------------------------------------------------------------------

   if (list_idx >= 0 .and. list_idx <= N_DIAG) then
      mlist => modal_aerosol_list(list_idx)
   else
      call endrun(subname//': list_idx out of bounds: list_idx = '//to_str(list_idx))
   end if

   ! Check for valid mode index
   if (mode_idx < 1  .or.  mode_idx > mlist%nmodes) then
      call endrun(subname//': mode list index out of range: mode_idx = '// &
           to_str(mode_idx)//', nmodes = '//to_str(mlist%nmodes))
   end if

   ! Get the index for the corresponding mode in the mode definition object
   m_idx = mlist%idx(mode_idx)

   ! Check for valid specie index
   if (spec_idx < 1  .or.  spec_idx > modes%comps(m_idx)%nspec) then
      call endrun(subname//': species list index out of range: spec_idx = '// &
           to_str(spec_idx)//', nspec = '//to_str(modes%comps(m_idx)%nspec))
   end if

   ! Get data source
   if (phase == 'a') then
      source = modes%comps(m_idx)%source_mmr_a(spec_idx)
      idx    = modes%comps(m_idx)%idx_mmr_a(spec_idx)
   else if (phase == 'c') then
      source = modes%comps(m_idx)%source_mmr_c(spec_idx)
      idx    = modes%comps(m_idx)%idx_mmr_c(spec_idx)
   else
      call endrun(subname//': unrecognized phase "'//phase//'"; must be "a" or "c"')
   end if

   select case(source)
   case ('A','N')
      mmr => constituents(:,:,idx)
   case ('Z')
      mmr => zero_cols
   case default
      call endrun(subname//': unrecognized source "'//source//'"; must be "A", "N" or "Z"')
   end select

end subroutine rad_cnst_get_mam_mmr_by_idx

!================================================================================================

subroutine rad_cnst_get_mam_mmr_by_idx_host(list_idx, mode_idx, spec_idx, phase, host, mmr)

   ! Host-binding handle variant: unpack the handle and delegate.

   integer,                   intent(in) :: list_idx    ! index of the climate or a diagnostic list
   integer,                   intent(in) :: mode_idx    ! mode index
   integer,                   intent(in) :: spec_idx    ! index of specie in the mode
   character(len=1),          intent(in) :: phase       ! 'a' for interstitial, 'c' for cloud borne
   type(aero_host_binding_t), intent(in) :: host
   real(r8),                  pointer    :: mmr(:,:)

   call rad_cnst_get_mam_mmr_by_idx(list_idx, mode_idx, spec_idx, phase, host%constituents, mmr)

end subroutine rad_cnst_get_mam_mmr_by_idx_host

!================================================================================================

subroutine rad_cnst_get_bin_mmr_by_idx_ccpp(list_idx, bin_idx, spec_idx, phase, constituents, mmr)

   ! Return pointer to mass mixing ratio for the sectional aerosol specie.

   use string_utils,   only: to_str
   use cam_abortutils, only: endrun
   use radiative_aerosol_definitions, only: N_DIAG, binlist_t, sectional_aerosol_list, bins

   ! Arguments
   integer,                     intent(in) :: list_idx
   integer,                     intent(in) :: bin_idx
   integer,                     intent(in) :: spec_idx
   character(len=1),            intent(in) :: phase
   real(kind_phys), target,     intent(in) :: constituents(:,:,:)
   real(r8),                    pointer    :: mmr(:,:)

   ! Local variables
   integer :: s_idx, idx
   character(len=1) :: source
   type(binlist_t), pointer :: slist
   character(len=*), parameter :: subname = 'rad_cnst_get_bin_mmr_by_idx'
   !-----------------------------------------------------------------------------

   if (list_idx >= 0 .and. list_idx <= N_DIAG) then
      slist => sectional_aerosol_list(list_idx)
   else
      call endrun(subname//': list_idx out of bounds: list_idx = '//to_str(list_idx))
   end if

   if (bin_idx < 1  .or.  bin_idx > slist%nbins) then
      call endrun(subname//': bin list index out of range: bin_idx = '// &
           to_str(bin_idx)//', nbins = '//to_str(slist%nbins))
   end if

   s_idx = slist%idx(bin_idx)

   if (spec_idx < 1  .or.  spec_idx > bins%comps(s_idx)%nspec) then
      call endrun(subname//': species list index out of range: spec_idx = '// &
           to_str(spec_idx)//', nspec = '//to_str(bins%comps(s_idx)%nspec))
   end if

   if (phase == 'a') then
      source = bins%comps(s_idx)%source_mmr_a(spec_idx)
      idx    = bins%comps(s_idx)%idx_mmr_a(spec_idx)
   else if (phase == 'c') then
      source = bins%comps(s_idx)%source_mmr_c(spec_idx)
      idx    = bins%comps(s_idx)%idx_mmr_c(spec_idx)
   else
      call endrun(subname//': unrecognized phase "'//phase//'"; must be "a" or "c"')
   end if

   select case(source)
   case ('A','N')
      mmr => constituents(:,:,idx)
   case ('Z')
      mmr => zero_cols
   case default
      call endrun(subname//': unrecognized source "'//source//'"; must be "A", "N" or "Z"')
   end select

end subroutine rad_cnst_get_bin_mmr_by_idx_ccpp

!================================================================================================

subroutine rad_cnst_get_bin_mmr_by_idx_host(list_idx, bin_idx, spec_idx, phase, host, mmr)

   ! Host-binding handle variant: unpack the handle and delegate.

   integer,                   intent(in) :: list_idx    ! index of the climate or a diagnostic list
   integer,                   intent(in) :: bin_idx     ! bin index
   integer,                   intent(in) :: spec_idx    ! index of specie in the bin
   character(len=1),          intent(in) :: phase       ! 'a' for interstitial, 'c' for cloud borne
   type(aero_host_binding_t), intent(in) :: host
   real(r8),                  pointer    :: mmr(:,:)

   call rad_cnst_get_bin_mmr_by_idx_ccpp(list_idx, bin_idx, spec_idx, phase, host%constituents, mmr)

end subroutine rad_cnst_get_bin_mmr_by_idx_host

!================================================================================================

subroutine rad_cnst_get_mam_mmr_idx(mode_idx, spec_idx, idx)

   ! Return constituent index of mam specie mass mixing ratio for aerosol modes in
   ! the climate list.

   use string_utils,   only: to_str
   use cam_abortutils, only: endrun
   use radiative_aerosol_definitions, only: modelist_t, modes, modal_aerosol_list

   integer, intent(in)  :: mode_idx
   integer, intent(in)  :: spec_idx
   integer, intent(out) :: idx

   integer :: m_idx
   type(modelist_t), pointer :: mlist
   character(len=*), parameter :: subname = 'rad_cnst_get_mam_mmr_idx'
   !-----------------------------------------------------------------------------

   mlist => modal_aerosol_list(0)

   if (mode_idx < 1  .or.  mode_idx > mlist%nmodes) then
      call endrun(subname//': mode list index out of range: mode_idx = '// &
           to_str(mode_idx)//', nmodes = '//to_str(mlist%nmodes))
   end if

   m_idx = mlist%idx(mode_idx)

   if (spec_idx < 1  .or.  spec_idx > modes%comps(m_idx)%nspec) then
      call endrun(subname//': species list index out of range: spec_idx = '// &
           to_str(spec_idx)//', nspec = '//to_str(modes%comps(m_idx)%nspec))
   end if

   idx = modes%comps(m_idx)%idx_mmr_a(spec_idx)

end subroutine rad_cnst_get_mam_mmr_idx

!================================================================================================

subroutine rad_cnst_get_carma_mmr_idx(bin_idx, spec_idx, idx)

   use string_utils,   only: to_str
   use cam_abortutils, only: endrun
   use radiative_aerosol_definitions, only: binlist_t, bins, sectional_aerosol_list

   integer, intent(in)  :: bin_idx
   integer, intent(in)  :: spec_idx
   integer, intent(out) :: idx

   integer :: b_idx
   type(binlist_t), pointer :: slist
   character(len=*), parameter :: subname = 'rad_cnst_get_carma_mmr_idx'
   !-----------------------------------------------------------------------------

   slist => sectional_aerosol_list(0)

   if (bin_idx < 1  .or.  bin_idx > slist%nbins) then
      call endrun(subname//': bin list index out of range: bin_idx = '// &
           to_str(bin_idx)//', nbins = '//to_str(slist%nbins))
   end if

   b_idx = slist%idx(bin_idx)

   if (spec_idx < 1  .or.  spec_idx > bins%comps(b_idx)%nspec) then
      call endrun(subname//': species list index out of range: spec_idx = '// &
           to_str(spec_idx)//', nspec = '//to_str(bins%comps(b_idx)%nspec))
   end if

   idx = bins%comps(b_idx)%idx_mmr_a(spec_idx)

end subroutine rad_cnst_get_carma_mmr_idx

!================================================================================================

subroutine rad_cnst_get_bin_mmr(list_idx, bin_idx, phase, constituents, mmr)

   use string_utils,   only: to_str
   use cam_abortutils, only: endrun
   use radiative_aerosol_definitions, only: N_DIAG, binlist_t, sectional_aerosol_list, bins

   integer,                     intent(in) :: list_idx
   integer,                     intent(in) :: bin_idx
   character(len=1),            intent(in) :: phase
   real(kind_phys), target,     intent(in) :: constituents(:,:,:)
   real(r8),                    pointer    :: mmr(:,:)

   integer :: m_idx, idx
   character(len=1) :: source
   type(binlist_t), pointer :: slist
   character(len=*), parameter :: subname = 'rad_cnst_get_bin_mmr'
   !-----------------------------------------------------------------------------

   if (list_idx >= 0 .and. list_idx <= N_DIAG) then
      slist => sectional_aerosol_list(list_idx)
   else
      call endrun(subname//': list_idx out of bounds: list_idx = '//to_str(list_idx))
   end if

   if (bin_idx < 1  .or.  bin_idx > slist%nbins) then
      call endrun(subname//': bin list index out of range: bin_idx = '// &
           to_str(bin_idx)//', nbins = '//to_str(slist%nbins))
   end if

   m_idx = slist%idx(bin_idx)

   if (phase == 'a') then
      source = bins%comps(m_idx)%source_mass_a
      idx    = bins%comps(m_idx)%idx_mass_a
   else if (phase == 'c') then
      source = bins%comps(m_idx)%source_mass_c
      idx    = bins%comps(m_idx)%idx_mass_c
   else
      call endrun(subname//': unrecognized phase "'//phase//'"; must be "a" or "c"')
   end if

   select case(source)
   case ('A','N')
      mmr => constituents(:,:,idx)
   case ('Z')
      mmr => zero_cols
   case default
      call endrun(subname//': unrecognized source "'//source//'"; must be "A", "N" or "Z"')
   end select

end subroutine rad_cnst_get_bin_mmr

!================================================================================================

subroutine rad_cnst_get_mode_num_ccpp(list_idx, mode_idx, phase, constituents, num)

   use string_utils,   only: to_str
   use cam_abortutils, only: endrun
   use radiative_aerosol_definitions, only: N_DIAG, modelist_t, modal_aerosol_list, modes

   integer,                     intent(in) :: list_idx
   integer,                     intent(in) :: mode_idx
   character(len=1),            intent(in) :: phase
   real(kind_phys), target,     intent(in) :: constituents(:,:,:)
   real(r8),                    pointer    :: num(:,:)

   integer :: m_idx, idx
   character(len=1) :: source
   type(modelist_t), pointer :: mlist
   character(len=*), parameter :: subname = 'rad_cnst_get_mode_num'
   !-----------------------------------------------------------------------------

   if (list_idx >= 0 .and. list_idx <= N_DIAG) then
      mlist => modal_aerosol_list(list_idx)
   else
      call endrun(subname//': list_idx out of bounds: list_idx = '//to_str(list_idx))
   end if

   if (mode_idx < 1  .or.  mode_idx > mlist%nmodes) then
      call endrun(subname//': mode list index out of range: mode_idx = '// &
           to_str(mode_idx)//', nmodes = '//to_str(mlist%nmodes))
   end if

   m_idx = mlist%idx(mode_idx)

   if (phase == 'a') then
      source = modes%comps(m_idx)%source_num_a
      idx    = modes%comps(m_idx)%idx_num_a
   else if (phase == 'c') then
      source = modes%comps(m_idx)%source_num_c
      idx    = modes%comps(m_idx)%idx_num_c
   else
      call endrun(subname//': unrecognized phase "'//phase//'"; must be "a" or "c"')
   end if

   select case(source)
   case ('A','N')
      num => constituents(:,:,idx)
   case ('Z')
      num => zero_cols
   case default
      call endrun(subname//': unrecognized source "'//source//'"; must be "A", "N" or "Z"')
   end select

end subroutine rad_cnst_get_mode_num_ccpp

!================================================================================================

subroutine rad_cnst_get_mode_num_host(list_idx, mode_idx, phase, host, num)

   ! Host-binding handle variant: unpack the handle and delegate.

   integer,                   intent(in) :: list_idx    ! index of the climate or a diagnostic list
   integer,                   intent(in) :: mode_idx    ! mode index
   character(len=1),          intent(in) :: phase       ! 'a' for interstitial, 'c' for cloud borne
   type(aero_host_binding_t), intent(in) :: host
   real(r8),                  pointer    :: num(:,:)

   call rad_cnst_get_mode_num_ccpp(list_idx, mode_idx, phase, host%constituents, num)

end subroutine rad_cnst_get_mode_num_host

!================================================================================================

subroutine rad_cnst_get_bin_num_ccpp(list_idx, bin_idx, phase, constituents, num)

   use string_utils,   only: to_str
   use cam_abortutils, only: endrun
   use radiative_aerosol_definitions, only: N_DIAG, binlist_t, sectional_aerosol_list, bins

   integer,                     intent(in) :: list_idx
   integer,                     intent(in) :: bin_idx
   character(len=1),            intent(in) :: phase
   real(kind_phys), target,     intent(in) :: constituents(:,:,:)
   real(r8),                    pointer    :: num(:,:)

   integer :: m_idx, idx
   character(len=1) :: source
   type(binlist_t), pointer :: slist
   character(len=*), parameter :: subname = 'rad_cnst_get_bin_num'
   !-----------------------------------------------------------------------------

   if (list_idx >= 0 .and. list_idx <= N_DIAG) then
      slist => sectional_aerosol_list(list_idx)
   else
      call endrun(subname//': list_idx out of bounds: list_idx = '//to_str(list_idx))
   end if

   if (bin_idx < 1  .or.  bin_idx > slist%nbins) then
      call endrun(subname//': bin list index out of range: bin_idx = '// &
           to_str(bin_idx)//', nbins = '//to_str(slist%nbins))
   end if

   m_idx = slist%idx(bin_idx)

   if (phase == 'a') then
      source = bins%comps(m_idx)%source_num_a
      idx    = bins%comps(m_idx)%idx_num_a
   else if (phase == 'c') then
      source = bins%comps(m_idx)%source_num_c
      idx    = bins%comps(m_idx)%idx_num_c
   else
      call endrun(subname//': unrecognized phase "'//phase//'"; must be "a" or "c"')
   end if

   select case(source)
   case ('A','N')
      num => constituents(:,:,idx)
   case ('Z')
      num => zero_cols
   case default
      call endrun(subname//': unrecognized source "'//source//'"; must be "A", "N" or "Z"')
   end select

end subroutine rad_cnst_get_bin_num_ccpp

!================================================================================================

subroutine rad_cnst_get_bin_num_host(list_idx, bin_idx, phase, host, num)

   ! Host-binding handle variant: unpack the handle and delegate.

   integer,                   intent(in) :: list_idx    ! index of the climate or a diagnostic list
   integer,                   intent(in) :: bin_idx     ! bin index
   character(len=1),          intent(in) :: phase       ! 'a' for interstitial, 'c' for cloud borne
   type(aero_host_binding_t), intent(in) :: host
   real(r8),                  pointer    :: num(:,:)

   call rad_cnst_get_bin_num_ccpp(list_idx, bin_idx, phase, host%constituents, num)

end subroutine rad_cnst_get_bin_num_host

!================================================================================================

subroutine rad_cnst_get_mode_num_idx(mode_idx, cnst_idx)

   use string_utils,   only: to_str
   use cam_abortutils, only: endrun
   use radiative_aerosol_definitions, only: modelist_t, modes, modal_aerosol_list

   integer,  intent(in)  :: mode_idx
   integer,  intent(out) :: cnst_idx

   integer :: m_idx
   character(len=1) :: source
   type(modelist_t), pointer :: mlist
   character(len=*), parameter :: subname = 'rad_cnst_get_mode_num_idx'
   !-----------------------------------------------------------------------------

   mlist => modal_aerosol_list(0)

   if (mode_idx < 1  .or.  mode_idx > mlist%nmodes) then
      call endrun(subname//': mode list index out of range: mode_idx = '// &
           to_str(mode_idx)//', nmodes = '//to_str(mlist%nmodes))
   end if

   m_idx = mlist%idx(mode_idx)

   source = modes%comps(m_idx)%source_num_a
   if (source /= 'A') then
      call endrun(subname//': requested mode number index not in constituent array; source = '//source)
   end if

   cnst_idx = modes%comps(m_idx)%idx_num_a

end subroutine rad_cnst_get_mode_num_idx

!================================================================================================

subroutine rad_cnst_get_bin_num_idx(bin_idx, cnst_idx)

   use string_utils,   only: to_str
   use cam_abortutils, only: endrun
   use radiative_aerosol_definitions, only: binlist_t, bins, sectional_aerosol_list

   integer,  intent(in)  :: bin_idx
   integer,  intent(out) :: cnst_idx

   integer :: b_idx
   character(len=1) :: source
   type(binlist_t), pointer :: slist
   character(len=*), parameter :: subname = 'rad_cnst_get_bin_num_idx'
   !-----------------------------------------------------------------------------

   slist => sectional_aerosol_list(0)

   if (bin_idx < 1  .or.  bin_idx > slist%nbins) then
      call endrun(subname//': bin list index out of range: bin_idx = '// &
           to_str(bin_idx)//', nbins = '//to_str(slist%nbins))
   end if

   b_idx = slist%idx(bin_idx)

   source = bins%comps(b_idx)%source_num_a
   if (source /= 'A') then
      call endrun(subname//': requested bin number index not in constituent array; source = '//source)
   end if

   cnst_idx = bins%comps(b_idx)%idx_num_a

end subroutine rad_cnst_get_bin_num_idx

!================================================================================================

subroutine rad_aer_diag_init(alist)

   ! Add diagnostic fields to the master fieldlist.

   use cam_history,         only: history_add_field
   use cam_history_support, only: horiz_only
   use cam_abortutils,      only: endrun
   use radiative_aerosol_definitions, only: aerlist_t

   type(aerlist_t), intent(inout) :: alist

   integer :: i, naer
   character(len=64) :: name
   character(len=2)  :: list_id
   character(len=4)  :: suffix
   character(len=128) :: long_name
   character(len=32) :: long_name_description
   !-----------------------------------------------------------------------------

   naer = alist%numaerosols
   if (naer == 0) return

   ! Determine whether this is a climate or diagnostic list.
   list_id = alist%list_id
   if (len_trim(list_id) == 0) then
      suffix = '_c'
      long_name_description = ' used in climate calculation'
   else
      suffix = '_d' // list_id
      long_name_description = ' used in diagnostic calculation'
   end if

   do i = 1, naer

      ! construct names for mass per layer diagnostic fields
      name = 'm_' // trim(alist%aer(i)%camname) // trim(suffix)
      alist%aer(i)%mass_name = name
      long_name = trim(alist%aer(i)%camname)//' mass per layer'//long_name_description
      call history_add_field(trim(name), trim(long_name), 'lev', 'A', 'kg/m^2')

      ! construct names for column burden diagnostic fields
      name = 'cb_' // trim(alist%aer(i)%camname) // trim(suffix)
      long_name = trim(alist%aer(i)%camname)//' column burden'//long_name_description
      call history_add_field(trim(name), trim(long_name), horiz_only, 'A', 'kg/m^2')

      ! error check for name length
      if (len_trim(name) > 64) then
         call endrun('rad_aer_diag_init: name longer than 64 characters: '//trim(name))
      end if

   end do

end subroutine rad_aer_diag_init

!================================================================================================

subroutine rad_aer_diag_out(list_idx, constituents, pdeldry, ncol)

   ! Output the mass per layer, and total column burdens for aerosol
   ! constituents in either the climate or diagnostic lists.
   ! Uses CCPP constituents array instead of physics state / pbuf.

   use shr_kind_mod,   only: shr_kind_cl
   use physconst,      only: rga
   use cam_history,    only: history_out_field
   use string_utils,   only: to_str
   use cam_abortutils, only: endrun, check_allocate
   use radiative_aerosol_definitions, only: N_DIAG, aerlist_t, bulk_aerosol_list

   ! Arguments
   integer,                        intent(in) :: list_idx
   real(kind_phys), target,        intent(in) :: constituents(:,:,:)
   real(r8),                       intent(in) :: pdeldry(:,:)
   integer,                        intent(in) :: ncol

   ! Local variables
   integer :: i, naer
   integer :: idx, nlev
   character(len=1)  :: source
   character(len=32) :: name, cbname
   real(r8), allocatable :: mass(:,:)
   real(r8), allocatable :: cb(:)
   real(r8), pointer :: mmr(:,:)
   type(aerlist_t), pointer :: aerlist
   integer :: ierr
   character(len=shr_kind_cl) :: errmsg
   character(len=*), parameter :: subname = 'rad_aer_diag_out'
   !-----------------------------------------------------------------------------

   nlev = size(constituents, 2)

   ! Associate pointer with requested aerosol list
   if (list_idx >= 0 .and. list_idx <= N_DIAG) then
      aerlist => bulk_aerosol_list(list_idx)
   else
      call endrun(subname//': list_idx out of range: list_idx = '//to_str(list_idx))
   end if

   naer = aerlist%numaerosols
   if (naer == 0) return

   allocate(mass(ncol, nlev), stat=ierr, errmsg=errmsg)
   call check_allocate(ierr, subname, 'mass(ncol, nlev)', &
                       file=__FILE__, line=__LINE__, errmsg=errmsg)
   allocate(cb(ncol), stat=ierr, errmsg=errmsg)
   call check_allocate(ierr, subname, 'cb(ncol)', &
                       file=__FILE__, line=__LINE__, errmsg=errmsg)

   do i = 1, naer

      source = aerlist%aer(i)%source
      idx    = aerlist%aer(i)%idx
      name   = aerlist%aer(i)%mass_name
      cbname = 'cb_' // name(3:len_trim(name))

      ! 'Z' sources are identically zero; output zero fields without doing the math
      select case(source)
      case ('A','N')
         mmr => constituents(:,:,idx)
         mass(:ncol,:) = mmr(:ncol,:) * pdeldry(:ncol,:) * rga
         cb(:ncol) = sum(mass(:ncol,:), 2)
      case ('Z')
         mass(:ncol,:) = 0._r8
         cb(:ncol) = 0._r8
      case default
         call endrun(subname//': unrecognized source "'//source//'"; must be "A", "N" or "Z"')
      end select

      call history_out_field(trim(name), mass(:ncol,:))
      call history_out_field(trim(cbname), cb(:ncol))

   end do

   deallocate(mass)
   deallocate(cb)

end subroutine rad_aer_diag_out

!================================================================================================

end module aerosol_mmr_host
