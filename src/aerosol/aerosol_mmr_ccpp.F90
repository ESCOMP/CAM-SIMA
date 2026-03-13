module aerosol_mmr_ccpp

!------------------------------------------------------------------------------------------------
!
! CAM-SIMA (CCPP) aerosol MMR retrieval routines.  These routines use
! the CCPP constituents array to return mixing ratio pointers.
!
! Ported from aerosol_mmr_cam — replaces pbuf/state%q with CCPP constituents.
!
!------------------------------------------------------------------------------------------------

use shr_kind_mod,   only: r8 => shr_kind_r8
use ccpp_kinds,     only: kind_phys

implicit none
private
save

! define generic interface for MMR retrieval
interface rad_cnst_get_aer_mmr
   module procedure rad_cnst_get_aer_mmr_by_idx
   module procedure rad_cnst_get_mam_mmr_by_idx
end interface

! values for constituents with requested value of zero
real(r8), allocatable, target :: zero_cols(:,:)

public :: aerosol_mmr_init    ! allocate zero_cols
public :: get_ccpp_idx
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
   use physics_grid, only: columns_on_task
   use vert_coord,   only: pver
   ! Allocate zero_cols array (must be called after grid/vert is set up)
   if (.not. allocated(zero_cols)) then
      allocate(zero_cols(columns_on_task, pver))
      zero_cols = 0._r8
   end if
end subroutine aerosol_mmr_init

!================================================================================================

integer function get_ccpp_idx(source, name, routine)

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
      call endrun(routine//' ERROR: invalid source for specie '//trim(name))
   end if

   get_ccpp_idx = idx

end function get_ccpp_idx

!===========================

subroutine resolve_mode_idx(modes)

   ! Initialize the mode definitions by looking up the relevant indices in the
   ! CCPP constituents array, and getting the physprop IDs

   use aerosol_physical_properties,      only: physprop_get_id
   use cam_abortutils, only: endrun
   use radiative_aerosol_definitions, only: modes_t

   ! Arguments
   type(modes_t), intent(inout) :: modes

   ! Local variables
   integer :: m, ispec, nspec

   character(len=*), parameter :: routine = 'resolve_mode_idx'
   !-----------------------------------------------------------------------------

   do m = 1, modes%nmodes

      ! indices for number mixing ratio components
      modes%comps(m)%idx_num_a = get_ccpp_idx(modes%comps(m)%source_num_a, modes%comps(m)%camname_num_a, routine)
      modes%comps(m)%idx_num_c = get_ccpp_idx(modes%comps(m)%source_num_c, modes%comps(m)%camname_num_c, routine)

      ! allocate memory for species
      nspec = modes%comps(m)%nspec
      allocate( &
         modes%comps(m)%idx_mmr_a(nspec), &
         modes%comps(m)%idx_mmr_c(nspec), &
         modes%comps(m)%idx_props(nspec)  )

      do ispec = 1, nspec

         ! indices for species mixing ratio components
         modes%comps(m)%idx_mmr_a(ispec) = get_ccpp_idx(modes%comps(m)%source_mmr_a(ispec), &
                                                   modes%comps(m)%camname_mmr_a(ispec), routine)
         modes%comps(m)%idx_mmr_c(ispec) = get_ccpp_idx(modes%comps(m)%source_mmr_c(ispec), &
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

   use aerosol_physical_properties,      only: physprop_get_id
   use cam_abortutils, only: endrun
   use radiative_aerosol_definitions, only: bins_t

   ! Arguments
   type(bins_t), intent(inout) :: bins

   ! Local variables
   integer :: m, ispec, nspec

   character(len=*), parameter :: routine = 'resolve_bin_idx'
   !-----------------------------------------------------------------------------

   do m = 1, bins%nbins

      ! indices for number mixing ratio components
      bins%comps(m)%idx_num_a = get_ccpp_idx(bins%comps(m)%source_num_a, bins%comps(m)%camname_num_a, routine)
      bins%comps(m)%idx_num_c = get_ccpp_idx(bins%comps(m)%source_num_c, bins%comps(m)%camname_num_c, routine)
      if ( bins%comps(m)%source_mass_a /= 'NOTSET' .and. bins%comps(m)%camname_mass_a /= 'NOTSET' ) then
         bins%comps(m)%idx_mass_a = get_ccpp_idx(bins%comps(m)%source_mass_a, bins%comps(m)%camname_mass_a, routine)
      endif
      if ( bins%comps(m)%source_mass_c /= 'NOTSET' .and. bins%comps(m)%camname_mass_c /= 'NOTSET' ) then
         bins%comps(m)%idx_mass_c = get_ccpp_idx(bins%comps(m)%source_mass_c, bins%comps(m)%camname_mass_c, routine)
      endif

      ! allocate memory for species
      nspec = bins%comps(m)%nspec
      allocate( &
         bins%comps(m)%idx_mmr_a(nspec), &
         bins%comps(m)%idx_mmr_c(nspec), &
         bins%comps(m)%idx_props(nspec)  )

      do ispec = 1, nspec

         ! indices for species mixing ratio components
         bins%comps(m)%idx_mmr_a(ispec) = get_ccpp_idx(bins%comps(m)%source_mmr_a(ispec), &
                                                   bins%comps(m)%camname_mmr_a(ispec), routine)
         bins%comps(m)%idx_mmr_c(ispec) = get_ccpp_idx(bins%comps(m)%source_mmr_c(ispec), &
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
      aerlist%aer(i)%idx = get_ccpp_idx(aerlist%aer(i)%source, aerlist%aer(i)%camname, routine)
   end do

end subroutine resolve_bulk_idx

!================================================================================================

subroutine rad_cnst_get_aer_mmr_by_idx(list_idx, aer_idx, constituents, mmr)

   ! Return pointer to mass mixing ratio for the bulk aerosol from the specified
   ! climate or diagnostic list, using the CCPP constituents array.

   use cam_logfile,    only: iulog
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
      write(iulog,*) subname//': list_idx =', list_idx
      call endrun(subname//': list_idx out of bounds')
   endif

   ! Check for valid input aerosol index
   if (aer_idx < 1  .or.  aer_idx > aerlist%numaerosols) then
      write(iulog,*) subname//': aer_idx= ', aer_idx, '  numaerosols= ', aerlist%numaerosols
      call endrun(subname//': aerosol list index out of range')
   end if

   ! Get data source
   source = aerlist%aer(aer_idx)%source
   idx    = aerlist%aer(aer_idx)%idx
   select case( source )
   case ('A','N')
      mmr => constituents(:,:,idx)
   case ('Z')
      mmr => zero_cols
   end select

end subroutine rad_cnst_get_aer_mmr_by_idx

!================================================================================================

subroutine rad_cnst_get_mam_mmr_by_idx(list_idx, mode_idx, spec_idx, phase, constituents, mmr)

   ! Return pointer to mass mixing ratio for the modal aerosol specie from the specified
   ! climate or diagnostic list, using the CCPP constituents array.

   use cam_logfile,    only: iulog
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
      write(iulog,*) subname//': list_idx =', list_idx
      call endrun(subname//': list_idx out of bounds')
   endif

   ! Check for valid mode index
   if (mode_idx < 1  .or.  mode_idx > mlist%nmodes) then
      write(iulog,*) subname//': mode_idx= ', mode_idx, '  nmodes= ', mlist%nmodes
      call endrun(subname//': mode list index out of range')
   end if

   ! Get the index for the corresponding mode in the mode definition object
   m_idx = mlist%idx(mode_idx)

   ! Check for valid specie index
   if (spec_idx < 1  .or.  spec_idx > modes%comps(m_idx)%nspec) then
      write(iulog,*) subname//': spec_idx= ', spec_idx, '  nspec= ', modes%comps(m_idx)%nspec
      call endrun(subname//': specie list index out of range')
   end if

   ! Get data source
   if (phase == 'a') then
      source = modes%comps(m_idx)%source_mmr_a(spec_idx)
      idx    = modes%comps(m_idx)%idx_mmr_a(spec_idx)
   else if (phase == 'c') then
      source = modes%comps(m_idx)%source_mmr_c(spec_idx)
      idx    = modes%comps(m_idx)%idx_mmr_c(spec_idx)
   else
      write(iulog,*) subname//': phase= ', phase
      call endrun(subname//': unrecognized phase; must be "a" or "c"')
   end if

   select case( source )
   case ('A','N')
      mmr => constituents(:,:,idx)
   case ('Z')
      mmr => zero_cols
   end select

end subroutine rad_cnst_get_mam_mmr_by_idx

!================================================================================================

subroutine rad_cnst_get_bin_mmr_by_idx(list_idx, bin_idx, spec_idx, phase, constituents, mmr)

   ! Return pointer to mass mixing ratio for the sectional aerosol specie.

   use cam_logfile,    only: iulog
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
      write(iulog,*) subname//': list_idx =', list_idx
      call endrun(subname//': list_idx out of bounds')
   endif

   if (bin_idx < 1  .or.  bin_idx > slist%nbins) then
      write(iulog,*) subname//': bin_idx= ', bin_idx, '  nbins= ', slist%nbins
      call endrun(subname//': bin list index out of range')
   end if

   s_idx = slist%idx(bin_idx)

   if (spec_idx < 1  .or.  spec_idx > bins%comps(s_idx)%nspec) then
      write(iulog,*) subname//': spec_idx= ', spec_idx, '  nspec= ', bins%comps(s_idx)%nspec
      call endrun(subname//': specie list index out of range')
   end if

   if (phase == 'a') then
      source = bins%comps(s_idx)%source_mmr_a(spec_idx)
      idx    = bins%comps(s_idx)%idx_mmr_a(spec_idx)
   else if (phase == 'c') then
      source = bins%comps(s_idx)%source_mmr_c(spec_idx)
      idx    = bins%comps(s_idx)%idx_mmr_c(spec_idx)
   else
      write(iulog,*) subname//': phase= ', phase
      call endrun(subname//': unrecognized phase; must be "a" or "c"')
   end if

   select case( source )
   case ('A','N')
      mmr => constituents(:,:,idx)
   case ('Z')
      mmr => zero_cols
   end select

end subroutine rad_cnst_get_bin_mmr_by_idx

!================================================================================================

subroutine rad_cnst_get_mam_mmr_idx(mode_idx, spec_idx, idx)

   ! Return constituent index of mam specie mass mixing ratio for aerosol modes in
   ! the climate list.

   use cam_logfile,    only: iulog
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
      write(iulog,*) subname//': mode_idx= ', mode_idx, '  nmodes= ', mlist%nmodes
      call endrun(subname//': mode list index out of range')
   end if

   m_idx = mlist%idx(mode_idx)

   if (spec_idx < 1  .or.  spec_idx > modes%comps(m_idx)%nspec) then
      write(iulog,*) subname//': spec_idx= ', spec_idx, '  nspec= ', modes%comps(m_idx)%nspec
      call endrun(subname//': specie list index out of range')
   end if

   idx = modes%comps(m_idx)%idx_mmr_a(spec_idx)

end subroutine rad_cnst_get_mam_mmr_idx

!================================================================================================

subroutine rad_cnst_get_carma_mmr_idx(bin_idx, spec_idx, idx)

   use cam_logfile,    only: iulog
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
      write(iulog,*) subname//': bin_idx= ', bin_idx, '  nbins= ', slist%nbins
      call endrun(subname//': bin list index out of range')
   end if

   b_idx = slist%idx(bin_idx)

   if (spec_idx < 1  .or.  spec_idx > bins%comps(b_idx)%nspec) then
      write(iulog,*) subname//': spec_idx= ', spec_idx, '  nspec= ', bins%comps(b_idx)%nspec
      call endrun(subname//': specie list index out of range')
   end if

   idx = bins%comps(b_idx)%idx_mmr_a(spec_idx)

end subroutine rad_cnst_get_carma_mmr_idx

!================================================================================================

subroutine rad_cnst_get_bin_mmr(list_idx, bin_idx, phase, constituents, mmr)

   use cam_logfile,    only: iulog
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
      write(iulog,*) subname//': list_idx =', list_idx
      call endrun(subname//': list_idx out of bounds')
   endif

   if (bin_idx < 1  .or.  bin_idx > slist%nbins) then
      write(iulog,*) subname//': bin_idx= ', bin_idx, '  nbins= ', slist%nbins
      call endrun(subname//': bin list index out of range')
   end if

   m_idx = slist%idx(bin_idx)

   if (phase == 'a') then
      source = bins%comps(m_idx)%source_mass_a
      idx    = bins%comps(m_idx)%idx_mass_a
   else if (phase == 'c') then
      source = bins%comps(m_idx)%source_mass_c
      idx    = bins%comps(m_idx)%idx_mass_c
   else
      write(iulog,*) subname//': phase= ', phase
      call endrun(subname//': unrecognized phase; must be "a" or "c"')
   end if

   select case( source )
   case ('A','N')
      mmr => constituents(:,:,idx)
   case ('Z')
      mmr => zero_cols
   end select

end subroutine rad_cnst_get_bin_mmr

!================================================================================================

subroutine rad_cnst_get_mode_num(list_idx, mode_idx, phase, constituents, num)

   use cam_logfile,    only: iulog
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
      write(iulog,*) subname//': list_idx =', list_idx
      call endrun(subname//': list_idx out of bounds')
   endif

   if (mode_idx < 1  .or.  mode_idx > mlist%nmodes) then
      write(iulog,*) subname//': mode_idx= ', mode_idx, '  nmodes= ', mlist%nmodes
      call endrun(subname//': mode list index out of range')
   end if

   m_idx = mlist%idx(mode_idx)

   if (phase == 'a') then
      source = modes%comps(m_idx)%source_num_a
      idx    = modes%comps(m_idx)%idx_num_a
   else if (phase == 'c') then
      source = modes%comps(m_idx)%source_num_c
      idx    = modes%comps(m_idx)%idx_num_c
   else
      write(iulog,*) subname//': phase= ', phase
      call endrun(subname//': unrecognized phase; must be "a" or "c"')
   end if

   select case( source )
   case ('A','N')
      num => constituents(:,:,idx)
   case ('Z')
      num => zero_cols
   end select

end subroutine rad_cnst_get_mode_num

!================================================================================================

subroutine rad_cnst_get_bin_num(list_idx, bin_idx, phase, constituents, num)

   use cam_logfile,    only: iulog
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
      write(iulog,*) subname//': list_idx =', list_idx
      call endrun(subname//': list_idx out of bounds')
   endif

   if (bin_idx < 1  .or.  bin_idx > slist%nbins) then
      write(iulog,*) subname//': bin_idx= ', bin_idx, '  nbins= ', slist%nbins
      call endrun(subname//': bin list index out of range')
   end if

   m_idx = slist%idx(bin_idx)

   if (phase == 'a') then
      source = bins%comps(m_idx)%source_num_a
      idx    = bins%comps(m_idx)%idx_num_a
   else if (phase == 'c') then
      source = bins%comps(m_idx)%source_num_c
      idx    = bins%comps(m_idx)%idx_num_c
   else
      write(iulog,*) subname//': phase= ', phase
      call endrun(subname//': unrecognized phase; must be "a" or "c"')
   end if

   select case( source )
   case ('A','N')
      num => constituents(:,:,idx)
   case ('Z')
      num => zero_cols
   end select

end subroutine rad_cnst_get_bin_num

!================================================================================================

subroutine rad_cnst_get_mode_num_idx(mode_idx, cnst_idx)

   use cam_logfile,    only: iulog
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
      write(iulog,*) subname//': mode_idx= ', mode_idx, '  nmodes= ', mlist%nmodes
      call endrun(subname//': mode list index out of range')
   end if

   m_idx = mlist%idx(mode_idx)

   source = modes%comps(m_idx)%source_num_a
   if (source /= 'A') then
      write(iulog,*) subname//': source= ', source
      call endrun(subname//': requested mode number index not in constituent array')
   end if

   cnst_idx = modes%comps(m_idx)%idx_num_a

end subroutine rad_cnst_get_mode_num_idx

!================================================================================================

subroutine rad_cnst_get_bin_num_idx(bin_idx, cnst_idx)

   use cam_logfile,    only: iulog
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
      write(iulog,*) subname//': bin_idx= ', bin_idx, '  nbins= ', slist%nbins
      call endrun(subname//': bin list index out of range')
   end if

   b_idx = slist%idx(bin_idx)

   source = bins%comps(b_idx)%source_num_a
   if (source /= 'A') then
      write(iulog,*) subname//': source= ', source
      call endrun(subname//': requested bin number index not in constituent array')
   end if

   cnst_idx = bins%comps(b_idx)%idx_num_a

end subroutine rad_cnst_get_bin_num_idx

!================================================================================================

subroutine rad_aer_diag_init(alist)

   ! Add diagnostic fields to the master fieldlist.

   use cam_history,         only: history_add_field
   use cam_history_support, only: horiz_only
   use cam_logfile,         only: iulog
   use cam_abortutils,      only: endrun
   use radiative_aerosol_definitions, only: aerlist_t

   type(aerlist_t), intent(inout) :: alist

   integer :: i, naer
   character(len=64) :: name
   character(len=2)  :: list_id
   character(len=4)  :: suffix
   character(len=128):: long_name
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
         write(iulog,*) 'rad_aer_diag_init: '//trim(name)//' longer than 64 characters'
         call endrun('rad_aer_diag_init: name too long: '//trim(name))
      end if

   end do

end subroutine rad_aer_diag_init

!================================================================================================

subroutine rad_aer_diag_out(list_idx, constituents, pdeldry, ncol)

   ! Output the mass per layer, and total column burdens for aerosol
   ! constituents in either the climate or diagnostic lists.
   ! Uses CCPP constituents array instead of physics state / pbuf.

   use physconst,      only: rga
   use cam_history,    only: history_out_field
   use cam_logfile,    only: iulog
   use cam_abortutils, only: endrun
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
   character(len=*), parameter :: subname = 'rad_aer_diag_out'
   !-----------------------------------------------------------------------------

   nlev = size(constituents, 2)

   ! Associate pointer with requested aerosol list
   if (list_idx >= 0 .and. list_idx <= N_DIAG) then
      aerlist => bulk_aerosol_list(list_idx)
   else
      write(iulog,*) subname//': list_idx = ', list_idx
      call endrun(subname//': list_idx out of range')
   endif

   naer = aerlist%numaerosols
   if (naer == 0) return

   allocate(mass(ncol, nlev))
   allocate(cb(ncol))

   do i = 1, naer

      source = aerlist%aer(i)%source
      idx    = aerlist%aer(i)%idx
      name   = aerlist%aer(i)%mass_name
      cbname = 'cb_' // name(3:len_trim(name))

      select case( source )
      case ('A','N')
         mmr => constituents(:,:,idx)
      case ('Z')
         mmr => zero_cols
      end select

      mass(:ncol,:) = mmr(:ncol,:) * pdeldry(:ncol,:) * rga
      call history_out_field(trim(name), mass(:ncol,:))

      cb(:ncol) = sum(mass(:ncol,:), 2)
      call history_out_field(trim(cbname), cb(:ncol))

   end do

   deallocate(mass)
   deallocate(cb)

end subroutine rad_aer_diag_out

!================================================================================================

end module aerosol_mmr_ccpp
