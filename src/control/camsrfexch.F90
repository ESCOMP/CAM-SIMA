module camsrfexch

   !-----------------------------------------------------------------------
   ! Module to handle data that is exchanged between the CAM atmosphere
   ! model and the surface models (land, sea-ice, and ocean).
   !-----------------------------------------------------------------------

   use shr_kind_mod,    only: r8 => shr_kind_r8, r4 => shr_kind_r4
   use constituents,    only: pcnst
   use shr_infnan_mod,  only: posinf => shr_infnan_posinf, assignment(=)
   use cam_abortutils,  only: endrun
   use string_utils,    only: to_str
   use cam_logfile,     only: iulog
   use physics_grid,    only: phys_grid_initialized
   use srf_field_check, only: active_Sl_ram1, active_Sl_fv, active_Sl_soilw
   use srf_field_check, only: active_Fall_flxdst1, active_Fall_flxvoc
   use srf_field_check, only: active_Fall_flxfire, active_Faxa_nhx
   use srf_field_check, only: active_Faxa_noy

   implicit none
   private

   ! Public interfaces
   public atm2hub_alloc ! Atmosphere to surface data allocation
   public hub2atm_alloc ! Merged hub surface to atmosphere data allocation
   public atm2hub_deallocate
   public hub2atm_deallocate
   public cam_export

   ! Public data types
   public cam_out_t     ! Data from atmosphere
   public cam_in_t      ! Merged surface data

   !---------------------------------------------------------------------------
   ! This is the data that is sent from the atmosphere to the surface models
   !---------------------------------------------------------------------------

   type cam_out_t
      integer  :: ncol                ! number of columns in chunk
   end type cam_out_t

   !---------------------------------------------------------------------------
   ! This is the merged state of and flux from sea-ice, land and ocean surfaces
   !---------------------------------------------------------------------------

   type cam_in_t
      integer  :: ncol                    ! number of active columns
   end type cam_in_t

!==============================================================================
CONTAINS
!==============================================================================

   subroutine hub2atm_alloc(cam_in)

      ! Allocate space for the surface to atmosphere data type. And initialize
      ! the values.

      ! ARGUMENTS:
      type(cam_in_t),   pointer   ::  cam_in ! Merged surface state

      ! LOCAL VARIABLES:
      integer                     :: ierror  ! Error code
      character(len=*), parameter :: subname = 'hub2atm_alloc'
      !-----------------------------------------------------------------------

      if ( .not. phys_grid_initialized ) then
         call endrun(subname//": phys_grid_init not called yet")
      end if
      if (associated(cam_in)) then
         deallocate(cam_in)
         nullify(cam_in)
      end if
      allocate(cam_in, stat=ierror)
      if ( ierror /= 0 )then
         call endrun(subname//': allocate cam_in failed with stat: '//&
                     to_str(ierror))
      end if

      cam_in%ncol = 0

   end subroutine hub2atm_alloc

   !===========================================================================

   subroutine atm2hub_alloc(cam_out)

      ! Allocate space for the atmosphere to surface data type. And initialize
      ! the values.

      ! ARGUMENTS:
      type(cam_out_t), pointer :: cam_out    ! Atmosphere to surface input

      ! LOCAL VARIABLES:
      integer :: ierror       ! Error code
      character(len=*), parameter :: subname = 'atm2hub_alloc'
      !-----------------------------------------------------------------------

      if (.not. phys_grid_initialized) then
         call endrun(subname//": phys_grid_init not called yet")
      end if
      if (associated(cam_out)) then
         deallocate(cam_out)
         nullify(cam_out)
      end if
      allocate(cam_out, stat=ierror)
      if ( ierror /= 0 )then
         call endrun(subname//': allocate cam_out failed with stat: '//&
                     to_str(ierror))

      end if

   end subroutine atm2hub_alloc

   !===========================================================================

   subroutine atm2hub_deallocate(cam_out)

      type(cam_out_t), pointer :: cam_out    ! Atmosphere to surface input
      !-----------------------------------------------------------------------

      if(associated(cam_out)) then
         deallocate(cam_out)
      end if
      nullify(cam_out)

   end subroutine atm2hub_deallocate

   !===========================================================================

   subroutine hub2atm_deallocate(cam_in)

      type(cam_in_t), pointer :: cam_in    ! Atmosphere to surface input

      !-----------------------------------------------------------------------
      if(associated(cam_in)) then
         deallocate(cam_in)
      end if
      nullify(cam_in)

   end subroutine hub2atm_deallocate

   !======================================================================

   subroutine cam_export(state, cam_out)

      ! Transfer atmospheric fields into necessary surface data structures

      use physics_types,    only: physics_state
      use vert_coord,       only: pver
      use physconst,        only: rair, mwdry, mwco2, gravit
      use constituents,     only: pcnst

      ! Input arguments
      type(physics_state), intent(in)    :: state
      type (cam_out_t),    intent(inout) :: cam_out

      ! Local variables

      integer :: i              ! Longitude index
      integer :: m              ! constituent index
      integer :: lchnk          ! Chunk index
      integer :: ncol
      integer :: psl_idx
      integer :: prec_dp_idx, snow_dp_idx, prec_sh_idx, snow_sh_idx
      integer :: prec_sed_idx,snow_sed_idx,prec_pcw_idx,snow_pcw_idx

      real(r8), pointer :: psl(:)

      real(r8), pointer :: prec_dp(:)  ! total precip. from deep convection
      real(r8), pointer :: snow_dp(:)  ! snow from deep convection
      real(r8), pointer :: prec_sh(:)  ! total precip. from shallow convection
      real(r8), pointer :: snow_sh(:)  ! snow from shallow convection
      real(r8), pointer :: prec_sed(:) ! total precip. from deep convection
      real(r8), pointer :: snow_sed(:) ! snow from deep convection
      real(r8), pointer :: prec_pcw(:) ! total precipl from shallow convection
      real(r8), pointer :: snow_pcw(:) ! snow from shallos convection
      !-----------------------------------------------------------------------

   end subroutine cam_export

end module camsrfexch
