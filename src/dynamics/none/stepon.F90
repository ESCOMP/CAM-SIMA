module stepon

   use shr_kind_mod,   only: r8 => SHR_KIND_R8
   use dyn_comp,       only: dyn_import_t, dyn_export_t
   use physics_types,  only: physics_state, physics_tend
   use spmd_utils,     only: iam, mpicom, npes

   implicit none
   private

   public :: stepon_init
   public :: stepon_timestep_init
   public :: stepon_run2
   public :: stepon_run3
   public :: stepon_final

!==============================================================================
CONTAINS
!==============================================================================

   subroutine stepon_init(cam_runtime_opts, dyn_in, dyn_out)
      use runtime_obj, only: runtime_options

      ! Dummy arguments
      type(runtime_options), intent(in) :: cam_runtime_opts
      type(dyn_import_t),    intent(in) :: dyn_in  ! Dynamics import container
      type(dyn_export_t),    intent(in) :: dyn_out ! Dynamics export container

   end subroutine stepon_init

   !===========================================================================

   subroutine stepon_timestep_init(dtime_out, cam_runtime_opts, phys_state,   &
        phys_tend, dyn_in, dyn_out)

      use runtime_obj,    only: runtime_options
      use time_manager,   only: get_step_size
      use cam_abortutils, only: endrun

      ! Dummy arguments
      real(r8),              intent(out)   :: dtime_out  ! Time-step (s)
      type(runtime_options), intent(in)    :: cam_runtime_opts
      type(physics_state),   intent(inout) :: phys_state ! Physics state object
      type(physics_tend),    intent(inout) :: phys_tend  ! Physics tend object
      type(dyn_import_t),    intent(inout) :: dyn_in     ! Dyn import container
      type(dyn_export_t),    intent(inout) :: dyn_out    ! Dyn export container
      !------------------------------------------------------------------------

      !Extract model time step in seconds from ESMF time manager:
      dtime_out = get_step_size()

      !Ensure that the time-step is a positive value:
      if (iam < npes) then
         if (dtime_out <= 0)  call endrun('stepon_timestep_init: bad dtime')
      end if

   end subroutine stepon_timestep_init

   !===========================================================================

   subroutine stepon_run2(cam_runtime_opts, phys_state, phys_tend,            &
        dyn_in, dyn_out)
      use runtime_obj, only: runtime_options

      ! Dummy arguments
      type(runtime_options), intent(in)    :: cam_runtime_opts
      type(physics_state),   intent(inout) :: phys_state ! Physics state object
      type(physics_tend),    intent(inout) :: phys_tend  ! Physics tend object
      type(dyn_import_t),    intent(inout) :: dyn_in     ! Dyn import container
      type(dyn_export_t),    intent(inout) :: dyn_out    ! Dyn export container

   end subroutine stepon_run2

   !===========================================================================

   subroutine stepon_run3(dtime, cam_runtime_opts, cam_out, phys_state,       &
        dyn_in, dyn_out)
      use runtime_obj, only: runtime_options
      use physics_types,  only: cam_out_t
      use dyn_comp,    only: dyn_run
      use perf_mod,    only: t_startf, t_stopf, t_barrierf

      ! Dummy arguments
      real(r8),              intent(in)    :: dtime      ! Time-step
      type(runtime_options), intent(in)    :: cam_runtime_opts
      type(cam_out_t),       intent(inout) :: cam_out    ! From CAM to surface
      type(physics_state),   intent(inout) :: phys_state ! Physics state object
      type(dyn_import_t),    intent(inout) :: dyn_in     ! Dyn import container
      type(dyn_export_t),    intent(inout) :: dyn_out    ! Dyn export container

      !------------------------------------------------------------------------

      ! Syncrhronize all PEs and then run dynamics (dyn_run):
      call t_barrierf('sync_dyn_run', mpicom)
      call t_startf('dyn_run')
      call dyn_run(dyn_out)
      call t_stopf('dyn_run')

   end subroutine stepon_run3

   !===========================================================================

   subroutine stepon_final(cam_runtime_opts, dyn_in, dyn_out)
      use runtime_obj, only: runtime_options

      ! Dummy arguments
      type(runtime_options), intent(in)    :: cam_runtime_opts
      type(dyn_import_t),    intent(inout) :: dyn_in  ! Dyn import container
      type(dyn_export_t),    intent(inout) :: dyn_out ! Dyn export container

   end subroutine stepon_final

   !===========================================================================

end module stepon
