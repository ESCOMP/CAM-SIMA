module stepon

use shr_kind_mod,   only: r8 => SHR_KIND_R8
use dyn_comp,       only: dyn_import_t, dyn_export_t
use physics_types,  only: physics_state, physics_tend
use spmd_utils,     only: iam, mpicom, npes

implicit none
private
save

public stepon_init
public stepon_run1
public stepon_run2
public stepon_run3
public stepon_final

!=========================================================================================
contains
!=========================================================================================

subroutine stepon_init(dyn_in, dyn_out )

   ! arguments
   type (dyn_import_t), intent(in) :: dyn_in  ! Dynamics import container
   type (dyn_export_t), intent(in) :: dyn_out ! Dynamics export container

end subroutine stepon_init

!=========================================================================================

subroutine stepon_run1( dtime_out, phys_state, phys_tend, dyn_in, dyn_out )

   use time_manager,   only: get_step_size
   use cam_abortutils, only: endrun

   ! Dummy arguments
   real(r8),            intent(out)   :: dtime_out   ! Time-step
   type(physics_state), intent(inout) :: phys_state
   type(physics_tend),  intent(inout) :: phys_tend
   type(dyn_import_t),  intent(inout) :: dyn_in  ! Dynamics import container
   type(dyn_export_t),  intent(inout) :: dyn_out ! Dynamics export container
   !----------------------------------------------------------------------------

   dtime_out = get_step_size()

   if (iam < npes) then
      if (dtime_out <= 0)  call endrun('stepon_run1: bad dtime')
   end if

end subroutine stepon_run1

!=========================================================================================

subroutine stepon_run2(phys_state, phys_tend, dyn_in, dyn_out)

   ! Dummy arguments
   type(physics_state), intent(inout) :: phys_state
   type(physics_tend),  intent(inout) :: phys_tend
   type(dyn_import_t),  intent(inout) :: dyn_in  ! Dynamics import container
   type(dyn_export_t),  intent(inout) :: dyn_out ! Dynamics export container

end subroutine stepon_run2

!=========================================================================================

subroutine stepon_run3(dtime, cam_out, phys_state, dyn_in, dyn_out)

   use camsrfexch,     only: cam_out_t
   use dyn_comp,       only: dyn_run
   use perf_mod,       only: t_startf, t_stopf, t_barrierf

   ! Dummy arguments
   real(r8),            intent(in)    :: dtime   ! Time-step
   type(cam_out_t),     intent(inout) :: cam_out ! Output from CAM to surface
   type(physics_state), intent(inout) :: phys_state
   type (dyn_import_t), intent(inout) :: dyn_in  ! Dynamics import container
   type (dyn_export_t), intent(inout) :: dyn_out ! Dynamics export container

   !--------------------------------------------------------------------------------------

   call t_barrierf('sync_dyn_run', mpicom)
   call t_startf('dyn_run')
   call dyn_run(dyn_out)
   call t_stopf('dyn_run')

end subroutine stepon_run3

!=========================================================================================

subroutine stepon_final(dyn_in, dyn_out)

   ! Dummy arguments
   type (dyn_import_t), intent(inout) :: dyn_in  ! Dynamics import container
   type (dyn_export_t), intent(inout) :: dyn_out ! Dynamics export container

end subroutine stepon_final

!=========================================================================================

end module stepon
