module stepon
    use camsrfexch,    only: cam_out_t
    use dyn_comp,      only: dyn_import_t, dyn_export_t
    use physics_types, only: physics_state, physics_tend
    use runtime_obj,   only: runtime_options
    use shr_kind_mod,  only: r8 => shr_kind_r8

    implicit none

    private
    ! Provide APIs required by CAM Control.
    public :: stepon_init
    public :: stepon_timestep_init
    public :: stepon_run2
    public :: stepon_run3
    public :: stepon_final
contains

! Called by `cam_init` in `src/control/cam_comp.F90`.
subroutine stepon_init(cam_runtime_opts, dyn_in, dyn_out)
    type(runtime_options), intent(in) :: cam_runtime_opts
    type(dyn_import_t),    intent(in) :: dyn_in
    type(dyn_export_t),    intent(in) :: dyn_out
end subroutine stepon_init

! Called by `cam_timestep_init` in `src/control/cam_comp.F90`.
subroutine stepon_timestep_init(dtime_phys, cam_runtime_opts, phys_state, phys_tend, dyn_in, dyn_out)
    real(r8),              intent(out)   :: dtime_phys
    type(runtime_options), intent(in)    :: cam_runtime_opts
    type(physics_state),   intent(inout) :: phys_state
    type(physics_tend),    intent(inout) :: phys_tend
    type(dyn_import_t),    intent(inout) :: dyn_in
    type(dyn_export_t),    intent(inout) :: dyn_out
end subroutine stepon_timestep_init

! Called by `cam_run2` in `src/control/cam_comp.F90`.
subroutine stepon_run2(cam_runtime_opts, phys_state, phys_tend, dyn_in, dyn_out)
    type(runtime_options), intent(in)    :: cam_runtime_opts
    type(physics_state),   intent(inout) :: phys_state
    type(physics_tend),    intent(inout) :: phys_tend
    type(dyn_import_t),    intent(inout) :: dyn_in
    type(dyn_export_t),    intent(inout) :: dyn_out
end subroutine stepon_run2

! Called by `cam_run3` in `src/control/cam_comp.F90`.
subroutine stepon_run3(dtime_phys, cam_runtime_opts, cam_out, phys_state, dyn_in, dyn_out)
    real(r8),              intent(in)    :: dtime_phys
    type(runtime_options), intent(in)    :: cam_runtime_opts
    type(cam_out_t),       intent(inout) :: cam_out
    type(physics_state),   intent(inout) :: phys_state
    type(dyn_import_t),    intent(inout) :: dyn_in
    type(dyn_export_t),    intent(inout) :: dyn_out
end subroutine stepon_run3

! Called by `cam_final` in `src/control/cam_comp.F90`.
subroutine stepon_final(cam_runtime_opts, dyn_in, dyn_out)
    type(runtime_options), intent(in)    :: cam_runtime_opts
    type(dyn_import_t),    intent(inout) :: dyn_in
    type(dyn_export_t),    intent(inout) :: dyn_out
end subroutine stepon_final

end module stepon
