! Copyright (C) 2025 University Corporation for Atmospheric Research (UCAR)
! SPDX-License-Identifier: Apache-2.0

!> This module, part of the MPAS interface, integrates MPAS dynamical core with CAM-SIMA by
!> implementing the necessary APIs and managing their interaction.
!>
!> It serves as a thin wrapper layer for plugging MPAS dynamical core into CAM-SIMA. Therefore,
!> most of the actual implementations are found elsewhere.
module stepon
    implicit none

    private
    ! Provide APIs required by CAM-SIMA.
    public :: stepon_init
    public :: stepon_timestep_init
    public :: stepon_run2
    public :: stepon_run3
    public :: stepon_final
contains
    ! Called by `cam_init` in `src/control/cam_comp.F90`.
    subroutine stepon_init(cam_runtime_opts, dyn_in, dyn_out)
        ! Module(s) from CAM-SIMA.
        use dyn_comp, only: dyn_export_t, dyn_import_t
        use runtime_obj, only: runtime_options

        type(runtime_options), intent(in) :: cam_runtime_opts
        type(dyn_import_t), intent(in) :: dyn_in
        type(dyn_export_t), intent(in) :: dyn_out
    end subroutine stepon_init

    ! Called by `cam_timestep_init` in `src/control/cam_comp.F90`.
    subroutine stepon_timestep_init(dtime_phys, cam_runtime_opts, phys_state, phys_tend, dyn_in, dyn_out)
        ! Module(s) from CAM-SIMA.
        use dyn_comp, only: dyn_export_t, dyn_import_t
        use dyn_coupling, only: dynamics_to_physics_coupling
        use physics_types, only: physics_state, physics_tend
        use runtime_obj, only: runtime_options
        use time_manager, only: get_step_size
        ! Module(s) from CCPP.
        use ccpp_kinds, only: kind_phys

        real(kind_phys), intent(out) :: dtime_phys
        type(runtime_options), intent(in) :: cam_runtime_opts
        type(physics_state), intent(in) :: phys_state
        type(physics_tend), intent(in) :: phys_tend
        type(dyn_import_t), intent(in) :: dyn_in
        type(dyn_export_t), intent(in) :: dyn_out

        ! Set timestep for physics.
        dtime_phys = real(get_step_size(), kind_phys)

        call dynamics_to_physics_coupling()
    end subroutine stepon_timestep_init

    ! Called by `cam_run2` in `src/control/cam_comp.F90`.
    subroutine stepon_run2(cam_runtime_opts, phys_state, phys_tend, dyn_in, dyn_out)
        ! Module(s) from CAM-SIMA.
        use dyn_comp, only: dyn_export_t, dyn_import_t
        use dyn_coupling, only: physics_to_dynamics_coupling
        use physics_types, only: physics_state, physics_tend
        use runtime_obj, only: runtime_options

        type(runtime_options), intent(in) :: cam_runtime_opts
        type(physics_state), intent(in) :: phys_state
        type(physics_tend), intent(in) :: phys_tend
        type(dyn_import_t), intent(in) :: dyn_in
        type(dyn_export_t), intent(in) :: dyn_out

        call physics_to_dynamics_coupling()
    end subroutine stepon_run2

    ! Called by `cam_run3` in `src/control/cam_comp.F90`.
    subroutine stepon_run3(dtime_phys, cam_runtime_opts, cam_out, phys_state, dyn_in, dyn_out)
        ! Module(s) from CAM-SIMA.
        use physics_types, only: cam_out_t
        use dyn_comp, only: dyn_export_t, dyn_import_t, dyn_run
        use physics_types, only: physics_state
        use runtime_obj, only: runtime_options
        ! Module(s) from CCPP.
        use ccpp_kinds, only: kind_phys

        real(kind_phys), intent(in) :: dtime_phys
        type(runtime_options), intent(in) :: cam_runtime_opts
        type(cam_out_t), intent(in) :: cam_out
        type(physics_state), intent(in) :: phys_state
        type(dyn_import_t), intent(in) :: dyn_in
        type(dyn_export_t), intent(in) :: dyn_out

        call dyn_run()
    end subroutine stepon_run3

    ! Called by `cam_final` in `src/control/cam_comp.F90`.
    subroutine stepon_final(cam_runtime_opts, dyn_in, dyn_out)
        ! Module(s) from CAM-SIMA.
        use dyn_comp, only: dyn_export_t, dyn_import_t, dyn_final
        use runtime_obj, only: runtime_options

        type(runtime_options), intent(in) :: cam_runtime_opts
        type(dyn_import_t), intent(in) :: dyn_in
        type(dyn_export_t), intent(in) :: dyn_out

        call dyn_final()
    end subroutine stepon_final
end module stepon
