! Copyright (C) 2025 University Corporation for Atmospheric Research (UCAR)
! SPDX-License-Identifier: Apache-2.0

!> This module, part of the MPAS interface, integrates MPAS dynamical core with CAM-SIMA by
!> implementing the necessary APIs and managing their interaction.
!>
!> It contains the instance of MPAS dynamical core, which is used extensively throughout CAM-SIMA.
!> It provides core functionalities such as the initialization, running, and finalization of MPAS
!> dynamical core.
module dyn_comp
    ! Module(s) from MPAS.
    use dyn_mpas_subdriver, only: kind_dyn_mpas => mpas_dynamical_core_real_kind, mpas_dynamical_core_type

    implicit none

    private
    ! Provide APIs required by CAM-SIMA.
    public :: dyn_import_t
    public :: dyn_export_t
    public :: dyn_readnl
    public :: dyn_init
    public :: dyn_run
    public :: dyn_final

    public :: dyn_debug_print
    public :: kind_dyn_mpas, mpas_dynamical_core

    ! NOTE:
    !> This derived type is not used by MPAS dynamical core. It exists only as a placeholder because CAM-SIMA requires it.
    !> Developers/Maintainers/Users who wish to interact with MPAS dynamical core may do so by using the "instance/object"
    !> below.
    type :: dyn_import_t
    end type dyn_import_t

    ! NOTE:
    !> This derived type is not used by MPAS dynamical core. It exists only as a placeholder because CAM-SIMA requires it.
    !> Developers/Maintainers/Users who wish to interact with MPAS dynamical core may do so by using the "instance/object"
    !> below.
    type :: dyn_export_t
    end type dyn_export_t

    interface
        module subroutine dyn_readnl(namelist_path)
            implicit none
            character(*), intent(in) :: namelist_path
        end subroutine dyn_readnl

        module subroutine dyn_init(cam_runtime_opts, dyn_in, dyn_out)
            use runtime_obj, only: runtime_options
            implicit none
            type(runtime_options), intent(in) :: cam_runtime_opts
            type(dyn_import_t), intent(in) :: dyn_in
            type(dyn_export_t), intent(in) :: dyn_out
        end subroutine dyn_init

        module subroutine dyn_run()
            implicit none
        end subroutine dyn_run

        module subroutine dyn_final()
            implicit none
        end subroutine dyn_final

        module subroutine dyn_debug_print(level, message, printer)
            implicit none
            integer, intent(in) :: level
            character(*), intent(in) :: message
            integer, optional, intent(in) :: printer
        end subroutine dyn_debug_print
    end interface

    !> The "instance/object" of MPAS dynamical core.
    type(mpas_dynamical_core_type) :: mpas_dynamical_core
end module dyn_comp
