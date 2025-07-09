! Copyright (C) 2025 University Corporation for Atmospheric Research (UCAR)
! SPDX-License-Identifier: Apache-2.0

!> This module, part of the MPAS interface, integrates MPAS dynamical core with CAM-SIMA by
!> implementing the necessary APIs and managing their interaction.
!>
!> It contains the instance of MPAS dynamical core, which is used extensively throughout CAM-SIMA.
!> It provides core functionalities such as the initialization, running, and finalization of MPAS
!> dynamical core. Various utility procedures for debug printing, exchanging constituent states,
!> inquiring mesh dimensions, etc. are also provided here.
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
    public :: dyn_exchange_constituent_states
    public :: dyn_inquire_mesh_dimensions
    public :: reverse
    public :: mpas_dynamical_core
    public :: ncells, ncells_solve, nedges, nedges_solve, nvertices, nvertices_solve, nvertlevels
    public :: ncells_global, nedges_global, nvertices_global, ncells_max, nedges_max
    public :: sphere_radius

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
            character(*), intent(in) :: namelist_path
        end subroutine dyn_readnl

        module subroutine dyn_init(cam_runtime_opts, dyn_in, dyn_out)
            use runtime_obj, only: runtime_options

            type(runtime_options), intent(in) :: cam_runtime_opts
            type(dyn_import_t), intent(in) :: dyn_in
            type(dyn_export_t), intent(in) :: dyn_out
        end subroutine dyn_init

        module subroutine dyn_run()
        end subroutine dyn_run

        module subroutine dyn_final()
        end subroutine dyn_final

        module subroutine dyn_debug_print(level, message, printer)
            integer, intent(in) :: level
            character(*), intent(in) :: message
            integer, optional, intent(in) :: printer
        end subroutine dyn_debug_print

        module subroutine dyn_exchange_constituent_states(direction, exchange, conversion)
            character(*), intent(in) :: direction
            logical, intent(in) :: exchange
            logical, intent(in) :: conversion
        end subroutine dyn_exchange_constituent_states

        module subroutine dyn_inquire_mesh_dimensions()
        end subroutine dyn_inquire_mesh_dimensions

        module pure function reverse(array)
            use shr_kind_mod, only: kind_r8 => shr_kind_r8

            real(kind_r8), intent(in) :: array(:)
            real(kind_r8) :: reverse(size(array))
        end function reverse
    end interface

    !> The "instance/object" of MPAS dynamical core.
    type(mpas_dynamical_core_type) :: mpas_dynamical_core

    ! Local and global mesh dimensions of MPAS dynamical core.
    ! Protected module variables that can only be initialized by `dyn_inquire_mesh_dimensions`.
    integer, protected :: ncells, ncells_solve, nedges, nedges_solve, nvertices, nvertices_solve, nvertlevels
    integer, protected :: ncells_global, nedges_global, nvertices_global, ncells_max, nedges_max
    real(kind_dyn_mpas), protected :: sphere_radius
contains
end module dyn_comp
