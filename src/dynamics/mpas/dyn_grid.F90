! Copyright (C) 2025 University Corporation for Atmospheric Research (UCAR)
! SPDX-License-Identifier: Apache-2.0

!> This module, part of the MPAS interface, integrates MPAS dynamical core with CAM-SIMA by
!> implementing the necessary APIs and managing their interaction.
!>
!> It reads and uses the information from MPAS mesh to initialize various model grids
!> (e.g., dynamics, physics) for CAM-SIMA in terms of dynamics decomposition.
module dyn_grid
    ! Module(s) from CAM-SIMA.
    use cam_grid_support, only: max_hcoordname_len
    use dyn_comp, only: kind_dyn_mpas

    implicit none

    private
    ! Provide APIs required by CAM-SIMA.
    public :: model_grid_init

    public :: dyn_grid_id
    public :: dyn_grid_name
    public :: dyn_inquire_mesh_dimensions
    public :: ncells, ncells_solve, nedges, nedges_solve, nvertices, nvertices_solve, nvertlevels
    public :: ncells_global, nedges_global, nvertices_global, ncells_max, nedges_max
    public :: sphere_radius

    interface
        module subroutine model_grid_init()
            implicit none
        end subroutine model_grid_init

        module subroutine dyn_inquire_mesh_dimensions()
            implicit none
        end subroutine dyn_inquire_mesh_dimensions

        module pure function dyn_grid_id(name)
            implicit none
            character(*), intent(in) :: name
            integer :: dyn_grid_id
        end function dyn_grid_id
    end interface

    ! Grid names that are to be registered with CAM-SIMA by calling `cam_grid_register`.
    ! Grid ids can be determined by calling `dyn_grid_id`.
    character(*), parameter :: dyn_grid_name(*) = [character(max_hcoordname_len) :: &
        'mpas_cell',  &
        'cam_cell',   &
        'mpas_edge',  &
        'mpas_vertex' &
    ]

    ! Local and global mesh dimensions of MPAS dynamical core.
    ! Protected module variables that can only be initialized by `dyn_inquire_mesh_dimensions`.
    integer, protected :: ncells, ncells_solve, nedges, nedges_solve, nvertices, nvertices_solve, nvertlevels
    integer, protected :: ncells_global, nedges_global, nvertices_global, ncells_max, nedges_max
    real(kind_dyn_mpas), protected :: sphere_radius
end module dyn_grid
