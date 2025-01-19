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

    implicit none

    private
    ! Provide APIs required by CAM-SIMA.
    public :: model_grid_init

    public :: dyn_grid_id
    public :: dyn_grid_name

    ! Grid names that are to be registered with CAM-SIMA by calling `cam_grid_register`.
    ! Grid ids can be determined by calling `dyn_grid_id`.
    character(*), parameter :: dyn_grid_name(*) = [ character(max_hcoordname_len) :: &
        'mpas_cell',  &
        'cam_cell',   &
        'mpas_edge',  &
        'mpas_vertex' &
    ]
contains
    !> Initialize various model grids (e.g., dynamics, physics) in terms of dynamics decomposition.
    !> Additionally, MPAS framework initialization and reading time-invariant (e.g., grid/mesh) variables
    !> are also being completed in this subroutine.
    !> (KCW, 2024-03-29)
    !
    ! Called by `cam_init` in `src/control/cam_comp.F90`.
    subroutine model_grid_init()
        ! Module(s) from CAM-SIMA.
        use cam_abortutils, only: endrun
        use cam_constituents, only: num_advected
        use cam_initfiles, only: initial_file_get_id
        use cam_logfile, only: debugout_debug, debugout_info, debugout_verbose
        use dyn_comp, only: dyn_debug_print, dyn_inquire_mesh_dimensions, mpas_dynamical_core, nvertlevels
        use dynconst, only: dynconst_init
        use string_utils, only: stringify
        use vert_coord, only: pver, vert_coord_init
        ! Module(s) from external libraries.
        use pio, only: file_desc_t

        character(*), parameter :: subname = 'dyn_grid::model_grid_init'
        type(file_desc_t), pointer :: pio_file

        call dyn_debug_print(debugout_debug, subname // ' entered')

        nullify(pio_file)

        ! Initialize mathematical and physical constants for dynamics.
        call dynconst_init()

        call dyn_debug_print(debugout_info, 'Initializing vertical coordinate indexes')

        ! Initialize vertical coordinate indexes.
        ! `pver` comes from CAM-SIMA namelist.
        ! `pverp` is set only after this call. Call it as soon as possible.
        call vert_coord_init(1, pver)

        ! Get PIO file descriptor for initial file.
        pio_file => initial_file_get_id()

        call dyn_debug_print(debugout_info, 'Initializing MPAS dynamical core (Phase 3/4)')

        ! Finish MPAS framework initialization, including
        ! the allocation of all blocks and fields managed by MPAS.
        call mpas_dynamical_core % init_phase3(num_advected, pio_file)

        call dyn_debug_print(debugout_info, 'Initializing MPAS mesh variables')

        ! Read time-invariant (e.g., grid/mesh) variables.
        call mpas_dynamical_core % read_write_stream(pio_file, 'r', 'invariant')

        nullify(pio_file)

        ! Compute local east, north and edge-normal unit vectors whenever time-invariant (e.g., grid/mesh) variables are read.
        call mpas_dynamical_core % compute_unit_vector()

        ! Inquire local and global mesh dimensions.
        call dyn_inquire_mesh_dimensions()

        ! Check for consistency in numbers of vertical layers.
        if (nvertlevels /= pver) then
            call dyn_debug_print(debugout_verbose, 'Number of vertical layers in CAM-SIMA namelist, pver = ' // &
                stringify([pver]))
            call dyn_debug_print(debugout_verbose, 'Number of vertical layers in initial file, nvertlevels = ' // &
                stringify([nvertlevels]))

            call endrun('Numbers of vertical layers mismatch', subname, __LINE__)
        end if

        call dyn_debug_print(debugout_info, 'Initializing reference pressure')

        ! Initialize reference pressure for use by physics.
        call init_reference_pressure()

        call dyn_debug_print(debugout_info, 'Initializing physics grid')

        ! Initialize physics grid.
        call init_physics_grid()

        call dyn_debug_print(debugout_info, 'Registering dynamics grid with CAM-SIMA')

        ! Register dynamics grid with CAM-SIMA.
        call define_cam_grid()

        call dyn_debug_print(debugout_debug, subname // ' completed')
    end subroutine model_grid_init

    !> Initialize reference pressure for use by physics.
    !> (KCW, 2024-03-25)
    subroutine init_reference_pressure()
        ! Module(s) from CAM-SIMA.
        use cam_abortutils, only: check_allocate
        use cam_history_support, only: add_vert_coord
        use cam_logfile, only: debugout_debug, debugout_verbose
        use dyn_comp, only: dyn_debug_print, mpas_dynamical_core
        use dynconst, only: constant_p0 => pref
        use ref_pres, only: ref_pres_init
        use std_atm_profile, only: std_atm_pres
        use string_utils, only: stringify
        use vert_coord, only: pver, pverp
        ! Module(s) from CESM Share.
        use shr_kind_mod, only: kind_r8 => shr_kind_r8
        ! Module(s) from MPAS.
        use dyn_mpas_subdriver, only: kind_dyn_mpas => mpas_dynamical_core_real_kind

        character(*), parameter :: subname = 'dyn_grid::init_reference_pressure'
        ! Number of pure pressure levels at model top.
        integer, parameter :: num_pure_p_lev = 0
        integer :: ierr
        integer :: k, l
        ! Reference pressure (Pa) at layer interfaces and midpoints.
        real(kind_r8), allocatable :: p_ref_int(:)
        real(kind_r8), allocatable :: p_ref_mid(:)
        ! In MPAS, the vertical coordinate is denoted as lowercase "zeta", a Greek alphabet.
        ! `zu` denotes zeta at u-wind levels (i.e., at layer midpoints).
        ! `zw` denotes zeta at w-wind levels (i.e., at layer interfaces).
        ! `dzw` denotes the delta/difference between `zw`.
        ! `rdzw` denotes the reciprocal of `dzw`.
        real(kind_r8), allocatable :: dzw(:)
        real(kind_dyn_mpas), pointer :: rdzw(:)
        real(kind_r8), pointer :: zu(:) ! CANNOT be safely deallocated because `add_vert_coord`
                                        ! just uses pointers to point at it internally.
        real(kind_r8), pointer :: zw(:) ! CANNOT be safely deallocated because `add_vert_coord`
                                        ! just uses pointers to point at it internally.

        call dyn_debug_print(debugout_debug, subname // ' entered')

        nullify(rdzw)
        nullify(zu)
        nullify(zw)

        ! Compute reference height.
        call mpas_dynamical_core % get_variable_pointer(rdzw, 'mesh', 'rdzw')

        allocate(dzw(pver), stat=ierr)
        call check_allocate(ierr, subname, 'dzw(pver)', 'dyn_grid', __LINE__)

        dzw(:) = 1.0_kind_r8 / real(rdzw(:), kind_r8)

        nullify(rdzw)

        allocate(zw(pverp), stat=ierr)
        call check_allocate(ierr, subname, 'zw(pverp)', 'dyn_grid', __LINE__)
        allocate(zu(pver), stat=ierr)
        call check_allocate(ierr, subname, 'zu(pver)', 'dyn_grid', __LINE__)

        ! In MPAS, zeta coordinates are stored in increasing order (i.e., bottom to top of atmosphere).
        ! In CAM-SIMA, however, index order is reversed (i.e., top to bottom of atmosphere).
        ! Compute in reverse below.
        zw(pverp) = 0.0_kind_r8

        do k = pver, 1, -1
            zw(k) = zw(k + 1) + dzw(pver - k + 1)
            zu(k) = 0.5_kind_r8 * (zw(k + 1) + zw(k))
        end do

        deallocate(dzw)

        ! Register zeta coordinates with history.
        call add_vert_coord('ilev', pverp, 'Height (zeta) level at layer interfaces', 'm', zw, &
            positive='up')
        call add_vert_coord('lev', pver, 'Height (zeta) level at layer midpoints', 'm', zu, &
            positive='up')

        ! Compute reference pressure from reference height.
        allocate(p_ref_int(pverp), stat=ierr)
        call check_allocate(ierr, subname, 'p_ref_int(pverp)', 'dyn_grid', __LINE__)

        call std_atm_pres(zw, p_ref_int, user_specified_ps=constant_p0)

        allocate(p_ref_mid(pver), stat=ierr)
        call check_allocate(ierr, subname, 'p_ref_mid(pver)', 'dyn_grid', __LINE__)

        p_ref_mid(:) = 0.5_kind_r8 * (p_ref_int(1:pver) + p_ref_int(2:pverp))

        ! Print a nice table of reference height and pressure.
        call dyn_debug_print(debugout_verbose, 'Reference layer information:')
        call dyn_debug_print(debugout_verbose, '----- | -------------- | --------------')
        call dyn_debug_print(debugout_verbose, 'Index |     Height (m) | Pressure (hPa)')

        do k = 1, pver
            call dyn_debug_print(debugout_verbose, repeat('-', 5) // ' |  ' // stringify([zw(k)]) // &
                ' |  ' // stringify([p_ref_int(k) / 100.0_kind_r8]))

            l = len(stringify([k]))

            call dyn_debug_print(debugout_verbose, repeat(' ', 5 - l) // stringify([k]) // ' |  ' // stringify([zu(k)]) // &
                ' |  ' // stringify([p_ref_mid(k) / 100.0_kind_r8]))
        end do

        k = pverp

        call dyn_debug_print(debugout_verbose, repeat('-', 5) // ' |  ' // stringify([zw(k)]) // &
            ' |  ' // stringify([p_ref_int(k) / 100.0_kind_r8]))

        call ref_pres_init(p_ref_int, p_ref_mid, num_pure_p_lev)

        deallocate(p_ref_int)
        deallocate(p_ref_mid)

        nullify(zu)
        nullify(zw)

        call dyn_debug_print(debugout_debug, subname // ' completed')
    end subroutine init_reference_pressure

    !> Initialize physics grid in terms of dynamics decomposition.
    !> Provide grid and mapping information between global and local indexes to physics by calling `phys_grid_init`.
    !> (KCW, 2024-03-27)
    subroutine init_physics_grid()
        ! Module(s) from CAM-SIMA.
        use cam_abortutils, only: check_allocate
        use cam_logfile, only: debugout_debug
        use dyn_comp, only: dyn_debug_print, mpas_dynamical_core, ncells_global, ncells_solve, sphere_radius
        use dynconst, only: constant_pi => pi, rad_to_deg
        use physics_column_type, only: kind_pcol, physics_column_t
        use physics_grid, only: phys_grid_init
        use spmd_utils, only: iam
        use string_utils, only: stringify
        ! Module(s) from CESM Share.
        use shr_kind_mod, only: kind_r8 => shr_kind_r8
        ! Module(s) from MPAS.
        use dyn_mpas_subdriver, only: kind_dyn_mpas => mpas_dynamical_core_real_kind

        character(*), parameter :: subname = 'dyn_grid::init_physics_grid'
        character(max_hcoordname_len), allocatable :: dyn_attribute_name(:)
        integer :: hdim1_d, hdim2_d ! First and second horizontal dimensions of physics grid.
        integer :: i
        integer :: ierr
        integer, pointer :: indextocellid(:)        ! Global indexes of cell centers.
        real(kind_dyn_mpas), pointer :: areacell(:) ! Cell areas (m2).
        real(kind_dyn_mpas), pointer :: latcell(:)  ! Cell center latitudes (rad).
        real(kind_dyn_mpas), pointer :: loncell(:)  ! Cell center longitudes (rad).
        type(physics_column_t), allocatable :: dyn_column(:) ! Grid and mapping information between global and local indexes.

        call dyn_debug_print(debugout_debug, subname // ' entered')

        nullify(areacell)
        nullify(indextocellid)
        nullify(latcell)
        nullify(loncell)

        hdim1_d = ncells_global

        ! Setting `hdim2_d` to `1` indicates unstructured grid.
        hdim2_d = 1

        call mpas_dynamical_core % get_variable_pointer(areacell, 'mesh', 'areaCell')
        call mpas_dynamical_core % get_variable_pointer(indextocellid, 'mesh', 'indexToCellID')
        call mpas_dynamical_core % get_variable_pointer(latcell, 'mesh', 'latCell')
        call mpas_dynamical_core % get_variable_pointer(loncell, 'mesh', 'lonCell')

        allocate(dyn_column(ncells_solve), stat=ierr)
        call check_allocate(ierr, subname, 'dyn_column(ncells_solve)', 'dyn_grid', __LINE__)

        do i = 1, ncells_solve
            ! Column information.
            dyn_column(i) % lat_rad = real(latcell(i), kind_pcol)
            dyn_column(i) % lon_rad = real(loncell(i), kind_pcol)
            dyn_column(i) % lat_deg = real(real(latcell(i), kind_r8) * rad_to_deg, kind_pcol)
            dyn_column(i) % lon_deg = real(real(loncell(i), kind_r8) * rad_to_deg, kind_pcol)
            ! Cell areas normalized to unit sphere.
            dyn_column(i) % area    = real(areacell(i) / (sphere_radius ** 2), kind_pcol)
            ! Cell weights normalized to unity.
            dyn_column(i) % weight  = &
                real(real(areacell(i), kind_r8) / (4.0_kind_r8 * constant_pi * real(sphere_radius, kind_r8) ** 2), kind_pcol)

            ! File decomposition.
            ! For unstructured grid, `coord_indices` is not used by `phys_grid_init`.
            dyn_column(i) % coord_indices(:) = -1
            ! In this case, `global_col_num` is used instead.
            dyn_column(i) % global_col_num   = indextocellid(i)

            ! Dynamics decomposition.
            dyn_column(i) % dyn_task         = iam
            dyn_column(i) % global_dyn_block = indextocellid(i)
            dyn_column(i) % local_dyn_block  = i
            ! `dyn_block_index` is not used due to no dynamics block offset, but it still needs to be allocated.
            allocate(dyn_column(i) % dyn_block_index(0), stat=ierr)
            call check_allocate(ierr, subname, 'dyn_column(' // stringify([i]) // ') % dyn_block_index(0)', &
                'dyn_grid', __LINE__)
        end do

        nullify(areacell)
        nullify(indextocellid)
        nullify(latcell)
        nullify(loncell)

        ! `phys_grid_init` expects to receive the `area` attribute from dynamics.
        ! However, do not let it because dynamics grid is different from physics grid.
        allocate(dyn_attribute_name(0), stat=ierr)
        call check_allocate(ierr, subname, 'dyn_attribute_name(0)', 'dyn_grid', __LINE__)

        call phys_grid_init(hdim1_d, hdim2_d, 'mpas', dyn_column, 'mpas_cell', dyn_attribute_name)

        deallocate(dyn_column)
        deallocate(dyn_attribute_name)

        call dyn_debug_print(debugout_debug, subname // ' completed')
    end subroutine init_physics_grid

    !> This subroutine defines and registers four variants of dynamics grids in terms of dynamics decomposition.
    !> Their names are listed in `dyn_grid_name` and their corresponding ids can be determined by calling `dyn_grid_id`.
    !> * "mpas_cell": Grid that is centered at MPAS "cell" points.
    !> * "cam_cell": Grid that is also centered at MPAS "cell" points but uses standard CAM-SIMA coordinate and dimension names.
    !> * "mpas_edge": Grid that is centered at MPAS "edge" points.
    !> * "mpas_vertex": Grid that is centered at MPAS "vertex" points.
    !> (KCW, 2024-03-28)
    subroutine define_cam_grid()
        ! Module(s) from CAM-SIMA.
        use cam_abortutils, only: check_allocate
        use cam_grid_support, only: cam_grid_attribute_register, cam_grid_register, &
                                    horiz_coord_create, horiz_coord_t
        use cam_logfile, only: debugout_debug, debugout_verbose
        use cam_map_utils, only: kind_imap => imap
        use dyn_comp, only: dyn_debug_print, mpas_dynamical_core, &
                            ncells_global, nedges_global, nvertices_global, &
                            ncells_solve, nedges_solve, nvertices_solve, &
                            sphere_radius
        use dynconst, only: constant_pi => pi, rad_to_deg
        use string_utils, only: stringify
        ! Module(s) from CESM Share.
        use shr_kind_mod, only: kind_r8 => shr_kind_r8
        ! Module(s) from MPAS.
        use dyn_mpas_subdriver, only: kind_dyn_mpas => mpas_dynamical_core_real_kind

        character(*), parameter :: subname = 'dyn_grid::define_cam_grid'
        integer :: i
        integer :: ierr
        integer, pointer :: indextocellid(:)   ! Global indexes of cell centers.
        integer, pointer :: indextoedgeid(:)   ! Global indexes of edge nodes.
        integer, pointer :: indextovertexid(:) ! Global indexes of vertex nodes.
        real(kind_dyn_mpas), pointer :: areacell(:)  ! Cell areas (m2).
        real(kind_dyn_mpas), pointer :: latcell(:)   ! Cell center latitudes (rad).
        real(kind_dyn_mpas), pointer :: latedge(:)   ! Edge node latitudes (rad).
        real(kind_dyn_mpas), pointer :: latvertex(:) ! Vertex node latitudes (rad).
        real(kind_dyn_mpas), pointer :: loncell(:)   ! Cell center longitudes (rad).
        real(kind_dyn_mpas), pointer :: lonedge(:)   ! Edge node longitudes (rad).
        real(kind_dyn_mpas), pointer :: lonvertex(:) ! Vertex node longitudes (rad).

        ! Global grid indexes. CAN be safely deallocated because its values are copied internally by
        ! `cam_grid_attribute_register` and `horiz_coord_create`.
        ! `kind_imap` is an integer kind of `PIO_OFFSET_KIND`.
        integer(kind_imap), pointer :: global_grid_index(:)
        ! Global grid maps. CANNOT be safely deallocated because `cam_grid_register`
        ! just uses pointers to point at it internally.
        ! `kind_imap` is an integer kind of `PIO_OFFSET_KIND`.
        integer(kind_imap), pointer :: global_grid_map(:, :)
        ! Cell areas (m2). CANNOT be safely deallocated because `cam_grid_attribute_register`
        ! just uses pointers to point at it internally.
        real(kind_r8), pointer :: cell_area(:)
        ! Cell weights normalized to unity. CANNOT be safely deallocated because `cam_grid_attribute_register`
        ! just uses pointers to point at it internally.
        real(kind_r8), pointer :: cell_weight(:)
        ! Latitude coordinates. CANNOT be safely deallocated because `cam_grid_register`
        ! just uses pointers to point at it internally.
        type(horiz_coord_t), pointer :: lat_coord
        ! Longitude coordinates. CANNOT be safely deallocated because `cam_grid_register`
        ! just uses pointers to point at it internally.
        type(horiz_coord_t), pointer :: lon_coord

        call dyn_debug_print(debugout_debug, subname // ' entered')

        nullify(indextocellid, indextoedgeid, indextovertexid)
        nullify(areacell)
        nullify(latcell, loncell)
        nullify(latedge, lonedge)
        nullify(latvertex, lonvertex)

        nullify(global_grid_index, global_grid_map)
        nullify(cell_area, cell_weight)
        nullify(lat_coord, lon_coord)

        ! Construct coordinate and grid objects for cell center grid (i.e., "mpas_cell").
        ! Standard MPAS coordinate and dimension names are used.

        call mpas_dynamical_core % get_variable_pointer(areacell, 'mesh', 'areaCell')
        call mpas_dynamical_core % get_variable_pointer(indextocellid, 'mesh', 'indexToCellID')
        call mpas_dynamical_core % get_variable_pointer(latcell, 'mesh', 'latCell')
        call mpas_dynamical_core % get_variable_pointer(loncell, 'mesh', 'lonCell')

        allocate(global_grid_index(ncells_solve), stat=ierr)
        call check_allocate(ierr, subname, 'global_grid_index(ncells_solve)', 'dyn_grid', __LINE__)

        global_grid_index(:) = int(indextocellid(1:ncells_solve), kind_imap)

        lat_coord => horiz_coord_create('latCell', 'nCells', ncells_global, 'latitude', 'degrees_north', &
            1, ncells_solve, real(latcell, kind_r8) * rad_to_deg, map=global_grid_index)
        lon_coord => horiz_coord_create('lonCell', 'nCells', ncells_global, 'longitude', 'degrees_east', &
            1, ncells_solve, real(loncell, kind_r8) * rad_to_deg, map=global_grid_index)

        allocate(cell_area(ncells_solve), stat=ierr)
        call check_allocate(ierr, subname, 'cell_area(ncells_solve)', 'dyn_grid', __LINE__)
        allocate(cell_weight(ncells_solve), stat=ierr)
        call check_allocate(ierr, subname, 'cell_weight(ncells_solve)', 'dyn_grid', __LINE__)
        allocate(global_grid_map(3, ncells_solve), stat=ierr)
        call check_allocate(ierr, subname, 'global_grid_map(3, ncells_solve)', 'dyn_grid', __LINE__)

        do i = 1, ncells_solve
            cell_area(i)   = real(areacell(i), kind_r8)
            cell_weight(i) = real(areacell(i), kind_r8) / (4.0_kind_r8 * constant_pi * real(sphere_radius, kind_r8) ** 2)

            global_grid_map(1, i) = int(i, kind_imap)
            global_grid_map(2, i) = int(1, kind_imap)
            global_grid_map(3, i) = global_grid_index(i)
        end do

        call dyn_debug_print(debugout_verbose, 'Registering dynamics grid "mpas_cell" with id ' // &
            stringify([dyn_grid_id('mpas_cell')]))

        call cam_grid_register('mpas_cell', dyn_grid_id('mpas_cell'), lat_coord, lon_coord, global_grid_map, &
            unstruct=.true., block_indexed=.false.)
        call cam_grid_attribute_register('mpas_cell', 'cell_area', 'MPAS cell area', 'nCells', cell_area, &
            map=global_grid_index)
        call cam_grid_attribute_register('mpas_cell', 'cell_weight', 'MPAS cell weight', 'nCells', cell_weight, &
            map=global_grid_index)

        nullify(areacell)
        nullify(cell_area, cell_weight)
        nullify(lat_coord, lon_coord)

        ! Construct coordinate and grid objects for cell center grid (i.e., "cam_cell").
        ! Standard CAM-SIMA coordinate and dimension names are used.

        lat_coord => horiz_coord_create('lat', 'ncol', ncells_global, 'latitude', 'degrees_north', &
            1, ncells_solve, real(latcell, kind_r8) * rad_to_deg, map=global_grid_index)
        lon_coord => horiz_coord_create('lon', 'ncol', ncells_global, 'longitude', 'degrees_east', &
            1, ncells_solve, real(loncell, kind_r8) * rad_to_deg, map=global_grid_index)

        call dyn_debug_print(debugout_verbose, 'Registering dynamics grid "cam_cell" with id ' // &
            stringify([dyn_grid_id('cam_cell')]))

        call cam_grid_register('cam_cell', dyn_grid_id('cam_cell'), lat_coord, lon_coord, global_grid_map, &
            unstruct=.true., block_indexed=.false.)

        nullify(indextocellid)
        nullify(latcell, loncell)

        deallocate(global_grid_index)
        nullify(global_grid_index, global_grid_map)
        nullify(lat_coord, lon_coord)

        ! Construct coordinate and grid objects for edge node grid (i.e., "mpas_edge").
        ! Standard MPAS coordinate and dimension names are used.

        call mpas_dynamical_core % get_variable_pointer(indextoedgeid, 'mesh', 'indexToEdgeID')
        call mpas_dynamical_core % get_variable_pointer(latedge, 'mesh', 'latEdge')
        call mpas_dynamical_core % get_variable_pointer(lonedge, 'mesh', 'lonEdge')

        allocate(global_grid_index(nedges_solve), stat=ierr)
        call check_allocate(ierr, subname, 'global_grid_index(nedges_solve)', 'dyn_grid', __LINE__)

        global_grid_index(:) = int(indextoedgeid(1:nedges_solve), kind_imap)

        lat_coord => horiz_coord_create('latEdge', 'nEdges', nedges_global, 'latitude', 'degrees_north', &
            1, nedges_solve, real(latedge, kind_r8) * rad_to_deg, map=global_grid_index)
        lon_coord => horiz_coord_create('lonEdge', 'nEdges', nedges_global, 'longitude', 'degrees_east', &
            1, nedges_solve, real(lonedge, kind_r8) * rad_to_deg, map=global_grid_index)

        allocate(global_grid_map(3, nedges_solve), stat=ierr)
        call check_allocate(ierr, subname, 'global_grid_map(3, nedges_solve)', 'dyn_grid', __LINE__)

        do i = 1, nedges_solve
            global_grid_map(1, i) = int(i, kind_imap)
            global_grid_map(2, i) = int(1, kind_imap)
            global_grid_map(3, i) = global_grid_index(i)
        end do

        call dyn_debug_print(debugout_verbose, 'Registering dynamics grid "mpas_edge" with id ' // &
            stringify([dyn_grid_id('mpas_edge')]))

        call cam_grid_register('mpas_edge', dyn_grid_id('mpas_edge'), lat_coord, lon_coord, global_grid_map, &
            unstruct=.true., block_indexed=.false.)

        nullify(indextoedgeid)
        nullify(latedge, lonedge)

        deallocate(global_grid_index)
        nullify(global_grid_index, global_grid_map)
        nullify(lat_coord, lon_coord)

        ! Construct coordinate and grid objects for vertex node grid (i.e., "mpas_vertex").
        ! Standard MPAS coordinate and dimension names are used.

        call mpas_dynamical_core % get_variable_pointer(indextovertexid, 'mesh', 'indexToVertexID')
        call mpas_dynamical_core % get_variable_pointer(latvertex, 'mesh', 'latVertex')
        call mpas_dynamical_core % get_variable_pointer(lonvertex, 'mesh', 'lonVertex')

        allocate(global_grid_index(nvertices_solve), stat=ierr)
        call check_allocate(ierr, subname, 'global_grid_index(nvertices_solve)', 'dyn_grid', __LINE__)

        global_grid_index(:) = int(indextovertexid(1:nvertices_solve), kind_imap)

        lat_coord => horiz_coord_create('latVertex', 'nVertices', nvertices_global, 'latitude', 'degrees_north', &
            1, nvertices_solve, real(latvertex, kind_r8) * rad_to_deg, map=global_grid_index)
        lon_coord => horiz_coord_create('lonVertex', 'nVertices', nvertices_global, 'longitude', 'degrees_east', &
            1, nvertices_solve, real(lonvertex, kind_r8) * rad_to_deg, map=global_grid_index)

        allocate(global_grid_map(3, nvertices_solve), stat=ierr)
        call check_allocate(ierr, subname, 'global_grid_map(3, nvertices_solve)', 'dyn_grid', __LINE__)

        do i = 1, nvertices_solve
           global_grid_map(1, i) = int(i, kind_imap)
           global_grid_map(2, i) = int(1, kind_imap)
           global_grid_map(3, i) = global_grid_index(i)
        end do

        call dyn_debug_print(debugout_verbose, 'Registering dynamics grid "mpas_vertex" with id ' // &
            stringify([dyn_grid_id('mpas_vertex')]))

        call cam_grid_register('mpas_vertex', dyn_grid_id('mpas_vertex'), lat_coord, lon_coord, global_grid_map, &
            unstruct=.true., block_indexed=.false.)

        nullify(indextovertexid)
        nullify(latvertex, lonvertex)

        deallocate(global_grid_index)
        nullify(global_grid_index, global_grid_map)
        nullify(lat_coord, lon_coord)

        call dyn_debug_print(debugout_debug, subname // ' completed')
    end subroutine define_cam_grid

    !> Helper function for returning grid id given its name.
    !> (KCW, 2024-03-27)
    pure function dyn_grid_id(name)
        ! Module(s) from CAM-SIMA.
        use physics_grid, only: phys_decomp

        character(*), intent(in) :: name
        integer :: dyn_grid_id

        integer :: i

        do i = 1, size(dyn_grid_name)
            if (trim(adjustl(dyn_grid_name(i))) == trim(adjustl(name))) then
                ! Grid ids count from `phys_decomp` + 1.
                ! This avoids id collisions between dynamics and physics grids.
                dyn_grid_id = phys_decomp + i

                return
            end if
        end do

        dyn_grid_id = 0
    end function dyn_grid_id
end module dyn_grid
