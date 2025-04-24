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

    !> The "instance/object" of MPAS dynamical core.
    type(mpas_dynamical_core_type) :: mpas_dynamical_core

    ! Local and global mesh dimensions of MPAS dynamical core.
    ! Protected module variables that can only be initialized by `dyn_inquire_mesh_dimensions`.
    integer, protected :: ncells, ncells_solve, nedges, nedges_solve, nvertices, nvertices_solve, nvertlevels
    integer, protected :: ncells_global, nedges_global, nvertices_global, ncells_max, nedges_max
    real(kind_dyn_mpas), protected :: sphere_radius
contains
    !> Print a debug message at a debug level. The debug message will be prefixed by "MPAS Interface (N): ", where `N`
    !> is the MPI rank. The debug level is one of the `debugout_*` constants from the `cam_logfile` module.
    !> If `printer` is not supplied, the MPI root rank will print. Otherwise, the designated MPI rank will print instead.
    !> (KCW, 2024-02-03)
    subroutine dyn_debug_print(level, message, printer)
        ! Module(s) from CAM-SIMA.
        use cam_logfile, only: debug_output, iulog
        use spmd_utils, only: iam, masterproc
        use string_utils, only: stringify

        integer, intent(in) :: level
        character(*), intent(in) :: message
        integer, optional, intent(in) :: printer

        ! Bail out early if the log level is less verbose than the debug level.
        if (debug_output < level) then
            return
        end if

        if (present(printer)) then
            if (iam /= printer) then
                return
            end if
        else
            if (.not. masterproc) then
                return
            end if
        end if

        write(iulog, '(a)') 'MPAS Interface (' // stringify([iam]) // '): ' // message
    end subroutine dyn_debug_print

    !> Read MPAS namelist from the supplied path.
    !> Additionally, perform early initialization of MPAS dynamical core.
    !> (KCW, 2024-02-09)
    !
    ! Called by `read_namelist` in `src/control/runtime_opts.F90`.
    subroutine dyn_readnl(namelist_path)
        ! Module(s) from CAM-SIMA.
        use cam_abortutils, only: endrun
        use cam_control_mod, only: initial_run
        use cam_instance, only: atm_id
        use cam_logfile, only: debug_output, debugout_debug, debugout_info, iulog
        use spmd_utils, only: mpicom
        use string_utils, only: stringify
        use time_manager, only: get_start_date, get_stop_date, get_run_duration, timemgr_get_calendar_cf
        ! Module(s) from CESM Share.
        use shr_file_mod, only: shr_file_getunit
        use shr_kind_mod, only: len_cs => shr_kind_cs
        use shr_pio_mod, only: shr_pio_getiosys
        ! Module(s) from external libraries.
        use pio, only: iosystem_desc_t

        character(*), intent(in) :: namelist_path

        character(*), parameter :: subname = 'dyn_comp::dyn_readnl'
        character(len_cs) :: cam_calendar
        integer :: log_unit(2)
        integer :: start_date_time(6), & ! YYYY, MM, DD, hh, mm, ss.
                   stop_date_time(6),  & ! YYYY, MM, DD, hh, mm, ss.
                   run_duration(4),    & ! DD, hh, mm, ss.
                   sec_since_midnight    ! Second(s) since midnight.
        type(iosystem_desc_t), pointer :: pio_iosystem

        call dyn_debug_print(debugout_debug, subname // ' entered')

        nullify(pio_iosystem)

        ! Get free units for MPAS so it can write its own log files, e.g., "log.atmosphere.0000.{out,err}".
        log_unit(1) = shr_file_getunit()
        log_unit(2) = shr_file_getunit()

        call dyn_debug_print(debugout_info, 'Initializing MPAS dynamical core (Phase 1/4)')

        ! Initialize MPAS framework with the supplied MPI communicator group, procedure pointer to terminate the model,
        ! log level, and units.
        call mpas_dynamical_core % init_phase1(mpicom, endrun, debug_output, iulog, log_unit)

        cam_calendar = timemgr_get_calendar_cf()

        call get_start_date(start_date_time(1), start_date_time(2), start_date_time(3), sec_since_midnight)
        start_date_time(4:6) = sec_to_hour_min_sec(sec_since_midnight)

        call get_stop_date(stop_date_time(1), stop_date_time(2), stop_date_time(3), sec_since_midnight)
        stop_date_time(4:6) = sec_to_hour_min_sec(sec_since_midnight)

        call get_run_duration(run_duration(1), sec_since_midnight)
        run_duration(2:4) = sec_to_hour_min_sec(sec_since_midnight)

        call dyn_debug_print(debugout_info, 'Reading namelist')

        ! Read MPAS-related namelist variables from `namelist_path`, e.g., "atm_in".
        call mpas_dynamical_core % read_namelist(namelist_path, &
            cam_calendar, start_date_time, stop_date_time, run_duration, initial_run)

        pio_iosystem => shr_pio_getiosys(atm_id)

        call dyn_debug_print(debugout_info, 'Initializing MPAS dynamical core (Phase 2/4)')

        ! Initialize MPAS framework with the supplied PIO system descriptor.
        call mpas_dynamical_core % init_phase2(pio_iosystem)

        nullify(pio_iosystem)

        call dyn_debug_print(debugout_debug, subname // ' completed')
    contains
        !> Convert second(s) to hour(s), minute(s), and second(s).
        !> (KCW, 2024-02-07)
        pure function sec_to_hour_min_sec(sec) result(hour_min_sec)
            integer, intent(in) :: sec
            integer :: hour_min_sec(3)

            ! These are all intended to be integer arithmetics.
            hour_min_sec(1) = sec / 3600
            hour_min_sec(2) = sec / 60 - hour_min_sec(1) * 60
            hour_min_sec(3) = sec - hour_min_sec(1) * 3600 - hour_min_sec(2) * 60
        end function sec_to_hour_min_sec
    end subroutine dyn_readnl

    !> Initialize MPAS dynamical core by one of the following:
    !> 1. Setting analytic initial condition;
    !> 2. Reading initial condition from a file;
    !> 3. Restarting from a file.
    !> (KCW, 2024-05-28)
    !
    ! Called by `cam_init` in `src/control/cam_comp.F90`.
    subroutine dyn_init(cam_runtime_opts, dyn_in, dyn_out)
        ! Module(s) from CAM-SIMA.
        use air_composition, only: thermodynamic_active_species_num, &
                                   thermodynamic_active_species_liq_num, &
                                   thermodynamic_active_species_ice_num, &
                                   thermodynamic_active_species_idx, thermodynamic_active_species_idx_dycore, &
                                   thermodynamic_active_species_liq_idx, thermodynamic_active_species_liq_idx_dycore, &
                                   thermodynamic_active_species_ice_idx, thermodynamic_active_species_ice_idx_dycore
        use cam_abortutils, only: check_allocate
        use cam_constituents, only: const_name, const_is_water_species, num_advected, readtrace
        use cam_control_mod, only: initial_run
        use cam_initfiles, only: initial_file_get_id, topo_file_get_id
        use cam_logfile, only: debugout_debug, debugout_info
        use cam_pio_utils, only: clean_iodesc_list
        use cam_thermo_formula, only: energy_formula_dycore, energy_formula_dycore_mpas
        use inic_analytic, only: analytic_ic_active
        use physics_types, only: dycore_energy_consistency_adjust
        use runtime_obj, only: runtime_options
        use string_utils, only: stringify
        use time_manager, only: get_step_size
        ! Module(s) from CCPP.
        use phys_vars_init_check, only: std_name_len
        ! Module(s) from external libraries.
        use pio, only: file_desc_t

        type(runtime_options), intent(in) :: cam_runtime_opts
        type(dyn_import_t), intent(in) :: dyn_in
        type(dyn_export_t), intent(in) :: dyn_out

        character(*), parameter :: subname = 'dyn_comp::dyn_init'
        character(std_name_len), allocatable :: constituent_name(:)
        integer :: coupling_time_interval
        integer :: i
        integer :: ierr
        logical, allocatable :: is_water_species(:)
        type(file_desc_t), pointer :: pio_init_file
        type(file_desc_t), pointer :: pio_topo_file

        call dyn_debug_print(debugout_debug, subname // ' entered')

        nullify(pio_init_file)
        nullify(pio_topo_file)

        ! Set the energy formula of dynamical core to MPAS for use in `cam_thermo`.
        energy_formula_dycore = energy_formula_dycore_mpas

        ! The total energy of dynamical core, which uses "MPAS formula" as set above, is not consistent with
        ! that of CAM physics, which uses "FV formula". Therefore, temperature and temperature tendency adjustments
        ! are needed at the end of each physics time step.
        dycore_energy_consistency_adjust = .true.

        allocate(constituent_name(num_advected), stat=ierr)
        call check_allocate(ierr, subname, 'constituent_name(num_advected)', 'dyn_comp', __LINE__)

        allocate(is_water_species(num_advected), stat=ierr)
        call check_allocate(ierr, subname, 'is_water_species(num_advected)', 'dyn_comp', __LINE__)

        do i = 1, num_advected
            constituent_name(i) = const_name(i)
            is_water_species(i) = const_is_water_species(i)
        end do

        call dyn_debug_print(debugout_info, 'Defining MPAS scalars and scalar tendencies')

        ! Inform MPAS about constituent names and their corresponding waterness.
        call mpas_dynamical_core % define_scalar(constituent_name, is_water_species)

        deallocate(constituent_name)
        deallocate(is_water_species)

        ! Provide mapping information between MPAS scalars and constituent names to CAM-SIMA.
        do i = 1, thermodynamic_active_species_num
            thermodynamic_active_species_idx_dycore(i) = &
                mpas_dynamical_core % map_mpas_scalar_index(thermodynamic_active_species_idx(i))
        end do

        do i = 1, thermodynamic_active_species_liq_num
            thermodynamic_active_species_liq_idx_dycore(i) = &
                mpas_dynamical_core % map_mpas_scalar_index(thermodynamic_active_species_liq_idx(i))
        end do

        do i = 1, thermodynamic_active_species_ice_num
            thermodynamic_active_species_ice_idx_dycore(i) = &
                mpas_dynamical_core % map_mpas_scalar_index(thermodynamic_active_species_ice_idx(i))
        end do

        call dyn_debug_print(debugout_debug, 'thermodynamic_active_species_num = ' // &
            stringify([thermodynamic_active_species_num]))
        call dyn_debug_print(debugout_debug, 'thermodynamic_active_species_liq_num = ' // &
            stringify([thermodynamic_active_species_liq_num]))
        call dyn_debug_print(debugout_debug, 'thermodynamic_active_species_ice_num = ' // &
            stringify([thermodynamic_active_species_ice_num]))

        call dyn_debug_print(debugout_debug, 'thermodynamic_active_species_idx_dycore = [' // &
            stringify(thermodynamic_active_species_idx_dycore) // ']')
        call dyn_debug_print(debugout_debug, 'thermodynamic_active_species_liq_idx_dycore = [' // &
            stringify(thermodynamic_active_species_liq_idx_dycore) // ']')
        call dyn_debug_print(debugout_debug, 'thermodynamic_active_species_ice_idx_dycore = [' // &
            stringify(thermodynamic_active_species_ice_idx_dycore) // ']')

        pio_init_file => initial_file_get_id()
        pio_topo_file => topo_file_get_id()

        if (initial_run) then
            ! Run type is initial run.

            call dyn_debug_print(debugout_info, 'Checking for consistency in topography data')

            call check_topography_data(pio_topo_file)

            if (analytic_ic_active()) then
                call dyn_debug_print(debugout_info, 'Initializing MPAS state variables by setting analytic initial condition')

                call set_analytic_initial_condition()
            else
                call dyn_debug_print(debugout_info, 'Initializing MPAS state variables by reading initial condition from a file')

                ! Perform default initialization for all constituents.
                ! Subsequently, they can be overridden depending on the namelist option (below) and
                ! the actual availability (checked and handled by MPAS).
                call dyn_exchange_constituent_states(direction='e', exchange=.true., conversion=.false.)

                ! Namelist option that controls if constituents are to be read from a file.
                if (readtrace) then
                    ! Read variables that belong to the "input" stream in MPAS.
                    call mpas_dynamical_core % read_write_stream(pio_init_file, 'r', 'input')
                else
                    ! Read variables that belong to the "input" stream in MPAS, excluding constituents.
                    call mpas_dynamical_core % read_write_stream(pio_init_file, 'r', 'input-scalars')
                end if
            end if
        else
            ! Run type is branch or restart run.

            call dyn_debug_print(debugout_info, 'Initializing MPAS state variables by restarting from a file')

            ! Read variables that belong to the "input" and "restart" streams in MPAS.
            call mpas_dynamical_core % read_write_stream(pio_init_file, 'r', 'input+restart')
        end if

        call clean_iodesc_list()
        call mark_variables_as_initialized()

        nullify(pio_init_file)
        nullify(pio_topo_file)

        ! This is the time interval for dynamics-physics coupling in CAM-SIMA.
        ! Each time MPAS dynamical core is called to run, it will integrate with time for this specific interval,
        ! then yield control back to the caller.
        coupling_time_interval = get_step_size()

        call dyn_debug_print(debugout_info, 'Initializing MPAS dynamical core (Phase 4/4)')

        ! Finish MPAS dynamical core initialization. After this point, MPAS dynamical core is ready for time integration.
        call mpas_dynamical_core % init_phase4(coupling_time_interval)

        call dyn_debug_print(debugout_debug, subname // ' completed')
    end subroutine dyn_init

    !> Check for consistency in topography data. The presence of topography file is inferred from the `pio_file` pointer.
    !> If topography file is used, check that the "PHIS" variable, which denotes surface geopotential,
    !> is consistent with the surface geometric height in MPAS.
    !> Otherwise, if topography file is not used, check that the surface geometric height in MPAS is zero.
    !> (KCW, 2024-05-10)
    subroutine check_topography_data(pio_file)
        ! Module(s) from CAM-SIMA.
        use cam_abortutils, only: check_allocate, endrun
        use cam_field_read, only: cam_read_field
        use cam_logfile, only: debug_output, debugout_debug, debugout_none, debugout_verbose
        use dynconst, only: constant_g => gravit
        ! Module(s) from CESM Share.
        use shr_kind_mod, only: kind_r8 => shr_kind_r8
        ! Module(s) from external libraries.
        use pio, only: file_desc_t, pio_file_is_open

        type(file_desc_t), pointer, intent(in) :: pio_file

        character(*), parameter :: subname = 'dyn_comp::check_topography_data'
        integer :: ierr
        logical :: success
        real(kind_r8), parameter :: error_tolerance = 1.0E-3_kind_r8 ! Error tolerance for consistency check.
        real(kind_r8), allocatable :: surface_geometric_height(:)    ! Computed from topography file.
        real(kind_r8), allocatable :: surface_geopotential(:)        ! Read from topography file.
        real(kind_dyn_mpas), pointer :: zgrid(:, :)                  ! From MPAS. Geometric height (m) at layer interfaces.

        call dyn_debug_print(debugout_debug, subname // ' entered')

        nullify(zgrid)

        call mpas_dynamical_core % get_variable_pointer(zgrid, 'mesh', 'zgrid')

        if (associated(pio_file)) then
            call dyn_debug_print(debugout_verbose, 'Topography file is used for consistency check')

            if (.not. pio_file_is_open(pio_file)) then
                call endrun('Invalid PIO file descriptor', subname, __LINE__)
            end if

            allocate(surface_geopotential(ncells_solve), stat=ierr)
            call check_allocate(ierr, subname, 'surface_geopotential(ncells_solve)', 'dyn_comp', __LINE__)

            allocate(surface_geometric_height(ncells_solve), stat=ierr)
            call check_allocate(ierr, subname, 'surface_geometric_height(ncells_solve)', 'dyn_comp', __LINE__)

            surface_geopotential(:) = 0.0_kind_r8
            surface_geometric_height(:) = 0.0_kind_r8

            call cam_read_field('PHIS', pio_file, surface_geopotential, success, &
                gridname='cam_cell', timelevel=1, log_output=(debug_output > debugout_none))

            if (.not. success) then
                call endrun('Failed to find variable "PHIS"', subname, __LINE__)
            end if

            surface_geometric_height(:) = surface_geopotential(:) / constant_g

            ! Surface geometric height in MPAS should match the values in topography file.
            if (any(abs(real(zgrid(1, 1:ncells_solve), kind_r8) - surface_geometric_height(:)) > error_tolerance)) then
                call endrun('Surface geometric height in MPAS is not consistent with topography data', subname, __LINE__)
            end if

            deallocate(surface_geopotential)
            deallocate(surface_geometric_height)
        else
            call dyn_debug_print(debugout_verbose, 'Topography file is not used for consistency check')

            ! Surface geometric height in MPAS should be zero.
            if (any(abs(real(zgrid(1, 1:ncells_solve), kind_r8)) > error_tolerance)) then
                call endrun('Surface geometric height in MPAS is not zero', subname, __LINE__)
            end if
        end if

        nullify(zgrid)

        call dyn_debug_print(debugout_debug, subname // ' completed')
    end subroutine check_topography_data

    !> Set analytic initial condition for MPAS.
    !> (KCW, 2024-05-22)
    subroutine set_analytic_initial_condition()
        ! Module(s) from CAM-SIMA.
        use cam_logfile, only: debugout_debug
        ! Module(s) from CESM Share.
        use shr_kind_mod, only: kind_r8 => shr_kind_r8

        character(*), parameter :: subname = 'dyn_comp::set_analytic_initial_condition'
        integer, allocatable :: global_grid_index(:)
        real(kind_r8), allocatable :: buffer_2d_real(:, :), buffer_3d_real(:, :, :)
        real(kind_r8), allocatable :: lat_rad(:), lon_rad(:)
        real(kind_r8), allocatable :: z_int(:, :)   ! Geometric height (m) at layer interfaces.
                                                    ! Dimension and vertical index orders follow CAM-SIMA convention.
        real(kind_dyn_mpas), pointer :: zgrid(:, :) ! Geometric height (m) at layer interfaces.
                                                    ! Dimension and vertical index orders follow MPAS convention.

        call dyn_debug_print(debugout_debug, subname // ' entered')

        call init_shared_variables()

        call set_mpas_state_u()
        call set_mpas_state_w()
        call set_mpas_state_scalars()
        call set_mpas_state_rho_theta()
        call set_mpas_state_rho_base_theta_base()

        call final_shared_variables()

        call dyn_debug_print(debugout_debug, subname // ' completed')
    contains
        !> Initialize variables that are shared and repeatedly used by the `set_mpas_state_*` internal subroutines.
        !> (KCW, 2024-05-13)
        subroutine init_shared_variables()
            ! Module(s) from CAM-SIMA.
            use cam_abortutils, only: check_allocate, endrun
            use cam_grid_support, only: cam_grid_get_latvals, cam_grid_get_lonvals, cam_grid_id
            use cam_logfile, only: debugout_verbose
            use dynconst, only: deg_to_rad
            use vert_coord, only: pverp

            character(*), parameter :: subname = 'dyn_comp::set_analytic_initial_condition::init_shared_variables'
            integer :: i
            integer :: ierr
            integer, pointer :: indextocellid(:)
            real(kind_r8), pointer :: lat_deg(:), lon_deg(:)

            call dyn_debug_print(debugout_verbose, 'Preparing to set analytic initial condition')

            nullify(zgrid)
            nullify(indextocellid)
            nullify(lat_deg, lon_deg)

            allocate(global_grid_index(ncells_solve), stat=ierr)
            call check_allocate(ierr, subname, 'global_grid_index(ncells_solve)', 'dyn_comp', __LINE__)

            call mpas_dynamical_core % get_variable_pointer(indextocellid, 'mesh', 'indexToCellID')

            global_grid_index(:) = indextocellid(1:ncells_solve)

            nullify(indextocellid)

            allocate(lat_rad(ncells_solve), stat=ierr)
            call check_allocate(ierr, subname, 'lat_rad(ncells_solve)', 'dyn_comp', __LINE__)

            allocate(lon_rad(ncells_solve), stat=ierr)
            call check_allocate(ierr, subname, 'lon_rad(ncells_solve)', 'dyn_comp', __LINE__)

            ! "mpas_cell" is a registered grid name that is defined in `dyn_grid`.
            lat_deg => cam_grid_get_latvals(cam_grid_id('mpas_cell'))
            lon_deg => cam_grid_get_lonvals(cam_grid_id('mpas_cell'))

            if (.not. associated(lat_deg)) then
                call endrun('Failed to find variable "lat_deg"', subname, __LINE__)
            end if

            if (.not. associated(lon_deg)) then
                call endrun('Failed to find variable "lon_deg"', subname, __LINE__)
            end if

            lat_rad(:) = lat_deg(:) * deg_to_rad
            lon_rad(:) = lon_deg(:) * deg_to_rad

            nullify(lat_deg, lon_deg)

            allocate(z_int(ncells_solve, pverp), stat=ierr)
            call check_allocate(ierr, subname, 'z_int(ncells_solve, pverp)', 'dyn_comp', __LINE__)

            call mpas_dynamical_core % get_variable_pointer(zgrid, 'mesh', 'zgrid')

            ! Vertical index order is reversed between CAM-SIMA and MPAS.
            do i = 1, ncells_solve
                z_int(i, :) = reverse(real(zgrid(:, i), kind_r8))
            end do
        end subroutine init_shared_variables

        !> Finalize variables that are shared and repeatedly used by the `set_mpas_state_*` internal subroutines.
        !> (KCW, 2024-05-13)
        subroutine final_shared_variables()
            character(*), parameter :: subname = 'dyn_comp::set_analytic_initial_condition::final_shared_variables'

            deallocate(global_grid_index)
            deallocate(lat_rad, lon_rad)
            deallocate(z_int)

            nullify(zgrid)
        end subroutine final_shared_variables

        !> Set MPAS state `u` (i.e., horizontal velocity at edge interfaces).
        !> (KCW, 2024-05-13)
        subroutine set_mpas_state_u()
            ! Module(s) from CAM-SIMA.
            use cam_abortutils, only: check_allocate
            use cam_logfile, only: debugout_verbose
            use dyn_tests_utils, only: vc_height
            use inic_analytic, only: dyn_set_inic_col
            use vert_coord, only: pver

            character(*), parameter :: subname = 'dyn_comp::set_analytic_initial_condition::set_mpas_state_u'
            integer :: i
            integer :: ierr
            real(kind_dyn_mpas), pointer :: ucellzonal(:, :), ucellmeridional(:, :)

            call dyn_debug_print(debugout_verbose, 'Setting MPAS state "u"')

            nullify(ucellzonal, ucellmeridional)

            allocate(buffer_2d_real(ncells_solve, pver), stat=ierr)
            call check_allocate(ierr, subname, 'buffer_2d_real(ncells_solve, pver)', 'dyn_comp', __LINE__)

            call mpas_dynamical_core % get_variable_pointer(ucellzonal, 'diag', 'uReconstructZonal')
            call mpas_dynamical_core % get_variable_pointer(ucellmeridional, 'diag', 'uReconstructMeridional')

            buffer_2d_real(:, :) = 0.0_kind_r8

            call dyn_set_inic_col(vc_height, lat_rad, lon_rad, global_grid_index, zint=z_int, u=buffer_2d_real)

            ! Vertical index order is reversed between CAM-SIMA and MPAS.
            do i = 1, ncells_solve
                ucellzonal(:, i) = real(reverse(buffer_2d_real(i, :)), kind_dyn_mpas)
            end do

            buffer_2d_real(:, :) = 0.0_kind_r8

            call dyn_set_inic_col(vc_height, lat_rad, lon_rad, global_grid_index, zint=z_int, v=buffer_2d_real)

            ! Vertical index order is reversed between CAM-SIMA and MPAS.
            do i = 1, ncells_solve
                ucellmeridional(:, i) = real(reverse(buffer_2d_real(i, :)), kind_dyn_mpas)
            end do

            deallocate(buffer_2d_real)

            nullify(ucellzonal, ucellmeridional)

            call mpas_dynamical_core % compute_edge_wind(wind_tendency=.false.)
        end subroutine set_mpas_state_u

        !> Set MPAS state `w` (i.e., vertical velocity at cell interfaces).
        !> (KCW, 2024-05-13)
        subroutine set_mpas_state_w()
            ! Module(s) from CAM-SIMA.
            use cam_logfile, only: debugout_verbose

            character(*), parameter :: subname = 'dyn_comp::set_analytic_initial_condition::set_mpas_state_w'
            real(kind_dyn_mpas), pointer :: w(:, :)

            call dyn_debug_print(debugout_verbose, 'Setting MPAS state "w"')

            nullify(w)

            call mpas_dynamical_core % get_variable_pointer(w, 'state', 'w', time_level=1)

            w(:, 1:ncells_solve) = 0.0_kind_dyn_mpas

            nullify(w)

            ! Because we are injecting data directly into MPAS memory, halo layers need to be updated manually.
            call mpas_dynamical_core % exchange_halo('w')
        end subroutine set_mpas_state_w

        !> Set MPAS state `scalars` (i.e., constituents).
        !> (KCW, 2024-05-17)
        subroutine set_mpas_state_scalars()
            ! Module(s) from CAM-SIMA.
            use cam_abortutils, only: check_allocate
            use cam_constituents, only: num_advected
            use cam_logfile, only: debugout_verbose
            use dyn_tests_utils, only: vc_height
            use inic_analytic, only: dyn_set_inic_col
            use vert_coord, only: pver

            ! CCPP standard name of `qv`, which denotes water vapor mixing ratio.
            character(*), parameter :: constituent_qv_standard_name = &
                'water_vapor_mixing_ratio_wrt_dry_air'

            character(*), parameter :: subname = 'dyn_comp::set_analytic_initial_condition::set_mpas_state_scalars'
            integer :: i, j
            integer :: ierr
            integer, allocatable :: constituent_index(:)
            integer, pointer :: index_qv
            real(kind_dyn_mpas), pointer :: scalars(:, :, :)

            call dyn_debug_print(debugout_verbose, 'Setting MPAS state "scalars"')

            nullify(index_qv)
            nullify(scalars)

            allocate(buffer_3d_real(ncells_solve, pver, num_advected), stat=ierr)
            call check_allocate(ierr, subname, 'buffer_3d_real(ncells_solve, pver, num_advected)', 'dyn_comp', __LINE__)

            allocate(constituent_index(num_advected), stat=ierr)
            call check_allocate(ierr, subname, 'constituent_index(num_advected)', 'dyn_comp', __LINE__)

            call mpas_dynamical_core % get_variable_pointer(index_qv, 'dim', 'index_qv')
            call mpas_dynamical_core % get_variable_pointer(scalars, 'state', 'scalars', time_level=1)

            buffer_3d_real(:, :, :) = 0.0_kind_r8
            constituent_index(:) = [(i, i = 1, num_advected)]

            call dyn_set_inic_col(vc_height, lat_rad, lon_rad, global_grid_index, zint=z_int, q=buffer_3d_real, &
                m_cnst=constituent_index)

            do i = 1, ncells_solve
                ! `j` is indexing into `scalars`, so it is regarded as MPAS scalar index.
                do j = 1, num_advected
                    ! Vertical index order is reversed between CAM-SIMA and MPAS.
                    scalars(j, :, i) = &
                        real(reverse(buffer_3d_real(i, :, mpas_dynamical_core % map_constituent_index(j))), kind_dyn_mpas)
                end do
            end do

            if (mpas_dynamical_core % get_constituent_name(mpas_dynamical_core % map_constituent_index(index_qv)) == &
                constituent_qv_standard_name) then
                ! The definition of `qv` matches exactly what MPAS wants. No conversion is needed.
                call dyn_debug_print(debugout_verbose, 'No conversion is needed for water vapor mixing ratio')
            else
                ! The definition of `qv` actually represents specific humidity. Conversion is needed.
                call dyn_debug_print(debugout_verbose, 'Conversion is needed and applied for water vapor mixing ratio')

                ! Convert specific humidity to water vapor mixing ratio.
                scalars(index_qv, :, 1:ncells_solve) = &
                    scalars(index_qv, :, 1:ncells_solve) / (1.0_kind_dyn_mpas - scalars(index_qv, :, 1:ncells_solve))
            end if

            deallocate(buffer_3d_real)
            deallocate(constituent_index)

            nullify(index_qv)
            nullify(scalars)

            ! Because we are injecting data directly into MPAS memory, halo layers need to be updated manually.
            call mpas_dynamical_core % exchange_halo('scalars')
        end subroutine set_mpas_state_scalars

        !> Set MPAS state `rho` (i.e., dry air density) and `theta` (i.e., potential temperature).
        !> (KCW, 2024-05-19)
        subroutine set_mpas_state_rho_theta()
            ! Module(s) from CAM-SIMA.
            use cam_abortutils, only: check_allocate
            use cam_logfile, only: debugout_verbose
            use dyn_tests_utils, only: vc_height
            use dynconst, only: constant_p0 => pref, constant_rd => rair, constant_rv => rh2o
            use inic_analytic, only: dyn_set_inic_col
            use vert_coord, only: pver

            character(*), parameter :: subname = 'dyn_comp::set_analytic_initial_condition::set_mpas_state_rho_theta'
            integer :: i, k
            integer :: ierr
            integer, pointer :: index_qv
            real(kind_r8), allocatable :: p_mid_col(:)  ! Pressure (Pa) at layer midpoints of each column. This is full pressure,
                                                        ! which also accounts for water vapor.
            real(kind_r8), allocatable :: p_sfc(:)      ! Pressure (Pa) at surface. This is full pressure,
                                                        ! which also accounts for water vapor.
            real(kind_r8), allocatable :: qv_mid_col(:) ! Water vapor mixing ratio (kg/kg) at layer midpoints of each column.
            real(kind_r8), allocatable :: t_mid(:, :)   ! Temperature (K) at layer midpoints.
            real(kind_r8), allocatable :: tm_mid_col(:) ! Modified "moist" temperature (K) at layer midpoints of each column.
                                                        ! Be advised that it is not virtual temperature.
                                                        ! See doi:10.5065/1DFH-6P97 and doi:10.1175/MWR-D-11-00215.1 for details.
            real(kind_dyn_mpas), pointer :: rho(:, :)
            real(kind_dyn_mpas), pointer :: theta(:, :)
            real(kind_dyn_mpas), pointer :: scalars(:, :, :)

            call dyn_debug_print(debugout_verbose, 'Setting MPAS state "rho" and "theta"')

            nullify(index_qv)
            nullify(rho)
            nullify(theta)
            nullify(scalars)

            allocate(p_sfc(ncells_solve), stat=ierr)
            call check_allocate(ierr, subname, 'p_sfc(ncells_solve)', 'dyn_comp', __LINE__)

            p_sfc(:) = 0.0_kind_r8

            call dyn_set_inic_col(vc_height, lat_rad, lon_rad, global_grid_index, zint=z_int, ps=p_sfc)

            allocate(buffer_2d_real(ncells_solve, pver), stat=ierr)
            call check_allocate(ierr, subname, 'buffer_2d_real(ncells_solve, pver)', 'dyn_comp', __LINE__)

            allocate(t_mid(pver, ncells_solve), stat=ierr)
            call check_allocate(ierr, subname, 't_mid(pver, ncells_solve)', 'dyn_comp', __LINE__)

            buffer_2d_real(:, :) = 0.0_kind_r8

            call dyn_set_inic_col(vc_height, lat_rad, lon_rad, global_grid_index, zint=z_int, t=buffer_2d_real)

            ! Vertical index order is reversed between CAM-SIMA and MPAS.
            do i = 1, ncells_solve
                t_mid(:, i) = reverse(buffer_2d_real(i, :))
            end do

            deallocate(buffer_2d_real)

            allocate(p_mid_col(pver), stat=ierr)
            call check_allocate(ierr, subname, 'p_mid_col(pver)', 'dyn_comp', __LINE__)

            allocate(qv_mid_col(pver), stat=ierr)
            call check_allocate(ierr, subname, 'qv_mid_col(pver)', 'dyn_comp', __LINE__)

            allocate(tm_mid_col(pver), stat=ierr)
            call check_allocate(ierr, subname, 'tm_mid_col(pver)', 'dyn_comp', __LINE__)

            call mpas_dynamical_core % get_variable_pointer(index_qv, 'dim', 'index_qv')
            call mpas_dynamical_core % get_variable_pointer(rho, 'diag', 'rho')
            call mpas_dynamical_core % get_variable_pointer(theta, 'diag', 'theta')
            call mpas_dynamical_core % get_variable_pointer(scalars, 'state', 'scalars', time_level=1)

            ! Set `rho` and `theta` column by column. This way, peak memory usage can be reduced.
            do i = 1, ncells_solve
                qv_mid_col(:) = real(scalars(index_qv, :, i), kind_r8)
                tm_mid_col(:) = t_mid(:, i) * (1.0_kind_r8 + constant_rv / constant_rd * qv_mid_col(:))

                ! Piecewise integrate hypsometric equation to derive `p_mid_col(1)`.
                ! The formulation used here is exact.
                p_mid_col(1) = p_by_hypsometric_equation( &
                    p_sfc(i), &
                    real(zgrid(1, i), kind_r8), &
                    tm_mid_col(1) / (1.0_kind_r8 + qv_mid_col(1)), &
                    0.5_kind_r8 * real(zgrid(2, i) + zgrid(1, i), kind_r8))

                ! Piecewise integrate hypsometric equation to derive subsequent `p_mid_col(k)`.
                ! The formulation used here is exact.
                do k = 2, pver
                    p_mid_col(k) = p_by_hypsometric_equation( &
                        p_by_hypsometric_equation( &
                            p_mid_col(k - 1), &
                            0.5_kind_r8 * real(zgrid(k, i) + zgrid(k - 1, i), kind_r8), &
                            tm_mid_col(k - 1) / (1.0_kind_r8 + qv_mid_col(k - 1)), &
                            real(zgrid(k, i), kind_r8)), &
                        real(zgrid(k, i), kind_r8), &
                        tm_mid_col(k) / (1.0_kind_r8 + qv_mid_col(k)), &
                        0.5_kind_r8 * real(zgrid(k + 1, i) + zgrid(k, i), kind_r8))
                end do

                rho(:, i) = real(p_mid_col(:) / (constant_rd * tm_mid_col(:)), kind_dyn_mpas)
                theta(:, i) = real(theta_by_poisson_equation(p_mid_col, t_mid(:, i), constant_p0), kind_dyn_mpas)
            end do

            deallocate(p_mid_col)
            deallocate(p_sfc)
            deallocate(qv_mid_col)
            deallocate(t_mid)
            deallocate(tm_mid_col)

            nullify(index_qv)
            nullify(rho)
            nullify(theta)
            nullify(scalars)

            ! Because we are injecting data directly into MPAS memory, halo layers need to be updated manually.
            call mpas_dynamical_core % exchange_halo('rho')
            call mpas_dynamical_core % exchange_halo('theta')
        end subroutine set_mpas_state_rho_theta

        !> Set MPAS state `rho_base` (i.e., base state dry air density) and `theta_base` (i.e., base state potential temperature).
        !> (KCW, 2024-05-21)
        subroutine set_mpas_state_rho_base_theta_base()
            ! Module(s) from CAM-SIMA.
            use cam_abortutils, only: check_allocate
            use cam_logfile, only: debugout_verbose
            use dynconst, only: constant_p0 => pref, constant_rd => rair
            use vert_coord, only: pver

            character(*), parameter :: subname = 'dyn_comp::set_analytic_initial_condition::set_mpas_state_rho_base_theta_base'
            integer :: i, k
            integer :: ierr
            real(kind_r8), parameter :: t_base = 250.0_kind_r8 ! Base state temperature (K) of dry isothermal atmosphere.
                                                               ! The value used here is identical to MPAS.
            real(kind_r8), allocatable :: p_base(:)            ! Base state pressure (Pa) at layer midpoints of each column.
            real(kind_dyn_mpas), pointer :: rho_base(:, :)
            real(kind_dyn_mpas), pointer :: theta_base(:, :)
            real(kind_dyn_mpas), pointer :: zz(:, :)

            call dyn_debug_print(debugout_verbose, 'Setting MPAS state "rho_base" and "theta_base"')

            nullify(rho_base)
            nullify(theta_base)
            nullify(zz)

            allocate(p_base(pver), stat=ierr)
            call check_allocate(ierr, subname, 'p_base(pver)', 'dyn_comp', __LINE__)

            call mpas_dynamical_core % get_variable_pointer(rho_base, 'diag', 'rho_base')
            call mpas_dynamical_core % get_variable_pointer(theta_base, 'diag', 'theta_base')
            call mpas_dynamical_core % get_variable_pointer(zz, 'mesh', 'zz')

            ! Set `rho_base` and `theta_base` column by column. This way, peak memory usage can be reduced.
            do i = 1, ncells_solve
                do k = 1, pver
                    ! Derive `p_base` by hypsometric equation.
                    ! The formulation used here is exact and identical to MPAS.
                    p_base(k) = p_by_hypsometric_equation( &
                        constant_p0, &
                        0.0_kind_r8, &
                        t_base, &
                        0.5_kind_r8 * real(zgrid(k + 1, i) + zgrid(k, i), kind_r8))
                end do

                rho_base(:, i) = real(p_base(:) / (constant_rd * t_base * real(zz(:, i), kind_r8)), kind_dyn_mpas)
                theta_base(:, i) = real(theta_by_poisson_equation(p_base, t_base, constant_p0), kind_dyn_mpas)
            end do

            deallocate(p_base)

            nullify(rho_base)
            nullify(theta_base)
            nullify(zz)

            ! Because we are injecting data directly into MPAS memory, halo layers need to be updated manually.
            call mpas_dynamical_core % exchange_halo('rho_base')
            call mpas_dynamical_core % exchange_halo('theta_base')
        end subroutine set_mpas_state_rho_base_theta_base

        ! ----- p_2, z_2 ----- (Layer 2)
        !       t_v
        ! ----- p_1, z_1 ----- (Layer 1)
        !
        !> Compute the pressure `p_2` at height `z_2` from the pressure `p_1` at height `z_1` by hypsometric equation.
        !> `t_v` is the mean virtual temperature between `z_1` and `z_2`. Essentially,
        !> \( P_2 = P_1 e^{\frac{-(z_2 - z_1) g}{R_d T_v}} \).
        !> (KCW, 2024-07-02)
        pure elemental function p_by_hypsometric_equation(p_1, z_1, t_v, z_2) result(p_2)
            ! Module(s) from CAM-SIMA.
            use dynconst, only: constant_g => gravit, constant_rd => rair

            real(kind_r8), intent(in) :: p_1, z_1, t_v, z_2
            real(kind_r8) :: p_2

            p_2 = p_1 * exp(-(z_2 - z_1) * constant_g / (constant_rd * t_v))
        end function p_by_hypsometric_equation

        ! ----- p_1, t_1 ----- (Arbitrary layer)
        !
        ! ----- p_0, t_0 ----- (Reference layer)
        !
        !> Compute the potential temperature `t_0` at reference pressure `p_0` from the temperature `t_1` at pressure `p_1` by
        !> Poisson equation. Essentially,
        !> \( \theta = T (\frac{P_0}{P})^{\frac{R_d}{C_p}} \).
        !> (KCW, 2024-07-02)
        pure elemental function theta_by_poisson_equation(p_1, t_1, p_0) result(t_0)
            ! Module(s) from CAM-SIMA.
            use dynconst, only: constant_cpd => cpair, constant_rd => rair

            real(kind_r8), intent(in) :: p_1, t_1, p_0
            real(kind_r8) :: t_0

            t_0 = t_1 * ((p_0 / p_1) ** (constant_rd / constant_cpd))
        end function theta_by_poisson_equation
    end subroutine set_analytic_initial_condition

    !> Exchange and/or convert constituent states between CAM-SIMA and MPAS.
    !> If `exchange` is `.true.` and `direction` is "e" or "export", set MPAS state `scalars` from physics state `constituents`.
    !> If `exchange` is `.true.` and `direction` is "i" or "import", set physics state `constituents` from MPAS state `scalars`.
    !> Think of it as "exporting/importing constituent states in CAM-SIMA to/from MPAS".
    !> Otherwise, if `exchange` is `.false.`, no exchange is performed at all.
    !> If `conversion` is `.true.`, appropriate conversion is performed for constituent mixing ratios that have different
    !> definitions between CAM-SIMA and MPAS (i.e., dry/moist).
    !> Otherwise, if `conversion` is `.false.`, no conversion is performed at all.
    !> This subroutine is intentionally designed to have these elaborate controls due to complications in CAM-SIMA.
    !> Some procedures in CAM-SIMA expect constituent states to be dry, while the others expect them to be moist.
    !> (KCW, 2024-09-26)
    subroutine dyn_exchange_constituent_states(direction, exchange, conversion)
        ! Module(s) from CAM-SIMA.
        use cam_abortutils, only: check_allocate, endrun
        use cam_constituents, only: const_is_dry, const_is_water_species, num_advected
        use cam_logfile, only: debugout_debug, debugout_info
        use physics_types, only: phys_state
        use vert_coord, only: pver
        ! Module(s) from CCPP.
        use cam_ccpp_cap, only: cam_constituents_array
        use ccpp_kinds, only: kind_phys
        ! Module(s) from CESM Share.
        use shr_kind_mod, only: kind_r8 => shr_kind_r8

        character(*), intent(in) :: direction
        logical, intent(in) :: exchange
        logical, intent(in) :: conversion

        character(*), parameter :: subname = 'dyn_comp::dyn_exchange_constituent_states'
        integer :: i, j
        integer :: ierr
        integer, allocatable :: is_water_species_index(:)
        logical, allocatable :: is_conversion_needed(:)
        logical, allocatable :: is_water_species(:)
        real(kind_phys), pointer :: constituents(:, :, :) ! This points to CCPP memory.
        real(kind_r8), allocatable :: sigma_all_q(:)      ! Summation of all water species mixing ratios.
        real(kind_dyn_mpas), pointer :: scalars(:, :, :)  ! This points to MPAS memory.

        call dyn_debug_print(debugout_debug, subname // ' entered')

        select case (trim(adjustl(direction)))
            case ('e', 'export')
                if (exchange) then
                    call dyn_debug_print(debugout_info, 'Setting MPAS state "scalars" from physics state "constituents"')
                end if

                if (conversion) then
                    call dyn_debug_print(debugout_info, 'Converting MPAS state "scalars"')
                end if
            case ('i', 'import')
                if (exchange) then
                    call dyn_debug_print(debugout_info, 'Setting physics state "constituents" from MPAS state "scalars"')
                end if

                if (conversion) then
                    call dyn_debug_print(debugout_info, 'Converting physics state "constituents"')
                end if
            case default
                call endrun('Unsupported exchange direction "' // trim(adjustl(direction)) // '"', subname, __LINE__)
        end select

        nullify(constituents)
        nullify(scalars)

        allocate(is_conversion_needed(num_advected), stat=ierr)
        call check_allocate(ierr, subname, &
            'is_conversion_needed(num_advected)', &
            'dyn_comp', __LINE__)

        allocate(is_water_species(num_advected), stat=ierr)
        call check_allocate(ierr, subname, &
            'is_water_species(num_advected)', &
            'dyn_comp', __LINE__)

        do j = 1, num_advected
            ! All constituent mixing ratios in MPAS are dry.
            ! Therefore, conversion in between is needed for any constituent mixing ratios that are not dry in CAM-SIMA.
            is_conversion_needed(j) = .not. const_is_dry(j)
            is_water_species(j) = const_is_water_species(j)
        end do

        allocate(is_water_species_index(count(is_water_species)), stat=ierr)
        call check_allocate(ierr, subname, &
            'is_water_species_index(count(is_water_species))', &
            'dyn_comp', __LINE__)

        allocate(sigma_all_q(pver), stat=ierr)
        call check_allocate(ierr, subname, &
            'sigma_all_q(pver)', &
            'dyn_comp', __LINE__)

        constituents => cam_constituents_array()

        if (.not. associated(constituents)) then
            call endrun('Failed to find variable "constituents"', subname, __LINE__)
        end if

        call mpas_dynamical_core % get_variable_pointer(scalars, 'state', 'scalars', time_level=1)

        if (trim(adjustl(direction)) == 'e' .or. trim(adjustl(direction)) == 'export') then
            do i = 1, ncells_solve
                if (conversion .and. any(is_conversion_needed)) then
                    ! The summation term of equation 8 in doi:10.1029/2017MS001257.
                    ! Using equation 7 here is not possible because it requires all constituent mixing ratio to be moist
                    ! on the RHS of it. There is no such guarantee in CAM-SIMA.
                    sigma_all_q(:) = reverse(phys_state % pdel(i, :) / phys_state % pdeldry(i, :))
                end if

                ! `j` is indexing into `scalars`, so it is regarded as MPAS scalar index.
                do j = 1, num_advected
                    if (exchange) then
                        ! Vertical index order is reversed between CAM-SIMA and MPAS.
                        scalars(j, :, i) = &
                            real(reverse(constituents(i, :, mpas_dynamical_core % map_constituent_index(j))), kind_dyn_mpas)
                    end if

                    if (conversion .and. is_conversion_needed(mpas_dynamical_core % map_constituent_index(j))) then
                        ! Equation 8 in doi:10.1029/2017MS001257.
                        scalars(j, :, i) = &
                            real(real(scalars(j, :, i), kind_r8) * sigma_all_q(:), kind_dyn_mpas)
                    end if
                end do
            end do
        else
            is_water_species_index(:) = &
                pack([(mpas_dynamical_core % map_mpas_scalar_index(i), i = 1, num_advected)], is_water_species)

            do i = 1, ncells_solve
                if (conversion .and. any(is_conversion_needed)) then
                    ! The summation term of equation 8 in doi:10.1029/2017MS001257.
                    sigma_all_q(:) = reverse(1.0_kind_r8 + sum(real(scalars(is_water_species_index, :, i), kind_r8), 1))
                end if

                ! `j` is indexing into `constituents`, so it is regarded as constituent index.
                do j = 1, num_advected
                    if (exchange) then
                        ! Vertical index order is reversed between CAM-SIMA and MPAS.
                        constituents(i, :, j) = &
                            reverse(real(scalars(mpas_dynamical_core % map_mpas_scalar_index(j), :, i), kind_r8))
                    end if

                    if (conversion .and. is_conversion_needed(j)) then
                        ! Equation 8 in doi:10.1029/2017MS001257.
                        constituents(i, :, j) = &
                            constituents(i, :, j) / sigma_all_q(:)
                    end if
                end do
            end do
        end if

        deallocate(is_conversion_needed)
        deallocate(is_water_species)
        deallocate(is_water_species_index)
        deallocate(sigma_all_q)

        nullify(constituents)
        nullify(scalars)

        if (trim(adjustl(direction)) == 'e' .or. trim(adjustl(direction)) == 'export') then
            ! Because we are injecting data directly into MPAS memory, halo layers need to be updated manually.
            call mpas_dynamical_core % exchange_halo('scalars')
        end if

        call dyn_debug_print(debugout_debug, subname // ' completed')
    end subroutine dyn_exchange_constituent_states

    !> Inquire local and global mesh dimensions. Save them as protected module variables.
    !> (KCW, 2024-11-21)
    subroutine dyn_inquire_mesh_dimensions()
        ! Module(s) from CAM-SIMA.
        use cam_logfile, only: debugout_debug, debugout_info
        use string_utils, only: stringify

        character(*), parameter :: subname = 'dyn_comp::dyn_inquire_mesh_dimensions'

        call dyn_debug_print(debugout_debug, subname // ' entered')

        call dyn_debug_print(debugout_info, 'Inquiring local and global mesh dimensions')

        call mpas_dynamical_core % get_local_mesh_dimension( &
            ncells, ncells_solve, nedges, nedges_solve, nvertices, nvertices_solve, nvertlevels)

        call mpas_dynamical_core % get_global_mesh_dimension( &
            ncells_global, nedges_global, nvertices_global, nvertlevels, ncells_max, nedges_max, &
            sphere_radius)

        call dyn_debug_print(debugout_debug, 'ncells_global    = ' // stringify([ncells_global]))
        call dyn_debug_print(debugout_debug, 'nedges_global    = ' // stringify([nedges_global]))
        call dyn_debug_print(debugout_debug, 'nvertices_global = ' // stringify([nvertices_global]))
        call dyn_debug_print(debugout_debug, 'nvertlevels      = ' // stringify([nvertlevels]))
        call dyn_debug_print(debugout_debug, 'ncells_max       = ' // stringify([ncells_max]))
        call dyn_debug_print(debugout_debug, 'nedges_max       = ' // stringify([nedges_max]))
        call dyn_debug_print(debugout_debug, 'sphere_radius    = ' // stringify([sphere_radius]))

        call dyn_debug_print(debugout_debug, subname // ' completed')
    end subroutine dyn_inquire_mesh_dimensions

    !> Mark everything in the `physics_types` module along with constituents as initialized
    !> to prevent physics from attempting to read them from a file.
    !> (KCW, 2024-05-23)
    subroutine mark_variables_as_initialized()
        ! Module(s) from CAM-SIMA.
        use cam_constituents, only: const_name, num_advected
        use cam_logfile, only: debugout_debug
        ! Module(s) from CCPP.
        use phys_vars_init_check, only: mark_as_initialized

        character(*), parameter :: subname = 'dyn_comp::mark_variables_as_initialized'
        integer :: i

        call dyn_debug_print(debugout_debug, subname // ' entered')

        ! The variables below are managed by dynamics interface.
        ! We are responsible for initializing and updating them.

        ! These variables are to be set during dynamics initialization.
        call mark_as_initialized('flag_for_dycore_energy_consistency_adjustment')
        call mark_as_initialized('total_energy_formula_for_dycore')

        ! These variables are to be set during dynamics-physics coupling.
        call mark_as_initialized('air_pressure')
        call mark_as_initialized('air_pressure_at_interface')
        call mark_as_initialized('air_pressure_of_dry_air')
        call mark_as_initialized('air_pressure_of_dry_air_at_interface')
        call mark_as_initialized('air_pressure_thickness')
        call mark_as_initialized('air_pressure_thickness_of_dry_air')
        call mark_as_initialized('air_temperature')
        call mark_as_initialized('dry_static_energy')
        call mark_as_initialized('eastward_wind')
        call mark_as_initialized('geopotential_height_wrt_surface')
        call mark_as_initialized('geopotential_height_wrt_surface_at_interface')
        call mark_as_initialized('lagrangian_tendency_of_air_pressure')
        call mark_as_initialized('ln_air_pressure')
        call mark_as_initialized('ln_air_pressure_at_interface')
        call mark_as_initialized('ln_air_pressure_of_dry_air')
        call mark_as_initialized('ln_air_pressure_of_dry_air_at_interface')
        call mark_as_initialized('northward_wind')
        call mark_as_initialized('reciprocal_of_air_pressure_thickness')
        call mark_as_initialized('reciprocal_of_air_pressure_thickness_of_dry_air')
        call mark_as_initialized('reciprocal_of_dimensionless_exner_function_wrt_surface_air_pressure')
        call mark_as_initialized('specific_heat_of_air_used_in_dycore')
        call mark_as_initialized('surface_air_pressure')
        call mark_as_initialized('surface_geopotential')
        call mark_as_initialized('surface_pressure_of_dry_air')
        call mark_as_initialized('tendency_of_air_temperature_due_to_model_physics')
        call mark_as_initialized('tendency_of_eastward_wind_due_to_model_physics')
        call mark_as_initialized('tendency_of_northward_wind_due_to_model_physics')

        ! CCPP standard names of constituents.
        do i = 1, num_advected
            call mark_as_initialized(trim(adjustl(const_name(i))))
        end do

        ! The variables below are not managed by dynamics interface. They are used by external CCPP physics schemes.
        ! While we are not responsible for initializing or updating them, we still need to help mark them as initialized.

        ! These variables are to be set externally by the `check_energy_chng` CCPP physics scheme.
        call mark_as_initialized('vertically_integrated_total_energy_using_dycore_energy_formula')
        call mark_as_initialized('vertically_integrated_total_energy_using_dycore_energy_formula_at_end_of_physics_timestep')
        call mark_as_initialized('vertically_integrated_total_energy_using_dycore_energy_formula_at_start_of_physics_timestep')
        call mark_as_initialized('vertically_integrated_total_energy_using_physics_energy_formula')
        call mark_as_initialized('vertically_integrated_total_energy_using_physics_energy_formula_at_start_of_physics_timestep')
        call mark_as_initialized('vertically_integrated_total_water')
        call mark_as_initialized('vertically_integrated_total_water_at_start_of_physics_timestep')

        call dyn_debug_print(debugout_debug, subname // ' completed')
    end subroutine mark_variables_as_initialized

    !> Run MPAS dynamical core to integrate the dynamical states with time.
    !> (KCW, 2024-07-11)
    subroutine dyn_run()
        ! Module(s) from CAM-SIMA.
        use cam_logfile, only: debugout_debug, debugout_info

        character(*), parameter :: subname = 'dyn_comp::dyn_run'

        call dyn_debug_print(debugout_debug, subname // ' entered')

        call dyn_debug_print(debugout_info, 'Running MPAS dynamical core')

        ! MPAS dynamical core will run until the coupling time interval is reached.
        call mpas_dynamical_core % run()

        call dyn_debug_print(debugout_debug, subname // ' completed')
    end subroutine dyn_run

    !> Finalize MPAS dynamical core as well as its framework.
    !> (KCW, 2024-10-04)
    subroutine dyn_final()
        ! Module(s) from CAM-SIMA.
        use cam_logfile, only: debugout_debug, debugout_info

        character(*), parameter :: subname = 'dyn_comp::dyn_final'

        call dyn_debug_print(debugout_debug, subname // ' entered')

        call dyn_debug_print(debugout_info, 'Finalizing MPAS dynamical core')

        ! Quick hack for dumping variables from MPAS dynamical core.
        ! Remove it once history and restart are wired up in CAM-SIMA.
        call dyn_variable_dump()

        ! After this point, do not access anything under MPAS dynamical core or runtime errors will ensue.
        call mpas_dynamical_core % final()

        call dyn_debug_print(debugout_debug, subname // ' completed')
    end subroutine dyn_final

    subroutine dyn_variable_dump()
        ! Module(s) from CAM-SIMA.
        use cam_abortutils, only: check_allocate, endrun
        use cam_instance, only: atm_id
        use physics_types, only: phys_state
        ! Module(s) from CESM Share.
        use shr_pio_mod, only: shr_pio_getioformat, shr_pio_getiosys, shr_pio_getiotype
        ! Module(s) from external libraries.
        use pio, only: file_desc_t, iosystem_desc_t, pio_createfile, pio_closefile, pio_clobber, pio_noerr

        character(*), parameter :: subname = 'dyn_comp::dyn_variable_dump'
        integer :: ierr
        integer :: pio_ioformat, pio_iotype
        real(kind_dyn_mpas), pointer :: surface_pressure(:)
        type(file_desc_t), pointer :: pio_file
        type(iosystem_desc_t), pointer :: pio_iosystem

        nullify(pio_file)
        nullify(pio_iosystem)
        nullify(surface_pressure)

        call mpas_dynamical_core % get_variable_pointer(surface_pressure, 'diag', 'surface_pressure')

        surface_pressure(1:ncells_solve) = real(phys_state % ps(:), kind_dyn_mpas)

        nullify(surface_pressure)

        call mpas_dynamical_core % exchange_halo('surface_pressure')

        allocate(pio_file, stat=ierr)
        call check_allocate(ierr, subname, 'pio_file', 'dyn_comp', __LINE__)

        pio_iosystem => shr_pio_getiosys(atm_id)

        pio_ioformat = shr_pio_getioformat(atm_id)
        pio_ioformat = ior(pio_ioformat, pio_clobber)

        pio_iotype = shr_pio_getiotype(atm_id)

        ierr = pio_createfile(pio_iosystem, pio_file, pio_iotype, 'dyn_variable_dump.nc', pio_ioformat)

        if (ierr /= pio_noerr) then
            call endrun('Failed to create file for variable dumping', subname, __LINE__)
        end if

        call mpas_dynamical_core % read_write_stream(pio_file, 'w', 'invariant+input+restart+output')

        call pio_closefile(pio_file)

        deallocate(pio_file)

        nullify(pio_file)
        nullify(pio_iosystem)
    end subroutine dyn_variable_dump

    !> Helper function for reversing the order of elements in `array`.
    !> (KCW, 2024-07-17)
    pure function reverse(array)
        ! Module(s) from CESM Share.
        use shr_kind_mod, only: kind_r8 => shr_kind_r8

        real(kind_r8), intent(in) :: array(:)
        real(kind_r8) :: reverse(size(array))

        integer :: n

        n = size(array)

        ! There is nothing to reverse.
        if (n == 0) then
            return
        end if

        reverse(:) = array(n:1:-1)
    end function reverse
end module dyn_comp
