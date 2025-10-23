! Copyright (C) 2025 University Corporation for Atmospheric Research (UCAR)
! SPDX-License-Identifier: Apache-2.0

!> This module, part of the MPAS interface, integrates MPAS dynamical core with CAM-SIMA by
!> implementing the necessary APIs and managing their interaction.
!>
!> It contains the instance of MPAS dynamical core, which is used extensively throughout CAM-SIMA.
!> It provides core functionalities such as the initialization, running, and finalization of MPAS
!> dynamical core.
submodule (dyn_comp) dyn_comp_impl
    implicit none
contains
    !> Print a debug message at a debug level. The debug message will be prefixed by "MPAS Interface (N): ", where `N`
    !> is the MPI rank. The debug level is one of the `debugout_*` constants from the `cam_logfile` module.
    !> If `printer` is not supplied, the MPI root rank will print. Otherwise, the designated MPI rank will print instead.
    !> (KCW, 2024-02-03)
    module subroutine dyn_debug_print(level, message, printer)
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
    module subroutine dyn_readnl(namelist_path)
        ! Module(s) from CAM-SIMA.
        use cam_abortutils, only: endrun
        use cam_control_mod, only: initial_run
        use cam_instance, only: atm_id
        use cam_logfile, only: debug_output, debugout_debug, debugout_info, iulog
        use dyn_procedures, only: sec_to_hour_min_sec
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
    end subroutine dyn_readnl

    !> Initialize MPAS dynamical core by one of the following:
    !> 1. Setting analytic initial condition;
    !> 2. Reading initial condition from a file;
    !> 3. Restarting from a file.
    !> (KCW, 2024-05-28)
    !
    ! Called by `cam_init` in `src/control/cam_comp.F90`.
    module subroutine dyn_init(cam_runtime_opts, dyn_in, dyn_out)
        ! Module(s) from CAM-SIMA.
        use cam_abortutils, only: check_allocate
        use cam_constituents, only: const_name, const_is_water_species, num_advected, readtrace
        use cam_control_mod, only: initial_run
        use cam_initfiles, only: initial_file_get_id, topo_file_get_id
        use cam_logfile, only: debugout_debug, debugout_info
        use cam_pio_utils, only: clean_iodesc_list
        use dyn_coupling, only: dyn_exchange_constituent_states
        use inic_analytic, only: analytic_ic_active
        use runtime_obj, only: runtime_options
        use time_manager, only: get_step_size
        ! Module(s) from CCPP.
        use phys_vars_init_check, only: std_name_len
        ! Module(s) from CESM Share.
        use shr_kind_mod, only: len_cx => shr_kind_cx
        ! Module(s) from external libraries.
        use pio, only: file_desc_t

        type(runtime_options), intent(in) :: cam_runtime_opts
        type(dyn_import_t), intent(in) :: dyn_in
        type(dyn_export_t), intent(in) :: dyn_out

        character(*), parameter :: subname = 'dyn_comp::dyn_init'
        character(len_cx) :: cerr
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

        allocate(constituent_name(num_advected), errmsg=cerr, stat=ierr)
        call check_allocate(ierr, subname, 'constituent_name(num_advected)', &
            file='dyn_comp', line=__LINE__, errmsg=trim(adjustl(cerr)))

        allocate(is_water_species(num_advected), errmsg=cerr, stat=ierr)
        call check_allocate(ierr, subname, 'is_water_species(num_advected)', &
            file='dyn_comp', line=__LINE__, errmsg=trim(adjustl(cerr)))

        do i = 1, num_advected
            constituent_name(i) = const_name(i)
            is_water_species(i) = const_is_water_species(i)
        end do

        call dyn_debug_print(debugout_info, 'Defining MPAS scalars and scalar tendencies')

        ! Inform MPAS about the constituent names and their corresponding waterness.
        call mpas_dynamical_core % define_scalar(constituent_name, is_water_species)

        deallocate(constituent_name)
        deallocate(is_water_species)

        call set_thermodynamic_active_species_mapping()
        call set_thermodynamic_energy_formula()

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

    !> Inform CAM-SIMA about the index mapping between MPAS scalars and CAM-SIMA constituents.
    !> (KCW, 2025-07-17)
    subroutine set_thermodynamic_active_species_mapping()
        ! Module(s) from CAM-SIMA.
        use air_composition, only: thermodynamic_active_species_num, &
                                   thermodynamic_active_species_liq_num, &
                                   thermodynamic_active_species_ice_num, &
                                   thermodynamic_active_species_idx, thermodynamic_active_species_idx_dycore, &
                                   thermodynamic_active_species_liq_idx, thermodynamic_active_species_liq_idx_dycore, &
                                   thermodynamic_active_species_ice_idx, thermodynamic_active_species_ice_idx_dycore
        use cam_logfile, only: debugout_debug
        use string_utils, only: stringify

        character(*), parameter :: subname = 'dyn_comp::set_thermodynamic_active_species_mapping'
        integer :: i

        call dyn_debug_print(debugout_debug, subname // ' entered')

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

        call dyn_debug_print(debugout_debug, subname // ' completed')
    end subroutine set_thermodynamic_active_species_mapping

    !> Set the thermodynamic energy formula of dynamical core to MPAS.
    !> (KCW, 2025-07-17)
    subroutine set_thermodynamic_energy_formula()
        ! Module(s) from CAM-SIMA.
        use cam_logfile, only: debugout_debug
        use cam_thermo_formula, only: energy_formula_dycore, energy_formula_dycore_mpas
        use physics_types, only: dycore_energy_consistency_adjust

        character(*), parameter :: subname = 'dyn_comp::set_thermodynamic_energy_formula'

        call dyn_debug_print(debugout_debug, subname // ' entered')

        ! Set the thermodynamic energy formula of dynamical core to MPAS for use in `cam_thermo`.
        energy_formula_dycore = energy_formula_dycore_mpas

        ! The total energy of dynamical core, which uses "MPAS formula" as set above, is not consistent with
        ! that of CAM physics, which uses "FV formula". Therefore, temperature and temperature tendency adjustments
        ! are needed at the end of each physics time step.
        dycore_energy_consistency_adjust = .true.

        call dyn_debug_print(debugout_debug, subname // ' completed')
    end subroutine set_thermodynamic_energy_formula

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
        use dyn_grid, only: ncells_solve
        use dynconst, only: constant_g => gravit
        ! Module(s) from CESM Share.
        use shr_kind_mod, only: kind_r8 => shr_kind_r8, &
                                len_cx => shr_kind_cx
        ! Module(s) from external libraries.
        use pio, only: file_desc_t, pio_file_is_open
        ! Module(s) from MPAS.
        use dyn_mpas_procedures, only: almost_equal

        type(file_desc_t), pointer, intent(in) :: pio_file

        character(*), parameter :: subname = 'dyn_comp::check_topography_data'
        character(len_cx) :: cerr
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

            allocate(surface_geopotential(ncells_solve), errmsg=cerr, stat=ierr)
            call check_allocate(ierr, subname, 'surface_geopotential(ncells_solve)', &
                file='dyn_comp', line=__LINE__, errmsg=trim(adjustl(cerr)))

            allocate(surface_geometric_height(ncells_solve), errmsg=cerr, stat=ierr)
            call check_allocate(ierr, subname, 'surface_geometric_height(ncells_solve)', &
                file='dyn_comp', line=__LINE__, errmsg=trim(adjustl(cerr)))

            surface_geopotential(:) = 0.0_kind_r8
            surface_geometric_height(:) = 0.0_kind_r8

            call cam_read_field('PHIS', pio_file, surface_geopotential, success, &
                gridname='cam_cell', timelevel=1, log_output=(debug_output > debugout_none))

            if (.not. success) then
                call endrun('Failed to find variable "PHIS"', subname, __LINE__)
            end if

            surface_geometric_height(:) = surface_geopotential(:) / constant_g

            ! Surface geometric height in MPAS should match the values in topography file.
            if (.not. all(almost_equal( &
                real(zgrid(1, 1:ncells_solve), kind_r8), surface_geometric_height, &
                relative_tolerance=error_tolerance))) then
                call endrun('Surface geometric height in MPAS is not consistent with topography data', subname, __LINE__)
            end if

            deallocate(surface_geopotential)
            deallocate(surface_geometric_height)
        else
            call dyn_debug_print(debugout_verbose, 'Topography file is not used for consistency check')

            ! Surface geometric height in MPAS should be zero.
            if (.not. all(almost_equal( &
                real(zgrid(1, 1:ncells_solve), kind_r8), 0.0_kind_r8, &
                relative_tolerance=error_tolerance))) then
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
            use dyn_grid, only: ncells_solve
            use dyn_procedures, only: reverse
            use dynconst, only: deg_to_rad
            use vert_coord, only: pverp
            ! Module(s) from CESM Share.
            use shr_kind_mod, only: len_cx => shr_kind_cx

            character(*), parameter :: subname = 'dyn_comp::set_analytic_initial_condition::init_shared_variables'
            character(len_cx) :: cerr
            integer :: i
            integer :: ierr
            integer, pointer :: indextocellid(:)
            real(kind_r8), pointer :: lat_deg(:), lon_deg(:)

            call dyn_debug_print(debugout_verbose, 'Preparing to set analytic initial condition')

            nullify(zgrid)
            nullify(indextocellid)
            nullify(lat_deg, lon_deg)

            allocate(global_grid_index(ncells_solve), errmsg=cerr, stat=ierr)
            call check_allocate(ierr, subname, 'global_grid_index(ncells_solve)', &
                file='dyn_comp', line=__LINE__, errmsg=trim(adjustl(cerr)))

            call mpas_dynamical_core % get_variable_pointer(indextocellid, 'mesh', 'indexToCellID')

            global_grid_index(:) = indextocellid(1:ncells_solve)

            nullify(indextocellid)

            allocate(lat_rad(ncells_solve), errmsg=cerr, stat=ierr)
            call check_allocate(ierr, subname, 'lat_rad(ncells_solve)', &
                file='dyn_comp', line=__LINE__, errmsg=trim(adjustl(cerr)))

            allocate(lon_rad(ncells_solve), errmsg=cerr, stat=ierr)
            call check_allocate(ierr, subname, 'lon_rad(ncells_solve)', &
                file='dyn_comp', line=__LINE__, errmsg=trim(adjustl(cerr)))

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

            allocate(z_int(ncells_solve, pverp), errmsg=cerr, stat=ierr)
            call check_allocate(ierr, subname, 'z_int(ncells_solve, pverp)', &
                file='dyn_comp', line=__LINE__, errmsg=trim(adjustl(cerr)))

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
            use dyn_grid, only: ncells_solve
            use dyn_procedures, only: reverse
            use dyn_tests_utils, only: vc_height
            use inic_analytic, only: dyn_set_inic_col
            use vert_coord, only: pver
            ! Module(s) from CESM Share.
            use shr_kind_mod, only: len_cx => shr_kind_cx

            character(*), parameter :: subname = 'dyn_comp::set_analytic_initial_condition::set_mpas_state_u'
            character(len_cx) :: cerr
            integer :: i
            integer :: ierr
            real(kind_dyn_mpas), pointer :: ucellzonal(:, :), ucellmeridional(:, :)

            call dyn_debug_print(debugout_verbose, 'Setting MPAS state "u"')

            nullify(ucellzonal, ucellmeridional)

            allocate(buffer_2d_real(ncells_solve, pver), errmsg=cerr, stat=ierr)
            call check_allocate(ierr, subname, 'buffer_2d_real(ncells_solve, pver)', &
                file='dyn_comp', line=__LINE__, errmsg=trim(adjustl(cerr)))

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
            use dyn_grid, only: ncells_solve

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
            use dyn_grid, only: ncells_solve
            use dyn_procedures, only: qv_of_sh, reverse
            use dyn_tests_utils, only: vc_height
            use inic_analytic, only: dyn_set_inic_col
            use vert_coord, only: pver
            ! Module(s) from CESM Share.
            use shr_kind_mod, only: len_cx => shr_kind_cx

            ! CCPP standard name of `qv`, which denotes water vapor mixing ratio.
            character(*), parameter :: constituent_qv_standard_name = &
                'water_vapor_mixing_ratio_wrt_dry_air'

            character(*), parameter :: subname = 'dyn_comp::set_analytic_initial_condition::set_mpas_state_scalars'
            character(len_cx) :: cerr
            integer :: i, j
            integer :: ierr
            integer, allocatable :: constituent_index(:)
            integer, pointer :: index_qv
            real(kind_dyn_mpas), pointer :: scalars(:, :, :)

            call dyn_debug_print(debugout_verbose, 'Setting MPAS state "scalars"')

            nullify(index_qv)
            nullify(scalars)

            allocate(buffer_3d_real(ncells_solve, pver, num_advected), errmsg=cerr, stat=ierr)
            call check_allocate(ierr, subname, 'buffer_3d_real(ncells_solve, pver, num_advected)', &
                file='dyn_comp', line=__LINE__, errmsg=trim(adjustl(cerr)))

            allocate(constituent_index(num_advected), errmsg=cerr, stat=ierr)
            call check_allocate(ierr, subname, 'constituent_index(num_advected)', &
                file='dyn_comp', line=__LINE__, errmsg=trim(adjustl(cerr)))

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
                scalars(index_qv, :, 1:ncells_solve) = real(qv_of_sh( &
                    real(scalars(index_qv, :, 1:ncells_solve), kind_r8)), kind_dyn_mpas)
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
            use dyn_grid, only: ncells_solve
            use dyn_procedures, only: p_by_hypsometric_equation, rho_by_equation_of_state, theta_by_poisson_equation, &
                                      tm_of_t_qv, tv_of_tm_qv, reverse
            use dyn_tests_utils, only: vc_height
            use dynconst, only: constant_cpd => cpair, constant_g => gravit, constant_p0 => pref, &
                                constant_rd => rair, constant_rv => rh2o
            use inic_analytic, only: dyn_set_inic_col
            use vert_coord, only: pver
            ! Module(s) from CESM Share.
            use shr_kind_mod, only: len_cx => shr_kind_cx

            character(*), parameter :: subname = 'dyn_comp::set_analytic_initial_condition::set_mpas_state_rho_theta'
            character(len_cx) :: cerr
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
            real(kind_r8), allocatable :: tv_mid_col(:) ! Virtual temperature (K) at layer midpoints of each column.
            real(kind_dyn_mpas), pointer :: rho(:, :)
            real(kind_dyn_mpas), pointer :: theta(:, :)
            real(kind_dyn_mpas), pointer :: scalars(:, :, :)

            call dyn_debug_print(debugout_verbose, 'Setting MPAS state "rho" and "theta"')

            nullify(index_qv)
            nullify(rho)
            nullify(theta)
            nullify(scalars)

            allocate(p_sfc(ncells_solve), errmsg=cerr, stat=ierr)
            call check_allocate(ierr, subname, 'p_sfc(ncells_solve)', &
                file='dyn_comp', line=__LINE__, errmsg=trim(adjustl(cerr)))

            p_sfc(:) = 0.0_kind_r8

            call dyn_set_inic_col(vc_height, lat_rad, lon_rad, global_grid_index, zint=z_int, ps=p_sfc)

            allocate(buffer_2d_real(ncells_solve, pver), errmsg=cerr, stat=ierr)
            call check_allocate(ierr, subname, 'buffer_2d_real(ncells_solve, pver)', &
                file='dyn_comp', line=__LINE__, errmsg=trim(adjustl(cerr)))

            allocate(t_mid(pver, ncells_solve), errmsg=cerr, stat=ierr)
            call check_allocate(ierr, subname, 't_mid(pver, ncells_solve)', &
                file='dyn_comp', line=__LINE__, errmsg=trim(adjustl(cerr)))

            buffer_2d_real(:, :) = 0.0_kind_r8

            call dyn_set_inic_col(vc_height, lat_rad, lon_rad, global_grid_index, zint=z_int, t=buffer_2d_real)

            ! Vertical index order is reversed between CAM-SIMA and MPAS.
            do i = 1, ncells_solve
                t_mid(:, i) = reverse(buffer_2d_real(i, :))
            end do

            deallocate(buffer_2d_real)

            allocate(p_mid_col(pver), errmsg=cerr, stat=ierr)
            call check_allocate(ierr, subname, 'p_mid_col(pver)', &
                file='dyn_comp', line=__LINE__, errmsg=trim(adjustl(cerr)))

            allocate(qv_mid_col(pver), errmsg=cerr, stat=ierr)
            call check_allocate(ierr, subname, 'qv_mid_col(pver)', &
                file='dyn_comp', line=__LINE__, errmsg=trim(adjustl(cerr)))

            allocate(tm_mid_col(pver), errmsg=cerr, stat=ierr)
            call check_allocate(ierr, subname, 'tm_mid_col(pver)', &
                file='dyn_comp', line=__LINE__, errmsg=trim(adjustl(cerr)))

            allocate(tv_mid_col(pver), errmsg=cerr, stat=ierr)
            call check_allocate(ierr, subname, 'tv_mid_col(pver)', &
                file='dyn_comp', line=__LINE__, errmsg=trim(adjustl(cerr)))

            call mpas_dynamical_core % get_variable_pointer(index_qv, 'dim', 'index_qv')
            call mpas_dynamical_core % get_variable_pointer(rho, 'diag', 'rho')
            call mpas_dynamical_core % get_variable_pointer(theta, 'diag', 'theta')
            call mpas_dynamical_core % get_variable_pointer(scalars, 'state', 'scalars', time_level=1)

            ! Set `rho` and `theta` column by column. This way, peak memory usage can be reduced.
            do i = 1, ncells_solve
                qv_mid_col(:) = real(scalars(index_qv, :, i), kind_r8)
                tm_mid_col(:) = tm_of_t_qv(constant_rd, constant_rv, t_mid(:, i), qv_mid_col)
                tv_mid_col(:) = tv_of_tm_qv(tm_mid_col, qv_mid_col)

                ! Piecewise integrate hypsometric equation to derive `p_mid_col(1)`.
                ! The formulation used here is exact.
                p_mid_col(1) = p_by_hypsometric_equation( &
                    constant_g, &
                    constant_rd, &
                    p_sfc(i), &
                    real(zgrid(1, i), kind_r8), &
                    tv_mid_col(1), &
                    0.5_kind_r8 * real(zgrid(2, i) + zgrid(1, i), kind_r8))

                ! Piecewise integrate hypsometric equation to derive subsequent `p_mid_col(k)`.
                ! The formulation used here is exact.
                do k = 2, pver
                    p_mid_col(k) = p_by_hypsometric_equation( &
                        constant_g, &
                        constant_rd, &
                        p_by_hypsometric_equation( &
                            constant_g, &
                            constant_rd, &
                            p_mid_col(k - 1), &
                            0.5_kind_r8 * real(zgrid(k, i) + zgrid(k - 1, i), kind_r8), &
                            tv_mid_col(k - 1), &
                            real(zgrid(k, i), kind_r8)), &
                        real(zgrid(k, i), kind_r8), &
                        tv_mid_col(k), &
                        0.5_kind_r8 * real(zgrid(k + 1, i) + zgrid(k, i), kind_r8))
                end do

                rho(:, i) = real(rho_by_equation_of_state( &
                    constant_rd, p_mid_col, tm_mid_col), kind_dyn_mpas)
                theta(:, i) = real(theta_by_poisson_equation( &
                    constant_cpd, constant_p0, constant_rd, t_mid(:, i), p_mid_col), kind_dyn_mpas)
            end do

            deallocate(p_mid_col)
            deallocate(p_sfc)
            deallocate(qv_mid_col)
            deallocate(t_mid)
            deallocate(tm_mid_col)
            deallocate(tv_mid_col)

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
            use dyn_grid, only: ncells_solve
            use dyn_procedures, only: p_by_hypsometric_equation, rho_by_equation_of_state, theta_by_poisson_equation
            use dynconst, only: constant_cpd => cpair, constant_g => gravit, constant_p0 => pref, &
                                constant_rd => rair
            use vert_coord, only: pver
            ! Module(s) from CESM Share.
            use shr_kind_mod, only: len_cx => shr_kind_cx

            character(*), parameter :: subname = 'dyn_comp::set_analytic_initial_condition::set_mpas_state_rho_base_theta_base'
            character(len_cx) :: cerr
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

            allocate(p_base(pver), errmsg=cerr, stat=ierr)
            call check_allocate(ierr, subname, 'p_base(pver)', &
                file='dyn_comp', line=__LINE__, errmsg=trim(adjustl(cerr)))

            call mpas_dynamical_core % get_variable_pointer(rho_base, 'diag', 'rho_base')
            call mpas_dynamical_core % get_variable_pointer(theta_base, 'diag', 'theta_base')
            call mpas_dynamical_core % get_variable_pointer(zz, 'mesh', 'zz')

            ! Set `rho_base` and `theta_base` column by column. This way, peak memory usage can be reduced.
            do i = 1, ncells_solve
                do k = 1, pver
                    ! Derive `p_base` by hypsometric equation.
                    ! The formulation used here is exact and identical to MPAS.
                    p_base(k) = p_by_hypsometric_equation( &
                        constant_g, &
                        constant_rd, &
                        constant_p0, &
                        0.0_kind_r8, &
                        t_base, &
                        0.5_kind_r8 * real(zgrid(k + 1, i) + zgrid(k, i), kind_r8))
                end do

                rho_base(:, i) = real(rho_by_equation_of_state( &
                    constant_rd, p_base, t_base) / real(zz(:, i), kind_r8), kind_dyn_mpas)
                theta_base(:, i) = real(theta_by_poisson_equation( &
                    constant_cpd, constant_p0, constant_rd, t_base, p_base), kind_dyn_mpas)
            end do

            deallocate(p_base)

            nullify(rho_base)
            nullify(theta_base)
            nullify(zz)

            ! Because we are injecting data directly into MPAS memory, halo layers need to be updated manually.
            call mpas_dynamical_core % exchange_halo('rho_base')
            call mpas_dynamical_core % exchange_halo('theta_base')
        end subroutine set_mpas_state_rho_base_theta_base
    end subroutine set_analytic_initial_condition

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
    module subroutine dyn_run()
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
    module subroutine dyn_final()
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
        use dyn_grid, only: ncells_solve
        use physics_types, only: phys_state
        ! Module(s) from CESM Share.
        use shr_kind_mod, only: len_cx => shr_kind_cx
        use shr_pio_mod, only: shr_pio_getioformat, shr_pio_getiosys, shr_pio_getiotype
        ! Module(s) from external libraries.
        use pio, only: file_desc_t, iosystem_desc_t, pio_createfile, pio_closefile, pio_clobber, pio_noerr

        character(*), parameter :: subname = 'dyn_comp::dyn_variable_dump'
        character(len_cx) :: cerr
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

        allocate(pio_file, errmsg=cerr, stat=ierr)
        call check_allocate(ierr, subname, 'pio_file', &
            file='dyn_comp', line=__LINE__, errmsg=trim(adjustl(cerr)))

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
end submodule dyn_comp_impl
