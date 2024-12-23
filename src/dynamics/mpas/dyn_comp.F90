module dyn_comp
    use dyn_mpas_subdriver, only: mpas_dynamical_core_type

    ! Modules from CAM-SIMA.
    use air_composition, only: thermodynamic_active_species_num, &
                               thermodynamic_active_species_liq_num, &
                               thermodynamic_active_species_ice_num, &
                               thermodynamic_active_species_idx, thermodynamic_active_species_idx_dycore, &
                               thermodynamic_active_species_liq_idx, thermodynamic_active_species_liq_idx_dycore, &
                               thermodynamic_active_species_ice_idx, thermodynamic_active_species_ice_idx_dycore
    use cam_abortutils, only: check_allocate, endrun
    use cam_constituents, only: const_name, const_is_dry, const_is_water_species, num_advected, readtrace
    use cam_control_mod, only: initial_run
    use cam_field_read, only: cam_read_field
    use cam_grid_support, only: cam_grid_get_latvals, cam_grid_get_lonvals, cam_grid_id
    use cam_initfiles, only: initial_file_get_id, topo_file_get_id
    use cam_instance, only: atm_id
    use cam_logfile, only: debug_output, debugout_none, iulog
    use cam_pio_utils, only: clean_iodesc_list
    use dyn_tests_utils, only: vc_height
    use dynconst, only: constant_cpd => cpair, constant_g => gravit, constant_p0 => pref, constant_pi => pi, &
                        constant_rd => rair, constant_rv => rh2o, &
                        deg_to_rad
    use inic_analytic, only: analytic_ic_active, dyn_set_inic_col
    use runtime_obj, only: runtime_options
    use spmd_utils, only: iam, masterproc, mpicom
    use string_utils, only: stringify
    use time_manager, only: get_start_date, get_stop_date, get_step_size, get_run_duration, timemgr_get_calendar_cf
    use vert_coord, only: pver, pverp

    ! Modules from CCPP.
    use cam_ccpp_cap, only: cam_constituents_array
    use ccpp_kinds, only: kind_phys
    use phys_vars_init_check, only: mark_as_initialized, std_name_len
    use physics_types, only: phys_state

    ! Modules from CESM Share.
    use shr_file_mod, only: shr_file_getunit
    use shr_kind_mod, only: kind_cs => shr_kind_cs, kind_r8 => shr_kind_r8
    use shr_pio_mod, only: shr_pio_getiosys

    ! Modules from external libraries.
    use pio, only: file_desc_t, iosystem_desc_t, pio_file_is_open

    implicit none

    private
    ! Provide APIs required by CAM-SIMA.
    public :: dyn_import_t
    public :: dyn_export_t
    public :: dyn_readnl
    public :: dyn_init
    public :: dyn_run
    ! public :: dyn_final

    public :: dyn_debug_print
    public :: dyn_exchange_constituent_state
    public :: reverse
    public :: mpas_dynamical_core
    public :: ncells, ncells_solve, nedges, nedges_solve, nvertices, nvertices_solve, nvertlevels
    public :: ncells_global, nedges_global, nvertices_global, ncells_max, nedges_max
    public :: sphere_radius

    !> NOTE:
    !> This derived type is not used by MPAS dynamical core. It exists only as a placeholder because CAM-SIMA requires it.
    !> Developers/Maintainers/Users who wish to interact with MPAS dynamical core may do so by using the "instance/object"
    !> below.
    type :: dyn_import_t
    end type dyn_import_t

    !> NOTE:
    !> This derived type is not used by MPAS dynamical core. It exists only as a placeholder because CAM-SIMA requires it.
    !> Developers/Maintainers/Users who wish to interact with MPAS dynamical core may do so by using the "instance/object"
    !> below.
    type :: dyn_export_t
    end type dyn_export_t

    !> The "instance/object" of MPAS dynamical core.
    type(mpas_dynamical_core_type) :: mpas_dynamical_core

    ! Local and global mesh dimensions of MPAS dynamical core.
    integer :: ncells, ncells_solve, nedges, nedges_solve, nvertices, nvertices_solve, nvertlevels
    integer :: ncells_global, nedges_global, nvertices_global, ncells_max, nedges_max
    real(kind_r8) :: sphere_radius
contains
    !> Print a debug message with optionally the value(s) of a variable.
    !> If `printer` is not supplied, the MPI root rank will print. Otherwise, the designated MPI rank will print instead.
    !> (KCW, 2024-02-03)
    subroutine dyn_debug_print(message, variable, printer)
        character(*), intent(in) :: message
        class(*), optional, intent(in) :: variable(:)
        integer, optional, intent(in) :: printer

        ! Bail out early if debug output is not requested.
        if (.not. (debug_output > debugout_none)) then
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

        if (present(variable)) then
            write(iulog, '(a)') 'dyn_debug_print (' // stringify([iam]) // '): ' // &
                message // stringify(variable)
        else
            write(iulog, '(a)') 'dyn_debug_print (' // stringify([iam]) // '): ' // &
                message
        end if
    end subroutine dyn_debug_print

    !> Read MPAS namelist from supplied path.
    !> Additionally, perform early initialization of MPAS dynamical core.
    !> (KCW, 2024-02-09)
    !
    ! Called by `read_namelist` in `src/control/runtime_opts.F90`.
    subroutine dyn_readnl(namelist_path)
        character(*), intent(in) :: namelist_path

        character(*), parameter :: subname = 'dyn_comp::dyn_readnl'
        character(kind_cs) :: cam_calendar
        integer :: log_unit(2)
        integer :: start_date_time(6), & ! YYYY, MM, DD, hh, mm, ss.
                   stop_date_time(6),  & ! YYYY, MM, DD, hh, mm, ss.
                   run_duration(4),    & ! DD, hh, mm, ss.
                   sec_since_midnight    ! Second(s) since midnight.
        type(iosystem_desc_t), pointer :: pio_iosystem

        nullify(pio_iosystem)

        ! Enable/disable the debug output of MPAS dynamical core according to the debug verbosity level of CAM-SIMA.
        mpas_dynamical_core % debug_output = (debug_output > debugout_none)

        ! Get free units for MPAS so it can write its own log files, e.g., `log.atmosphere.0000.{out,err}`.
        log_unit(1) = shr_file_getunit()
        log_unit(2) = shr_file_getunit()

        ! Initialize MPAS framework with supplied MPI communicator group and log units.
        call mpas_dynamical_core % init_phase1(mpicom, endrun, iulog, log_unit)

        cam_calendar = timemgr_get_calendar_cf()

        call get_start_date(start_date_time(1), start_date_time(2), start_date_time(3), sec_since_midnight)
        start_date_time(4:6) = sec_to_hour_min_sec(sec_since_midnight)

        call get_stop_date(stop_date_time(1), stop_date_time(2), stop_date_time(3), sec_since_midnight)
        stop_date_time(4:6) = sec_to_hour_min_sec(sec_since_midnight)

        call get_run_duration(run_duration(1), sec_since_midnight)
        run_duration(2:4) = sec_to_hour_min_sec(sec_since_midnight)

        ! Read MPAS-related namelist variables from `namelist_path`, e.g., `atm_in`.
        call mpas_dynamical_core % read_namelist(namelist_path, &
            cam_calendar, start_date_time, stop_date_time, run_duration, initial_run)

        pio_iosystem => shr_pio_getiosys(atm_id)

        ! Initialize MPAS framework with supplied PIO system descriptor.
        call mpas_dynamical_core % init_phase2(pio_iosystem)

        nullify(pio_iosystem)
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
        use cam_thermo_formula,   only: energy_formula_dycore, ENERGY_FORMULA_DYCORE_MPAS
        use phys_vars_init_check, only: mark_as_initialized
        use physics_types,        only: dycore_energy_consistency_adjust

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

        nullify(pio_init_file)
        nullify(pio_topo_file)

        ! Set dynamical core energy formula for use in cam_thermo.
        energy_formula_dycore = ENERGY_FORMULA_DYCORE_MPAS
        call mark_as_initialized('total_energy_formula_for_dycore')

        ! Dynamical core energy is not consistent with CAM physics and requires
        ! temperature and temperature tendency adjustment at end of physics.
        dycore_energy_consistency_adjust = .true.
        call mark_as_initialized('flag_for_dycore_energy_consistency_adjustment')

        allocate(constituent_name(num_advected), stat=ierr)
        call check_allocate(ierr, subname, 'constituent_name(num_advected)', 'dyn_comp', __LINE__)

        allocate(is_water_species(num_advected), stat=ierr)
        call check_allocate(ierr, subname, 'is_water_species(num_advected)', 'dyn_comp', __LINE__)

        do i = 1, num_advected
            constituent_name(i) = const_name(i)
            is_water_species(i) = const_is_water_species(i)
        end do

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

        pio_init_file => initial_file_get_id()
        pio_topo_file => topo_file_get_id()

        if (initial_run) then
            ! Run type is initial run.

            call dyn_debug_print('Calling check_topography_data')

            call check_topography_data(pio_topo_file)

            if (analytic_ic_active()) then
                call dyn_debug_print('Calling set_analytic_initial_condition')

                call set_analytic_initial_condition()
            else
                ! Perform default initialization for all constituents.
                ! Subsequently, they can be overridden depending on the namelist option (below) and
                ! the actual availability (checked and handled by MPAS).
                call dyn_debug_print('Calling dyn_exchange_constituent_state')

                call dyn_exchange_constituent_state(direction='e', exchange=.true., conversion=.false.)

                ! Namelist option that controls if constituents are to be read from the file.
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

            ! Read variables that belong to the "input" and "restart" streams in MPAS.
            call mpas_dynamical_core % read_write_stream(pio_init_file, 'r', 'input+restart')
        end if

        call clean_iodesc_list()
        call mark_variable_as_initialized()

        nullify(pio_init_file)
        nullify(pio_topo_file)

        ! This is the time interval for dynamics-physics coupling in CAM-SIMA.
        ! Each time MPAS dynamical core is called to run, it will integrate with time for this specific interval,
        ! then yield control back to the caller.
        coupling_time_interval = get_step_size()

        ! Finish MPAS dynamical core initialization. After this point, MPAS dynamical core is ready for time integration.
        call mpas_dynamical_core % init_phase4(coupling_time_interval)
    end subroutine dyn_init

    !> Check for consistency in topography data. The presence of topography file is inferred from the `pio_file` pointer.
    !> If topography file is used, check that the `PHIS` variable, which denotes surface geopotential,
    !> is consistent with the surface geometric height in MPAS.
    !> Otherwise, if topography file is not used, check that the surface geometric height in MPAS is zero.
    !> (KCW, 2024-05-10)
    subroutine check_topography_data(pio_file)
        type(file_desc_t), pointer, intent(in) :: pio_file

        character(*), parameter :: subname = 'dyn_comp::check_topography_data'
        integer :: ierr
        logical :: success
        real(kind_r8), parameter :: error_tolerance = 1.0E-3_kind_r8 ! Error tolerance for consistency check.
        real(kind_r8), allocatable :: surface_geometric_height(:)    ! Computed from topography file.
        real(kind_r8), allocatable :: surface_geopotential(:)        ! Read from topography file.
        real(kind_r8), pointer :: zgrid(:, :)                        ! From MPAS. Geometric height (meters) at layer interfaces.

        nullify(zgrid)

        call mpas_dynamical_core % get_variable_pointer(zgrid, 'mesh', 'zgrid')

        if (associated(pio_file)) then
            call dyn_debug_print('Topography file is used')

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
            if (any(abs(zgrid(1, 1:ncells_solve) - surface_geometric_height) > error_tolerance)) then
                call endrun('Surface geometric height in MPAS is not consistent with topography data', subname, __LINE__)
            end if

            deallocate(surface_geopotential)
            deallocate(surface_geometric_height)
        else
            call dyn_debug_print('Topography file is not used')

            ! Surface geometric height in MPAS should be zero.
            if (any(abs(zgrid(1, 1:ncells_solve)) > error_tolerance)) then
                call endrun('Surface geometric height in MPAS is not zero', subname, __LINE__)
            end if
        end if

        nullify(zgrid)
    end subroutine check_topography_data

    !> Set analytic initial condition for MPAS.
    !> (KCW, 2024-05-22)
    subroutine set_analytic_initial_condition()
        character(*), parameter :: subname = 'dyn_comp::set_analytic_initial_condition'
        integer, allocatable :: global_grid_index(:)
        real(kind_r8), allocatable :: buffer_2d_real(:, :), buffer_3d_real(:, :, :)
        real(kind_r8), allocatable :: lat_rad(:), lon_rad(:)
        real(kind_r8), allocatable :: z_int(:, :) ! Geometric height (meters) at layer interfaces.
                                                  ! Dimension and vertical index orders follow CAM-SIMA convention.
        real(kind_r8), pointer :: zgrid(:, :)     ! Geometric height (meters) at layer interfaces.
                                                  ! Dimension and vertical index orders follow MPAS convention.

        call init_shared_variables()

        call set_mpas_state_u()
        call set_mpas_state_w()
        call set_mpas_state_scalars()
        call set_mpas_state_rho_theta()
        call set_mpas_state_rho_base_theta_base()

        call final_shared_variables()
    contains
        !> Initialize variables that are shared and repeatedly used by the `set_mpas_state_*` internal subroutines.
        !> (KCW, 2024-05-13)
        subroutine init_shared_variables()
            character(*), parameter :: subname = 'dyn_comp::set_analytic_initial_condition::init_shared_variables'
            integer :: ierr
            integer :: i
            integer, pointer :: indextocellid(:)
            real(kind_r8), pointer :: lat_deg(:), lon_deg(:)

            call dyn_debug_print('Preparing to set analytic initial condition')

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
                z_int(i, :) = reverse(zgrid(:, i))
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
            character(*), parameter :: subname = 'dyn_comp::set_analytic_initial_condition::set_mpas_state_u'
            integer :: ierr
            integer :: i
            real(kind_r8), pointer :: ucellzonal(:, :), ucellmeridional(:, :)

            call dyn_debug_print('Setting MPAS state "u"')

            nullify(ucellzonal, ucellmeridional)

            allocate(buffer_2d_real(ncells_solve, pver), stat=ierr)
            call check_allocate(ierr, subname, 'buffer_2d_real(ncells_solve, pver)', 'dyn_comp', __LINE__)

            call mpas_dynamical_core % get_variable_pointer(ucellzonal, 'diag', 'uReconstructZonal')
            call mpas_dynamical_core % get_variable_pointer(ucellmeridional, 'diag', 'uReconstructMeridional')

            buffer_2d_real(:, :) = 0.0_kind_r8

            call dyn_set_inic_col(vc_height, lat_rad, lon_rad, global_grid_index, zint=z_int, u=buffer_2d_real)

            ! Vertical index order is reversed between CAM-SIMA and MPAS.
            do i = 1, ncells_solve
                ucellzonal(:, i) = reverse(buffer_2d_real(i, :))
            end do

            buffer_2d_real(:, :) = 0.0_kind_r8

            call dyn_set_inic_col(vc_height, lat_rad, lon_rad, global_grid_index, zint=z_int, v=buffer_2d_real)

            ! Vertical index order is reversed between CAM-SIMA and MPAS.
            do i = 1, ncells_solve
                ucellmeridional(:, i) = reverse(buffer_2d_real(i, :))
            end do

            deallocate(buffer_2d_real)

            nullify(ucellzonal, ucellmeridional)

            call mpas_dynamical_core % compute_edge_wind(.false.)
        end subroutine set_mpas_state_u

        !> Set MPAS state `w` (i.e., vertical velocity at cell interfaces).
        !> (KCW, 2024-05-13)
        subroutine set_mpas_state_w()
            character(*), parameter :: subname = 'dyn_comp::set_analytic_initial_condition::set_mpas_state_w'
            real(kind_r8), pointer :: w(:, :)

            call dyn_debug_print('Setting MPAS state "w"')

            nullify(w)

            call mpas_dynamical_core % get_variable_pointer(w, 'state', 'w', time_level=1)

            w(:, 1:ncells_solve) = 0.0_kind_r8

            nullify(w)

            ! Because we are injecting data directly into MPAS memory, halo layers need to be updated manually.
            call mpas_dynamical_core % exchange_halo('w')
        end subroutine set_mpas_state_w

        !> Set MPAS state `scalars` (i.e., constituents).
        !> (KCW, 2024-05-17)
        subroutine set_mpas_state_scalars()
            ! CCPP standard name of `qv`, which denotes water vapor mixing ratio.
            character(*), parameter :: constituent_qv_standard_name = &
                'water_vapor_mixing_ratio_wrt_dry_air'

            character(*), parameter :: subname = 'dyn_comp::set_analytic_initial_condition::set_mpas_state_scalars'
            integer :: i, j
            integer :: ierr
            integer, allocatable :: constituent_index(:)
            integer, pointer :: index_qv
            real(kind_r8), pointer :: scalars(:, :, :)

            call dyn_debug_print('Setting MPAS state "scalars"')

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
                        reverse(buffer_3d_real(i, :, mpas_dynamical_core % map_constituent_index(j)))
                end do
            end do

            if (mpas_dynamical_core % get_constituent_name(mpas_dynamical_core % map_constituent_index(index_qv)) == &
                constituent_qv_standard_name) then
                ! The definition of `qv` matches exactly what MPAS wants. No conversion is needed.
                call dyn_debug_print('No conversion is needed for water vapor mixing ratio')
            else
                ! The definition of `qv` actually represents specific humidity. Conversion is needed.
                call dyn_debug_print('Conversion is needed and applied for water vapor mixing ratio')

                ! Convert specific humidity to water vapor mixing ratio.
                scalars(index_qv, :, 1:ncells_solve) = &
                    scalars(index_qv, :, 1:ncells_solve) / (1.0_kind_r8 - scalars(index_qv, :, 1:ncells_solve))
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
            real(kind_r8), pointer :: rho(:, :)
            real(kind_r8), pointer :: theta(:, :)
            real(kind_r8), pointer :: scalars(:, :, :)

            call dyn_debug_print('Setting MPAS state "rho" and "theta"')

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
                qv_mid_col(:) = scalars(index_qv, :, i)
                tm_mid_col(:) = t_mid(:, i) * (1.0_kind_r8 + constant_rv / constant_rd * qv_mid_col(:))

                ! Piecewise integrate hypsometric equation to derive `p_mid_col(1)`.
                ! The formulation used here is exact.
                p_mid_col(1) = p_by_hypsometric_equation( &
                    p_sfc(i), &
                    zgrid(1, i), &
                    tm_mid_col(1) / (1.0_kind_r8 + qv_mid_col(1)), &
                    0.5_kind_r8 * (zgrid(2, i) + zgrid(1, i)))

                ! Piecewise integrate hypsometric equation to derive subsequent `p_mid_col(k)`.
                ! The formulation used here is exact.
                do k = 2, pver
                    p_mid_col(k) = p_by_hypsometric_equation( &
                        p_by_hypsometric_equation( &
                            p_mid_col(k - 1), &
                            0.5_kind_r8 * (zgrid(k, i) + zgrid(k - 1, i)), &
                            tm_mid_col(k - 1) / (1.0_kind_r8 + qv_mid_col(k - 1)), &
                            zgrid(k, i)), &
                        zgrid(k, i), &
                        tm_mid_col(k) / (1.0_kind_r8 + qv_mid_col(k)), &
                        0.5_kind_r8 * (zgrid(k + 1, i) + zgrid(k, i)))
                end do

                rho(:, i) = p_mid_col(:) / (constant_rd * tm_mid_col(:))
                theta(:, i) = theta_by_poisson_equation(p_mid_col, t_mid(:, i), constant_p0)
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
            character(*), parameter :: subname = 'dyn_comp::set_analytic_initial_condition::set_mpas_state_rho_base_theta_base'
            integer :: i, k
            integer :: ierr
            real(kind_r8), parameter :: t_base = 250.0_kind_r8 ! Base state temperature (K) of dry isothermal atmosphere.
                                                               ! The value used here is identical to MPAS.
            real(kind_r8), allocatable :: p_base(:)            ! Base state pressure (Pa) at layer midpoints of each column.
            real(kind_r8), pointer :: rho_base(:, :)
            real(kind_r8), pointer :: theta_base(:, :)
            real(kind_r8), pointer :: zz(:, :)

            call dyn_debug_print('Setting MPAS state "rho_base" and "theta_base"')

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
                        0.5_kind_r8 * (zgrid(k + 1, i) + zgrid(k, i)))
                end do

                rho_base(:, i) = p_base(:) / (constant_rd * t_base * zz(:, i))
                theta_base(:, i) = theta_by_poisson_equation(p_base, t_base, constant_p0)
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
        !> `t_v` is the mean virtual temperature between `z_1` and `z_2`.
        !> (KCW, 2024-07-02)
        pure elemental function p_by_hypsometric_equation(p_1, z_1, t_v, z_2) result(p_2)
            real(kind_r8), intent(in) :: p_1, z_1, t_v, z_2
            real(kind_r8) :: p_2

            p_2 = p_1 * exp(-(z_2 - z_1) * constant_g / (constant_rd * t_v))
        end function p_by_hypsometric_equation

        ! ----- p_1, t_1 ----- (Arbitrary layer)
        !
        ! ----- p_0, t_0 ----- (Reference layer)
        !
        !> Compute the potential temperature `t_0` at reference pressure `p_0` from the temperature `t_1` at pressure `p_1` by
        !> Poisson equation.
        !> (KCW, 2024-07-02)
        pure elemental function theta_by_poisson_equation(p_1, t_1, p_0) result(t_0)
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
    !> If `conversion` is `.true.`, appropriate conversion is performed for constituent mixing ratio that has different
    !> definitions between CAM-SIMA and MPAS (i.e., dry/moist).
    !> Otherwise, if `conversion` is `.false.`, no conversion is performed at all.
    !> This subroutine is intentionally designed to have these elaborate controls due to complications in CAM-SIMA.
    !> Some procedures in CAM-SIMA expect constituent states to be dry, while the others expect them to be moist.
    !> (KCW, 2024-09-26)
    subroutine dyn_exchange_constituent_state(direction, exchange, conversion)
        character(*), intent(in) :: direction
        logical, intent(in) :: exchange
        logical, intent(in) :: conversion

        character(*), parameter :: subname = 'dyn_comp::dyn_exchange_constituent_state'
        integer :: i, j
        integer :: ierr
        integer, allocatable :: is_water_species_index(:)
        logical, allocatable :: is_conversion_needed(:)
        logical, allocatable :: is_water_species(:)
        real(kind_phys), pointer :: constituents(:, :, :) ! This points to CCPP memory.
        real(kind_r8), allocatable :: sigma_all_q(:)      ! Summation of all water species mixing ratios.
        real(kind_r8), pointer :: scalars(:, :, :)        ! This points to MPAS memory.

        select case (trim(adjustl(direction)))
            case ('e', 'export')
                if (exchange) then
                    call dyn_debug_print('Setting MPAS state "scalars" from physics state "constituents"')
                end if

                if (conversion) then
                    call dyn_debug_print('Converting MPAS state "scalars"')
                end if
            case ('i', 'import')
                if (exchange) then
                    call dyn_debug_print('Setting physics state "constituents" from MPAS state "scalars"')
                end if

                if (conversion) then
                    call dyn_debug_print('Converting physics state "constituents"')
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
                            reverse(constituents(i, :, mpas_dynamical_core % map_constituent_index(j)))
                    end if

                    if (conversion .and. is_conversion_needed(mpas_dynamical_core % map_constituent_index(j))) then
                        ! Equation 8 in doi:10.1029/2017MS001257.
                        scalars(j, :, i) = &
                            scalars(j, :, i) * sigma_all_q(:)
                    end if
                end do
            end do
        else
            is_water_species_index(:) = &
                pack([(mpas_dynamical_core % map_mpas_scalar_index(i), i = 1, num_advected)], is_water_species)

            do i = 1, ncells_solve
                if (conversion .and. any(is_conversion_needed)) then
                    ! The summation term of equation 8 in doi:10.1029/2017MS001257.
                    sigma_all_q(:) = reverse(1.0_kind_r8 + sum(scalars(is_water_species_index, :, i), 1))
                end if

                ! `j` is indexing into `constituents`, so it is regarded as constituent index.
                do j = 1, num_advected
                    if (exchange) then
                        ! Vertical index order is reversed between CAM-SIMA and MPAS.
                        constituents(i, :, j) = &
                            reverse(scalars(mpas_dynamical_core % map_mpas_scalar_index(j), :, i))
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
    end subroutine dyn_exchange_constituent_state

    !> Mark everything in the `physics_{state,tend}` derived types along with constituents as initialized
    !> to prevent physics from attempting to read them from a file. These variables are to be exchanged later
    !> during dynamics-physics coupling.
    !> (KCW, 2024-05-23)
    subroutine mark_variable_as_initialized()
        character(*), parameter :: subname = 'dyn_comp::mark_variable_as_initialized'
        integer :: i

        ! CCPP standard names of physical quantities in the `physics_{state,tend}` derived types.
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

        call mark_as_initialized('specific_heat_of_air_used_in_dycore')

        ! These energy variables are calculated by check_energy_timestep_init
        ! but need to be marked here
        call mark_as_initialized('vertically_integrated_total_energy_at_end_of_physics_timestep')
        call mark_as_initialized('vertically_integrated_total_energy_using_dycore_energy_formula')
        call mark_as_initialized('vertically_integrated_total_energy_using_dycore_energy_formula_at_start_of_physics_timestep')
        call mark_as_initialized('vertically_integrated_total_energy_using_physics_energy_formula')
        call mark_as_initialized('vertically_integrated_total_energy_using_physics_energy_formula_at_start_of_physics_timestep')
        call mark_as_initialized('vertically_integrated_total_water')
        call mark_as_initialized('vertically_integrated_total_water_at_start_of_physics_timestep')
    end subroutine mark_variable_as_initialized

    !> Run MPAS dynamical core to integrate the dynamical states with time.
    !> (KCW, 2024-07-11)
    subroutine dyn_run()
        character(*), parameter :: subname = 'dyn_comp::dyn_run'

        ! MPAS dynamical core will run until the coupling time interval is reached.
        call mpas_dynamical_core % run()
    end subroutine dyn_run

    ! Not used for now. Intended to be called by `stepon_final` in `src/dynamics/mpas/stepon.F90`.
    ! subroutine dyn_final()
    ! end subroutine dyn_final

    !> Helper function for reversing the order of elements in `array`.
    !> (KCW, 2024-07-17)
    pure function reverse(array)
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
