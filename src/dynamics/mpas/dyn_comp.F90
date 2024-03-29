module dyn_comp
    use dyn_mpas_subdriver, only: mpas_dynamical_core_type

    ! Modules from CAM.
    use cam_abortutils, only: endrun
    use cam_control_mod, only: initial_run
    use cam_instance, only: atm_id
    use cam_logfile, only: debug_output, debugout_none, iulog
    use runtime_obj, only: runtime_options
    use spmd_utils, only: iam, masterproc, mpicom
    use string_utils, only: stringify
    use time_manager, only: get_start_date, get_stop_date, get_run_duration, timemgr_get_calendar_cf

    ! Modules from CESM Share.
    use shr_file_mod, only: shr_file_getunit
    use shr_kind_mod, only: shr_kind_cs
    use shr_pio_mod, only: shr_pio_getiosys

    ! Modules from external libraries.
    use pio, only: iosystem_desc_t

    implicit none

    private
    ! Provide APIs required by CAM Control.
    public :: dyn_import_t
    public :: dyn_export_t
    public :: dyn_readnl
    public :: dyn_init
    ! public :: dyn_run
    ! public :: dyn_final

    public :: dyn_debug_print
    public :: mpas_dynamical_core

    type :: dyn_import_t
    end type dyn_import_t

    type :: dyn_export_t
    end type dyn_export_t

    !> The "instance/object" of MPAS dynamical core.
    type(mpas_dynamical_core_type) :: mpas_dynamical_core
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
        character(shr_kind_cs) :: cam_calendar
        integer :: log_unit(2)
        integer :: start_date_time(6), & ! YYYY, MM, DD, hh, mm, ss.
                   stop_date_time(6),  & ! YYYY, MM, DD, hh, mm, ss.
                   run_duration(4),    & ! DD, hh, mm, ss.
                   sec_since_midnight    ! Second(s) since midnight.
        type(iosystem_desc_t), pointer :: pio_iosystem => null()

        ! Enable/disable the debug output of MPAS dynamical core according to the debug verbosity level of CAM-SIMA.
        mpas_dynamical_core % debug_output = (debug_output > debugout_none)

        ! Get free units for MPAS so it can write its own log files, e.g., `log.atmosphere.0000.{out,err}`.
        log_unit(1) = shr_file_getunit()
        log_unit(2) = shr_file_getunit()

        ! Initialize MPAS framework with supplied MPI communicator group and log units.
        ! See comment blocks in `src/dynamics/mpas/driver/dyn_mpas_subdriver.F90` for details.
        call mpas_dynamical_core % init_phase1(mpicom, endrun, iulog, log_unit)

        cam_calendar = timemgr_get_calendar_cf()

        call get_start_date(start_date_time(1), start_date_time(2), start_date_time(3), sec_since_midnight)
        start_date_time(4:6) = sec_to_hour_min_sec(sec_since_midnight)

        call get_stop_date(stop_date_time(1), stop_date_time(2), stop_date_time(3), sec_since_midnight)
        stop_date_time(4:6) = sec_to_hour_min_sec(sec_since_midnight)

        call get_run_duration(run_duration(1), sec_since_midnight)
        run_duration(2:4) = sec_to_hour_min_sec(sec_since_midnight)

        ! Read MPAS-related namelist variables from `namelist_path`, e.g., `atm_in`.
        ! See comment blocks in `src/dynamics/mpas/driver/dyn_mpas_subdriver.F90` for details.
        call mpas_dynamical_core % read_namelist(namelist_path, &
            cam_calendar, start_date_time, stop_date_time, run_duration, initial_run)

        pio_iosystem => shr_pio_getiosys(atm_id)

        ! Initialize MPAS framework with supplied PIO system descriptor.
        ! See comment blocks in `src/dynamics/mpas/driver/dyn_mpas_subdriver.F90` for details.
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

    ! Called by `cam_init` in `src/control/cam_comp.F90`.
    subroutine dyn_init(cam_runtime_opts, dyn_in, dyn_out)
        type(runtime_options), intent(in)  :: cam_runtime_opts
        type(dyn_import_t),    intent(out) :: dyn_in
        type(dyn_export_t),    intent(out) :: dyn_out
    end subroutine dyn_init

    ! Not used for now. Intended to be called by `stepon_run*` in `src/dynamics/mpas/stepon.F90`.
    ! subroutine dyn_run()
    ! end subroutine dyn_run

    ! Not used for now. Intended to be called by `stepon_final` in `src/dynamics/mpas/stepon.F90`.
    ! subroutine dyn_final()
    ! end subroutine dyn_final
end module dyn_comp
