module dyn_mpas_subdriver
    !-------------------------------------------------------------------------------
    ! module dyn_mpas_subdriver
    !
    ! This module manages the life cycle (i.e., initialization, running, and
    ! finalization) of MPAS as a dynamical core within CAM-SIMA.
    !
    !-------------------------------------------------------------------------------

    use, intrinsic :: iso_fortran_env, only: output_unit

    ! Modules from external libraries.
    use mpi, only: mpi_comm_null, mpi_comm_rank, mpi_success
    use pio, only: iosystem_desc_t, pio_iosystem_is_active

    ! Modules from MPAS.
    use atm_core_interface, only: atm_setup_core, atm_setup_domain
    use mpas_derived_types, only: core_type, domain_type
    use mpas_domain_routines, only: mpas_allocate_domain
    use mpas_framework, only: mpas_framework_init_phase1, mpas_framework_init_phase2
    use mpas_kind_types, only: strkind
    use mpas_pool_routines, only: mpas_pool_get_config

    implicit none

    private
    public :: mpas_dynamical_core_type

    abstract interface
        ! This interface is compatible with `endrun` from CAM-SIMA.
        subroutine model_error_if(message, file, line)
            character(*),           intent(in) :: message
            character(*), optional, intent(in) :: file
            integer,      optional, intent(in) :: line
        end subroutine model_error_if
    end interface

    !> The "class" of MPAS dynamical core.
    !> Important data structures like states of MPAS dynamical core are encapsulated inside this derived type to prevent misuse.
    !> Type-bound procedures provide well-defined APIs for CAM-SIMA to interact with MPAS dynamical core.
    type :: mpas_dynamical_core_type
        private

        integer :: log_unit = output_unit
        integer :: mpi_comm = mpi_comm_null
        integer :: mpi_rank = 0
        logical :: mpi_rank_root = .false.

        ! Actual implementation is supplied at runtime.
        procedure(model_error_if), nopass, pointer :: model_error => null()

        type(core_type), pointer :: corelist => null()
        type(domain_type), pointer :: domain_ptr => null()
    contains
        private

        procedure, pass, public :: debug_print => dyn_mpas_debug_print
        procedure, pass, public :: init_phase1 => dyn_mpas_init_phase1
        procedure, pass, public :: read_namelist => dyn_mpas_read_namelist
        procedure, pass, public :: init_phase2 => dyn_mpas_init_phase2
    end type mpas_dynamical_core_type
contains
    !> Print a debug message with optionally the value(s) of a variable.
    !> If `printer` is not supplied, the MPI root rank will print. Otherwise, the designated MPI rank will print instead.
    !> (KCW, 2024-02-03)
    subroutine dyn_mpas_debug_print(self, message, variable, printer)
        class(mpas_dynamical_core_type), intent(in) :: self
        character(*), intent(in) :: message
        class(*), optional, intent(in) :: variable(:)
        integer, optional, intent(in) :: printer

#ifdef DEBUG
        if (present(printer)) then
            if (self % mpi_rank /= printer) then
                return
            end if
        else
            if (.not. self % mpi_rank_root) then
                return
            end if
        end if

        if (present(variable)) then
            write(self % log_unit, '(a)') 'dyn_mpas_debug_print (' // stringify([self % mpi_rank]) // '): ' // &
                message // stringify(variable)
        else
            write(self % log_unit, '(a)') 'dyn_mpas_debug_print (' // stringify([self % mpi_rank]) // '): ' // &
                message
        end if
#endif
    end subroutine dyn_mpas_debug_print

    !> Convert one or more values of any intrinsic data types to a character string for pretty printing.
    !> If `value` contains more than one element, the elements will be stringified, delimited by `separator`, then concatenated.
    !> If `value` contains exactly one element, the element will be stringified without using `separator`.
    !> If `value` contains zero element or is of unsupported data types, an empty character string is produced.
    !> If `separator` is not supplied, it defaults to `, ` (i.e., a comma and a space).
    !> (KCW, 2024-02-04)
    pure function stringify(value, separator)
        use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64

        class(*), intent(in) :: value(:)
        character(*), optional, intent(in) :: separator
        character(:), allocatable :: stringify

        integer, parameter :: sizelimit = 1024

        character(:), allocatable :: buffer, delimiter, format
        integer :: i, n, offset

        if (present(separator)) then
            delimiter = separator
        else
            delimiter = ', '
        end if

        n = min(size(value), sizelimit)

        if (n == 0) then
            stringify = ''

            return
        end if

        select type (value)
            type is (character(*))
                allocate(character(len(value) * n + len(delimiter) * (n - 1)) :: buffer)

                buffer(:) = ''
                offset = 0

                do i = 1, n
                    if (len(delimiter) > 0 .and. i > 1) then
                        buffer(offset + 1:offset + len(delimiter)) = delimiter
                        offset = offset + len(delimiter)
                    end if

                    if (len_trim(adjustl(value(i))) > 0) then
                        buffer(offset + 1:offset + len_trim(adjustl(value(i)))) = trim(adjustl(value(i)))
                        offset = offset + len_trim(adjustl(value(i)))
                    end if
                end do
            type is (integer(int32))
                allocate(character(11 * n + len(delimiter) * (n - 1)) :: buffer)
                allocate(character(17 + len(delimiter) + floor(log10(real(n))) + 1) :: format)

                write(format, '(a, i0, 3a)') '(ss, ', n, '(i0, :, "', delimiter, '"))'
                write(buffer, format) value
            type is (integer(int64))
                allocate(character(20 * n + len(delimiter) * (n - 1)) :: buffer)
                allocate(character(17 + len(delimiter) + floor(log10(real(n))) + 1) :: format)

                write(format, '(a, i0, 3a)') '(ss, ', n, '(i0, :, "', delimiter, '"))'
                write(buffer, format) value
            type is (logical)
                allocate(character(1 * n + len(delimiter) * (n - 1)) :: buffer)
                allocate(character(13 + len(delimiter) + floor(log10(real(n))) + 1) :: format)

                write(format, '(a, i0, 3a)') '(', n, '(l1, :, "', delimiter, '"))'
                write(buffer, format) value
            type is (real(real32))
                allocate(character(13 * n + len(delimiter) * (n - 1)) :: buffer)

                if (maxval(abs(value)) < 1.0e5_real32) then
                    allocate(character(20 + len(delimiter) + floor(log10(real(n))) + 1) :: format)
                    write(format, '(a, i0, 3a)') '(ss, ', n, '(f13.6, :, "', delimiter, '"))'
                else
                    allocate(character(23 + len(delimiter) + floor(log10(real(n))) + 1) :: format)
                    write(format, '(a, i0, 3a)') '(ss, ', n, '(es13.6e2, :, "', delimiter, '"))'
                end if

                write(buffer, format) value
            type is (real(real64))
                allocate(character(13 * n + len(delimiter) * (n - 1)) :: buffer)

                if (maxval(abs(value)) < 1.0e5_real64) then
                    allocate(character(20 + len(delimiter) + floor(log10(real(n))) + 1) :: format)
                    write(format, '(a, i0, 3a)') '(ss, ', n, '(f13.6, :, "', delimiter, '"))'
                else
                    allocate(character(23 + len(delimiter) + floor(log10(real(n))) + 1) :: format)
                    write(format, '(a, i0, 3a)') '(ss, ', n, '(es13.6e2, :, "', delimiter, '"))'
                end if

                write(buffer, format) value
            class default
                stringify = ''

                return
        end select

        stringify = trim(buffer)
    end function stringify

    !-------------------------------------------------------------------------------
    ! subroutine dyn_mpas_init_phase1
    !
    !> \brief  Tracks `mpas_init` up to the point of reading namelist
    !> \author Michael Duda
    !> \date   19 April 2019
    !> \details
    !>  This subroutine follows the stand-alone MPAS subdriver up to, but not
    !>  including, the point where namelist is read.
    !> \addenda
    !>  Ported and refactored for CAM-SIMA. (KCW, 2024-02-02)
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_init_phase1(self, mpi_comm, model_error_impl, log_unit, mpas_log_unit)
        class(mpas_dynamical_core_type), intent(inout) :: self
        integer, intent(in) :: mpi_comm
        procedure(model_error_if) :: model_error_impl
        integer, intent(in) :: log_unit
        integer, intent(in) :: mpas_log_unit(2)

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_init_phase1'
        integer :: ierr

        self % mpi_comm = mpi_comm
        self % model_error => model_error_impl

        if (self % mpi_comm == mpi_comm_null) then
            call self % model_error('Invalid MPI communicator group', subname, __LINE__)
        end if

        call mpi_comm_rank(self % mpi_comm, self % mpi_rank, ierr)

        if (ierr /= mpi_success) then
            call self % model_error('Invalid MPI communicator group', subname, __LINE__)
        end if

        self % mpi_rank_root = (self % mpi_rank == 0)
        self % log_unit = log_unit

        call self % debug_print(subname // ' entered')

        call self % debug_print('Allocating core')

        allocate(self % corelist, stat=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to allocate corelist', subname, __LINE__)
        end if

        nullify(self % corelist % next)

        call self % debug_print('Allocating domain')

        allocate(self % corelist % domainlist, stat=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to allocate corelist % domainlist', subname, __LINE__)
        end if

        nullify(self % corelist % domainlist % next)

        self % domain_ptr => self % corelist % domainlist
        self % domain_ptr % core => self % corelist

        call mpas_allocate_domain(self % domain_ptr)

        self % domain_ptr % domainid = 0

        call self % debug_print('Calling mpas_framework_init_phase1')

        ! Initialize MPAS framework with supplied MPI communicator group.
        call mpas_framework_init_phase1(self % domain_ptr % dminfo, mpi_comm=self % mpi_comm)

        call self % debug_print('Setting up core')

        call atm_setup_core(self % corelist)

        call self % debug_print('Setting up domain')

        call atm_setup_domain(self % domain_ptr)

        call self % debug_print('Setting up log')

        ! Set up the log manager as early as possible so we can use it for any errors/messages during subsequent
        ! initialization steps.
        !
        ! We need:
        ! 1) `domain_ptr` to be allocated;
        ! 2) `dmpar_init` to be completed for accessing `dminfo`;
        ! 3) `*_setup_core` to assign the `setup_log` function pointer.
        ierr = self % domain_ptr % core % setup_log(self % domain_ptr % loginfo, self % domain_ptr, unitnumbers=mpas_log_unit)

        if (ierr /= 0) then
            call self % model_error('Failed to setup log for MPAS', subname, __LINE__)
        end if

        ! At this point, we should be ready to read namelist in `dyn_comp::dyn_readnl`.
        call self % debug_print(subname // ' completed')
    end subroutine dyn_mpas_init_phase1

    !-------------------------------------------------------------------------------
    ! subroutine dyn_mpas_read_namelist
    !
    !> \brief  Tracks `mpas_init` where namelist is being read
    !> \author Kuan-Chih Wang
    !> \date   2024-02-09
    !> \details
    !>  This subroutine calls upstream MPAS functionality for reading its own
    !>  namelist. After that, override designated namelist variables according to
    !>  information provided from CAM-SIMA.
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_read_namelist(self, namelist_path, &
            cf_calendar, start_date_time, stop_date_time, run_duration, initial_run)
        class(mpas_dynamical_core_type), intent(inout) :: self
        character(*), intent(in) :: namelist_path, cf_calendar
        integer, intent(in) :: start_date_time(6), & ! YYYY, MM, DD, hh, mm, ss.
                               stop_date_time(6),  & ! YYYY, MM, DD, hh, mm, ss.
                               run_duration(4)       ! DD, hh, mm, ss.
        logical, intent(in) :: initial_run

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_read_namelist'
        character(strkind) :: mpas_calendar
        character(strkind), pointer :: config_value_c => null()
        integer :: ierr
        logical, pointer :: config_value_l => null()

        call self % debug_print(subname // ' entered')

        call self % debug_print('Reading namelist at ', [namelist_path])

        ! Override namelist filename so that we can rely on upstream MPAS functionality for reading its own namelist.
        ! The case of missing namelist groups (i.e., `iostat == iostat_end` or `iostat == iostat_eor`) will be handled gracefully.
        ! All namelist variables will have reasonable default values even if they are missing.
        self % domain_ptr % namelist_filename = trim(adjustl(namelist_path))

        ierr = self % domain_ptr % core % setup_namelist( &
            self % domain_ptr % configs, self % domain_ptr % namelist_filename, self % domain_ptr % dminfo)

        if (ierr /= 0) then
            call self % model_error('Namelist setup failed for core ' // trim(self % domain_ptr % core % corename), &
                subname, __LINE__)
        end if

        ! Override designated namelist variables according to information provided from CAM-SIMA.
        ! These include runtime settings that cannot be determined beforehand.

        call self % debug_print('Overriding designated namelist variables')

        ! CAM-SIMA seems to follow "NetCDF Climate and Forecast (CF) Metadata Conventions" for calendar names. See
        ! CF-1.11, section "4.4.1. Calendar".
        ! However, this is not the case for MPAS. Translate calendar names between CF and MPAS.
        select case (trim(adjustl(cf_calendar)))
            case ('360_day')
                mpas_calendar = '360day'
            case ('365_day', 'noleap')
                mpas_calendar = 'gregorian_noleap'
            case ('gregorian', 'standard')
                ! `gregorian` is a deprecated alternative name for `standard`.
                mpas_calendar = 'gregorian'
            case default
                call self % model_error('Unsupported calendar type "' // trim(adjustl(cf_calendar)) // '"', &
                    subname, __LINE__)
        end select

        call mpas_pool_get_config(self % domain_ptr % configs, 'config_calendar_type', config_value_c)
        config_value_c = trim(adjustl(mpas_calendar))

        call self % debug_print('config_calendar_type = ', [config_value_c])

        nullify(config_value_c)

        ! MPAS represents date and time in ISO 8601 format. However, the separator between date and time is `_`
        ! instead of standard `T`.
        ! Format in `YYYY-MM-DD_hh:mm:ss` is acceptable.
        call mpas_pool_get_config(self % domain_ptr % configs, 'config_start_time', config_value_c)
        config_value_c = stringify(start_date_time(1:3), '-') // '_' // stringify(start_date_time(4:6), ':')

        call self % debug_print('config_start_time = ', [config_value_c])

        nullify(config_value_c)

        call mpas_pool_get_config(self % domain_ptr % configs, 'config_stop_time', config_value_c)
        config_value_c = stringify(stop_date_time(1:3), '-') // '_' // stringify(stop_date_time(4:6), ':')

        call self % debug_print('config_stop_time = ', [config_value_c])

        nullify(config_value_c)

        ! Format in `DD_hh:mm:ss` is acceptable.
        call mpas_pool_get_config(self % domain_ptr % configs, 'config_run_duration', config_value_c)
        config_value_c = stringify([run_duration(1)]) // '_' // stringify(run_duration(2:4), ':')

        call self % debug_print('config_run_duration = ', [config_value_c])

        nullify(config_value_c)

        ! Reflect current run type to MPAS.
        if (initial_run) then
            ! Run type is initial run.
            call mpas_pool_get_config(self % domain_ptr % configs, 'config_do_restart', config_value_l)
            config_value_l = .false.
        else
            ! Run type is branch or restart run.
            call mpas_pool_get_config(self % domain_ptr % configs, 'config_do_restart', config_value_l)
            config_value_l = .true.
        end if

        call self % debug_print('config_do_restart = ', [config_value_l])

        nullify(config_value_l)

        call self % debug_print(subname // ' completed')
    end subroutine dyn_mpas_read_namelist

    !-------------------------------------------------------------------------------
    ! subroutine dyn_mpas_init_phase2
    !
    !> \brief  Tracks `mpas_init` after namelist has been read
    !> \author Michael Duda
    !> \date   19 April 2019
    !> \details
    !>  This subroutine follows the stand-alone MPAS subdriver from the point
    !>  where we call the second phase of MPAS framework initialization up
    !>  to the check on the existence of the streams.<core> file.
    !> \addenda
    !>  Ported and refactored for CAM-SIMA. (KCW, 2024-02-07)
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_init_phase2(self, pio_iosystem)
        class(mpas_dynamical_core_type), intent(inout) :: self
        type(iosystem_desc_t), pointer, intent(in) :: pio_iosystem

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_init_phase2'
        integer :: ierr
        logical :: pio_iosystem_active

        call self % debug_print(subname // ' entered')

        call self % debug_print('Checking PIO system descriptor')

        if (.not. associated(pio_iosystem)) then
            call self % model_error('Invalid PIO system descriptor', subname, __LINE__)
        end if

        call pio_iosystem_is_active(pio_iosystem, pio_iosystem_active)

        if (.not. pio_iosystem_active) then
            call self % model_error('Invalid PIO system descriptor', subname, __LINE__)
        end if

        call self % debug_print('Calling mpas_framework_init_phase2')

        ! Initialize MPAS framework with supplied PIO system descriptor.
        call mpas_framework_init_phase2(self % domain_ptr, io_system=pio_iosystem)

        ierr = self % domain_ptr % core % define_packages(self % domain_ptr % packages)

        if (ierr /= 0) then
            call self % model_error('Package definition failed for core ' // trim(self % domain_ptr % core % corename), &
                subname, __LINE__)
        end if

        ierr = self % domain_ptr % core % setup_packages( &
            self % domain_ptr % configs, self % domain_ptr % packages, self % domain_ptr % iocontext)

        if (ierr /= 0) then
            call self % model_error('Package setup failed for core ' // trim(self % domain_ptr % core % corename), &
                subname, __LINE__)
        end if

        ierr = self % domain_ptr % core % setup_decompositions(self % domain_ptr % decompositions)

        if (ierr /= 0) then
            call self % model_error('Decomposition setup failed for core ' // trim(self % domain_ptr % core % corename), &
                subname, __LINE__)
        end if

        ierr = self % domain_ptr % core % setup_clock(self % domain_ptr % clock, self % domain_ptr % configs)

        if (ierr /= 0) then
            call self % model_error('Clock setup failed for core ' // trim(self % domain_ptr % core % corename), &
                subname, __LINE__)
        end if

        ! At this point, we should be ready to set up decompositions, build halos, allocate blocks, etc.
        ! in `dyn_grid::model_grid_init`.
        call self % debug_print(subname // ' completed')
    end subroutine dyn_mpas_init_phase2
end module dyn_mpas_subdriver
