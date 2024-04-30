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
    use pio, only: file_desc_t, iosystem_desc_t, pio_file_is_open, pio_iosystem_is_active, &
                   pio_inq_varid, pio_inq_varndims, pio_noerr

    ! Modules from MPAS.
    use atm_core_interface, only: atm_setup_core, atm_setup_domain
    use mpas_bootstrapping, only: mpas_bootstrap_framework_phase1, mpas_bootstrap_framework_phase2
    use mpas_derived_types, only: core_type, domain_type, &
                                  mpas_pool_type, mpas_pool_field_info_type, &
                                  mpas_pool_character, mpas_pool_real, mpas_pool_integer, &
                                  mpas_stream_type, mpas_stream_noerr, &
                                  mpas_io_native_precision, mpas_io_pnetcdf, mpas_io_read, mpas_io_write, &
                                  field0dchar, field1dchar, &
                                  field0dinteger, field1dinteger, field2dinteger, field3dinteger, &
                                  field0dreal, field1dreal, field2dreal, field3dreal, field4dreal, field5dreal
    use mpas_dmpar, only: mpas_dmpar_exch_halo_field, &
                          mpas_dmpar_max_int, mpas_dmpar_sum_int
    use mpas_domain_routines, only: mpas_allocate_domain
    use mpas_framework, only: mpas_framework_init_phase1, mpas_framework_init_phase2
    use mpas_io_streams, only: mpas_createstream, mpas_closestream, mpas_streamaddfield, &
                               mpas_readstream, mpas_writestream, mpas_writestreamatt
    use mpas_kind_types, only: rkind, strkind
    use mpas_pool_routines, only: mpas_pool_create_pool, mpas_pool_destroy_pool, mpas_pool_get_subpool, &
                                  mpas_pool_add_config, mpas_pool_get_config, &
                                  mpas_pool_get_array, &
                                  mpas_pool_get_dimension, &
                                  mpas_pool_get_field, mpas_pool_get_field_info
    use mpas_stream_manager, only: postread_reindex, prewrite_reindex, postwrite_reindex

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

        logical, public :: debug_output = .false.

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
        procedure, pass, public :: init_phase3 => dyn_mpas_init_phase3
        procedure, pass, public :: read_write_stream => dyn_mpas_read_write_stream
        procedure, pass :: init_stream_with_pool => dyn_mpas_init_stream_with_pool
        procedure, pass, public :: exchange_halo => dyn_mpas_exchange_halo

        ! Accessor subroutines for users to access internal states of MPAS dynamical core.

        procedure, pass, public :: get_global_mesh_dimension => dyn_mpas_get_global_mesh_dimension

        procedure, pass :: get_pool_pointer => dyn_mpas_get_pool_pointer

        procedure, pass :: get_variable_pointer_c0 => dyn_mpas_get_variable_pointer_c0
        procedure, pass :: get_variable_pointer_c1 => dyn_mpas_get_variable_pointer_c1
        procedure, pass :: get_variable_pointer_i0 => dyn_mpas_get_variable_pointer_i0
        procedure, pass :: get_variable_pointer_i1 => dyn_mpas_get_variable_pointer_i1
        procedure, pass :: get_variable_pointer_i2 => dyn_mpas_get_variable_pointer_i2
        procedure, pass :: get_variable_pointer_i3 => dyn_mpas_get_variable_pointer_i3
        procedure, pass :: get_variable_pointer_l0 => dyn_mpas_get_variable_pointer_l0
        procedure, pass :: get_variable_pointer_r0 => dyn_mpas_get_variable_pointer_r0
        procedure, pass :: get_variable_pointer_r1 => dyn_mpas_get_variable_pointer_r1
        procedure, pass :: get_variable_pointer_r2 => dyn_mpas_get_variable_pointer_r2
        procedure, pass :: get_variable_pointer_r3 => dyn_mpas_get_variable_pointer_r3
        procedure, pass :: get_variable_pointer_r4 => dyn_mpas_get_variable_pointer_r4
        procedure, pass :: get_variable_pointer_r5 => dyn_mpas_get_variable_pointer_r5
        generic, public :: get_variable_pointer => get_variable_pointer_c0, get_variable_pointer_c1, &
                                                   get_variable_pointer_i0, get_variable_pointer_i1, &
                                                   get_variable_pointer_i2, get_variable_pointer_i3, &
                                                   get_variable_pointer_l0, &
                                                   get_variable_pointer_r0, get_variable_pointer_r1, &
                                                   get_variable_pointer_r2, get_variable_pointer_r3, &
                                                   get_variable_pointer_r4, get_variable_pointer_r5
        procedure, pass :: get_variable_value_c0 => dyn_mpas_get_variable_value_c0
        procedure, pass :: get_variable_value_c1 => dyn_mpas_get_variable_value_c1
        procedure, pass :: get_variable_value_i0 => dyn_mpas_get_variable_value_i0
        procedure, pass :: get_variable_value_i1 => dyn_mpas_get_variable_value_i1
        procedure, pass :: get_variable_value_i2 => dyn_mpas_get_variable_value_i2
        procedure, pass :: get_variable_value_i3 => dyn_mpas_get_variable_value_i3
        procedure, pass :: get_variable_value_l0 => dyn_mpas_get_variable_value_l0
        procedure, pass :: get_variable_value_r0 => dyn_mpas_get_variable_value_r0
        procedure, pass :: get_variable_value_r1 => dyn_mpas_get_variable_value_r1
        procedure, pass :: get_variable_value_r2 => dyn_mpas_get_variable_value_r2
        procedure, pass :: get_variable_value_r3 => dyn_mpas_get_variable_value_r3
        procedure, pass :: get_variable_value_r4 => dyn_mpas_get_variable_value_r4
        procedure, pass :: get_variable_value_r5 => dyn_mpas_get_variable_value_r5
        generic, public :: get_variable_value => get_variable_value_c0, get_variable_value_c1, &
                                                 get_variable_value_i0, get_variable_value_i1, &
                                                 get_variable_value_i2, get_variable_value_i3, &
                                                 get_variable_value_l0, &
                                                 get_variable_value_r0, get_variable_value_r1, &
                                                 get_variable_value_r2, get_variable_value_r3, &
                                                 get_variable_value_r4, get_variable_value_r5
    end type mpas_dynamical_core_type

    !> This derived type conveys information similar to the `var` and `var_array` elements in MPAS registry.
    !> For example, in MPAS registry, the "xCell" variable is described as:
    !>     <var name="xCell" type="real" dimensions="nCells" units="m" description="Cartesian x-coordinate of cells" />
    !> Here, it is described as:
    !>     var_info_type(name="xCell", type="real", rank=1)
    type :: var_info_type
        private

        character(64) :: name = ''
        character(10) :: type = ''
        integer :: rank = 0
    end type var_info_type

    ! Developers/Maintainers should keep the following parameters up-to-date with MPAS registry.

    !> This list corresponds to the "invariant" stream in MPAS registry.
    !> It consists of variables that are members of the "mesh" struct.
    type(var_info_type), parameter :: invariant_var_info_list(*) = [ &
        var_info_type('angleEdge'                       , 'real'      , 1), &
        var_info_type('areaCell'                        , 'real'      , 1), &
        var_info_type('areaTriangle'                    , 'real'      , 1), &
        var_info_type('bdyMaskCell'                     , 'integer'   , 1), &
        var_info_type('bdyMaskEdge'                     , 'integer'   , 1), &
        var_info_type('bdyMaskVertex'                   , 'integer'   , 1), &
        var_info_type('cellTangentPlane'                , 'real'      , 3), &
        var_info_type('cell_gradient_coef_x'            , 'real'      , 2), &
        var_info_type('cell_gradient_coef_y'            , 'real'      , 2), &
        var_info_type('cellsOnCell'                     , 'integer'   , 2), &
        var_info_type('cellsOnEdge'                     , 'integer'   , 2), &
        var_info_type('cellsOnVertex'                   , 'integer'   , 2), &
        var_info_type('cf1'                             , 'real'      , 0), &
        var_info_type('cf2'                             , 'real'      , 0), &
        var_info_type('cf3'                             , 'real'      , 0), &
        var_info_type('coeffs_reconstruct'              , 'real'      , 3), &
        var_info_type('dcEdge'                          , 'real'      , 1), &
        var_info_type('defc_a'                          , 'real'      , 2), &
        var_info_type('defc_b'                          , 'real'      , 2), &
        var_info_type('deriv_two'                       , 'real'      , 3), &
        var_info_type('dss'                             , 'real'      , 2), &
        var_info_type('dvEdge'                          , 'real'      , 1), &
        var_info_type('dzu'                             , 'real'      , 1), &
        var_info_type('edgeNormalVectors'               , 'real'      , 2), &
        var_info_type('edgesOnCell'                     , 'integer'   , 2), &
        var_info_type('edgesOnEdge'                     , 'integer'   , 2), &
        var_info_type('edgesOnVertex'                   , 'integer'   , 2), &
        var_info_type('fEdge'                           , 'real'      , 1), &
        var_info_type('fVertex'                         , 'real'      , 1), &
        var_info_type('fzm'                             , 'real'      , 1), &
        var_info_type('fzp'                             , 'real'      , 1), &
        var_info_type('indexToCellID'                   , 'integer'   , 1), &
        var_info_type('indexToEdgeID'                   , 'integer'   , 1), &
        var_info_type('indexToVertexID'                 , 'integer'   , 1), &
        var_info_type('kiteAreasOnVertex'               , 'real'      , 2), &
        var_info_type('latCell'                         , 'real'      , 1), &
        var_info_type('latEdge'                         , 'real'      , 1), &
        var_info_type('latVertex'                       , 'real'      , 1), &
        var_info_type('localVerticalUnitVectors'        , 'real'      , 2), &
        var_info_type('lonCell'                         , 'real'      , 1), &
        var_info_type('lonEdge'                         , 'real'      , 1), &
        var_info_type('lonVertex'                       , 'real'      , 1), &
        var_info_type('meshDensity'                     , 'real'      , 1), &
        var_info_type('nEdgesOnCell'                    , 'integer'   , 1), &
        var_info_type('nEdgesOnEdge'                    , 'integer'   , 1), &
        var_info_type('nominalMinDc'                    , 'real'      , 0), &
        var_info_type('qv_init'                         , 'real'      , 1), &
        var_info_type('rdzu'                            , 'real'      , 1), &
        var_info_type('rdzw'                            , 'real'      , 1), &
        var_info_type('t_init'                          , 'real'      , 2), &
        var_info_type('u_init'                          , 'real'      , 1), &
        var_info_type('v_init'                          , 'real'      , 1), &
        var_info_type('verticesOnCell'                  , 'integer'   , 2), &
        var_info_type('verticesOnEdge'                  , 'integer'   , 2), &
        var_info_type('weightsOnEdge'                   , 'real'      , 2), &
        var_info_type('xCell'                           , 'real'      , 1), &
        var_info_type('xEdge'                           , 'real'      , 1), &
        var_info_type('xVertex'                         , 'real'      , 1), &
        var_info_type('yCell'                           , 'real'      , 1), &
        var_info_type('yEdge'                           , 'real'      , 1), &
        var_info_type('yVertex'                         , 'real'      , 1), &
        var_info_type('zCell'                           , 'real'      , 1), &
        var_info_type('zEdge'                           , 'real'      , 1), &
        var_info_type('zVertex'                         , 'real'      , 1), &
        var_info_type('zb'                              , 'real'      , 3), &
        var_info_type('zb3'                             , 'real'      , 3), &
        var_info_type('zgrid'                           , 'real'      , 2), &
        var_info_type('zxu'                             , 'real'      , 2), &
        var_info_type('zz'                              , 'real'      , 2)  &
    ]

    ! Whether a variable should be in input or restart can be determined by looking at
    ! the `atm_init_coupled_diagnostics` subroutine in MPAS.
    ! If a variable first appears on the LHS of an equation, it should be in restart.
    ! If a variable first appears on the RHS of an equation, it should be in input.

    !> This list corresponds to the "input" stream in MPAS registry.
    !> It consists of variables that are members of the "diag" and "state" struct.
    !> Only variables that are specific to the "input" stream are included.
    type(var_info_type), parameter :: input_var_info_list(*) = [ &
        var_info_type('Time'                            , 'real'      , 1), &
        var_info_type('initial_time'                    , 'character' , 0), &
        var_info_type('relhum'                          , 'real'      , 3), &
        var_info_type('rho'                             , 'real'      , 3), &
        var_info_type('rho_base'                        , 'real'      , 3), &
        var_info_type('scalars'                         , 'real'      , 3), &
        var_info_type('theta'                           , 'real'      , 3), &
        var_info_type('theta_base'                      , 'real'      , 3), &
        var_info_type('u'                               , 'real'      , 3), &
        var_info_type('w'                               , 'real'      , 3), &
        var_info_type('xtime'                           , 'character' , 1)  &
    ]

    !> This list corresponds to the "restart" stream in MPAS registry.
    !> It consists of variables that are members of the "diag" and "state" struct.
    !> Only variables that are specific to the "restart" stream are included.
    type(var_info_type), parameter :: restart_var_info_list(*) = [ &
        var_info_type('exner'                           , 'real'      , 1), &
        var_info_type('exner_base'                      , 'real'      , 1), &
        var_info_type('pressure_base'                   , 'real'      , 1), &
        var_info_type('pressure_p'                      , 'real'      , 1), &
        var_info_type('rho_p'                           , 'real'      , 1), &
        var_info_type('rho_zz'                          , 'real'      , 1), &
        var_info_type('rtheta_base'                     , 'real'      , 1), &
        var_info_type('rtheta_p'                        , 'real'      , 1), &
        var_info_type('ru'                              , 'real'      , 1), &
        var_info_type('ru_p'                            , 'real'      , 1), &
        var_info_type('rw'                              , 'real'      , 1), &
        var_info_type('rw_p'                            , 'real'      , 1), &
        var_info_type('theta_m'                         , 'real'      , 1)  &
    ]
contains
    !> Print a debug message with optionally the value(s) of a variable.
    !> If `printer` is not supplied, the MPI root rank will print. Otherwise, the designated MPI rank will print instead.
    !> (KCW, 2024-02-03)
    subroutine dyn_mpas_debug_print(self, message, variable, printer)
        class(mpas_dynamical_core_type), intent(in) :: self
        character(*), intent(in) :: message
        class(*), optional, intent(in) :: variable(:)
        integer, optional, intent(in) :: printer

        ! Bail out early if debug output is not requested.
        if (.not. self % debug_output) then
            return
        end if

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
        class(mpas_dynamical_core_type), intent(in) :: self
        character(*), intent(in) :: namelist_path, cf_calendar
        integer, intent(in) :: start_date_time(6), & ! YYYY, MM, DD, hh, mm, ss.
                               stop_date_time(6),  & ! YYYY, MM, DD, hh, mm, ss.
                               run_duration(4)       ! DD, hh, mm, ss.
        logical, intent(in) :: initial_run

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_read_namelist'
        character(strkind) :: mpas_calendar
        character(strkind), pointer :: config_pointer_c => null()
        integer :: ierr
        logical, pointer :: config_pointer_l => null()

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

        call mpas_pool_get_config(self % domain_ptr % configs, 'config_calendar_type', config_pointer_c)

        if (.not. associated(config_pointer_c)) then
            call self % model_error('Failed to find config "config_calendar_type"', subname, __LINE__)
        end if

        config_pointer_c = trim(adjustl(mpas_calendar))
        call self % debug_print('config_calendar_type = ', [config_pointer_c])
        nullify(config_pointer_c)

        ! MPAS represents date and time in ISO 8601 format. However, the separator between date and time is `_`
        ! instead of standard `T`.
        ! Format in `YYYY-MM-DD_hh:mm:ss` is acceptable.
        call mpas_pool_get_config(self % domain_ptr % configs, 'config_start_time', config_pointer_c)

        if (.not. associated(config_pointer_c)) then
            call self % model_error('Failed to find config "config_start_time"', subname, __LINE__)
        end if

        config_pointer_c = stringify(start_date_time(1:3), '-') // '_' // stringify(start_date_time(4:6), ':')
        call self % debug_print('config_start_time = ', [config_pointer_c])
        nullify(config_pointer_c)

        call mpas_pool_get_config(self % domain_ptr % configs, 'config_stop_time', config_pointer_c)

        if (.not. associated(config_pointer_c)) then
            call self % model_error('Failed to find config "config_stop_time"', subname, __LINE__)
        end if

        config_pointer_c = stringify(stop_date_time(1:3), '-') // '_' // stringify(stop_date_time(4:6), ':')
        call self % debug_print('config_stop_time = ', [config_pointer_c])
        nullify(config_pointer_c)

        ! Format in `DD_hh:mm:ss` is acceptable.
        call mpas_pool_get_config(self % domain_ptr % configs, 'config_run_duration', config_pointer_c)

        if (.not. associated(config_pointer_c)) then
            call self % model_error('Failed to find config "config_run_duration"', subname, __LINE__)
        end if

        config_pointer_c = stringify([run_duration(1)]) // '_' // stringify(run_duration(2:4), ':')
        call self % debug_print('config_run_duration = ', [config_pointer_c])
        nullify(config_pointer_c)

        ! Reflect current run type to MPAS.
        call mpas_pool_get_config(self % domain_ptr % configs, 'config_do_restart', config_pointer_l)

        if (.not. associated(config_pointer_l)) then
            call self % model_error('Failed to find config "config_do_restart"', subname, __LINE__)
        end if

        if (initial_run) then
            ! Run type is initial run.
            config_pointer_l = .false.
        else
            ! Run type is branch or restart run.
            config_pointer_l = .true.
        end if

        call self % debug_print('config_do_restart = ', [config_pointer_l])
        nullify(config_pointer_l)

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
    !>  to the check on the existence of the `streams.<core>` file.
    !> \addenda
    !>  Ported and refactored for CAM-SIMA. (KCW, 2024-02-07)
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_init_phase2(self, pio_iosystem)
        class(mpas_dynamical_core_type), intent(in) :: self
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

    !-------------------------------------------------------------------------------
    ! subroutine dyn_mpas_init_phase3
    !
    !> \brief  Tracks `mpas_init` up to the point of calling `core_init`
    !> \author Michael Duda
    !> \date   19 April 2019
    !> \details
    !>  This subroutine follows the stand-alone MPAS subdriver after the check on
    !>  the existence of the `streams.<core>` file up to, but not including,
    !>  the point where `core_init` is called. It completes MPAS framework
    !>  initialization, including the allocation of all blocks and fields managed
    !>  by MPAS.
    !> \addenda
    !>  Ported and refactored for CAM-SIMA. (KCW, 2024-03-06)
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_init_phase3(self, cam_pcnst, pio_file)
        class(mpas_dynamical_core_type), intent(in) :: self
        integer, intent(in) :: cam_pcnst
        type(file_desc_t), pointer, intent(in) :: pio_file

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_init_phase3'
        character(strkind) :: mesh_filename
        integer :: mesh_format

        call self % debug_print(subname // ' entered')

        call self % debug_print('Number of constituents is ', [cam_pcnst])

        ! Adding a config named `cam_pcnst` with the number of constituents will indicate to MPAS that
        ! it is operating as a dynamical core, and therefore it needs to allocate scalars separately
        ! from other Registry-defined fields. The special logic is located in `atm_setup_block`.
        ! This must be done before calling `mpas_bootstrap_framework_phase1`.
        call mpas_pool_add_config(self % domain_ptr % configs, 'cam_pcnst', cam_pcnst)

        ! Not actually used because a PIO file descriptor is directly supplied.
        mesh_filename = 'external mesh'
        mesh_format = mpas_io_pnetcdf

        call self % debug_print('Checking PIO file descriptor')

        if (.not. associated(pio_file)) then
            call self % model_error('Invalid PIO file descriptor', subname, __LINE__)
        end if

        if (.not. pio_file_is_open(pio_file)) then
            call self % model_error('Invalid PIO file descriptor', subname, __LINE__)
        end if

        call self % debug_print('Calling mpas_bootstrap_framework_phase1')

        ! Finish setting up blocks.
        call mpas_bootstrap_framework_phase1(self % domain_ptr, mesh_filename, mesh_format, pio_file_desc=pio_file)

        call self % debug_print('Calling mpas_bootstrap_framework_phase2')

        ! Finish setting up fields.
        call mpas_bootstrap_framework_phase2(self % domain_ptr, pio_file_desc=pio_file)

        call self % debug_print(subname // ' completed')
    end subroutine dyn_mpas_init_phase3

    !-------------------------------------------------------------------------------
    ! subroutine dyn_mpas_read_write_stream
    !
    !> \brief  Read or write an MPAS stream
    !> \author Kuan-Chih Wang
    !> \date   2024-03-15
    !> \details
    !>  In the context of MPAS, the concept of a "pool" resembles a group of
    !>  (related) variables, while the concept of a "stream" resembles a file.
    !>  This subroutine reads or writes an MPAS stream. It provides the mechanism
    !>  for CAM-SIMA to input/output data to/from MPAS dynamical core.
    !>  Analogous to the `{read,write}_stream` subroutines in MPAS stream manager.
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_read_write_stream(self, pio_file, stream_mode, stream_name)
        class(mpas_dynamical_core_type), intent(in) :: self
        type(file_desc_t), pointer, intent(in) :: pio_file
        character(*), intent(in) :: stream_mode
        character(*), intent(in) :: stream_name

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_read_write_stream'
        integer :: i, ierr
        type(mpas_pool_type), pointer :: mpas_pool => null()
        type(mpas_stream_type), pointer :: mpas_stream => null()
        type(var_info_type), allocatable :: var_info_list(:)

        call self % debug_print(subname // ' entered')

        call self % debug_print('Initializing stream "' // trim(adjustl(stream_name)) // '"')

        call self % init_stream_with_pool(mpas_pool, mpas_stream, pio_file, stream_mode, stream_name)

        if (.not. associated(mpas_pool)) then
            call self % model_error('Failed to initialize stream "' // trim(adjustl(stream_name)) // '"', subname, __LINE__)
        end if

        if (.not. associated(mpas_stream)) then
            call self % model_error('Failed to initialize stream "' // trim(adjustl(stream_name)) // '"', subname, __LINE__)
        end if

        select case (trim(adjustl(stream_mode)))
            case ('r', 'read')
                call self % debug_print('Reading stream "' // trim(adjustl(stream_name)) // '"')

                call mpas_readstream(mpas_stream, 1, ierr=ierr)

                if (ierr /= mpas_stream_noerr) then
                    call self % model_error('Failed to read stream "' // trim(adjustl(stream_name)) // '"', subname, __LINE__)
                end if

                ! Exchange halo layers because new data have just been read.
                var_info_list = parse_stream_name(stream_name)

                do i = 1, size(var_info_list)
                    call self % exchange_halo(var_info_list(i) % name)
                end do

                ! For any connectivity arrays in this stream, convert global indexes to local indexes.
                call postread_reindex(self % domain_ptr % blocklist % allfields, self % domain_ptr % packages, &
                    mpas_pool, mpas_pool)
            case ('w', 'write')
                call self % debug_print('Writing stream "' // trim(adjustl(stream_name)) // '"')

                ! WARNING:
                ! The `{pre,post}write_reindex` subroutines are STATEFUL because they store information inside their module
                ! (i.e., module variables). They MUST be called in pairs, like below, to prevent undefined behaviors.

                ! For any connectivity arrays in this stream, temporarily convert local indexes to global indexes.
                call prewrite_reindex(self % domain_ptr % blocklist % allfields, self % domain_ptr % packages, &
                    mpas_pool, mpas_pool)

                call mpas_writestream(mpas_stream, 1, ierr=ierr)

                if (ierr /= mpas_stream_noerr) then
                    call self % model_error('Failed to write stream "' // trim(adjustl(stream_name)) // '"', subname, __LINE__)
                end if

                ! For any connectivity arrays in this stream, reset global indexes back to local indexes.
                call postwrite_reindex(self % domain_ptr % blocklist % allfields, mpas_pool)
            case default
                call self % model_error('Unsupported stream mode "' // trim(adjustl(stream_mode)) // '"', subname, __LINE__)
        end select

        call self % debug_print('Closing stream "' // trim(adjustl(stream_name)) // '"')

        call mpas_closestream(mpas_stream, ierr=ierr)

        if (ierr /= mpas_stream_noerr) then
            call self % model_error('Failed to close stream "' // trim(adjustl(stream_name)) // '"', subname, __LINE__)
        end if

        ! Deallocate temporary pointers to avoid memory leaks.
        call mpas_pool_destroy_pool(mpas_pool)
        nullify(mpas_pool)

        deallocate(mpas_stream)
        nullify(mpas_stream)

        call self % debug_print(subname // ' completed')
    end subroutine dyn_mpas_read_write_stream

    !-------------------------------------------------------------------------------
    ! subroutine dyn_mpas_init_stream_with_pool
    !
    !> \brief  Initialize an MPAS stream with an accompanying MPAS pool
    !> \author Kuan-Chih Wang
    !> \date   2024-03-14
    !> \details
    !>  In the context of MPAS, the concept of a "pool" resembles a group of
    !>  (related) variables, while the concept of a "stream" resembles a file.
    !>  This subroutine initializes an MPAS stream with an accompanying MPAS pool by
    !>  adding variable and attribute information to them. After that, MPAS is ready
    !>  to perform IO on them.
    !>  Analogous to the `build_stream` and `mpas_stream_mgr_add_field`
    !>  subroutines in MPAS stream manager.
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_init_stream_with_pool(self, mpas_pool, mpas_stream, pio_file, stream_mode, stream_name)
        class(mpas_dynamical_core_type), intent(in) :: self
        type(mpas_pool_type), pointer, intent(out) :: mpas_pool
        type(mpas_stream_type), pointer, intent(out) :: mpas_stream
        type(file_desc_t), pointer, intent(in) :: pio_file
        character(*), intent(in) :: stream_mode
        character(*), intent(in) :: stream_name

        interface add_stream_attribute
            procedure :: add_stream_attribute_0d
            procedure :: add_stream_attribute_1d
        end interface add_stream_attribute

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_init_stream_with_pool'
        character(strkind) :: stream_filename
        integer :: i, ierr, ndims, stream_format, varid
        type(field0dchar), pointer :: field_0d_char => null()
        type(field1dchar), pointer :: field_1d_char => null()
        type(field0dinteger), pointer :: field_0d_integer => null()
        type(field1dinteger), pointer :: field_1d_integer => null()
        type(field2dinteger), pointer :: field_2d_integer => null()
        type(field3dinteger), pointer :: field_3d_integer => null()
        type(field0dreal), pointer :: field_0d_real => null()
        type(field1dreal), pointer :: field_1d_real => null()
        type(field2dreal), pointer :: field_2d_real => null()
        type(field3dreal), pointer :: field_3d_real => null()
        type(field4dreal), pointer :: field_4d_real => null()
        type(field5dreal), pointer :: field_5d_real => null()
        type(var_info_type), allocatable :: var_info_list(:)

        call self % debug_print(subname // ' entered')

        call mpas_pool_create_pool(mpas_pool)

        allocate(mpas_stream, stat=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to allocate stream "' // trim(adjustl(stream_name)) // '"', subname, __LINE__)
        end if

        ! Not actually used because a PIO file descriptor is directly supplied.
        stream_filename = 'external stream'
        stream_format = mpas_io_pnetcdf

        call self % debug_print('Checking PIO file descriptor')

        if (.not. associated(pio_file)) then
            call self % model_error('Invalid PIO file descriptor', subname, __LINE__)
        end if

        if (.not. pio_file_is_open(pio_file)) then
            call self % model_error('Invalid PIO file descriptor', subname, __LINE__)
        end if

        select case (trim(adjustl(stream_mode)))
            case ('r', 'read')
                call self % debug_print('Creating "' // trim(adjustl(stream_name)) // '" stream for reading')

                call mpas_createstream( &
                    mpas_stream, self % domain_ptr % iocontext, stream_filename, stream_format, mpas_io_read,  &
                    clobberrecords=.false., clobberfiles=.false., truncatefiles=.false., &
                    precision=mpas_io_native_precision, pio_file_desc=pio_file, ierr=ierr)
            case ('w', 'write')
                call self % debug_print('Creating "' // trim(adjustl(stream_name)) // '" stream for writing')

                call mpas_createstream( &
                    mpas_stream, self % domain_ptr % iocontext, stream_filename, stream_format, mpas_io_write, &
                    clobberrecords=.false., clobberfiles=.false., truncatefiles=.false., &
                    precision=mpas_io_native_precision, pio_file_desc=pio_file, ierr=ierr)
            case default
                call self % model_error('Unsupported stream mode "' // trim(adjustl(stream_mode)) // '"', subname, __LINE__)
        end select

        if (ierr /= mpas_stream_noerr) then
            call self % model_error('Failed to create stream "' // trim(adjustl(stream_name)) // '"', subname, __LINE__)
        end if

        var_info_list = parse_stream_name(stream_name)

        ! Add variables to stream.
        call self % debug_print('Adding variables to stream')

        do i = 1, size(var_info_list)
            call self % debug_print('var_info_list(' // stringify([i]) // ') % name = ' // &
                stringify([var_info_list(i) % name]))
            call self % debug_print('var_info_list(' // stringify([i]) // ') % type = ' // &
                stringify([var_info_list(i) % type]))
            call self % debug_print('var_info_list(' // stringify([i]) // ') % rank = ' // &
                stringify([var_info_list(i) % rank]))

            if (trim(adjustl(stream_mode)) == 'r' .or. trim(adjustl(stream_mode)) == 'read') then
                ! Check if "<variable name>" is present.
                ierr = pio_inq_varid(pio_file, trim(adjustl(var_info_list(i) % name)), varid)

                ! Do not hard crash the model if a variable is missing and cannot be read.
                ! This can happen if users attempt to initialize/restart the model with data generated by
                ! older versions of MPAS. Print a debug message to let users decide if this is acceptable.
                if (ierr /= pio_noerr) then
                    call self % debug_print('Skipping variable "' // trim(adjustl(var_info_list(i) % name)) // '"')

                    cycle
                end if

                ierr = pio_inq_varndims(pio_file, varid, ndims)

                if (ierr /= pio_noerr) then
                    call self % model_error('Failed to inquire variable rank for "' // trim(adjustl(var_info_list(i) % name)) // &
                        '"', subname, __LINE__)
                end if

                if (ndims /= var_info_list(i) % rank) then
                    call self % model_error('Variable rank mismatch for "' // trim(adjustl(var_info_list(i) % name)) // &
                        '"', subname, __LINE__)
                end if
            end if

            ! Add "<variable name>" to pool with the value of `1`.
            ! The existence of "<variable name>" in pool causes it to be considered for IO in MPAS.
            call mpas_pool_add_config(mpas_pool, trim(adjustl(var_info_list(i) % name)), 1)
            ! Add "<variable name>:packages" to pool with the value of an empty character string.
            ! This causes "<variable name>" to be always considered active for IO in MPAS.
            call mpas_pool_add_config(mpas_pool, trim(adjustl(var_info_list(i) % name) // ':packages'), '')

            ! Add "<variable name>" to stream.
            select case (trim(adjustl(var_info_list(i) % type)))
                case ('character')
                    select case (var_info_list(i) % rank)
                        case (0)
                            call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                                trim(adjustl(var_info_list(i) % name)), field_0d_char, timelevel=1)

                            if (.not. associated(field_0d_char)) then
                                call self % model_error('Failed to find variable "' // trim(adjustl(var_info_list(i) % name)) // &
                                    '"', subname, __LINE__)
                            end if

                            call mpas_streamaddfield(mpas_stream, field_0d_char, ierr=ierr)

                            nullify(field_0d_char)
                        case (1)
                            call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                                trim(adjustl(var_info_list(i) % name)), field_1d_char, timelevel=1)

                            if (.not. associated(field_1d_char)) then
                                call self % model_error('Failed to find variable "' // trim(adjustl(var_info_list(i) % name)) // &
                                    '"', subname, __LINE__)
                            end if

                            call mpas_streamaddfield(mpas_stream, field_1d_char, ierr=ierr)

                            nullify(field_1d_char)
                        case default
                            call self % model_error('Unsupported variable rank ' // stringify([var_info_list(i) % rank]), &
                                subname, __LINE__)
                    end select
                case ('integer')
                    select case (var_info_list(i) % rank)
                        case (0)
                            call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                                trim(adjustl(var_info_list(i) % name)), field_0d_integer, timelevel=1)

                            if (.not. associated(field_0d_integer)) then
                                call self % model_error('Failed to find variable "' // trim(adjustl(var_info_list(i) % name)) // &
                                    '"', subname, __LINE__)
                            end if

                            call mpas_streamaddfield(mpas_stream, field_0d_integer, ierr=ierr)

                            nullify(field_0d_integer)
                        case (1)
                            call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                                trim(adjustl(var_info_list(i) % name)), field_1d_integer, timelevel=1)

                            if (.not. associated(field_1d_integer)) then
                                call self % model_error('Failed to find variable "' // trim(adjustl(var_info_list(i) % name)) // &
                                    '"', subname, __LINE__)
                            end if

                            call mpas_streamaddfield(mpas_stream, field_1d_integer, ierr=ierr)

                            nullify(field_1d_integer)
                        case (2)
                            call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                                trim(adjustl(var_info_list(i) % name)), field_2d_integer, timelevel=1)

                            if (.not. associated(field_2d_integer)) then
                                call self % model_error('Failed to find variable "' // trim(adjustl(var_info_list(i) % name)) // &
                                    '"', subname, __LINE__)
                            end if

                            call mpas_streamaddfield(mpas_stream, field_2d_integer, ierr=ierr)

                            nullify(field_2d_integer)
                        case (3)
                            call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                                trim(adjustl(var_info_list(i) % name)), field_3d_integer, timelevel=1)

                            if (.not. associated(field_3d_integer)) then
                                call self % model_error('Failed to find variable "' // trim(adjustl(var_info_list(i) % name)) // &
                                    '"', subname, __LINE__)
                            end if

                            call mpas_streamaddfield(mpas_stream, field_3d_integer, ierr=ierr)

                            nullify(field_3d_integer)
                        case default
                            call self % model_error('Unsupported variable rank ' // stringify([var_info_list(i) % rank]), &
                                subname, __LINE__)
                    end select
                case ('real')
                    select case (var_info_list(i) % rank)
                        case (0)
                            call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                                trim(adjustl(var_info_list(i) % name)), field_0d_real, timelevel=1)

                            if (.not. associated(field_0d_real)) then
                                call self % model_error('Failed to find variable "' // trim(adjustl(var_info_list(i) % name)) // &
                                    '"', subname, __LINE__)
                            end if

                            call mpas_streamaddfield(mpas_stream, field_0d_real, ierr=ierr)

                            nullify(field_0d_real)
                        case (1)
                            call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                                trim(adjustl(var_info_list(i) % name)), field_1d_real, timelevel=1)

                            if (.not. associated(field_1d_real)) then
                                call self % model_error('Failed to find variable "' // trim(adjustl(var_info_list(i) % name)) // &
                                    '"', subname, __LINE__)
                            end if

                            call mpas_streamaddfield(mpas_stream, field_1d_real, ierr=ierr)

                            nullify(field_1d_real)
                        case (2)
                            call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                                trim(adjustl(var_info_list(i) % name)), field_2d_real, timelevel=1)

                            if (.not. associated(field_2d_real)) then
                                call self % model_error('Failed to find variable "' // trim(adjustl(var_info_list(i) % name)) // &
                                    '"', subname, __LINE__)
                            end if

                            call mpas_streamaddfield(mpas_stream, field_2d_real, ierr=ierr)

                            nullify(field_2d_real)
                        case (3)
                            call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                                trim(adjustl(var_info_list(i) % name)), field_3d_real, timelevel=1)

                            if (.not. associated(field_3d_real)) then
                                call self % model_error('Failed to find variable "' // trim(adjustl(var_info_list(i) % name)) // &
                                    '"', subname, __LINE__)
                            end if

                            call mpas_streamaddfield(mpas_stream, field_3d_real, ierr=ierr)

                            nullify(field_3d_real)
                        case (4)
                            call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                                trim(adjustl(var_info_list(i) % name)), field_4d_real, timelevel=1)

                            if (.not. associated(field_4d_real)) then
                                call self % model_error('Failed to find variable "' // trim(adjustl(var_info_list(i) % name)) // &
                                    '"', subname, __LINE__)
                            end if

                            call mpas_streamaddfield(mpas_stream, field_4d_real, ierr=ierr)

                            nullify(field_4d_real)
                        case (5)
                            call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                                trim(adjustl(var_info_list(i) % name)), field_5d_real, timelevel=1)

                            if (.not. associated(field_5d_real)) then
                                call self % model_error('Failed to find variable "' // trim(adjustl(var_info_list(i) % name)) // &
                                    '"', subname, __LINE__)
                            end if

                            call mpas_streamaddfield(mpas_stream, field_5d_real, ierr=ierr)

                            nullify(field_5d_real)
                        case default
                            call self % model_error('Unsupported variable rank ' // stringify([var_info_list(i) % rank]), &
                                subname, __LINE__)
                    end select
                case default
                    call self % model_error('Unsupported variable type "' // trim(adjustl(var_info_list(i) % type)) // &
                        '"', subname, __LINE__)
            end select

            if (ierr /= mpas_stream_noerr) then
                call self % model_error('Failed to add variable "' // trim(adjustl(var_info_list(i) % name)) // &
                    '" to stream "' // trim(adjustl(stream_name)) // '"', subname, __LINE__)
            end if
        end do

        deallocate(var_info_list)

        if (trim(adjustl(stream_mode)) == 'w' .or. trim(adjustl(stream_mode)) == 'write') then
            ! Add MPAS-specific attributes to stream.
            call self % debug_print('Adding attributes to stream')

            ! Attributes related to MPAS core (i.e., `core_type`).
            call add_stream_attribute('conventions', self % domain_ptr % core % conventions)
            call add_stream_attribute('core_name', self % domain_ptr % core % corename)
            call add_stream_attribute('git_version', self % domain_ptr % core % git_version)
            call add_stream_attribute('model_name', self % domain_ptr % core % modelname)
            call add_stream_attribute('source', self % domain_ptr % core % source)

            ! Attributes related to MPAS domain (i.e., `domain_type`).
            call add_stream_attribute('is_periodic', self % domain_ptr % is_periodic)
            call add_stream_attribute('mesh_spec', self % domain_ptr % mesh_spec)
            call add_stream_attribute('on_a_sphere', self % domain_ptr % on_a_sphere)
            call add_stream_attribute('parent_id', self % domain_ptr %  parent_id)
            call add_stream_attribute('sphere_radius', self % domain_ptr % sphere_radius)
            call add_stream_attribute('x_period', self % domain_ptr % x_period)
            call add_stream_attribute('y_period', self % domain_ptr % y_period)
        end if

        call self % debug_print(subname // ' completed')
    contains
        !> Helper subroutine for adding a 0-d stream attribute by calling `mpas_writestreamatt` with error checking.
        !> (KCW, 2024-03-14)
        subroutine add_stream_attribute_0d(attribute_name, attribute_value)
            character(*), intent(in) :: attribute_name
            class(*), intent(in) :: attribute_value

            select type (attribute_value)
                type is (character(*))
                    call mpas_writestreamatt(mpas_stream, &
                        trim(adjustl(attribute_name)), trim(adjustl(attribute_value)), syncval=.false., ierr=ierr)
                type is (integer)
                    call mpas_writestreamatt(mpas_stream, &
                        trim(adjustl(attribute_name)), attribute_value, syncval=.false., ierr=ierr)
                type is (logical)
                    if (attribute_value) then
                        ! Logical `.true.` becomes character string `YES`.
                        call mpas_writestreamatt(mpas_stream, &
                            trim(adjustl(attribute_name)), 'YES', syncval=.false., ierr=ierr)
                    else
                        ! Logical `.false.` becomes character string `NO`.
                        call mpas_writestreamatt(mpas_stream, &
                            trim(adjustl(attribute_name)), 'NO', syncval=.false., ierr=ierr)
                    end if
                type is (real(rkind))
                    call mpas_writestreamatt(mpas_stream, &
                        trim(adjustl(attribute_name)), attribute_value, syncval=.false., ierr=ierr)
                class default
                    call self % model_error('Unsupported attribute type (Must be one of: character, integer, logical, real)', &
                        subname, __LINE__)
            end select

            if (ierr /= mpas_stream_noerr) then
                call self % model_error('Failed to add attribute "' // trim(adjustl(attribute_name)) // &
                    '" to stream "' // trim(adjustl(stream_name)) // '"', subname, __LINE__)
            end if
        end subroutine add_stream_attribute_0d

        !> Helper subroutine for adding a 1-d stream attribute by calling `mpas_writestreamatt` with error checking.
        !> (KCW, 2024-03-14)
        subroutine add_stream_attribute_1d(attribute_name, attribute_value)
            character(*), intent(in) :: attribute_name
            class(*), intent(in) :: attribute_value(:)

            select type (attribute_value)
                type is (integer)
                    call mpas_writestreamatt(mpas_stream, &
                        trim(adjustl(attribute_name)), attribute_value, syncval=.false., ierr=ierr)
                type is (real(rkind))
                    call mpas_writestreamatt(mpas_stream, &
                        trim(adjustl(attribute_name)), attribute_value, syncval=.false., ierr=ierr)
                class default
                    call self % model_error('Unsupported attribute type (Must be one of: integer, real)', &
                        subname, __LINE__)
            end select

            if (ierr /= mpas_stream_noerr) then
                call self % model_error('Failed to add attribute "' // trim(adjustl(attribute_name)) // &
                    '" to stream "' // trim(adjustl(stream_name)) // '"', subname, __LINE__)
            end if
        end subroutine add_stream_attribute_1d
    end subroutine dyn_mpas_init_stream_with_pool

    !> Parse one or more stream names and return variable information contained in those streams as a list of `var_info_type`.
    !> Multiple stream names should be separated by `+` (i.e., a plus).
    !> Duplicate variable names in the resulting list are discarded.
    !> (KCW, 2024-03-15)
    pure recursive function parse_stream_name(stream_name) result(var_info_list)
        character(*), intent(in) :: stream_name
        type(var_info_type), allocatable :: var_info_list(:)

        character(64), allocatable :: var_name_list(:)
        integer :: i, n, offset
        type(var_info_type), allocatable :: var_info_list_append(:)

        allocate(var_info_list(0))

        n = len(stream_name)
        offset = 0

        if (offset + 1 > n) then
            return
        end if

        i = index(stream_name(offset + 1:), '+')

        do while (i > 0)
            if (i > 1) then
                var_info_list_append = parse_stream_name(stream_name(offset + 1:offset + i - 1))
                var_info_list = [var_info_list, var_info_list_append]

                deallocate(var_info_list_append)
            end if

            offset = offset + i

            if (offset + 1 > n) then
                exit
            end if

            i = index(stream_name(offset + 1:), '+')
        end do

        if (offset + 1 > n) then
            return
        end if

        select case (trim(adjustl(stream_name(offset + 1:))))
            case ('invariant')
                allocate(var_info_list_append, source=invariant_var_info_list)
            case ('input')
                allocate(var_info_list_append, source=input_var_info_list)
            case ('restart')
                allocate(var_info_list_append, source=restart_var_info_list)
            case default
                allocate(var_info_list_append(0))
        end select

        var_info_list = [var_info_list, var_info_list_append]

        ! Discard duplicate variable information by names.
        var_name_list = var_info_list(:) % name
        var_info_list = var_info_list(index_unique(var_name_list))

        deallocate(var_info_list_append)
        deallocate(var_name_list)
    end function parse_stream_name

    !> Return the index of unique elements in `array`, which can be any intrinsic data types, as an integer array.
    !> If `array` contains zero element or is of unsupported data types, an empty integer array is produced.
    !> (KCW, 2024-03-22)
    pure function index_unique(array)
        use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64

        class(*), intent(in) :: array(:)
        integer, allocatable :: index_unique(:)

        character(:), allocatable :: array_c(:)
        integer :: i, n
        logical :: mask_unique(size(array))

        n = size(array)

        if (n == 0) then
            allocate(index_unique(0))

            return
        end if

        mask_unique = .false.

        select type (array)
            type is (character(*))
                ! Workaround for a bug in Cray wrapper compiler for GNU Fortran.
                ! When a character string array is passed as the actual argument to the unlimited polymorphic dummy argument,
                ! its array indexing is mishandled.
                allocate(character(len(array)) :: array_c(size(array)))

                array_c(:) = array(:)

                do i = 1, n
                    if (.not. any(array_c(i) == array_c .and. mask_unique)) then
                        mask_unique(i) = .true.
                    end if
                end do

                deallocate(array_c)
            type is (integer(int32))
                do i = 1, n
                    if (.not. any(array(i) == array .and. mask_unique)) then
                        mask_unique(i) = .true.
                    end if
                end do
            type is (integer(int64))
                do i = 1, n
                    if (.not. any(array(i) == array .and. mask_unique)) then
                        mask_unique(i) = .true.
                    end if
                end do
            type is (logical)
                do i = 1, n
                    if (.not. any((array(i) .eqv. array) .and. mask_unique)) then
                        mask_unique(i) = .true.
                    end if
                end do
            type is (real(real32))
                do i = 1, n
                    if (.not. any(array(i) == array .and. mask_unique)) then
                        mask_unique(i) = .true.
                    end if
                end do
            type is (real(real64))
                do i = 1, n
                    if (.not. any(array(i) == array .and. mask_unique)) then
                        mask_unique(i) = .true.
                    end if
                end do
            class default
                allocate(index_unique(0))

                return
        end select

        index_unique = pack([(i, i = 1, n)], mask_unique)
    end function index_unique

    !-------------------------------------------------------------------------------
    ! subroutine dyn_mpas_exchange_halo
    !
    !> \brief  Updates the halo layers of the named field
    !> \author Michael Duda
    !> \date   16 January 2020
    !> \details
    !>  Given a field name that is defined in MPAS registry, this subroutine updates
    !>  the halo layers for that field.
    !> \addenda
    !>  Ported and refactored for CAM-SIMA. (KCW, 2024-03-18)
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_exchange_halo(self, field_name)
        class(mpas_dynamical_core_type), intent(in) :: self
        character(*), intent(in) :: field_name

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_exchange_halo'
        type(field1dinteger), pointer :: field_1d_integer => null()
        type(field2dinteger), pointer :: field_2d_integer => null()
        type(field3dinteger), pointer :: field_3d_integer => null()
        type(field1dreal), pointer :: field_1d_real => null()
        type(field2dreal), pointer :: field_2d_real => null()
        type(field3dreal), pointer :: field_3d_real => null()
        type(field4dreal), pointer :: field_4d_real => null()
        type(field5dreal), pointer :: field_5d_real => null()
        type(mpas_pool_field_info_type) :: mpas_pool_field_info

        call self % debug_print(subname // ' entered')

        call self % debug_print('Inquiring field information for "' // trim(adjustl(field_name)) // '"')

        call mpas_pool_get_field_info(self % domain_ptr % blocklist % allfields, &
            trim(adjustl(field_name)), mpas_pool_field_info)

        if (mpas_pool_field_info % fieldtype == -1 .or. &
            mpas_pool_field_info % ndims == -1 .or. &
            mpas_pool_field_info % nhalolayers == -1) then
            call self % model_error('Invalid field information for "' // trim(adjustl(field_name)) // '"', subname, __LINE__)
        end if

        ! No halo layers to exchange. This field is not decomposed.
        if (mpas_pool_field_info % nhalolayers == 0) then
            call self % debug_print('Skipping field "' // trim(adjustl(field_name)) // '"')

            return
        end if

        call self % debug_print('Exchanging halo layers for "' // trim(adjustl(field_name)) // '"')

        select case (mpas_pool_field_info % fieldtype)
            case (mpas_pool_integer)
                select case (mpas_pool_field_info % ndims)
                    case (1)
                        call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                            trim(adjustl(field_name)), field_1d_integer, timelevel=1)

                        if (.not. associated(field_1d_integer)) then
                            call self % model_error('Failed to find field "' // trim(adjustl(field_name)) // &
                                '"', subname, __LINE__)
                        end if

                        call mpas_dmpar_exch_halo_field(field_1d_integer)

                        nullify(field_1d_integer)
                    case (2)
                        call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                            trim(adjustl(field_name)), field_2d_integer, timelevel=1)

                        if (.not. associated(field_2d_integer)) then
                            call self % model_error('Failed to find field "' // trim(adjustl(field_name)) // &
                                '"', subname, __LINE__)
                        end if

                        call mpas_dmpar_exch_halo_field(field_2d_integer)

                        nullify(field_2d_integer)
                    case (3)
                        call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                            trim(adjustl(field_name)), field_3d_integer, timelevel=1)

                        if (.not. associated(field_3d_integer)) then
                            call self % model_error('Failed to find field "' // trim(adjustl(field_name)) // &
                                '"', subname, __LINE__)
                        end if

                        call mpas_dmpar_exch_halo_field(field_3d_integer)

                        nullify(field_3d_integer)
                    case default
                        call self % model_error('Unsupported field rank ' // stringify([mpas_pool_field_info % ndims]), &
                            subname, __LINE__)
                end select
            case (mpas_pool_real)
                select case (mpas_pool_field_info % ndims)
                    case (1)
                        call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                            trim(adjustl(field_name)), field_1d_real, timelevel=1)

                        if (.not. associated(field_1d_real)) then
                            call self % model_error('Failed to find field "' // trim(adjustl(field_name)) // &
                                '"', subname, __LINE__)
                        end if

                        call mpas_dmpar_exch_halo_field(field_1d_real)

                        nullify(field_1d_real)
                    case (2)
                        call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                            trim(adjustl(field_name)), field_2d_real, timelevel=1)

                        if (.not. associated(field_2d_real)) then
                            call self % model_error('Failed to find field "' // trim(adjustl(field_name)) // &
                                '"', subname, __LINE__)
                        end if

                        call mpas_dmpar_exch_halo_field(field_2d_real)

                        nullify(field_2d_real)
                    case (3)
                        call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                            trim(adjustl(field_name)), field_3d_real, timelevel=1)

                        if (.not. associated(field_3d_real)) then
                            call self % model_error('Failed to find field "' // trim(adjustl(field_name)) // &
                                '"', subname, __LINE__)
                        end if

                        call mpas_dmpar_exch_halo_field(field_3d_real)

                        nullify(field_3d_real)
                    case (4)
                        call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                            trim(adjustl(field_name)), field_4d_real, timelevel=1)

                        if (.not. associated(field_4d_real)) then
                            call self % model_error('Failed to find field "' // trim(adjustl(field_name)) // &
                                '"', subname, __LINE__)
                        end if

                        call mpas_dmpar_exch_halo_field(field_4d_real)

                        nullify(field_4d_real)
                    case (5)
                        call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                            trim(adjustl(field_name)), field_5d_real, timelevel=1)

                        if (.not. associated(field_5d_real)) then
                            call self % model_error('Failed to find field "' // trim(adjustl(field_name)) // &
                                '"', subname, __LINE__)
                        end if

                        call mpas_dmpar_exch_halo_field(field_5d_real)

                        nullify(field_5d_real)
                    case default
                        call self % model_error('Unsupported field rank ' // stringify([mpas_pool_field_info % ndims]), &
                            subname, __LINE__)
                end select
            case default
                call self % model_error('Unsupported field type (Must be one of: integer, real)', subname, __LINE__)
        end select

        call self % debug_print(subname // ' completed')
    end subroutine dyn_mpas_exchange_halo

    !-------------------------------------------------------------------------------
    ! subroutine dyn_mpas_get_global_mesh_dimension
    !
    !> \brief  Returns global mesh dimensions
    !> \author Michael Duda
    !> \date   22 August 2019
    !> \details
    !>  This subroutine returns global mesh dimensions, including:
    !>  * Numbers of global mesh cells, edges, vertices and vertical levels
    !>    across all tasks.
    !>  * Maximum numbers of mesh cells and edges/vertices among all tasks.
    !>  * Sphere radius.
    !> \addenda
    !>  Ported and refactored for CAM-SIMA. (KCW, 2024-03-25)
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_get_global_mesh_dimension(self, &
            ncells_global, nedges_global, nvertices_global, nvertlevels, ncells_max, nedges_max, &
            sphere_radius)
        class(mpas_dynamical_core_type), intent(in) :: self
        integer, intent(out) :: ncells_global, nedges_global, nvertices_global, nvertlevels, ncells_max, nedges_max
        real(rkind), intent(out) :: sphere_radius

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_global_mesh_dimension'
        integer, pointer :: maxedges_pointer => null()
        integer, pointer :: ncellssolve_pointer => null()
        integer, pointer :: nedgessolve_pointer => null()
        integer, pointer :: nverticessolve_pointer => null()
        integer, pointer :: nvertlevels_pointer => null()

        call self % get_variable_pointer(maxedges_pointer, 'dim', 'maxEdges')
        call self % get_variable_pointer(ncellssolve_pointer, 'dim', 'nCellsSolve')
        call self % get_variable_pointer(nedgessolve_pointer, 'dim', 'nEdgesSolve')
        call self % get_variable_pointer(nverticessolve_pointer, 'dim', 'nVerticesSolve')
        call self % get_variable_pointer(nvertlevels_pointer, 'dim', 'nVertLevels')

        call mpas_dmpar_sum_int(self % domain_ptr % dminfo, ncellssolve_pointer, ncells_global)
        call mpas_dmpar_sum_int(self % domain_ptr % dminfo, nedgessolve_pointer, nedges_global)
        call mpas_dmpar_sum_int(self % domain_ptr % dminfo, nverticessolve_pointer, nvertices_global)

        ! Vertical levels are not decomposed.
        ! All tasks have the same number of vertical levels.
        nvertlevels = nvertlevels_pointer

        call mpas_dmpar_max_int(self % domain_ptr % dminfo, ncellssolve_pointer, ncells_max)

        nedges_max = maxedges_pointer
        sphere_radius = self % domain_ptr % sphere_radius

        nullify(maxedges_pointer)
        nullify(ncellssolve_pointer)
        nullify(nedgessolve_pointer)
        nullify(nverticessolve_pointer)
        nullify(nvertlevels_pointer)
    end subroutine dyn_mpas_get_global_mesh_dimension

    !> Helper subroutine for returning a pointer of `mpas_pool_type` to the named pool.
    !> It is used by the `dyn_mpas_get_variable_{pointer,value}_*` subroutines to draw a variable from a pool.
    !> (KCW, 2024-03-21)
    subroutine dyn_mpas_get_pool_pointer(self, pool_pointer, pool_name)
        class(mpas_dynamical_core_type), intent(in) :: self
        type(mpas_pool_type), pointer, intent(out) :: pool_pointer
        character(*), intent(in) :: pool_name

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_pool_pointer'

        nullify(pool_pointer)

        select case (trim(adjustl(pool_name)))
            case ('all')
                pool_pointer => self % domain_ptr % blocklist % allfields
            case ('cfg')
                pool_pointer => self % domain_ptr % configs
            case ('dim')
                pool_pointer => self % domain_ptr % blocklist % dimensions
            case ('diag', 'mesh', 'state', 'tend')
                call mpas_pool_get_subpool(self % domain_ptr % blocklist % allstructs, trim(adjustl(pool_name)), pool_pointer)
            case default
                call self % model_error('Unsupported pool name "' // trim(adjustl(pool_name)) // '"', subname, __LINE__)
        end select

        if (.not. associated(pool_pointer)) then
            call self % model_error('Failed to find pool "' // trim(adjustl(pool_name)) // '"', subname, __LINE__)
        end if
    end subroutine dyn_mpas_get_pool_pointer

    !-------------------------------------------------------------------------------
    ! subroutine dyn_mpas_get_variable_pointer_*
    !
    !> \brief  A family of accessor subroutines for MPAS dynamical core instance
    !> \author Kuan-Chih Wang
    !> \date   2024-03-21
    !> \details
    !>  The `dyn_mpas_get_variable_pointer_*` subroutines are a family of accessor
    !>  subroutines for drawing the REFERENCE of an internal variable from
    !>  MPAS dynamical core instance. The `get_variable_pointer` generic interface
    !>  should be used instead of the specific ones.
    !>  WARNING:
    !>  USE OF THIS SUBROUTINE FAMILY IS HIGHLY DISCOURAGED BECAUSE INTERNAL
    !>  STATES OF MPAS DYNAMICAL CORE INSTANCE COULD BE MODIFIED THROUGH THE
    !>  RETURNED POINTER. THESE ARE UNCHARTED WATERS SO BE SURE WHAT YOU ARE
    !>  DOING.
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_get_variable_pointer_c0(self, variable_pointer, pool_name, variable_name, time_level)
        class(mpas_dynamical_core_type), intent(in) :: self
        character(strkind), pointer, intent(out) :: variable_pointer
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_pointer_c0'
        type(mpas_pool_type), pointer :: mpas_pool => null()

        call self % get_pool_pointer(mpas_pool, pool_name)
        nullify(variable_pointer)

        if (trim(adjustl(pool_name)) == 'cfg') then
            ! Special case for config-related variables. They must be retrieved by calling `mpas_pool_get_config`.
            call mpas_pool_get_config(mpas_pool, trim(adjustl(variable_name)), variable_pointer)
        else
            call mpas_pool_get_array(mpas_pool, trim(adjustl(variable_name)), variable_pointer, timelevel=time_level)
        end if

        if (.not. associated(variable_pointer)) then
            call self % model_error('Failed to find variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(mpas_pool)
    end subroutine dyn_mpas_get_variable_pointer_c0

    subroutine dyn_mpas_get_variable_pointer_c1(self, variable_pointer, pool_name, variable_name, time_level)
        class(mpas_dynamical_core_type), intent(in) :: self
        character(strkind), pointer, intent(out) :: variable_pointer(:)
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_pointer_c1'
        type(mpas_pool_type), pointer :: mpas_pool => null()

        call self % get_pool_pointer(mpas_pool, pool_name)
        nullify(variable_pointer)
        call mpas_pool_get_array(mpas_pool, trim(adjustl(variable_name)), variable_pointer, timelevel=time_level)

        if (.not. associated(variable_pointer)) then
            call self % model_error('Failed to find variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(mpas_pool)
    end subroutine dyn_mpas_get_variable_pointer_c1

    subroutine dyn_mpas_get_variable_pointer_i0(self, variable_pointer, pool_name, variable_name, time_level)
        class(mpas_dynamical_core_type), intent(in) :: self
        integer, pointer, intent(out) :: variable_pointer
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_pointer_i0'
        type(mpas_pool_type), pointer :: mpas_pool => null()

        call self % get_pool_pointer(mpas_pool, pool_name)
        nullify(variable_pointer)

        if (trim(adjustl(pool_name)) == 'cfg') then
            ! Special case for config-related variables. They must be retrieved by calling `mpas_pool_get_config`.
            call mpas_pool_get_config(mpas_pool, trim(adjustl(variable_name)), variable_pointer)
        else if (trim(adjustl(pool_name)) == 'dim') then
            ! Special case for dimension-related variables. They must be retrieved by calling `mpas_pool_get_dimension`.
            call mpas_pool_get_dimension(mpas_pool, trim(adjustl(variable_name)), variable_pointer)
        else
            call mpas_pool_get_array(mpas_pool, trim(adjustl(variable_name)), variable_pointer, timelevel=time_level)
        end if

        if (.not. associated(variable_pointer)) then
            call self % model_error('Failed to find variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(mpas_pool)
    end subroutine dyn_mpas_get_variable_pointer_i0

    subroutine dyn_mpas_get_variable_pointer_i1(self, variable_pointer, pool_name, variable_name, time_level)
        class(mpas_dynamical_core_type), intent(in) :: self
        integer, pointer, intent(out) :: variable_pointer(:)
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_pointer_i1'
        type(mpas_pool_type), pointer :: mpas_pool => null()

        call self % get_pool_pointer(mpas_pool, pool_name)
        nullify(variable_pointer)

        if (trim(adjustl(pool_name)) == 'dim') then
            ! Special case for dimension-related variables. They must be retrieved by calling `mpas_pool_get_dimension`.
            call mpas_pool_get_dimension(mpas_pool, trim(adjustl(variable_name)), variable_pointer)
        else
            call mpas_pool_get_array(mpas_pool, trim(adjustl(variable_name)), variable_pointer, timelevel=time_level)
        end if

        if (.not. associated(variable_pointer)) then
            call self % model_error('Failed to find variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(mpas_pool)
    end subroutine dyn_mpas_get_variable_pointer_i1

    subroutine dyn_mpas_get_variable_pointer_i2(self, variable_pointer, pool_name, variable_name, time_level)
        class(mpas_dynamical_core_type), intent(in) :: self
        integer, pointer, intent(out) :: variable_pointer(:, :)
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_pointer_i2'
        type(mpas_pool_type), pointer :: mpas_pool => null()

        call self % get_pool_pointer(mpas_pool, pool_name)
        nullify(variable_pointer)
        call mpas_pool_get_array(mpas_pool, trim(adjustl(variable_name)), variable_pointer, timelevel=time_level)

        if (.not. associated(variable_pointer)) then
            call self % model_error('Failed to find variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(mpas_pool)
    end subroutine dyn_mpas_get_variable_pointer_i2

    subroutine dyn_mpas_get_variable_pointer_i3(self, variable_pointer, pool_name, variable_name, time_level)
        class(mpas_dynamical_core_type), intent(in) :: self
        integer, pointer, intent(out) :: variable_pointer(:, :, :)
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_pointer_i3'
        type(mpas_pool_type), pointer :: mpas_pool => null()

        call self % get_pool_pointer(mpas_pool, pool_name)
        nullify(variable_pointer)
        call mpas_pool_get_array(mpas_pool, trim(adjustl(variable_name)), variable_pointer, timelevel=time_level)

        if (.not. associated(variable_pointer)) then
            call self % model_error('Failed to find variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(mpas_pool)
    end subroutine dyn_mpas_get_variable_pointer_i3

    subroutine dyn_mpas_get_variable_pointer_l0(self, variable_pointer, pool_name, variable_name, time_level)
        class(mpas_dynamical_core_type), intent(in) :: self
        logical, pointer, intent(out) :: variable_pointer
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_pointer_l0'
        type(mpas_pool_type), pointer :: mpas_pool => null()

        call self % get_pool_pointer(mpas_pool, pool_name)
        nullify(variable_pointer)

        if (trim(adjustl(pool_name)) == 'cfg') then
            ! Special case for config-related variables. They must be retrieved by calling `mpas_pool_get_config`.
            call mpas_pool_get_config(mpas_pool, trim(adjustl(variable_name)), variable_pointer)
        end if

        if (.not. associated(variable_pointer)) then
            call self % model_error('Failed to find variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(mpas_pool)
    end subroutine dyn_mpas_get_variable_pointer_l0

    subroutine dyn_mpas_get_variable_pointer_r0(self, variable_pointer, pool_name, variable_name, time_level)
        class(mpas_dynamical_core_type), intent(in) :: self
        real(rkind), pointer, intent(out) :: variable_pointer
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_pointer_r0'
        type(mpas_pool_type), pointer :: mpas_pool => null()

        call self % get_pool_pointer(mpas_pool, pool_name)
        nullify(variable_pointer)

        if (trim(adjustl(pool_name)) == 'cfg') then
            ! Special case for config-related variables. They must be retrieved by calling `mpas_pool_get_config`.
            call mpas_pool_get_config(mpas_pool, trim(adjustl(variable_name)), variable_pointer)
        else
            call mpas_pool_get_array(mpas_pool, trim(adjustl(variable_name)), variable_pointer, timelevel=time_level)
        end if

        if (.not. associated(variable_pointer)) then
            call self % model_error('Failed to find variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(mpas_pool)
    end subroutine dyn_mpas_get_variable_pointer_r0

    subroutine dyn_mpas_get_variable_pointer_r1(self, variable_pointer, pool_name, variable_name, time_level)
        class(mpas_dynamical_core_type), intent(in) :: self
        real(rkind), pointer, intent(out) :: variable_pointer(:)
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_pointer_r1'
        type(mpas_pool_type), pointer :: mpas_pool => null()

        call self % get_pool_pointer(mpas_pool, pool_name)
        nullify(variable_pointer)
        call mpas_pool_get_array(mpas_pool, trim(adjustl(variable_name)), variable_pointer, timelevel=time_level)

        if (.not. associated(variable_pointer)) then
            call self % model_error('Failed to find variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(mpas_pool)
    end subroutine dyn_mpas_get_variable_pointer_r1

    subroutine dyn_mpas_get_variable_pointer_r2(self, variable_pointer, pool_name, variable_name, time_level)
        class(mpas_dynamical_core_type), intent(in) :: self
        real(rkind), pointer, intent(out) :: variable_pointer(:, :)
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_pointer_r2'
        type(mpas_pool_type), pointer :: mpas_pool => null()

        call self % get_pool_pointer(mpas_pool, pool_name)
        nullify(variable_pointer)
        call mpas_pool_get_array(mpas_pool, trim(adjustl(variable_name)), variable_pointer, timelevel=time_level)

        if (.not. associated(variable_pointer)) then
            call self % model_error('Failed to find variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(mpas_pool)
    end subroutine dyn_mpas_get_variable_pointer_r2

    subroutine dyn_mpas_get_variable_pointer_r3(self, variable_pointer, pool_name, variable_name, time_level)
        class(mpas_dynamical_core_type), intent(in) :: self
        real(rkind), pointer, intent(out) :: variable_pointer(:, :, :)
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_pointer_r3'
        type(mpas_pool_type), pointer :: mpas_pool => null()

        call self % get_pool_pointer(mpas_pool, pool_name)
        nullify(variable_pointer)
        call mpas_pool_get_array(mpas_pool, trim(adjustl(variable_name)), variable_pointer, timelevel=time_level)

        if (.not. associated(variable_pointer)) then
            call self % model_error('Failed to find variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(mpas_pool)
    end subroutine dyn_mpas_get_variable_pointer_r3

    subroutine dyn_mpas_get_variable_pointer_r4(self, variable_pointer, pool_name, variable_name, time_level)
        class(mpas_dynamical_core_type), intent(in) :: self
        real(rkind), pointer, intent(out) :: variable_pointer(:, :, :, :)
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_pointer_r4'
        type(mpas_pool_type), pointer :: mpas_pool => null()

        call self % get_pool_pointer(mpas_pool, pool_name)
        nullify(variable_pointer)
        call mpas_pool_get_array(mpas_pool, trim(adjustl(variable_name)), variable_pointer, timelevel=time_level)

        if (.not. associated(variable_pointer)) then
            call self % model_error('Failed to find variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(mpas_pool)
    end subroutine dyn_mpas_get_variable_pointer_r4

    subroutine dyn_mpas_get_variable_pointer_r5(self, variable_pointer, pool_name, variable_name, time_level)
        class(mpas_dynamical_core_type), intent(in) :: self
        real(rkind), pointer, intent(out) :: variable_pointer(:, :, :, :, :)
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_pointer_r5'
        type(mpas_pool_type), pointer :: mpas_pool => null()

        call self % get_pool_pointer(mpas_pool, pool_name)
        nullify(variable_pointer)
        call mpas_pool_get_array(mpas_pool, trim(adjustl(variable_name)), variable_pointer, timelevel=time_level)

        if (.not. associated(variable_pointer)) then
            call self % model_error('Failed to find variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(mpas_pool)
    end subroutine dyn_mpas_get_variable_pointer_r5

    !-------------------------------------------------------------------------------
    ! subroutine dyn_mpas_get_variable_value_*
    !
    !> \brief  A family of accessor subroutines for MPAS dynamical core instance
    !> \author Kuan-Chih Wang
    !> \date   2024-03-21
    !> \details
    !>  The `dyn_mpas_get_variable_value_*` subroutines are a family of accessor
    !>  subroutines for drawing the VALUE of an internal variable from
    !>  MPAS dynamical core instance. The `get_variable_value` generic interface
    !>  should be used instead of the specific ones.
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_get_variable_value_c0(self, variable_value, pool_name, variable_name, time_level)
        class(mpas_dynamical_core_type), intent(in) :: self
        character(strkind), allocatable, intent(out) :: variable_value
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_value_c0'
        character(strkind), pointer :: variable_pointer => null()
        integer :: ierr

        call self % get_variable_pointer(variable_pointer, pool_name, variable_name, time_level=time_level)
        allocate(variable_value, source=variable_pointer, stat=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to allocate variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(variable_pointer)
    end subroutine dyn_mpas_get_variable_value_c0

    subroutine dyn_mpas_get_variable_value_c1(self, variable_value, pool_name, variable_name, time_level)
        class(mpas_dynamical_core_type), intent(in) :: self
        character(strkind), allocatable, intent(out) :: variable_value(:)
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_value_c1'
        character(strkind), pointer :: variable_pointer(:) => null()
        integer :: ierr

        call self % get_variable_pointer(variable_pointer, pool_name, variable_name, time_level=time_level)
        allocate(variable_value, source=variable_pointer, stat=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to allocate variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(variable_pointer)
    end subroutine dyn_mpas_get_variable_value_c1

    subroutine dyn_mpas_get_variable_value_i0(self, variable_value, pool_name, variable_name, time_level)
        class(mpas_dynamical_core_type), intent(in) :: self
        integer, allocatable, intent(out) :: variable_value
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_value_i0'
        integer, pointer :: variable_pointer => null()
        integer :: ierr

        call self % get_variable_pointer(variable_pointer, pool_name, variable_name, time_level=time_level)
        allocate(variable_value, source=variable_pointer, stat=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to allocate variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(variable_pointer)
    end subroutine dyn_mpas_get_variable_value_i0

    subroutine dyn_mpas_get_variable_value_i1(self, variable_value, pool_name, variable_name, time_level)
        class(mpas_dynamical_core_type), intent(in) :: self
        integer, allocatable, intent(out) :: variable_value(:)
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_value_i1'
        integer, pointer :: variable_pointer(:) => null()
        integer :: ierr

        call self % get_variable_pointer(variable_pointer, pool_name, variable_name, time_level=time_level)
        allocate(variable_value, source=variable_pointer, stat=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to allocate variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(variable_pointer)
    end subroutine dyn_mpas_get_variable_value_i1

    subroutine dyn_mpas_get_variable_value_i2(self, variable_value, pool_name, variable_name, time_level)
        class(mpas_dynamical_core_type), intent(in) :: self
        integer, allocatable, intent(out) :: variable_value(:, :)
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_value_i2'
        integer, pointer :: variable_pointer(:, :) => null()
        integer :: ierr

        call self % get_variable_pointer(variable_pointer, pool_name, variable_name, time_level=time_level)
        allocate(variable_value, source=variable_pointer, stat=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to allocate variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(variable_pointer)
    end subroutine dyn_mpas_get_variable_value_i2

    subroutine dyn_mpas_get_variable_value_i3(self, variable_value, pool_name, variable_name, time_level)
        class(mpas_dynamical_core_type), intent(in) :: self
        integer, allocatable, intent(out) :: variable_value(:, :, :)
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_value_i3'
        integer, pointer :: variable_pointer(:, :, :) => null()
        integer :: ierr

        call self % get_variable_pointer(variable_pointer, pool_name, variable_name, time_level=time_level)
        allocate(variable_value, source=variable_pointer, stat=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to allocate variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(variable_pointer)
    end subroutine dyn_mpas_get_variable_value_i3

    subroutine dyn_mpas_get_variable_value_l0(self, variable_value, pool_name, variable_name, time_level)
        class(mpas_dynamical_core_type), intent(in) :: self
        logical, allocatable, intent(out) :: variable_value
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_value_l0'
        logical, pointer :: variable_pointer => null()
        integer :: ierr

        call self % get_variable_pointer(variable_pointer, pool_name, variable_name, time_level=time_level)
        allocate(variable_value, source=variable_pointer, stat=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to allocate variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(variable_pointer)
    end subroutine dyn_mpas_get_variable_value_l0

    subroutine dyn_mpas_get_variable_value_r0(self, variable_value, pool_name, variable_name, time_level)
        class(mpas_dynamical_core_type), intent(in) :: self
        real(rkind), allocatable, intent(out) :: variable_value
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_value_r0'
        real(rkind), pointer :: variable_pointer => null()
        integer :: ierr

        call self % get_variable_pointer(variable_pointer, pool_name, variable_name, time_level=time_level)
        allocate(variable_value, source=variable_pointer, stat=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to allocate variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(variable_pointer)
    end subroutine dyn_mpas_get_variable_value_r0

    subroutine dyn_mpas_get_variable_value_r1(self, variable_value, pool_name, variable_name, time_level)
        class(mpas_dynamical_core_type), intent(in) :: self
        real(rkind), allocatable, intent(out) :: variable_value(:)
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_value_r1'
        real(rkind), pointer :: variable_pointer(:) => null()
        integer :: ierr

        call self % get_variable_pointer(variable_pointer, pool_name, variable_name, time_level=time_level)
        allocate(variable_value, source=variable_pointer, stat=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to allocate variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(variable_pointer)
    end subroutine dyn_mpas_get_variable_value_r1

    subroutine dyn_mpas_get_variable_value_r2(self, variable_value, pool_name, variable_name, time_level)
        class(mpas_dynamical_core_type), intent(in) :: self
        real(rkind), allocatable, intent(out) :: variable_value(:, :)
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_value_r2'
        real(rkind), pointer :: variable_pointer(:, :) => null()
        integer :: ierr

        call self % get_variable_pointer(variable_pointer, pool_name, variable_name, time_level=time_level)
        allocate(variable_value, source=variable_pointer, stat=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to allocate variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(variable_pointer)
    end subroutine dyn_mpas_get_variable_value_r2

    subroutine dyn_mpas_get_variable_value_r3(self, variable_value, pool_name, variable_name, time_level)
        class(mpas_dynamical_core_type), intent(in) :: self
        real(rkind), allocatable, intent(out) :: variable_value(:, :, :)
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_value_r3'
        real(rkind), pointer :: variable_pointer(:, :, :) => null()
        integer :: ierr

        call self % get_variable_pointer(variable_pointer, pool_name, variable_name, time_level=time_level)
        allocate(variable_value, source=variable_pointer, stat=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to allocate variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(variable_pointer)
    end subroutine dyn_mpas_get_variable_value_r3

    subroutine dyn_mpas_get_variable_value_r4(self, variable_value, pool_name, variable_name, time_level)
        class(mpas_dynamical_core_type), intent(in) :: self
        real(rkind), allocatable, intent(out) :: variable_value(:, :, :, :)
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_value_r4'
        real(rkind), pointer :: variable_pointer(:, :, :, :) => null()
        integer :: ierr

        call self % get_variable_pointer(variable_pointer, pool_name, variable_name, time_level=time_level)
        allocate(variable_value, source=variable_pointer, stat=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to allocate variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(variable_pointer)
    end subroutine dyn_mpas_get_variable_value_r4

    subroutine dyn_mpas_get_variable_value_r5(self, variable_value, pool_name, variable_name, time_level)
        class(mpas_dynamical_core_type), intent(in) :: self
        real(rkind), allocatable, intent(out) :: variable_value(:, :, :, :, :)
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_value_r5'
        real(rkind), pointer :: variable_pointer(:, :, :, :, :) => null()
        integer :: ierr

        call self % get_variable_pointer(variable_pointer, pool_name, variable_name, time_level=time_level)
        allocate(variable_value, source=variable_pointer, stat=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to allocate variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(variable_pointer)
    end subroutine dyn_mpas_get_variable_value_r5
end module dyn_mpas_subdriver
