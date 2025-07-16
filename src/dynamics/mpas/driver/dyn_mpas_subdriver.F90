! Copyright (C) 2025 University Corporation for Atmospheric Research (UCAR)
! SPDX-License-Identifier: Apache-2.0

!> This module, the MPAS subdriver, manages the life cycle (i.e., initialization, running, and
!> finalization) of MPAS as a dynamical core within CAM-SIMA as well as potentially other
!> host models.
!>
!> It is a ground-up implementation that not only adheres to the Fortran 2018 standard, but also
!> incorporates a modern object-oriented design. As such, the implementation details of MPAS are
!> abstracted away from CAM-SIMA, which enables a more stable interface between the two.
!>
!> Users should begin by creating an "instance" of MPAS dynamical core from the `mpas_dynamical_core_type`
!> derived type. Then, interaction with the instance is done through its public type-bound procedures.
!> Developers wishing to integrate MPAS dynamical core into other host models could take advantage of
!> the object-oriented design to add new functionalities or modify existing ones simply by extending
!> the `mpas_dynamical_core_type` derived type.
module dyn_mpas_subdriver
    use, intrinsic :: iso_fortran_env, only: output_unit
    ! Module(s) from external libraries.
#ifdef MPAS_USE_MPI_F08
    use mpi_f08, only: mpi_comm_null, mpi_comm_rank, mpi_success, &
                       mpi_comm_type => mpi_comm, operator(==)
#else
    use mpi, only: mpi_comm_null, mpi_comm_rank, mpi_success
#endif
    ! Module(s) from MPAS.
    use mpas_derived_types, only: core_type, domain_type
    use mpas_kind_types, only: rkind, strkind

    implicit none

    private
    public :: mpas_dynamical_core_real_kind
    public :: mpas_dynamical_core_type

    abstract interface
        !> This procedure interface is modeled after the `endrun` subroutine from CAM-SIMA.
        !> It will be called whenever MPAS dynamical core encounters a fatal error and cannot continue.
        subroutine model_error_if(message, file, line)
            character(*),           intent(in) :: message
            character(*), optional, intent(in) :: file
            integer,      optional, intent(in) :: line
        end subroutine model_error_if
    end interface

    ! The supported log levels of MPAS dynamical core.

    !> Log nothing.
    integer, parameter :: log_level_quiet = 0
    !> Log plain and user-friendly information about the status of MPAS dynamical core.
    !> Public procedures should start with this log level.
    integer, parameter :: log_level_info = 1
    !> Same as the above, but for private procedures.
    integer, parameter :: log_level_verbose = 2
    !> Log elaborate information about the inner workings of MPAS dynamical core, which may be useful for diagnosing issues.
    !> However, the log volume may be very large.
    integer, parameter :: log_level_debug = 3

    !> The native floating-point precision of MPAS dynamical core.
    integer, parameter :: mpas_dynamical_core_real_kind = rkind

    !> The "class" of MPAS dynamical core.
    !> Important data structures like the internal states of MPAS dynamical core are encapsulated inside this derived type
    !> to prevent misuse. Type-bound procedures provide stable and well-defined APIs for CAM-SIMA to interact with
    !> MPAS dynamical core.
    type :: mpas_dynamical_core_type
        private

        ! Initialized by `dyn_mpas_init_phase1`.
        integer :: log_level = log_level_quiet
        integer :: log_unit = output_unit
#ifdef MPAS_USE_MPI_F08
        type(mpi_comm_type) :: mpi_comm = mpi_comm_null
#else
        integer :: mpi_comm = mpi_comm_null
#endif
        integer :: mpi_rank = 0
        logical :: mpi_rank_root = .false.

        ! Actual implementation is supplied at run-time.
        procedure(model_error_if), nopass, pointer :: model_error => null()

        type(core_type), pointer :: corelist => null()
        type(domain_type), pointer :: domain_ptr => null()

        ! Initialized by `dyn_mpas_init_phase3`.
        integer :: number_of_constituents = 0

        ! Initialized by `dyn_mpas_define_scalar`.
        character(strkind), allocatable :: constituent_name(:)
        integer, allocatable :: index_constituent_to_mpas_scalar(:)
        integer, allocatable :: index_mpas_scalar_to_constituent(:)
        logical, allocatable :: is_water_species(:)

        ! Initialized by `dyn_mpas_init_phase4`.
        integer :: coupling_time_interval = 0
        integer :: number_of_time_steps = 0
    contains
        private

        procedure, pass, public :: debug_print => dyn_mpas_debug_print
        procedure, pass, public :: init_phase1 => dyn_mpas_init_phase1
        procedure, pass, public :: read_namelist => dyn_mpas_read_namelist
        procedure, pass, public :: init_phase2 => dyn_mpas_init_phase2
        procedure, pass, public :: init_phase3 => dyn_mpas_init_phase3
        procedure, pass, public :: define_scalar => dyn_mpas_define_scalar
        procedure, pass, public :: read_write_stream => dyn_mpas_read_write_stream
        procedure, pass :: init_stream_with_pool => dyn_mpas_init_stream_with_pool
        procedure, pass :: check_variable_status => dyn_mpas_check_variable_status
        procedure, pass, public :: exchange_halo => dyn_mpas_exchange_halo
        procedure, pass, public :: compute_unit_vector => dyn_mpas_compute_unit_vector
        procedure, pass, public :: compute_edge_wind => dyn_mpas_compute_edge_wind
        procedure, pass, public :: compute_cell_relative_vorticity => dyn_mpas_compute_cell_relative_vorticity
        procedure, pass, public :: init_phase4 => dyn_mpas_init_phase4
        procedure, pass, public :: run => dyn_mpas_run
        procedure, pass, public :: final => dyn_mpas_final

        ! Accessor procedures for users to access the internal states of MPAS dynamical core.

        procedure, pass, public :: get_constituent_name => dyn_mpas_get_constituent_name
        procedure, pass, public :: get_constituent_index => dyn_mpas_get_constituent_index

        procedure, pass, public :: map_mpas_scalar_index => dyn_mpas_map_mpas_scalar_index
        procedure, pass, public :: map_constituent_index => dyn_mpas_map_constituent_index

        procedure, pass, public :: get_local_mesh_dimension => dyn_mpas_get_local_mesh_dimension
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
    !> ```
    !> <var name="xCell" type="real" dimensions="nCells" units="m" description="Cartesian x-coordinate of cells" />
    !> ```
    !> Here, it is described as:
    !> ```
    !> var_info_type(name="xCell", type="real", rank=1)
    !> ```
    !> However, note that MPAS treats the "Time" dimension specially. It is implemented as 1-d pointer arrays of
    !> custom derived types. For a variable with the "Time" dimension, its rank needs to be subtracted by one.
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
    ! The remaining ones of interest should be in output.

    !> This list corresponds to the "input" stream in MPAS registry.
    !> It consists of variables that are members of the "diag" and "state" struct.
    !> Only variables that are specific to the "input" stream are included.
    type(var_info_type), parameter :: input_var_info_list(*) = [ &
        var_info_type('Time'                            , 'real'      , 0), &
        var_info_type('initial_time'                    , 'character' , 0), &
        var_info_type('rho'                             , 'real'      , 2), &
        var_info_type('rho_base'                        , 'real'      , 2), &
        var_info_type('scalars'                         , 'real'      , 3), &
        var_info_type('theta'                           , 'real'      , 2), &
        var_info_type('theta_base'                      , 'real'      , 2), &
        var_info_type('u'                               , 'real'      , 2), &
        var_info_type('w'                               , 'real'      , 2), &
        var_info_type('xtime'                           , 'character' , 0)  &
    ]

    !> This list corresponds to the "restart" stream in MPAS registry.
    !> It consists of variables that are members of the "diag" and "state" struct.
    !> Only variables that are specific to the "restart" stream are included.
    type(var_info_type), parameter :: restart_var_info_list(*) = [ &
        var_info_type('exner'                           , 'real'      , 2), &
        var_info_type('exner_base'                      , 'real'      , 2), &
        var_info_type('pressure_base'                   , 'real'      , 2), &
        var_info_type('pressure_p'                      , 'real'      , 2), &
        var_info_type('rho_p'                           , 'real'      , 2), &
        var_info_type('rho_zz'                          , 'real'      , 2), &
        var_info_type('rtheta_base'                     , 'real'      , 2), &
        var_info_type('rtheta_p'                        , 'real'      , 2), &
        var_info_type('ru'                              , 'real'      , 2), &
        var_info_type('ru_p'                            , 'real'      , 2), &
        var_info_type('rw'                              , 'real'      , 2), &
        var_info_type('rw_p'                            , 'real'      , 2), &
        var_info_type('theta_m'                         , 'real'      , 2)  &
    ]

    !> This list corresponds to the "output" stream in MPAS registry.
    !> It consists of variables that are members of the "diag" struct.
    !> Only variables that are specific to the "output" stream are included.
    type(var_info_type), parameter :: output_var_info_list(*) = [ &
        var_info_type('divergence'                      , 'real'      , 2), &
        var_info_type('pressure'                        , 'real'      , 2), &
        var_info_type('relhum'                          , 'real'      , 2), &
        var_info_type('surface_pressure'                , 'real'      , 1), &
        var_info_type('uReconstructMeridional'          , 'real'      , 2), &
        var_info_type('uReconstructZonal'               , 'real'      , 2), &
        var_info_type('vorticity'                       , 'real'      , 2)  &
    ]
contains
    !-------------------------------------------------------------------------------
    ! subroutine dyn_mpas_debug_print
    !
    !> summary: Print a debug message at a debug level.
    !> author: Kuan-Chih Wang
    !> date: 2024-02-03
    !>
    !> This subroutine prints a debug message at a debug level. The debug message
    !> will be prefixed by "MPAS Subdriver (N): ", where `N` is the MPI rank. The
    !> debug level is one of the `log_level_*` constants.
    !> If `printer` is not supplied, the MPI root rank will print. Otherwise,
    !> the designated MPI rank will print instead.
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_debug_print(self, level, message, printer)
        class(mpas_dynamical_core_type), intent(in) :: self
        integer, intent(in) :: level
        character(*), intent(in) :: message
        integer, optional, intent(in) :: printer

        ! Bail out early if the log level is less verbose than the debug level.
        if (self % log_level < level) then
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

        write(self % log_unit, '(a)') 'MPAS Subdriver (' // stringify([self % mpi_rank]) // '): ' // message
    end subroutine dyn_mpas_debug_print

    !> Convert one or more values of any intrinsic data types to a character string for pretty printing.
    !> If `value` contains more than one element, the elements will be stringified, delimited by `separator`, then concatenated.
    !> If `value` contains exactly one element, the element will be stringified without using `separator`.
    !> If `value` contains zero element or is of unsupported data types, an empty character string is produced.
    !> If `separator` is not supplied, it defaults to ", " (i.e., a comma and a space).
    !> (KCW, 2024-02-04)
    pure function stringify(value, separator)
        use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64

        class(*), intent(in) :: value(:)
        character(*), optional, intent(in) :: separator
        character(:), allocatable :: stringify

        integer, parameter :: sizelimit = 1024

        character(:), allocatable :: buffer, delimiter, format
        character(:), allocatable :: value_c(:)
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

                ! Workaround for a bug in GNU Fortran >= 12. This is perhaps the manifestation of GCC Bugzilla Bug 100819.
                ! When a character string array is passed as the actual argument to an unlimited polymorphic dummy argument,
                ! its array index and length parameter are mishandled.
                allocate(character(len(value)) :: value_c(size(value)))

                value_c(:) = value(:)

                do i = 1, n
                    if (len(delimiter) > 0 .and. i > 1) then
                        buffer(offset + 1:offset + len(delimiter)) = delimiter
                        offset = offset + len(delimiter)
                    end if

                    if (len_trim(adjustl(value_c(i))) > 0) then
                        buffer(offset + 1:offset + len_trim(adjustl(value_c(i)))) = trim(adjustl(value_c(i)))
                        offset = offset + len_trim(adjustl(value_c(i)))
                    end if
                end do

                deallocate(value_c)
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
    !> summary: Track `mpas_init` up to the point of reading namelist.
    !> author: Michael Duda
    !> date: 19 April 2019
    !>
    !> This subroutine follows the stand-alone MPAS subdriver up to, but not
    !> including, the point where namelist is read.
    !> Ported and refactored for CAM-SIMA. (KCW, 2024-02-02)
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_init_phase1(self, mpi_comm, model_error_impl, log_level, log_unit, mpas_log_unit)
        ! Module(s) from MPAS.
        use atm_core_interface, only: atm_setup_core, atm_setup_domain
        use mpas_domain_routines, only: mpas_allocate_domain
        use mpas_framework, only: mpas_framework_init_phase1

        class(mpas_dynamical_core_type), intent(inout) :: self
#ifdef MPAS_USE_MPI_F08
        type(mpi_comm_type), intent(in) :: mpi_comm
#else
        integer, intent(in) :: mpi_comm
#endif
        procedure(model_error_if) :: model_error_impl
        integer, intent(in) :: log_level
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
        self % log_level = max(min(log_level, log_level_debug), log_level_quiet)
        self % log_unit = log_unit

        call self % debug_print(log_level_debug, subname // ' entered')

        call self % debug_print(log_level_info, 'Allocating core')

        allocate(self % corelist, stat=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to allocate corelist', subname, __LINE__)
        end if

        nullify(self % corelist % next)

        call self % debug_print(log_level_info, 'Allocating domain')

        allocate(self % corelist % domainlist, stat=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to allocate corelist % domainlist', subname, __LINE__)
        end if

        nullify(self % corelist % domainlist % next)

        self % domain_ptr => self % corelist % domainlist
        self % domain_ptr % core => self % corelist

        call mpas_allocate_domain(self % domain_ptr)

        self % domain_ptr % domainid = 0

        call self % debug_print(log_level_info, 'Initializing MPAS framework (Phase 1/2)')

        ! Initialize MPAS framework with the supplied MPI communicator group.
        call mpas_framework_init_phase1(self % domain_ptr % dminfo, external_comm=self % mpi_comm)

        call self % debug_print(log_level_info, 'Setting up core')

        call atm_setup_core(self % corelist)

        call self % debug_print(log_level_info, 'Setting up domain')

        call atm_setup_domain(self % domain_ptr)

        call self % debug_print(log_level_info, 'Setting up log')

        ! Set up the log manager as early as possible so we can use it for any errors/messages during subsequent
        ! initialization steps.
        !
        ! We need:
        ! 1. `domain_ptr` to be allocated;
        ! 2. `dmpar_init` to be completed for accessing `dminfo`;
        ! 3. `*_setup_core` to assign the `setup_log` procedure pointer.
        ierr = self % domain_ptr % core % setup_log(self % domain_ptr % loginfo, self % domain_ptr, unitnumbers=mpas_log_unit)

        if (ierr /= 0) then
            call self % model_error('Log setup failed for core ' // trim(self % domain_ptr % core % corename), &
                subname, __LINE__)
        end if

        ! At this point, we should be ready to read namelist in `dyn_mpas_read_namelist`.
        call self % debug_print(log_level_debug, subname // ' completed')
    end subroutine dyn_mpas_init_phase1

    !-------------------------------------------------------------------------------
    ! subroutine dyn_mpas_read_namelist
    !
    !> summary: Track `mpas_init` where namelist is being read.
    !> author: Kuan-Chih Wang
    !> date: 2024-02-09
    !>
    !> This subroutine calls upstream MPAS functionality for reading its own
    !> namelist. After that, override designated namelist variables according to
    !> the information provided from CAM-SIMA.
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
        character(strkind), pointer :: config_pointer_c
        integer :: ierr
        logical, pointer :: config_pointer_l

        call self % debug_print(log_level_debug, subname // ' entered')

        nullify(config_pointer_c)
        nullify(config_pointer_l)

        call self % debug_print(log_level_info, 'Reading namelist at "' // trim(adjustl(namelist_path)) // '"')

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

        ! Override designated namelist variables according to the information provided from CAM-SIMA.
        ! These include run-time settings that cannot be determined beforehand.

        call self % debug_print(log_level_info, 'Overriding designated namelist variables')

        ! CAM-SIMA seems to follow "NetCDF Climate and Forecast (CF) Metadata Conventions" for calendar names. See
        ! CF-1.12, section "4.4.2. Calendar", in doi:10.5281/zenodo.14275599.
        ! However, this is not the case for MPAS. Translate calendar names between CF and MPAS.
        select case (trim(adjustl(cf_calendar)))
            case ('360_day')
                mpas_calendar = '360day'
            case ('365_day', 'noleap')
                mpas_calendar = 'gregorian_noleap'
            case ('gregorian', 'standard')
                ! "gregorian" is a deprecated alternative name for "standard".
                mpas_calendar = 'gregorian'
            case default
                call self % model_error('Unsupported calendar type "' // trim(adjustl(cf_calendar)) // '"', &
                    subname, __LINE__)
        end select

        call self % get_variable_pointer(config_pointer_c, 'cfg', 'config_calendar_type')

        config_pointer_c = trim(adjustl(mpas_calendar))
        call self % debug_print(log_level_debug, 'config_calendar_type = ' // trim(config_pointer_c))
        nullify(config_pointer_c)

        ! MPAS represents date and time in ISO 8601 format. However, the separator between date and time is "_"
        ! instead of standard "T".
        ! Format in "YYYY-MM-DD_hh:mm:ss" is acceptable.
        call self % get_variable_pointer(config_pointer_c, 'cfg', 'config_start_time')

        config_pointer_c = stringify(start_date_time(1:3), '-') // '_' // stringify(start_date_time(4:6), ':')
        call self % debug_print(log_level_debug, 'config_start_time = ' // trim(config_pointer_c))
        nullify(config_pointer_c)

        call self % get_variable_pointer(config_pointer_c, 'cfg', 'config_stop_time')

        config_pointer_c = stringify(stop_date_time(1:3), '-') // '_' // stringify(stop_date_time(4:6), ':')
        call self % debug_print(log_level_debug, 'config_stop_time = ' // trim(config_pointer_c))
        nullify(config_pointer_c)

        ! Format in "DD_hh:mm:ss" is acceptable.
        call self % get_variable_pointer(config_pointer_c, 'cfg', 'config_run_duration')

        config_pointer_c = stringify([run_duration(1)]) // '_' // stringify(run_duration(2:4), ':')
        call self % debug_print(log_level_debug, 'config_run_duration = ' // trim(config_pointer_c))
        nullify(config_pointer_c)

        ! Reflect current run type to MPAS.
        call self % get_variable_pointer(config_pointer_l, 'cfg', 'config_do_restart')

        if (initial_run) then
            ! Run type is initial run.
            config_pointer_l = .false.
        else
            ! Run type is branch or restart run.
            config_pointer_l = .true.
        end if

        call self % debug_print(log_level_debug, 'config_do_restart = ' // stringify([config_pointer_l]))
        nullify(config_pointer_l)

        ! At this point, we should be ready to follow up with the rest of MPAS framework initialization
        ! in `dyn_mpas_init_phase2`.
        call self % debug_print(log_level_debug, subname // ' completed')
    end subroutine dyn_mpas_read_namelist

    !-------------------------------------------------------------------------------
    ! subroutine dyn_mpas_init_phase2
    !
    !> summary: Track `mpas_init` after namelist has been read.
    !> author: Michael Duda
    !> date: 19 April 2019
    !>
    !> This subroutine follows the stand-alone MPAS subdriver from the point
    !> where we call the second phase of MPAS framework initialization up
    !> to the check on the existence of the "streams.<core>" file.
    !> Ported and refactored for CAM-SIMA. (KCW, 2024-02-07)
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_init_phase2(self, pio_iosystem)
        ! Module(s) from external libraries.
        use pio, only: iosystem_desc_t, pio_iosystem_is_active
        ! Module(s) from MPAS.
        use mpas_framework, only: mpas_framework_init_phase2
        use mpas_stream_inquiry, only: mpas_stream_inquiry_new_streaminfo

        class(mpas_dynamical_core_type), intent(in) :: self
        type(iosystem_desc_t), pointer, intent(in) :: pio_iosystem

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_init_phase2'
        integer :: ierr
        logical :: pio_iosystem_active

        call self % debug_print(log_level_debug, subname // ' entered')

        call self % debug_print(log_level_info, 'Checking PIO system descriptor')

        if (.not. associated(pio_iosystem)) then
            call self % model_error('Invalid PIO system descriptor', subname, __LINE__)
        end if

        call pio_iosystem_is_active(pio_iosystem, pio_iosystem_active)

        if (.not. pio_iosystem_active) then
            call self % model_error('Invalid PIO system descriptor', subname, __LINE__)
        end if

        call self % debug_print(log_level_info, 'Initializing MPAS framework (Phase 2/2)')

        ! Initialize MPAS framework with the supplied PIO system descriptor.
        call mpas_framework_init_phase2(self % domain_ptr, io_system=pio_iosystem)

        ! Instantiate `streaminfo`, but do not actually initialize it. Any queries made to it will always return `.false.`.
        ! This is the intended behavior because MPAS as a dynamical core is not responsible for managing IO.
        self % domain_ptr % streaminfo => mpas_stream_inquiry_new_streaminfo()

        if (.not. associated(self % domain_ptr % streaminfo)) then
            call self % model_error('Stream info instantiation failed for core ' // trim(self % domain_ptr % core % corename), &
                subname, __LINE__)
        end if

        call self % debug_print(log_level_info, 'Defining packages')

        ierr = self % domain_ptr % core % define_packages(self % domain_ptr % packages)

        if (ierr /= 0) then
            call self % model_error('Package definition failed for core ' // trim(self % domain_ptr % core % corename), &
                subname, __LINE__)
        end if

        call self % debug_print(log_level_info, 'Setting up packages')

        ierr = self % domain_ptr % core % setup_packages( &
            self % domain_ptr % configs, self % domain_ptr % streaminfo, &
            self % domain_ptr % packages, self % domain_ptr % iocontext)

        if (ierr /= 0) then
            call self % model_error('Package setup failed for core ' // trim(self % domain_ptr % core % corename), &
                subname, __LINE__)
        end if

        call self % debug_print(log_level_info, 'Setting up decompositions')

        ierr = self % domain_ptr % core % setup_decompositions(self % domain_ptr % decompositions)

        if (ierr /= 0) then
            call self % model_error('Decomposition setup failed for core ' // trim(self % domain_ptr % core % corename), &
                subname, __LINE__)
        end if

        call self % debug_print(log_level_info, 'Setting up clock')

        ierr = self % domain_ptr % core % setup_clock(self % domain_ptr % clock, self % domain_ptr % configs)

        if (ierr /= 0) then
            call self % model_error('Clock setup failed for core ' // trim(self % domain_ptr % core % corename), &
                subname, __LINE__)
        end if

        ! At this point, we should be ready to set up decompositions, build halos, allocate blocks, etc.
        ! in `dyn_mpas_init_phase3`.
        call self % debug_print(log_level_debug, subname // ' completed')
    end subroutine dyn_mpas_init_phase2

    !-------------------------------------------------------------------------------
    ! subroutine dyn_mpas_init_phase3
    !
    !> summary: Track `mpas_init` up to the point of calling `atm_core_init`.
    !> author: Michael Duda
    !> date: 19 April 2019
    !>
    !> This subroutine follows the stand-alone MPAS subdriver after the check on
    !> the existence of the "streams.<core>" file up to, but not including,
    !> the point where `atm_core_init` is called. It completes MPAS framework
    !> initialization, including the allocation of all blocks and fields managed
    !> by MPAS. However, note that scalars are allocated, but not yet defined.
    !> `dyn_mpas_define_scalar` must be called afterwards. Also note that MPAS uses
    !> the term "scalar", but CAM-SIMA calls it "constituent".
    !> Ported and refactored for CAM-SIMA. (KCW, 2024-03-06)
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_init_phase3(self, number_of_constituents, pio_file)
        ! Module(s) from external libraries.
        use pio, only: file_desc_t, pio_file_is_open
        ! Module(s) from MPAS.
        use mpas_bootstrapping, only: mpas_bootstrap_framework_phase1, mpas_bootstrap_framework_phase2
        use mpas_derived_types, only: mpas_io_pnetcdf, mpas_pool_type
        use mpas_pool_routines, only: mpas_pool_add_config, mpas_pool_add_dimension, mpas_pool_get_dimension

        class(mpas_dynamical_core_type), intent(inout) :: self
        integer, intent(in) :: number_of_constituents
        type(file_desc_t), pointer, intent(in) :: pio_file

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_init_phase3'
        character(strkind) :: mesh_filename
        integer :: mesh_format
        integer, pointer :: num_scalars
        type(mpas_pool_type), pointer :: mpas_pool

        call self % debug_print(log_level_debug, subname // ' entered')

        nullify(mpas_pool)
        nullify(num_scalars)

        ! In MPAS, there must be at least one constituent, `qv`, which denotes water vapor mixing ratio.
        ! Because MPAS has some hard-coded array accesses through the `index_qv` index, it will crash
        ! (i.e., segmentation fault due to invalid memory access) if `qv` is not allocated.
        self % number_of_constituents = max(1, number_of_constituents)

        call self % debug_print(log_level_info, 'Number of constituents is ' // stringify([self % number_of_constituents]))

        ! Adding a config named "cam_pcnst" with the number of constituents will indicate to MPAS that
        ! it is operating as a dynamical core, and therefore it needs to allocate scalars separately
        ! from other Registry-defined fields. The special logic is located in `atm_setup_block`.
        ! This must be done before calling `mpas_bootstrap_framework_phase1`.
        call mpas_pool_add_config(self % domain_ptr % configs, 'cam_pcnst', self % number_of_constituents)

        ! Not actually used because a PIO file descriptor is directly supplied.
        mesh_filename = 'external mesh'
        mesh_format = mpas_io_pnetcdf

        call self % debug_print(log_level_info, 'Checking PIO file descriptor')

        if (.not. associated(pio_file)) then
            call self % model_error('Invalid PIO file descriptor', subname, __LINE__)
        end if

        if (.not. pio_file_is_open(pio_file)) then
            call self % model_error('Invalid PIO file descriptor', subname, __LINE__)
        end if

        call self % debug_print(log_level_info, 'Bootstrapping MPAS framework (Phase 1/2)')

        ! Finish setting up blocks.
        call mpas_bootstrap_framework_phase1(self % domain_ptr, mesh_filename, mesh_format, pio_file_desc=pio_file)

        call self % debug_print(log_level_info, 'Bootstrapping MPAS framework (Phase 2/2)')

        ! Finish setting up fields.
        call mpas_bootstrap_framework_phase2(self % domain_ptr, pio_file_desc=pio_file)

        ! "num_scalars" is a dimension variable, but it only exists in MPAS "state" pool.
        ! Fix this inconsistency by also adding it to MPAS "dimension" pool.
        call self % get_pool_pointer(mpas_pool, 'state')

        call mpas_pool_get_dimension(mpas_pool, 'num_scalars', num_scalars)

        if (.not. associated(num_scalars)) then
            call self % model_error('Failed to find variable "num_scalars"', subname, __LINE__)
        end if

        ! While we are at it, check if its value is consistent.
        if (num_scalars /= self % number_of_constituents) then
            call self % model_error('Failed to allocate constituents', subname, __LINE__)
        end if

        call mpas_pool_add_dimension(self % domain_ptr % blocklist % dimensions, 'num_scalars', num_scalars)

        nullify(mpas_pool)
        nullify(num_scalars)

        ! At this point, what follows next depends on the specific use case. In no particular order:
        ! * Use `dyn_mpas_define_scalar` to define the names of constituents at run-time.
        ! * Use `dyn_mpas_read_write_stream` to read mesh variables.
        !   * Follow up with a call to `dyn_mpas_compute_unit_vector` immediately. This is by design.
        ! * For setting analytic initial condition, use `get_variable_pointer` to inject data directly into MPAS memory.
        !   * Use `dyn_mpas_compute_edge_wind` where appropriate.
        !   * Use `dyn_mpas_exchange_halo` where appropriate.
        ! * Use `dyn_mpas_read_write_stream` to read initial condition or restart.
        ! * Finally, use `dyn_mpas_init_phase4` to conclude the initialization of MPAS dynamical core.
        call self % debug_print(log_level_debug, subname // ' completed')
    end subroutine dyn_mpas_init_phase3

    !-------------------------------------------------------------------------------
    ! subroutine dyn_mpas_define_scalar
    !
    !> summary: Define the names of constituents at run-time.
    !> author: Michael Duda
    !> date: 21 May 2020
    !>
    !> Given arrays of constituent names and their corresponding waterness, which
    !> must have sizes equal to the number of constituents used to call
    !> `dyn_mpas_init_phase3`, this subroutine defines the scalars inside MPAS.
    !> Note that MPAS uses the term "scalar", but CAM-SIMA calls it "constituent".
    !> Furthermore, because MPAS expects all water scalars to appear in a
    !> contiguous index range, this subroutine may reorder the scalars to satisfy
    !> this constrain. Index mapping between MPAS scalars and constituent names
    !> can be looked up through `index_constituent_to_mpas_scalar` and
    !> `index_mpas_scalar_to_constituent`.
    !> Ported and refactored for CAM-SIMA. (KCW, 2024-05-19)
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_define_scalar(self, constituent_name, is_water_species)
        ! Module(s) from MPAS.
        use mpas_derived_types, only: field3dreal, mpas_pool_type
        use mpas_pool_routines, only: mpas_pool_add_dimension, mpas_pool_get_field

        class(mpas_dynamical_core_type), intent(inout) :: self
        character(*), intent(in) :: constituent_name(:)
        logical, intent(in) :: is_water_species(:)

        !> Possible CCPP standard names of `qv`, which denotes water vapor mixing ratio.
        !> They are hard-coded here because MPAS needs to know where `qv` is.
        !> Index 1 is exactly what MPAS wants. Others also work, but need to be converted.
        character(*), parameter :: mpas_scalar_qv_standard_name(*) = [ character(strkind) :: &
            'water_vapor_mixing_ratio_wrt_dry_air', &
            'water_vapor_mixing_ratio_wrt_moist_air', &
            'water_vapor_mixing_ratio_wrt_moist_air_and_condensed_water' &
        ]

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_define_scalar'
        integer :: i, j, ierr
        integer :: index_qv, index_water_start, index_water_end
        integer :: time_level
        type(field3dreal), pointer :: field_3d_real
        type(mpas_pool_type), pointer :: mpas_pool

        call self % debug_print(log_level_debug, subname // ' entered')

        nullify(field_3d_real)
        nullify(mpas_pool)

        if (self % number_of_constituents == 0) then
            call self % model_error('Constituents must be allocated before being defined', subname, __LINE__)
        end if

        ! Input sanitization.

        if (size(constituent_name) /= size(is_water_species)) then
            call self % model_error('Mismatch between numbers of constituent names and their waterness', subname, __LINE__)
        end if

        if (size(constituent_name) == 0 .and. self % number_of_constituents == 1) then
            ! If constituent definitions are empty, `qv` is the only constituent per MPAS requirements.
            ! See `dyn_mpas_init_phase3` for details.
            allocate(self % constituent_name(1), stat=ierr)

            if (ierr /= 0) then
                call self % model_error('Failed to allocate constituent_name', subname, __LINE__)
            end if

            allocate(self % is_water_species(1), stat=ierr)

            if (ierr /= 0) then
                call self % model_error('Failed to allocate is_water_species', subname, __LINE__)
            end if

            self % constituent_name(1) = mpas_scalar_qv_standard_name(1)
            self % is_water_species(1) = .true.
        else
            if (size(constituent_name) /= self % number_of_constituents) then
                call self % model_error('Mismatch between numbers of constituents and their names', subname, __LINE__)
            end if

            if (any(len_trim(adjustl(constituent_name)) > len(self % constituent_name))) then
                call self % model_error('Constituent names are too long', subname, __LINE__)
            end if

            allocate(self % constituent_name(self % number_of_constituents), stat=ierr)

            if (ierr /= 0) then
                call self % model_error('Failed to allocate constituent_name', subname, __LINE__)
            end if

            self % constituent_name(:) = adjustl(constituent_name)

            allocate(self % is_water_species(self % number_of_constituents), stat=ierr)

            if (ierr /= 0) then
                call self % model_error('Failed to allocate is_water_species', subname, __LINE__)
            end if

            self % is_water_species(:) = is_water_species(:)

            if (size(self % constituent_name) /= size(index_unique(self % constituent_name))) then
                call self % model_error('Constituent names must be unique', subname, __LINE__)
            end if

            ! `qv` must be present in constituents per MPAS requirements. It is a water species by definition.
            ! See `dyn_mpas_init_phase3` for details.
            index_qv = 0

            ! Lower index in `mpas_scalar_qv_standard_name` has higher precedence, with index 1 being exactly what MPAS wants.
            set_index_qv: do i = 1, size(mpas_scalar_qv_standard_name)
                do j = 1, self % number_of_constituents
                    if (self % constituent_name(j) == mpas_scalar_qv_standard_name(i) .and. self % is_water_species(j)) then
                        index_qv = j

                        ! The best candidate of `qv` has been found. Exit prematurely.
                        exit set_index_qv
                    end if
                end do
            end do set_index_qv

            if (index_qv == 0) then
                call self % model_error('Constituent names must contain one of: ' // &
                    stringify(mpas_scalar_qv_standard_name) // ', and it must be a water species', subname, __LINE__)
            end if
        end if

        ! Create index mapping between MPAS scalars and constituent names. For example,
        ! MPAS scalar index `i` corresponds to constituent index `index_mpas_scalar_to_constituent(i)`.

        call self % debug_print(log_level_info, 'Creating index mapping between MPAS scalars and CAM-SIMA constituents')

        allocate(self % index_mpas_scalar_to_constituent(self % number_of_constituents), stat=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to allocate index_mpas_scalar_to_constituent', subname, __LINE__)
        end if

        self % index_mpas_scalar_to_constituent(:) = 0
        j = 1

        ! Place water species first per MPAS requirements.
        do i = 1, self % number_of_constituents
            if (self % is_water_species(i)) then
                self % index_mpas_scalar_to_constituent(j) = i
                j = j + 1
            end if
        end do

        index_water_start = 1
        index_water_end = count(self % is_water_species)

        ! Place non-water species second per MPAS requirements.
        do i = 1, self % number_of_constituents
            if (.not. self % is_water_species(i)) then
                self % index_mpas_scalar_to_constituent(j) = i
                j = j + 1
            end if
        end do

        ! Create inverse index mapping between MPAS scalars and constituent names. For example,
        ! Constituent index `i` corresponds to MPAS scalar index `index_constituent_to_mpas_scalar(i)`.

        call self % debug_print(log_level_info, 'Creating inverse index mapping between MPAS scalars and CAM-SIMA constituents')

        allocate(self % index_constituent_to_mpas_scalar(self % number_of_constituents), stat=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to allocate index_constituent_to_mpas_scalar', subname, __LINE__)
        end if

        self % index_constituent_to_mpas_scalar(:) = 0

        do i = 1, self % number_of_constituents
            self % index_constituent_to_mpas_scalar(self % index_mpas_scalar_to_constituent(i)) = i
        end do

        ! Set the index of `qv` in terms of MPAS scalars.
        index_qv = self % index_constituent_to_mpas_scalar(index_qv)

        ! Print information about constituents.
        do i = 1, self % number_of_constituents
            call self % debug_print(log_level_verbose, 'Constituent index ' // stringify([i]))
            call self % debug_print(log_level_verbose, '    Constituent name: ' // &
                trim(self % constituent_name(i)))
            call self % debug_print(log_level_verbose, '    Is water species: ' // &
                stringify([self % is_water_species(i)]))
            call self % debug_print(log_level_verbose, '    Index mapping from constituent to MPAS scalar: ' // &
                stringify([i]) // ' -> ' // stringify([self % index_constituent_to_mpas_scalar(i)]))
        end do

        ! Define "scalars" for MPAS.

        call self % debug_print(log_level_info, 'Defining MPAS scalars')

        call self % get_pool_pointer(mpas_pool, 'state')

        call mpas_pool_add_dimension(mpas_pool, 'index_qv', index_qv)
        call mpas_pool_add_dimension(mpas_pool, 'moist_start', index_water_start)
        call mpas_pool_add_dimension(mpas_pool, 'moist_end', index_water_end)

        ! MPAS "state" pool has two time levels.
        time_level = 2

        do i = 1, time_level
            call mpas_pool_get_field(mpas_pool, 'scalars', field_3d_real, timelevel=i)

            if (.not. associated(field_3d_real)) then
                call self % model_error('Failed to find variable "scalars"', subname, __LINE__)
            end if

            do j = 1, self % number_of_constituents
                field_3d_real % constituentnames(j) = &
                    trim(adjustl(self % constituent_name(self % index_mpas_scalar_to_constituent(j))))

                ! Print information about MPAS scalars. Only do it once.
                if (i == 1) then
                    call self % debug_print(log_level_verbose, 'MPAS scalar index ' // stringify([j]))
                    call self % debug_print(log_level_verbose, '    MPAS scalar name: ' // &
                        trim(field_3d_real % constituentnames(j)))
                    call self % debug_print(log_level_verbose, '    Is water species: ' // &
                        stringify([self % is_water_species(self % index_mpas_scalar_to_constituent(j))]))
                    call self % debug_print(log_level_verbose, '    Index mapping from MPAS scalar to constituent: ' // &
                        stringify([j]) // ' -> ' // stringify([self % index_mpas_scalar_to_constituent(j)]))
                end if
            end do

            nullify(field_3d_real)
        end do

        nullify(mpas_pool)

        ! Define "scalars_tend" for MPAS.

        call self % debug_print(log_level_info, 'Defining MPAS scalar tendencies')

        call self % get_pool_pointer(mpas_pool, 'tend')

        call mpas_pool_add_dimension(mpas_pool, 'index_qv', index_qv)
        call mpas_pool_add_dimension(mpas_pool, 'moist_start', index_water_start)
        call mpas_pool_add_dimension(mpas_pool, 'moist_end', index_water_end)

        ! MPAS "tend" pool only has one time level.
        time_level = 1

        do i = 1, time_level
            call mpas_pool_get_field(mpas_pool, 'scalars_tend', field_3d_real, timelevel=i)

            if (.not. associated(field_3d_real)) then
                call self % model_error('Failed to find variable "scalars_tend"', subname, __LINE__)
            end if

            do j = 1, self % number_of_constituents
                field_3d_real % constituentnames(j) = &
                    'tendency_of_' // trim(adjustl(self % constituent_name(self % index_mpas_scalar_to_constituent(j))))

                ! Print information about MPAS scalar tendencies. Only do it once.
                if (i == 1) then
                    call self % debug_print(log_level_verbose, 'MPAS scalar tendency index ' // stringify([j]))
                    call self % debug_print(log_level_verbose, '    MPAS scalar tendency name: ' // &
                        trim(field_3d_real % constituentnames(j)))
                    call self % debug_print(log_level_verbose, '    Is water species: ' // &
                        stringify([self % is_water_species(self % index_mpas_scalar_to_constituent(j))]))
                    call self % debug_print(log_level_verbose, '    Index mapping from MPAS scalar tendency to constituent: ' // &
                        stringify([j]) // ' -> ' // stringify([self % index_mpas_scalar_to_constituent(j)]))
                end if
            end do

            nullify(field_3d_real)
        end do

        nullify(mpas_pool)

        ! For consistency, also add dimension variables to MPAS "dimension" pool.

        call mpas_pool_add_dimension(self % domain_ptr % blocklist % dimensions, 'index_qv', index_qv)
        call mpas_pool_add_dimension(self % domain_ptr % blocklist % dimensions, 'moist_start', index_water_start)
        call mpas_pool_add_dimension(self % domain_ptr % blocklist % dimensions, 'moist_end', index_water_end)

        call self % debug_print(log_level_debug, 'index_qv = ' // stringify([index_qv]))
        call self % debug_print(log_level_debug, 'moist_start = ' // stringify([index_water_start]))
        call self % debug_print(log_level_debug, 'moist_end = ' // stringify([index_water_end]))

        call self % debug_print(log_level_debug, subname // ' completed')
    end subroutine dyn_mpas_define_scalar

    !-------------------------------------------------------------------------------
    ! subroutine dyn_mpas_read_write_stream
    !
    !> summary: Read or write an MPAS stream.
    !> author: Kuan-Chih Wang
    !> date: 2024-03-15
    !>
    !> In the context of MPAS, the concept of a "pool" resembles a group of
    !> (related) variables, while the concept of a "stream" resembles a file.
    !> This subroutine reads or writes an MPAS stream. It provides the mechanism
    !> for CAM-SIMA to input/output data to/from MPAS dynamical core.
    !> Analogous to the `{read,write}_stream` subroutines in MPAS stream manager.
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_read_write_stream(self, pio_file, stream_mode, stream_name)
        ! Module(s) from external libraries.
        use pio, only: file_desc_t
        ! Module(s) from MPAS.
        use mpas_derived_types, only: mpas_pool_type, mpas_stream_noerr, mpas_stream_type
        use mpas_io_streams, only: mpas_closestream, mpas_readstream, mpas_writestream
        use mpas_pool_routines, only: mpas_pool_destroy_pool
        use mpas_stream_manager, only: postread_reindex, prewrite_reindex, postwrite_reindex

        class(mpas_dynamical_core_type), intent(in) :: self
        type(file_desc_t), pointer, intent(in) :: pio_file
        character(*), intent(in) :: stream_mode
        character(*), intent(in) :: stream_name

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_read_write_stream'
        integer :: i, ierr
        type(mpas_pool_type), pointer :: mpas_pool
        type(mpas_stream_type), pointer :: mpas_stream
        type(var_info_type), allocatable :: var_info_list(:)

        call self % debug_print(log_level_debug, subname // ' entered')

        nullify(mpas_pool)
        nullify(mpas_stream)

        call self % debug_print(log_level_info, 'Initializing stream "' // trim(adjustl(stream_name)) // '"')

        call self % init_stream_with_pool(mpas_pool, mpas_stream, pio_file, stream_mode, stream_name)

        if (.not. associated(mpas_pool)) then
            call self % model_error('Failed to initialize stream "' // trim(adjustl(stream_name)) // '"', subname, __LINE__)
        end if

        if (.not. associated(mpas_stream)) then
            call self % model_error('Failed to initialize stream "' // trim(adjustl(stream_name)) // '"', subname, __LINE__)
        end if

        select case (trim(adjustl(stream_mode)))
            case ('r', 'read')
                call self % debug_print(log_level_info, 'Reading stream "' // trim(adjustl(stream_name)) // '"')

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
                call self % debug_print(log_level_info, 'Writing stream "' // trim(adjustl(stream_name)) // '"')

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

        call self % debug_print(log_level_info, 'Closing stream "' // trim(adjustl(stream_name)) // '"')

        call mpas_closestream(mpas_stream, ierr=ierr)

        if (ierr /= mpas_stream_noerr) then
            call self % model_error('Failed to close stream "' // trim(adjustl(stream_name)) // '"', subname, __LINE__)
        end if

        ! Deallocate temporary pointers to avoid memory leaks.
        call mpas_pool_destroy_pool(mpas_pool)
        nullify(mpas_pool)

        deallocate(mpas_stream)
        nullify(mpas_stream)

        call self % debug_print(log_level_debug, subname // ' completed')
    end subroutine dyn_mpas_read_write_stream

    !-------------------------------------------------------------------------------
    ! subroutine dyn_mpas_init_stream_with_pool
    !
    !> summary: Initialize an MPAS stream with an accompanying MPAS pool.
    !> author: Kuan-Chih Wang
    !> date: 2024-03-14
    !>
    !> In the context of MPAS, the concept of a "pool" resembles a group of
    !> (related) variables, while the concept of a "stream" resembles a file.
    !> This subroutine initializes an MPAS stream with an accompanying MPAS pool by
    !> adding variable and attribute information to them. After that, MPAS is ready
    !> to perform IO on them.
    !> Analogous to the `build_stream` and `mpas_stream_mgr_add_field`
    !> subroutines in MPAS stream manager.
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_init_stream_with_pool(self, mpas_pool, mpas_stream, pio_file, stream_mode, stream_name)
        ! Module(s) from external libraries.
        use pio, only: file_desc_t, pio_file_is_open
        ! Module(s) from MPAS.
        use mpas_derived_types, only: field0dchar, field1dchar, &
                                      field0dinteger, field1dinteger, field2dinteger, field3dinteger, &
                                      field0dreal, field1dreal, field2dreal, field3dreal, field4dreal, field5dreal, &
                                      mpas_io_native_precision, mpas_io_pnetcdf, mpas_io_read, mpas_io_write, &
                                      mpas_pool_type, mpas_stream_noerr, mpas_stream_type
        use mpas_io_streams, only: mpas_createstream, mpas_streamaddfield
        use mpas_pool_routines, only: mpas_pool_add_config, mpas_pool_create_pool, mpas_pool_get_field

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
        integer :: i, ierr, stream_format
        !> Whether a variable is present on the file (i.e., `pio_file`).
        logical, allocatable :: var_is_present(:)
        !> Whether a variable is type, kind, and rank compatible with what MPAS expects on the file (i.e., `pio_file`).
        logical, allocatable :: var_is_tkr_compatible(:)
        type(field0dchar), pointer :: field_0d_char
        type(field1dchar), pointer :: field_1d_char
        type(field0dinteger), pointer :: field_0d_integer
        type(field1dinteger), pointer :: field_1d_integer
        type(field2dinteger), pointer :: field_2d_integer
        type(field3dinteger), pointer :: field_3d_integer
        type(field0dreal), pointer :: field_0d_real
        type(field1dreal), pointer :: field_1d_real
        type(field2dreal), pointer :: field_2d_real
        type(field3dreal), pointer :: field_3d_real
        type(field4dreal), pointer :: field_4d_real
        type(field5dreal), pointer :: field_5d_real
        type(var_info_type), allocatable :: var_info_list(:)

        call self % debug_print(log_level_debug, subname // ' entered')

        nullify(field_0d_char)
        nullify(field_1d_char)
        nullify(field_0d_integer)
        nullify(field_1d_integer)
        nullify(field_2d_integer)
        nullify(field_3d_integer)
        nullify(field_0d_real)
        nullify(field_1d_real)
        nullify(field_2d_real)
        nullify(field_3d_real)
        nullify(field_4d_real)
        nullify(field_5d_real)

        call mpas_pool_create_pool(mpas_pool)

        allocate(mpas_stream, stat=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to allocate stream "' // trim(adjustl(stream_name)) // '"', subname, __LINE__)
        end if

        ! Not actually used because a PIO file descriptor is directly supplied.
        stream_filename = 'external stream'
        stream_format = mpas_io_pnetcdf

        call self % debug_print(log_level_verbose, 'Checking PIO file descriptor')

        if (.not. associated(pio_file)) then
            call self % model_error('Invalid PIO file descriptor', subname, __LINE__)
        end if

        if (.not. pio_file_is_open(pio_file)) then
            call self % model_error('Invalid PIO file descriptor', subname, __LINE__)
        end if

        select case (trim(adjustl(stream_mode)))
            case ('r', 'read')
                call self % debug_print(log_level_verbose, 'Creating stream "' // trim(adjustl(stream_name)) // '" for reading')

                call mpas_createstream( &
                    mpas_stream, self % domain_ptr % iocontext, stream_filename, stream_format, mpas_io_read,  &
                    clobberrecords=.false., clobberfiles=.false., truncatefiles=.false., &
                    precision=mpas_io_native_precision, pio_file_desc=pio_file, ierr=ierr)
            case ('w', 'write')
                call self % debug_print(log_level_verbose, 'Creating stream "' // trim(adjustl(stream_name)) // '" for writing')

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

        ! Add variables contained in `var_info_list` to stream.
        do i = 1, size(var_info_list)
            call self % debug_print(log_level_debug, 'var_info_list(' // stringify([i]) // ') % name = ' // &
                stringify([var_info_list(i) % name]))
            call self % debug_print(log_level_debug, 'var_info_list(' // stringify([i]) // ') % type = ' // &
                stringify([var_info_list(i) % type]))
            call self % debug_print(log_level_debug, 'var_info_list(' // stringify([i]) // ') % rank = ' // &
                stringify([var_info_list(i) % rank]))

            if (trim(adjustl(stream_mode)) == 'r' .or. trim(adjustl(stream_mode)) == 'read') then
                call self % check_variable_status(var_is_present, var_is_tkr_compatible, pio_file, var_info_list(i))

                ! Do not hard crash the model if a variable is missing and cannot be read.
                ! This can happen if users attempt to initialize/restart the model with data generated by
                ! older versions of MPAS. Print a debug message to let users decide if this is acceptable.
                if (.not. any(var_is_present)) then
                    call self % debug_print(log_level_verbose, 'Skipping variable "' // trim(adjustl(var_info_list(i) % name)) // &
                        '" due to not present')

                    cycle
                end if

                if (any(var_is_present .and. .not. var_is_tkr_compatible)) then
                    call self % debug_print(log_level_verbose, 'Skipping variable "' // trim(adjustl(var_info_list(i) % name)) // &
                        '" due to not TKR compatible')

                    cycle
                end if
            end if

            ! Add "<variable name>" to pool with the value of `1`.
            ! The existence of "<variable name>" in pool causes it to be considered for IO in MPAS.
            call mpas_pool_add_config(mpas_pool, trim(adjustl(var_info_list(i) % name)), 1)
            ! Add "<variable name>:packages" to pool with the value of an empty character string.
            ! This causes "<variable name>" to be always considered active for IO in MPAS.
            call mpas_pool_add_config(mpas_pool, trim(adjustl(var_info_list(i) % name) // ':packages'), '')

            ! Add "<variable name>" to stream.
            call self % debug_print(log_level_verbose, 'Adding variable "' // trim(adjustl(var_info_list(i) % name)) // &
                '" to stream "' // trim(adjustl(stream_name)) // '"')

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
                            call self % model_error('Unsupported variable rank ' // stringify([var_info_list(i) % rank]) // &
                                ' for "' // trim(adjustl(var_info_list(i) % name)) // '"', subname, __LINE__)
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
                            call self % model_error('Unsupported variable rank ' // stringify([var_info_list(i) % rank]) // &
                                ' for "' // trim(adjustl(var_info_list(i) % name)) // '"', subname, __LINE__)
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
                            call self % model_error('Unsupported variable rank ' // stringify([var_info_list(i) % rank]) // &
                                ' for "' // trim(adjustl(var_info_list(i) % name)) // '"', subname, __LINE__)
                    end select
                case default
                    call self % model_error('Unsupported variable type "' // trim(adjustl(var_info_list(i) % type)) // &
                        '" for "' // trim(adjustl(var_info_list(i) % name)) // '"', subname, __LINE__)
            end select

            if (ierr /= mpas_stream_noerr) then
                call self % model_error('Failed to add variable "' // trim(adjustl(var_info_list(i) % name)) // &
                    '" to stream "' // trim(adjustl(stream_name)) // '"', subname, __LINE__)
            end if
        end do

        if (trim(adjustl(stream_mode)) == 'w' .or. trim(adjustl(stream_mode)) == 'write') then
            ! Add MPAS-specific attributes to stream.

            ! Attributes related to MPAS core (i.e., `core_type`).
            call add_stream_attribute('conventions', self % domain_ptr % core % conventions)
            call add_stream_attribute('core_name', self % domain_ptr % core % corename)
            call add_stream_attribute('git_version', self % domain_ptr % core % git_version)
            call add_stream_attribute('model_name', self % domain_ptr % core % modelname)
            call add_stream_attribute('source', self % domain_ptr % core % source)
            call add_stream_attribute('version', self % domain_ptr % core % modelversion)

            ! Attributes related to MPAS domain (i.e., `domain_type`).
            call add_stream_attribute('is_periodic', self % domain_ptr % is_periodic)
            call add_stream_attribute('mesh_spec', self % domain_ptr % mesh_spec)
            call add_stream_attribute('on_a_sphere', self % domain_ptr % on_a_sphere)
            call add_stream_attribute('parent_id', self % domain_ptr % parent_id)
            call add_stream_attribute('sphere_radius', self % domain_ptr % sphere_radius)
            call add_stream_attribute('x_period', self % domain_ptr % x_period)
            call add_stream_attribute('y_period', self % domain_ptr % y_period)
        end if

        call self % debug_print(log_level_debug, subname // ' completed')
    contains
        !> Helper subroutine for adding a 0-d stream attribute by calling `mpas_writestreamatt` with error checking.
        !> (KCW, 2024-03-14)
        subroutine add_stream_attribute_0d(attribute_name, attribute_value)
            ! Module(s) from MPAS.
            use mpas_io_streams, only: mpas_writestreamatt

            character(*), intent(in) :: attribute_name
            class(*), intent(in) :: attribute_value

            call self % debug_print(log_level_verbose, 'Adding attribute "' // trim(adjustl(attribute_name)) // &
                '" to stream "' // trim(adjustl(stream_name)) // '"')

            select type (attribute_value)
                type is (character(*))
                    call mpas_writestreamatt(mpas_stream, &
                        trim(adjustl(attribute_name)), trim(adjustl(attribute_value)), syncval=.false., ierr=ierr)
                type is (integer)
                    call mpas_writestreamatt(mpas_stream, &
                        trim(adjustl(attribute_name)), attribute_value, syncval=.false., ierr=ierr)
                type is (logical)
                    if (attribute_value) then
                        ! Logical `.true.` becomes character string "YES".
                        call mpas_writestreamatt(mpas_stream, &
                            trim(adjustl(attribute_name)), 'YES', syncval=.false., ierr=ierr)
                    else
                        ! Logical `.false.` becomes character string "NO".
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
            ! Module(s) from MPAS.
            use mpas_io_streams, only: mpas_writestreamatt

            character(*), intent(in) :: attribute_name
            class(*), intent(in) :: attribute_value(:)

            call self % debug_print(log_level_verbose, 'Adding attribute "' // trim(adjustl(attribute_name)) // &
                '" to stream "' // trim(adjustl(stream_name)) // '"')

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

    !> Parse a stream name, which consists of one or more stream name fragments, and return the corresponding variable information
    !> as a list of `var_info_type`. Multiple stream name fragments should be separated by "+" (i.e., a plus, meaning "addition"
    !> operation) or "-" (i.e., a minus, meaning "subtraction" operation).
    !> A stream name fragment can be a predefined stream name (e.g., "invariant", "input", etc.) or a single variable name.
    !> For example, a stream name of "invariant+input+restart" means the union of variables in the "invariant", "input", and
    !> "restart" streams.
    !> Duplicate variable information in the resulting list is discarded.
    !> (KCW, 2024-06-01)
    pure function parse_stream_name(stream_name) result(var_info_list)
        character(*), intent(in) :: stream_name
        type(var_info_type), allocatable :: var_info_list(:)

        character(*), parameter :: supported_stream_name_operator = '+-'
        character(1) :: stream_name_operator
        character(:), allocatable :: stream_name_fragment
        character(len(invariant_var_info_list % name)), allocatable :: var_name_list(:)
        integer :: i, j, n, offset
        type(var_info_type), allocatable :: var_info_list_buffer(:)

        n = len_trim(stream_name)

        if (n == 0) then
            ! Empty character string means empty list.
            var_info_list = parse_stream_name_fragment('')

            return
        end if

        i = scan(stream_name, supported_stream_name_operator)

        if (i == 0) then
            ! No operators are present in the stream name. It is just a single stream name fragment.
            stream_name_fragment = stream_name
            var_info_list = parse_stream_name_fragment(stream_name_fragment)

            return
        end if

        offset = 0
        var_info_list = parse_stream_name_fragment('')

        do while (.true.)
            ! Extract operator from the stream name.
            if (offset > 0) then
                stream_name_operator = stream_name(offset:offset)
            else
                stream_name_operator = '+'
            end if

            ! Extract stream name fragment from the stream name.
            if (i > 1) then
                stream_name_fragment = stream_name(offset + 1:offset + i - 1)
            else
                stream_name_fragment = ''
            end if

            ! Process the stream name fragment according to the operator.
            if (len_trim(stream_name_fragment) > 0) then
                var_info_list_buffer = parse_stream_name_fragment(stream_name_fragment)

                select case (stream_name_operator)
                    case ('+')
                        var_info_list = [var_info_list, var_info_list_buffer]
                    case ('-')
                        do j = 1, size(var_info_list_buffer)
                            var_name_list = var_info_list % name
                            var_info_list = pack(var_info_list, var_name_list /= var_info_list_buffer(j) % name)
                        end do
                    case default
                        ! Do nothing for unknown operators. Should not happen at all.
                end select
            end if

            offset = offset + i

            ! Terminate loop when everything in the stream name has been processed.
            if (offset + 1 > n) then
                exit
            end if

            i = scan(stream_name(offset + 1:), supported_stream_name_operator)

            ! Run the loop one last time for the remaining stream name fragment.
            if (i == 0) then
                i = n - offset + 1
            end if
        end do

        ! Discard duplicate variable information by names.
        var_name_list = var_info_list % name
        var_info_list = var_info_list(index_unique(var_name_list))
    end function parse_stream_name

    !> Parse a stream name fragment and return the corresponding variable information as a list of `var_info_type`.
    !> A stream name fragment can be a predefined stream name (e.g., "invariant", "input", etc.) or a single variable name.
    !> (KCW, 2024-06-01)
    pure function parse_stream_name_fragment(stream_name_fragment) result(var_info_list)
        character(*), intent(in) :: stream_name_fragment
        type(var_info_type), allocatable :: var_info_list(:)

        character(len(invariant_var_info_list % name)), allocatable :: var_name_list(:)
        type(var_info_type), allocatable :: var_info_list_buffer(:)

        select case (trim(adjustl(stream_name_fragment)))
            case ('')
                allocate(var_info_list(0))
            case ('invariant')
                allocate(var_info_list, source=invariant_var_info_list)
            case ('input')
                allocate(var_info_list, source=input_var_info_list)
            case ('restart')
                allocate(var_info_list, source=restart_var_info_list)
            case ('output')
                allocate(var_info_list, source=output_var_info_list)
            case default
                allocate(var_info_list(0))

                var_name_list = invariant_var_info_list % name

                if (any(var_name_list == trim(adjustl(stream_name_fragment)))) then
                    var_info_list_buffer = pack(invariant_var_info_list, var_name_list == trim(adjustl(stream_name_fragment)))
                    var_info_list = [var_info_list, var_info_list_buffer]
                end if

                var_name_list = input_var_info_list % name

                if (any(var_name_list == trim(adjustl(stream_name_fragment)))) then
                    var_info_list_buffer = pack(input_var_info_list, var_name_list == trim(adjustl(stream_name_fragment)))
                    var_info_list = [var_info_list, var_info_list_buffer]
                end if

                var_name_list = restart_var_info_list % name

                if (any(var_name_list == trim(adjustl(stream_name_fragment)))) then
                    var_info_list_buffer = pack(restart_var_info_list, var_name_list == trim(adjustl(stream_name_fragment)))
                    var_info_list = [var_info_list, var_info_list_buffer]
                end if

                var_name_list = output_var_info_list % name

                if (any(var_name_list == trim(adjustl(stream_name_fragment)))) then
                    var_info_list_buffer = pack(output_var_info_list, var_name_list == trim(adjustl(stream_name_fragment)))
                    var_info_list = [var_info_list, var_info_list_buffer]
                end if
        end select
    end function parse_stream_name_fragment

    !> Return the index of unique elements in `array`, which can be any intrinsic data types, as an integer array.
    !> If `array` contains zero element or is of unsupported data types, an empty integer array is produced.
    !> For example, `index_unique([1, 2, 3, 1, 2, 3, 4, 5])` returns `[1, 2, 3, 7, 8]`.
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
                ! Workaround for a bug in GNU Fortran >= 12. This is perhaps the manifestation of GCC Bugzilla Bug 100819.
                ! When a character string array is passed as the actual argument to an unlimited polymorphic dummy argument,
                ! its array index and length parameter are mishandled.
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
    ! subroutine dyn_mpas_check_variable_status
    !
    !> summary: Check and return variable status on the given file.
    !> author: Kuan-Chih Wang
    !> date: 2024-06-04
    !>
    !> On the given file (i.e., `pio_file`), this subroutine checks whether the
    !> given variable (i.e., `var_info`) is present, and whether it is "TKR"
    !> compatible with what MPAS expects. "TKR" means type, kind, and rank.
    !> This subroutine can handle both ordinary variables and variable arrays.
    !> They are indicated by the `var` and `var_array` elements, respectively,
    !> in MPAS registry. For an ordinary variable, the checks are performed on
    !> itself. Otherwise, for a variable array, the checks are performed on its
    !> constituent parts instead.
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_check_variable_status(self, var_is_present, var_is_tkr_compatible, pio_file, var_info)
        ! Module(s) from external libraries.
        use pio, only: file_desc_t, pio_file_is_open, &
                       pio_char, pio_int, pio_real, pio_double, &
                       pio_inq_varid, pio_inq_varndims, pio_inq_vartype, pio_noerr
        ! Module(s) from MPAS.
        use mpas_derived_types, only: field0dchar, field1dchar, &
                                      field0dinteger, field1dinteger, field2dinteger, field3dinteger, &
                                      field0dreal, field1dreal, field2dreal, field3dreal, field4dreal, field5dreal
        use mpas_kind_types, only: r4kind, r8kind
        use mpas_pool_routines, only: mpas_pool_get_field

        class(mpas_dynamical_core_type), intent(in) :: self
        logical, allocatable, intent(out) :: var_is_present(:)
        logical, allocatable, intent(out) :: var_is_tkr_compatible(:)
        type(file_desc_t), pointer, intent(in) :: pio_file
        type(var_info_type), intent(in) :: var_info

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_check_variable_status'
        character(strkind), allocatable :: var_name_list(:)
        integer :: i, ierr, varid, varndims, vartype
        type(field0dchar), pointer :: field_0d_char
        type(field1dchar), pointer :: field_1d_char
        type(field0dinteger), pointer :: field_0d_integer
        type(field1dinteger), pointer :: field_1d_integer
        type(field2dinteger), pointer :: field_2d_integer
        type(field3dinteger), pointer :: field_3d_integer
        type(field0dreal), pointer :: field_0d_real
        type(field1dreal), pointer :: field_1d_real
        type(field2dreal), pointer :: field_2d_real
        type(field3dreal), pointer :: field_3d_real
        type(field4dreal), pointer :: field_4d_real
        type(field5dreal), pointer :: field_5d_real

        call self % debug_print(log_level_debug, subname // ' entered')

        nullify(field_0d_char)
        nullify(field_1d_char)
        nullify(field_0d_integer)
        nullify(field_1d_integer)
        nullify(field_2d_integer)
        nullify(field_3d_integer)
        nullify(field_0d_real)
        nullify(field_1d_real)
        nullify(field_2d_real)
        nullify(field_3d_real)
        nullify(field_4d_real)
        nullify(field_5d_real)

        ! Extract a list of variable names to check on the file.
        ! For an ordinary variable, this list just contains its name.
        ! For a variable array, this list contains the names of its constituent parts.
        select case (trim(adjustl(var_info % type)))
            case ('character')
                select case (var_info % rank)
                    case (0)
                        call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                            trim(adjustl(var_info % name)), field_0d_char, timelevel=1)

                        if (.not. associated(field_0d_char)) then
                            call self % model_error('Failed to find variable "' // trim(adjustl(var_info % name)) // &
                                '"', subname, __LINE__)
                        end if

                        if (field_0d_char % isvararray .and. associated(field_0d_char % constituentnames)) then
                            allocate(var_name_list(size(field_0d_char % constituentnames)), stat=ierr)

                            if (ierr /= 0) then
                                call self % model_error('Failed to allocate var_name_list', subname, __LINE__)
                            end if

                            var_name_list(:) = field_0d_char % constituentnames(:)
                        end if

                        nullify(field_0d_char)
                    case (1)
                        call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                            trim(adjustl(var_info % name)), field_1d_char, timelevel=1)

                        if (.not. associated(field_1d_char)) then
                            call self % model_error('Failed to find variable "' // trim(adjustl(var_info % name)) // &
                                '"', subname, __LINE__)
                        end if

                        if (field_1d_char % isvararray .and. associated(field_1d_char % constituentnames)) then
                            allocate(var_name_list(size(field_1d_char % constituentnames)), stat=ierr)

                            if (ierr /= 0) then
                                call self % model_error('Failed to allocate var_name_list', subname, __LINE__)
                            end if

                            var_name_list(:) = field_1d_char % constituentnames(:)
                        end if

                        nullify(field_1d_char)
                    case default
                        call self % model_error('Unsupported variable rank ' // stringify([var_info % rank]) // &
                            ' for "' // trim(adjustl(var_info % name)) // '"', subname, __LINE__)
                end select
            case ('integer')
                select case (var_info % rank)
                    case (0)
                        call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                            trim(adjustl(var_info % name)), field_0d_integer, timelevel=1)

                        if (.not. associated(field_0d_integer)) then
                            call self % model_error('Failed to find variable "' // trim(adjustl(var_info % name)) // &
                                '"', subname, __LINE__)
                        end if

                        if (field_0d_integer % isvararray .and. associated(field_0d_integer % constituentnames)) then
                            allocate(var_name_list(size(field_0d_integer % constituentnames)), stat=ierr)

                            if (ierr /= 0) then
                                call self % model_error('Failed to allocate var_name_list', subname, __LINE__)
                            end if

                            var_name_list(:) = field_0d_integer % constituentnames(:)
                        end if

                        nullify(field_0d_integer)
                    case (1)
                        call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                            trim(adjustl(var_info % name)), field_1d_integer, timelevel=1)

                        if (.not. associated(field_1d_integer)) then
                            call self % model_error('Failed to find variable "' // trim(adjustl(var_info % name)) // &
                                '"', subname, __LINE__)
                        end if

                        if (field_1d_integer % isvararray .and. associated(field_1d_integer % constituentnames)) then
                            allocate(var_name_list(size(field_1d_integer % constituentnames)), stat=ierr)

                            if (ierr /= 0) then
                                call self % model_error('Failed to allocate var_name_list', subname, __LINE__)
                            end if

                            var_name_list(:) = field_1d_integer % constituentnames(:)
                        end if

                        nullify(field_1d_integer)
                    case (2)
                        call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                            trim(adjustl(var_info % name)), field_2d_integer, timelevel=1)

                        if (.not. associated(field_2d_integer)) then
                            call self % model_error('Failed to find variable "' // trim(adjustl(var_info % name)) // &
                                '"', subname, __LINE__)
                        end if

                        if (field_2d_integer % isvararray .and. associated(field_2d_integer % constituentnames)) then
                            allocate(var_name_list(size(field_2d_integer % constituentnames)), stat=ierr)

                            if (ierr /= 0) then
                                call self % model_error('Failed to allocate var_name_list', subname, __LINE__)
                            end if

                            var_name_list(:) = field_2d_integer % constituentnames(:)
                        end if

                        nullify(field_2d_integer)
                    case (3)
                        call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                            trim(adjustl(var_info % name)), field_3d_integer, timelevel=1)

                        if (.not. associated(field_3d_integer)) then
                            call self % model_error('Failed to find variable "' // trim(adjustl(var_info % name)) // &
                                '"', subname, __LINE__)
                        end if

                        if (field_3d_integer % isvararray .and. associated(field_3d_integer % constituentnames)) then
                            allocate(var_name_list(size(field_3d_integer % constituentnames)), stat=ierr)

                            if (ierr /= 0) then
                                call self % model_error('Failed to allocate var_name_list', subname, __LINE__)
                            end if

                            var_name_list(:) = field_3d_integer % constituentnames(:)
                        end if

                        nullify(field_3d_integer)
                    case default
                        call self % model_error('Unsupported variable rank ' // stringify([var_info % rank]) // &
                            ' for "' // trim(adjustl(var_info % name)) // '"', subname, __LINE__)
                end select
            case ('real')
                select case (var_info % rank)
                    case (0)
                        call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                            trim(adjustl(var_info % name)), field_0d_real, timelevel=1)

                        if (.not. associated(field_0d_real)) then
                            call self % model_error('Failed to find variable "' // trim(adjustl(var_info % name)) // &
                                '"', subname, __LINE__)
                        end if

                        if (field_0d_real % isvararray .and. associated(field_0d_real % constituentnames)) then
                            allocate(var_name_list(size(field_0d_real % constituentnames)), stat=ierr)

                            if (ierr /= 0) then
                                call self % model_error('Failed to allocate var_name_list', subname, __LINE__)
                            end if

                            var_name_list(:) = field_0d_real % constituentnames(:)
                        end if

                        nullify(field_0d_real)
                    case (1)
                        call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                            trim(adjustl(var_info % name)), field_1d_real, timelevel=1)

                        if (.not. associated(field_1d_real)) then
                            call self % model_error('Failed to find variable "' // trim(adjustl(var_info % name)) // &
                                '"', subname, __LINE__)
                        end if

                        if (field_1d_real % isvararray .and. associated(field_1d_real % constituentnames)) then
                            allocate(var_name_list(size(field_1d_real % constituentnames)), stat=ierr)

                            if (ierr /= 0) then
                                call self % model_error('Failed to allocate var_name_list', subname, __LINE__)
                            end if

                            var_name_list(:) = field_1d_real % constituentnames(:)
                        end if

                        nullify(field_1d_real)
                    case (2)
                        call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                            trim(adjustl(var_info % name)), field_2d_real, timelevel=1)

                        if (.not. associated(field_2d_real)) then
                            call self % model_error('Failed to find variable "' // trim(adjustl(var_info % name)) // &
                                '"', subname, __LINE__)
                        end if

                        if (field_2d_real % isvararray .and. associated(field_2d_real % constituentnames)) then
                            allocate(var_name_list(size(field_2d_real % constituentnames)), stat=ierr)

                            if (ierr /= 0) then
                                call self % model_error('Failed to allocate var_name_list', subname, __LINE__)
                            end if

                            var_name_list(:) = field_2d_real % constituentnames(:)
                        end if

                        nullify(field_2d_real)
                    case (3)
                        call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                            trim(adjustl(var_info % name)), field_3d_real, timelevel=1)

                        if (.not. associated(field_3d_real)) then
                            call self % model_error('Failed to find variable "' // trim(adjustl(var_info % name)) // &
                                '"', subname, __LINE__)
                        end if

                        if (field_3d_real % isvararray .and. associated(field_3d_real % constituentnames)) then
                            allocate(var_name_list(size(field_3d_real % constituentnames)), stat=ierr)

                            if (ierr /= 0) then
                                call self % model_error('Failed to allocate var_name_list', subname, __LINE__)
                            end if

                            var_name_list(:) = field_3d_real % constituentnames(:)
                        end if

                        nullify(field_3d_real)
                    case (4)
                        call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                            trim(adjustl(var_info % name)), field_4d_real, timelevel=1)

                        if (.not. associated(field_4d_real)) then
                            call self % model_error('Failed to find variable "' // trim(adjustl(var_info % name)) // &
                                '"', subname, __LINE__)
                        end if

                        if (field_4d_real % isvararray .and. associated(field_4d_real % constituentnames)) then
                            allocate(var_name_list(size(field_4d_real % constituentnames)), stat=ierr)

                            if (ierr /= 0) then
                                call self % model_error('Failed to allocate var_name_list', subname, __LINE__)
                            end if

                            var_name_list(:) = field_4d_real % constituentnames(:)
                        end if

                        nullify(field_4d_real)
                    case (5)
                        call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, &
                            trim(adjustl(var_info % name)), field_5d_real, timelevel=1)

                        if (.not. associated(field_5d_real)) then
                            call self % model_error('Failed to find variable "' // trim(adjustl(var_info % name)) // &
                                '"', subname, __LINE__)
                        end if

                        if (field_5d_real % isvararray .and. associated(field_5d_real % constituentnames)) then
                            allocate(var_name_list(size(field_5d_real % constituentnames)), stat=ierr)

                            if (ierr /= 0) then
                                call self % model_error('Failed to allocate var_name_list', subname, __LINE__)
                            end if

                            var_name_list(:) = field_5d_real % constituentnames(:)
                        end if

                        nullify(field_5d_real)
                    case default
                        call self % model_error('Unsupported variable rank ' // stringify([var_info % rank]) // &
                            ' for "' // trim(adjustl(var_info % name)) // '"', subname, __LINE__)
                end select
            case default
                call self % model_error('Unsupported variable type "' // trim(adjustl(var_info % type)) // &
                    '" for "' // trim(adjustl(var_info % name)) // '"', subname, __LINE__)
        end select

        if (.not. allocated(var_name_list)) then
            allocate(var_name_list(1), stat=ierr)

            if (ierr /= 0) then
                call self % model_error('Failed to allocate var_name_list', subname, __LINE__)
            end if

            var_name_list(1) = var_info % name
        end if

        allocate(var_is_present(size(var_name_list)), stat=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to allocate var_is_present', subname, __LINE__)
        end if

        var_is_present(:) = .false.

        allocate(var_is_tkr_compatible(size(var_name_list)), stat=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to allocate var_is_tkr_compatible', subname, __LINE__)
        end if

        var_is_tkr_compatible(:) = .false.

        if (.not. associated(pio_file)) then
            return
        end if

        if (.not. pio_file_is_open(pio_file)) then
            return
        end if

        call self % debug_print(log_level_verbose, 'Checking variable "' // trim(adjustl(var_info % name)) // &
            '" for presence and TKR compatibility')

        do i = 1, size(var_name_list)
            ! Check if the variable is present on the file.
            ierr = pio_inq_varid(pio_file, trim(adjustl(var_name_list(i))), varid)

            if (ierr /= pio_noerr) then
                cycle
            end if

            var_is_present(i) = .true.

            ! Check if the variable is "TK"R compatible between MPAS and the file.
            ierr = pio_inq_vartype(pio_file, varid, vartype)

            if (ierr /= pio_noerr) then
                cycle
            end if

            select case (trim(adjustl(var_info % type)))
                case ('character')
                    if (vartype /= pio_char) then
                        cycle
                    end if
                case ('integer')
                    if (vartype /= pio_int) then
                        cycle
                    end if
                case ('real')
                    ! When MPAS dynamical core is compiled at single precision, pairing it with double precision input data
                    ! is not allowed to prevent loss of precision.
                    if (rkind == r4kind .and. vartype /= pio_real) then
                        cycle
                    end if

                    ! When MPAS dynamical core is compiled at double precision, pairing it with single and double precision
                    ! input data is allowed.
                    if (rkind == r8kind .and. vartype /= pio_real .and. vartype /= pio_double) then
                        cycle
                    end if
                case default
                    cycle
            end select

            ! Check if the variable is TK"R" compatible between MPAS and the file.
            ierr = pio_inq_varndims(pio_file, varid, varndims)

            if (ierr /= pio_noerr) then
                cycle
            end if

            if (varndims /= var_info % rank) then
                cycle
            end if

            var_is_tkr_compatible(i) = .true.
        end do

        call self % debug_print(log_level_debug, 'var_name_list = ' // stringify(var_name_list))
        call self % debug_print(log_level_debug, 'var_is_present = ' // stringify(var_is_present))
        call self % debug_print(log_level_debug, 'var_is_tkr_compatible = ' // stringify(var_is_tkr_compatible))

        call self % debug_print(log_level_debug, subname // ' completed')
    end subroutine dyn_mpas_check_variable_status

    !-------------------------------------------------------------------------------
    ! subroutine dyn_mpas_exchange_halo
    !
    !> summary: Update the halo layers of the named field.
    !> author: Michael Duda
    !> date: 16 January 2020
    !>
    !> Given a field name that is defined in MPAS registry, this subroutine updates
    !> the halo layers for that field.
    !> Ported and refactored for CAM-SIMA. (KCW, 2024-03-18)
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_exchange_halo(self, field_name)
        ! Module(s) from MPAS.
        use mpas_derived_types, only: field1dinteger, field2dinteger, field3dinteger, &
                                      field1dreal, field2dreal, field3dreal, field4dreal, field5dreal, &
                                      mpas_pool_field_info_type, mpas_pool_integer, mpas_pool_real
        use mpas_dmpar, only: mpas_dmpar_exch_halo_field
        use mpas_pool_routines, only: mpas_pool_get_field, mpas_pool_get_field_info

        class(mpas_dynamical_core_type), intent(in) :: self
        character(*), intent(in) :: field_name

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_exchange_halo'
        type(field1dinteger), pointer :: field_1d_integer
        type(field2dinteger), pointer :: field_2d_integer
        type(field3dinteger), pointer :: field_3d_integer
        type(field1dreal), pointer :: field_1d_real
        type(field2dreal), pointer :: field_2d_real
        type(field3dreal), pointer :: field_3d_real
        type(field4dreal), pointer :: field_4d_real
        type(field5dreal), pointer :: field_5d_real
        type(mpas_pool_field_info_type) :: mpas_pool_field_info

        call self % debug_print(log_level_debug, subname // ' entered')

        nullify(field_1d_integer)
        nullify(field_2d_integer)
        nullify(field_3d_integer)
        nullify(field_1d_real)
        nullify(field_2d_real)
        nullify(field_3d_real)
        nullify(field_4d_real)
        nullify(field_5d_real)

        call self % debug_print(log_level_info, 'Inquiring field information for "' // trim(adjustl(field_name)) // '"')

        call mpas_pool_get_field_info(self % domain_ptr % blocklist % allfields, &
            trim(adjustl(field_name)), mpas_pool_field_info)

        if (mpas_pool_field_info % fieldtype == -1 .or. &
            mpas_pool_field_info % ndims == -1 .or. &
            mpas_pool_field_info % nhalolayers == -1) then
            call self % model_error('Invalid field information for "' // trim(adjustl(field_name)) // '"', subname, __LINE__)
        end if

        ! No halo layers to exchange. This field is not decomposed.
        if (mpas_pool_field_info % nhalolayers == 0) then
            call self % debug_print(log_level_info, 'Skipping field "' // trim(adjustl(field_name)) // &
                '" due to not decomposed')

            return
        end if

        call self % debug_print(log_level_info, 'Exchanging halo layers for "' // trim(adjustl(field_name)) // '"')

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

        call self % debug_print(log_level_debug, subname // ' completed')
    end subroutine dyn_mpas_exchange_halo

    !-------------------------------------------------------------------------------
    ! subroutine dyn_mpas_compute_unit_vector
    !
    !> summary: Compute local east, north, and edge-normal unit vectors.
    !> author: Michael Duda
    !> date: 15 January 2020
    !>
    !> This subroutine computes the local east and north unit vectors at all cells,
    !> storing the results in MPAS "mesh" pool as the "east" and "north" variables,
    !> respectively. It also computes the edge-normal unit vectors at all edges by
    !> calling `mpas_initialize_vectors`.
    !> Before calling this subroutine, MPAS "mesh" pool must contain the "latCell"
    !> and "lonCell" variables that are valid for all cells (not just solve cells),
    !> plus any additional variables that are required by
    !> `mpas_initialize_vectors`.
    !> For stand-alone MPAS, the whole deal is handled by `init_dirs_forphys`
    !> during physics initialization. However, MPAS as a dynamical core does
    !> not have physics, hence the existence of this subroutine.
    !> Ported and refactored for CAM-SIMA. (KCW, 2024-04-23)
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_compute_unit_vector(self)
        ! Module(s) from MPAS.
        use mpas_derived_types, only: mpas_pool_type
        use mpas_vector_operations, only: mpas_initialize_vectors

        class(mpas_dynamical_core_type), intent(in) :: self

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_compute_unit_vector'
        integer :: i
        integer, pointer :: ncells
        real(rkind), pointer :: latcell(:), loncell(:)
        real(rkind), pointer :: east(:, :), north(:, :)
        type(mpas_pool_type), pointer :: mpas_pool

        call self % debug_print(log_level_debug, subname // ' entered')

        nullify(ncells)
        nullify(latcell, loncell)

        nullify(east, north)

        nullify(mpas_pool)

        ! Input.
        call self % get_variable_pointer(ncells, 'dim', 'nCells')
        call self % get_variable_pointer(latcell, 'mesh', 'latCell')
        call self % get_variable_pointer(loncell, 'mesh', 'lonCell')

        ! Output.
        call self % get_variable_pointer(east, 'mesh', 'east')
        call self % get_variable_pointer(north, 'mesh', 'north')

        call self % debug_print(log_level_info, 'Computing unit vectors')

        do i = 1, ncells
            east(1, i) = -sin(loncell(i))
            east(2, i) =  cos(loncell(i))
            east(3, i) =  0.0_rkind
            ! `r3_normalize` has been inlined below.
            east(1:3, i) = east(1:3, i) / sqrt(sum(east(1:3, i) * east(1:3, i)))

            north(1, i) = -cos(loncell(i)) * sin(latcell(i))
            north(2, i) = -sin(loncell(i)) * sin(latcell(i))
            north(3, i) =  cos(latcell(i))
            ! `r3_normalize` has been inlined below.
            north(1:3, i) = north(1:3, i) / sqrt(sum(north(1:3, i) * north(1:3, i)))
        end do

        nullify(ncells)
        nullify(latcell, loncell)

        nullify(east, north)

        call self % get_pool_pointer(mpas_pool, 'mesh')
        call mpas_initialize_vectors(mpas_pool)

        nullify(mpas_pool)

        call self % debug_print(log_level_debug, subname // ' completed')
    end subroutine dyn_mpas_compute_unit_vector

    !-------------------------------------------------------------------------------
    ! subroutine dyn_mpas_compute_edge_wind
    !
    !> summary: Compute the edge-normal wind (tendency) vectors at edge points.
    !> author: Michael Duda
    !> date: 16 January 2020
    !>
    !> This subroutine computes the edge-normal wind vectors at edge points (i.e.,
    !> the "u" variable in MPAS "state" pool) from the wind components at cell
    !> points (i.e., the "uReconstruct{Zonal,Meridional}" variables in MPAS "diag"
    !> pool). In MPAS, the former are PROGNOSTIC variables, while the latter are
    !> DIAGNOSTIC variables that are "reconstructed" from the former.
    !> This subroutine is essentially the inverse function of that reconstruction.
    !> The purpose is to provide an alternative way for MPAS to initialize from
    !> zonal and meridional wind components at cell points.
    !> If `wind_tendency` is `.true.`, this subroutine operates on the wind
    !> tendency due to physics instead.
    !> Ported and refactored for CAM-SIMA. (KCW, 2024-05-08)
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_compute_edge_wind(self, wind_tendency)
        class(mpas_dynamical_core_type), intent(in) :: self
        logical, intent(in) :: wind_tendency

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_compute_edge_wind'
        integer :: cell1, cell2, i
        integer, pointer :: cellsonedge(:, :)
        integer, pointer :: nedges
        real(rkind), pointer :: east(:, :), north(:, :)
        real(rkind), pointer :: edgenormalvectors(:, :)
        real(rkind), pointer :: ucellzonal(:, :), ucellmeridional(:, :)
        real(rkind), pointer :: uedge(:, :)

        call self % debug_print(log_level_debug, subname // ' entered')

        nullify(nedges)

        nullify(ucellzonal, ucellmeridional)

        nullify(cellsonedge)
        nullify(east, north)
        nullify(edgenormalvectors)

        nullify(uedge)

        ! Make sure halo layers are up-to-date before computation.
        if (wind_tendency) then
            call self % exchange_halo('tend_uzonal')
            call self % exchange_halo('tend_umerid')
        else
            call self % exchange_halo('uReconstructZonal')
            call self % exchange_halo('uReconstructMeridional')
        end if

        ! Input.
        call self % get_variable_pointer(nedges, 'dim', 'nEdges')

        if (wind_tendency) then
            call self % get_variable_pointer(ucellzonal, 'tend_physics', 'tend_uzonal')
            call self % get_variable_pointer(ucellmeridional, 'tend_physics', 'tend_umerid')
        else
            call self % get_variable_pointer(ucellzonal, 'diag', 'uReconstructZonal')
            call self % get_variable_pointer(ucellmeridional, 'diag', 'uReconstructMeridional')
        end if

        call self % get_variable_pointer(cellsonedge, 'mesh', 'cellsOnEdge')
        call self % get_variable_pointer(east, 'mesh', 'east')
        call self % get_variable_pointer(north, 'mesh', 'north')
        call self % get_variable_pointer(edgenormalvectors, 'mesh', 'edgeNormalVectors')

        ! Output.
        if (wind_tendency) then
            call self % get_variable_pointer(uedge, 'tend_physics', 'tend_ru_physics')
        else
            call self % get_variable_pointer(uedge, 'state', 'u', time_level=1)
        end if

        if (wind_tendency) then
            call self % debug_print(log_level_info, 'Computing edge-normal wind tendency vectors')
        else
            call self % debug_print(log_level_info, 'Computing edge-normal wind vectors')
        end if

        do i = 1, nedges
            cell1 = cellsonedge(1, i)
            cell2 = cellsonedge(2, i)

            uedge(:, i) = ucellzonal(:, cell1) * 0.5_rkind * (edgenormalvectors(1, i) * east(1, cell1)  + &
                                                              edgenormalvectors(2, i) * east(2, cell1)  + &
                                                              edgenormalvectors(3, i) * east(3, cell1)) + &
                          ucellmeridional(:, cell1) * 0.5_rkind * (edgenormalvectors(1, i) * north(1, cell1)  + &
                                                                   edgenormalvectors(2, i) * north(2, cell1)  + &
                                                                   edgenormalvectors(3, i) * north(3, cell1)) + &
                          ucellzonal(:, cell2) * 0.5_rkind * (edgenormalvectors(1, i) * east(1, cell2)  + &
                                                              edgenormalvectors(2, i) * east(2, cell2)  + &
                                                              edgenormalvectors(3, i) * east(3, cell2)) + &
                          ucellmeridional(:, cell2) * 0.5_rkind * (edgenormalvectors(1, i) * north(1, cell2)  + &
                                                                   edgenormalvectors(2, i) * north(2, cell2)  + &
                                                                   edgenormalvectors(3, i) * north(3, cell2))
        end do

        nullify(nedges)

        nullify(ucellzonal, ucellmeridional)

        nullify(cellsonedge)
        nullify(east, north)
        nullify(edgenormalvectors)

        nullify(uedge)

        ! Make sure halo layers are up-to-date after computation.
        if (wind_tendency) then
            call self % exchange_halo('tend_ru_physics')
        else
            call self % exchange_halo('u')
        end if

        call self % debug_print(log_level_debug, subname // ' completed')
    end subroutine dyn_mpas_compute_edge_wind

    !-------------------------------------------------------------------------------
    ! subroutine dyn_mpas_compute_cell_relative_vorticity
    !
    !> summary: Compute the relative vorticities at cell points.
    !> author: Kuan-Chih Wang
    !> date: 2025-04-12
    !>
    !> MPAS uses staggered C-grid for spatial discretization, where relative
    !> vorticities are located at vertex points because wind vectors are located at
    !> edge points. However, physics schemes that use relative vorticities as input
    !> usually want them at cell points instead.
    !> This subroutine computes the relative vorticity at each cell point from its
    !> surrounding vertex points and returns the results.
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_compute_cell_relative_vorticity(self, cell_relative_vorticity)
        class(mpas_dynamical_core_type), intent(in) :: self
        real(rkind), allocatable, intent(out) :: cell_relative_vorticity(:, :)

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_compute_cell_relative_vorticity'
        integer :: i, k
        integer :: ierr
        integer, pointer :: ncellssolve, nvertlevels
        integer, pointer :: kiteforcell(:, :), nedgesoncell(:), verticesoncell(:, :)
        real(rkind), pointer :: areacell(:), kiteareasonvertex(:, :), vorticity(:, :)

        nullify(ncellssolve, nvertlevels)
        nullify(kiteforcell, nedgesoncell, verticesoncell)
        nullify(areacell, kiteareasonvertex, vorticity)

        ! Input.
        call self % get_variable_pointer(ncellssolve, 'dim', 'nCellsSolve')
        call self % get_variable_pointer(nvertlevels, 'dim', 'nVertLevels')

        call self % get_variable_pointer(kiteforcell, 'mesh', 'kiteForCell')
        call self % get_variable_pointer(nedgesoncell, 'mesh', 'nEdgesOnCell')
        call self % get_variable_pointer(verticesoncell, 'mesh', 'verticesOnCell')

        call self % get_variable_pointer(areacell, 'mesh', 'areaCell')
        call self % get_variable_pointer(kiteareasonvertex, 'mesh', 'kiteAreasOnVertex')
        call self % get_variable_pointer(vorticity, 'diag', 'vorticity')

        ! Output.
        allocate(cell_relative_vorticity(nvertlevels, ncellssolve), stat=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to allocate cell_relative_vorticity', subname, __LINE__)
        end if

        do i = 1, ncellssolve
            do k = 1, nvertlevels
                cell_relative_vorticity(k, i) = regrid_from_vertex_to_cell(i, k, &
                    nedgesoncell, verticesoncell, kiteforcell, kiteareasonvertex, areacell, &
                    vorticity)
            end do
        end do

        nullify(ncellssolve, nvertlevels)
        nullify(kiteforcell, nedgesoncell, verticesoncell)
        nullify(areacell, kiteareasonvertex, vorticity)
    end subroutine dyn_mpas_compute_cell_relative_vorticity

    !-------------------------------------------------------------------------------
    ! function regrid_from_vertex_to_cell
    !
    !> summary: Regrid values from vertex points to the specified cell point.
    !> author: Kuan-Chih Wang
    !> date: 2025-04-12
    !>
    !> This function computes the area weighted average (i.e., `cell_value`) at the
    !> specified cell point (i.e., `cell_index` and `cell_level`) from the values
    !> at its surrounding vertex points (i.e., `vertex_value`).
    !> The formulation used here is adapted and generalized from the
    !> `atm_compute_solve_diagnostics` subroutine in MPAS.
    !
    !-------------------------------------------------------------------------------
    pure function regrid_from_vertex_to_cell(cell_index, cell_level, &
            nverticesoncell, verticesoncell, kiteforcell, kiteareasonvertex, areacell, &
            vertex_value) result(cell_value)
        integer, intent(in) :: cell_index, cell_level
        integer, intent(in) :: nverticesoncell(:), verticesoncell(:, :), kiteforcell(:, :)
        real(rkind), intent(in) :: kiteareasonvertex(:, :), areacell(:)
        real(rkind), intent(in) :: vertex_value(:, :)
        real(rkind) :: cell_value

        integer :: i, j, vertex_index

        cell_value = 0.0_rkind

        do i = 1, nverticesoncell(cell_index)
            j = kiteforcell(i, cell_index)
            vertex_index = verticesoncell(i, cell_index)

            cell_value = cell_value + &
                kiteareasonvertex(j, vertex_index) * vertex_value(cell_level, vertex_index)
        end do

        cell_value = cell_value / areacell(cell_index)
    end function regrid_from_vertex_to_cell

    !-------------------------------------------------------------------------------
    ! subroutine dyn_mpas_init_phase4
    !
    !> summary: Track `atm_core_init` to finish MPAS dynamical core initialization.
    !> author: Michael Duda
    !> date: 29 February 2020
    !>
    !> This subroutine completes MPAS dynamical core initialization.
    !> Essentially, it closely follows what is done in `atm_core_init`, but without
    !> any calls to MPAS diagnostics manager or MPAS stream manager.
    !> Ported and refactored for CAM-SIMA. (KCW, 2024-05-25)
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_init_phase4(self, coupling_time_interval)
        ! Module(s) from MPAS.
        use atm_core, only: atm_mpas_init_block
        use atm_time_integration, only: mpas_atm_dynamics_init
        use mpas_atm_dimensions, only: mpas_atm_set_dims
        use mpas_atm_halos, only: atm_build_halo_groups, exchange_halo_group
        use mpas_atm_threading, only: mpas_atm_threading_init
        use mpas_attlist, only: mpas_modify_att
        use mpas_constants, only: mpas_constants_compute_derived
        use mpas_derived_types, only: field0dreal, field2dreal, mpas_pool_type, mpas_time_type, &
                                      mpas_start_time
        use mpas_field_routines, only: mpas_allocate_scratch_field
        use mpas_pool_routines, only: mpas_pool_get_field, mpas_pool_initialize_time_levels
        use mpas_string_utils, only: mpas_string_replace
        use mpas_timekeeping, only: mpas_get_clock_time, mpas_get_time

        class(mpas_dynamical_core_type), intent(inout) :: self
        integer, intent(in) :: coupling_time_interval ! Set the time interval, in seconds, over which MPAS dynamical core
                                                      ! should integrate each time it is called to run.

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_init_phase4'
        character(strkind) :: date_time
        character(strkind), pointer :: initial_time_1, initial_time_2
        character(strkind), pointer :: xtime
        integer :: ierr
        integer, pointer :: nvertlevels, maxedges, maxedges2, num_scalars
        logical, pointer :: config_do_restart
        real(rkind), pointer :: config_dt
        type(field0dreal), pointer :: field_0d_real
        type(field2dreal), pointer :: field_2d_real
        type(mpas_pool_type), pointer :: mpas_pool
        type(mpas_time_type) :: mpas_time

        call self % debug_print(log_level_debug, subname // ' entered')

        nullify(initial_time_1, initial_time_2)
        nullify(xtime)
        nullify(nvertlevels, maxedges, maxedges2, num_scalars)
        nullify(config_do_restart)
        nullify(config_dt)
        nullify(field_0d_real)
        nullify(field_2d_real)
        nullify(mpas_pool)

        if (coupling_time_interval <= 0) then
            call self % model_error('Invalid coupling time interval ' // stringify([real(coupling_time_interval, rkind)]), &
                subname, __LINE__)
        end if

        call self % get_variable_pointer(config_dt, 'cfg', 'config_dt')

        if (config_dt <= 0.0_rkind) then
            call self % model_error('Invalid time step ' // stringify([config_dt]), &
                subname, __LINE__)
        end if

        ! `config_dt` in MPAS is a floating-point number. Testing floating-point numbers for divisibility is not trivial and
        ! should be done carefully.
        if (.not. almost_divisible(real(coupling_time_interval, rkind), config_dt)) then
            call self % model_error('Coupling time interval ' // stringify([real(coupling_time_interval, rkind)]) // &
                ' must be divisible by time step ' // stringify([config_dt]), subname, __LINE__)
        end if

        self % coupling_time_interval = coupling_time_interval
        self % number_of_time_steps = 0

        call self % debug_print(log_level_info, 'Coupling time interval is ' // &
            stringify([real(self % coupling_time_interval, rkind)]) // ' seconds')
        call self % debug_print(log_level_info, 'Time step is ' // &
            stringify([config_dt]) // ' seconds')

        nullify(config_dt)

        ! Compute derived constants.
        call mpas_constants_compute_derived()

        ! Set up OpenMP threading.
        call self % debug_print(log_level_info, 'Setting up OpenMP threading')

        call mpas_atm_threading_init(self % domain_ptr % blocklist, ierr=ierr)

        if (ierr /= 0) then
            call self % model_error('OpenMP threading setup failed for core ' // trim(self % domain_ptr % core % corename), &
                subname, __LINE__)
        end if

        ! Set up inner dimensions used by arrays in optimized dynamics subroutines.
        call self % debug_print(log_level_info, 'Setting up dimensions')

        call self % get_variable_pointer(nvertlevels, 'dim', 'nVertLevels')
        call self % get_variable_pointer(maxedges, 'dim', 'maxEdges')
        call self % get_variable_pointer(maxedges2, 'dim', 'maxEdges2')
        call self % get_variable_pointer(num_scalars, 'dim', 'num_scalars')

        call mpas_atm_set_dims(nvertlevels, maxedges, maxedges2, num_scalars)

        nullify(nvertlevels, maxedges, maxedges2, num_scalars)

        ! Build halo exchange groups and set the `exchange_halo_group` procedure pointer, which is used to
        ! exchange the halo layers of all fields in the named group.
        nullify(exchange_halo_group)

        call atm_build_halo_groups(self % domain_ptr, ierr=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to build halo exchange groups', subname, __LINE__)
        end if

        if (.not. associated(exchange_halo_group)) then
            call self % model_error('Failed to build halo exchange groups', subname, __LINE__)
        end if

        ! Variables in MPAS "state" pool have more than one time level. Copy the values from the first time level of
        ! such variables into all subsequent time levels to initialize them.
        call self % get_variable_pointer(config_do_restart, 'cfg', 'config_do_restart')

        if (.not. config_do_restart) then
            ! Run type is initial run.
            call self % debug_print(log_level_info, 'Initializing time levels')

            call self % get_pool_pointer(mpas_pool, 'state')

            call mpas_pool_initialize_time_levels(mpas_pool)

            nullify(mpas_pool)
        end if

        nullify(config_do_restart)

        call exchange_halo_group(self % domain_ptr, 'initialization:u', ierr=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to exchange halo layers for group "initialization:u"', subname, __LINE__)
        end if

        ! Initialize atmospheric variables (e.g., momentum, thermodynamic... variables in governing equations)
        ! as well as various aspects of time in MPAS.

        call self % debug_print(log_level_info, 'Initializing atmospheric variables')

        ! Controlled by `config_start_time` in namelist.
        mpas_time = mpas_get_clock_time(self % domain_ptr % clock, mpas_start_time, ierr=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to get time for "mpas_start_time"', subname, __LINE__)
        end if

        call mpas_get_time(mpas_time, datetimestring=date_time, ierr=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to get time for "mpas_start_time"', subname, __LINE__)
        end if

        ! Controlled by `config_dt` in namelist.
        call self % get_pool_pointer(mpas_pool, 'mesh')
        call self % get_variable_pointer(config_dt, 'cfg', 'config_dt')

        call atm_mpas_init_block(self % domain_ptr % dminfo, self % domain_ptr % streammanager, self % domain_ptr % blocklist, &
            mpas_pool, config_dt)

        nullify(mpas_pool)
        nullify(config_dt)

        call self % get_variable_pointer(xtime, 'state', 'xtime', time_level=1)

        xtime = date_time

        nullify(xtime)

        ! Initialize `initial_time` in the second time level. We need to do this manually because initial states
        ! are read into time level 1, and if we write anything from time level 2, `initial_time` will be invalid.
        call self % get_variable_pointer(initial_time_1, 'state', 'initial_time', time_level=1)
        call self % get_variable_pointer(initial_time_2, 'state', 'initial_time', time_level=2)

        initial_time_2 = initial_time_1

        ! Set time units to CF-compliant "seconds since <date and time>".
        call self % get_pool_pointer(mpas_pool, 'state')

        call mpas_pool_get_field(mpas_pool, 'Time', field_0d_real, timelevel=1)

        if (.not. associated(field_0d_real)) then
            call self % model_error('Failed to find variable "Time"', subname, __LINE__)
        end if

        call mpas_modify_att(field_0d_real % attlists(1) % attlist, 'units', &
            'seconds since ' // mpas_string_replace(initial_time_1, '_', ' '), ierr=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to set time units', subname, __LINE__)
        end if

        nullify(initial_time_1, initial_time_2)
        nullify(mpas_pool)
        nullify(field_0d_real)

        call exchange_halo_group(self % domain_ptr, 'initialization:pv_edge,ru,rw', ierr=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to exchange halo layers for group "initialization:pv_edge,ru,rw"', subname, __LINE__)
        end if

        call self % debug_print(log_level_info, 'Initializing dynamics')

        ! Prepare dynamics for time integration.
        call mpas_atm_dynamics_init(self % domain_ptr)

        ! Some additional "scratch" fields are needed for interoperability with CAM-SIMA, but they are not initialized by
        ! `mpas_atm_dynamics_init`. Initialize them below.
        call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, 'tend_uzonal', field_2d_real, timelevel=1)
        call mpas_allocate_scratch_field(field_2d_real)
        nullify(field_2d_real)

        call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, 'tend_umerid', field_2d_real, timelevel=1)
        call mpas_allocate_scratch_field(field_2d_real)
        nullify(field_2d_real)

        call self % debug_print(log_level_debug, subname // ' completed')

        call self % debug_print(log_level_info, 'Successful initialization of MPAS dynamical core')
    contains
        !> Test if `a` is divisible by `b`, where `a` and `b` are both reals.
        !> (KCW, 2024-05-25)
        pure function almost_divisible(a, b)
            real(rkind), intent(in) :: a, b
            logical :: almost_divisible

            real(rkind) :: error_tolerance

            error_tolerance = epsilon(1.0_rkind) * max(abs(a), abs(b))

            if (almost_equal(mod(abs(a), abs(b)), 0.0_rkind, absolute_tolerance=error_tolerance) .or. &
                almost_equal(mod(abs(a), abs(b)), abs(b), absolute_tolerance=error_tolerance)) then
                almost_divisible = .true.

                return
            end if

            almost_divisible = .false.
        end function almost_divisible

        !> Test `a` and `b` for approximate equality, where `a` and `b` are both reals.
        !> (KCW, 2024-05-25)
        pure function almost_equal(a, b, absolute_tolerance, relative_tolerance)
            real(rkind), intent(in) :: a, b
            real(rkind), optional, intent(in) :: absolute_tolerance, relative_tolerance
            logical :: almost_equal

            real(rkind) :: error_tolerance

            if (present(relative_tolerance)) then
                error_tolerance = relative_tolerance * max(abs(a), abs(b))
            else
                error_tolerance = epsilon(1.0_rkind) * max(abs(a), abs(b))
            end if

            if (present(absolute_tolerance)) then
                error_tolerance = max(absolute_tolerance, error_tolerance)
            end if

            if (abs(a - b) <= error_tolerance) then
                almost_equal = .true.

                return
            end if

            almost_equal = .false.
        end function almost_equal
    end subroutine dyn_mpas_init_phase4

    !-------------------------------------------------------------------------------
    ! subroutine dyn_mpas_run
    !
    !> summary: Integrate the dynamical states with time.
    !> author: Michael Duda
    !> date: 29 February 2020
    !>
    !> This subroutine calls MPAS dynamical solver in a loop, with each iteration
    !> of the loop advancing the dynamical states forward by one time step, until
    !> the coupling time interval is reached.
    !> Essentially, it closely follows what is done in `atm_core_run`, but without
    !> any calls to MPAS diagnostics manager or MPAS stream manager.
    !> Ported and refactored for CAM-SIMA. (KCW, 2024-06-21)
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_run(self)
        ! Module(s) from MPAS.
        use atm_core, only: atm_compute_output_diagnostics, atm_do_timestep
        use mpas_derived_types, only: mpas_pool_type, mpas_time_type, mpas_timeinterval_type, &
                                      mpas_now
        use mpas_pool_routines, only: mpas_pool_shift_time_levels
        use mpas_timekeeping, only: mpas_advance_clock, mpas_get_clock_time, mpas_get_time, &
                                    mpas_set_timeinterval, &
                                    operator(+), operator(<)

        class(mpas_dynamical_core_type), intent(inout) :: self

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_run'
        character(strkind) :: date_time
        integer :: ierr
        real(rkind), pointer :: config_dt
        type(mpas_pool_type), pointer :: mpas_pool_diag, mpas_pool_mesh, mpas_pool_state
        type(mpas_time_type) :: mpas_time_end, mpas_time_now ! This derived type is analogous to `ESMF_Time`.
        type(mpas_timeinterval_type) :: mpas_time_interval   ! This derived type is analogous to `ESMF_TimeInterval`.

        call self % debug_print(log_level_debug, subname // ' entered')

        nullify(config_dt)
        nullify(mpas_pool_diag, mpas_pool_mesh, mpas_pool_state)

        call self % get_variable_pointer(config_dt, 'cfg', 'config_dt')
        call self % get_pool_pointer(mpas_pool_diag, 'diag')
        call self % get_pool_pointer(mpas_pool_mesh, 'mesh')
        call self % get_pool_pointer(mpas_pool_state, 'state')

        mpas_time_now = mpas_get_clock_time(self % domain_ptr % clock, mpas_now, ierr=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to get time for "mpas_now"', subname, __LINE__)
        end if

        call mpas_get_time(mpas_time_now, datetimestring=date_time, ierr=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to get time for "mpas_now"', subname, __LINE__)
        end if

        call self % debug_print(log_level_info, 'Time integration of MPAS dynamical core begins at ' // trim(adjustl(date_time)))

        call mpas_set_timeinterval(mpas_time_interval, s=self % coupling_time_interval, ierr=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to set coupling time interval', subname, __LINE__)
        end if

        ! The `+` operator is overloaded here.
        mpas_time_end = mpas_time_now + mpas_time_interval

        ! Integrate until the coupling time interval is reached.
        ! The `<` operator is overloaded here.
        do while (mpas_time_now < mpas_time_end)
            ! Number of time steps that has been completed in this MPAS dynamical core instance.
            self % number_of_time_steps = self % number_of_time_steps + 1

            ! Advance the dynamical states forward in time by `config_dt` seconds.
            ! Current states are in time level 1. Upon exit, time level 2 will contain updated states.
            call atm_do_timestep(self % domain_ptr, config_dt, self % number_of_time_steps)

            ! MPAS "state" pool has two time levels.
            ! Swap them after advancing a time step.
            call mpas_pool_shift_time_levels(mpas_pool_state)

            call mpas_advance_clock(self % domain_ptr % clock, ierr=ierr)

            if (ierr /= 0) then
                call self % model_error('Failed to advance clock', subname, __LINE__)
            end if

            mpas_time_now = mpas_get_clock_time(self % domain_ptr % clock, mpas_now, ierr=ierr)

            if (ierr /= 0) then
                call self % model_error('Failed to get time for "mpas_now"', subname, __LINE__)
            end if

            call self % debug_print(log_level_info, 'Time step ' // stringify([self % number_of_time_steps]) // ' completed')
        end do

        call mpas_get_time(mpas_time_now, datetimestring=date_time, ierr=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to get time for "mpas_now"', subname, __LINE__)
        end if

        call self % debug_print(log_level_info, 'Time integration of MPAS dynamical core ends at ' // trim(adjustl(date_time)))

        ! Compute diagnostic variables like "pressure", "rho", and "theta" from time level 1 of MPAS "state" pool
        ! by calling upstream MPAS functionality.
        call atm_compute_output_diagnostics(mpas_pool_state, 1, mpas_pool_diag, mpas_pool_mesh)

        nullify(config_dt)
        nullify(mpas_pool_diag, mpas_pool_mesh, mpas_pool_state)

        call self % debug_print(log_level_debug, subname // ' completed')
    end subroutine dyn_mpas_run

    !-------------------------------------------------------------------------------
    ! subroutine dyn_mpas_final
    !
    !> summary: Finalize MPAS dynamical core as well as its framework.
    !> author: Michael Duda
    !> date: 29 February 2020
    !>
    !> This subroutine finalizes and cleans up MPAS dynamical core as well as its
    !> framework that was set up during initialization. Finalization happens in
    !> reverse chronological order.
    !> Essentially, it closely follows what is done in `atm_core_finalize` and
    !> `mpas_finalize`, except that here, there is no need to call MPAS diagnostics
    !> manager or MPAS stream manager.
    !> Ported and refactored for CAM-SIMA. (KCW, 2024-10-10)
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_final(self)
        ! Module(s) from MPAS.
        use atm_time_integration, only: mpas_atm_dynamics_finalize
        use mpas_atm_halos, only: atm_destroy_halo_groups, exchange_halo_group
        use mpas_atm_threading, only: mpas_atm_threading_finalize
        use mpas_decomp, only: mpas_decomp_destroy_decomp_list
        use mpas_derived_types, only: field2dreal
        use mpas_field_routines, only: mpas_deallocate_scratch_field
        use mpas_framework, only: mpas_framework_finalize
        use mpas_log, only: mpas_log_finalize
        use mpas_pool_routines, only: mpas_pool_get_field
        use mpas_timekeeping, only: mpas_destroy_clock
        use mpas_timer, only: mpas_timer_write_header, mpas_timer_write, mpas_timer_finalize

        class(mpas_dynamical_core_type), intent(inout) :: self

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_final'
        integer :: ierr
        type(field2dreal), pointer :: field_2d_real

        call self % debug_print(log_level_debug, subname // ' entered')

        nullify(field_2d_real)

        ! First, wind down MPAS dynamical core by calling its own finalization procedures.

        ! Some additional "scratch" fields are needed for interoperability with CAM-SIMA, but they are not finalized by
        ! `mpas_atm_dynamics_finalize`. Finalize them below.
        call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, 'tend_uzonal', field_2d_real, timelevel=1)
        call mpas_deallocate_scratch_field(field_2d_real)
        nullify(field_2d_real)

        call mpas_pool_get_field(self % domain_ptr % blocklist % allfields, 'tend_umerid', field_2d_real, timelevel=1)
        call mpas_deallocate_scratch_field(field_2d_real)
        nullify(field_2d_real)

        call self % debug_print(log_level_info, 'Finalizing dynamics')

        ! Opposite to `mpas_atm_dynamics_init`.
        call mpas_atm_dynamics_finalize(self % domain_ptr)

        ! Opposite to `atm_build_halo_groups`.
        call atm_destroy_halo_groups(self % domain_ptr, ierr=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to destroy halo exchange groups', subname, __LINE__)
        end if

        nullify(exchange_halo_group)

        call self % debug_print(log_level_info, 'Cleaning up OpenMP threading')

        ! Opposite to `mpas_atm_threading_init`.
        call mpas_atm_threading_finalize(self % domain_ptr % blocklist, ierr=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to clean up OpenMP threading', subname, __LINE__)
        end if

        call self % debug_print(log_level_info, 'Cleaning up clock')

        ! Opposite to `mpas_create_clock`, which was called by `atm_simulation_clock_init`, then `atm_setup_clock`.
        call mpas_destroy_clock(self % domain_ptr % clock, ierr=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to clean up clock', subname, __LINE__)
        end if

        call self % debug_print(log_level_info, 'Cleaning up decompositions')

        ! Opposite to `mpas_decomp_create_decomp_list`, which was called by `atm_setup_decompositions`.
        call mpas_decomp_destroy_decomp_list(self % domain_ptr % decompositions)

        deallocate(self % domain_ptr % streaminfo)

        ! Write timing information to log.
        call mpas_timer_write_header()
        call mpas_timer_write()

        ! Opposite to `mpas_timer_init`, which was called by `mpas_framework_init_phase2`.
        call mpas_timer_finalize(self % domain_ptr)

        call self % debug_print(log_level_info, 'Cleaning up log')

        ! Opposite to `mpas_log_init`, which was called by `atm_setup_log`.
        call mpas_log_finalize(ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to clean up log', subname, __LINE__)
        end if

        call self % debug_print(log_level_info, 'Finalizing MPAS framework')

        ! Opposite to `mpas_framework_init_phase1` and `mpas_framework_init_phase2`.
        call mpas_framework_finalize(self % domain_ptr % dminfo, self % domain_ptr)

        call self % debug_print(log_level_debug, subname // ' completed')

        call self % debug_print(log_level_info, 'Successful finalization of MPAS dynamical core')

        ! Second, clean up this MPAS dynamical core instance.

        ! Initialized by `dyn_mpas_init_phase4`.
        self % coupling_time_interval = 0
        self % number_of_time_steps = 0

        ! Initialized by `dyn_mpas_define_scalar`.
        deallocate(self % constituent_name)
        deallocate(self % index_constituent_to_mpas_scalar)
        deallocate(self % index_mpas_scalar_to_constituent)
        deallocate(self % is_water_species)

        ! Initialized by `dyn_mpas_init_phase3`.
        self % number_of_constituents = 0

        ! Initialized by `dyn_mpas_init_phase1`.
        self % log_level = log_level_quiet
        self % log_unit = output_unit
        self % mpi_comm = mpi_comm_null
        self % mpi_rank = 0
        self % mpi_rank_root = .false.

        nullify(self % model_error)

        deallocate(self % corelist % domainlist)
        deallocate(self % corelist)

        nullify(self % corelist)
        nullify(self % domain_ptr)
    end subroutine dyn_mpas_final

    !-------------------------------------------------------------------------------
    ! function dyn_mpas_get_constituent_name
    !
    !> summary: Query constituent name by its index.
    !> author: Kuan-Chih Wang
    !> date: 2024-05-16
    !>
    !> This function returns the constituent name that corresponds to the given
    !> constituent index. In case of errors, an empty character string is produced.
    !
    !-------------------------------------------------------------------------------
    pure function dyn_mpas_get_constituent_name(self, constituent_index) result(constituent_name)
        class(mpas_dynamical_core_type), intent(in) :: self
        integer, intent(in) :: constituent_index

        character(:), allocatable :: constituent_name

        ! Catch segmentation fault.
        if (.not. allocated(self % constituent_name)) then
            constituent_name = ''

            return
        end if

        if (constituent_index < lbound(self % constituent_name, 1) .or. &
            constituent_index > ubound(self % constituent_name, 1)) then
            constituent_name = ''

            return
        end if

        constituent_name = trim(adjustl(self % constituent_name(constituent_index)))
    end function dyn_mpas_get_constituent_name

    !-------------------------------------------------------------------------------
    ! function dyn_mpas_get_constituent_index
    !
    !> summary: Query constituent index by its name.
    !> author: Kuan-Chih Wang
    !> date: 2024-05-16
    !>
    !> This function returns the constituent index that corresponds to the given
    !> constituent name. In case of errors, zero is produced.
    !
    !-------------------------------------------------------------------------------
    pure function dyn_mpas_get_constituent_index(self, constituent_name) result(constituent_index)
        class(mpas_dynamical_core_type), intent(in) :: self
        character(*), intent(in) :: constituent_name

        integer :: i
        integer :: constituent_index

        ! Catch segmentation fault.
        if (.not. allocated(self % constituent_name)) then
            constituent_index = 0

            return
        end if

        do i = 1, self % number_of_constituents
            if (trim(adjustl(constituent_name)) == trim(adjustl(self % constituent_name(i)))) then
                constituent_index = i

                return
            end if
        end do

        constituent_index = 0
    end function dyn_mpas_get_constituent_index

    !-------------------------------------------------------------------------------
    ! function dyn_mpas_map_mpas_scalar_index
    !
    !> summary: Map MPAS scalar index from constituent index.
    !> author: Kuan-Chih Wang
    !> date: 2024-05-16
    !>
    !> This function returns the MPAS scalar index that corresponds to the given
    !> constituent index. In case of errors, zero is produced.
    !
    !-------------------------------------------------------------------------------
    pure function dyn_mpas_map_mpas_scalar_index(self, constituent_index) result(mpas_scalar_index)
        class(mpas_dynamical_core_type), intent(in) :: self
        integer, intent(in) :: constituent_index

        integer :: mpas_scalar_index

        ! Catch segmentation fault.
        if (.not. allocated(self % index_constituent_to_mpas_scalar)) then
            mpas_scalar_index = 0

            return
        end if

        if (constituent_index < lbound(self % index_constituent_to_mpas_scalar, 1) .or. &
            constituent_index > ubound(self % index_constituent_to_mpas_scalar, 1)) then
            mpas_scalar_index = 0

            return
        end if

        mpas_scalar_index = self % index_constituent_to_mpas_scalar(constituent_index)
    end function dyn_mpas_map_mpas_scalar_index

    !-------------------------------------------------------------------------------
    ! function dyn_mpas_map_constituent_index
    !
    !> summary: Map constituent index from MPAS scalar index.
    !> author: Kuan-Chih Wang
    !> date: 2024-05-16
    !>
    !> This function returns the constituent index that corresponds to the given
    !> MPAS scalar index. In case of errors, zero is produced.
    !
    !-------------------------------------------------------------------------------
    pure function dyn_mpas_map_constituent_index(self, mpas_scalar_index) result(constituent_index)
        class(mpas_dynamical_core_type), intent(in) :: self
        integer, intent(in) :: mpas_scalar_index

        integer :: constituent_index

        ! Catch segmentation fault.
        if (.not. allocated(self % index_mpas_scalar_to_constituent)) then
            constituent_index = 0

            return
        end if

        if (mpas_scalar_index < lbound(self % index_mpas_scalar_to_constituent, 1) .or. &
            mpas_scalar_index > ubound(self % index_mpas_scalar_to_constituent, 1)) then
            constituent_index = 0

            return
        end if

        constituent_index = self % index_mpas_scalar_to_constituent(mpas_scalar_index)
    end function dyn_mpas_map_constituent_index

    !-------------------------------------------------------------------------------
    ! subroutine dyn_mpas_get_local_mesh_dimension
    !
    !> summary: Return local mesh dimensions.
    !> author: Kuan-Chih Wang
    !> date: 2024-05-09
    !>
    !> This subroutine returns local mesh dimensions, including:
    !> * Numbers of local mesh cells, edges, vertices, and vertical levels
    !>   on each individual task, both with/without halo points.
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_get_local_mesh_dimension(self, &
            ncells, ncells_solve, nedges, nedges_solve, nvertices, nvertices_solve, nvertlevels)
        class(mpas_dynamical_core_type), intent(in) :: self
        integer, intent(out) :: ncells, ncells_solve, nedges, nedges_solve, nvertices, nvertices_solve, nvertlevels

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_local_mesh_dimension'
        integer, pointer :: ncells_pointer
        integer, pointer :: ncellssolve_pointer
        integer, pointer :: nedges_pointer
        integer, pointer :: nedgessolve_pointer
        integer, pointer :: nvertices_pointer
        integer, pointer :: nverticessolve_pointer
        integer, pointer :: nvertlevels_pointer

        call self % debug_print(log_level_debug, subname // ' entered')

        nullify(ncells_pointer)
        nullify(ncellssolve_pointer)
        nullify(nedges_pointer)
        nullify(nedgessolve_pointer)
        nullify(nvertices_pointer)
        nullify(nverticessolve_pointer)
        nullify(nvertlevels_pointer)

        call self % debug_print(log_level_info, 'Inquiring local mesh dimensions')

        call self % get_variable_pointer(ncells_pointer, 'dim', 'nCells')
        call self % get_variable_pointer(ncellssolve_pointer, 'dim', 'nCellsSolve')
        call self % get_variable_pointer(nedges_pointer, 'dim', 'nEdges')
        call self % get_variable_pointer(nedgessolve_pointer, 'dim', 'nEdgesSolve')
        call self % get_variable_pointer(nvertices_pointer, 'dim', 'nVertices')
        call self % get_variable_pointer(nverticessolve_pointer, 'dim', 'nVerticesSolve')
        call self % get_variable_pointer(nvertlevels_pointer, 'dim', 'nVertLevels')

        ncells = ncells_pointer                  ! Number of cells, including halo cells.
        ncells_solve = ncellssolve_pointer       ! Number of cells, excluding halo cells.
        nedges = nedges_pointer                  ! Number of edges, including halo edges.
        nedges_solve = nedgessolve_pointer       ! Number of edges, excluding halo edges.
        nvertices = nvertices_pointer            ! Number of vertices, including halo vertices.
        nvertices_solve = nverticessolve_pointer ! Number of vertices, excluding halo vertices.

        ! Vertical levels are not decomposed.
        ! All tasks have the same number of vertical levels.
        nvertlevels = nvertlevels_pointer

        nullify(ncells_pointer)
        nullify(ncellssolve_pointer)
        nullify(nedges_pointer)
        nullify(nedgessolve_pointer)
        nullify(nvertices_pointer)
        nullify(nverticessolve_pointer)
        nullify(nvertlevels_pointer)

        call self % debug_print(log_level_debug, subname // ' completed')
    end subroutine dyn_mpas_get_local_mesh_dimension

    !-------------------------------------------------------------------------------
    ! subroutine dyn_mpas_get_global_mesh_dimension
    !
    !> summary: Return global mesh dimensions.
    !> author: Michael Duda
    !> date: 22 August 2019
    !>
    !> This subroutine returns global mesh dimensions, including:
    !> * Numbers of global mesh cells, edges, vertices, and vertical levels
    !>   across all tasks.
    !> * Maximum numbers of mesh cells and edges/vertices among all tasks.
    !> * Sphere radius.
    !> Ported and refactored for CAM-SIMA. (KCW, 2024-03-25)
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_get_global_mesh_dimension(self, &
            ncells_global, nedges_global, nvertices_global, nvertlevels, ncells_max, nedges_max, &
            sphere_radius)
        ! Module(s) from MPAS.
        use mpas_dmpar, only: mpas_dmpar_max_int, mpas_dmpar_sum_int

        class(mpas_dynamical_core_type), intent(in) :: self
        integer, intent(out) :: ncells_global, nedges_global, nvertices_global, nvertlevels, ncells_max, nedges_max
        real(rkind), intent(out) :: sphere_radius

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_global_mesh_dimension'
        integer, pointer :: maxedges_pointer
        integer, pointer :: ncellssolve_pointer
        integer, pointer :: nedgessolve_pointer
        integer, pointer :: nverticessolve_pointer
        integer, pointer :: nvertlevels_pointer

        call self % debug_print(log_level_debug, subname // ' entered')

        nullify(maxedges_pointer)
        nullify(ncellssolve_pointer)
        nullify(nedgessolve_pointer)
        nullify(nverticessolve_pointer)
        nullify(nvertlevels_pointer)

        call self % debug_print(log_level_info, 'Inquiring global mesh dimensions')

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

        call self % debug_print(log_level_debug, subname // ' completed')
    end subroutine dyn_mpas_get_global_mesh_dimension

    !-------------------------------------------------------------------------------
    ! subroutine dyn_mpas_get_pool_pointer
    !
    !> summary: Return a pointer of `mpas_pool_type` to the named pool.
    !> author: Kuan-Chih Wang
    !> date: 2024-03-21
    !>
    !> This subroutine returns a pointer of `mpas_pool_type` to the named pool.
    !> Supported pool names include: "all", "cfg", "dim", and a subset of the
    !> `var_struct` elements in MPAS registry.
    !> It is mostly used by the `dyn_mpas_get_variable_{pointer,value}_*`
    !> subroutines to draw a variable from a pool.
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_get_pool_pointer(self, pool_pointer, pool_name)
        ! Module(s) from MPAS.
        use mpas_derived_types, only: mpas_pool_type
        use mpas_pool_routines, only: mpas_pool_get_subpool

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
            case ('diag', 'mesh', 'state', 'tend', 'tend_physics')
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
    !> summary: A family of accessor subroutines for MPAS dynamical core instance.
    !> author: Kuan-Chih Wang
    !> date: 2024-03-21
    !>
    !> The `dyn_mpas_get_variable_pointer_*` subroutines are a family of accessor
    !> subroutines for drawing the REFERENCE of an internal variable from
    !> MPAS dynamical core instance. The `get_variable_pointer` generic interface
    !> should be used instead of the specific ones.
    !> WARNING:
    !> USE OF THIS SUBROUTINE FAMILY IS HIGHLY DISCOURAGED BECAUSE THE INTERNAL
    !> STATES OF MPAS DYNAMICAL CORE INSTANCE COULD BE MODIFIED THROUGH THE
    !> RETURNED POINTER. THESE ARE UNCHARTED WATERS SO BE SURE WHAT YOU ARE
    !> DOING.
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_get_variable_pointer_c0(self, variable_pointer, pool_name, variable_name, time_level)
        ! Module(s) from MPAS.
        use mpas_derived_types, only: mpas_pool_type
        use mpas_pool_routines, only: mpas_pool_get_array, mpas_pool_get_config

        class(mpas_dynamical_core_type), intent(in) :: self
        character(strkind), pointer, intent(out) :: variable_pointer
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_pointer_c0'
        type(mpas_pool_type), pointer :: mpas_pool

        nullify(mpas_pool)
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
        ! Module(s) from MPAS.
        use mpas_derived_types, only: mpas_pool_type
        use mpas_pool_routines, only: mpas_pool_get_array

        class(mpas_dynamical_core_type), intent(in) :: self
        character(strkind), pointer, intent(out) :: variable_pointer(:)
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_pointer_c1'
        type(mpas_pool_type), pointer :: mpas_pool

        nullify(mpas_pool)
        call self % get_pool_pointer(mpas_pool, pool_name)
        nullify(variable_pointer)
        call mpas_pool_get_array(mpas_pool, trim(adjustl(variable_name)), variable_pointer, timelevel=time_level)

        if (.not. associated(variable_pointer)) then
            call self % model_error('Failed to find variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(mpas_pool)
    end subroutine dyn_mpas_get_variable_pointer_c1

    subroutine dyn_mpas_get_variable_pointer_i0(self, variable_pointer, pool_name, variable_name, time_level)
        ! Module(s) from MPAS.
        use mpas_derived_types, only: mpas_pool_type
        use mpas_pool_routines, only: mpas_pool_get_array, mpas_pool_get_config, mpas_pool_get_dimension

        class(mpas_dynamical_core_type), intent(in) :: self
        integer, pointer, intent(out) :: variable_pointer
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_pointer_i0'
        type(mpas_pool_type), pointer :: mpas_pool

        nullify(mpas_pool)
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
        ! Module(s) from MPAS.
        use mpas_derived_types, only: mpas_pool_type
        use mpas_pool_routines, only: mpas_pool_get_array, mpas_pool_get_dimension

        class(mpas_dynamical_core_type), intent(in) :: self
        integer, pointer, intent(out) :: variable_pointer(:)
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_pointer_i1'
        type(mpas_pool_type), pointer :: mpas_pool

        nullify(mpas_pool)
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
        ! Module(s) from MPAS.
        use mpas_derived_types, only: mpas_pool_type
        use mpas_pool_routines, only: mpas_pool_get_array

        class(mpas_dynamical_core_type), intent(in) :: self
        integer, pointer, intent(out) :: variable_pointer(:, :)
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_pointer_i2'
        type(mpas_pool_type), pointer :: mpas_pool

        nullify(mpas_pool)
        call self % get_pool_pointer(mpas_pool, pool_name)
        nullify(variable_pointer)
        call mpas_pool_get_array(mpas_pool, trim(adjustl(variable_name)), variable_pointer, timelevel=time_level)

        if (.not. associated(variable_pointer)) then
            call self % model_error('Failed to find variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(mpas_pool)
    end subroutine dyn_mpas_get_variable_pointer_i2

    subroutine dyn_mpas_get_variable_pointer_i3(self, variable_pointer, pool_name, variable_name, time_level)
        ! Module(s) from MPAS.
        use mpas_derived_types, only: mpas_pool_type
        use mpas_pool_routines, only: mpas_pool_get_array

        class(mpas_dynamical_core_type), intent(in) :: self
        integer, pointer, intent(out) :: variable_pointer(:, :, :)
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_pointer_i3'
        type(mpas_pool_type), pointer :: mpas_pool

        nullify(mpas_pool)
        call self % get_pool_pointer(mpas_pool, pool_name)
        nullify(variable_pointer)
        call mpas_pool_get_array(mpas_pool, trim(adjustl(variable_name)), variable_pointer, timelevel=time_level)

        if (.not. associated(variable_pointer)) then
            call self % model_error('Failed to find variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(mpas_pool)
    end subroutine dyn_mpas_get_variable_pointer_i3

    subroutine dyn_mpas_get_variable_pointer_l0(self, variable_pointer, pool_name, variable_name, time_level)
        ! Module(s) from MPAS.
        use mpas_derived_types, only: mpas_pool_type
        use mpas_pool_routines, only: mpas_pool_get_config

        class(mpas_dynamical_core_type), intent(in) :: self
        logical, pointer, intent(out) :: variable_pointer
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_pointer_l0'
        type(mpas_pool_type), pointer :: mpas_pool

        nullify(mpas_pool)
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
        ! Module(s) from MPAS.
        use mpas_derived_types, only: mpas_pool_type
        use mpas_pool_routines, only: mpas_pool_get_array, mpas_pool_get_config

        class(mpas_dynamical_core_type), intent(in) :: self
        real(rkind), pointer, intent(out) :: variable_pointer
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_pointer_r0'
        type(mpas_pool_type), pointer :: mpas_pool

        nullify(mpas_pool)
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
        ! Module(s) from MPAS.
        use mpas_derived_types, only: mpas_pool_type
        use mpas_pool_routines, only: mpas_pool_get_array

        class(mpas_dynamical_core_type), intent(in) :: self
        real(rkind), pointer, intent(out) :: variable_pointer(:)
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_pointer_r1'
        type(mpas_pool_type), pointer :: mpas_pool

        nullify(mpas_pool)
        call self % get_pool_pointer(mpas_pool, pool_name)
        nullify(variable_pointer)
        call mpas_pool_get_array(mpas_pool, trim(adjustl(variable_name)), variable_pointer, timelevel=time_level)

        if (.not. associated(variable_pointer)) then
            call self % model_error('Failed to find variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(mpas_pool)
    end subroutine dyn_mpas_get_variable_pointer_r1

    subroutine dyn_mpas_get_variable_pointer_r2(self, variable_pointer, pool_name, variable_name, time_level)
        ! Module(s) from MPAS.
        use mpas_derived_types, only: mpas_pool_type
        use mpas_pool_routines, only: mpas_pool_get_array

        class(mpas_dynamical_core_type), intent(in) :: self
        real(rkind), pointer, intent(out) :: variable_pointer(:, :)
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_pointer_r2'
        type(mpas_pool_type), pointer :: mpas_pool

        nullify(mpas_pool)
        call self % get_pool_pointer(mpas_pool, pool_name)
        nullify(variable_pointer)
        call mpas_pool_get_array(mpas_pool, trim(adjustl(variable_name)), variable_pointer, timelevel=time_level)

        if (.not. associated(variable_pointer)) then
            call self % model_error('Failed to find variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(mpas_pool)
    end subroutine dyn_mpas_get_variable_pointer_r2

    subroutine dyn_mpas_get_variable_pointer_r3(self, variable_pointer, pool_name, variable_name, time_level)
        ! Module(s) from MPAS.
        use mpas_derived_types, only: mpas_pool_type
        use mpas_pool_routines, only: mpas_pool_get_array

        class(mpas_dynamical_core_type), intent(in) :: self
        real(rkind), pointer, intent(out) :: variable_pointer(:, :, :)
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_pointer_r3'
        type(mpas_pool_type), pointer :: mpas_pool

        nullify(mpas_pool)
        call self % get_pool_pointer(mpas_pool, pool_name)
        nullify(variable_pointer)
        call mpas_pool_get_array(mpas_pool, trim(adjustl(variable_name)), variable_pointer, timelevel=time_level)

        if (.not. associated(variable_pointer)) then
            call self % model_error('Failed to find variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(mpas_pool)
    end subroutine dyn_mpas_get_variable_pointer_r3

    subroutine dyn_mpas_get_variable_pointer_r4(self, variable_pointer, pool_name, variable_name, time_level)
        ! Module(s) from MPAS.
        use mpas_derived_types, only: mpas_pool_type
        use mpas_pool_routines, only: mpas_pool_get_array

        class(mpas_dynamical_core_type), intent(in) :: self
        real(rkind), pointer, intent(out) :: variable_pointer(:, :, :, :)
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_pointer_r4'
        type(mpas_pool_type), pointer :: mpas_pool

        nullify(mpas_pool)
        call self % get_pool_pointer(mpas_pool, pool_name)
        nullify(variable_pointer)
        call mpas_pool_get_array(mpas_pool, trim(adjustl(variable_name)), variable_pointer, timelevel=time_level)

        if (.not. associated(variable_pointer)) then
            call self % model_error('Failed to find variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(mpas_pool)
    end subroutine dyn_mpas_get_variable_pointer_r4

    subroutine dyn_mpas_get_variable_pointer_r5(self, variable_pointer, pool_name, variable_name, time_level)
        ! Module(s) from MPAS.
        use mpas_derived_types, only: mpas_pool_type
        use mpas_pool_routines, only: mpas_pool_get_array

        class(mpas_dynamical_core_type), intent(in) :: self
        real(rkind), pointer, intent(out) :: variable_pointer(:, :, :, :, :)
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_pointer_r5'
        type(mpas_pool_type), pointer :: mpas_pool

        nullify(mpas_pool)
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
    !> summary: A family of accessor subroutines for MPAS dynamical core instance.
    !> author: Kuan-Chih Wang
    !> date: 2024-03-21
    !>
    !> The `dyn_mpas_get_variable_value_*` subroutines are a family of accessor
    !> subroutines for drawing the VALUE of an internal variable from
    !> MPAS dynamical core instance. The `get_variable_value` generic interface
    !> should be used instead of the specific ones.
    !
    !-------------------------------------------------------------------------------
    subroutine dyn_mpas_get_variable_value_c0(self, variable_value, pool_name, variable_name, time_level)
        class(mpas_dynamical_core_type), intent(in) :: self
        character(strkind), allocatable, intent(out) :: variable_value
        character(*), intent(in) :: pool_name
        character(*), intent(in) :: variable_name
        integer, optional, intent(in) :: time_level

        character(*), parameter :: subname = 'dyn_mpas_subdriver::dyn_mpas_get_variable_value_c0'
        character(strkind), pointer :: variable_pointer
        integer :: ierr

        nullify(variable_pointer)
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
        character(strkind), pointer :: variable_pointer(:)
        integer :: ierr

        nullify(variable_pointer)
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
        integer, pointer :: variable_pointer
        integer :: ierr

        nullify(variable_pointer)
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
        integer, pointer :: variable_pointer(:)
        integer :: ierr

        nullify(variable_pointer)
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
        integer, pointer :: variable_pointer(:, :)
        integer :: ierr

        nullify(variable_pointer)
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
        integer, pointer :: variable_pointer(:, :, :)
        integer :: ierr

        nullify(variable_pointer)
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
        logical, pointer :: variable_pointer
        integer :: ierr

        nullify(variable_pointer)
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
        real(rkind), pointer :: variable_pointer
        integer :: ierr

        nullify(variable_pointer)
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
        real(rkind), pointer :: variable_pointer(:)
        integer :: ierr

        nullify(variable_pointer)
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
        real(rkind), pointer :: variable_pointer(:, :)
        integer :: ierr

        nullify(variable_pointer)
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
        real(rkind), pointer :: variable_pointer(:, :, :)
        integer :: ierr

        nullify(variable_pointer)
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
        real(rkind), pointer :: variable_pointer(:, :, :, :)
        integer :: ierr

        nullify(variable_pointer)
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
        real(rkind), pointer :: variable_pointer(:, :, :, :, :)
        integer :: ierr

        nullify(variable_pointer)
        call self % get_variable_pointer(variable_pointer, pool_name, variable_name, time_level=time_level)
        allocate(variable_value, source=variable_pointer, stat=ierr)

        if (ierr /= 0) then
            call self % model_error('Failed to allocate variable "' // trim(adjustl(variable_name)) // '"', subname, __LINE__)
        end if

        nullify(variable_pointer)
    end subroutine dyn_mpas_get_variable_value_r5
end module dyn_mpas_subdriver
