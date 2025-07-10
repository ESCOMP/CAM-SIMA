! Copyright (C) 2025 University Corporation for Atmospheric Research (UCAR)
! SPDX-License-Identifier: Apache-2.0

!> This module, part of the MPAS interface, integrates MPAS dynamical core with CAM-SIMA by
!> implementing the necessary APIs and managing their interaction.
!>
!> It implements the bidirectional coupling between dynamics and physics states.
module dyn_coupling
    implicit none

    private
    ! Provide APIs required by CAM-SIMA.
    public :: dynamics_to_physics_coupling
    public :: physics_to_dynamics_coupling
contains
    !> Perform one-way coupling from the dynamics output states to the physics input states.
    !> The other coupling direction is implemented by its counterpart, `physics_to_dynamics_coupling`.
    !> (KCW, 2024-07-31)
    subroutine dynamics_to_physics_coupling()
        ! Module(s) from CAM-SIMA.
        use cam_logfile, only: debugout_debug, debugout_info
        use dyn_comp, only: dyn_debug_print, dyn_exchange_constituent_states, ncells_solve
        ! Module(s) from CESM Share.
        use shr_kind_mod, only: kind_r8 => shr_kind_r8
        ! Module(s) from MPAS.
        use dyn_mpas_subdriver, only: kind_dyn_mpas => mpas_dynamical_core_real_kind

        character(*), parameter :: subname = 'dyn_coupling::dynamics_to_physics_coupling'
        integer :: column_index
        integer, allocatable :: is_water_species_index(:)
        integer, pointer :: index_qv
        ! Variable name suffixes have the following meanings:
        ! `*_col`: Variable is of each column.
        ! `*_int`: Variable is at layer interfaces.
        ! `*_mid`: Variable is at layer midpoints.
        real(kind_r8), allocatable :: pd_int_col(:), &       ! Dry hydrostatic air pressure (Pa).
                                      pd_mid_col(:), &       ! Dry non-hydrostatic air pressure (Pa).
                                      p_int_col(:), &        ! Full hydrostatic air pressure (Pa).
                                      p_mid_col(:), &        ! Full non-hydrostatic air pressure (Pa).
                                      z_int_col(:)           ! Geometric height (m).
        real(kind_r8), allocatable :: dpd_col(:), &          ! Dry air pressure difference (Pa) between layer interfaces.
                                      dp_col(:), &           ! Full air pressure difference (Pa) between layer interfaces.
                                      dz_col(:)              ! Geometric height difference (m) between layer interfaces.
        real(kind_r8), allocatable :: qv_mid_col(:), &       ! Water vapor mixing ratio (kg kg-1).
                                      sigma_all_q_mid_col(:) ! Summation of all water species mixing ratios (kg kg-1).
        real(kind_r8), allocatable :: rhod_mid_col(:), &     ! Dry air density (kg m-3).
                                      rho_mid_col(:)         ! Full air density (kg m-3).
        real(kind_r8), allocatable :: t_mid_col(:), &        ! Temperature (K).
                                      tm_mid_col(:), &       ! Modified "moist" temperature (K).
                                      tv_mid_col(:)          ! Virtual temperature (K).
        real(kind_r8), allocatable :: u_mid_col(:), &        ! Eastward wind velocity (m s-1).
                                      v_mid_col(:), &        ! Northward wind velocity (m s-1).
                                      omega_mid_col(:)       ! Vertical wind velocity (Pa s-1).
        real(kind_dyn_mpas), pointer :: exner(:, :)
        real(kind_dyn_mpas), pointer :: rho_zz(:, :)
        real(kind_dyn_mpas), pointer :: scalars(:, :, :)
        real(kind_dyn_mpas), pointer :: theta_m(:, :)
        real(kind_dyn_mpas), pointer :: ucellzonal(:, :), ucellmeridional(:, :), w(:, :)
        real(kind_dyn_mpas), pointer :: zgrid(:, :)
        real(kind_dyn_mpas), pointer :: zz(:, :)

        call dyn_debug_print(debugout_debug, subname // ' entered')

        call init_shared_variables()

        call dyn_exchange_constituent_states(direction='i', exchange=.true., conversion=.false.)

        call dyn_debug_print(debugout_info, 'Setting physics state variables column by column')

        ! Set variables in the `physics_state` derived type column by column.
        ! This way, peak memory usage can be reduced.
        do column_index = 1, ncells_solve
            call update_shared_variables(column_index)
            call set_physics_state_column(column_index)
        end do

        call set_physics_state_external()

        call final_shared_variables()

        call dyn_debug_print(debugout_debug, subname // ' completed')
    contains
        !> Initialize variables that are shared and repeatedly used by the `update_shared_variables` and
        !> `set_physics_state_column` internal subroutines.
        !> (KCW, 2024-07-20)
        subroutine init_shared_variables()
            ! Module(s) from CAM-SIMA.
            use cam_abortutils, only: check_allocate
            use cam_constituents, only: const_is_water_species, num_advected
            use dyn_comp, only: mpas_dynamical_core
            use vert_coord, only: pver, pverp

            character(*), parameter :: subname = 'dyn_coupling::dynamics_to_physics_coupling::init_shared_variables'
            integer :: i
            integer :: ierr
            logical, allocatable :: is_water_species(:)

            call dyn_debug_print(debugout_info, 'Preparing for dynamics-physics coupling')

            nullify(index_qv)
            nullify(exner)
            nullify(rho_zz)
            nullify(scalars)
            nullify(theta_m)
            nullify(ucellzonal, ucellmeridional, w)
            nullify(zgrid)
            nullify(zz)

            allocate(is_water_species(num_advected), stat=ierr)
            call check_allocate(ierr, subname, &
                'is_water_species(num_advected)', &
                'dyn_coupling', __LINE__)

            do i = 1, num_advected
                is_water_species(i) = const_is_water_species(i)
            end do

            allocate(is_water_species_index(count(is_water_species)), stat=ierr)
            call check_allocate(ierr, subname, &
                'is_water_species_index(count(is_water_species))', &
                'dyn_coupling', __LINE__)

            is_water_species_index(:) = &
                pack([(mpas_dynamical_core % map_mpas_scalar_index(i), i = 1, num_advected)], is_water_species)

            deallocate(is_water_species)

            allocate(pd_int_col(pverp), pd_mid_col(pver), p_int_col(pverp), p_mid_col(pver), z_int_col(pverp), stat=ierr)
            call check_allocate(ierr, subname, &
                'pd_int_col(pverp), pd_mid_col(pver), p_int_col(pverp), p_mid_col(pver), z_int_col(pverp)', &
                'dyn_coupling', __LINE__)

            allocate(dpd_col(pver), dp_col(pver), dz_col(pver), stat=ierr)
            call check_allocate(ierr, subname, &
                'dpd_col(pver), dp_col(pver), dz_col(pver)', &
                'dyn_coupling', __LINE__)

            allocate(qv_mid_col(pver), sigma_all_q_mid_col(pver), stat=ierr)
            call check_allocate(ierr, subname, &
                'qv_mid_col(pver), sigma_all_q_mid_col(pver)', &
                'dyn_coupling', __LINE__)

            allocate(rhod_mid_col(pver), rho_mid_col(pver), stat=ierr)
            call check_allocate(ierr, subname, &
                'rhod_mid_col(pver), rho_mid_col(pver)', &
                'dyn_coupling', __LINE__)

            allocate(t_mid_col(pver), tm_mid_col(pver), tv_mid_col(pver), stat=ierr)
            call check_allocate(ierr, subname, &
                't_mid_col(pver), tm_mid_col(pver), tv_mid_col(pver)', &
                'dyn_coupling', __LINE__)

            allocate(u_mid_col(pver), v_mid_col(pver), omega_mid_col(pver), stat=ierr)
            call check_allocate(ierr, subname, &
                'u_mid_col(pver), v_mid_col(pver), omega_mid_col(pver)', &
                'dyn_coupling', __LINE__)

            call mpas_dynamical_core % get_variable_pointer(index_qv, 'dim', 'index_qv')
            call mpas_dynamical_core % get_variable_pointer(exner, 'diag', 'exner')
            call mpas_dynamical_core % get_variable_pointer(rho_zz, 'state', 'rho_zz', time_level=1)
            call mpas_dynamical_core % get_variable_pointer(scalars, 'state', 'scalars', time_level=1)
            call mpas_dynamical_core % get_variable_pointer(theta_m, 'state', 'theta_m', time_level=1)
            call mpas_dynamical_core % get_variable_pointer(ucellzonal, 'diag', 'uReconstructZonal')
            call mpas_dynamical_core % get_variable_pointer(ucellmeridional, 'diag', 'uReconstructMeridional')
            call mpas_dynamical_core % get_variable_pointer(w, 'state', 'w', time_level=1)
            call mpas_dynamical_core % get_variable_pointer(zgrid, 'mesh', 'zgrid')
            call mpas_dynamical_core % get_variable_pointer(zz, 'mesh', 'zz')
        end subroutine init_shared_variables

        !> Finalize variables that are shared and repeatedly used by the `update_shared_variables` and
        !> `set_physics_state_column` internal subroutines.
        !> (KCW, 2024-07-20)
        subroutine final_shared_variables()
            character(*), parameter :: subname = 'dyn_coupling::dynamics_to_physics_coupling::final_shared_variables'

            deallocate(is_water_species_index)
            deallocate(pd_int_col, pd_mid_col, p_int_col, p_mid_col, z_int_col)
            deallocate(dpd_col, dp_col, dz_col)
            deallocate(qv_mid_col, sigma_all_q_mid_col)
            deallocate(rhod_mid_col, rho_mid_col)
            deallocate(t_mid_col, tm_mid_col, tv_mid_col)
            deallocate(u_mid_col, v_mid_col, omega_mid_col)

            nullify(index_qv)
            nullify(exner)
            nullify(rho_zz)
            nullify(scalars)
            nullify(theta_m)
            nullify(ucellzonal, ucellmeridional, w)
            nullify(zgrid)
            nullify(zz)
        end subroutine final_shared_variables

        !> Update variables for the specific column, indicated by `i`. This subroutine and `set_physics_state_column`
        !> should be called in pairs.
        !> (KCW, 2024-07-30)
        subroutine update_shared_variables(i)
            ! Module(s) from CAM-SIMA.
            use dynconst, only: constant_g => gravit, constant_rd => rair, constant_rv => rh2o
            use vert_coord, only: pver, pverp

            integer, intent(in) :: i

            character(*), parameter :: subname = 'dyn_coupling::dynamics_to_physics_coupling::update_shared_variables'
            integer :: k
            ! Proximity limit, in fraction, on how close `p{,d}_mid_col` is allowed to be around its surrounding `p{,d}_int_col`.
            real(kind_r8), parameter :: p_int_mid_proximity_limit = 0.05_kind_r8

            ! The summation term of equation 5 in doi:10.1029/2017MS001257.
            sigma_all_q_mid_col(:) = 1.0_kind_r8 + sum(scalars(is_water_species_index, :, i), 1)

            ! Compute thermodynamic variables.

            ! By definition.
            z_int_col(:) = real(zgrid(:, i), kind_r8)
            dz_col(:) = z_int_col(2:pverp) - z_int_col(1:pver)
            qv_mid_col(:) = real(scalars(index_qv, :, i), kind_r8)
            rhod_mid_col(:) = real(rho_zz(:, i) * zz(:, i), kind_r8)

            ! Equation 5 in doi:10.1029/2017MS001257.
            rho_mid_col(:) = rhod_mid_col(:) * sigma_all_q_mid_col(:)

            ! Hydrostatic equation.
            dpd_col(:) = -rhod_mid_col(:) * constant_g * dz_col(:)
            dp_col(:) = -rho_mid_col(:) * constant_g * dz_col(:)

            ! By definition of Exner function. Also see below.
            tm_mid_col(:) = real(theta_m(:, i) * exner(:, i), kind_r8)

            ! The paragraph below equation 2.7 in doi:10.5065/1DFH-6P97.
            ! The paragraph below equation 2 in doi:10.1175/MWR-D-11-00215.1.
            t_mid_col(:) = tm_mid_col(:) / &
                (1.0_kind_r8 + constant_rv / constant_rd * qv_mid_col(:))

            ! Equation 16 in doi:10.1029/2017MS001257.
            ! The numerator terms are just `tm_mid_col` here (i.e., modified "moist" temperature).
            tv_mid_col(:) = tm_mid_col(:) / sigma_all_q_mid_col(:)

            ! Equation of state.
            pd_mid_col(:) = rhod_mid_col(:) * constant_rd * t_mid_col(:)
            p_mid_col(:) = rho_mid_col(:) * constant_rd * tv_mid_col(:)

            ! By definition.
            pd_int_col(pverp) = pd_mid_col(pver) + 0.5_kind_r8 * dpd_col(pver)
            p_int_col(pverp) = p_mid_col(pver) + 0.5_kind_r8 * dp_col(pver)

            ! Integrate downward.
            do k = pver, 1, -1
                pd_int_col(k) = pd_int_col(k + 1) - dpd_col(k)
                p_int_col(k) = p_int_col(k + 1) - dp_col(k)
            end do

            ! `p{,d}_mid_col` is not guaranteed to be bounded by `p{,d}_int_col` because the former is non-hydrostatic
            ! while the latter is hydrostatic. In high-resolution simulations, the former could exceed the latter,
            ! leading to a model crash in physics.
            ! Impose range limits on `p{,d}_mid_col` so it is bounded by `p{,d}_int_col`.
            pd_mid_col(:) = &
                max(min(pd_mid_col, &
                pd_int_col(1:pver) + dpd_col(:) * p_int_mid_proximity_limit), &
                pd_int_col(2:pverp) - dpd_col(:) * p_int_mid_proximity_limit)
            p_mid_col(:) = &
                max(min(p_mid_col, &
                p_int_col(1:pver) + dp_col(:) * p_int_mid_proximity_limit), &
                p_int_col(2:pverp) - dp_col(:) * p_int_mid_proximity_limit)

            ! Compute momentum variables.

            ! By definition.
            u_mid_col(:) = real(ucellzonal(:, i), kind_r8)
            v_mid_col(:) = real(ucellmeridional(:, i), kind_r8)
            omega_mid_col(:) = -rhod_mid_col(:) * constant_g * 0.5_kind_r8 * real(w(1:pver, i) + w(2:pverp, i), kind_r8)
        end subroutine update_shared_variables

        !> Set variables for the specific column, indicated by `i`, in the `physics_state` derived type.
        !> This subroutine and `update_shared_variables` should be called in pairs.
        !> (KCW, 2024-07-30)
        subroutine set_physics_state_column(i)
            ! Module(s) from CAM-SIMA.
            use dyn_comp, only: reverse
            use dynconst, only: constant_g => gravit
            use physics_types, only: phys_state

            integer, intent(in) :: i

            character(*), parameter :: subname = 'dyn_coupling::dynamics_to_physics_coupling::set_physics_state_column'

            ! Vertical index order is reversed between CAM-SIMA and MPAS.
            ! Always call `reverse` when assigning anything to/from the `physics_state` derived type.

            phys_state % u(i, :) = reverse(u_mid_col)
            phys_state % v(i, :) = reverse(v_mid_col)
            phys_state % omega(i, :) = reverse(omega_mid_col)

            phys_state % psdry(i) = pd_int_col(1)
            phys_state % pintdry(i, :) = reverse(pd_int_col)
            phys_state % pmiddry(i, :) = reverse(pd_mid_col)
            phys_state % pdeldry(i, :) = reverse(-dpd_col)
            phys_state % lnpintdry(i, :) = log(phys_state % pintdry(i, :))
            phys_state % lnpmiddry(i, :) = log(phys_state % pmiddry(i, :))
            phys_state % rpdeldry(i, :) = 1.0_kind_r8 / phys_state % pdeldry(i, :)

            phys_state % ps(i) = p_int_col(1)
            phys_state % pint(i, :) = reverse(p_int_col)
            phys_state % pmid(i, :) = reverse(p_mid_col)
            phys_state % pdel(i, :) = reverse(-dp_col)
            phys_state % lnpint(i, :) = log(phys_state % pint(i, :))
            phys_state % lnpmid(i, :) = log(phys_state % pmid(i, :))
            phys_state % rpdel(i, :) = 1.0_kind_r8 / phys_state % pdel(i, :)

            phys_state % t(i, :) = reverse(t_mid_col)

            phys_state % phis(i) = constant_g * z_int_col(1)
        end subroutine set_physics_state_column

        !> Set variables in the `physics_state` derived type by calling external procedures.
        !> (KCW, 2024-07-30)
        subroutine set_physics_state_external()
            ! Module(s) from CAM-SIMA.
            use cam_abortutils, only: check_allocate, endrun
            use cam_constituents, only: const_qmin, num_advected
            use cam_thermo, only: cam_thermo_dry_air_update, cam_thermo_water_update
            use cam_thermo_formula, only: energy_formula_dycore_mpas
            use dyn_comp, only: mpas_dynamical_core
            use dynconst, only: constant_g => gravit
            use physics_types, only: cappav, cp_or_cv_dycore, cpairv, lagrangian_vertical, phys_state, rairv, zvirv
            use runtime_obj, only: cam_runtime_opts
            use string_utils, only: stringify
            use vert_coord, only: pver, pverp
            ! Module(s) from CCPP.
            use cam_ccpp_cap, only: cam_constituents_array, cam_model_const_properties
            use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t
            use ccpp_kinds, only: kind_phys
            use geopotential_temp, only: geopotential_temp_run
            use qneg, only: qneg_run
            use static_energy, only: update_dry_static_energy_run
            ! Module(s) from CESM Share.
            use shr_kind_mod, only: len_cx => shr_kind_cx

            character(*), parameter :: subname = 'dyn_coupling::dynamics_to_physics_coupling::set_physics_state_external'
            character(len_cx) :: cerr
            integer :: i
            integer :: ierr
            real(kind_phys), allocatable :: minimum_constituents(:)
            real(kind_phys), pointer :: constituents(:, :, :)
            type(ccpp_constituent_prop_ptr_t), pointer :: constituent_properties(:)

            call dyn_debug_print(debugout_info, 'Setting physics state variables externally')

            nullify(constituents)
            nullify(constituent_properties)

            allocate(minimum_constituents(num_advected), stat=ierr)
            call check_allocate(ierr, subname, &
                'minimum_constituents(num_advected)', &
                'dyn_coupling', __LINE__)

            do i = 1, num_advected
                minimum_constituents(i) = const_qmin(i)
            end do

            constituents => cam_constituents_array()

            if (.not. associated(constituents)) then
                call endrun('Failed to find variable "constituents"', subname, __LINE__)
            end if

            constituent_properties => cam_model_const_properties()

            if (.not. associated(constituent_properties)) then
                call endrun('Failed to find variable "constituent_properties"', subname, __LINE__)
            end if

            ! Update `cappav`, `cpairv`, `rairv`, `zvirv`, etc. as needed by calling `cam_thermo_dry_air_update`.
            ! Note that this subroutine expects constituents to be dry.
            call cam_thermo_dry_air_update( &
                constituents, phys_state % t, ncells_solve, pver, cam_runtime_opts % update_thermodynamic_variables())

            ! Update `cp_or_cv_dycore` by calling `cam_thermo_water_update`.
            ! Note that this subroutine expects constituents to be dry.
            call cam_thermo_water_update( &
                constituents, ncells_solve, pver, energy_formula_dycore_mpas, cp_or_cv_dycore)

            ! This variable name is really misleading. It actually represents the reciprocal of Exner function
            ! with respect to surface pressure. This definition is sometimes used for boundary layer work. See
            ! the paragraph below equation 1.5.1c in doi:10.1007/978-94-009-3027-8.
            ! Also note that `cappav` is updated externally by `cam_thermo_dry_air_update`.
            do i = 1, ncells_solve
                phys_state % exner(i, :) = (phys_state % ps(i) / phys_state % pmid(i, :)) ** cappav(i, :)
            end do

            ! Note that constituents become moist after this.
            call dyn_exchange_constituent_states(direction='i', exchange=.false., conversion=.true.)

            ! Impose minimum limits on constituents.
            call qneg_run(subname, ncells_solve, pver, minimum_constituents, constituents, ierr, cerr)

            if (ierr /= 0) then
                call endrun('Failed to impose minimum limits on constituents externally' // new_line('') // &
                    'External procedure returned with ' // stringify([ierr]) // ': ' // trim(adjustl(cerr)), &
                    subname, __LINE__)
            end if

            ! Set `zi` (i.e., geopotential height at layer interfaces) and `zm` (i.e., geopotential height at layer midpoints).
            ! Note that `rairv` and `zvirv` are updated externally by `cam_thermo_dry_air_update`.
            call geopotential_temp_run( &
                pver, lagrangian_vertical, pver, 1, pverp, 1, num_advected, &
                phys_state % lnpint, phys_state % pint, phys_state % pmid, phys_state % pdel, phys_state % rpdel, phys_state % t, &
                constituents(:, :, mpas_dynamical_core % map_constituent_index(index_qv)), constituents, &
                constituent_properties, rairv, constant_g, zvirv, phys_state % zi, phys_state % zm, ncells_solve, ierr, cerr)

            if (ierr /= 0) then
                call endrun('Failed to set variable "zi" and "zm" externally' // new_line('') // &
                    'External procedure returned with ' // stringify([ierr]) // ': ' // trim(adjustl(cerr)), &
                    subname, __LINE__)
            end if

            ! Set `dse` (i.e., dry static energy).
            ! Note that `cpairv` is updated externally by `cam_thermo_dry_air_update`.
            call update_dry_static_energy_run( &
                pver, constant_g, phys_state % t, phys_state % zm, phys_state % phis, phys_state % dse, cpairv, ierr, cerr)

            if (ierr /= 0) then
                call endrun('Failed to set variable "dse" externally' // new_line('') // &
                    'External procedure returned with ' // stringify([ierr]) // ': ' // trim(adjustl(cerr)), &
                    subname, __LINE__)
            end if

            deallocate(minimum_constituents)

            nullify(constituents)
            nullify(constituent_properties)
        end subroutine set_physics_state_external
    end subroutine dynamics_to_physics_coupling

    !> Perform one-way coupling from the physics output states to the dynamics input states.
    !> The other coupling direction is implemented by its counterpart, `dynamics_to_physics_coupling`.
    !> (KCW, 2024-09-20)
    subroutine physics_to_dynamics_coupling()
        ! Module(s) from CAM-SIMA.
        use cam_logfile, only: debugout_debug
        use dyn_comp, only: dyn_debug_print, dyn_exchange_constituent_states
        ! Module(s) from CESM Share.
        use shr_kind_mod, only: kind_r8 => shr_kind_r8
        ! Module(s) from MPAS.
        use dyn_mpas_subdriver, only: kind_dyn_mpas => mpas_dynamical_core_real_kind

        character(*), parameter :: subname = 'dyn_coupling::physics_to_dynamics_coupling'
        integer, pointer :: index_qv
        real(kind_r8), allocatable :: qv_prev(:, :) ! Water vapor mixing ratio (kg kg-1)
                                                    ! before being updated by physics.
        real(kind_dyn_mpas), pointer :: rho_zz(:, :)
        real(kind_dyn_mpas), pointer :: scalars(:, :, :)
        real(kind_dyn_mpas), pointer :: zz(:, :)

        call dyn_debug_print(debugout_debug, subname // ' entered')

        call init_shared_variables()

        call dyn_exchange_constituent_states(direction='e', exchange=.true., conversion=.true.)

        call set_mpas_physics_tendency_ru()
        call set_mpas_physics_tendency_rho()
        call set_mpas_physics_tendency_rtheta()

        call final_shared_variables()

        call dyn_debug_print(debugout_debug, subname // ' completed')
    contains
        !> Initialize variables that are shared and repeatedly used by the `set_mpas_physics_tendency_*` internal subroutines.
        !> (KCW, 2024-09-13)
        subroutine init_shared_variables()
            ! Module(s) from CAM-SIMA.
            use cam_abortutils, only: check_allocate
            use cam_logfile, only: debugout_info
            use dyn_comp, only: mpas_dynamical_core, ncells_solve
            use vert_coord, only: pver

            character(*), parameter :: subname = 'dyn_coupling::physics_to_dynamics_coupling::init_shared_variables'
            integer :: ierr

            call dyn_debug_print(debugout_info, 'Preparing for physics-dynamics coupling')

            nullify(index_qv)
            nullify(rho_zz)
            nullify(scalars)
            nullify(zz)

            call mpas_dynamical_core % get_variable_pointer(index_qv, 'dim', 'index_qv')
            call mpas_dynamical_core % get_variable_pointer(rho_zz, 'state', 'rho_zz', time_level=1)
            call mpas_dynamical_core % get_variable_pointer(scalars, 'state', 'scalars', time_level=1)
            call mpas_dynamical_core % get_variable_pointer(zz, 'mesh', 'zz')

            allocate(qv_prev(pver, ncells_solve), stat=ierr)
            call check_allocate(ierr, subname, &
                'qv_prev(pver, ncells_solve)', &
                'dyn_coupling', __LINE__)

            ! Save water vapor mixing ratio before being updated by physics because `set_mpas_physics_tendency_rtheta`
            ! needs it. This must be done before calling `dyn_exchange_constituent_states`.
            qv_prev(:, :) = real(scalars(index_qv, :, 1:ncells_solve), kind_r8)
        end subroutine init_shared_variables

        !> Finalize variables that are shared and repeatedly used by the `set_mpas_physics_tendency_*` internal subroutines.
        !> (KCW, 2024-09-13)
        subroutine final_shared_variables()
            character(*), parameter :: subname = 'dyn_coupling::physics_to_dynamics_coupling::final_shared_variables'

            deallocate(qv_prev)

            nullify(index_qv)
            nullify(rho_zz)
            nullify(scalars)
            nullify(zz)
        end subroutine final_shared_variables

        !> Set MPAS physics tendency `tend_ru_physics` (i.e., "coupled" tendency of horizontal velocity at edge interfaces
        !> due to physics). In MPAS, a "coupled" variable means that it is multiplied by a vertical metric term, `rho_zz`.
        !> (KCW, 2024-09-11)
        subroutine set_mpas_physics_tendency_ru()
            ! Module(s) from CAM-SIMA.
            use cam_logfile, only: debugout_info
            use dyn_comp, only: reverse, mpas_dynamical_core, ncells_solve
            use physics_types, only: phys_tend

            character(*), parameter :: subname = 'dyn_coupling::physics_to_dynamics_coupling::set_mpas_physics_tendency_ru'
            integer :: i
            real(kind_dyn_mpas), pointer :: u_tendency(:, :), v_tendency(:, :)

            call dyn_debug_print(debugout_info, 'Setting MPAS physics tendency "tend_ru_physics"')

            nullify(u_tendency, v_tendency)

            call mpas_dynamical_core % get_variable_pointer(u_tendency, 'tend_physics', 'tend_uzonal')
            call mpas_dynamical_core % get_variable_pointer(v_tendency, 'tend_physics', 'tend_umerid')

            ! Vertical index order is reversed between CAM-SIMA and MPAS.
            ! Always call `reverse` when assigning anything to/from the `physics_tend` derived type.
            do i = 1, ncells_solve
                u_tendency(:, i) = real(reverse(phys_tend % dudt_total(i, :)) * real(rho_zz(:, i), kind_r8), kind_dyn_mpas)
                v_tendency(:, i) = real(reverse(phys_tend % dvdt_total(i, :)) * real(rho_zz(:, i), kind_r8), kind_dyn_mpas)
            end do

            nullify(u_tendency, v_tendency)

            call mpas_dynamical_core % compute_edge_wind(wind_tendency=.true.)
        end subroutine set_mpas_physics_tendency_ru

        !> Set MPAS physics tendency `tend_rho_physics` (i.e., "coupled" tendency of dry air density due to physics).
        !> In MPAS, a "coupled" variable means that it is multiplied by a vertical metric term, `rho_zz`.
        !> (KCW, 2024-09-11)
        subroutine set_mpas_physics_tendency_rho()
            ! Module(s) from CAM-SIMA.
            use cam_logfile, only: debugout_info
            use dyn_comp, only: mpas_dynamical_core, ncells_solve

            character(*), parameter :: subname = 'dyn_coupling::physics_to_dynamics_coupling::set_mpas_physics_tendency_rho'
            real(kind_dyn_mpas), pointer :: rho_tendency(:, :)

            call dyn_debug_print(debugout_info, 'Setting MPAS physics tendency "tend_rho_physics"')

            nullify(rho_tendency)

            call mpas_dynamical_core % get_variable_pointer(rho_tendency, 'tend_physics', 'tend_rho_physics')

            ! The material derivative of `rho` (i.e., dry air density) is zero for incompressible fluid.
            rho_tendency(:, 1:ncells_solve) = 0.0_kind_dyn_mpas

            nullify(rho_tendency)

            ! Because we are injecting data directly into MPAS memory, halo layers need to be updated manually.
            call mpas_dynamical_core % exchange_halo('tend_rho_physics')
        end subroutine set_mpas_physics_tendency_rho

        !> Set MPAS physics tendency `tend_rtheta_physics` (i.e., "coupled" tendency of modified "moist" potential temperature
        !> due to physics). In MPAS, a "coupled" variable means that it is multiplied by a vertical metric term, `rho_zz`.
        !> (KCW, 2024-09-19)
        subroutine set_mpas_physics_tendency_rtheta()
            ! Module(s) from CAM-SIMA.
            use cam_abortutils, only: check_allocate
            use cam_logfile, only: debugout_info
            use dyn_comp, only: reverse, mpas_dynamical_core, ncells_solve
            use dynconst, only: constant_rd => rair, constant_rv => rh2o
            use physics_types, only: dtime_phys, phys_tend
            use vert_coord, only: pver

            character(*), parameter :: subname = 'dyn_coupling::physics_to_dynamics_coupling::set_mpas_physics_tendency_rtheta'
            integer :: i
            integer :: ierr
            ! Variable name suffixes have the following meanings:
            ! `*_col`: Variable is of each column.
            ! `*_prev`: Variable is before being updated by physics.
            ! `*_curr`: Variable is after being updated by physics.
            real(kind_r8), allocatable :: qv_col_prev(:), qv_col_curr(:)         ! Water vapor mixing ratio (kg kg-1).
            real(kind_r8), allocatable :: rhod_col(:)                            ! Dry air density (kg m-3).
            real(kind_r8), allocatable :: t_col_prev(:), t_col_curr(:)           ! Temperature (K).
            real(kind_r8), allocatable :: theta_col_prev(:), theta_col_curr(:)   ! Potential temperature (K).
            real(kind_r8), allocatable :: thetam_col_prev(:), thetam_col_curr(:) ! Modified "moist" potential temperature (K).
            real(kind_dyn_mpas), pointer :: theta_m(:, :)
            real(kind_dyn_mpas), pointer :: theta_m_tendency(:, :)

            call dyn_debug_print(debugout_info, 'Setting MPAS physics tendency "tend_rtheta_physics"')

            nullify(theta_m)
            nullify(theta_m_tendency)

            allocate(qv_col_prev(pver), qv_col_curr(pver), stat=ierr)
            call check_allocate(ierr, subname, &
                'qv_col_prev(pver), qv_col_curr(pver)', &
                'dyn_coupling', __LINE__)

            allocate(rhod_col(pver), stat=ierr)
            call check_allocate(ierr, subname, &
                'rhod_col(pver)', &
                'dyn_coupling', __LINE__)

            allocate(t_col_prev(pver), t_col_curr(pver), stat=ierr)
            call check_allocate(ierr, subname, &
                't_col_prev(pver), t_col_curr(pver)', &
                'dyn_coupling', __LINE__)

            allocate(theta_col_prev(pver), theta_col_curr(pver), stat=ierr)
            call check_allocate(ierr, subname, &
                'theta_col_prev(pver), theta_col_curr(pver)', &
                'dyn_coupling', __LINE__)

            allocate(thetam_col_prev(pver), thetam_col_curr(pver), stat=ierr)
            call check_allocate(ierr, subname, &
                'thetam_col_prev(pver), thetam_col_curr(pver)', &
                'dyn_coupling', __LINE__)

            call mpas_dynamical_core % get_variable_pointer(theta_m, 'state', 'theta_m', time_level=1)
            call mpas_dynamical_core % get_variable_pointer(theta_m_tendency, 'tend_physics', 'tend_rtheta_physics')

            ! Set `theta_m_tendency` column by column. This way, peak memory usage can be reduced.
            do i = 1, ncells_solve
                qv_col_curr(:) = real(scalars(index_qv, :, i), kind_r8)
                qv_col_prev(:) = qv_prev(:, i)
                rhod_col(:) = real(rho_zz(:, i) * zz(:, i), kind_r8)

                thetam_col_prev(:) = real(theta_m(:, i), kind_r8)
                theta_col_prev(:) = thetam_col_prev(:) / (1.0_kind_r8 + constant_rv / constant_rd * qv_col_prev(:))
                t_col_prev(:) = t_of_theta_rhod_qv(theta_col_prev, rhod_col, qv_col_prev)

                ! Vertical index order is reversed between CAM-SIMA and MPAS.
                ! Always call `reverse` when assigning anything to/from the `physics_tend` derived type.
                t_col_curr(:) = t_col_prev(:) + reverse(phys_tend % dtdt_total(i, :)) * dtime_phys
                theta_col_curr(:) = theta_of_t_rhod_qv(t_col_curr, rhod_col, qv_col_curr)
                thetam_col_curr(:) = theta_col_curr(:) * (1.0_kind_r8 + constant_rv / constant_rd * qv_col_curr(:))

                theta_m_tendency(:, i) = &
                    real((thetam_col_curr(:) - thetam_col_prev(:)) * real(rho_zz(:, i), kind_r8) / dtime_phys, kind_dyn_mpas)
            end do

            deallocate(qv_col_prev, qv_col_curr)
            deallocate(rhod_col)
            deallocate(t_col_prev, t_col_curr)
            deallocate(theta_col_prev, theta_col_curr)
            deallocate(thetam_col_prev, thetam_col_curr)

            nullify(theta_m)
            nullify(theta_m_tendency)

            ! Because we are injecting data directly into MPAS memory, halo layers need to be updated manually.
            call mpas_dynamical_core % exchange_halo('tend_rtheta_physics')
        end subroutine set_mpas_physics_tendency_rtheta

        !> Compute temperature `t` as a function of potential temperature `theta`, dry air density `rhod` and water vapor
        !> mixing ratio `qv`. Essentially,
        !> \( T = \theta^{\frac{C_p}{C_v}} [\frac{\rho_d R_d (1 + \frac{R_v}{R_d} q_v)}{P_0}]^{\frac{R_d}{C_v}} \).
        !> The formulation comes from Poisson equation with equation of state plugged in and arranging
        !> for temperature. This function is the exact inverse of `theta_of_t_rhod_qv`, which means that:
        !> `t == t_of_theta_rhod_qv(theta_of_t_rhod_qv(t, rhod, qv), rhod, qv)`.
        !> (KCW, 2024-09-13)
        pure elemental function t_of_theta_rhod_qv(theta, rhod, qv) result(t)
            ! Module(s) from CAM-SIMA.
            use dynconst, only: constant_cpd => cpair, constant_p0 => pref, &
                                constant_rd => rair, constant_rv => rh2o

            real(kind_r8), intent(in) :: theta, rhod, qv
            real(kind_r8) :: t

            real(kind_r8) :: constant_cvd ! Specific heat of dry air at constant volume.

            ! Mayer's relation.
            constant_cvd = constant_cpd - constant_rd

            ! Poisson equation with equation of state plugged in and arranging for temperature. For equation of state,
            ! it can be shown that the effect of water vapor can be passed on to the temperature term entirely such that
            ! dry air density and dry air gas constant can be used at all times. This modified "moist" temperature is
            ! described herein:
            ! The paragraph below equation 2.7 in doi:10.5065/1DFH-6P97.
            ! The paragraph below equation 2 in doi:10.1175/MWR-D-11-00215.1.
            !
            ! In all, solve the below equation set for $T$ in terms of $\theta$, $\rho_d$ and $q_v$:
            ! \begin{equation*}
            !     \begin{cases}
            !         \theta &= T (\frac{P_0}{P})^{\frac{R_d}{C_p}} \\
            !         P &= \rho_d R_d T_m \\
            !         T_m &= T (1 + \frac{R_v}{R_d} q_v)
            !     \end{cases}
            ! \end{equation*}
            t = (theta ** (constant_cpd / constant_cvd)) * &
                (((rhod * constant_rd * (1.0_kind_r8 + constant_rv / constant_rd * qv)) / constant_p0) ** &
                (constant_rd / constant_cvd))
        end function t_of_theta_rhod_qv

        !> Compute potential temperature `theta` as a function of temperature `t`, dry air density `rhod` and water vapor
        !> mixing ratio `qv`. Essentially,
        !> \( \theta = T^{\frac{C_v}{C_p}} [\frac{P_0}{\rho_d R_d (1 + \frac{R_v}{R_d} q_v)}]^{\frac{R_d}{C_p}} \).
        !> The formulation comes from Poisson equation with equation of state plugged in and arranging
        !> for potential temperature. This function is the exact inverse of `t_of_theta_rhod_qv`, which means that:
        !> `theta == theta_of_t_rhod_qv(t_of_theta_rhod_qv(theta, rhod, qv), rhod, qv)`.
        !> (KCW, 2024-09-13)
        pure elemental function theta_of_t_rhod_qv(t, rhod, qv) result(theta)
            ! Module(s) from CAM-SIMA.
            use dynconst, only: constant_cpd => cpair, constant_p0 => pref, &
                                constant_rd => rair, constant_rv => rh2o

            real(kind_r8), intent(in) :: t, rhod, qv
            real(kind_r8) :: theta

            real(kind_r8) :: constant_cvd ! Specific heat of dry air at constant volume.

            ! Mayer's relation.
            constant_cvd = constant_cpd - constant_rd

            ! Poisson equation with equation of state plugged in and arranging for potential temperature. For equation of state,
            ! it can be shown that the effect of water vapor can be passed on to the temperature term entirely such that
            ! dry air density and dry air gas constant can be used at all times. This modified "moist" temperature is
            ! described herein:
            ! The paragraph below equation 2.7 in doi:10.5065/1DFH-6P97.
            ! The paragraph below equation 2 in doi:10.1175/MWR-D-11-00215.1.
            !
            ! In all, solve the below equation set for $\theta$ in terms of $T$, $\rho_d$ and $q_v$:
            ! \begin{equation*}
            !     \begin{cases}
            !         \theta &= T (\frac{P_0}{P})^{\frac{R_d}{C_p}} \\
            !         P &= \rho_d R_d T_m \\
            !         T_m &= T (1 + \frac{R_v}{R_d} q_v)
            !     \end{cases}
            ! \end{equation*}
            theta = (t ** (constant_cvd / constant_cpd)) * &
                ((constant_p0 / (rhod * constant_rd * (1.0_kind_r8 + constant_rv / constant_rd * qv))) ** &
                (constant_rd / constant_cpd))
        end function theta_of_t_rhod_qv
    end subroutine physics_to_dynamics_coupling
end module dyn_coupling
