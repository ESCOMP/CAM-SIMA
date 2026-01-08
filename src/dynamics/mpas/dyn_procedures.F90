! Copyright (C) 2025 University Corporation for Atmospheric Research (UCAR)
! SPDX-License-Identifier: Apache-2.0

!> This module provides standardized procedures (i.e., functions and subroutines) that serve as
!> reusable building blocks for larger and more complex functionalities elsewhere. Specifically,
!> procedures in this module are intended to be used by CAM-SIMA and therefore are compiled
!> together with it.
!>
!> Computational procedures implement formulas that are universal in atmospheric sciences. They
!> should be designated as `elemental` where possible to aid compiler optimizations, such as
!> vectorization.
!> Utility procedures implement simple and well-defined operations that can be easily tested.
module dyn_procedures
    implicit none

    private
    ! Computational procedures.
    public :: p_by_equation_of_state, rho_by_equation_of_state, t_by_equation_of_state
    public :: exner_function
    public :: dp_by_hydrostatic_equation
    public :: p_by_hypsometric_equation
    public :: t_by_poisson_equation, theta_by_poisson_equation
    public :: omega_of_w_rho, w_of_omega_rho
    public :: qv_of_sh, sh_of_qv
    public :: t_of_theta_rhod_qv, theta_of_t_rhod_qv
    public :: t_of_tm_qv, tm_of_t_qv
    public :: tm_of_tv_qv, tv_of_tm_qv
    ! Utility procedures.
    public :: reverse
    public :: sec_to_hour_min_sec

    interface exner_function
        module procedure exner_function_of_cpd_p0_rd_p
        module procedure exner_function_of_kappa_p0_p
    end interface exner_function
contains
    !> Compute the pressure `p` from the density `rho` and the temperature `t` by equation of state. Essentially,
    !> \( P = \rho R T \). Equation of state may take other forms, such as \( P_d = \rho_d R_d T \), \( P = \rho R_d T_v \),
    !> \( P = \rho_d R_d T_m \). Choose wisely which form to use.
    !> (KCW, 2025-07-10)
    pure elemental function p_by_equation_of_state(constant_r, rho, t) result(p)
        use, intrinsic :: iso_fortran_env, only: real64

        real(real64), intent(in) :: constant_r, rho, t
        real(real64) :: p

        p = rho * constant_r * t
    end function p_by_equation_of_state

    !> Compute the density `rho` from the pressure `p` and the temperature `t` by equation of state. Essentially,
    !> \( \rho = \frac{P}{R T} \). Equation of state may take other forms, such as \( P_d = \rho_d R_d T \), \( P = \rho R_d T_v \),
    !> \( P = \rho_d R_d T_m \). Choose wisely which form to use.
    !> (KCW, 2025-07-10)
    pure elemental function rho_by_equation_of_state(constant_r, p, t) result(rho)
        use, intrinsic :: iso_fortran_env, only: real64

        real(real64), intent(in) :: constant_r, p, t
        real(real64) :: rho

        rho = p / (constant_r * t)
    end function rho_by_equation_of_state

    !> Compute the temperature `t` from the pressure `p` and the density `rho` by equation of state. Essentially,
    !> \( T = \frac{P}{\rho R} \). Equation of state may take other forms, such as \( P_d = \rho_d R_d T \), \( P = \rho R_d T_v \),
    !> \( P = \rho_d R_d T_m \). Choose wisely which form to use.
    !> (KCW, 2025-07-10)
    pure elemental function t_by_equation_of_state(constant_r, p, rho) result(t)
        use, intrinsic :: iso_fortran_env, only: real64

        real(real64), intent(in) :: constant_r, p, rho
        real(real64) :: t

        t = p / (rho * constant_r)
    end function t_by_equation_of_state

    !> Compute the Exner function `pi` from the pressure `p`. Essentially, \( \Pi = (\frac{P}{P_0})^{\frac{R_d}{C_{pd}}} \).
    !> (KCW, 2025-07-10)
    pure elemental function exner_function_of_cpd_p0_rd_p(constant_cpd, constant_p0, constant_rd, p) result(pi)
        use, intrinsic :: iso_fortran_env, only: real64

        real(real64), intent(in) :: constant_cpd, constant_p0, constant_rd, p
        real(real64) :: pi

        pi = (p / constant_p0) ** (constant_rd / constant_cpd)
    end function exner_function_of_cpd_p0_rd_p

    !> Compute the Exner function `pi` from the pressure `p`. Essentially, \( \Pi = (\frac{P}{P_0})^{\kappa} \).
    !> (KCW, 2025-08-16)
    pure elemental function exner_function_of_kappa_p0_p(constant_kappa, constant_p0, p) result(pi)
        use, intrinsic :: iso_fortran_env, only: real64

        real(real64), intent(in) :: constant_kappa, constant_p0, p
        real(real64) :: pi

        pi = (p / constant_p0) ** constant_kappa
    end function exner_function_of_kappa_p0_p

    !> Compute the pressure difference `dp` from the density `rho` and the height difference `dz` by hydrostatic equation.
    !> Essentially, \( \mathrm{d} P = -\rho g \mathrm{d} z \).
    !> (KCW, 2025-07-10)
    pure elemental function dp_by_hydrostatic_equation(constant_g, rho, dz) result(dp)
        use, intrinsic :: iso_fortran_env, only: real64

        real(real64), intent(in) :: constant_g, rho, dz
        real(real64) :: dp

        dp = -rho * constant_g * dz
    end function dp_by_hydrostatic_equation

    ! ----- p2, z2 ----- (Layer 2)
    !       tv
    ! ----- p1, z1 ----- (Layer 1)
    !
    !> Compute the pressure `p2` at height `z2` from the pressure `p1` at height `z1` by hypsometric equation.
    !> `tv` is the mean virtual temperature between `z1` and `z2`. Essentially,
    !> \( P_2 = P_1 e^{\frac{-(z_2 - z_1) g}{R_d T_v}} \).
    !> (KCW, 2024-07-02)
    pure elemental function p_by_hypsometric_equation(constant_g, constant_rd, p1, z1, tv, z2) result(p2)
        use, intrinsic :: iso_fortran_env, only: real64

        real(real64), intent(in) :: constant_g, constant_rd, p1, z1, tv, z2
        real(real64) :: p2

        p2 = p1 * exp(-(z2 - z1) * constant_g / (constant_rd * tv))
    end function p_by_hypsometric_equation

    ! ------ p, t --------- (Arbitrary layer)
    !
    ! ----- p0, theta ----- (Reference layer)
    !
    !> Compute the temperature `t` at pressure `p` from the potential temperature `theta` at reference pressure `p0` by
    !> Poisson equation. Essentially, \( T = \theta (\frac{P}{P_0})^{\frac{R_d}{C_{pd}}} \).
    !> (KCW, 2025-07-14)
    pure elemental function t_by_poisson_equation(constant_cpd, constant_p0, constant_rd, theta, p) result(t)
        use, intrinsic :: iso_fortran_env, only: real64

        real(real64), intent(in) :: constant_cpd, constant_p0, constant_rd, theta, p
        real(real64) :: t

        t = theta * ((p / constant_p0) ** (constant_rd / constant_cpd))
    end function t_by_poisson_equation

    ! ------ p, t --------- (Arbitrary layer)
    !
    ! ----- p0, theta ----- (Reference layer)
    !
    !> Compute the potential temperature `theta` at reference pressure `p0` from the temperature `t` at pressure `p` by
    !> Poisson equation. Essentially, \( \theta = T (\frac{P_0}{P})^{\frac{R_d}{C_{pd}}} \).
    !> (KCW, 2024-07-02)
    pure elemental function theta_by_poisson_equation(constant_cpd, constant_p0, constant_rd, t, p) result(theta)
        use, intrinsic :: iso_fortran_env, only: real64

        real(real64), intent(in) :: constant_cpd, constant_p0, constant_rd, t, p
        real(real64) :: theta

        theta = t * ((constant_p0 / p) ** (constant_rd / constant_cpd))
    end function theta_by_poisson_equation

    !> Compute the vertical pressure velocity `omega` as a function of the vertical velocity `w` and the density `rho`.
    !> Essentially, \( \omega = -\rho g w \).
    !> (KCW, 2025-07-10)
    pure elemental function omega_of_w_rho(constant_g, w, rho) result(omega)
        use, intrinsic :: iso_fortran_env, only: real64

        real(real64), intent(in) :: constant_g, w, rho
        real(real64) :: omega

        omega = -rho * constant_g * w
    end function omega_of_w_rho

    !> Compute the vertical velocity `w` as a function of the vertical pressure velocity `omega` and the density `rho`.
    !> Essentially, \( w = -\frac{\omega}{\rho g} \).
    !> (KCW, 2025-07-10)
    pure elemental function w_of_omega_rho(constant_g, omega, rho) result(w)
        use, intrinsic :: iso_fortran_env, only: real64

        real(real64), intent(in) :: constant_g, omega, rho
        real(real64) :: w

        w = -omega / (rho * constant_g)
    end function w_of_omega_rho

    !> Compute the water vapor mixing ratio `qv` as a function of the specific humidity `sh`.
    !> Essentially, \( q_v = \frac{SH}{1 - SH} \).
    !> (KCW, 2025-07-21)
    pure elemental function qv_of_sh(sh) result(qv)
        use, intrinsic :: iso_fortran_env, only: real64

        real(real64), intent(in) :: sh
        real(real64) :: qv

        qv = sh / (1.0_real64 - sh)
    end function qv_of_sh

    !> Compute the specific humidity `sh` as a function of the water vapor mixing ratio `qv`.
    !> Essentially, \( SH = \frac{q_v}{1 + q_v} \).
    !> (KCW, 2025-07-21)
    pure elemental function sh_of_qv(qv) result(sh)
        use, intrinsic :: iso_fortran_env, only: real64

        real(real64), intent(in) :: qv
        real(real64) :: sh

        sh = qv / (1.0_real64 + qv)
    end function sh_of_qv

    !> Compute the temperature `t` as a function of the potential temperature `theta`, the dry air density `rhod`, and
    !> the water vapor mixing ratio `qv`. Essentially,
    !> \( T = \theta^{\frac{C_{pd}}{C_{vd}}} [\frac{\rho_d R_d (1 + \frac{R_v}{R_d} q_v)}{P_0}]^{\frac{R_d}{C_{vd}}} \).
    !> The formulation comes from Poisson equation with equation of state plugged in and arranging
    !> for temperature. This function is the exact inverse of `theta_of_t_rhod_qv`, which means that:
    !> `t == t_of_theta_rhod_qv(..., theta_of_t_rhod_qv(..., t, rhod, qv), rhod, qv)`.
    !> (KCW, 2024-09-13)
    pure elemental function t_of_theta_rhod_qv(constant_cpd, constant_p0, constant_rd, constant_rv, theta, rhod, qv) result(t)
        use, intrinsic :: iso_fortran_env, only: real64

        real(real64), intent(in) :: constant_cpd, constant_p0, constant_rd, constant_rv, theta, rhod, qv
        real(real64) :: t

        real(real64) :: constant_cvd ! Specific heat of dry air at constant volume.

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
        !         \theta &= T (\frac{P_0}{P})^{\frac{R_d}{C_{pd}}} \\[0pt]
        !         P &= \rho_d R_d T_m \\[0pt]
        !         T_m &= T (1 + \frac{R_v}{R_d} q_v)
        !     \end{cases}
        ! \end{equation*}
        t = (theta ** (constant_cpd / constant_cvd)) * &
            (((rhod * constant_rd * (1.0_real64 + constant_rv / constant_rd * qv)) / constant_p0) ** &
            (constant_rd / constant_cvd))
    end function t_of_theta_rhod_qv

    !> Compute the potential temperature `theta` as a function of the temperature `t`, the dry air density `rhod`, and
    !> the water vapor mixing ratio `qv`. Essentially,
    !> \( \theta = T^{\frac{C_{vd}}{C_{pd}}} [\frac{P_0}{\rho_d R_d (1 + \frac{R_v}{R_d} q_v)}]^{\frac{R_d}{C_{pd}}} \).
    !> The formulation comes from Poisson equation with equation of state plugged in and arranging
    !> for potential temperature. This function is the exact inverse of `t_of_theta_rhod_qv`, which means that:
    !> `theta == theta_of_t_rhod_qv(..., t_of_theta_rhod_qv(..., theta, rhod, qv), rhod, qv)`.
    !> (KCW, 2024-09-13)
    pure elemental function theta_of_t_rhod_qv(constant_cpd, constant_p0, constant_rd, constant_rv, t, rhod, qv) result(theta)
        use, intrinsic :: iso_fortran_env, only: real64

        real(real64), intent(in) :: constant_cpd, constant_p0, constant_rd, constant_rv, t, rhod, qv
        real(real64) :: theta

        real(real64) :: constant_cvd ! Specific heat of dry air at constant volume.

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
        !         \theta &= T (\frac{P_0}{P})^{\frac{R_d}{C_{pd}}} \\[0pt]
        !         P &= \rho_d R_d T_m \\[0pt]
        !         T_m &= T (1 + \frac{R_v}{R_d} q_v)
        !     \end{cases}
        ! \end{equation*}
        theta = (t ** (constant_cvd / constant_cpd)) * &
            ((constant_p0 / (rhod * constant_rd * (1.0_real64 + constant_rv / constant_rd * qv))) ** &
            (constant_rd / constant_cpd))
    end function theta_of_t_rhod_qv

    !> Compute the temperature `t` as a function of the modified moist temperature `tm` and the water vapor mixing ratio `qv`.
    !> Essentially, \( T = \frac{T_m}{1 + \frac{R_v}{R_d} q_v} \). This modified "moist" temperature is described herein:
    !> The paragraph below equation 2.7 in doi:10.5065/1DFH-6P97.
    !> The paragraph below equation 2 in doi:10.1175/MWR-D-11-00215.1.
    !> Similarly, it can be shown that the relation, \( \theta = \frac{\theta_m}{1 + \frac{R_v}{R_d} q_v} \), also holds.
    !> (KCW, 2025-07-10)
    pure elemental function t_of_tm_qv(constant_rd, constant_rv, tm, qv) result(t)
        use, intrinsic :: iso_fortran_env, only: real64

        real(real64), intent(in) :: constant_rd, constant_rv, tm, qv
        real(real64) :: t

        t = tm / (1.0_real64 + constant_rv / constant_rd * qv)
    end function t_of_tm_qv

    !> Compute the modified moist temperature `tm` as a function of the temperature `t` and the water vapor mixing ratio `qv`.
    !> Essentially, \( T_m = T (1 + \frac{R_v}{R_d} q_v) \). This modified "moist" temperature is described herein:
    !> The paragraph below equation 2.7 in doi:10.5065/1DFH-6P97.
    !> The paragraph below equation 2 in doi:10.1175/MWR-D-11-00215.1.
    !> Similarly, it can be shown that the relation, \( \theta_m = \theta (1 + \frac{R_v}{R_d} q_v) \), also holds.
    !> (KCW, 2025-07-10)
    pure elemental function tm_of_t_qv(constant_rd, constant_rv, t, qv) result(tm)
        use, intrinsic :: iso_fortran_env, only: real64

        real(real64), intent(in) :: constant_rd, constant_rv, t, qv
        real(real64) :: tm

        tm = t * (1.0_real64 + constant_rv / constant_rd * qv)
    end function tm_of_t_qv

    !> Compute the modified moist temperature `tm` as a function of the virtual temperature `tv` and
    !> the water vapor mixing ratio `qv`.
    !> Essentially, \( T_m = T_v (1 + q_v) \). This modified "moist" temperature is described herein:
    !> The paragraph below equation 2.7 in doi:10.5065/1DFH-6P97.
    !> The paragraph below equation 2 in doi:10.1175/MWR-D-11-00215.1.
    !> Similarly, it can be shown that the relation, \( \theta_m = \theta_v (1 + q_v) \), also holds.
    !> (KCW, 2025-07-11)
    pure elemental function tm_of_tv_qv(tv, qv) result(tm)
        use, intrinsic :: iso_fortran_env, only: real64

        real(real64), intent(in) :: tv, qv
        real(real64) :: tm

        tm = tv * (1.0_real64 + qv)
    end function tm_of_tv_qv

    !> Compute the virtual temperature `tv` as a function of the modified moist temperature `tm` and
    !> the water vapor mixing ratio `qv`.
    !> Essentially, \( T_v = \frac{T_m}{1 + q_v} \). This modified "moist" temperature is described herein:
    !> The paragraph below equation 2.7 in doi:10.5065/1DFH-6P97.
    !> The paragraph below equation 2 in doi:10.1175/MWR-D-11-00215.1.
    !> Similarly, it can be shown that the relation, \( \theta_v = \frac{\theta_m}{1 + q_v} \), also holds.
    !> (KCW, 2025-07-11)
    pure elemental function tv_of_tm_qv(tm, qv) result(tv)
        use, intrinsic :: iso_fortran_env, only: real64

        real(real64), intent(in) :: tm, qv
        real(real64) :: tv

        tv = tm / (1.0_real64 + qv)
    end function tv_of_tm_qv

    !> Reverse the order of elements in `array`.
    !> (KCW, 2024-07-17)
    pure function reverse(array)
        use, intrinsic :: iso_fortran_env, only: real64

        real(real64), intent(in) :: array(:)
        real(real64) :: reverse(size(array))

        integer :: n

        n = size(array)

        ! There is nothing to reverse.
        if (n == 0) then
            return
        end if

        reverse(:) = array(n:1:-1)
    end function reverse

    !> Convert second(s) to hour(s), minute(s), and second(s).
    !> (KCW, 2024-02-07)
    pure function sec_to_hour_min_sec(sec) result(hour_min_sec)
        use, intrinsic :: iso_fortran_env, only: int32

        integer(int32), intent(in) :: sec
        integer(int32) :: hour_min_sec(3)

        ! These are all intended to be integer arithmetic.
        hour_min_sec(1) = sec / 3600_int32
        hour_min_sec(2) = sec / 60_int32 - hour_min_sec(1) * 60_int32
        hour_min_sec(3) = sec - hour_min_sec(1) * 3600_int32 - hour_min_sec(2) * 60_int32
    end function sec_to_hour_min_sec
end module dyn_procedures
