module dynconst

   !Physical constants with the same precision (kind) as the dycore.

   use shr_kind_mod, only: kind_dyn=>shr_kind_r8
   use physconst,    only: phys_pi=>pi

   implicit none
   private

   !Physical constants:

   !Please note that pi must be a parameter, as it is used in
   !parameter calls in the dynamics itself.  This should be ok,
   !though, as Pi is an actual mathematical constant that should
   !never change:

   !circle's circumference/diameter [unitless]
   real(kind_dyn), parameter, public :: pi = real(phys_pi, kind_dyn)

   ! Convert degrees to radians
   real(kind_dyn), parameter, public :: deg_to_rad = pi / 180.0_kind_dyn
   ! Convert radians to degrees
   real(kind_dyn), parameter, public :: rad_to_deg = 180.0_kind_dyn / pi

   ! radius of earth [m]
   real(kind_dyn), protected, public :: rearth
   ! reciprocal of earth's radius [1/m]
   real(kind_dyn), protected, public :: ra
   ! earth's rotation rate [rad/sec]
   real(kind_dyn), protected, public :: omega
   ! gravitational acceleration [m/s**2]
   real(kind_dyn), protected, public :: gravit
   ! specific heat of dry air [J/K/kg]
   real(kind_dyn), protected, public :: cpair
   ! Dry air gas constant [J/K/kg]
   real(kind_dyn), protected, public :: rair
   ! water vapor gas constant [J/K/kg]
   real(kind_dyn), protected, public :: rh2o
   ! reference surface pressure [Pa]
   real(kind_dyn), protected, public :: pref
   ! reference temperature [K]
   real(kind_dyn), protected, public :: tref
   ! reference lapse rate [K/m]
   real(kind_dyn), protected, public :: lapse_rate
   ! R/Cp
   real(kind_dyn), protected, public :: cappa
   ! Standard pressure [Pa]
   real(kind_dyn), protected, public :: pstd


   !Public routines:
   public :: dynconst_init

!==============================================================================
CONTAINS
!==============================================================================

   subroutine dynconst_init

      !Subroutine to initialize physical
      !and mathematical constants used
      !by the dynamics with the same
      !real kind used by the dynamics itself.

      use physconst, only: phys_rearth=>rearth
      use physconst, only: phys_ra=>rearth_recip
      use physconst, only: phys_omega=>omega
      use physconst, only: phys_cpair=>cpair
      use physconst, only: phys_gravit=>gravit
      use physconst, only: phys_pref=>pref
      use physconst, only: phys_tref=>tref
      use physconst, only: phys_lapse_rate=>lapse_rate
      use physconst, only: phys_cappa=>cappa
      use physconst, only: phys_rair=>rair
      use physconst, only: phys_rh2o=>rh2o
      use physconst, only: phys_pstd=>pstd

      !Set constants used by the dynamics:

      rearth     = real(phys_rearth, kind_dyn)
      ra         = real(phys_ra, kind_dyn)
      omega      = real(phys_omega, kind_dyn)
      cpair      = real(phys_cpair, kind_dyn)
      rair       = real(phys_rair, kind_dyn)
      rh2o       = real(phys_rh2o, kind_dyn)
      gravit     = real(phys_gravit, kind_dyn)
      pref       = real(phys_pref, kind_dyn)
      tref       = real(phys_tref, kind_dyn)
      lapse_rate = real(phys_lapse_rate, kind_dyn)
      cappa      = real(phys_cappa, kind_dyn)
      pstd       = real(phys_pstd, kind_dyn)
   end subroutine dynconst_init

end module dynconst
