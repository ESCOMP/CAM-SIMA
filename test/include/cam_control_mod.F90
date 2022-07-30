module cam_control_mod
!------------------------------------------------------------------------------
!
! High level control variables.  Information received from the driver/coupler is
! stored here.
!
!------------------------------------------------------------------------------

   use shr_kind_mod,     only: r8=>shr_kind_r8, cs=>shr_kind_cs, cl=>shr_kind_cl
   use spmd_utils,       only: masterproc
   use cam_logfile,      only: iulog
   use cam_abortutils,   only: endrun

   implicit none
   public
   save

   ! Public Routines:
   !
   !   cam_ctrl_init
   !   cam_ctrl_set_orbit
   !   cam_ctrl_set_physics_type

   character(len=cl), protected :: caseid  ! case ID
   character(len=cl), protected :: ctitle  ! case title

   logical, protected :: initial_run  ! startup mode which only requires a minimal initial file
   logical, protected :: restart_run  ! continue a previous run; requires a restart file
   logical, protected :: branch_run   ! branch from a previous run; requires a restart file

   logical, protected :: adiabatic         ! true => no physics
   logical, protected :: ideal_phys        ! true => run Held-Suarez (1994) physics
   logical, protected :: kessler_phys      ! true => run Kessler physics
   logical, protected :: tj2016_phys       ! true => run tj2016 physics
   logical, protected :: simple_phys       ! true => adiabatic or ideal_phys or kessler_phys
   !         or tj2016
   logical, protected :: aqua_planet       ! Flag to run model in "aqua planet" mode
   logical, protected :: moist_physics     ! true => moist physics enabled, i.e.,
   ! (.not. ideal_phys) .and. (.not. adiabatic)

   logical, protected :: brnch_retain_casename ! true => branch run may use same caseid as
   !         the run being branched from

   real(r8), protected :: eccen       ! Earth's eccentricity factor (unitless) (typically 0 to 0.1)
   real(r8), protected :: obliqr      ! Earth's obliquity in radians
   real(r8), protected :: lambm0      ! Mean longitude of perihelion at the
   ! vernal equinox (radians)
   real(r8), protected :: mvelpp      ! Earth's moving vernal equinox longitude
   ! of perihelion plus pi (radians)
end module cam_control_mod
