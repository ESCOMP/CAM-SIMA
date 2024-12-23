module cam_thermo_formula

   use runtime_obj, only: unset_int

   implicit none
   private
   save

   ! saves energy formula to use for physics and dynamical core
   ! for use in cam_thermo, air_composition and other modules
   ! separated into cam_thermo_formula module for clean dependencies

   ! energy_formula options (was vcoord in CAM and stored in dyn_tests_utils)
   integer, public, parameter :: ENERGY_FORMULA_DYCORE_FV   = 0  ! vc_moist_pressure
   integer, public, parameter :: ENERGY_FORMULA_DYCORE_SE   = 1  ! vc_dry_pressure
   integer, public, parameter :: ENERGY_FORMULA_DYCORE_MPAS = 2  ! vc_height

   !> \section arg_table_cam_thermo_formula  Argument Table
   !! \htmlinclude cam_thermo_formula.html
   ! energy_formula_dycore: energy formula used for dynamical core
   ! written by the dynamical core
   integer, public :: energy_formula_dycore  = unset_int
   ! energy_formula_physics: energy formula used for physics
   integer, public :: energy_formula_physics = unset_int

   ! Public subroutines
   public :: cam_thermo_formula_init

contains
   subroutine cam_thermo_formula_init()
      use phys_vars_init_check, only: mark_as_initialized

      ! Physics energy formulation is always FV (moist pressure coordinate)
      energy_formula_physics = ENERGY_FORMULA_DYCORE_FV
      call mark_as_initialized("total_energy_formula_for_physics")

   end subroutine cam_thermo_formula_init

end module cam_thermo_formula
