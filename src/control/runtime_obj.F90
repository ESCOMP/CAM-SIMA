module runtime_obj

   use shr_kind_mod, only: CS => SHR_KIND_CS
   use shr_kind_mod, only: r8=>shr_kind_r8
   use ccpp_kinds,   only: kind_phys
   implicit none
   private

   !> \section arg_table_runtime_obj  Argument Table
   !! \htmlinclude arg_table_runtime_obj.html
   !!
   real(kind_phys),  public, parameter :: unset_real   = huge(1.0_r8)

   character(len=*), public, parameter :: unset_str    = 'UNSET'
   integer,          public, parameter :: unset_int    = huge(1)

   ! Water vapor constituent standard name
   character(len=*), public, parameter :: wv_stdname = 'water_vapor_mixing_ratio_wrt_moist_air_and_condensed_water'

   ! Public interfaces and data
   type, public :: runtime_options
      character(len=CS), private            :: phys_suite = unset_str
      character(len=CS), private            :: dycore     = unset_str
      character(len=16), private            :: waccmx_opt = unset_str
      ! update_thermo_variables: update thermo "constants" to composition-dependent thermo variables
      logical,           private :: update_thermo_variables = .false.
   contains
      ! General runtime access
      procedure, public :: physics_suite
      procedure, public :: get_dycore
      procedure, public :: set_dycore
      procedure, public :: suite_as_list
      ! Runtime parameters of interest to dycore
      procedure, public :: waccmx_on
      procedure, public :: waccmx_option
      procedure, public :: update_thermodynamic_variables
   end type runtime_options

   type(runtime_options), public :: cam_runtime_opts

   public :: cam_set_runtime_opts

   ! Private data
   logical :: runtime_configured = .false.

CONTAINS

   pure character(len=CS) function physics_suite(self)
      class(runtime_options), intent(in) :: self

      physics_suite = trim(self%phys_suite)
   end function physics_suite

   pure character(len=CS) function get_dycore(self)
      class(runtime_options), intent(in) :: self

      get_dycore = trim(self%dycore)
   end function get_dycore

   subroutine set_dycore(self, dycore_in)
      class(runtime_options), intent(inout) :: self
      character(len=*),       intent(in)    :: dycore_in

      self%dycore = trim(dycore_in)

   end subroutine set_dycore

   pure function suite_as_list(self) result(slist)
      class(runtime_options), intent(in) :: self
      character(len=CS) :: slist(1)

      slist = (/ trim(self%phys_suite) /)
   end function suite_as_list

   pure logical function waccmx_on(self)
      class(runtime_options), intent(in) :: self

      waccmx_on = trim(self%waccmx_opt) /= unset_str

   end function waccmx_on

   pure character(len=16) function waccmx_option(self)
      class(runtime_options), intent(in) :: self

      waccmx_option = trim(self%waccmx_opt)

   end function waccmx_option

   pure logical function update_thermodynamic_variables(self)
      class(runtime_options), intent(in) :: self

      update_thermodynamic_variables = self%update_thermo_variables

   end function update_thermodynamic_variables

   subroutine cam_set_runtime_opts(phys_suite, waccmx_opt)
      use cam_abortutils, only: endrun

      ! Initialize the CAM runtime object
      character(len=CS), intent(in) :: phys_suite
      character(len=16), intent(in) :: waccmx_opt

      if (runtime_configured) then
         ! We might need more action to reset this so do not allow it now
         call endrun("CAM runtime DDT already configured")
      end if

      cam_runtime_opts%phys_suite = trim(phys_suite)
      cam_runtime_opts%waccmx_opt = trim(waccmx_opt)
      cam_runtime_opts%update_thermo_variables = (trim(waccmx_opt) == 'ionosphere' .or. &
            trim(waccmx_opt) == 'neutral')

      runtime_configured = .true.

   end subroutine cam_set_runtime_opts

end module runtime_obj
