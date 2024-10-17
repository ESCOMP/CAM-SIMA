module cam_physics_control
!------------------------------------------------------------------------------
!
! High level physics control variables.  Information received from the
! driver/coupler is stored here.
!
!------------------------------------------------------------------------------

   use spmd_utils,       only: masterproc
   use cam_logfile,      only: iulog
   use cam_abortutils,   only: endrun
   use cam_control_mod,  only: aqua_planet

   implicit none
   public
   save

   ! Public Routines:
   !
   !   cam_ctrl_set_physics_type

   logical, protected :: simple_phys       ! true => adiabatic or ideal_phys or kessler_phys
                                           !         or tj2016 or grayrad


!==============================================================================
CONTAINS
!==============================================================================

   subroutine cam_ctrl_set_physics_type()

      use shr_kind_mod, only: SHR_KIND_CS
      use cam_ccpp_cap, only: ccpp_physics_suite_list

      ! Local variables:

      ! suite_names: List of CCPP suites
      character(len=SHR_KIND_CS), allocatable :: suite_names(:)
      ! suite_name: CCPP suite we are running
      character(len=SHR_KIND_CS)              :: suite_name
      logical                                 :: adiabatic
      logical                                 :: ideal_phys
      logical                                 :: kessler_phys
      logical                                 :: tj2016_phys
      logical                                 :: grayrad_phys
      logical                                 :: moist_physics

      character(len=*), parameter :: subname = 'cam_ctrl_set_physics_type'

      !Determine CCPP physics suite names:
      call ccpp_physics_suite_list(suite_names)
      suite_name = suite_names(1)

      adiabatic = trim(suite_name) == 'adiabatic'
      ideal_phys = trim(suite_name) == 'held_suarez_1994'
      kessler_phys = trim(suite_name) == 'kessler'
      tj2016_phys = trim(suite_name) == 'tj2016'
      grayrad_phys = trim(suite_name) == 'grayrad'

      simple_phys = adiabatic .or. ideal_phys .or. kessler_phys .or. tj2016_phys .or.  grayrad_phys

      moist_physics = .not. (adiabatic .or. ideal_phys)

      if ((.not. moist_physics) .and. aqua_planet) then
         call endrun (subname//': FATAL: AQUA_PLANET not compatible with dry physics package, ('//trim(suite_name)//')')
      end if

      if (masterproc) then
         if (adiabatic) then
            write(iulog,*) 'Run model ADIABATICALLY (i.e. no physics)'
            write(iulog,*) '  Global energy fixer is on for dycores.'
         else if (ideal_phys) then
            write(iulog,*) 'Run model with Held-Suarez physics forcing'
         else if (kessler_phys) then
            write(iulog,*) 'Run model with Kessler warm-rain physics forcing'
         else if (tj2016_phys) then
            write(iulog,*) 'Run model with Thatcher-Jablonowski (2016) physics forcing (moist Held-Suarez)'
         else if (grayrad_phys) then
            write(iulog,*) 'Run model with Frierson (2006) gray radiation physics'
         end if
      end if

   end subroutine cam_ctrl_set_physics_type

end module cam_physics_control
