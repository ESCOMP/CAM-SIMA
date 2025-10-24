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

   character(len=cl), protected :: caseid = ''  ! case ID
   character(len=cl), protected :: ctitle = ''  ! case title

   logical, protected :: initial_run  ! startup mode which only requires a minimal initial file
   logical, protected :: restart_run  ! continue a previous run; requires a restart file
   logical, protected :: branch_run   ! branch from a previous run; requires a restart file
   logical, protected :: post_assim   ! We are resuming after a pause

   logical, protected :: brnch_retain_casename ! true => branch run may use same caseid as
                                               !         the run being branched from

   !> \section arg_table_cam_control_mod  Argument Table
   !! \htmlinclude arg_table_cam_control_mod.html
   logical,  protected :: aqua_planet ! Flag to run model in "aqua planet" mode
   real(r8), protected :: eccen       ! Earth's eccentricity factor (unitless) (typically 0 to 0.1)
   real(r8), protected :: obliqr      ! Earth's obliquity in radians
   real(r8), protected :: lambm0      ! Mean longitude of perihelion at the
   ! vernal equinox (radians)
   real(r8), protected :: mvelpp      ! Earth's moving vernal equinox longitude
   ! of perihelion plus pi (radians)

!==============================================================================
CONTAINS
!==============================================================================

   subroutine cam_ctrl_init(caseid_in, ctitle_in, initial_run_in,             &
        restart_run_in, branch_run_in, post_assim_in,                         &
        aqua_planet_in,  brnch_retain_casename_in)

      character(len=cl), intent(in) :: caseid_in            ! case ID
      character(len=cl), intent(in) :: ctitle_in            ! case title
      logical,           intent(in) :: initial_run_in       ! true => inital run
      logical,           intent(in) :: restart_run_in       ! true => restart run
      logical,           intent(in) :: branch_run_in        ! true => branch run
      logical,           intent(in) :: post_assim_in        ! true => resume mode
      logical,           intent(in) :: aqua_planet_in       ! Flag to run model in "aqua planet" mode
      logical,           intent(in) :: brnch_retain_casename_in ! Flag to allow a branch to use the same
      ! caseid as the run being branched from.

      character(len=*), parameter :: sub='cam_ctrl_init'
      !------------------------------------------------------------------------

      caseid = caseid_in
      ctitle = ctitle_in

      initial_run = initial_run_in
      restart_run = restart_run_in
      branch_run  = branch_run_in
      post_assim  = post_assim_in

      aqua_planet = aqua_planet_in

      brnch_retain_casename = brnch_retain_casename_in

      if (masterproc) then
         write(iulog,*)' '
         write(iulog,*)' ------------------------------------------'
         write(iulog,*)' *********** CAM LOG OUTPUT ***************'
         write(iulog,*)' ------------------------------------------'
         if (restart_run) then
            write(iulog,*) '  Restart of an earlier run'
         else if (branch_run) then
            write(iulog,*) '  Branch of an earlier run'
         else if (post_assim) then
            write(iulog,*) '  DART run using CAM initial mode'
         else
            write(iulog,*) '         Initial run'
         end if
         write(iulog,*) ' ********** CASE = ',trim(caseid),' **********'
         write(iulog,'(1x,a)') ctitle


         if (aqua_planet) write(iulog,*) 'Run model in "AQUA_PLANET" mode'

      end if

   end subroutine cam_ctrl_init

   !---------------------------------------------------------------------------

   subroutine cam_ctrl_set_orbit(eccen_in, obliqr_in, lambm0_in, mvelpp_in)
      use phys_vars_init_check, only: mark_as_initialized

      real(r8), intent(in) :: eccen_in
      real(r8), intent(in) :: obliqr_in
      real(r8), intent(in) :: lambm0_in
      real(r8), intent(in) :: mvelpp_in

      eccen  = eccen_in
      obliqr = obliqr_in
      lambm0 = lambm0_in
      mvelpp = mvelpp_in

      call mark_as_initialized('planet_orbital_eccentricity_factor')

   end subroutine cam_ctrl_set_orbit

end module cam_control_mod
