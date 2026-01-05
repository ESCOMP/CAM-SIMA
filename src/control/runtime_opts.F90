module runtime_opts

   !-----------------------------------------------------------------------
   !
   ! Provide driver level routine for making calls to the namelist readers
   ! for the infrastructure and the dycore and physics parameterizations.
   !
   !-----------------------------------------------------------------------

   use shr_kind_mod,    only: r8=>shr_kind_r8

   implicit none
   private
   save

   public :: read_namelist

!=======================================================================
contains
!=======================================================================

   subroutine read_namelist(nlfilename, single_column, scmlat, scmlon)

      use spmd_utils,                only: mpicom, masterproc, masterprocid
      use cam_abortutils,            only: endrun
      use cam_logfile,               only: cam_logfile_readnl, iulog
      use cam_initfiles,             only: cam_initfiles_readnl
      use cam_constituents,          only: cam_constituents_readnl
      use cam_ccpp_scheme_namelists, only: cam_read_ccpp_scheme_namelists
      use runtime_obj,               only: cam_set_runtime_opts, unset_str
      use cam_ccpp_cap,              only: ccpp_physics_suite_schemes

!      use physics_grid,        only: physics_grid_readnl

      use cam_history,         only: history_readnl

!      use scamMod,             only: scam_readnl
      use physconst,           only: physconst_readnl
      use phys_comp,           only: phys_readnl, phys_suite_name
      use vert_coord,          only: vert_coord_readnl
      use ref_pres,            only: ref_pres_readnl
!      use phys_debug_util,     only: phys_debug_readnl

!      use cam_diagnostics,     only: diag_readnl
      use inic_analytic_utils, only: analytic_ic_readnl

      use tropopause_climo_read, only: tropopause_climo_readnl
      use radiation_namelist,    only: radiation_readnl
      use gravity_wave_drag_ridge_read, only: gravity_wave_drag_ridge_read_readnl

!      use tracers,             only: tracers_readnl
!      use nudging,             only: nudging_readnl

      use dyn_comp,            only: dyn_readnl
      !use ionosphere_interface,only: ionosphere_readnl

      !---------------------------Arguments-----------------------------------

      character(len=*), intent(in) :: nlfilename
      logical,          intent(in) :: single_column
      real(r8),         intent(in) :: scmlat
      real(r8),         intent(in) :: scmlon

      !---------------------------Local variables-----------------------------
      character(len=16)              :: waccmx_opt
      character(len=512)             :: errmsg
      character(len=64), allocatable :: schemes(:)
      integer                        :: errflg

      character(len=*), parameter    ::  subname = "read_namelist"

      ! Initialize system-wide runtime configuration variables
      waccmx_opt = unset_str

      !-----------------------------------------------------------------------
      ! Call subroutines for modules to read their own namelist.
      ! In some cases namelist default values may depend on settings from
      ! other modules, so there may be an order dependence in the following
      ! calls.
      ! ***N.B.*** In particular, physconst_readnl should be called before
      !            the other readnl methods in case that method is used to set
      !            physical constants, some of which are set at runtime
      !            by the physconst_readnl method.
      ! Modules that read their own namelist are responsible for making sure
      ! all processes receive the values.
      ! Note that namelists for physics schemes are read by
      !    cam_read_ccpp_scheme_namelists

      call cam_logfile_readnl(nlfilename)
!      call physics_grid_readnl(nlfilename)
      call physconst_readnl(nlfilename)
      call cam_initfiles_readnl(nlfilename)
      call cam_constituents_readnl(nlfilename)
      call history_readnl(nlfilename)
      call phys_readnl(nlfilename) ! Should set phys_suite_name
      call vert_coord_readnl(nlfilename)
      call ref_pres_readnl(nlfilename)
!      call phys_debug_readnl(nlfilename)
!      call diag_readnl(nlfilename)
!      call check_energy_readnl(nlfilename)
      call analytic_ic_readnl(nlfilename)
      call tropopause_climo_readnl(nlfilename)
      call radiation_readnl(nlfilename)
!      call scam_readnl(nlfilename, single_column, scmlat, scmlon)
!      call nudging_readnl(nlfilename)
      call gravity_wave_drag_ridge_read_readnl(nlfilename)
      call dyn_readnl(nlfilename)

      ! Read the namelists for active physics schemes
      errflg = 0
      call ccpp_physics_suite_schemes(phys_suite_name, schemes, errmsg, errflg)
      if (errflg /= 0) then
         call endrun(subname//"Error: "//trim(errmsg),                        &
              file=__FILE__, line=__LINE__-3)
      end if
      call cam_read_ccpp_scheme_namelists(nlfilename, schemes,                &
           mpicom, masterprocid, masterproc, iulog)

      ! Finally, set the system-wide runtime configuration object
      call cam_set_runtime_opts(phys_suite_name, waccmx_opt)

   end subroutine read_namelist

   !=======================================================================

end module runtime_opts
