module cam_comp
!-----------------------------------------------------------------------
!
! Community Atmosphere Model (CAM) component interfaces.
!
! This interface layer is CAM specific, i.e., it deals entirely with CAM
! specific data structures.  It is the layer above this, either atm_comp_mct
! or atm_comp_esmf, which translates between CAM and either MCT or ESMF
! data structures in order to interface with the driver/coupler.
!
!-----------------------------------------------------------------------

   use shr_kind_mod,              only: r8 => SHR_KIND_R8
   use shr_kind_mod,              only: cl=>SHR_KIND_CL, cs=>SHR_KIND_CS, cx=>SHR_KIND_CX
   use shr_sys_mod,               only: shr_sys_flush

   use spmd_utils,                only: masterproc, mpicom
   use cam_control_mod,           only: cam_ctrl_init, cam_ctrl_set_orbit
   use cam_physics_control,       only: cam_ctrl_set_physics_type
   use cam_control_mod,           only: caseid, ctitle
   use runtime_opts,              only: read_namelist
   use runtime_obj,               only: cam_runtime_opts
   use time_manager,              only: timemgr_init, get_step_size
   use time_manager,              only: get_nstep
   use time_manager,              only: is_first_step, is_first_restart_step
   use time_manager,              only: get_curr_calday

   use physics_types,             only: phys_state, phys_tend
   use physics_types,             only: dtime_phys
   use physics_types,             only: calday
   use physics_types,             only: is_first_timestep, nstep
   use dyn_comp,                  only: dyn_import_t, dyn_export_t

   use perf_mod,                  only: t_barrierf, t_startf, t_stopf
   use cam_logfile,               only: iulog
   use cam_abortutils,            only: endrun
   use ccpp_constituent_prop_mod, only: ccpp_constituent_properties_t

   implicit none
   private

   public cam_init           ! First phase of CAM initialization
   public cam_timestep_init  ! CAM timestep initialization
   public cam_run1           ! CAM run method phase 1
   public cam_run2           ! CAM run method phase 2
   public cam_run3           ! CAM run method phase 3
   public cam_run4           ! CAM run method phase 4
   public cam_timestep_final ! CAM timestep finalization
   public cam_final          ! CAM Finalization

   type(dyn_import_t) :: dyn_in   ! Dynamics import container
   type(dyn_export_t) :: dyn_out  ! Dynamics export container

   logical  :: BFB_CAM_SCAM_IOP = .false.

   ! Currently, the host (CAM-SIMA) adds only water vapor (specific humidity)
   ! as a constituent when not requested by the physics.  However, it is
   ! unclear if this is actually necessary.
   type(ccpp_constituent_properties_t), allocatable, target :: host_constituents(:)

   ! Private interface (here to avoid circular dependency)
   private :: cam_register_constituents


!-----------------------------------------------------------------------
CONTAINS
!-----------------------------------------------------------------------

   subroutine cam_init(caseid, ctitle, model_doi_url,                         &
        initial_run_in, restart_run_in, branch_run_in,                        &
        post_assim_in, calendar,                                              &
        brnch_retain_casename, aqua_planet,                                   &
        single_column, scmlat, scmlon,                                        &
        eccen, obliqr, lambm0, mvelpp,                                        &
        perpetual_run, perpetual_ymd,                                         &
        dtime, start_ymd, start_tod, ref_ymd, ref_tod,                        &
        stop_ymd, stop_tod, curr_ymd, curr_tod)

      !-----------------------------------------------------------------------
      !
      ! CAM component initialization.
      !
      !-----------------------------------------------------------------------

      use cam_initfiles,             only: cam_initfiles_open
      use dyn_grid,                  only: model_grid_init
      use phys_comp,                 only: phys_init, phys_suite_name
      use phys_comp,                 only: phys_register
      use dyn_comp,                  only: dyn_init
!      use cam_restart,               only: cam_read_restart
      use cam_history,               only: history_init_files
!      use history_scam,              only: scm_intht
      use cam_pio_utils,             only: init_pio_subsystem
      use cam_instance,              only: inst_suffix
!      use history_defaults,          only: initialize_iop_history
      use stepon,                    only: stepon_init
      use air_composition,           only: air_composition_init
      use cam_ccpp_cap,              only: cam_ccpp_initialize_constituents
      use cam_ccpp_cap,              only: cam_model_const_properties
      use physics_grid,              only: columns_on_task
      use vert_coord,                only: pver
      use phys_vars_init_check,      only: mark_as_initialized
      use tropopause_climo_read,     only: tropopause_climo_read_file
      use gravity_wave_drag_ridge_read, only: gravity_wave_drag_ridge_read_file
      use orbital_data,              only: orbital_data_init
      use ccpp_kinds,                only: kind_phys
      use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t
      use musica_ccpp_dependencies,  only: musica_ccpp_dependencies_init

      ! Arguments
      character(len=cl), intent(in) :: caseid                ! case ID
      character(len=cl), intent(in) :: ctitle                ! case title
      character(len=cl), intent(in) :: model_doi_url         ! CESM model DOI
      logical,           intent(in) :: initial_run_in        ! is inital run?
      logical,           intent(in) :: restart_run_in        ! is restart run?
      logical,           intent(in) :: branch_run_in         ! is branch run?
      logical,           intent(in) :: post_assim_in         ! true => resume mode
      character(len=cs), intent(in) :: calendar              ! Calendar type
      ! brnch_retain_casename is a flag to allow a branch to use the same
      ! caseid as the run being branched from.
      logical,           intent(in) :: brnch_retain_casename
      ! aqua_planet is a flag to run model in "aqua planet" mode
      logical,           intent(in) :: aqua_planet

      logical,           intent(in) :: single_column
      real(r8),          intent(in) :: scmlat
      real(r8),          intent(in) :: scmlon

      real(r8),          intent(in) :: eccen
      real(r8),          intent(in) :: obliqr
      real(r8),          intent(in) :: lambm0
      real(r8),          intent(in) :: mvelpp

      logical, intent(in)      :: perpetual_run ! true => perpetual mode enabled
      integer, intent(in)      :: perpetual_ymd ! Perpetual date (YYYYMMDD)
      integer, intent(in)      :: dtime         ! model timestep (sec)

      integer, intent(in)      :: start_ymd     ! Start date (YYYYMMDD)
      integer, intent(in)      :: start_tod     ! Start time of day (sec)
      integer, intent(in)      :: curr_ymd      ! Start date (YYYYMMDD)
      integer, intent(in)      :: curr_tod      ! Start time of day (sec)
      integer, intent(in)      :: stop_ymd      ! Stop date (YYYYMMDD)
      integer, intent(in)      :: stop_tod      ! Stop time of day (sec)
      integer, intent(in)      :: ref_ymd       ! Reference date (YYYYMMDD)
      integer, intent(in)      :: ref_tod       ! Reference time of day (sec)

      ! Local variables
      character(len=cs)        :: filein        ! Input namelist filename
      integer                  :: errflg
      character(len=cx)        :: errmsg

      type(ccpp_constituent_prop_ptr_t), pointer :: constituent_properties(:)
      !-----------------------------------------------------------------------

      call init_pio_subsystem()

      ! Initializations using data passed from coupler.
      call cam_ctrl_init(                                                     &
           caseid_in=caseid,                                                  &
           ctitle_in=ctitle,                                                  &
           initial_run_in=initial_run_in,                                     &
           restart_run_in=restart_run_in,                                     &
           branch_run_in=branch_run_in,                                       &
           post_assim_in=post_assim_in,                                       &
           aqua_planet_in=aqua_planet,                                        &
           brnch_retain_casename_in=brnch_retain_casename)

      call cam_ctrl_set_orbit(eccen, obliqr, lambm0, mvelpp)

      call timemgr_init(                                                      &
           dtime, calendar, start_ymd, start_tod, ref_ymd,                    &
           ref_tod, stop_ymd, stop_tod, curr_ymd, curr_tod,                   &
           perpetual_run, perpetual_ymd, initial_run_in)

      dtime_phys = 0.0_r8
      call mark_as_initialized('timestep_for_physics')

      is_first_timestep = .true.
      call mark_as_initialized('is_first_timestep')

      nstep = get_nstep()
      call mark_as_initialized('current_timestep_number')

      ! Get current fractional calendar day. Needs to be updated at every timestep.
      calday = get_curr_calday()
      call mark_as_initialized('fractional_calendar_days_on_end_of_current_timestep')

      ! Read CAM namelists.
      filein = "atm_in" // trim(inst_suffix)
      call read_namelist(filein, single_column, scmlat, scmlon)

      ! Determine if physics is "simple", which needs to be known by some dycores:
      call cam_ctrl_set_physics_type()

      ! Open initial or restart file, and topo file if specified.
      call cam_initfiles_open()

      ! Call CCPP physics register phase (must happen before
      !   cam_register_constituents)
      call phys_register()

      ! Initialize constituent information
      !    This will set the total number of constituents and the
      !    number of advected constituents.
      call cam_register_constituents(cam_runtime_opts)

      ! Initialize composition-dependent constants:
      call air_composition_init()

      ! Initialize model grids and decompositions
      call model_grid_init()

      ! Initialize constituent data
      call cam_ccpp_initialize_constituents(columns_on_task, pver, errflg, errmsg)

      ! Initialize ghg surface values before default initial distributions
      ! are set in dyn_init
      !!XXgoldyXX: This needs to be converted to CCPP and the issue of
      !            dyn vs. phys needs to be addressed
      !call chem_surfvals_init()

      ! initialize ionosphere
      !!XXgoldyXX: Leaving this place. Why before dyn_init?
      !call ionosphere_init()

      if (initial_run_in) then

         call dyn_init(cam_runtime_opts, dyn_in, dyn_out)

      else

!!XXgoldyXX: v need to import this
!         call cam_read_restart(cam_in, cam_out, dyn_in, dyn_out,              &
!              stop_ymd, stop_tod)
!!XXgoldyXX: ^ need to import this

!!XXgoldyXX: v need to import this
!         if (BFB_CAM_SCAM_IOP) then
!            call initialize_iop_history()
!         end if
!!XXgoldyXX: ^ need to import this
      end if

      ! Read tropopause climatology
      call tropopause_climo_read_file()

      ! Read gravity wave drag data for ridge parameterization
      call gravity_wave_drag_ridge_read_file()

      ! TEMPORARY:  Prescribe realistic but inaccurate physical quantities
      ! necessary for MUSICA that are currently unavailable in CAM-SIMA.
      !
      ! Remove this when MUSICA input data are available from CAM-SIMA or
      ! other physics schemes.
      constituent_properties => cam_model_const_properties()
      call musica_ccpp_dependencies_init(columns_on_task, pver, &
           constituent_properties, phys_suite_name)

      ! Initialize orbital data
      call orbital_data_init(columns_on_task)

      call phys_init()

!!XXgoldyXX: v need to import this
!      call bldfld ()  ! master field list (if branch, only does hash tables)
!!XXgoldyXX: ^ need to import this

      call stepon_init(cam_runtime_opts, dyn_in, dyn_out)

      ! if (single_column) then
      !    call scm_intht()
      ! end if
      call history_init_files(model_doi_url, caseid, ctitle)

   end subroutine cam_init

   !
   !-----------------------------------------------------------------------
   !
   subroutine cam_timestep_init()
      !-----------------------------------------------------------------------
      !
      ! Purpose:   Timestep init runs at the start of each timestep
      !
      !-----------------------------------------------------------------------

      use phys_comp,                 only: phys_timestep_init
      use physics_grid,              only: lat_rad, lon_rad
      use orbital_data,              only: orbital_data_advance
      use stepon,                    only: stepon_timestep_init
      use cam_ccpp_cap,              only: cam_constituents_array
      use cam_ccpp_cap,              only: cam_model_const_properties
      use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t
      use ccpp_kinds,                only: kind_phys
      use musica_ccpp_dependencies,  only: set_initial_musica_concentrations

      real(kind_phys), pointer :: constituents_array(:,:,:)
      type(ccpp_constituent_prop_ptr_t), pointer :: constituent_properties(:)

      ! Update current fractional calendar day. Needs to be updated at every timestep.
      calday = get_curr_calday()

      ! Update the orbital data
      call orbital_data_advance(calday, lat_rad, lon_rad)

      ! Update timestep flags in physics state
      is_first_timestep = is_first_step()
      nstep = get_nstep()

      !----------------------------------------------------------
      ! First phase of dynamics (at least couple from dynamics to physics)
      ! Return time-step for physics from dynamics.
      !----------------------------------------------------------
      call t_barrierf('sync_stepon_timestep_init', mpicom)
      call t_startf('stepon_timestep_init')
      call stepon_timestep_init(dtime_phys, cam_runtime_opts, phys_state, phys_tend,   &
           dyn_in, dyn_out)
      call t_stopf('stepon_timestep_init')

      !----------------------------------------------------------
      ! TEMPORARY:  Set initial MUSICA constituent values
      !
      !            This is a temporary workaround to initialize
      !            MUSICA constituent values until the file I/O
      !            capability is implemented.
      !            Remove this when MUSICA species are initialized
      !            by CAM-SIMA.
      !----------------------------------------------------------
      if (is_first_timestep) then
         constituents_array => cam_constituents_array()
         constituent_properties => cam_model_const_properties()
         call set_initial_musica_concentrations(constituents_array, &
              constituent_properties)
      end if

      !
      !----------------------------------------------------------
      ! PHYS_TIMESTEP_INIT Call the Physics package
      !----------------------------------------------------------
      !
      call phys_timestep_init()

   end subroutine cam_timestep_init
   !
   !-----------------------------------------------------------------------
   !
   subroutine cam_run1()
      !-----------------------------------------------------------------------
      !
      ! Purpose:   First phase of atmosphere model run method.
      !            Runs first phase of dynamics and first phase of
      !            physics (before surface model updates).
      !
      !-----------------------------------------------------------------------

      use phys_comp, only: phys_run1
!      use ionosphere_interface, only: ionosphere_run1

      !----------------------------------------------------------
      ! first phase of ionosphere -- write to IC file if needed
      !----------------------------------------------------------
      !call ionosphere_run1()

      !
      !----------------------------------------------------------
      ! PHYS_RUN Call the Physics package
      !----------------------------------------------------------
      !
      call t_barrierf('sync_phys_run1', mpicom)
      call t_startf('phys_run1')
      call phys_run1()
      call t_stopf('phys_run1')

   end subroutine cam_run1

   !
   !-----------------------------------------------------------------------
   !

   subroutine cam_run2()
      !-----------------------------------------------------------------------
      !
      ! Purpose:   Second phase of atmosphere model run method.
      !            Run the second phase physics, run methods that
      !            require the surface model updates.  And run the
      !            second phase of dynamics that at least couples
      !            between physics to dynamics.
      !
      !-----------------------------------------------------------------------

      use phys_comp, only: phys_run2
      use stepon,    only: stepon_run2
!      use ionosphere_interface, only: ionosphere_run2

      !
      ! Second phase of physics (after surface model update)
      !
      call t_barrierf('sync_phys_run2', mpicom)
      call t_startf('phys_run2')
      call phys_run2()
      call t_stopf('phys_run2')

      !
      ! Second phase of dynamics (at least couple from physics to dynamics)
      !
      call t_barrierf('sync_stepon_run2', mpicom)
      call t_startf('stepon_run2')
      call stepon_run2(cam_runtime_opts, phys_state, phys_tend, dyn_in, dyn_out)
      call t_stopf('stepon_run2')

      !
      ! Ion transport
      !
!      call t_startf('ionosphere_run2')
!      call ionosphere_run2( phys_state, dyn_in)
!      call t_stopf ('ionosphere_run2')

   end subroutine cam_run2

   !
   !-----------------------------------------------------------------------
   !

   subroutine cam_run3()
      !-----------------------------------------------------------------------
      !
      ! Purpose:  Third phase of atmosphere model run method. This consists
      !           of the third phase of the dynamics. For some dycores
      !           this will be the actual dynamics run, for others the
      !           dynamics happens before physics in phase 1.
      !
      !-----------------------------------------------------------------------
      use stepon, only: stepon_run3
      use physics_types, only: cam_out          ! Output from CAM to surface

      !-----------------------------------------------------------------------

      !
      ! Third phase of dynamics
      !
      call t_barrierf('sync_stepon_run3', mpicom)
      call t_startf('stepon_run3')
      call stepon_run3(dtime_phys, cam_runtime_opts, cam_out, phys_state,     &
           dyn_in, dyn_out)
      call t_stopf ('stepon_run3')

   end subroutine cam_run3

   !
   !-----------------------------------------------------------------------
   !

   subroutine cam_run4(rstwr, nlend,                         &
        yr_spec, mon_spec, day_spec, sec_spec)

      !-----------------------------------------------------------------------
      !
      ! Purpose:  Final phase of atmosphere model run method. This consists
      !           of all the restart output, history writes, and other
      !           file output.
      !
      !-----------------------------------------------------------------------
!      use cam_restart,  only: cam_write_restart
!      use qneg_module,  only: qneg_print_summary

      logical,         intent(in)           :: rstwr    ! write restart file
      logical,         intent(in)           :: nlend    ! this is final timestep
      integer,         intent(in), optional :: yr_spec  ! Simulation year
      integer,         intent(in), optional :: mon_spec ! Simulation month
      integer,         intent(in), optional :: day_spec ! Simulation day
      integer,         intent(in), optional :: sec_spec ! Secs in current simulation day

      !
      ! Write restart files
      !
      if (rstwr) then
         call t_startf('cam_write_restart')
         if (present(yr_spec) .and. present(mon_spec) .and.                   &
              present(day_spec).and.present(sec_spec)) then
!!XXgoldyXX: v need to import this
!            call cam_write_restart(cam_in, cam_out, dyn_out, yr_spec=yr_spec, &
!                 mon_spec=mon_spec, day_spec=day_spec, sec_spec= sec_spec)
!!XXgoldyXX: ^ need to import this
         else
!!XXgoldyXX: v need to import this
!            call cam_write_restart(cam_in, cam_out, dyn_out)
!!XXgoldyXX: ^ need to import this
         end if
         call t_stopf('cam_write_restart')
      end if

   end subroutine cam_run4

   !
   !-----------------------------------------------------------------------
   !
   subroutine cam_timestep_final(rstwr, nlend, do_ncdata_check, do_history_write)
      !-----------------------------------------------------------------------
      !
      ! Purpose:   Timestep final runs at the end of each timestep
      !
      !-----------------------------------------------------------------------

      use phys_comp,    only: phys_timestep_final
      use cam_history,  only: history_write_files
      use cam_history,  only: history_wrap_up
      logical, intent(in)  :: rstwr    ! write restart file
      logical, intent(in)  :: nlend    ! this is final timestep
      !Flag for whether a snapshot (ncdata) check should be run or not
      ! - flag is true if this is not the first or last step
      logical, intent(in)  :: do_ncdata_check
      !Flag for whether to perform the history write
      logical, optional, intent(in) :: do_history_write

      logical :: history_write_loc

      if (present(do_history_write)) then
         history_write_loc = do_history_write
      else
         history_write_loc = .true.
      end if

      if (history_write_loc) then
         call history_write_files()
      end if
      ! peverwhee - todo: handle restarts
      call history_wrap_up(rstwr, nlend)

      !
      !----------------------------------------------------------
      ! PHYS_TIMESTEP_FINAL Call the Physics package
      !----------------------------------------------------------
      !
      call phys_timestep_final(do_ncdata_check)
      call shr_sys_flush(iulog)

   end subroutine cam_timestep_final

   !
   !-----------------------------------------------------------------------
   !

   subroutine cam_final()
      !-----------------------------------------------------------------------
      !
      ! Purpose:  CAM finalization.
      !
      !-----------------------------------------------------------------------
      use stepon,               only: stepon_final
      use phys_comp,            only: phys_final
      use cam_initfiles,        only: cam_initfiles_close
!      use ionosphere_interface, only: ionosphere_final
      use cam_control_mod,      only: initial_run

      !-----------------------------------------------------------------------

      call phys_final()
      call stepon_final(cam_runtime_opts, dyn_in, dyn_out)
!      call ionosphere_final()

      if (initial_run) then
         call cam_initfiles_close()
      end if

      ! This flush attempts to ensure that asynchronous diagnostic prints
      !   from all processes do not get mixed up with the "END OF MODEL RUN"
      !   message printed by masterproc below.  The test-model script
      !   searches for this message in the output log to figure out if
      !   CAM completed successfully.
      call shr_sys_flush( 0 )       ! Flush all output to standard error
      call shr_sys_flush( iulog )   ! Flush all output to the CAM log file

      if (masterproc) then
         write(iulog,9300) nstep-1,nstep
9300     format (//'Number of completed timesteps:',i6,/,'Time step ',i6,     &
              ' partially done to provide convectively adjusted and ',        &
              'time filtered values for history tape.')
         write(iulog,*)' '
         write(iulog,*)'******* END OF MODEL RUN *******'
      end if

   end subroutine cam_final

!-----------------------------------------------------------------------

   subroutine cam_register_constituents(cam_runtime_opts)
      ! Call the CCPP interface to register all constituents for the
      ! physics suite being invoked during this run.
      use cam_abortutils,            only: endrun, check_allocate
      use runtime_obj,               only: runtime_options
      use runtime_obj,               only: wv_stdname
      use phys_comp,                 only: phys_suite_name
      use cam_constituents,          only: cam_constituents_init
      use cam_constituents,          only: const_set_qmin, const_get_index
      use ccpp_kinds,                only: kind_phys
      use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t
      use cam_ccpp_cap,              only: cam_ccpp_register_constituents
      use cam_ccpp_cap,              only: cam_ccpp_number_constituents
      use cam_ccpp_cap,              only: cam_model_const_properties
      use cam_ccpp_cap,              only: cam_ccpp_is_scheme_constituent

      ! Dummy arguments
      type(runtime_options), intent(in) :: cam_runtime_opts
      ! Local variables
      logical                                        :: is_constituent
      integer                                        :: num_advect
      integer                                        :: const_idx
      integer                                        :: errflg
      character(len=512)                             :: errmsg
      type(ccpp_constituent_prop_ptr_t), pointer     :: const_props(:)
      character(len=*), parameter :: subname = 'cam_register_constituents: '

      ! Initalize error flag and message:
      errflg = 0
      errmsg = ''

      ! Check if water vapor is already marked as a constituent by the
      ! physics:
      call cam_ccpp_is_scheme_constituent(wv_stdname, is_constituent, errflg, errmsg)

      if (errflg /= 0) then
         call endrun(subname//trim(errmsg), file=__FILE__, line=__LINE__)
      end if

      !If not requested by the physics, then add water vapor to the
      !constituents object:
      !-------------------------------------------
      if (.not. is_constituent) then

         ! Allocate host_constituents object:
         allocate(host_constituents(1), stat=errflg, errmsg=errmsg)
         call check_allocate(errflg, subname, 'host_constituents(1)',                   &
                             file=__FILE__, line=__LINE__, errmsg=errmsg)

         ! Register the constituents so they can be advected:
         call host_constituents(1)%instantiate( &
              std_name=wv_stdname,              &
              long_name="water vapor mixing ratio w.r.t moist air and condensed_water", &
              units="kg kg-1",                                                          &
              default_value=0._kind_phys,                                               &
              vertical_dim="vertical_layer_dimension",                                  &
              advected=.true.,                                                          &
           errcode=errflg, errmsg=errmsg)

         if (errflg /= 0) then
            call endrun(subname//trim(errmsg), file=__FILE__, line=__LINE__)
         end if
      else
         ! Allocate zero-size object so nothing is added
         ! to main constituents object:
         allocate(host_constituents(0), stat=errflg, errmsg=errmsg)
         call check_allocate(errflg, subname, 'host_constituents(0)',                   &
                             file=__FILE__, line=__LINE__, errmsg=errmsg)
      end if
      !-------------------------------------------

      !Combine host and physics constituents into a single
      !constituents object:
      call cam_ccpp_register_constituents(             &
           host_constituents, errcode=errflg, errmsg=errmsg)

      if (errflg /= 0) then
         call endrun(subname//trim(errmsg), file=__FILE__, line=__LINE__)
      end if

      !Determine total number of advected constituents:
      call cam_ccpp_number_constituents(num_advect, advected=.true.,                    &
           errcode=errflg, errmsg=errmsg)

      if (errflg /= 0) then
         call endrun(subname//trim(errmsg), file=__FILE__, line=__LINE__)
      end if

      ! Grab a pointer to the constituent array
      const_props => cam_model_const_properties()

      ! Initialize the constituents module
      call cam_constituents_init(const_props, num_advect)

      ! Finally, for CAM moist physics, one will need to use a
      ! non-zero minimum value for water vapor, so update that
      ! value here:
      !-------------------------------------------
      if (phys_suite_name /= 'held_suarez_1994') then !Held-Suarez is "dry" physics

         ! Get constituent index for water vapor:
         call const_get_index(wv_stdname, const_idx)

         ! Set new minimum value:
         call const_set_qmin(const_idx, 1.E-12_kind_phys)
      end if
      !-------------------------------------------


   end subroutine cam_register_constituents

!-----------------------------------------------------------------------

end module cam_comp
