module stepon

#ifdef scam
use scamMod,                only: use_iop, doiopupdate, single_column, &
                                  setiopupdate, readiopdata
use se_single_column_mod,   only: scm_setfield, iop_broadcast
#endif

implicit none
private

public stepon_init
public stepon_timestep_init
public stepon_run2
public stepon_run3
public stepon_final

!=========================================================================================
contains
!=========================================================================================

subroutine stepon_init(cam_runtime_opts, dyn_in, dyn_out)
   use cam_constituents,    only: num_constituents
   use cam_constituents,    only: const_longname
   use cam_constituents,    only: const_diag_name
   use runtime_obj,         only: runtime_options
   use cam_history,         only: history_add_field
   use cam_history_support, only: horiz_only

   ! SE/CAM interface:
   use dyn_comp,            only: dyn_import_t, dyn_export_t
   use dyn_comp,            only: cnst_diag_name_gll

   ! SE dycore:
   use dimensions_mod, only: fv_nphys, cnst_longname_gll, qsize

   ! Dummy arguments
   type(runtime_options), intent(in) :: cam_runtime_opts ! Runtime settings object
   type(dyn_import_t),    intent(in) :: dyn_in           ! Dynamics import container
   type(dyn_export_t),    intent(in) :: dyn_out          ! Dynamics export container

   ! local variables
   integer :: m, m_cnst

   !----------------------------------------------------------------------------
   ! These fields on dynamics grid are output before the call to d_p_coupling.
   do m_cnst = 1, qsize
     call history_add_field(trim(cnst_diag_name_gll(m_cnst))//'_gll',  trim(cnst_longname_gll(m_cnst)), &
         'lev', 'inst', 'kg kg-1', gridname='GLL')
     call history_add_field(trim(cnst_diag_name_gll(m_cnst))//'dp_gll', trim(cnst_longname_gll(m_cnst))//'*dp', &
         'lev', 'inst', 'kg kg-1', gridname='GLL')
   end do

   call history_add_field('U_gll'      ,'U wind on gll grid'         ,'lev'      , 'inst', 'm s-1 ',gridname='GLL')
   call history_add_field('V_gll'      ,'V wind on gll grid'         ,'lev'      , 'inst', 'm s-1 ',gridname='GLL')
   call history_add_field('T_gll'      ,'T on gll grid'              ,'lev'      , 'inst', 'K '    ,gridname='GLL')
   call history_add_field('dp_ref_gll' ,'dp dry / dp_ref on gll grid','lev'      , 'inst', '1'     ,gridname='GLL')
   call history_add_field('PSDRY_gll'  ,'psdry on gll grid'          ,horiz_only , 'inst', 'Pa '   ,gridname='GLL')
   call history_add_field('PS_gll'     ,'ps on gll grid'             ,horiz_only , 'inst', 'Pa '   ,gridname='GLL')
   call history_add_field('PHIS_gll'   ,'PHIS on gll grid'           ,horiz_only , 'inst', 'Pa '   ,gridname='GLL')

   ! Fields for initial condition files
   call history_add_field('U&IC',  'Zonal wind'      ,'lev' ,  'inst', 'm s-1', gridname='GLL')
   call history_add_field('V&IC',  'Meridional wind' ,'lev' ,  'inst', 'm s-1', gridname='GLL')

   ! Don't need to register U&IC V&IC as vector components since we don't interpolate IC files
   !Uncomment or delete once history output "groups" have been
   !implemented in CAM-SIMA -JN:
   !call add_default('U&IC',0, 'inst')
   !call add_default('V&IC',0, 'inst')

   call history_add_field('PS&IC', 'Surface pressure' ,horiz_only , 'inst', 'Pa', gridname='GLL')
   call history_add_field('T&IC',  'Temperature'      ,'lev'      , 'inst', 'K ', gridname='GLL')

   !Uncomment or delete once history output "groups" have been
   !implemented in CAM-SIMA -JN:
   !call add_default('PS&IC', 0, 'inst')
   !call add_default('T&IC',  0, 'inst')

   do m_cnst = 1, num_constituents
      call history_add_field(trim(const_diag_name(m_cnst))//'&IC', trim(const_longname(m_cnst)), &
                  'lev', 'inst', 'kg kg-1', gridname='GLL')

      !Uncomment or delete once history output "groups" have been
      !implemented in CAM-SIMA -JN:
      !call add_default(trim(const_diag_name(m_cnst))//'&IC', 0, 'inst')
   end do

end subroutine stepon_init

!=========================================================================================

subroutine stepon_timestep_init(dtime_out, cam_runtime_opts, phys_state,      &
     phys_tend, dyn_in, dyn_out)

   use shr_kind_mod,   only: r8=>shr_kind_r8
   use perf_mod,       only: t_startf, t_stopf, t_barrierf
   use runtime_obj,    only: runtime_options
   use physics_types,  only: physics_state, physics_tend
   use time_manager,   only: get_step_size
   use cam_abortutils, only: endrun
   use spmd_utils,     only: iam, mpicom

   ! SE/CAM interface:
   use dyn_comp,       only: dyn_import_t, dyn_export_t
   use dp_coupling,    only: d_p_coupling                ! dynamics-physics coupling

   ! SE dycore:
   use se_dyn_time_mod, only: tstep                      ! dynamics timestep
   use parallel_mod,    only: par

   ! Dummy arguments
   real(r8),              intent(out)   :: dtime_out        ! Time-step (s)
   type(runtime_options), intent(in)    :: cam_runtime_opts ! Runtime settings object
   type(physics_state),   intent(inout) :: phys_state       ! Physics state object
   type(physics_tend),    intent(inout) :: phys_tend        ! Physics tendency object
   type(dyn_import_t),    intent(inout) :: dyn_in           ! Dynamics import container
   type(dyn_export_t),    intent(inout) :: dyn_out          ! Dynamics export container
   !----------------------------------------------------------------------------

   !Extract model time step in seconds from ESMF time manager:
   dtime_out = get_step_size()

   !Ensure that the model and dynamics time-steps are positive values:
   if (iam < par%nprocs) then
      if (tstep <= 0)      call endrun('stepon_timestep_init: bad tstep')
      if (dtime_out <= 0)  call endrun('stepon_timestep_init: bad dtime')

      ! write diagnostic fields on gll grid and initial file
      call diag_dynvar_ic(dyn_out%elem, dyn_out%fvm)
   end if
#ifdef scam

   ! Determine whether it is time for an IOP update;
   ! doiopupdate set to true if model time step > next available IOP


   if (use_iop .and. masterproc) then
       call setiopupdate
   end if

   if (single_column) then

     ! If first restart step then ensure that IOP data is read
     if (is_first_restart_step()) then
        if (masterproc) call readiopdata( hvcoord%hyam, hvcoord%hybm, hvcoord%hyai, hvcoord%hybi, hvcoord%ps0  )
        call iop_broadcast()
     endif

     iop_update_phase1 = .true.
     if ((is_first_restart_step() .or. doiopupdate) .and. masterproc) then
        call readiopdata( hvcoord%hyam, hvcoord%hybm, hvcoord%hyai, hvcoord%hybi, hvcoord%ps0  )
     endif
     call iop_broadcast()

     call scm_setfield(dyn_out%elem,iop_update_phase1)
   endif
#endif
   ! Synchronize all PEs and then transfer dynamics variables to physics:
   call t_barrierf('sync_d_p_coupling', mpicom)
   call t_startf('d_p_coupling')
   ! Move data into phys_state structure.
   call d_p_coupling(cam_runtime_opts, phys_state, phys_tend, dyn_out)
   call t_stopf('d_p_coupling')
end subroutine stepon_timestep_init

!=========================================================================================

subroutine stepon_run2(cam_runtime_opts, phys_state, phys_tend, dyn_in, dyn_out)

   use runtime_obj,    only: runtime_options
   use physics_types,  only: physics_state, physics_tend
   use perf_mod,       only: t_startf, t_stopf, t_barrierf
   use spmd_utils,     only: mpicom, iam

   ! SE/CAM interface:
   use dp_coupling,    only: p_d_coupling
   use dyn_grid,       only: TimeLevel
   use dyn_comp,       only: dyn_import_t, dyn_export_t

   ! SE dycore:
   use se_dyn_time_mod,  only: TimeLevel_Qdp
   use control_mod,      only: qsplit
   use prim_advance_mod, only: tot_energy_dyn
   use dimensions_mod,   only: nelemd
   use parallel_mod,     only: par

   ! Dummy arguments
   type(runtime_options), intent(in)    :: cam_runtime_opts ! Runtime settings object
   type(physics_state),   intent(inout) :: phys_state       ! Physics state object
   type(physics_tend),    intent(inout) :: phys_tend        ! Physics tendency object
   type(dyn_import_t),    intent(inout) :: dyn_in           ! Dynamics import container
   type(dyn_export_t),    intent(inout) :: dyn_out          ! Dynamics export container

   ! Local variables
   integer :: tl_f, tl_fQdp

   !----------------------------------------------------------------------------

   !Determine appropriate time values:
   tl_f = TimeLevel%n0   ! timelevel which was adjusted by physics
   call TimeLevel_Qdp(TimeLevel, qsplit, tl_fQdp)
   ! Synchronize all PEs and then transfer physics variables to dynamics:
   call t_barrierf('sync_p_d_coupling', mpicom)
   call t_startf('p_d_coupling')
   ! copy from phys structures -> dynamics structures
   call p_d_coupling(cam_runtime_opts, phys_state, phys_tend, dyn_in, tl_f, tl_fQdp)
   call t_stopf('p_d_coupling')

   if (iam < par%nprocs) then
      call tot_energy_dyn(dyn_in%elem,dyn_in%fvm, 1, nelemd, tl_f, tl_fQdp,'dED')
   end if

end subroutine stepon_run2

!=========================================================================================

subroutine stepon_run3(dtime, cam_runtime_opts, cam_out, phys_state, dyn_in, dyn_out)

   use shr_kind_mod,   only: r8=>shr_kind_r8
   use runtime_obj,    only: runtime_options
   use physics_types,  only: cam_out_t, physics_state
   use perf_mod,       only: t_startf, t_stopf, t_barrierf
   use spmd_utils,     only: mpicom

   ! SE/CAM interface:
   use dyn_comp,       only: dyn_import_t, dyn_export_t
   use dyn_comp,       only: dyn_run
   use dyn_grid,       only: TimeLevel
#ifdef scam
   use advect_tend,    only: compute_write_iop_fields
#endif
   use advect_tend,    only: compute_adv_tends_xyz

   ! SE dycore:
   use se_dyn_time_mod,only: TimeLevel_Qdp
   use control_mod,    only: qsplit
   use dimensions_mod, only: nelemd

   ! Dummy arguments
   real(r8),              intent(in)    :: dtime            ! Time-step
   type(runtime_options), intent(in)    :: cam_runtime_opts ! Runtime settings object
   type(cam_out_t),       intent(inout) :: cam_out          ! Output from CAM to surface
   type(physics_state),   intent(inout) :: phys_state       ! Physics state object
   type(dyn_import_t),    intent(inout) :: dyn_in           ! Dynamics import container
   type(dyn_export_t),    intent(inout) :: dyn_out          ! Dynamics export container

   ! Local variables
   integer :: tl_f, tl_fQdp
   !--------------------------------------------------------------------------------------
#ifdef scam
   if (single_column) then
      ! Update IOP properties e.g. omega, divT, divQ
      iop_update_phase1 = .false.
      if (doiopupdate) then
         if (masterproc) call readiopdata( hvcoord%hyam, hvcoord%hybm, hvcoord%hyai, hvcoord%hybi, hvcoord%ps0  )
         call iop_broadcast()
         call scm_setfield(dyn_out%elem,iop_update_phase1)
      endif
   endif
#endif
   ! Determine appropriate time values and
   ! initalize advected constituent mixing ratios:
   call t_startf('comp_adv_tends1')
   tl_f = TimeLevel%n0
   call TimeLevel_Qdp(TimeLevel, qsplit, tl_fQdp)
   call compute_adv_tends_xyz(dyn_in%elem,dyn_in%fvm,1,nelemd,tl_fQdp,tl_f)
#ifdef scam
   if (write_camiop) call compute_write_iop_fields(dyn_in%elem,dyn_in%fvm,1,nelemd,tl_fQdp,tl_f)
#endif
   call t_stopf('comp_adv_tends1')

   ! Synchronize all PEs and then run dynamics (dyn_run):
   call t_barrierf('sync_dyn_run', mpicom)
   call t_startf('dyn_run')
   call dyn_run(dyn_out)
   call t_stopf('dyn_run')

   ! Determine appropriate time values and
   ! calculate constituent advection tendencies:
   call t_startf('comp_adv_tends2')
   tl_f = TimeLevel%n0
   call TimeLevel_Qdp(TimeLevel, qsplit, tl_fQdp)
   call compute_adv_tends_xyz(dyn_in%elem,dyn_in%fvm,1,nelemd,tl_fQdp,tl_f)
#ifdef scam
   if (write_camiop) call compute_write_iop_fields(dyn_in%elem,dyn_in%fvm,1,nelemd,tl_fQdp,tl_f)
#endif
   call t_stopf('comp_adv_tends2')

end subroutine stepon_run3

!=========================================================================================

subroutine stepon_final(cam_runtime_opts, dyn_in, dyn_out)

   use runtime_obj, only: runtime_options

   ! SE/CAM interface:
   use dyn_comp,    only: dyn_import_t, dyn_export_t

   ! SE dycore

   ! Dummy arguments
   type(runtime_options), intent(in)    :: cam_runtime_opts ! Runtime settings object
   type(dyn_import_t),    intent(inout) :: dyn_in           ! Dynamics import container
   type(dyn_export_t),    intent(inout) :: dyn_out          ! Dynamics export container

end subroutine stepon_final

!=========================================================================================

subroutine diag_dynvar_ic(elem, fvm)

   use shr_kind_mod,           only: r8=>shr_kind_r8, cl=>shr_kind_cl
   use cam_constituents,       only: const_is_wet
   use cam_constituents,       only: const_diag_name
   use cam_history,            only: is_history_field_active, history_out_field
   !use cam_history,           only: write_inithist
   use cam_history_support,    only: fieldname_len
   use air_composition,        only: thermodynamic_active_species_idx
   use air_composition,        only: thermodynamic_active_species_idx_dycore
   use dyn_thermo,             only: get_sum_species, get_ps, get_dp_ref
   use hycoef,                 only: hyai, hybi, ps0
   use cam_abortutils,         only: endrun, check_allocate

   ! SE/CAM interface:
   use dyn_grid,               only: TimeLevel
   use dyn_comp,               only: cnst_diag_name_gll

   ! SE dycore:
   use se_dyn_time_mod,        only: TimeLevel_Qdp   !  dynamics typestep
   use control_mod,            only: qsplit
   use hybrid_mod,             only: config_thread_region, get_loop_ranges
   use hybrid_mod,             only: hybrid_t
   use dimensions_mod,         only: np, npsq, nc, nhc, fv_nphys, qsize, ntrac, nlev
   use dimensions_mod,         only: nelemd
   use element_mod,            only: element_t
   use parallel_mod,           only: par
   use fvm_control_volume_mod, only: fvm_struct
   use fvm_mapping,            only: fvm2dyn

   ! Dummy arguments
   type(element_t) , intent(in)    :: elem(1:nelemd)
   type(fvm_struct), intent(inout) :: fvm(:)

   ! Local variables
   integer              :: ie, i, j, k, m, m_cnst, nq
   integer              :: tl_f, tl_qdp
   integer              :: iret
   character(len=cl)    :: errmsg

   character(len=fieldname_len) :: tfname

   type(hybrid_t)        :: hybrid
   integer               :: nets, nete
   real(r8), allocatable :: ftmp(:,:,:)
   real(r8), allocatable :: fld_fvm(:,:,:,:,:), fld_gll(:,:,:,:,:)
   real(r8), allocatable :: fld_2d(:,:)
   logical,  allocatable :: llimiter(:)
   real(r8)              :: qtmp(np,np,nlev), dp_ref(np,np,nlev), ps_ref(np,np)
   real(r8), allocatable :: factor_array(:,:,:)

   character(len=*), parameter :: subname = 'diag_dynvar_ic'
   !----------------------------------------------------------------------------

   tl_f = timelevel%n0
   call TimeLevel_Qdp(TimeLevel, qsplit, tl_Qdp)

   allocate(ftmp(npsq,nlev,2), stat=iret, errmsg=errmsg)
   call check_allocate(iret, subname, 'ftmp(npsq,nlev,2)', &
                       file=__FILE__, line=__LINE__, errmsg=errmsg)

   ! Output tracer fields for analysis of advection schemes
   do m_cnst = 1, qsize
     tfname = trim(cnst_diag_name_gll(m_cnst))//'_gll'
     if (is_history_field_active(tfname)) then
       do ie = 1, nelemd
         qtmp(:,:,:) =  elem(ie)%state%Qdp(:,:,:,m_cnst,tl_qdp)/&
              elem(ie)%state%dp3d(:,:,:,tl_f)
         do j = 1, np
           do i = 1, np
             ftmp(i+(j-1)*np,:,1) = elem(ie)%state%Qdp(i,j,:,m_cnst,tl_qdp)/&
                  elem(ie)%state%dp3d(i,j,:,tl_f)
           end do
         end do
         call history_out_field(tfname, ftmp(:,:,1))
       end do
     end if
   end do

   do m_cnst = 1, qsize
     tfname = trim(cnst_diag_name_gll(m_cnst))//'dp_gll'
     if (is_history_field_active(tfname)) then
       do ie = 1, nelemd
         do j = 1, np
           do i = 1, np
             ftmp(i+(j-1)*np,:,1) = elem(ie)%state%Qdp(i,j,:,m_cnst,tl_qdp)
           end do
         end do
         call history_out_field(tfname, ftmp(:,:,1))
       end do
     end if
   end do

   if (is_history_field_active('U_gll') .or. is_history_field_active('V_gll')) then
      do ie = 1, nelemd
         do j = 1, np
            do i = 1, np
               ftmp(i+(j-1)*np,:,1) = elem(ie)%state%v(i,j,1,:,tl_f)
               ftmp(i+(j-1)*np,:,2) = elem(ie)%state%v(i,j,2,:,tl_f)
            end do
         end do
         call history_out_field('U_gll', ftmp(:,:,1))
         call history_out_field('V_gll', ftmp(:,:,2))
      end do
   end if

   if (is_history_field_active('T_gll')) then
      do ie = 1, nelemd
         do j = 1, np
            do i = 1, np
               ftmp(i+(j-1)*np,:,1) = elem(ie)%state%T(i,j,:,tl_f)
            end do
         end do
         call history_out_field('T_gll', ftmp(:,:,1))
      end do
   end if

   if (is_history_field_active('dp_ref_gll')) then
     do ie = 1, nelemd
       call get_dp_ref(hyai,hybi,ps0,elem(ie)%state%phis(:,:),dp_ref(:,:,:),ps_ref(:,:))
         do j = 1, np
            do i = 1, np
               ftmp(i+(j-1)*np,:,1) = elem(ie)%state%dp3d(i,j,:,tl_f)/dp_ref(i,j,:)
            end do
         end do
         call history_out_field('dp_ref_gll', ftmp(:,:,1))
      end do
   end if

   if (is_history_field_active('PSDRY_gll')) then
      do ie = 1, nelemd
         do j = 1, np
            do i = 1, np
               ftmp(i+(j-1)*np,1,1) = elem(ie)%state%psdry(i,j)
            end do
         end do
         call history_out_field('PSDRY_gll', ftmp(:,1,1))
      end do
   end if

   if (is_history_field_active('PS_gll')) then
     allocate(fld_2d(np,np), stat=iret, errmsg=errmsg)
     call check_allocate(iret, subname, 'fld_2d(np, np)', &
                       file=__FILE__, line=__LINE__, errmsg=errmsg)

     do ie = 1, nelemd
       call get_ps(elem(ie)%state%Qdp(:,:,:,:,tl_Qdp),&
            thermodynamic_active_species_idx_dycore,elem(ie)%state%dp3d(:,:,:,tl_f),fld_2d,hyai(1)*ps0)
         do j = 1, np
            do i = 1, np
              ftmp(i+(j-1)*np,1,1) = fld_2d(i,j)
            end do
         end do
         call history_out_field('PS_gll', ftmp(:,1,1))

       end do
       deallocate(fld_2d)
   end if

   if (is_history_field_active('PHIS_gll')) then
      do ie = 1, nelemd
         call history_out_field('PHIS_gll', RESHAPE(elem(ie)%state%phis, (/np*np/)))
      end do
   end if

   !if (write_inithist()) then !Un-comment once IC file writing is available -JN
   if (.false.) then
     allocate(fld_2d(np,np), stat=iret, errmsg=errmsg)
     call check_allocate(iret, subname, 'fld_2d(np, np)', &
                       file=__FILE__, line=__LINE__, errmsg=errmsg)

     do ie = 1, nelemd
       call get_ps(elem(ie)%state%Qdp(:,:,:,:,tl_Qdp),&
            thermodynamic_active_species_idx_dycore,elem(ie)%state%dp3d(:,:,:,tl_f),fld_2d,hyai(1)*ps0)
       do j = 1, np
         do i = 1, np
           ftmp(i+(j-1)*np,1,1) = fld_2d(i,j)
         end do
       end do
       call history_out_field('PS&IC', ftmp(:,1,1))
     end do
     deallocate(fld_2d)
     if (fv_nphys < 1) then
        allocate(factor_array(np,np,nlev), stat=iret, errmsg=errmsg)
        call check_allocate(iret, subname, 'factor_array(np,np,nlev)', &
                            file=__FILE__, line=__LINE__, errmsg=errmsg)
     end if

     do ie = 1, nelemd

         call history_out_field('T&IC', RESHAPE(elem(ie)%state%T(:,:,:,tl_f),   (/npsq,nlev/)))
         call history_out_field('U&IC', RESHAPE(elem(ie)%state%v(:,:,1,:,tl_f), (/npsq,nlev/)))
         call history_out_field('V&IC', RESHAPE(elem(ie)%state%v(:,:,2,:,tl_f), (/npsq,nlev/)))

         if (fv_nphys < 1) then
            call get_sum_species(elem(ie)%state%Qdp(:,:,:,:,tl_qdp), &
               thermodynamic_active_species_idx_dycore, factor_array,dp_dry=elem(ie)%state%dp3d(:,:,:,tl_f))
            factor_array(:,:,:) = 1.0_r8/factor_array(:,:,:)
            do m_cnst = 1, qsize
               if (const_is_wet(m_cnst)) then
                  call history_out_field(trim(const_diag_name(m_cnst))//'&IC', &
                       RESHAPE(factor_array(:,:,:)*elem(ie)%state%Qdp(:,:,:,m_cnst,tl_qdp)/&
                       elem(ie)%state%dp3d(:,:,:,tl_f), (/npsq,nlev/)))
               else
                  call history_out_field(trim(const_diag_name(m_cnst))//'&IC', &
                       RESHAPE(elem(ie)%state%Qdp(:,:,:,m_cnst,tl_qdp)/&
                       elem(ie)%state%dp3d(:,:,:,tl_f), (/npsq,nlev/)))
               end if
            end do
         end if

      end do

      if (fv_nphys > 0) then
         !JMD $OMP PARALLEL NUM_THREADS(horz_num_threads), DEFAULT(SHARED), PRIVATE(hybrid,nets,nete,n)
         !JMD        hybrid = config_thread_region(par,'horizontal')
         hybrid = config_thread_region(par,'serial')
         call get_loop_ranges(hybrid, ibeg=nets, iend=nete)

         allocate(fld_fvm(1-nhc:nc+nhc,1-nhc:nc+nhc,nlev,ntrac,nets:nete), stat=iret, errmsg=errmsg)
         call check_allocate(iret, subname, &
                             'fld_fvm(1-nhc:nc+nhc,1-nhc:nc+nhc,nlev,ntrac,nets:nete)', &
                             file=__FILE__, line=__LINE__, errmsg=errmsg)

         allocate(fld_gll(np,np,nlev,ntrac,nets:nete), stat=iret, errmsg=errmsg)
         call check_allocate(iret, subname, &
                             'fld_gll(np,np,nlev,ntrac,nets:nete)', &
                             file=__FILE__, line=__LINE__, errmsg=errmsg)

         allocate(llimiter(ntrac), stat=iret, errmsg=errmsg)
         call check_allocate(iret, subname, 'llimiter(ntrac)', &
                             file=__FILE__, line=__LINE__, errmsg=errmsg)

         allocate(factor_array(nc,nc,nlev), stat=iret, errmsg=errmsg)
         call check_allocate(iret, subname, 'factor_array(nc,nc,nlev)', &
                             file=__FILE__, line=__LINE__, errmsg=errmsg)

         llimiter = .true.

         do ie = nets, nete
           call get_sum_species(fvm(ie)%c(1:nc,1:nc,:,:),thermodynamic_active_species_idx,factor_array)
           factor_array(:,:,:) = 1.0_r8/factor_array(:,:,:)
           do m_cnst = 1, ntrac
             if (const_is_wet(m_cnst)) then
               fld_fvm(1:nc,1:nc,:,m_cnst,ie) = fvm(ie)%c(1:nc,1:nc,:,m_cnst)*factor_array(:,:,:)
             else
               fld_fvm(1:nc,1:nc,:,m_cnst,ie) = fvm(ie)%c(1:nc,1:nc,:,m_cnst)
             end if
           end do
         end do

         call fvm2dyn(fld_fvm, fld_gll, hybrid, nets, nete, nlev, ntrac, fvm(nets:nete), llimiter)

         do ie = nets, nete
            do m_cnst = 1, ntrac
               call history_out_field(trim(const_diag_name(m_cnst))//'&IC', &
                    RESHAPE(fld_gll(:,:,:,m_cnst,ie), (/npsq,nlev/)))
            end do
         end do

         deallocate(fld_fvm)
         deallocate(fld_gll)
         deallocate(llimiter)
      end if
      deallocate(factor_array)
   end if  ! if (write_inithist)

   deallocate(ftmp)

end subroutine diag_dynvar_ic

!=========================================================================================

end module stepon
