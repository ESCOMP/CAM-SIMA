module dp_coupling

!-------------------------------------------------------------------------------
! dynamics - physics coupling module
!-------------------------------------------------------------------------------

use shr_kind_mod,     only: r8=>shr_kind_r8
use ccpp_kinds,       only: kind_phys
use cam_constituents, only: const_is_wet, num_advected

use spmd_dyn,         only: local_dp_map
use spmd_utils,       only: iam
use dyn_grid,         only: TimeLevel, edgebuf
use dyn_comp,         only: dyn_export_t, dyn_import_t

use runtime_obj,      only: runtime_options
use physics_types,    only: physics_state, physics_tend
use physics_grid,     only: pcols => columns_on_task, get_dyn_col_p
use vert_coord,       only: pver, pverp

use dp_mapping,       only: nphys_pts

use perf_mod,         only: t_startf, t_stopf, t_barrierf
use cam_abortutils,   only: endrun, check_allocate

!SE dycore:
use parallel_mod,     only: par
use thread_mod,       only: horz_num_threads, max_num_threads
use hybrid_mod,       only: config_thread_region, get_loop_ranges, hybrid_t
use dimensions_mod,   only: np, npsq, nelemd, nlev, nc, qsize, ntrac, fv_nphys

use dof_mod,          only: UniquePoints, PutUniquePoints
use element_mod,      only: element_t

implicit none
private
save

public :: d_p_coupling, p_d_coupling

real(r8), allocatable :: q_prev(:,:,:) ! Previous Q for computing tendencies

real(kind_phys), allocatable :: qmin_vals(:) !Consitutent minimum values array

!=========================================================================================
CONTAINS
!=========================================================================================

subroutine d_p_coupling(cam_runtime_opts, phys_state, phys_tend, dyn_out)

   ! Convert the dynamics output state into the physics input state.
   ! Note that all pressures and tracer mixing ratios coming from the dycore are based on
   ! dry air mass.

   use gravity_waves_sources,     only: gws_src_fnct
   use hycoef,                    only: hyai, ps0
   use test_fvm_mapping,          only: test_mapping_overwrite_dyn_state, test_mapping_output_phys_state
   use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t
   use cam_ccpp_cap,              only: cam_constituents_array
   use cam_constituents,          only: const_name

   !SE dycore:
   use fvm_mapping,            only: dyn2phys_vector, dyn2phys_all_vars
   use time_mod,               only: timelevel_qdp
   use control_mod,            only: qsplit

   ! arguments
   type(runtime_options), intent(in)    :: cam_runtime_opts ! Runtime settings object
   type(dyn_export_t),    intent(inout) :: dyn_out          ! dynamics export
   type(physics_state),   intent(inout) :: phys_state
   type(physics_tend ),   intent(inout) :: phys_tend


   ! LOCAL VARIABLES
   type(element_t), pointer     :: elem(:)      ! pointer to dyn_out element array
   integer                      :: ie           ! indices over elements
   integer                      :: icol, ilyr   ! indices over columns, layers

   real(r8),  allocatable :: ps_tmp(:,:)         ! temp array to hold ps
   real(r8),  allocatable :: dp3d_tmp(:,:,:)     ! temp array to hold dp3d
   real(r8),  allocatable :: dp3d_tmp_tmp(:,:)
   real(r8),  allocatable :: phis_tmp(:,:)       ! temp array to hold phis
   real(r8),  allocatable :: T_tmp(:,:,:)        ! temp array to hold T
   real(r8),  allocatable :: uv_tmp(:,:,:,:)     ! temp array to hold u and v
   real(r8),  allocatable :: q_tmp(:,:,:,:)      ! temp to hold advected constituents
   real(r8),  allocatable :: omega_tmp(:,:,:)    ! temp array to hold omega
   real(kind=kind_phys),  pointer :: const_data_ptr(:,:,:) ! pointer to constituent array

   type(ccpp_constituent_prop_ptr_t), pointer :: cprops_ptr(:) !pointer to constituent properties

   ! Frontogenesis
   real (kind=r8),  allocatable :: frontgf(:,:,:)     ! temp arrays to hold frontogenesis
   real (kind=r8),  allocatable :: frontga(:,:,:)     ! function (frontgf) and angle (frontga)

   integer              :: ncols,ierr
   integer              :: blk_ind(1), m, m_cnst
   integer              :: nphys

   real(r8), allocatable :: qgll(:,:,:,:)
   real(r8)              :: inv_dp3d(np,np,nlev)
   integer               :: tl_f, tl_qdp_np0, tl_qdp_np1

   character(len=*), parameter :: subname = 'd_p_coupling'
   character(len=200) :: stdname_test

   !----------------------------------------------------------------------------

   if (.not. local_dp_map) then
      call endrun('d_p_coupling: Weak scaling does not support load balancing')
   end if

   elem => dyn_out%elem
   tl_f = TimeLevel%n0
   call TimeLevel_Qdp(TimeLevel, qsplit, tl_qdp_np0,tl_qdp_np1)

   if (fv_nphys > 0) then
      nphys = fv_nphys
   else
     allocate(qgll(np,np,nlev,num_advected), stat=ierr)
     call check_allocate(ierr, subname, 'qgll(np,np,nlev,num_advected)', &
                         file=__FILE__, line=__LINE__)

     nphys = np
   end if

   const_data_ptr => cam_constituents_array()

   ! Allocate temporary arrays to hold data for physics decomposition
   allocate(ps_tmp(nphys_pts,nelemd), stat=ierr)
   call check_allocate(ierr, subname, 'ps_tmp(nphys_pts,nelemd)', &
                       file=__FILE__, line=__LINE__)

   allocate(dp3d_tmp(nphys_pts,pver,nelemd), stat=ierr)
   call check_allocate(ierr, subname, 'dp3d_tmp(nphys_pts,pver,nelemd)', &
                       file=__FILE__, line=__LINE__)

   allocate(dp3d_tmp_tmp(nphys_pts,pver), stat=ierr)
   call check_allocate(ierr, subname, 'dp3d_tmp_tmp(nphys_pts,pver)', &
                       file=__FILE__, line=__LINE__)

   allocate(phis_tmp(nphys_pts,nelemd), stat=ierr)
   call check_allocate(ierr, subname, 'phis_tmp(nphys_pts,nelemd)', &
                       file=__FILE__, line=__LINE__)

   allocate(T_tmp(nphys_pts,pver,nelemd), stat=ierr)
   call check_allocate(ierr, subname, 'T_tmp(nphys_pts,pver,nelemd)', &
                       file=__FILE__, line=__LINE__)

   allocate(uv_tmp(nphys_pts,2,pver,nelemd), stat=ierr)
   call check_allocate(ierr, subname, 'uv_tmp(nphys_pts,2,pver,nelemd)', &
                       file=__FILE__, line=__LINE__)

   allocate(q_tmp(nphys_pts,pver,num_advected,nelemd), stat=ierr)
   call check_allocate(ierr, subname, 'q_tmp(nphys_pts,pver,num_advected,nelemd)', &
                       file=__FILE__, line=__LINE__)

   allocate(omega_tmp(nphys_pts,pver,nelemd), stat=ierr)
   call check_allocate(ierr, subname, 'omega_tmp(nphys_pts,pver,nelemd)', &
                       file=__FILE__, line=__LINE__)

   if (cam_runtime_opts%gw_front() .or. &
       cam_runtime_opts%gw_front_igw()) then

      allocate(frontgf(nphys_pts,pver,nelemd), stat=ierr)
      call check_allocate(ierr, subname, 'frontgf(nphys_pts,pver,nelemd)', &
                          file=__FILE__, line=__LINE__)

      allocate(frontga(nphys_pts,pver,nelemd), stat=ierr)
      call check_allocate(ierr, subname, 'frontga(nphys_pts,pver,nelemd)', &
                          file=__FILE__, line=__LINE__)
   end if

   if (iam < par%nprocs) then

      ! Gravity Waves
      if (cam_runtime_opts%gw_front() .or. &
          cam_runtime_opts%gw_front_igw()) then

         ! Calculate frontogenesis function and angle
         call gws_src_fnct(elem, tl_f, tl_qdp_np0, frontgf, frontga, nphys)

      end if

      if (fv_nphys > 0) then
         call test_mapping_overwrite_dyn_state(elem,dyn_out%fvm)
         !******************************************************************
         ! physics runs on an FVM grid: map GLL vars to physics grid
         !******************************************************************
         call t_startf('dyn2phys')
         ! note that the fvm halo has been filled in prim_run_subcycle
         ! if physics grid resolution is not equal to fvm resolution
         call dyn2phys_all_vars(1,nelemd,elem, dyn_out%fvm,&
              num_advected,hyai(1)*ps0,tl_f,               &
              ! output
              dp3d_tmp, ps_tmp, q_tmp, T_tmp,              &
              omega_tmp, phis_tmp                          &
              )
         do ie = 1, nelemd
            uv_tmp(:,:,:,ie) = &
               dyn2phys_vector(elem(ie)%state%v(:,:,:,:,tl_f),elem(ie))
         end do
         call t_stopf('dyn2phys')
      else

         !******************************************************************
         ! Physics runs on GLL grid: collect unique points before mapping to
         ! physics decomposition
         !******************************************************************

         if (qsize < num_advected) then
            call endrun('d_p_coupling: Fewer GLL tracers advected than required')
         end if

         call t_startf('UniquePoints')
         do ie = 1, nelemd
           inv_dp3d(:,:,:) = 1.0_r8/elem(ie)%state%dp3d(:,:,:,tl_f)
           do m = 1, num_advected
             qgll(:,:,:,m) = elem(ie)%state%Qdp(:,:,:,m,tl_qdp_np0)*inv_dp3d(:,:,:)
           end do
            ncols = elem(ie)%idxP%NumUniquePts
            call UniquePoints(elem(ie)%idxP, elem(ie)%state%psdry(:,:), ps_tmp(1:ncols,ie))
            call UniquePoints(elem(ie)%idxP, nlev, elem(ie)%state%dp3d(:,:,:,tl_f), dp3d_tmp(1:ncols,:,ie))
            call UniquePoints(elem(ie)%idxP, nlev, elem(ie)%state%T(:,:,:,tl_f), T_tmp(1:ncols,:,ie))
            call UniquePoints(elem(ie)%idxV, 2, nlev, elem(ie)%state%V(:,:,:,:,tl_f), uv_tmp(1:ncols,:,:,ie))
            call UniquePoints(elem(ie)%idxV, nlev, elem(ie)%derived%omega, omega_tmp(1:ncols,:,ie))

            call UniquePoints(elem(ie)%idxP, elem(ie)%state%phis, phis_tmp(1:ncols,ie))
            call UniquePoints(elem(ie)%idxP, nlev, num_advected, qgll,q_tmp(1:ncols,:,:,ie))

         end do
         call t_stopf('UniquePoints')

      end if ! if fv_nphys>0

   else

      ps_tmp(:,:)      = 0._r8
      T_tmp(:,:,:)     = 0._r8
      uv_tmp(:,:,:,:)  = 0._r8
      omega_tmp(:,:,:) = 0._r8
      phis_tmp(:,:)    = 0._r8
      q_tmp(:,:,:,:)   = 0._r8

      if (cam_runtime_opts%gw_front() .or. &
          cam_runtime_opts%gw_front_igw()) then

         frontgf(:,:,:) = 0._r8
         frontga(:,:,:) = 0._r8

      end if

   endif ! iam < par%nprocs

   if (fv_nphys < 1) then
      deallocate(qgll)
   end if

   ! q_prev is for saving the tracer fields for calculating tendencies
   if (.not. allocated(q_prev)) then
      allocate(q_prev(pcols,pver,num_advected), stat=ierr)
      call check_allocate(ierr, subname, 'q_prev(pcols,pver,num_advected)', &
                          file=__FILE__, line=__LINE__)
   end if
   q_prev = 0.0_r8

   call t_startf('dpcopy')
   !$omp parallel do num_threads(max_num_threads) private (icol, ie, blk_ind, ilyr, m)
   do icol = 1, pcols
      call get_dyn_col_p(icol, ie, blk_ind)
      phys_state%psdry(icol) = real(ps_tmp(blk_ind(1), ie), kind_phys)
      phys_state%phis(icol)  = real(phis_tmp(blk_ind(1), ie), kind_phys)
      do ilyr = 1, pver
         phys_state%pdeldry(icol, ilyr) = real(dp3d_tmp(blk_ind(1), ilyr, ie), kind_phys)
         phys_state%t(icol, ilyr)       = real(T_tmp(blk_ind(1), ilyr, ie), kind_phys)
         phys_state%u(icol, ilyr)       = real(uv_tmp(blk_ind(1), 1, ilyr, ie), kind_phys)
         phys_state%v(icol, ilyr)       = real(uv_tmp(blk_ind(1), 2, ilyr, ie), kind_phys)
         phys_state%omega(icol, ilyr)   = real(omega_tmp(blk_ind(1), ilyr, ie), kind_phys)

         if (cam_runtime_opts%gw_front() .or. cam_runtime_opts%gw_front_igw()) then
            phys_state%frontgf(icol, ilyr) = real(frontgf(blk_ind(1), ilyr, ie), kind_phys)
            phys_state%frontga(icol, ilyr) = real(frontga(blk_ind(1), ilyr, ie), kind_phys)
         end if
      end do

      do m = 1, num_advected
         do ilyr = 1, pver
            const_data_ptr(icol, ilyr,m) = real(q_tmp(blk_ind(1), ilyr,m, ie), kind_phys)
         end do
      end do
   end do

   call t_stopf('dpcopy')

   ! Save the tracer fields input to physics package for calculating tendencies
   ! The mixing ratios are all dry at this point.
   q_prev(1:pcols,1:pver,:) = real(const_data_ptr(1:pcols,1:pver,1:num_advected), r8)

   call test_mapping_output_phys_state(phys_state,dyn_out%fvm)

   ! Deallocate the temporary arrays
   deallocate(ps_tmp)
   deallocate(dp3d_tmp)
   deallocate(phis_tmp)
   deallocate(T_tmp)
   deallocate(uv_tmp)
   deallocate(q_tmp)
   deallocate(omega_tmp)

   ! Constituent mixing rations in phys_state are all dry at this point.
   ! After return from derived_phys_dry the 'wet' constituents have been converted to wet mmr.
   call t_startf('derived_phys')
   call derived_phys_dry(cam_runtime_opts, phys_state, phys_tend)
   call t_stopf('derived_phys')

end subroutine d_p_coupling

!=========================================================================================

subroutine p_d_coupling(cam_runtime_opts, phys_state, phys_tend, dyn_in, tl_f, tl_qdp)

   ! Convert the physics output state into the dynamics input state.
   use test_fvm_mapping, only: test_mapping_overwrite_tendencies
   use test_fvm_mapping, only: test_mapping_output_mapped_tendencies
   use cam_ccpp_cap,     only: cam_constituents_array
   use cam_constituents, only: num_advected
   use cam_constituents, only: const_is_water_species

   ! SE dycore:
   use bndry_mod,        only: bndry_exchange
   use edge_mod,         only: edgeVpack, edgeVunpack
   use fvm_mapping,      only: phys2dyn_forcings_fvm

   ! arguments
   type(runtime_options), intent(in)     :: cam_runtime_opts ! Runtime settings object
   type(physics_state),   intent(inout)  :: phys_state
   type(physics_tend),    intent(inout)  :: phys_tend
   integer,               intent(in)     :: tl_qdp, tl_f
   type(dyn_import_t),    intent(inout)  :: dyn_in
   type(hybrid_t)                        :: hybrid

   ! LOCAL VARIABLES
   integer                  :: ic , ncols       ! index
   type(element_t), pointer :: elem(:)          ! pointer to dyn_in element array
   integer                  :: ie               ! index for elements
   integer                  :: blk_ind(1)       ! element offset
   integer                  :: icol, ilyr       ! indices for column, layer
   real(kind=kind_phys),  pointer :: const_data_ptr(:,:,:) ! constituent data pointer

   real(r8),  allocatable   :: dp_phys(:,:,:)   ! temp array to hold dp on physics grid
   real(r8),  allocatable   :: T_tmp(:,:,:)     ! temp array to hold T
   real(r8),  allocatable   :: dq_tmp(:,:,:,:)  ! temp array to hold q
   real(r8),  allocatable   :: uv_tmp(:,:,:,:)  ! temp array to hold uv
   integer                  :: m, i, j, k

   real(kind_phys)          :: factor
   integer                  :: num_trac
   integer                  :: nets, nete
   integer                  :: kptr, ii
   integer                  :: ierr

   character(len=*), parameter :: subname='p_d_coupling'
   !----------------------------------------------------------------------------

   if (.not. local_dp_map) then
      call endrun('p_d_coupling: Weak scaling does not support load balancing')
   end if

   if (iam < par%nprocs) then
      elem => dyn_in%elem
   else
      nullify(elem)
   end if

   allocate(T_tmp(nphys_pts,pver,nelemd), stat=ierr)
   call check_allocate(ierr, subname, 'T_tmp(nphys_pts,pver,nelemd)', &
                       file=__FILE__, line=__LINE__)

   allocate(uv_tmp(nphys_pts,2,pver,nelemd), stat=ierr)
   call check_allocate(ierr, subname, 'uv_tmp(nphys_pts,2,pver,nelemd)', &
                       file=__FILE__, line=__LINE__)

   allocate(dq_tmp(nphys_pts,pver,num_advected,nelemd), stat=ierr)
   call check_allocate(ierr, subname, 'dq_tmp(nphys_pts,pver,num_advected,nelemd)', &
                       file=__FILE__, line=__LINE__)

   allocate(dp_phys(nphys_pts,pver,nelemd), stat=ierr)
   call check_allocate(ierr, subname, 'dp_phys(nphys_pts,pver,nelemd)', &
                       file=__FILE__, line=__LINE__)

   T_tmp  = 0.0_r8
   uv_tmp = 0.0_r8
   dq_tmp = 0.0_r8

   !Grab pointer to constituent array
   const_data_ptr => cam_constituents_array()

   !Convert wet mixing ratios to dry, which for CAM
   !configurations is only the water species:
   !$omp parallel do num_threads(max_num_threads) private (k, i, m)
   do ilyr = 1, nlev
      do icol=1, pcols
         !Determine wet to dry adjustment factor:
         factor = phys_state%pdel(icol,ilyr)/phys_state%pdeldry(icol,ilyr)

         !This should ideally check if a constituent is a wet
         !mixing ratio or not, but until that is working properly
         !in the CCPP framework just check for the water species status
         !instead, which is all that CAM configurations require:
         do m=1, num_advected
            if (const_is_water_species(m)) then
               const_data_ptr(icol,ilyr,m) = factor*const_data_ptr(icol,ilyr,m)
            end if
         end do
      end do
   end do

   if (.not. allocated(q_prev)) then
      call endrun('p_d_coupling: q_prev not allocated')
   end if
   call t_startf('pd_copy')
   !$omp parallel do num_threads(max_num_threads) private (icol, ie, blk_ind, ilyr, m)
   do icol = 1, pcols
      call get_dyn_col_p(icol, ie, blk_ind)

      ! test code -- does nothing unless cpp macro debug_coupling is defined.
      call test_mapping_overwrite_tendencies(phys_state,            &
           phys_tend, pcols, q_prev(1:pcols,:,:),      &
           dyn_in%fvm)

      do ilyr = 1, pver
         dp_phys(blk_ind(1),ilyr,ie)  = real(phys_state%pdeldry(icol,ilyr), r8)
         T_tmp(blk_ind(1),ilyr,ie)    = real(phys_tend%dTdt_total(icol,ilyr), r8)
         uv_tmp(blk_ind(1),1,ilyr,ie) = real(phys_tend%dudt_total(icol,ilyr), r8)
         uv_tmp(blk_ind(1),2,ilyr,ie) = real(phys_tend%dvdt_total(icol,ilyr), r8)
         do m = 1, num_advected
            dq_tmp(blk_ind(1),ilyr,m,ie) =                                    &
                 (real(const_data_ptr(icol,ilyr,m), r8) - q_prev(icol,ilyr,m))
         end do
      end do
   end do
   call t_stopf('pd_copy')


   if (iam < par%nprocs) then

      if (fv_nphys > 0) then

         ! put forcings into fvm structure
         num_trac = max(qsize,ntrac)
         do ie = 1, nelemd
            do j = 1, fv_nphys
               do i = 1, fv_nphys
                  ii = i + (j-1)*fv_nphys
                  dyn_in%fvm(ie)%ft(i,j,1:pver)                 = T_tmp(ii,1:pver,ie)
                  dyn_in%fvm(ie)%fm(i,j,1:2,1:pver)             = uv_tmp(ii,1:2,1:pver,ie)
                  dyn_in%fvm(ie)%fc_phys(i,j,1:pver,1:num_trac) = dq_tmp(ii,1:pver,1:num_trac,ie)
                  dyn_in%fvm(ie)%dp_phys(i,j,1:pver)            = dp_phys(ii,1:pver,ie)
               end do
            end do
         end do

         !JMD $OMP PARALLEL NUM_THREADS(horz_num_threads), DEFAULT(SHARED), PRIVATE(hybrid,nets,nete,n)
         !JMD        hybrid = config_thread_region(par,'horizontal')
         hybrid = config_thread_region(par,'serial')
         call get_loop_ranges(hybrid,ibeg=nets,iend=nete)

         ! high-order mapping of ft and fm (and fq if no cslam) using fvm technology
         call t_startf('phys2dyn')
         call phys2dyn_forcings_fvm(elem, dyn_in%fvm, hybrid,nets,nete,ntrac==0, tl_f, tl_qdp)
         call t_stopf('phys2dyn')
      else

         call t_startf('putUniquePoints')

         !$omp parallel do num_threads(max_num_threads) private(ie,ncols)
         do ie = 1, nelemd
            ncols = elem(ie)%idxP%NumUniquePts
            call putUniquePoints(elem(ie)%idxP, nlev, T_tmp(1:ncols,:,ie),       &
               elem(ie)%derived%fT(:,:,:))
            call putUniquePoints(elem(ie)%idxV, 2, nlev, uv_tmp(1:ncols,:,:,ie), &
               elem(ie)%derived%fM(:,:,:,:))
            call putUniquePoints(elem(ie)%idxV, nlev, num_advected, dq_tmp(1:ncols,:,:,ie), &
               elem(ie)%derived%fQ(:,:,:,:))
         end do
         call t_stopf('putUniquePoints')
      end if
   end if

   deallocate(T_tmp)
   deallocate(uv_tmp)
   deallocate(dq_tmp)

   ! Boundary exchange for physics forcing terms.
   ! For physics on GLL grid, for points with duplicate degrees of freedom,
   ! putuniquepoints() set one of the element values and set the others to zero,
   ! so do a simple sum (boundary exchange with no weights).
   ! For physics grid, we interpolated into all points, so do weighted average.

   call t_startf('p_d_coupling:bndry_exchange')

   do ie = 1, nelemd
      if (fv_nphys > 0) then
         do k = 1, nlev
            dyn_in%elem(ie)%derived%FM(:,:,1,k) =                          &
                 dyn_in%elem(ie)%derived%FM(:,:,1,k) *                     &
                 dyn_in%elem(ie)%spheremp(:,:)
            dyn_in%elem(ie)%derived%FM(:,:,2,k) =                          &
                 dyn_in%elem(ie)%derived%FM(:,:,2,k) *                     &
                 dyn_in%elem(ie)%spheremp(:,:)
            dyn_in%elem(ie)%derived%FT(:,:,k) =                            &
                 dyn_in%elem(ie)%derived%FT(:,:,k) *                       &
                 dyn_in%elem(ie)%spheremp(:,:)
            do m = 1, qsize
               dyn_in%elem(ie)%derived%FQ(:,:,k,m) =                       &
                    dyn_in%elem(ie)%derived%FQ(:,:,k,m) *                  &
                    dyn_in%elem(ie)%spheremp(:,:)
            end do
         end do
      end if
      kptr = 0
      call edgeVpack(edgebuf, dyn_in%elem(ie)%derived%FM(:,:,:,:), 2*nlev, kptr, ie)
      kptr = kptr + 2*nlev
      call edgeVpack(edgebuf, dyn_in%elem(ie)%derived%FT(:,:,:), nlev, kptr, ie)
      kptr = kptr + nlev
      call edgeVpack(edgebuf, dyn_in%elem(ie)%derived%FQ(:,:,:,:), nlev*qsize, kptr, ie)
   end do

   if (iam < par%nprocs) then
     call bndry_exchange(par, edgebuf, location='p_d_coupling')
   end if

   do ie = 1, nelemd
      kptr = 0
      call edgeVunpack(edgebuf, dyn_in%elem(ie)%derived%FM(:,:,:,:), 2*nlev, kptr, ie)
      kptr = kptr + 2*nlev
      call edgeVunpack(edgebuf, dyn_in%elem(ie)%derived%FT(:,:,:), nlev, kptr, ie)
      kptr = kptr + nlev
      call edgeVunpack(edgebuf, dyn_in%elem(ie)%derived%FQ(:,:,:,:), nlev*qsize, kptr, ie)
      if (fv_nphys > 0) then
         do k = 1, nlev
            dyn_in%elem(ie)%derived%FM(:,:,1,k) =                             &
                 dyn_in%elem(ie)%derived%FM(:,:,1,k) *                        &
                 dyn_in%elem(ie)%rspheremp(:,:)
            dyn_in%elem(ie)%derived%FM(:,:,2,k) =                             &
                 dyn_in%elem(ie)%derived%FM(:,:,2,k) *                        &
                 dyn_in%elem(ie)%rspheremp(:,:)
            dyn_in%elem(ie)%derived%FT(:,:,k) =                               &
                 dyn_in%elem(ie)%derived%FT(:,:,k) *                          &
                 dyn_in%elem(ie)%rspheremp(:,:)
            do m = 1, qsize
               dyn_in%elem(ie)%derived%FQ(:,:,k,m) =                          &
                    dyn_in%elem(ie)%derived%FQ(:,:,k,m) *                     &
                    dyn_in%elem(ie)%rspheremp(:,:)
            end do
         end do
      end if
   end do
   call t_stopf('p_d_coupling:bndry_exchange')

   if (iam < par%nprocs .and. fv_nphys > 0) then
      call test_mapping_output_mapped_tendencies(dyn_in%fvm(1:nelemd), elem(1:nelemd), &
                                                 1, nelemd, tl_f, tl_qdp)
   end if
end subroutine p_d_coupling

!=========================================================================================

subroutine derived_phys_dry(cam_runtime_opts, phys_state, phys_tend)

   ! The ps, pdel, and q components of phys_state are all dry on input.
   ! On output the psdry and pdeldry components are initialized; ps and pdel are
   ! updated to contain contribution from water vapor only; the 'wet' constituent
   ! mixing ratios are converted to a wet basis.  Initialize geopotential heights.
   ! Finally compute energy and water column integrals of the physics input state.

   use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t
   use cam_ccpp_cap,      only: cam_constituents_array
   use cam_ccpp_cap,      only: cam_model_const_properties
   use cam_constituents,  only: num_advected
   use cam_constituents,  only: const_is_water_species
   use cam_constituents,  only: const_get_index
   use cam_constituents,  only: const_qmin
   use runtime_obj,       only: wv_stdname
   use physics_types,     only: lagrangian_vertical
   use physconst,         only: cpair, gravit, zvir
   use cam_thermo,        only: cam_thermo_dry_air_update, cam_thermo_water_update
   use air_composition,   only: thermodynamic_active_species_num
   use air_composition,   only: thermodynamic_active_species_idx
   use air_composition,   only: dry_air_species_num
   use physics_types,     only: cpairv, rairv, zvirv, cappav
   use physics_types,     only: cp_or_cv_dycore
   use physics_grid,      only: columns_on_task
   use geopotential_temp, only: geopotential_temp_run
   use static_energy,     only: update_dry_static_energy_run
   use qneg,              only: qneg_run
   use hycoef,            only: hyai, ps0
   use shr_vmath_mod,     only: shr_vmath_log
   use shr_kind_mod,      only: shr_kind_cx
   use dyn_comp,          only: ixo, ixo2, ixh, ixh2
   use cam_thermo_formula,only: ENERGY_FORMULA_DYCORE_SE

   ! arguments
   type(runtime_options), intent(in)    :: cam_runtime_opts ! Runtime settings object
   type(physics_state),   intent(inout) :: phys_state
   type(physics_tend ),   intent(inout) :: phys_tend

   ! local variables
   real(kind_phys) :: factor_array(pcols,nlev)
   real(kind_phys), pointer :: const_data_ptr(:,:,:)

   !constituent properties pointer
   type(ccpp_constituent_prop_ptr_t), pointer :: const_prop_ptr(:)

   integer :: m, i, k, m_cnst
   integer :: ix_q

   !Needed for "geopotential_temp" CCPP scheme
   integer :: errflg
   character(len=shr_kind_cx) :: errmsg

   character(len=*), parameter :: subname = 'derived_phys_dry'

   !--------------------------------------------
   !  Variables needed for WACCM-X
   !--------------------------------------------
    real(r8) :: mmrSum_O_O2_H                ! Sum of mass mixing ratios for O, O2, and H
    real(r8), parameter :: mmrMin=1.e-20_r8  ! lower limit of o2, o, and h mixing ratios
    real(r8), parameter :: N2mmrMin=1.e-6_r8 ! lower limit of o2, o, and h mixing ratios
    real(r8), parameter :: H2lim=6.e-5_r8    ! H2 limiter: 10x global H2 MMR (Roble, 1995)
   !----------------------------------------------------------------------------

   ! Nullify pointers
   nullify(const_data_ptr)
   nullify(const_prop_ptr)

   ! Set constituent indices
   call const_get_index(wv_stdname, ix_q)

   ! Grab pointer to constituent and properties arrays
   const_data_ptr => cam_constituents_array()
   const_prop_ptr => cam_model_const_properties()

   ! Create qmin array (if not already generated):
   if (.not.allocated(qmin_vals)) then
     allocate(qmin_vals(size(const_prop_ptr)), stat=errflg)
     call check_allocate(errflg, subname, &
                         'qmin_vals(size(cam_model_const_properties))', &
                         file=__FILE__, line=__LINE__)


     ! Set relevant minimum values for each constituent:
     do m = 1, size(qmin_vals)
       qmin_vals(m) = const_qmin(m)
     end do
   end if

   ! Evaluate derived quantities

   ! dry pressure variables

   !$omp parallel do num_threads(horz_num_threads) private (i)
   do i = 1, pcols
      ! Set model-top values:
      phys_state%psdry(i)     = real(hyai(1)*ps0, kind_phys) + sum(phys_state%pdeldry(i,:))
      phys_state%pintdry(i,1) = real(hyai(1)*ps0, kind_phys)
   end do

   ! Calculate (natural) logarithm:
   call shr_vmath_log(phys_state%pintdry(1:pcols,1), &
                      phys_state%lnpintdry(1:pcols,1), pcols)

   !$omp parallel do num_threads(horz_num_threads) private (k, i)
   do k = 1, nlev
      do i = 1, pcols
         ! Calculate dry pressure variables for rest of column:
         phys_state%pintdry(i,k+1) = phys_state%pintdry(i,k) + phys_state%pdeldry(i,k)
         phys_state%rpdeldry(i,k)  = 1._kind_phys/phys_state%pdeldry(i,k)
         phys_state%pmiddry(i,k)   = 0.5_kind_phys*(phys_state%pintdry(i,k+1) + &
                                         phys_state%pintdry(i,k))
      end do

      ! Calculate (natural) logarithms:
      call shr_vmath_log(phys_state%pintdry(1:pcols,k+1),&
                         phys_state%lnpintdry(1:pcols,k+1), pcols)

      call shr_vmath_log(phys_state%pmiddry(1:pcols,k), &
                         phys_state%lnpmiddry(1:pcols,k), pcols)
   end do

   ! wet pressure variables (should be removed from physics!)
   factor_array(:,:) = 1.0_kind_phys
   !$omp parallel do num_threads(horz_num_threads) private (k, i, m_cnst)
   do m_cnst = dry_air_species_num + 1, thermodynamic_active_species_num
      ! include all water species in the factor array.
      m = thermodynamic_active_species_idx(m_cnst)
      do k = 1, nlev
         do i = 1, pcols
            ! at this point all q's are dry
            factor_array(i,k) = factor_array(i,k) + const_data_ptr(i,k,m)
         end do
      end do
   end do

   !$omp parallel do num_threads(horz_num_threads) private (k, i)
   do k = 1, nlev
      do i = 1, pcols
         phys_state%pdel(i,k) = phys_state%pdeldry(i,k) * factor_array(i,k)
      end do
   end do

   ! initialize vertical loop - model top pressure

   !$omp parallel do num_threads(horz_num_threads) private (i)
   do i=1, pcols
      ! Set model-top values assuming zero moisture:
      phys_state%ps(i)     = phys_state%pintdry(i,1)
      phys_state%pint(i,1) = phys_state%pintdry(i,1)
   end do

   !$omp parallel do num_threads(horz_num_threads) private (k, i)
   do k = 1, nlev
      do i=1, pcols
         ! Calculate wet (total) pressure variables for rest of column:
         phys_state%pint(i,k+1) = phys_state%pint(i,k) + phys_state%pdel(i,k)
         phys_state%pmid(i,k)   = (phys_state%pint(i,k+1) + phys_state%pint(i,k))/2._kind_phys
         phys_state%ps(i)       =  phys_state%ps(i) + phys_state%pdel(i,k)
      end do
      ! Calculate (natural) logarithms:
      call shr_vmath_log(phys_state%pint(1:pcols,k), phys_state%lnpint(1:pcols,k), pcols)
      call shr_vmath_log(phys_state%pmid(1:pcols,k), phys_state%lnpmid(1:pcols,k), pcols)
   end do
   call shr_vmath_log(phys_state%pint(1:pcols,pverp),phys_state%lnpint(1:pcols,pverp),pcols)

   !$omp parallel do num_threads(horz_num_threads) private (k, i)
   do k = 1, nlev
      do i = 1, pcols
         phys_state%rpdel(i,k) = 1._kind_phys/phys_state%pdel(i,k)
      end do
   end do

   !------------------------------------------------------------
   ! Apply limiters to mixing ratios of major species (WACCMX):
   ! Ensure N2 = 1 - (O2 + O + H) mmr is greater than 0
   ! Check for unusually large H2 values and set to lower value.
   !------------------------------------------------------------
   if (cam_runtime_opts%waccmx_option() == 'ionosphere' .or. &
       cam_runtime_opts%waccmx_option() == 'neutral')  then

      do i=1,pcols
         do k=1,pver

            if (const_data_ptr(i,k,ixo) < mmrMin) const_data_ptr(i,k,ixo) = mmrMin
            if (const_data_ptr(i,k,ixo2) < mmrMin) const_data_ptr(i,k,ixo2) = mmrMin

            mmrSum_O_O2_H = const_data_ptr(i,k,ixo)+const_data_ptr(i,k,ixo2)+const_data_ptr(i,k,ixh)

            if ((1._r8-mmrMin-mmrSum_O_O2_H) < 0._r8) then

               const_data_ptr(i,k,ixo) = const_data_ptr(i,k,ixo) * (1._r8 - N2mmrMin) / mmrSum_O_O2_H

               const_data_ptr(i,k,ixo2) = const_data_ptr(i,k,ixo2) * (1._r8 - N2mmrMin) / mmrSum_O_O2_H

               const_data_ptr(i,k,ixh) = const_data_ptr(i,k,ixh) * (1._r8 - N2mmrMin) / mmrSum_O_O2_H

            endif

            if(const_data_ptr(i,k,ixh2) > H2lim) then
               const_data_ptr(i,k,ixh2) = H2lim
            endif

         end do
      end do
   endif

   ! Ensure tracers are all greater than or equal to their
   ! minimum-allowed value:
   call qneg_run('D_P_COUPLING', columns_on_task, pver, &
                 qmin_vals, const_data_ptr, errflg, errmsg)

   !-----------------------------------------------------------------------------
   ! Call cam_thermo_update. If cam_runtime_opts%update_thermodynamic_variables()
   ! returns .true., cam_thermo_update will compute cpairv, rairv, mbarv, and cappav as
   ! constituent dependent variables. It will also:
   ! Compute molecular viscosity(kmvis) and conductivity(kmcnd).
   ! Update zvirv registry variable; calculated for WACCM-X.
   !-----------------------------------------------------------------------------
   if (dry_air_species_num > 0) then
      call cam_thermo_dry_air_update( &
           mmr                     = const_data_ptr, & ! dry MMR
           T                       = phys_state%t,   &
           ncol                    = pcols,          &
           pver                    = pver,           &
           update_thermo_variables = cam_runtime_opts%update_thermodynamic_variables() &
      )
   else
      zvirv(:,:) = zvir
   end if

   !
   ! update cp_or_cv_dycore in SIMA state.
   ! (note: at this point q is dry)
   !
   call cam_thermo_water_update( &
        mmr             = const_data_ptr,           & ! dry MMR
        ncol            = pcols,                    &
        pver            = pver,                     &
        energy_formula  = ENERGY_FORMULA_DYCORE_SE, &
        cp_or_cv_dycore = cp_or_cv_dycore           &
   )

   !$omp parallel do num_threads(horz_num_threads) private (k, i)
   do k = 1, nlev
      do i = 1, pcols
         phys_state%exner(i,k) = (phys_state%pint(i,pver+1)/phys_state%pmid(i,k))**cappav(i,k)
      end do
   end do

   ! ========= Q is dry ^^^ ---- Q is moist vvv ========= !

   !
   ! CAM physics expects that: water tracers (including moisture) are moist; the rest dry mixing ratio
   ! at this point Q is converted to moist.
   !
   factor_array(:,1:nlev) = 1._kind_phys/factor_array(:,1:nlev)

   !$omp parallel do num_threads(horz_num_threads) private (m, k, i)
   do m = 1, num_advected
      do k = 1, nlev
         do i = 1, pcols
            ! This should ideally check if a constituent is a wet
            ! mixing ratio or not, but until that is working properly
            ! in the CCPP framework just check for the water species status
            ! instead, which is all that CAM physics requires:
            if (const_is_water_species(m)) then
              const_data_ptr(i,k,m) = factor_array(i,k)*const_data_ptr(i,k,m)
            end if
         end do
      end do
   end do

   ! Call geopotential_temp CCPP scheme:
   call geopotential_temp_run(pver, lagrangian_vertical, pver, 1,                        &
                              pverp, 1, num_advected, phys_state%lnpint,                 &
                              phys_state%pint, phys_state%pmid, phys_state%pdel,         &
                              phys_state%rpdel, phys_state%t, const_data_ptr(:,:,ix_q),  &
                              const_data_ptr, const_prop_ptr, rairv, gravit, zvirv,      &
                              phys_state%zi, phys_state%zm, columns_on_task, errflg,     &
                              errmsg)

   ! Compute initial dry static energy, include surface geopotential
   call update_dry_static_energy_run(pver, gravit, phys_state%t, phys_state%zm,          &
                                     phys_state%phis, phys_state%dse, cpairv,            &
                                     errflg, errmsg)

end subroutine derived_phys_dry

!=========================================================================================

subroutine thermodynamic_consistency(phys_state, const_data_ptr, phys_tend, ncols, pver)
  !
   ! Adjust the physics temperature tendency for thermal energy consistency with the
   ! dynamics.
   ! Note: mixing ratios are assumed to be dry.
   !
   use physconst,         only: cpair
   use air_composition,   only: get_cp

   ! SE dycore:
   use dimensions_mod,    only: lcp_moist
   use control_mod,       only: phys_dyn_cp

   type(physics_state), intent(in)    :: phys_state
   real(kind_phys), pointer           :: const_data_ptr(:,:,:)
   type(physics_tend ), intent(inout) :: phys_tend
   integer,  intent(in)               :: ncols, pver

   real(kind_phys) :: inv_cp(ncols,pver)
   !----------------------------------------------------------------------------

   if (lcp_moist.and.phys_dyn_cp==1) then
     !
     ! scale temperature tendency so that thermal energy increment from physics
     ! matches SE (not taking into account dme adjust)
     !
     ! note that if lcp_moist=.false. then there is thermal energy increment
     ! consistency (not taking into account dme adjust)
     !
     call get_cp(const_data_ptr(1:ncols,1:pver,1:num_advected),.true.,inv_cp)

     phys_tend%dTdt_total(1:ncols,1:pver) = phys_tend%dTdt_total(1:ncols,1:pver)*cpair*inv_cp
   end if
end subroutine thermodynamic_consistency

!=========================================================================================

end module dp_coupling
