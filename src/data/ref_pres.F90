module ref_pres
!--------------------------------------------------------------------------
!
! Provides access to reference pressures for use by the physics
! parameterizations.  The pressures are provided by the dynamical core
! since it determines the grid used by the physics.
!
! Note that the init method for this module is called before the init
! method in physpkg; therefore, most physics modules can use these
! reference pressures during their init phases.
!
!--------------------------------------------------------------------------

   use ccpp_kinds,      only: kind_phys
   use cam_abortutils,  only: endrun

   implicit none
   public
   save

   ! Reference pressures (Pa)
   !> \section arg_table_ref_pres  Argument Table
   !! \htmlinclude ref_pres.html

   real(kind_phys), protected, allocatable :: pref_edge(:)      ! Layer edges
   real(kind_phys), protected, allocatable :: pref_mid(:)       ! Layer midpoints
   real(kind_phys), protected, allocatable :: pref_mid_norm(:)  ! Layer midpoints normalized by
                                                                ! surface pressure ('eta' coordinate)

   real(kind_phys), protected :: ptop_ref                       ! air pressure at top of model
                                                                ! reference profile

   ! Number of top levels using pure pressure representation
   integer, protected :: num_pr_lev

   ! Pressure used to set troposphere cloud physics top (Pa)
   real(kind_phys), protected :: trop_cloud_top_press = 0._kind_phys

   ! Top level for troposphere cloud physics
   integer, protected :: trop_cloud_top_lev

   ! Pressure used to set MAM process top (Pa)
   real(kind_phys), protected :: clim_modal_aero_top_press = 0._kind_phys

   ! Top level for MAM processes that impact climate
   integer, protected :: clim_modal_aero_top_lev

   ! Molecular diffusion is calculated only if the model top is below this
   ! pressure (Pa).
   real(kind_phys), protected :: do_molec_press = 0.1_kind_phys

   ! Pressure used to set bottom of molecular diffusion region (Pa).
   real(kind_phys), protected :: molec_diff_bot_press = 50._kind_phys

   ! Flag for molecular diffusion, and molecular diffusion level index.
   logical, protected :: do_molec_diff = .false.
   integer, protected :: nbot_molec = 0

!====================================================================================
contains
!====================================================================================

   subroutine ref_pres_readnl(nlfile)

      use spmd_utils,      only: masterproc, masterprocid, mpicom, npes
      use shr_nl_mod,      only: find_group_name => shr_nl_find_group_name
      use mpi,             only: mpi_real8

      character(len=*), intent(in) :: nlfile  ! filepath for file containing namelist input

      ! Local variables
      integer :: unitn, ierr
      character(len=*), parameter :: subname = 'ref_pres_readnl'

      namelist /ref_pres_nl/ trop_cloud_top_press, clim_modal_aero_top_press,&
           do_molec_press, molec_diff_bot_press
      !-----------------------------------------------------------------------------

      if (masterproc) then
         open( newunit=unitn, file=trim(nlfile), status='old' )
         call find_group_name(unitn, 'ref_pres_nl', status=ierr)
         if (ierr == 0) then
            read(unitn, ref_pres_nl, iostat=ierr)
            if (ierr /= 0) then
               call endrun(subname // ':: ERROR reading namelist')
            end if
         end if
         close(unitn)
         ! Check that top for modal aerosols is not lower than
         ! top for clouds.
         if (clim_modal_aero_top_press > trop_cloud_top_press) then
            call endrun("ERROR: clim_modal_aero_top press must be less &
                        &than or equal to trop_cloud_top_press.")
         end if
      end if

      ! Broadcast namelist variables
      call mpi_bcast(trop_cloud_top_press,      1,  mpi_real8, masterprocid, mpicom, ierr)
      call mpi_bcast(clim_modal_aero_top_press, 1,  mpi_real8, masterprocid, mpicom, ierr)
      call mpi_bcast(do_molec_press,            1,  mpi_real8, masterprocid, mpicom, ierr)
      call mpi_bcast(molec_diff_bot_press,      1,  mpi_real8, masterprocid, mpicom, ierr)

   end subroutine ref_pres_readnl

!====================================================================================

   subroutine ref_pres_init(pref_edge_in, pref_mid_in, num_pr_lev_in)

      use phys_vars_init_check, only: mark_as_initialized
      use string_utils,         only: to_str
      use vert_coord,           only: pver, pverp

      ! Initialize reference pressures

      ! arguments
      real(kind_phys), intent(in) :: pref_edge_in(:) ! reference pressure at layer edges (Pa)
      real(kind_phys), intent(in) :: pref_mid_in(:)  ! reference pressure at layer midpoints (Pa)
      integer,  intent(in) :: num_pr_lev_in          ! number of top levels using pure pressure representation

      ! local variables
      real(kind_phys) :: psurf_ref                   ! air pressure at bottom of model reference profile
      integer :: iret ! return status integer
      character(len=*), parameter :: subname = 'ref_pres_init'

      !---------------------------------------------------------------------------

      ! Allocate variables:

      allocate (pref_edge(pverp), stat=iret)
      if (iret /= 0) then
         call endrun(subname//': allocate pref_edge(pverp) failed with stat: '//to_str(iret))
      end if

      allocate (pref_mid(pver), stat=iret)
      if (iret /= 0) then
         call endrun(subname//': allocate pref_mid(pver) failed with stat: '//to_str(iret))
      end if

      allocate (pref_mid_norm(pver), stat=iret)
      if (iret /= 0) then
         call endrun(subname//': allocate pref_mid_norm(pver) failed with stat: '//to_str(iret))
      end if

      pref_edge = pref_edge_in
      pref_mid  = pref_mid_in
      num_pr_lev = num_pr_lev_in

      ptop_ref = pref_edge(1)
      psurf_ref = pref_edge(pverp)

      pref_mid_norm = pref_mid/psurf_ref

      ! Find level corresponding to the top of troposphere clouds.
      trop_cloud_top_lev = press_lim_idx(pver, trop_cloud_top_press, &
         top=.true.)

      ! Find level corresponding to the top for MAM processes.
      clim_modal_aero_top_lev = press_lim_idx(pver, clim_modal_aero_top_press, &
         top=.true.)

      ! Find level corresponding to the molecular diffusion bottom.
      do_molec_diff = (ptop_ref < do_molec_press)
      if (do_molec_diff) then
         nbot_molec = press_lim_idx(pver, molec_diff_bot_press, &
            top=.false.)
      end if

      ! Tell rest of model that variables have been initialized:
      ! pref_edge_in
      call mark_as_initialized("reference_pressure_at_interface")
      ! pref_mid_in
      call mark_as_initialized("reference_pressure_in_atmosphere_layer")
      ! pref_mid_norm
      call mark_as_initialized("reference_pressure_in_atmosphere_layer_normalized_by_surface_reference_pressure")
      ! ptop_ref
      call mark_as_initialized("air_pressure_at_top_of_atmosphere_model")
      ! num_pr_lev
      call mark_as_initialized("number_of_pure_pressure_levels_at_top")
      ! trop_cloud_top_lev
      call mark_as_initialized("vertical_layer_index_of_troposphere_cloud_physics_top")
      ! clim_modal_aero_top_lev
      call mark_as_initialized("index_of_air_pressure_at_top_of_aerosol_model")
      ! do_molec_press
      call mark_as_initialized("largest_model_top_pressure_that_allows_molecular_diffusion")
      ! molec_diff_bot_press
      call mark_as_initialized("pressure_at_bottom_of_molecular_diffusion")
      ! do_molec_diff
      call mark_as_initialized("flag_for_molecular_diffusion")
      ! nbot_molec
      call mark_as_initialized("index_of_pressure_at_bottom_of_molecular_diffusion")

   end subroutine ref_pres_init

!====================================================================================

   ! Convert pressure limiters to the appropriate level.
   pure function press_lim_idx(pver, pres, top) result(lev_idx_lim)

      use vert_coord, only: index_top_layer, index_bottom_layer

      ! Input arguments:
      integer, intent(in)         :: pver ! Number of vertical levels
      real(kind_phys), intent(in) :: pres ! Air pressure (Pa)
      logical,  intent(in)        :: top  ! Is this a top or bottom limit?

      ! Local variables:
      integer :: lev_idx_lim, lev_idx, loop_dir

      ! Determine direction of vertical coordinate loop:
      if (index_top_layer > index_bottom_layer) then
         ! Assume "top" loop is counting down:
         loop_dir = -1
      else
         ! Assume "top" loop is counting up:
         loop_dir = 1
      end if

      ! Determine pressure limit level:
      if (top) then
         lev_idx_lim = index_bottom_layer + loop_dir
         do lev_idx = index_top_layer, index_bottom_layer, loop_dir
            if (pref_mid(lev_idx) > pres) then
               lev_idx_lim = lev_idx
               exit
            end if
         end do
      else
         lev_idx_lim = index_top_layer - loop_dir
         do lev_idx = index_bottom_layer, index_top_layer, -1*loop_dir
            if (pref_mid(lev_idx) < pres) then
               lev_idx_lim = lev_idx
               exit
            end if
         end do
      end if

   end function press_lim_idx

!====================================================================================
end module ref_pres
