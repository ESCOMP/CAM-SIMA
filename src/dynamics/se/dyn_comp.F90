module dyn_comp

! CAM interfaces to the SE Dynamical Core

use shr_kind_mod,           only: r8=>shr_kind_r8, cl=>shr_kind_cl
use dynconst,               only: pi

!SE dycore:
use element_mod,            only: element_t
use fvm_control_volume_mod, only: fvm_struct

implicit none
private

public ::          &
     dyn_import_t, &
     dyn_export_t, &
     dyn_readnl,   &
     dyn_init,     &
     dyn_run,      &
     dyn_final

type dyn_import_t
  type (element_t),  pointer :: elem(:) => null()
  type (fvm_struct), pointer :: fvm(:) => null()
end type dyn_import_t

type dyn_export_t
  type (element_t),  pointer :: elem(:) => null()
  type (fvm_struct), pointer :: fvm(:) => null()
end type dyn_export_t

! Namelist
logical, public, protected :: write_restart_unstruct

! constituent indices for waccm-x dry air properties
integer, public, protected :: &
   ixo  = -1, &
   ixo2 = -1, &
   ixh  = -1, &
   ixh2 = -1

interface read_dyn_var
  module procedure read_dyn_field_2d
  module procedure read_dyn_field_3d
end interface read_dyn_var

real(r8), parameter :: rad2deg = 180.0_r8 / pi
real(r8), parameter :: deg2rad = pi / 180.0_r8

! Character array used to hold diagnostic names for constituent
! fields on GLL grid:
character(len=cl), allocatable, public, protected :: cnst_diag_name_gll(:)

! Stash of namelist se_statediag_numtrac. The final statediag_numtrac
! is computed in dyn_init once num_advected (via qsize) is valid; it cannot
! be computed in dyn_readnl because that runs before cam_register_constituents.
integer, private :: se_statediag_numtrac_save = 0

!===============================================================================
contains
!===============================================================================

subroutine dyn_readnl(NLFileName)
   use mpi,              only: mpi_real8, mpi_integer, mpi_character, mpi_logical
   use air_composition,  only: thermodynamic_active_species_num
   use shr_nl_mod,       only: find_group_name => shr_nl_find_group_name
   use spmd_utils,       only: masterproc, masterprocid, mpicom, npes
   use dyn_grid,         only: se_write_grid_file, se_grid_filename, se_write_gll_corners
   use native_mapping,   only: native_mapping_readnl
   use vert_coord,       only: pver
   use cam_logfile,      only: iulog
   use cam_abortutils,   only: endrun
   use cam_control_mod,  only: initial_run

   !SE dycore:
   use namelist_mod,    only: homme_set_defaults, homme_postprocess_namelist
   use control_mod,     only: hypervis_subcycle, hypervis_subcycle_sponge
   use control_mod,     only: hypervis_subcycle_q, statefreq, runtype
   use control_mod,     only: nu, nu_div, nu_p, nu_q, nu_top, qsplit, rsplit
   use control_mod,     only: vert_remap_uvTq_alg, vert_remap_tracer_alg
   use control_mod,     only: tstep_type, rk_stage_user
   use control_mod,     only: ftype, limiter_option, partmethod
   use control_mod,     only: topology, variable_nsplit
   use control_mod,     only: fine_ne, hypervis_power, hypervis_scaling
   use control_mod,     only: max_hypervis_courant, statediag_numtrac,refined_mesh
   use control_mod,     only: molecular_diff, pgf_formulation, dribble_in_rsplit_loop
   use control_mod,     only: sponge_del4_nu_div_fac, sponge_del4_nu_fac, sponge_del4_lev
   use dimensions_mod,  only: ne, npart, fv_nphys, use_cslam
   use dimensions_mod,  only: large_Courant_incr
   use dimensions_mod,  only: fvm_supercycling, fvm_supercycling_jet
   use dimensions_mod,  only: kmin_jet, kmax_jet
   use params_mod,      only: SFCURVE
   use parallel_mod,    only: par, initmpi
   use thread_mod,      only: initomp, max_num_threads
   use thread_mod,      only: horz_num_threads, vert_num_threads, tracer_num_threads
   use se_dyn_time_mod, only: nsplit

   ! Dummy argument
   character(len=*), intent(in) :: NLFileName

   ! Local variables
   integer                      :: unitn, ierr,k

   ! SE Namelist variables
   integer                      :: se_fine_ne
   integer                      :: se_ftype
   integer                      :: se_statediag_numtrac
   integer                      :: se_fv_nphys
   real(r8)                     :: se_hypervis_power
   real(r8)                     :: se_hypervis_scaling
   integer                      :: se_hypervis_subcycle
   integer                      :: se_hypervis_subcycle_sponge
   integer                      :: se_hypervis_subcycle_q
   integer                      :: se_limiter_option
   real(r8)                     :: se_max_hypervis_courant
   character(len=cl)            :: se_mesh_file
   integer                      :: se_ne
   integer                      :: se_npes
   integer                      :: se_nsplit
   real(r8)                     :: se_nu
   real(r8)                     :: se_nu_div
   real(r8)                     :: se_nu_p
   real(r8)                     :: se_nu_top
   real(r8)                     :: se_sponge_del4_nu_fac
   real(r8)                     :: se_sponge_del4_nu_div_fac
   integer                      :: se_sponge_del4_lev
   integer                      :: se_qsplit
   logical                      :: se_refined_mesh
   integer                      :: se_rsplit
   integer                      :: se_statefreq
   integer                      :: se_tstep_type
   character(len=32)            :: se_vert_remap_T
   character(len=32)            :: se_vert_remap_uvTq_alg
   character(len=32)            :: se_vert_remap_tracer_alg
   integer                      :: se_horz_num_threads
   integer                      :: se_vert_num_threads
   integer                      :: se_tracer_num_threads
   logical                      :: se_write_restart_unstruct
   logical                      :: se_large_Courant_incr
   integer                      :: se_fvm_supercycling
   integer                      :: se_fvm_supercycling_jet
   integer                      :: se_kmin_jet
   integer                      :: se_kmax_jet
   real(r8)                     :: se_molecular_diff
   integer                      :: se_pgf_formulation
   real(r8)                     :: se_dribble_in_rsplit_loop

   namelist /dyn_se_nl/            &
      se_fine_ne,                  & ! For refined meshes
      se_ftype,                    & ! forcing type
      se_statediag_numtrac,        &
      se_fv_nphys,                 &
      se_hypervis_power,           &
      se_hypervis_scaling,         &
      se_hypervis_subcycle,        &
      se_hypervis_subcycle_sponge, &
      se_hypervis_subcycle_q,      &
      se_limiter_option,           &
      se_max_hypervis_courant,     &
      se_mesh_file,                & ! Refined mesh definition file
      se_ne,                       &
      se_npes,                     &
      se_nsplit,                   & ! # of dyn steps per physics timestep
      se_nu,                       &
      se_nu_div,                   &
      se_nu_p,                     &
      se_nu_top,                   &
      se_sponge_del4_nu_fac,       &
      se_sponge_del4_nu_div_fac,   &
      se_sponge_del4_lev,          &
      se_qsplit,                   &
      se_refined_mesh,             &
      se_rsplit,                   &
      se_statefreq,                & ! number of steps per printstate call
      se_tstep_type,               &
      se_vert_remap_T,             &
      se_vert_remap_uvTq_alg,      &
      se_vert_remap_tracer_alg,    &
      se_write_grid_file,          &
      se_grid_filename,            &
      se_write_gll_corners,        &
      se_horz_num_threads,         &
      se_vert_num_threads,         &
      se_tracer_num_threads,       &
      se_write_restart_unstruct,   &
      se_large_Courant_incr,       &
      se_fvm_supercycling,         &
      se_fvm_supercycling_jet,     &
      se_kmin_jet,                 &
      se_kmax_jet,                 &
      se_molecular_diff,           &
      se_pgf_formulation,          &
      se_dribble_in_rsplit_loop

   !--------------------------------------------------------------------------

   ! defaults for variables if not set by the namelist
   se_fine_ne                  = -1
   se_hypervis_power           = 0
   se_hypervis_scaling         = 0
   se_max_hypervis_courant     = 1.0e99_r8
   se_mesh_file                = ''
   se_write_restart_unstruct   = .false.

   ! Read the namelist (dyn_se_nl)
   call MPI_barrier(mpicom, ierr)
   if (masterproc) then
      write(iulog, *) "dyn_readnl: reading dyn_se_nl namelist..."
      open( newunit=unitn, file=trim(NLFileName), status='old' )
      call find_group_name(unitn, 'dyn_se_nl', status=ierr)
      if (ierr == 0) then
         read(unitn, dyn_se_nl, iostat=ierr)
         if (ierr /= 0) then
            call endrun('dyn_readnl: ERROR reading dyn_se_nl namelist')
         end if
      end if
      close(unitn)
   end if

   ! Broadcast namelist values to all PEs
   call MPI_bcast(se_fine_ne, 1, mpi_integer, masterprocid, mpicom, ierr)
   call MPI_bcast(se_ftype, 1, mpi_integer, masterprocid, mpicom, ierr)
   call MPI_bcast(se_statediag_numtrac, 1, mpi_integer, masterprocid, mpicom, ierr)
   call MPI_bcast(se_hypervis_power, 1, mpi_real8, masterprocid, mpicom, ierr)
   call MPI_bcast(se_hypervis_scaling, 1, mpi_real8, masterprocid, mpicom, ierr)
   call MPI_bcast(se_hypervis_subcycle, 1, mpi_integer, masterprocid, mpicom, ierr)
   call MPI_bcast(se_hypervis_subcycle_sponge, 1, mpi_integer, masterprocid, mpicom, ierr)
   call MPI_bcast(se_hypervis_subcycle_q, 1, mpi_integer, masterprocid, mpicom, ierr)
   call MPI_bcast(se_limiter_option, 1, mpi_integer, masterprocid, mpicom, ierr)
   call MPI_bcast(se_max_hypervis_courant, 1, mpi_real8, masterprocid, mpicom, ierr)
   call MPI_bcast(se_mesh_file, cl,  mpi_character, masterprocid, mpicom, ierr)
   call MPI_bcast(se_ne, 1, mpi_integer, masterprocid, mpicom, ierr)
   call MPI_bcast(se_npes, 1, mpi_integer, masterprocid, mpicom, ierr)
   call MPI_bcast(se_nsplit, 1, mpi_integer, masterprocid, mpicom, ierr)
   call MPI_bcast(se_nu, 1, mpi_real8, masterprocid, mpicom, ierr)
   call MPI_bcast(se_nu_div, 1, mpi_real8, masterprocid, mpicom, ierr)
   call MPI_bcast(se_nu_p, 1, mpi_real8, masterprocid, mpicom, ierr)
   call MPI_bcast(se_nu_top, 1, mpi_real8, masterprocid, mpicom, ierr)
   call MPI_bcast(se_sponge_del4_nu_fac, 1, mpi_real8, masterprocid, mpicom, ierr)
   call MPI_bcast(se_sponge_del4_nu_div_fac, 1, mpi_real8, masterprocid, mpicom, ierr)
   call MPI_bcast(se_sponge_del4_lev, 1, mpi_integer, masterprocid, mpicom, ierr)
   call MPI_bcast(se_qsplit, 1, mpi_integer, masterprocid, mpicom, ierr)
   call MPI_bcast(se_refined_mesh, 1, mpi_logical, masterprocid, mpicom, ierr)
   call MPI_bcast(se_rsplit, 1, mpi_integer, masterprocid, mpicom, ierr)
   call MPI_bcast(se_statefreq, 1, mpi_integer, masterprocid, mpicom, ierr)
   call MPI_bcast(se_tstep_type, 1, mpi_integer, masterprocid, mpicom, ierr)
   call MPI_bcast(se_vert_remap_T, 32, mpi_character, masterprocid, mpicom, ierr)
   call MPI_bcast(se_vert_remap_uvTq_alg, 32, mpi_character, masterprocid, mpicom, ierr)
   call MPI_bcast(se_vert_remap_tracer_alg, 32, mpi_character, masterprocid, mpicom, ierr)
   call MPI_bcast(se_fv_nphys, 1, mpi_integer, masterprocid, mpicom, ierr)
   call MPI_bcast(se_write_grid_file, 16,  mpi_character, masterprocid, mpicom, ierr)
   call MPI_bcast(se_grid_filename, cl, mpi_character, masterprocid, mpicom, ierr)
   call MPI_bcast(se_write_gll_corners, 1,  mpi_logical, masterprocid, mpicom, ierr)
   call MPI_bcast(se_horz_num_threads, 1, MPI_integer, masterprocid, mpicom,ierr)
   call MPI_bcast(se_vert_num_threads, 1, MPI_integer, masterprocid, mpicom,ierr)
   call MPI_bcast(se_tracer_num_threads, 1, MPI_integer, masterprocid, mpicom,ierr)
   call MPI_bcast(se_write_restart_unstruct, 1, mpi_logical, masterprocid, mpicom, ierr)
   call MPI_bcast(se_large_Courant_incr, 1, mpi_logical, masterprocid, mpicom, ierr)
   call MPI_bcast(se_fvm_supercycling, 1, mpi_integer, masterprocid, mpicom, ierr)
   call MPI_bcast(se_fvm_supercycling_jet, 1, mpi_integer, masterprocid, mpicom, ierr)
   call MPI_bcast(se_kmin_jet, 1, mpi_integer, masterprocid, mpicom, ierr)
   call MPI_bcast(se_kmax_jet, 1, mpi_integer, masterprocid, mpicom, ierr)
   call MPI_bcast(se_molecular_diff, 1, mpi_real8, masterprocid, mpicom, ierr)
   call MPI_bcast(se_pgf_formulation, 1, mpi_integer, masterprocid, mpicom, ierr)
   call MPI_bcast(se_dribble_in_rsplit_loop, 1, mpi_integer, masterprocid, mpicom, ierr)
   ! If se_npes is set to negative one, then make it match host model:
   if (se_npes == -1) then
      se_npes = npes
   else if (se_npes <= 0) then
      ! se_npes is not a positive integer:
      call endrun('dyn_readnl: ERROR: se_npes must either be > 0 or exactly -1')
   else if (se_npes > npes) then
      ! se_npes is too large:
      call endrun('dyn_readnl: ERROR: se_npes must be <= number of atmosphere pes (npes)')
   end if

   ! Initialize the SE structure that holds the MPI decomposition information
   par = initmpi(se_npes)
   call initomp()

   if (se_fvm_supercycling < 0) se_fvm_supercycling = se_rsplit
   if (se_fvm_supercycling_jet < 0) se_fvm_supercycling_jet = se_rsplit

   ! Go ahead and enforce ne = 0 for refined mesh runs
   if (se_refined_mesh) then
      se_ne = 0
   end if

   ! Set HOMME defaults
   call homme_set_defaults()
   ! Set HOMME variables not in CAM's namelist but with different CAM defaults
   partmethod                = SFCURVE
   npart                     = se_npes
   ! CAM requires forward-in-time, subcycled dynamics
   ! RK2 3 stage tracers, sign-preserving conservative
   rk_stage_user             = 3
   topology                  = "cube"
   ! Finally, set the HOMME variables which have different names
   fine_ne                   = se_fine_ne
   ftype                     = se_ftype
   ! Stash the raw namelist value; the MIN with num_advected must be deferred
   ! to dyn_init (num_advected is still 0 at this point — see module header
   ! comment on se_statediag_numtrac_save).
   se_statediag_numtrac_save = se_statediag_numtrac
   sponge_del4_nu_fac        = se_sponge_del4_nu_fac
   sponge_del4_nu_div_fac    = se_sponge_del4_nu_div_fac
   sponge_del4_lev           = se_sponge_del4_lev
   hypervis_power            = se_hypervis_power
   hypervis_scaling          = se_hypervis_scaling
   hypervis_subcycle         = se_hypervis_subcycle
   if (hypervis_subcycle_sponge<0) then
     hypervis_subcycle_sponge = hypervis_subcycle
   else
     hypervis_subcycle_sponge = se_hypervis_subcycle_sponge
   end if
   hypervis_subcycle_q      = se_hypervis_subcycle_q
   limiter_option           = se_limiter_option
   max_hypervis_courant     = se_max_hypervis_courant
   refined_mesh             = se_refined_mesh
   ne                       = se_ne
   nsplit                   = se_nsplit
   nu                       = se_nu
   nu_div                   = se_nu_div
   nu_p                     = se_nu_p
   nu_q                     = se_nu_p !for tracer-wind consistency nu_q must me equal to nu_p
   nu_top                   = se_nu_top
   qsplit                   = se_qsplit
   rsplit                   = se_rsplit
   statefreq                = se_statefreq
   tstep_type               = se_tstep_type
   vert_remap_uvTq_alg      = set_vert_remap(se_vert_remap_T, se_vert_remap_uvTq_alg)
   vert_remap_tracer_alg    = set_vert_remap(se_vert_remap_T, se_vert_remap_tracer_alg)
   fv_nphys                 = se_fv_nphys
   large_Courant_incr       = se_large_Courant_incr
   fvm_supercycling         = se_fvm_supercycling
   fvm_supercycling_jet     = se_fvm_supercycling_jet
   kmin_jet                 = se_kmin_jet
   kmax_jet                 = se_kmax_jet
   variable_nsplit          = .false.
   molecular_diff           = se_molecular_diff
   pgf_formulation          = se_pgf_formulation
   dribble_in_rsplit_loop   = se_dribble_in_rsplit_loop
   if (rsplit < 1) then
      call endrun('dyn_readnl: rsplit must be > 0')
   end if

   ! if restart or branch run
   if (.not. initial_run) then
      runtype = 1
   end if

   ! HOMME wants 'none' to indicate no mesh file
   if (len_trim(se_mesh_file) == 0) then
      se_mesh_file = 'none'
      if (se_refined_mesh) then
         call endrun('dyn_readnl ERROR: se_refined_mesh=.true. but no se_mesh_file')
      end if
   end if
   call homme_postprocess_namelist(se_mesh_file, par)

   ! Set threading numbers to reasonable values
   if ((se_horz_num_threads == 0) .and. (se_vert_num_threads == 0) .and. (se_tracer_num_threads == 0)) then
      ! The user has not set any threading values, choose defaults
      se_horz_num_threads = 1
      se_vert_num_threads = max_num_threads
      se_tracer_num_threads = se_vert_num_threads
   end if
   if (se_horz_num_threads < 1) then
      se_horz_num_threads = 1
   end if
   if (se_vert_num_threads < 1) then
      se_vert_num_threads = 1
   end if
   if (se_tracer_num_threads < 1) then
      se_tracer_num_threads = 1
   end if
   horz_num_threads = se_horz_num_threads
   vert_num_threads = se_vert_num_threads
   tracer_num_threads = se_tracer_num_threads

   write_restart_unstruct = se_write_restart_unstruct

   if (se_kmin_jet<0            ) kmin_jet             = 1
   if (se_kmax_jet<0            ) kmax_jet             = pver

   if (masterproc) then
      write(iulog, '(a,i0)')   'dyn_readnl: se_ftype                    = ',ftype
      write(iulog, '(a,i0)')   'dyn_readnl: se_statediag_numtrac        = ',se_statediag_numtrac_save
      write(iulog, '(a,i0)')   'dyn_readnl: se_hypervis_subcycle        = ',se_hypervis_subcycle
      write(iulog, '(a,i0)')   'dyn_readnl: se_hypervis_subcycle_sponge = ',se_hypervis_subcycle_sponge
      write(iulog, '(a,i0)')   'dyn_readnl: se_hypervis_subcycle_q      = ',se_hypervis_subcycle_q
      write(iulog, '(a,l4)')   'dyn_readnl: se_large_Courant_incr       = ',se_large_Courant_incr
      write(iulog, '(a,i0)')   'dyn_readnl: se_limiter_option           = ',se_limiter_option
      if (.not. se_refined_mesh) then
         write(iulog, '(a,i0)')'dyn_readnl: se_ne                       = ',se_ne
      end if
      write(iulog, '(a,i0)')   'dyn_readnl: se_npes                     = ',se_npes
      write(iulog, '(a,i0)')   'dyn_readnl: se_nsplit                   = ',se_nsplit
      !
      ! se_nu<0 then coefficients are set automatically in module global_norms_mod
      !
      if (se_nu_div>0) &
           write(iulog, '(a,e9.2)') 'dyn_readnl: se_nu                       = ',se_nu
      if (se_nu_div>0) &
           write(iulog, '(a,e9.2)') 'dyn_readnl: se_nu_div                   = ',se_nu_div
      if (se_nu_p>0) then
        write(iulog, '(a,e9.2)') 'dyn_readnl: se_nu_p                     = ',se_nu_p
        write(iulog, '(a)') 'Note that nu_q must be the same as nu_p for  mass / tracer inconsistency'
      end if
      write(iulog, '(a,e9.2)') 'dyn_readnl: se_nu_top                     = ',se_nu_top
      write(iulog, *)   'dyn_readnl: se_sponge_del4_nu_fac         = ',se_sponge_del4_nu_fac
      if (se_sponge_del4_nu_fac < 0) write(iulog, '(a)')   ' (automatically set based on model top location)'
      write(iulog, *)   'dyn_readnl: se_sponge_del4_nu_div_fac     = ',se_sponge_del4_nu_div_fac
      if (se_sponge_del4_nu_div_fac < 0)  write(iulog, '(a)')   ' (automatically set based on model top location)'
      write(iulog, *)   'dyn_readnl: se_sponge_del4_lev            = ',se_sponge_del4_lev
      if (se_sponge_del4_lev < 0)  write(iulog, '(a)')   ' (automatically set based on model top location)'
      write(iulog, '(a,i0)')   'dyn_readnl: se_qsplit                     = ',se_qsplit
      write(iulog, '(a,i0)')   'dyn_readnl: se_rsplit                     = ',se_rsplit
      write(iulog, '(a,i0)')   'dyn_readnl: se_statefreq                  = ',se_statefreq
      write(iulog, '(a,i0)')   'dyn_readnl: se_pgf_formulation            = ',pgf_formulation
      write(iulog, '(a,i0)')   'dyn_readnl: se_tstep_type                 = ',se_tstep_type
      write(iulog, '(a,a)')    'dyn_readnl: se_vert_remap_T               = ',trim(se_vert_remap_T)
      write(iulog, '(a,a)')    'dyn_readnl: se_vert_remap_uvTq_alg        = ',trim(se_vert_remap_uvTq_alg)
      write(iulog, '(a,a)')    'dyn_readnl: se_vert_remap_tracer_alg      = ',trim(se_vert_remap_tracer_alg)
      write(iulog, '(a,i0)')   'dyn_readnl: se_fvm_supercycling           = ',fvm_supercycling
      write(iulog, '(a,i0)')   'dyn_readnl: se_fvm_supercycling_jet       = ',fvm_supercycling_jet
      write(iulog, '(a,i0)')   'dyn_readnl: se_kmin_jet                   = ',kmin_jet
      write(iulog, '(a,i0)')   'dyn_readnl: se_kmax_jet                   = ',kmax_jet
      if (se_refined_mesh) then
         write(iulog, '(a)') 'dyn_readnl: Refined mesh simulation'
         write(iulog, '(a)') 'dyn_readnl: se_mesh_file = ',trim(se_mesh_file)
         if (hypervis_power /= 0) then
           write(iulog, '(a)') 'Using scalar viscosity (Zarzycki et al 2014 JClim)'
           write(iulog, '(a,e11.4)') 'dyn_readnl: se_hypervis_power = ',se_hypervis_power, ', (tensor hyperviscosity)'
           write(iulog, '(a,e11.4)') 'dyn_readnl: se_max_hypervis_courant = ',se_max_hypervis_courant
         end if
         if (hypervis_scaling /= 0) then
           write(iulog, '(a)') 'Using tensor viscosity (Guba et al., 2014)'
           write(iulog, '(a,e11.4)') 'dyn_readnl: se_hypervis_scaling = ',se_hypervis_scaling
         end if
      end if

      if (use_cslam) then
         write(iulog, '(a)') 'dyn_readnl: physics will run on FVM points; advection by CSLAM'
         write(iulog,'(a,i0)') 'dyn_readnl: se_fv_nphys = ', fv_nphys
      else
         write(iulog, '(a)') 'dyn_readnl: physics will run on SE GLL points'
      end if
      write(iulog, '(a,i0)') 'dyn_readnl: se_horz_num_threads = ',horz_num_threads
      write(iulog, '(a,i0)') 'dyn_readnl: se_vert_num_threads = ',vert_num_threads
      write(iulog, '(a,i0)') 'dyn_readnl: se_tracer_num_threads = ',tracer_num_threads
      if (trim(se_write_grid_file) == 'SCRIP') then
         write(iulog,'(2a)') "dyn_readnl: write SCRIP grid file = ", trim(se_grid_filename)
      else
         write(iulog,'(a)') "dyn_readnl: do not write grid file"
      end if
      write(iulog,'(a,l1)') 'dyn_readnl: write gll corners to SEMapping.nc = ', &
                            se_write_gll_corners
      write(iulog,'(a,l1)') 'dyn_readnl: write restart data on unstructured grid = ', &
                            se_write_restart_unstruct
      write(iulog, '(a,e9.2)') 'dyn_readnl: se_molecular_diff  = ', molecular_diff
   end if

   call native_mapping_readnl(NLFileName)

   !---------------------------------------------------------------------------
   contains
   !---------------------------------------------------------------------------

      integer function set_vert_remap( remap_T, remap_alg )

         use cam_abortutils, only: endrun
         use cam_logfile,    only: iulog

         ! Convert namelist input strings to the internally used integers.

         character(len=*), intent(in) :: remap_T    ! scheme for remapping temperature
         character(len=*), intent(in) :: remap_alg  ! remapping algorithm

         ! check valid remap_T values:
         if (remap_T /= 'thermal_energy_over_P' .and. remap_T /= 'Tv_over_logP') then
            write(iulog,*)'set_vert_remap: invalid remap_T= ',trim(remap_T)
            call endrun('set_vert_remap: invalid remap_T')
         end if

         select case (remap_alg)
         case ('PPM_bc_mirror')
            set_vert_remap = 1
         case ('PPM_bc_PCoM')
            set_vert_remap = 2
         case ('PPM_bc_linear_extrapolation')
            set_vert_remap = 10
         case ('FV3_PPM')
            if (remap_T == 'thermal_energy_over_P') then
               set_vert_remap = -4
            else
               set_vert_remap = -40
            end if
         case ('FV3_CS')
            if (remap_T == 'thermal_energy_over_P') then
               set_vert_remap = -9
            else
               set_vert_remap = -90
            end if
         case ('FV3_CS_2dz_filter')
            if (remap_T == 'thermal_energy_over_P') then
               set_vert_remap = -10
            else
               set_vert_remap = -100
            end if
         case ('FV3_non_monotone_CS_2dz_filter')
            if (remap_T == 'thermal_energy_over_P') then
               set_vert_remap = -11
            else
               set_vert_remap = -110
            end if
         case default
            write(iulog,*)'set_vert_remap: invalid remap_alg= ',trim(remap_alg)
            call endrun('set_vert_remap: invalid remap_alg')
         end select

      end function set_vert_remap

end subroutine dyn_readnl

!=========================================================================================

subroutine dyn_init(cam_runtime_opts, dyn_in, dyn_out)
   use runtime_obj,         only: runtime_options
   use cam_logfile,         only: iulog
   use dyn_grid,            only: elem, fvm, hvcoord
   use dyn_grid,            only: TimeLevel
   use cam_pio_utils,       only: clean_iodesc_list
   use cam_abortutils,      only: check_allocate
   use spmd_utils,          only: iam, masterproc
   use cam_constituents,    only: const_name, const_longname, num_advected
   use cam_constituents,    only: const_get_index, const_is_wet, const_qmin
   use cam_constituents,    only: const_diag_name
   use cam_initfiles,       only: initial_file_get_id, topo_file_get_id
   use cam_control_mod,     only: initial_run
   use air_composition,     only: thermodynamic_active_species_num, thermodynamic_active_species_idx
   use air_composition,     only: thermodynamic_active_species_idx_dycore
   use dynconst,            only: cpair, pstd
   use dyn_thermo,          only: get_molecular_diff_coef_reference
   use cam_history,         only: history_add_field
   use cam_history_support, only: horiz_only
   !use cam_history,        only: register_vector_field
   use gravity_waves_sources, only: gws_init
   use cam_thermo_formula, only: energy_formula_dycore, ENERGY_FORMULA_DYCORE_SE
   use physics_types,      only: dycore_energy_consistency_adjust
   use phys_vars_init_check, only: mark_as_initialized

   !SE dycore:
   use parallel_mod,       only: par
   use prim_advance_mod,   only: prim_advance_init
   use thread_mod,         only: horz_num_threads
   use hybrid_mod,         only: hybrid_t, get_loop_ranges, config_thread_region
   use dimensions_mod,     only: nu_scale_top, nlev, ntrac, qsize
   use dimensions_mod,     only: ksponge_end, kmvis_ref, kmcnd_ref,rho_ref,km_sponge_factor
   use dimensions_mod,     only: cnst_name_gll, cnst_longname_gll, use_cslam
   use dimensions_mod,     only: irecons_tracer_lev, irecons_tracer, kord_tr, kord_tr_cslam
   use prim_driver_mod,    only: prim_init2
   use se_dyn_time_mod,    only: time_at
   use control_mod,        only: runtype, nu_top, molecular_diff
   use control_mod,        only: vert_remap_uvTq_alg, vert_remap_tracer_alg
   use control_mod,        only: statediag_numtrac
   use std_atm_profile,    only: std_atm_height

   ! Dummy arguments:
   type(runtime_options), intent(inout)  :: cam_runtime_opts
   type(dyn_import_t),    intent(out)    :: dyn_in
   type(dyn_export_t),    intent(out)    :: dyn_out

   ! Local variables
   integer             :: nets, nete, ie, k, kmol_end, mfound
   real(r8), parameter :: Tinit = 300.0_r8
   real(r8)            :: press(1), ptop, tref,z(1)

   type(hybrid_t)      :: hybrid

   integer :: ixq, ixcldice, ixcldliq, ixrain, ixsnow, ixgraupel
   integer :: m_cnst, m
   integer :: iret
   character(len=cl) :: errmsg

   ! variables for initializing energy and axial angular momentum diagnostics
   integer, parameter                         :: num_stages = 14
   character (len = 4), dimension(num_stages) :: stage         = (/"dED","dAF","dBD","dBL","dAL","dAD","dAR","dBF","dBH","dCH","dAH","dBS","dAS","p2d"/)
   character (len = 70),dimension(num_stages) :: stage_txt = (/&
      " end of previous dynamics                           ",& !dED
      " from previous remapping or state passed to dynamics",& !dAF - state in beginning of nsplit loop
      " state after applying CAM forcing                   ",& !dBD - state after applyCAMforcing
      " before floating dynamics                           ",& !dBL
      " after floating dynamics                            ",& !dAL
      " before vertical remapping                          ",& !dAD - state before vertical remapping
      " after vertical remapping                           ",& !dAR - state at end of nsplit loop
      " state passed to parameterizations                  ",& !dBF
      " state before hypervis                              ",& !dBH
      " state after hypervis but before adding heating term",& !dCH
      " state after hypervis                               ",& !dAH
      " state before sponge layer diffusion                ",& !dBS - state before sponge del2
      " state after sponge layer diffusion                 ",& !dAS - state after sponge del2
      " phys2dyn mapping errors (requires ftype-1)         " & !p2d - for assessing phys2dyn mapping errors
      /)

   integer :: istage, ivars
   character (len=108) :: str1, str2, str3

   logical :: history_budget      ! output tendencies and state variables for budgets
   integer :: budget_hfile_num

   character(len=*), parameter :: subname = 'dyn_init'

   real(r8) :: km_sponge_factor_local(nlev+1)
   !----------------------------------------------------------------------------
   ! Set dynamical core energy formula for use in cam_thermo.
   energy_formula_dycore = ENERGY_FORMULA_DYCORE_SE
   call mark_as_initialized("total_energy_formula_for_dycore")

   ! Dynamical core energy is not consistent with CAM physics and requires
   ! temperature and temperature tendency adjustment at end of physics.
   dycore_energy_consistency_adjust = .true.
   call mark_as_initialized("flag_for_dycore_energy_consistency_adjustment")

   ! Set name of dycore in runtime object
   call cam_runtime_opts%set_dycore('se')

   ! Finalize statediag_numtrac now that the number of advected
   ! constituents is known (qsize is set by dimensions_mod_init, which runs
   ! after cam_register_constituents).
   statediag_numtrac = MIN(se_statediag_numtrac_save, qsize)
   if (masterproc) then
      write(iulog, '(a,i0)') 'dyn_init: final statediag_numtrac = ', statediag_numtrac
   end if

   ! Now allocate and set condenstate vars
   allocate(cnst_name_gll(qsize), stat=iret, errmsg=errmsg)
   call check_allocate(iret, subname, 'cnst_name_gll(qsize)', &
                       file=__FILE__, line=__LINE__, errmsg=errmsg)

   allocate(cnst_longname_gll(qsize), stat=iret, errmsg=errmsg)
   call check_allocate(iret, subname, 'cnst_longname_gll(qsize)', &
                       file=__FILE__, line=__LINE__, errmsg=errmsg)

   allocate(cnst_diag_name_gll(qsize), stat=iret, errmsg=errmsg)
   call check_allocate(iret, subname, 'cnst_diag_name_gll(qsize)', &
                       file=__FILE__, line=__LINE__, errmsg=errmsg)


   allocate(kord_tr(qsize), stat=iret, errmsg=errmsg)
   call check_allocate(iret, subname, 'kord_tr(qsize)', &
                       file=__FILE__, line=__LINE__, errmsg=errmsg)

   kord_tr(:) = vert_remap_tracer_alg
   if (use_cslam) then
     allocate(kord_tr_cslam(ntrac), stat=iret, errmsg=errmsg)
     call check_allocate(iret, subname, 'kord_tr_cslam(ntrac)', &
                         file=__FILE__, line=__LINE__, errmsg=errmsg)

     kord_tr_cslam(:) = vert_remap_tracer_alg
   end if

   do m=1,qsize
     !
     ! The "_gll" index variables below are used to keep track of condensate-loading tracers
     ! since they are not necessarily indexed contiguously and not necessarily in the same
     ! order (physics is in charge of the order)
     !
     ! if running with CSLAM then the SE (gll) condensate-loading water tracers are always
     ! indexed contiguously (q,cldliq,cldice,rain,snow,graupel) - see above
     !
     ! CSLAM tracers are always indexed as in physics
     ! of no CSLAM then SE tracers are always indexed as in physics
     !
     if (use_cslam) then
       !
       ! note that in this case qsize = thermodynamic_active_species_num
       !
       thermodynamic_active_species_idx_dycore(m) = m
       kord_tr_cslam(thermodynamic_active_species_idx(m)) = vert_remap_uvTq_alg
       kord_tr(m)                                 = vert_remap_uvTq_alg
       cnst_name_gll    (m)                       = const_name    (thermodynamic_active_species_idx(m))
       cnst_longname_gll(m)                       = const_longname(thermodynamic_active_species_idx(m))
       cnst_diag_name_gll(m)                      = const_diag_name(thermodynamic_active_species_idx(m))
     else
       !
       ! if not running with CSLAM then the condensate-loading water tracers are not necessarily
       ! indexed contiguously (are indexed as in physics)
       !
       if (m.le.thermodynamic_active_species_num) then
         thermodynamic_active_species_idx_dycore(m) = thermodynamic_active_species_idx(m)
         kord_tr(thermodynamic_active_species_idx_dycore(m)) = vert_remap_uvTq_alg
       end if
       cnst_name_gll(m)      = const_name(m)
       cnst_longname_gll(m)  = const_longname(m)
       cnst_diag_name_gll(m) = const_diag_name(m)
     end if
   end do
#ifdef energy_budget_code
   do m=1,thermodynamic_active_species_liq_num
     if (use_cslam) then
       do mfound=1,qsize
         if (TRIM(const_name(thermodynamic_active_species_liq_idx(m)))==TRIM(cnst_name_gll(mfound))) then
           thermodynamic_active_species_liq_idx_dycore(m) = mfound
         end if
       end do
     else
       thermodynamic_active_species_liq_idx_dycore(m) = thermodynamic_active_species_liq_idx(m)
     end if
     if (masterproc) then
       write(iulog,*) subname//": m,thermodynamic_active_species_idx_liq_dycore: ",m,thermodynamic_active_species_liq_idx_dycore(m)
     end if
   end do
   do m=1,thermodynamic_active_species_ice_num
     if (use_cslam) then
       do mfound=1,qsize
         if (TRIM(const_name(thermodynamic_active_species_ice_idx(m)))==TRIM(cnst_name_gll(mfound))) then
           thermodynamic_active_species_ice_idx_dycore(m) = mfound
         end if
       end do
     else
       thermodynamic_active_species_ice_idx_dycore(m) = thermodynamic_active_species_ice_idx(m)
     end if
     if (masterproc) then
       write(iulog,*) subname//": m,thermodynamic_active_species_idx_ice_dycore: ",m,thermodynamic_active_species_ice_idx_dycore(m)
     end if
   end do
#endif
   !
   ! Initialize the import/export objects
   !
   if(iam < par%nprocs) then
     dyn_in%elem  => elem
     dyn_in%fvm   => fvm

     dyn_out%elem => elem
     dyn_out%fvm  => fvm
   else
     nullify(dyn_in%elem)
     nullify(dyn_in%fvm)
     nullify(dyn_out%elem)
     nullify(dyn_out%fvm)
   end if

   call set_phis(dyn_in)

   if (initial_run) then
     call read_inidat(dyn_in)
#ifdef scam
     if (use_iop .and. masterproc) then
       call setiopupdate_init()
       call readiopdata( hvcoord%hyam, hvcoord%hybm, hvcoord%hyai, hvcoord%hybi, hvcoord%ps0 )
       call scm_setinitial(dyn_in%elem)
     end if
     call clean_iodesc
#endif
     call clean_iodesc_list()
   end if
   !
   ! initialize diffusion in dycore
   !
   kmol_end = 0
   if (molecular_diff>0) then
     !
     ! molecular diffusion and thermal conductivity reference values
     !
     if (masterproc) write(iulog,*) subname//": initialize molecular diffusion reference profiles"
     tref = 1000._r8     !mean value at model top for solar max
     km_sponge_factor = molecular_diff
     km_sponge_factor_local = molecular_diff
     !
     ! get rho, kmvis and kmcnd at mid-levels
     !
     call get_molecular_diff_coef_reference(tref,&
          (hvcoord%hyam(:)+hvcoord%hybm(:))*hvcoord%ps0,km_sponge_factor,&
          kmvis_ref,kmcnd_ref,rho_ref)

     if (masterproc) then
        write(iulog,*) "Molecular viscosity and thermal conductivity reference profile"
        write(iulog,*) "k, p, z, km_sponge_factor, kmvis_ref/rho_ref, kmcnd_ref/(cp*rho_ref):"
     end if
     do k=1,nlev
       ! only apply molecular viscosity where viscosity is > 1000 m/s^2
       if (MIN(kmvis_ref(k)/rho_ref(k),kmcnd_ref(k)/(cpair*rho_ref(k)))>1000.0_r8) then
         if (masterproc) then
           press = hvcoord%hyam(k)*hvcoord%ps0+hvcoord%hybm(k)*pstd
           call std_atm_height(press,z)
           write(iulog,'(i3,5e11.4)') k,press, z,km_sponge_factor(k),kmvis_ref(k)/rho_ref(k),kmcnd_ref(k)/(cpair*rho_ref(k))
         end if
         kmol_end = k
       else
         kmvis_ref(k) = 1.0_r8
         kmcnd_ref(k) = 1.0_r8
       end if
     end do
   else
     ! -1.0E6 is an arbitrary unrealistic value.  But it is used in the calculation
     ! of a diagnostic quantity in global_norms_mod so can't be set to huge or nan.
     kmvis_ref(:) = -1.0E6_r8
     kmcnd_ref(:) = -1.0E6_r8
     rho_ref(:)   = -1.0E6_r8
   end if
   !
   irecons_tracer_lev(:) = irecons_tracer !use high-order CSLAM in all layers
   !
   ! compute scaling of traditional sponge layer damping (following cd_core.F90 in CAM-FV)
   !
   nu_scale_top(:) = 0.0_r8
   if (nu_top>0) then
      ptop  = hvcoord%hyai(1)*hvcoord%ps0
      if (ptop>300.0_r8) then
         !
         ! for low tops the tanh formulae below makes the sponge excessively deep
         !
         nu_scale_top(1) = 4.0_r8
         nu_scale_top(2) = 2.0_r8
         nu_scale_top(3) = 1.0_r8
         ksponge_end = 3
      else if (ptop>100.0_r8) then
         !
         ! CAM6 top (~225 Pa) or CAM7 low top
         !
         ! For backwards compatibility numbers below match tanh profile
         ! used in FV
         !
         nu_scale_top(1) = 4.4_r8
         nu_scale_top(2) = 1.3_r8
         nu_scale_top(3) = 3.9_r8
         ksponge_end = 3
      else if (ptop>1e-1_r8) then
         !
         ! CAM7 FMT
         !
         nu_scale_top(1) = 3.0_r8
         nu_scale_top(2) = 1.0_r8
         nu_scale_top(3) = 0.1_r8
         nu_scale_top(4) = 0.05_r8
         ksponge_end = 4
      else if (ptop>1e-4_r8) then
         !
         ! WACCM and WACCM-x
         !
         nu_scale_top(1) = 5.0_r8
         nu_scale_top(2) = 5.0_r8
         nu_scale_top(3) = 5.0_r8
         nu_scale_top(4) = 2.0_r8
         nu_scale_top(5) = 1.0_r8
         nu_scale_top(6) = 0.1_r8
         ksponge_end = 6
      end if
   else
      ksponge_end = 0
   end if
   ksponge_end = MAX(MAX(ksponge_end,1),kmol_end)
   if (masterproc) then
     write(iulog,*) subname//": ksponge_end = ",ksponge_end
     write(iulog,*) subname//": sponge layer Laplacian damping"
     write(iulog,*) "k, p, z, nu_scale_top, nu (actual Laplacian damping coefficient)"
     if (nu_top>0) then
       do k=1,ksponge_end+1
         press = (hvcoord%hyam(k)+hvcoord%hybm(k))*hvcoord%ps0
         call std_atm_height(press,z)
         write(iulog,'(i3,4e11.4)') k,press,z,&
              nu_scale_top(k),nu_scale_top(k)*nu_top
       end do
     end if
   end if

   if (iam < par%nprocs) then
      call prim_advance_init(par,elem)
      !$OMP PARALLEL NUM_THREADS(horz_num_threads), DEFAULT(SHARED), PRIVATE(hybrid,nets,nete)
      hybrid = config_thread_region(par,'horizontal')
      call get_loop_ranges(hybrid, ibeg=nets, iend=nete)
      call prim_init2(elem, fvm, hybrid, nets, nete, TimeLevel, hvcoord)
      !$OMP END PARALLEL

      ! initialize gravity wave sources
      call gws_init(elem)
   end if  ! iam < par%nprocs

   call history_add_field ('nu_kmvis', 'Molecular viscosity Laplacian coefficient', 'lev', 'avg', '1',                gridname='GLL')
   call history_add_field ('nu_kmcnd', 'Thermal conductivity Laplacian coefficient', 'lev', 'avg', '1',               gridname='GLL')
   call history_add_field ('nu_kmcnd_dp', 'Thermal conductivity like Laplacian coefficient on dp', 'lev', 'avg', '1', gridname='GLL')

   ! Forcing from physics on the GLL grid
   call history_add_field ('FU', 'Zonal wind forcing term on GLL grid', 'lev', 'avg', 'm s-2',      gridname='GLL_hist')
   call history_add_field ('FV', 'Meridional wind forcing term on GLL grid', 'lev', 'avg', 'm s-2', gridname='GLL_hist')
   !call register_vector_field('FU', 'FV') !<-For interpolated history output (not yet implemented)
   call history_add_field ('FT', 'Temperature forcing term on GLL grid', 'lev', 'avg', 'K s-1',     gridname='GLL_hist')

   ! Tracer forcing on fvm (CSLAM) grid and internal CSLAM pressure fields
   if (use_cslam) then
      do m = 1, ntrac
         call history_add_field (trim(const_diag_name(m))//'_fvm', trim(const_longname(m)), 'lev', 'inst', 'kg/kg',   &
            gridname='FVM')

         call history_add_field ('F'//trim(const_diag_name(m))//'_fvm', &
           trim(const_longname(m))//' mixing ratio forcing term (q_new-q_old) on fvm grid', &
           'lev', 'inst', 'kg kg-1 s-1', gridname='FVM')
      end do

      call history_add_field ('dp_fvm' , 'CSLAM Pressure level thickness', 'lev', 'inst', 'Pa',   gridname='FVM')
      call history_add_field ('PSDRY_fvm', 'CSLAM dry surface pressure', horiz_only, 'inst','Pa', gridname='FVM')
   end if

   do m_cnst = 1, qsize
     call history_add_field ('F'//trim(cnst_diag_name_gll(m_cnst))//'_gll',  &
          trim(cnst_longname_gll(m_cnst))//' mixing ratio forcing term (q_new-q_old) on GLL grid', &
          'lev', 'inst', 'kg kg-1 s-1', gridname='GLL')
   end do

   ! Energy diagnostics and axial angular momentum diagnostics
   call history_add_field ('ABS_dPSdt', 'Absolute surface pressure tendency', horiz_only, 'avg', 'Pa s-1', gridname='GLL')

   if (use_cslam) then
#ifdef waccm_debug
     call history_add_field ('CSLAM_gamma',  'Courant number from CSLAM', 'lev', 'avg', '1', gridname='FVM')
#endif
     call history_add_field ('WV_PDC',   'Total column water vapor lost in physics-dynamics coupling', horiz_only, 'avg', 'kg m-2', gridname='FVM')
     call history_add_field ('WL_PDC',   'Total column cloud water lost in physics-dynamics coupling', horiz_only, 'avg', 'kg m-2', gridname='FVM')
     call history_add_field ('WI_PDC',   'Total column cloud ice lost in physics-dynamics coupling'  , horiz_only, 'avg', 'kg m-2', gridname='FVM')
     call history_add_field ('TT_PDC',   'Total column test tracer lost in physics-dynamics coupling', horiz_only, 'avg', 'kg m-2', gridname='FVM')
   else
     call history_add_field ('WV_PDC',   'Total column water vapor lost in physics-dynamics coupling', horiz_only, 'avg', 'kg m-2', gridname='GLL')
     call history_add_field ('WL_PDC',   'Total column cloud water lost in physics-dynamics coupling', horiz_only, 'avg', 'kg m-2', gridname='GLL')
     call history_add_field ('WI_PDC',   'Total column cloud ice lost in physics-dynamics coupling'  , horiz_only, 'avg', 'kg m-2', gridname='GLL')
     call history_add_field ('TT_PDC',   'Total column test tracer lost in physics-dynamics coupling', horiz_only, 'avg', 'kg m-2', gridname='GLL')
   end if

#ifdef energy_budget_code
   do istage = 1,SIZE(stage)
      do ivars=1,SIZE(vars)
         write(str1,*) TRIM(ADJUSTL(vars(ivars))),TRIM(ADJUSTL("_")),TRIM(ADJUSTL(stage(istage)))
         write(str2,*) TRIM(ADJUSTL(vars_descriptor(ivars))),&
             TRIM(ADJUSTL(" ")),TRIM(ADJUSTL(stage_txt(istage)))
         write(str3,*) TRIM(ADJUSTL(vars_unit(ivars)))
         if (ntrac>0.and.massv(ivars)) then
           call history_add_field (TRIM(ADJUSTL(str1)),   horiz_only, 'avg', TRIM(ADJUSTL(str3)),TRIM(ADJUSTL(str2)), gridname='FVM')
         else
           call history_add_field (TRIM(ADJUSTL(str1)),   horiz_only, 'avg', TRIM(ADJUSTL(str3)),TRIM(ADJUSTL(str2)), gridname='GLL')
         end if
      end do
   end do
#endif

#ifdef cam_thermo_history
   !
   ! add dynamical core tracer tendency output
   !
   if (use_cslam) then
     do m = 1, num_advected
       call history_add_field(tottnam(m), trim(const_name(m))//' horz + vert', 'lev','avg','kg kg-1 s-1',  &
            gridname='FVM')
     end do
   else
     do m = 1, num_advected
       call history_add_field(tottnam(m), trim(const_name(m))//' horz + vert', 'lev','avg','kg kg-1 s-1',  &
            gridname='GLL')
     end do
   end if
#endif

! Need SIMA-specific mechanism to automatically add groups of history fields (maybe
! via labels in history_add_field?). For now, just comment out all references to
! add_default.
#if 0
   call phys_getopts(history_budget_out=history_budget, history_budget_histfile_num_out=budget_hfile_num)
   if ( history_budget ) then
      call const_get_index('water_vapor_mixing_ratio_wrt_moist_air_and_condensed_water', ixq)
      call const_get_index('cloud_liquid_water_mixing_ratio_wrt_moist_air_and_condensed_water',  &
           ixcldliq)
      call const_get_index('cloud_ice_mixing_ratio_wrt_moist_air_and_condensed_water', ixcldice)
      call add_default(tottnam(     ixq), budget_hfile_num, ' ')
      call add_default(tottnam(ixcldliq), budget_hfile_num, ' ')
      call add_default(tottnam(ixcldice), budget_hfile_num, ' ')
   end if
#endif

  ! constituent indices for waccm-x
  if ( cam_runtime_opts%waccmx_option() == 'ionosphere' .or. &
       cam_runtime_opts%waccmx_option() == 'neutral' ) then
     call const_get_index('atomic_oxygen_mixing_ratio_wrt_total_mass',  ixo)
     call const_get_index('oxygen_mixing_ratio_wrt_total_mass', ixo2)
     call const_get_index('atomic_hydrogen_mixing_ratio_wrt_total_mass',  ixh)
     call const_get_index('hydrogen_mixing_ratio_wrt_total_mass', ixh2)
  end if

#ifdef cam_thermo_history
   if (thermo_budget_history) then
      ! Register stages for budgets
      do istage = 1, num_stages
         call cam_budget_em_snapshot(TRIM(ADJUSTL(stage(istage))), 'dyn', &
              longname=TRIM(ADJUSTL(stage_txt(istage))))
      end do
      !
      ! Register tendency (difference) budgets
      !
      call cam_budget_em_register('dEdt_floating_dyn'   ,'dAL','dBL','dyn','dif', &
                      longname="dE/dt floating dynamics (dAL-dBL)"               )
      call cam_budget_em_register('dEdt_vert_remap'     ,'dAR','dAD','dyn','dif', &
                      longname="dE/dt vertical remapping (dAR-dAD)"              )
      call cam_budget_em_register('dEdt_phys_tot_in_dyn','dBD','dAF','dyn','dif', &
                      longname="dE/dt physics tendency in dynamics (dBD-dAF)"    )
      call cam_budget_em_register('dEdt_del4'          ,'dCH','dBH','dyn','dif', &
                      longname="dE/dt del4 (dCH-dBH)"                            )
      call cam_budget_em_register('dEdt_del4_fric_heat','dAH','dCH','dyn','dif', &
                      longname="dE/dt del4 frictional heating (dAH-dCH)"         )
      call cam_budget_em_register('dEdt_del4_tot'      ,'dAH','dBH','dyn','dif', &
                      longname="dE/dt del4 + del4 frictional heating (dAH-dBH)"  )
      call cam_budget_em_register('dEdt_del2_sponge'   ,'dAS','dBS','dyn','dif', &
                      longname="dE/dt del2 sponge (dAS-dBS)"                     )
      !
      ! Register derived budgets
      !
      call cam_budget_em_register('dEdt_dycore'        ,'dEdt_floating_dyn','dEdt_vert_remap'   ,'dyn','sum', &
                      longname="dE/dt adiabatic dynamics"                              )
      call cam_budget_em_register('dEdt_del2_del4_tot' ,'dEdt_del4_tot'    ,'dEdt_del2_sponge'  ,'dyn','sum', &
                      longname="dE/dt explicit diffusion total"                        )
      call cam_budget_em_register('dEdt_residual'      ,'dEdt_floating_dyn','dEdt_del2_del4_tot','dyn','dif', &
                      longname="dE/dt residual (dEdt_floating_dyn-dEdt_del2_del4_tot)" )
   end if
   !
   ! add dynamical core tracer tendency output
   !
   if (use_cslam) then
     do m = 1, pcnst
       call history_add_field(tottnam(m), trim(const_name(m))//' horz + vert', 'lev','avg', &
           'kg kg-1 s-1', gridname='FVM')
     end do
   else
     do m = 1, pcnst
       call history_add_field(tottnam(m), trim(const_name(m))//' horz + vert', 'lev','avg', &
            'kg kg-1 s-1', gridname='GLL')
     end do
   end if
   call phys_getopts(history_budget_out=history_budget, history_budget_histfile_num_out=budget_hfile_num)

! Need SIMA-specific mechanism to automatically add groups of history fields (maybe
! via labels in history_add_field?). For now, just comment out all references to
! add_default.
#if 0
   if ( history_budget ) then
      call cnst_get_ind('CLDLIQ', ixcldliq)
      call cnst_get_ind('CLDICE', ixcldice)
      call add_default(tottnam(       1), budget_hfile_num, ' ')
      call add_default(tottnam(ixcldliq), budget_hfile_num, ' ')
      call add_default(tottnam(ixcldice), budget_hfile_num, ' ')
   end if
#endif
#endif
end subroutine dyn_init

!=========================================================================================

subroutine dyn_run(dyn_state)
   use air_composition,  only: thermodynamic_active_species_num, dry_air_species_num
   use air_composition,  only: thermodynamic_active_species_idx_dycore
   use cam_history,      only: is_history_field_active, history_out_field
   use spmd_utils,       only: iam
   use dyn_grid,         only: TimeLevel, hvcoord
   use time_manager,     only: get_step_size
   use cam_abortutils,   only: check_allocate

   !SE dycore:
   use parallel_mod,     only: par
   use prim_driver_mod,  only: prim_run_subcycle
   use dimensions_mod,   only: cnst_name_gll, nelemd, np, npsq
   use dimensions_mod,   only: nc, nlev, use_cslam, ntrac, qsize
   use se_dyn_time_mod,  only: tstep, nsplit, timelevel_qdp, tevolve
   use hybrid_mod,       only: hybrid_t, config_thread_region, get_loop_ranges
   use control_mod,      only: qsplit, rsplit, ftype_conserve
   use thread_mod,       only: horz_num_threads
   use bndry_mod,        only: bndry_exchange
#ifdef scam
   use scamMod,          only: single_column, use_3dfrc
   use se_single_column_mod, only: apply_SC_forcing,ie_scm
#else
   logical, parameter :: single_column = .false. !Always assume single-column mode is off.
   integer, parameter :: ie_scm = 1              !Make up a value so that the model compiles
#endif

   type(dyn_export_t), intent(inout) :: dyn_state

   type(hybrid_t)    :: hybrid
   integer           :: tl_f
   integer           :: n
   integer           :: nets, nete, ithr
   integer           :: i, ie, j, k, m, nq, m_cnst
   integer           :: n0_qdp, nsplit_local
   integer           :: iret
   logical           :: ldiag
   character(len=cl) :: errmsg

   real(r8), allocatable :: ftmp(:,:,:)
   real(r8) :: dtime
   real(r8) :: rec2dt, pdel

   real(r8), allocatable, dimension(:,:,:) :: ps_before
   real(r8), allocatable, dimension(:,:,:) :: abs_ps_tend
   real (kind=r8)                          :: omega_cn(2,nelemd) !min and max of vertical Courant number
   integer                                 :: nets_in,nete_in

   character(len=*), parameter :: subname = 'dyn_run'

   !----------------------------------------------------------------------------

   nsplit_local = nsplit
   tevolve = 0._r8

   if (iam >= par%nprocs) return

   ldiag = is_history_field_active('ABS_dPSdt')
   if (ldiag) then
      allocate(ps_before(np,np,nelemd), stat=iret, errmsg=errmsg)
      call check_allocate(iret, subname, 'ps_before(np,np,nelemd)', &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)

      allocate(abs_ps_tend(np,np,nelemd), stat=iret, errmsg=errmsg)
      call check_allocate(iret, subname, 'abs_ps_tend(np,np,nelemd)', &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)

   end if

   !$OMP PARALLEL NUM_THREADS(horz_num_threads), DEFAULT(SHARED), PRIVATE(hybrid,nets,nete,n,ie,m,i,j,k,ftmp)
   hybrid = config_thread_region(par,'horizontal')
   call get_loop_ranges(hybrid, ibeg=nets, iend=nete)

   dtime = get_step_size()
   rec2dt = 1._r8/dtime

   tl_f = TimeLevel%n0   ! timelevel which was adjusted by physics
   call TimeLevel_Qdp(TimeLevel, qsplit, n0_qdp)!get n0_qdp for diagnostics call

   ! output physics forcing
   if (is_history_field_active('FU') .or. is_history_field_active('FV') .or. is_history_field_active('FT')) then

      allocate(ftmp(npsq*nelemd,nlev,3),stat=iret, errmsg=errmsg)
      call check_allocate(iret, subname, 'ftmp(npsq*nelemd,nlev,3)', &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)

      n = 0
      do ie = nets, nete
         do k = 1, nlev
            do j = 1, np
               do i = 1, np
                  ftmp(n+(i+(j-1)*np),k,1) = dyn_state%elem(ie)%derived%FM(i,j,1,k)
                  ftmp(n+(i+(j-1)*np),k,2) = dyn_state%elem(ie)%derived%FM(i,j,2,k)
                  ftmp(n+(i+(j-1)*np),k,3) = dyn_state%elem(ie)%derived%FT(i,j,k)
               end do
            end do
         end do

         !Update element count:
         n = n + npsq
      end do

      ! Write fields to history tape:
      call history_out_field('FU', ftmp(:,:,1))
      call history_out_field('FV', ftmp(:,:,2))
      call history_out_field('FT', ftmp(:,:,3))

      ! Deallocate temporary output variable:
      deallocate(ftmp)

   end if

   do m = 1, qsize
     if (is_history_field_active('F'//trim(cnst_diag_name_gll(m))//'_gll')) then
       do ie = nets, nete
         call history_out_field('F'//trim(cnst_diag_name_gll(m))//'_gll',&
              RESHAPE(dyn_state%elem(ie)%derived%FQ(:,:,:,m), (/np*np,nlev/)))
       end do
     end if
   end do

   ! convert elem(ie)%derived%fq to mass tendency
   if (.not.use_cslam) then
     do ie = nets, nete
       do m = 1, qsize
         do k = 1, nlev
           do j = 1, np
             do i = 1, np
               dyn_state%elem(ie)%derived%FQ(i,j,k,m) = dyn_state%elem(ie)%derived%FQ(i,j,k,m)* &
                    rec2dt*dyn_state%elem(ie)%state%dp3d(i,j,k,tl_f)
             end do
           end do
         end do
       end do
     end do
   end if
   if (ftype_conserve>0.and..not.use_cslam) then
     do ie = nets, nete
       do k=1,nlev
         do j=1,np
           do i = 1, np
             pdel = dyn_state%elem(ie)%state%dp3d(i,j,k,tl_f)
             do nq=dry_air_species_num+1,thermodynamic_active_species_num
               m_cnst = thermodynamic_active_species_idx_dycore(nq)
               pdel = pdel + (dyn_state%elem(ie)%state%qdp(i,j,k,m_cnst,n0_qdp)+dyn_state%elem(ie)%derived%FQ(i,j,k,m_cnst)*dtime)
             end do
             dyn_state%elem(ie)%derived%FDP(i,j,k) = pdel
           end do
         end do
       end do
     end do
   end if
   if (use_cslam) then
     do ie = nets, nete
       do m = 1, ntrac
         do k = 1, nlev
           do j = 1, nc
             do i = 1, nc
               dyn_state%fvm(ie)%fc(i,j,k,m) = dyn_state%fvm(ie)%fc(i,j,k,m)* &
                    rec2dt!*dyn_state%fvm(ie)%dp_fvm(i,j,k)
             end do
           end do
         end do
       end do
     end do
   end if
   if (ldiag) then
      abs_ps_tend(:,:,nets:nete) = 0.0_r8
   endif
   do n = 1, nsplit_local

      if (ldiag) then
         do ie = nets, nete
            ps_before(:,:,ie) = dyn_state%elem(ie)%state%psdry(:,:)
         end do
      end if

      if (single_column) then
         nets_in=ie_scm
         nete_in=ie_scm
      else
         nets_in=nets
         nete_in=nete
      end if

      ! forward-in-time RK, with subcycling
      call prim_run_subcycle(dyn_state%elem, dyn_state%fvm, hybrid, nets_in, nete_in, &
                             tstep, TimeLevel, hvcoord, n, single_column, omega_cn)

      if (ldiag) then
         do ie = nets, nete
            abs_ps_tend(:,:,ie) = abs_ps_tend(:,:,ie) +                                &
               ABS(ps_before(:,:,ie)-dyn_state%elem(ie)%state%psdry(:,:)) &
               /(tstep*qsplit*rsplit)
         end do
      end if

   end do

   if (ldiag) then
      do ie=nets,nete
         abs_ps_tend(:,:,ie)=abs_ps_tend(:,:,ie)/DBLE(nsplit)
         call history_out_field('ABS_dPSdt',RESHAPE(abs_ps_tend(:,:,ie),(/npsq/)))
      end do
   end if

   !$OMP END PARALLEL

   if (ldiag) then
      deallocate(ps_before,abs_ps_tend)
   endif
#ifdef SCAM
   if (single_column) then
      call apply_SC_forcing(dyn_state%elem,hvcoord,TimeLevel,3,.false.)
   end if
#endif

   ! output vars on CSLAM fvm grid
   call write_dyn_vars(dyn_state)

end subroutine dyn_run

!===============================================================================

subroutine dyn_final(DYN_STATE, RESTART_FILE)

   !SE dycore:
   use element_mod, only: elem_state_t

   type (elem_state_t), target     :: DYN_STATE
   character(LEN=*)   , intent(IN) :: RESTART_FILE

end subroutine dyn_final

!===============================================================================

subroutine read_inidat(dyn_in)
   use pio,                  only: file_desc_t
   use pio,                  only: pio_seterrorhandling, PIO_BCAST_ERROR
   use air_composition,      only: thermodynamic_active_species_num, dry_air_species_num
   use air_composition,      only: thermodynamic_active_species_idx
   use shr_sys_mod,          only: shr_sys_flush
   use hycoef,               only: hyai, hybi, ps0
   use phys_vars_init_check, only: mark_as_initialized
   use cam_history_support,  only: max_fieldname_len
   use spmd_utils,           only: iam, masterproc
   use cam_abortutils,       only: endrun, check_allocate
   use cam_logfile,          only: iulog
   use dyn_grid,             only: ini_grid_name, hvcoord
   use cam_map_utils,        only: iMap
   use cam_grid_support,     only: cam_grid_id, cam_grid_get_gcid, cam_grid_get_latvals
   use cam_grid_support,     only: cam_grid_get_lonvals, cam_grid_dimensions, max_hcoordname_len
   use inic_analytic,        only: analytic_ic_active, analytic_ic_set_ic
   use cam_initfiles,        only: pertlim, initial_file_get_id, topo_file_get_id
   use cam_constituents,     only: num_advected, const_name
   use cam_constituents,     only: const_is_water_species, const_qmin, const_is_wet
   use dyn_tests_utils,      only: vcoord=>vc_dry_pressure

   !This should eventually be replaced with the "const_diag_name" function from "cam_constituents".
   !The only issue is that right now atmospheric_physics doesn't add diagnostic names for all water
   !species variables.
   use phys_vars_init_check, only: phys_var_num, phys_var_stdnames, input_var_names
   use physics_data,         only: find_input_name_idx

   !SE-dycore:
   use parallel_mod,         only: par
   use element_mod,          only: timelevels
   use fvm_mapping,          only: dyn2fvm_mass_vars
   use bndry_mod,            only: bndry_exchange
   use control_mod,          only: runtype,initial_global_ave_dry_ps
   use prim_driver_mod,      only: prim_set_dry_mass
   use cam_initfiles,        only: scale_dry_air_mass
   use edgetype_mod,         only: edgebuffer_t
   use edge_mod,             only: initEdgeBuffer, FreeEdgeBuffer, edgeVpack, edgeVunpack
   use dimensions_mod,       only: np, npsq, nc, ntrac, qsize
   use dimensions_mod,       only: nlev, nelemd, use_cslam

   ! Arguments
   type (dyn_import_t), target, intent(inout) :: dyn_in   ! dynamics import

   ! Local variables

   integer(iMap), pointer           :: ldof(:) ! Basic (2D) grid dof

   type(file_desc_t), pointer       :: fh_ini, fh_topo

   type(element_t), pointer         :: elem(:)

   real(r8), allocatable            :: qtmp(:,:,:,:,:)    ! (np,np,nlev,nelemd,n)
   real(r8), allocatable            :: dbuf2(:,:)         ! (npsq,nelemd)
   real(r8), allocatable            :: dbuf3(:,:,:)       ! (npsq,nlev,nelemd)
   real(r8), allocatable            :: phis_tmp(:,:)      ! (npsp,nelemd)
   real(r8), allocatable            :: factor_array(:,:,:,:) ! (np,np,nlev,nelemd)
   logical,  allocatable            :: pmask(:)           ! (npsq*nelemd) unique grid vals

   character(len=max_hcoordname_len):: grid_name
   real(r8), allocatable            :: latvals(:),latvals_phys(:)
   real(r8), allocatable            :: lonvals(:),lonvals_phys(:)
   real(r8), pointer                :: latvals_deg(:)
   real(r8), pointer                :: lonvals_deg(:)

   integer                          :: ie, k, t
   character(len=max_fieldname_len) :: fieldname, fieldname2
   logical                          :: found
   logical                          :: inic_wet           ! true if initial condition is based on
                                                          ! wet pressure and water species
   integer                          :: kptr, m_cnst
   type(EdgeBuffer_t)               :: edge

   character(len=max_fieldname_len) :: dimname, varname
   integer                          :: ierr

   integer                          :: rndm_seed_sz
   integer, allocatable             :: rndm_seed(:)
   integer                          :: dims(2)
   integer                          :: pio_errtype
   real(r8)                         :: pertval
   integer                          :: i, j, indx, nq
   integer                          :: dyn_cols
   character(len=cl)                :: errmsg
   character(len=*), parameter      :: subname='READ_INIDAT'

   character(len=cl), allocatable   :: const_ic_name(:)
   character(len=cl)                :: std_name
   integer                          :: const_ic_names_idx

   ! fvm vars
   real(r8), allocatable            :: inv_dp_darea_fvm(:,:,:)
   real(r8)                         :: min_val, max_val

   real(r8)                         :: dp_tmp, pstmp(np,np)

   ! Variables for analytic initial conditions
   integer,  allocatable            :: glob_ind(:)
   integer,  allocatable            :: m_ind(:)
   real(r8), allocatable            :: dbuf4(:,:,:,:)
   !----------------------------------------------------------------------------

   fh_ini  => initial_file_get_id()
   fh_topo => topo_file_get_id()
   if (iam < par%nprocs) then
      elem => dyn_in%elem
   else
      nullify(elem)
   end if

   allocate(qtmp(np,np,nlev,nelemd,num_advected), stat=ierr, errmsg=errmsg)
   call check_allocate(ierr, subname, 'qtmp(np,np,nlev,nelemd,num_advected)', &
                       file=__FILE__, line=__LINE__, errmsg=errmsg)

   qtmp = 0._r8

   ! Set mask to indicate which columns are active
   nullify(ldof)
   call cam_grid_get_gcid(cam_grid_id(ini_grid_name), ldof)
   allocate(pmask(npsq*nelemd), stat=ierr, errmsg=errmsg)
   call check_allocate(ierr, subname, 'pmask(npsq*nelemd)', &
                       file=__FILE__, line=__LINE__, errmsg=errmsg)

   pmask(:) = (ldof /= 0)

   ! lat/lon needed in radians
   latvals_deg => cam_grid_get_latvals(cam_grid_id(ini_grid_name))
   lonvals_deg => cam_grid_get_lonvals(cam_grid_id(ini_grid_name))
   allocate(latvals(np*np*nelemd), stat=ierr, errmsg=errmsg)
   call check_allocate(ierr, subname, 'latvals(np*np*nelemd)', &
                       file=__FILE__, line=__LINE__, errmsg=errmsg)

   allocate(lonvals(np*np*nelemd), stat=ierr, errmsg=errmsg)
   call check_allocate(ierr, subname, 'lonvals(np*np*nelemd)', &
                       file=__FILE__, line=__LINE__, errmsg=errmsg)

   latvals(:) = latvals_deg(:)*deg2rad
   lonvals(:) = lonvals_deg(:)*deg2rad

   ! Set PIO to return error codes when reading data from IC file.
   call pio_seterrorhandling(fh_ini, PIO_BCAST_ERROR, pio_errtype)

   ! The grid name is defined in dyn_grid::define_cam_grids.
   ! Get the number of columns in the global GLL grid.
   call cam_grid_dimensions(ini_grid_name, dims)
   dyn_cols = dims(1)

   ! Set ICs.  Either from analytic expressions or read from file.

   if (analytic_ic_active() .and. (iam < par%nprocs)) then

      ! PHIS has already been set by set_phis.  Get local copy for
      ! possible use in setting T and PS in the analytic IC code.
      allocate(phis_tmp(npsq,nelemd), stat=ierr)
      call check_allocate(ierr, subname, 'phis_tmp(npsq,nelemd)', &
                          file=__FILE__, line=__LINE__)

      do ie = 1, nelemd
         k = 1
         do j = 1, np
            do i = 1, np
               phis_tmp(k,ie) = elem(ie)%state%phis(i,j)
               k = k + 1
            end do
         end do
      end do

      inic_wet = .false.
      allocate(glob_ind(npsq * nelemd), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'glob_ind(npsq*nelemd)', &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)

      j = 1
      do ie = 1, nelemd
         do i = 1, npsq
            ! Create a global(ish) column index
            glob_ind(j) = elem(ie)%GlobalId
            j = j + 1
         end do
      end do

      ! First, initialize all the variables, then assign
      allocate(dbuf4(npsq, nlev, nelemd, (qsize + 4)), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'dbuf4(npsq,nlev,nelemd,(qsize+4))', &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)

      dbuf4 = 0.0_r8
      allocate(m_ind(qsize), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'm_ind(qsize)', &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)

      do m_cnst = 1, qsize
         m_ind(m_cnst) = thermodynamic_active_species_idx(m_cnst)
      end do

      ! Init tracers on the GLL grid.  Note that analytic_ic_set_ic makes
      ! use of cnst_init_default for the tracers except water vapor.

      call analytic_ic_set_ic(vcoord, latvals, lonvals, glob_ind,  &
         PS=dbuf4(:,1,:,(qsize+1)), U=dbuf4(:,:,:,(qsize+2)),      &
         V=dbuf4(:,:,:,(qsize+3)), T=dbuf4(:,:,:,(qsize+4)),       &
         Q=dbuf4(:,:,:,1:qsize), m_cnst=m_ind, mask=pmask(:),      &
         PHIS_IN=PHIS_tmp)

      ! Deallocate variables that are no longer used:
      deallocate(glob_ind)
      deallocate(phis_tmp)

      do ie = 1, nelemd
         indx = 1
         do j = 1, np
            do i = 1, np
               ! PS
               elem(ie)%state%psdry(i,j) = dbuf4(indx, 1, ie, (qsize+1))
               ! U
               elem(ie)%state%v(i,j,1,:,1) = dbuf4(indx, :, ie, (qsize+2))
               ! V
               elem(ie)%state%v(i,j,2,:,1) = dbuf4(indx, :, ie, (qsize+3))
               ! T
               elem(ie)%state%T(i,j,:,1) = dbuf4(indx, :, ie, (qsize+4))
               indx = indx + 1
            end do
         end do
      end do

      ! Tracers to be advected on GLL grid.
      ! Note that fvm tracers are initialized below.
      do m_cnst = 1, qsize
         do ie = 1, nelemd
            qtmp(:,:,:,ie,m_cnst) = 0.0_r8
            indx = 1
            do j = 1, np
               do i = 1, np
                  ! Set qtmp at the unique columns only
                  if (pmask(((ie - 1) * npsq) + indx)) then
                     qtmp(i,j,:,ie,m_ind(m_cnst)) = dbuf4(indx, :, ie, m_cnst)
                  end if
                  indx = indx + 1
               end do
            end do
         end do
      end do

      ! Deallocate variables that are not longer used:
      deallocate(m_ind)
      deallocate(dbuf4)

   else

      ! Read ICs from file.  Assume all fields in the initial file are on the GLL grid.

      allocate(dbuf2(npsq,nelemd), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'dbuf2(npsq,nelemd)', &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)

      allocate(dbuf3(npsq,nlev,nelemd), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'dbuf3(npsq,nlev,nelemd)', &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)

      ! Check that number of columns in IC file matches grid definition.
      call check_file_layout(fh_ini, elem, dyn_cols, 'ncdata', .true., dimname)

      ! Read 2-D field

      fieldname  = 'PS'
      fieldname2 = 'PSDRY'
      if (dyn_field_exists(fh_ini, trim(fieldname), required=.false.)) then
         inic_wet = .true.
         call read_dyn_var(trim(fieldname), fh_ini, dimname, dbuf2)
      elseif (dyn_field_exists(fh_ini, trim(fieldname2), required=.false.)) then
         inic_wet = .false.
         call read_dyn_var(trim(fieldname2), fh_ini, dimname, dbuf2)
      else
         call endrun(trim(subname)//': PS or PSDRY must be on GLL grid')
      end if
#ifndef planet_mars
      if (iam < par%nprocs) then
         if (minval(dbuf2, mask=reshape(pmask, (/npsq,nelemd/))) < 10000._r8) then
            call endrun(trim(subname)//': Problem reading ps or psdry field -- bad values')
         end if
      end if
#endif
      do ie = 1, nelemd
         indx = 1
         do j = 1, np
            do i = 1, np
               elem(ie)%state%psdry(i,j) = dbuf2(indx,ie) ! can be either wet or dry ps
               indx = indx + 1
            end do
         end do
      end do

      ! Read in 3-D fields

      if (dyn_field_exists(fh_ini, 'U')) then
         call read_dyn_var('U', fh_ini, dimname, dbuf3)
      else
         call endrun(trim(subname)//': U not found')
      end if
      do ie = 1, nelemd
         elem(ie)%state%v = 0.0_r8
         indx = 1
         do j = 1, np
            do i = 1, np
               elem(ie)%state%v(i,j,1,:,1) = dbuf3(indx,:,ie)
               indx = indx + 1
            end do
         end do
      end do

      if (dyn_field_exists(fh_ini, 'V')) then
         call read_dyn_var('V', fh_ini, dimname, dbuf3)
      else
         call endrun(trim(subname)//': V not found')
      end if
      do ie = 1, nelemd
         indx = 1
         do j = 1, np
            do i = 1, np
               elem(ie)%state%v(i,j,2,:,1) = dbuf3(indx,:,ie)
               indx = indx + 1
            end do
         end do
      end do

      if (dyn_field_exists(fh_ini, 'T')) then
         call read_dyn_var('T', fh_ini, dimname, dbuf3)
      else
         call endrun(trim(subname)//': T not found')
      end if
      do ie=1,nelemd
         elem(ie)%state%T = 0.0_r8
         indx = 1
         do j = 1, np
            do i = 1, np
               elem(ie)%state%T(i,j,:,1) = dbuf3(indx,:,ie)
               indx = indx + 1
            end do
         end do
      end do

      if (pertlim .ne. 0.0_r8) then
         if (masterproc) then
            write(iulog,*) trim(subname), ': Adding random perturbation bounded', &
               'by +/- ', pertlim, ' to initial temperature field'
         end if

         call random_seed(size=rndm_seed_sz)
         allocate(rndm_seed(rndm_seed_sz), stat=ierr, errmsg=errmsg)
         call check_allocate(ierr, subname, 'rndm_seed(rndm_seed_sz)', &
                             file=__FILE__, line=__LINE__, errmsg=errmsg)

         do ie = 1, nelemd
            ! seed random number generator based on element ID
            ! (possibly include a flag to allow clock-based random seeding)
            rndm_seed = elem(ie)%GlobalId
            call random_seed(put=rndm_seed)
            do i = 1, np
               do j = 1, np
                  do k = 1, nlev
                     call random_number(pertval)
                     pertval = 2.0_r8*pertlim*(0.5_r8 - pertval)
                     elem(ie)%state%T(i,j,k,1) = elem(ie)%state%T(i,j,k,1)*(1.0_r8 + pertval)
                  end do
               end do
            end do
         end do

         deallocate(rndm_seed)
      end if

      ! Cleanup
      deallocate(dbuf2)
      deallocate(dbuf3)

   end if ! analytic_ic_active

   ! Read in or cold-initialize all the tracer fields.
   ! Data is read in on the GLL grid.
   ! Both GLL and FVM tracer fields are initialized based on the
   ! dimension qsize or ntrac for GLL or FVM tracers respectively.
   ! Data is only read in on GLL so if FVM tracers are active,
   ! interpolation is performed.
   !
   ! If analytic ICs are being used, we allow constituents in an initial
   ! file to overwrite mixing ratios set by the default constituent initialization
   ! except for the water species.

   if (ntrac > qsize) then
      if (ntrac < num_advected) then
         write(errmsg, '(a,3(i0,a))') ': ntrac (',ntrac,') > qsize (',qsize, &
            ') but < num_advected (',num_advected,')'
         call endrun(trim(subname)//errmsg)
      end if
   else if (qsize < num_advected) then
      write(errmsg, '(a,2(i0,a))') ': qsize (',qsize,') < num_advected (',num_advected,')'
      call endrun(trim(subname)//errmsg)
   end if

   ! Generate list of all advected constituent input names.  Note that the CCPP
   ! Constituents array automatically packs all advected species to the beginning
   ! of the list:
   !------------
   allocate(const_ic_name(num_advected), stat=ierr, errmsg=errmsg)
   call check_allocate(ierr, subname, 'const_ic_name(num_advected)', &
                       file=__FILE__, line=__LINE__, errmsg=errmsg)

   ! Initialize to variable name that will likely never be found in the IC file:
   const_ic_name(:) = 'NONAME_NEVERFOUND'

   do m_cnst = 1, num_advected

      ! Extract constituent standard name:
      std_name = const_name(m_cnst)

      ! Find input name array index to extract correct input names:
      const_ic_names_idx = -1
      do k=1, phys_var_num
         if(trim(phys_var_stdnames(k)) == trim(std_name)) then
            const_ic_names_idx = k
            exit
         end if
      end do

      ! Skip to next constituent if not found in input names list:
      if (const_ic_names_idx < 0) cycle

      ! The first name in IC names list should be the correct
      ! name for standard CAM IC (ncdata) files:
      const_ic_name(m_cnst) = input_var_names(1, const_ic_names_idx)
   end do
   !-------------

   ! If using analytic ICs the initial file only needs the horizonal grid
   ! dimension checked in the case that the file contains constituent mixing
   ! ratios.

   do m_cnst = 1, num_advected
      if (.not. const_is_water_species(m_cnst)) then
         if (dyn_field_exists(fh_ini, trim(const_ic_name(m_cnst)), required=.false.)) then
            call check_file_layout(fh_ini, elem, dyn_cols, 'ncdata', .true., dimname)
            exit
         end if
      end if
   end do

   allocate(dbuf3(npsq,nlev,nelemd), stat=ierr, errmsg=errmsg)
   call check_allocate(ierr, subname, 'dbuf3(npsq,nlev,nelemd)', &
                       file=__FILE__, line=__LINE__, errmsg=errmsg)

   do m_cnst = 1, num_advected

      if (analytic_ic_active() .and. const_is_water_species(m_cnst)) cycle

      found = dyn_field_exists(fh_ini, trim(const_ic_name(m_cnst)), required=.false.)

      if (found) then
         call read_dyn_var(trim(const_ic_name(m_cnst)), fh_ini, dimname, dbuf3)
      end if

      do ie = 1, nelemd
         ! Copy tracers defined on GLL grid into Eulerian array
         ! Make sure tracers have at least minimum value
         do k=1, nlev
            indx = 1
            do j = 1, np
               do i = 1, np
                  ! Set qtmp at the unique columns only: zero non-unique columns
                  if (pmask(((ie - 1) * npsq) + indx)) then
                     qtmp(i,j, k, ie, m_cnst) = max(const_qmin(m_cnst),dbuf3(indx,k,ie))
                  else
                     qtmp(i,j, k, ie, m_cnst) = 0.0_r8
                  end if
                  indx = indx + 1
               end do
            end do
         end do
      end do

   end do ! num_advected

   ! Cleanup
   deallocate(dbuf3)

   ! Put the error handling back the way it was
   call pio_seterrorhandling(fh_ini, pio_errtype)

   ! Cleanup
   deallocate(pmask)
   deallocate(latvals)
   deallocate(lonvals)

   if (associated(ldof)) then
      deallocate(ldof)
      nullify(ldof)
   end if

   ! once we've read or initialized all the fields we do a boundary exchange to
   ! update the redundent columns in the dynamics
   if(iam < par%nprocs) then
      call initEdgeBuffer(par, edge, elem, (3+num_advected)*nlev + 2 )
   end if
   do ie = 1, nelemd
      kptr = 0
      call edgeVpack(edge, elem(ie)%state%psdry,1,kptr,ie)
      kptr = kptr + 1
      call edgeVpack(edge, elem(ie)%state%v(:,:,:,:,1),2*nlev,kptr,ie)
      kptr = kptr + (2 * nlev)
      call edgeVpack(edge, elem(ie)%state%T(:,:,:,1),nlev,kptr,ie)
      kptr = kptr + nlev
      call edgeVpack(edge, qtmp(:,:,:,ie,:),nlev*num_advected,kptr,ie)
   end do
   if(iam < par%nprocs) then
      call bndry_exchange(par,edge,location='read_inidat')
   end if
   do ie = 1, nelemd
      kptr = 0
      call edgeVunpack(edge, elem(ie)%state%psdry,1,kptr,ie)
      kptr = kptr + 1
      call edgeVunpack(edge, elem(ie)%state%v(:,:,:,:,1),2*nlev,kptr,ie)
      kptr = kptr + (2 * nlev)
      call edgeVunpack(edge, elem(ie)%state%T(:,:,:,1),nlev,kptr,ie)
      kptr = kptr + nlev
      call edgeVunpack(edge, qtmp(:,:,:,ie,:),nlev*num_advected,kptr,ie)
   end do

   if (inic_wet) then
      !
      ! convert to dry
      !
      ! (this has to be done after edge-exchange since shared points between elements are only
      !  initialized in one element and not the other!)
      !
      if (par%masterproc) then
         write(iulog,*) 'Convert specific/wet mixing ratios to dry'
      end if

      allocate(factor_array(np,np,nlev,nelemd), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'factor_array(np,np,nlev,nelemd)', &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)

      !
      ! compute: factor_array = 1/(1-sum(q))
      !
      factor_array(:,:,:,:) = 1.0_r8
      do ie = 1, nelemd
         do k = dry_air_species_num+1, thermodynamic_active_species_num
            m_cnst = thermodynamic_active_species_idx(k)
            factor_array(:,:,:,ie) = factor_array(:,:,:,ie) - qtmp(:,:,:,ie,m_cnst)
         end do
      end do
      factor_array(:,:,:,:) = 1.0_r8/factor_array(:,:,:,:)

      do m_cnst = 1, num_advected
         if (const_is_wet(m_cnst)) then
            do ie = 1, nelemd
               do k = 1, nlev
                  do j = 1, np
                     do i = 1, np
                        ! convert wet mixing ratio to dry
                        qtmp(i,j,k,ie,m_cnst) = qtmp(i,j,k,ie,m_cnst) * factor_array(i,j,k,ie)

                        ! truncate negative values if they were not analytically specified
                        if (.not. analytic_ic_active()) then
                           qtmp(i,j,k,ie,m_cnst) = max(const_qmin(m_cnst), qtmp(i,j,k,ie,m_cnst))
                        end if
                     end do
                  end do
               end do
            end do
         end if
      end do

      ! initialize dp3d and qdp
      !
      ! compute: factor_array = 1/(1+sum(q))

      factor_array(:,:,:,:) = 1.0_r8
      do ie = 1, nelemd
         do k = dry_air_species_num+1, thermodynamic_active_species_num
            m_cnst = thermodynamic_active_species_idx(k)
            factor_array(:,:,:,ie) = factor_array(:,:,:,ie) + qtmp(:,:,:,ie,m_cnst)
         end do
      end do
      factor_array(:,:,:,:) = 1.0_r8/factor_array(:,:,:,:)
      do ie = 1, nelemd
         ! pstmp is the wet ps
         pstmp = elem(ie)%state%psdry(:,:)
         ! start accumulating the dry air pressure differences across each layer
         elem(ie)%state%psdry(:,:) = hyai(1)*ps0
         do k=1,nlev
            do j = 1,np
               do i = 1,np
                  dp_tmp = ((hyai(k+1) - hyai(k))*ps0) + &
                           ((hybi(k+1) - hybi(k))*pstmp(i,j))
                  if (.not. analytic_ic_active()) then

                     ! if analytic_ic then the surface pressure is already dry
                     ! (note that it is not correct to convert to moist pressure
                     ! in analytic_ic and not have the #ifndef statement here
                     ! since the dry levels are in a different location than
                     ! what is obtained from algorithm below)

                     ! convert dp_tmp to dry
                     dp_tmp = dp_tmp*factor_array(i,j,k,ie)
                  end if

                  elem(ie)%state%dp3d(i,j,k,:) = dp_tmp

                  ! compute dry surface pressure; note that at this point
                  !
                  ! dp3d .NE. (hyai(k+1) - hyai(k))*ps0 + (hybi(k+1) - hybi(k))*ps(i,j)

                  elem(ie)%state%psdry(i,j) = elem(ie)%state%psdry(i,j)+elem(ie)%state%dp3d(i,j,k,1)
               end do
            end do
         end do
      end do

      deallocate(factor_array)

   else

      ! initial condition is based on dry surface pressure and constituents
      !
      ! we only need to initialize state%dp3d

      do ie = 1, nelemd
         do k = 1, nlev
            do j = 1, np
               do i = 1, np
                  elem(ie)%state%dp3d(i,j,k,:) = (hyai(k+1) - hyai(k))*ps0 + &
                                                 (hybi(k+1) - hybi(k))*elem(ie)%state%psdry(i,j)
               end do
            end do
         end do
      end do
   end if

   ! If scale_dry_air_mass > 0.0 then scale dry air mass to scale_dry_air_mass global average dry pressure
   if (runtype == 0) then
     if (scale_dry_air_mass > 0.0_r8) then
       if (iam < par%nprocs) then
         call prim_set_dry_mass(elem, hvcoord, scale_dry_air_mass, qtmp)
       end if
     end if
   end if

   ! store Q values:
   !
   ! if CSLAM is NOT active then state%Qdp for all constituents
   ! if CSLAM active then we only advect water vapor and condensate
   ! loading tracers in state%qdp

   if (use_cslam) then
      do ie = 1, nelemd
         do nq = 1, thermodynamic_active_species_num
            m_cnst = thermodynamic_active_species_idx(nq)
            do k = 1, nlev
               do j = 1, np
                  do i = 1, np
                     elem(ie)%state%Qdp(i,j,k,nq,:) = &
                                 elem(ie)%state%dp3d(i,j,k,1)*qtmp(i,j,k,ie,m_cnst)
                  end do
               end do
            end do
         end do
      end do
   else
      do ie = 1, nelemd
         do m_cnst = 1, qsize
            do k = 1, nlev
               do j = 1, np
                  do i = 1, np
                     elem(ie)%state%Qdp(i,j,k,m_cnst,:)=&
                        elem(ie)%state%dp3d(i,j,k,1)*qtmp(i,j,k,ie,m_cnst)
                  end do
               end do
            end do
         end do
      end do
   end if

   ! interpolate fvm tracers and fvm pressure variables

   if (use_cslam) then
      if (par%masterproc) then
         write(iulog,*) 'Initializing dp_fvm from spectral element dp'
      end if

      do ie = 1, nelemd

         ! note that the area over fvm cells as computed from subcell_integration is up to 1.0E-6
         ! different than the areas (exact) computed by CSLAM
         !
         ! Map the constituents which are also to be transported by dycore
         call dyn2fvm_mass_vars(elem(ie)%state%dp3d(:,:,:,1),elem(ie)%state%psdry(:,:),&
            qtmp(:,:,:,ie,1:ntrac),&
            dyn_in%fvm(ie)%dp_fvm(1:nc,1:nc,:),dyn_in%fvm(ie)%psC(1:nc,1:nc),&
            dyn_in%fvm(ie)%c(1:nc,1:nc,:,1:ntrac),&
            ntrac,elem(ie)%metdet,dyn_in%fvm(ie)%inv_se_area_sphere(1:nc,1:nc))
      end do

      if(par%masterproc) then
         write(iulog,*) 'FVM tracers, FVM pressure variables and se_area_sphere initialized.'
      end if

   end if    ! (ntrac > 0)

   ! Cleanup
   deallocate(qtmp)

   do ie = 1, nelemd
      do t = 2, timelevels
         elem(ie)%state%v(:,:,:,:,t) = elem(ie)%state%v(:,:,:,:,1)
         elem(ie)%state%T(:,:,:,t)   = elem(ie)%state%T(:,:,:,1)
      end do
   end do

   if(iam < par%nprocs) then
      call FreeEdgeBuffer(edge)
   end if

   !Finally, mark variables as initialized so that physics doesn't try to set
   !the initial values itself:
   call mark_as_initialized("surface_air_pressure")
   call mark_as_initialized("air_pressure_thickness")
   call mark_as_initialized("eastward_wind")  !eastward wind
   call mark_as_initialized("northward_wind") !northward wind
   call mark_as_initialized("air_temperature")

   !Mark all advected constituents as initialized
   do m_cnst = 1, num_advected
      call mark_as_initialized(const_name(m_cnst))
   end do

   !These calls may be removed if geopotential_t is only allowed to run
   !in a CCPP physics suite:
   call mark_as_initialized("geopotential_height_wrt_surface")
   call mark_as_initialized("geopotential_height_wrt_surface_at_interface")
   call mark_as_initialized("dry_static_energy")

   !These variables are calculated in d_p_coupling, but need to be marked here:
   call mark_as_initialized("air_pressure")
   call mark_as_initialized("ln_air_pressure")
   call mark_as_initialized("air_pressure_at_interface")
   call mark_as_initialized("ln_air_pressure_at_interface")
   call mark_as_initialized("air_pressure_thickness_of_dry_air")
   call mark_as_initialized("surface_pressure_of_dry_air")
   call mark_as_initialized("air_pressure_of_dry_air")
   call mark_as_initialized("air_pressure_of_dry_air_at_interface")
   call mark_as_initialized("ln_air_pressure_of_dry_air_at_interface")
   call mark_as_initialized("ln_air_pressure_of_dry_air")
   call mark_as_initialized("reciprocal_of_air_pressure_thickness_of_dry_air")
   call mark_as_initialized("reciprocal_of_air_pressure_thickness")
   call mark_as_initialized("reciprocal_of_dimensionless_exner_function_wrt_surface_air_pressure")
   call mark_as_initialized("lagrangian_tendency_of_air_pressure")
   call mark_as_initialized("tendency_of_air_temperature_due_to_model_physics")
   call mark_as_initialized("tendency_of_eastward_wind_due_to_model_physics")
   call mark_as_initialized("tendency_of_northward_wind_due_to_model_physics")
   call mark_as_initialized("specific_heat_of_air_used_in_dycore")
   call mark_as_initialized("frontogenesis_function")
   call mark_as_initialized("frontogenesis_angle")
   call mark_as_initialized("relative_vorticity")

   ! These energy variables are calculated by check_energy_timestep_init
   ! but need to be marked here
   call mark_as_initialized("vertically_integrated_total_energy_using_physics_energy_formula_at_start_of_physics_timestep")
   call mark_as_initialized("vertically_integrated_total_energy_using_physics_energy_formula")
   call mark_as_initialized("vertically_integrated_total_energy_using_dycore_energy_formula_at_start_of_physics_timestep")
   call mark_as_initialized("vertically_integrated_total_energy_using_dycore_energy_formula")
   call mark_as_initialized("vertically_integrated_total_water_at_start_of_physics_timestep")
   call mark_as_initialized("vertically_integrated_total_water")
   call mark_as_initialized("vertically_integrated_total_energy_using_dycore_energy_formula_at_end_of_physics_timestep")

end subroutine read_inidat


!========================================================================================

subroutine set_phis(dyn_in)

   ! Set PHIS according to the following rules.
   !
   ! 1) If a topo file is specified use it.  This option has highest precedence.
   ! 2) If not using topo file, but analytic_ic option is on, use analytic phis.
   ! 3) Set phis = 0.0.
   !
   ! If using the physics grid then the topo file will be on that grid since its
   ! contents are primarily for the physics parameterizations, and the values of
   ! PHIS should be consistent with the values of sub-grid variability (e.g., SGH)
   ! which are computed on the physics grid.  In this case phis on the physics grid
   ! will be interpolated to the GLL grid.

   use pio,                  only: file_desc_t, pio_inq_dimid, pio_inq_dimlen
   use pio,                  only: pio_seterrorhandling, PIO_NOERR, PIO_BCAST_ERROR
   use cam_abortutils,       only: endrun, check_allocate
   use cam_logfile,          only: iulog
   use phys_vars_init_check, only: mark_as_initialized
   use cam_history_support,  only: max_fieldname_len
   use cam_grid_support,     only: max_hcoordname_len, cam_grid_id
   use cam_grid_support,     only: cam_grid_get_latvals, cam_grid_get_lonvals
   use cam_grid_support,     only: cam_grid_dimensions, cam_grid_get_gcid
   use cam_map_utils,        only: iMap
   use spmd_utils,           only: iam, masterproc
   use cam_initfiles,        only: topo_file_get_id
   use dyn_grid,             only: ini_grid_name, edgebuf
   use inic_analytic,        only: analytic_ic_active, analytic_ic_set_ic
   use dyn_tests_utils,      only: vcoord=>vc_dry_pressure

   !SE dycore:
   use dimensions_mod,       only: np, npsq, nelemd, fv_nphys
   use dimensions_mod,       only: use_cslam
   use parallel_mod,         only: par
   use bndry_mod,            only: bndry_exchange
   use edge_mod,             only: edgeVpack, edgeVunpack

#ifdef scam
   use scamMod,              only: single_column
#else
   logical, parameter :: single_column = .false. !Always assume single-column mode is off.
#endif

   ! Arguments
   type (dyn_import_t), target, intent(inout) :: dyn_in   ! dynamics import

   ! local variables
   type(file_desc_t), pointer       :: fh_topo

   type(element_t), pointer         :: elem(:)

   real(r8), allocatable            :: phis_tmp(:,:)      ! (npsq,nelemd)
   real(r8), allocatable            :: phis_phys_tmp(:,:) ! (fv_nphys**2,nelemd)

   integer                          :: i, ie, indx, j, kptr
   integer                          :: ierr, pio_errtype
   character(len=cl)                :: errmsg

   character(len=max_fieldname_len) :: fieldname
   character(len=max_fieldname_len) :: fieldname_gll
   character(len=max_hcoordname_len):: grid_name
   integer                          :: dims(2)
   integer                          :: dyn_cols
   integer                          :: ncol_did
   integer                          :: ncol_size

   integer(iMap), pointer           :: ldof(:)            ! Basic (2D) grid dof
   logical,  allocatable            :: pmask(:)           ! (npsq*nelemd) unique columns

   ! Variables for analytic initial conditions
   integer,  allocatable            :: glob_ind(:)
   logical,  allocatable            :: pmask_phys(:)
   real(r8), pointer                :: latvals_deg(:)
   real(r8), pointer                :: lonvals_deg(:)
   real(r8), allocatable            :: latvals(:)
   real(r8), allocatable            :: lonvals(:)
   real(r8), allocatable            :: latvals_phys(:)
   real(r8), allocatable            :: lonvals_phys(:)

   character(len=*), parameter      :: subname='set_phis'
   !----------------------------------------------------------------------------

   fh_topo => topo_file_get_id()

   if (iam < par%nprocs) then
      elem => dyn_in%elem
   else
      nullify(elem)
   end if

   allocate(phis_tmp(npsq,nelemd), stat=ierr, errmsg=errmsg)
   call check_allocate(ierr, subname, 'phis_tmp(npsq,nelemd)', &
                       file=__FILE__, line=__LINE__, errmsg=errmsg)

   phis_tmp = 0.0_r8

   if (use_cslam) then
      allocate(phis_phys_tmp(fv_nphys**2,nelemd), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'phis_phys_tmp(fv_nphys**2,nelemd)', &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)

      phis_phys_tmp = 0.0_r8
      do ie=1,nelemd
        elem(ie)%sub_elem_mass_flux=0.0_r8
#ifdef waccm_debug
        dyn_in%fvm(ie)%CSLAM_gamma = 0.0_r8
#endif
      end do
   end if

   ! Set mask to indicate which columns are active in GLL grid.
   nullify(ldof)
   call cam_grid_get_gcid(cam_grid_id('GLL'), ldof)
   allocate(pmask(npsq*nelemd), stat=ierr, errmsg=errmsg)
   call check_allocate(ierr, subname, 'pmask(npsq*nelemd)', &
                       file=__FILE__, line=__LINE__, errmsg=errmsg)

   pmask(:) = (ldof /= 0)
   deallocate(ldof)

   if (associated(fh_topo)) then

      ! Set PIO to return error flags.
      call pio_seterrorhandling(fh_topo, PIO_BCAST_ERROR, pio_errtype)

      ! Set name of grid object which will be used to read data from file
      ! into internal data structure via PIO.
      if (single_column) then
         grid_name = 'SCM'
      else
         if (.not.use_cslam) then
            grid_name = 'GLL'
         else
            grid_name = 'physgrid_d'
         end if
      end if
      ! Get number of global columns from the grid object and check that
      ! it matches the file data.
      call cam_grid_dimensions(grid_name, dims)
      dyn_cols = dims(1)

      ! The dimension of the unstructured grid in the TOPO file is 'ncol'.
      ierr = pio_inq_dimid(fh_topo, 'ncol', ncol_did)
      if (ierr /= PIO_NOERR) then
         call endrun(subname//': dimension ncol not found in bnd_topo file')
      end if

      ierr = pio_inq_dimlen(fh_topo, ncol_did, ncol_size)
      if (ierr /= PIO_NOERR) then
         call endrun(subname//': dimension ncol does not have a value in bnd_topo file')
      end if

      if (ncol_size /= dyn_cols .and. .not. single_column) then
         if (masterproc) then
            write(iulog,*) subname//': ncol_size=', ncol_size, ' : dyn_cols=', dyn_cols
         end if
         call endrun(subname//': ncol size in bnd_topo file does not match grid definition')
      end if

      fieldname = 'PHIS'
      fieldname_gll = 'PHIS_gll'
      if (use_cslam.and.dyn_field_exists(fh_topo, trim(fieldname_gll),required=.false.)) then
         !
         ! If physgrid it is recommended to read in PHIS on the GLL grid and then
         ! map to the physgrid in d_p_coupling
         !
         ! This requires a topo file with PHIS_gll on it ...
         !
         if (masterproc) then
            write(iulog, *) "Reading in PHIS on GLL grid (mapped to physgrid in d_p_coupling)"
         end if
         call read_dyn_var(fieldname_gll, fh_topo, 'ncol_gll', phis_tmp)
      else if (dyn_field_exists(fh_topo, trim(fieldname))) then
         if (.not.use_cslam) then
            if (masterproc) then
               write(iulog, *) "Reading in PHIS"
            end if
            call read_dyn_var(fieldname, fh_topo, 'ncol', phis_tmp)
         else
            !
            ! For backwards compatibility we allow reading in PHIS on the physgrid
            ! which is then mapped to the GLL grid and back to the physgrid in d_p_coupling
            ! (the latter is to avoid noise in derived quantities such as PSL)
            !
            if (masterproc) then
               write(iulog, *) "Reading in PHIS on physgrid"
               write(iulog, *) "Recommended to read in PHIS on GLL grid"
            end if
            call read_phys_field_2d(fieldname, fh_topo, 'ncol', phis_phys_tmp)
            call map_phis_from_physgrid_to_gll(dyn_in%fvm, elem, phis_phys_tmp, &
                 phis_tmp, pmask)
            deallocate(phis_phys_tmp)
         end if
      else
         call endrun(subname//': Could not find PHIS field on input datafile')
      end if

      ! Put the error handling back the way it was
      call pio_seterrorhandling(fh_topo, pio_errtype)

   else if (analytic_ic_active() .and. (iam < par%nprocs)) then

      ! lat/lon needed in radians
      latvals_deg => cam_grid_get_latvals(cam_grid_id('GLL'))
      lonvals_deg => cam_grid_get_lonvals(cam_grid_id('GLL'))
      allocate(latvals(np*np*nelemd), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'latvals(np*np*nelemd)', &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)

      allocate(lonvals(np*np*nelemd), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'lonvals(np*np*nelemd)', &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)

      latvals(:) = latvals_deg(:)*deg2rad
      lonvals(:) = lonvals_deg(:)*deg2rad

      allocate(glob_ind(npsq*nelemd), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'glob_ind(npsq*nelemd)', &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)

      j = 1
      do ie = 1, nelemd
         do i = 1, npsq
            ! Create a global(ish) column index
            glob_ind(j) = elem(ie)%GlobalId
            j = j + 1
         end do
      end do
      call analytic_ic_set_ic(vcoord, latvals, lonvals, glob_ind, &
                              PHIS_OUT=phis_tmp, mask=pmask(:))
      deallocate(glob_ind)
    end if


   deallocate(pmask)

   ! Set PHIS in element objects
   do ie = 1, nelemd
      elem(ie)%state%phis = 0.0_r8
      indx = 1
      do j = 1, np
         do i = 1, np
            elem(ie)%state%phis(i,j) = phis_tmp(indx, ie)
            indx = indx + 1
         end do
      end do
   end do
   deallocate(phis_tmp)

   ! boundary exchange to update the redundent columns in the element objects
   do ie = 1, nelemd
      kptr = 0
      call edgeVpack(edgebuf, elem(ie)%state%phis, 1, kptr, ie)
   end do
   if(iam < par%nprocs) then
      call bndry_exchange(par, edgebuf, location=subname)
   end if
   do ie = 1, nelemd
      kptr = 0
      call edgeVunpack(edgebuf, elem(ie)%state%phis,1,kptr,ie)
   end do

   !Mark phis as initialized so that physics doesn't try to set
   !the initial values itself:
   call mark_as_initialized("surface_geopotential")

end subroutine set_phis

!========================================================================================

subroutine check_file_layout(file, elem, dyn_cols, file_desc, dyn_ok, dimname)

   ! This routine is only called when data will be read from the initial file.  It is not
   ! called when the initial file is only supplying vertical coordinate info.

   use cam_history_support,  only: max_fieldname_len
   use spmd_utils,           only: iam, masterproc
   use pio,                  only: pio_inq_dimid, pio_inq_dimlen
   use pio,                  only: PIO_NOERR, file_desc_t
   use cam_abortutils,       only: endrun
   use shr_sys_mod,          only: shr_sys_flush
   use dyn_grid,             only: ini_grid_name
   use cam_logfile,          only: iulog
   use cam_grid_support,     only: cam_grid_get_dim_names, cam_grid_id

   !SE dycore:
   use dimensions_mod,       only: nelemd, np, npsq

   type(file_desc_t), pointer       :: file
   type(element_t),   pointer       :: elem(:)
   integer,           intent(in)    :: dyn_cols
   character(len=*),  intent(in)    :: file_desc
   logical,           intent(in)    :: dyn_ok ! .true. iff ncol_d is okay
   character(len=*),  intent(out)   :: dimname

   integer                          :: ncol_did, ncol_size
   integer                          :: ierr
   integer                          :: ie, i, j
   integer                          :: grid_id
   integer                          :: indx
   real(r8)                         :: dbuf2(npsq, nelemd)
   logical                          :: found
   character(len=max_fieldname_len) :: dimname2, coordname

   character(len=*), parameter      :: subname = 'check_file_layout'
   !----------------------------------------------------------------------------

   ! Check that number of columns in IC file matches grid definition.

   call cam_grid_get_dim_names(cam_grid_id(ini_grid_name), dimname, dimname2)

   ierr = pio_inq_dimid(file, trim(dimname), ncol_did)
   if (ierr /= PIO_NOERR) then
      call endrun(subname//': ERROR: either ncol or ncol_d dimension not found in ' &
         //trim(file_desc)//' file')
   end if

   ierr = pio_inq_dimlen(file, ncol_did, ncol_size)
   if (ncol_size /= dyn_cols) then
      if (masterproc) then
         write(iulog, '(a,2(a,i0))') trim(subname), ': ncol_size=', ncol_size, &
             ' : dyn_cols=', dyn_cols
      end if
      call endrun(subname//': ERROR: dimension ncol size not same as in ncdata file')
   end if

   ! Set coordinate name associated with dimname.
   if (dimname == 'ncol') then
      coordname = 'lat'
   else
      coordname = 'lat_d'
   end if

    !! Check to make sure file is in correct order
   call read_dyn_var(coordname, file, dimname, dbuf2)
   found = .true.
   do ie = 1, nelemd
      indx = 1
      do j = 1, np
         do i = 1, np
            if (abs(dbuf2(indx,ie)) > 1.e-12_r8) then
               if (abs((elem(ie)%spherep(i,j)%lat*rad2deg - dbuf2(indx,ie)) / &
                    dbuf2(indx,ie)) > 1.0e-10_r8) then
                  write(iulog, '(2a,4(i0,a),f11.5,a,f11.5)')                  &
                       "ncdata file latitudes not in correct column order",   &
                       ' on task ', iam, ': elem(', ie, ')%spherep(', i,      &
                       ', ', j, ')%lat = ', elem(ie)%spherep(i,j)%lat,        &
                       ' /= ', dbuf2(indx, ie)*deg2rad
                  call shr_sys_flush(iulog)
                  found = .false.
               end if
            end if
            indx = indx + 1
         end do
      end do
   end do
   if (.not. found) then
      call endrun("ncdata file latitudes not in correct column order")
   end if

   if (dimname == 'ncol') then
      coordname = 'lon'
   else
      coordname = 'lon_d'
   end if

   call read_dyn_var(coordname, file, dimname, dbuf2)
   do ie = 1, nelemd
      indx = 1
      do j = 1, np
         do i = 1, np
            if (abs(dbuf2(indx,ie)) > 1.e-12_r8) then
               if (abs((elem(ie)%spherep(i,j)%lon*rad2deg - dbuf2(indx,ie)) / &
                    dbuf2(indx,ie)) > 1.0e-10_r8) then
                  write(iulog, '(2a,4(i0,a),f11.5,a,f11.5)')                  &
                       "ncdata file longitudes not in correct column order",  &
                       ' on task ', iam, ': elem(', ie, ')%spherep(', i,      &
                       ', ', j, ')%lon = ', elem(ie)%spherep(i,j)%lon,        &
                       ' /= ', dbuf2(indx, ie)*deg2rad
                  call shr_sys_flush(iulog)
                  found = .false.
               end if
            end if
            indx = indx + 1
         end do
      end do
   end do
   if (.not. found) then
      call endrun("ncdata file longitudes not in correct column order")
   end if

end subroutine check_file_layout

!========================================================================================

logical function dyn_field_exists(fh, fieldname, required)

   use pio,            only: var_desc_t, PIO_inq_varid
   use pio,            only: file_desc_t, PIO_NOERR
   use cam_abortutils, only: endrun

   type(file_desc_t), intent(in) :: fh
   character(len=*),  intent(in) :: fieldname
   logical, optional, intent(in) :: required

   ! Local variables
   logical                  :: found
   logical                  :: field_required
   integer                  :: ret
   type(var_desc_t)         :: varid
   character(len=128)       :: errormsg
   !--------------------------------------------------------------------------

   if (present(required)) then
      field_required = required
   else
      field_required = .true.
   end if

   ret = PIO_inq_varid(fh, trim(fieldname), varid)
   found = (ret == PIO_NOERR)
   if (.not. found) then
      if (field_required) then
         write(errormsg, *) trim(fieldname),' was not present in the input file.'
         call endrun('DYN_FIELD_EXISTS: '//errormsg)
      end if
   end if

   dyn_field_exists = found

end function dyn_field_exists

!========================================================================================

subroutine read_dyn_field_2d(fieldname, fh, dimname, buffer)

   use pio,                only: file_desc_t
   use cam_field_read,     only: cam_read_field
   use cam_abortutils,     only: endrun
   use shr_infnan_mod,     only: shr_infnan_isnan
   use dyn_grid,           only: ini_grid_name

   ! Dummy arguments
   character(len=*),  intent(in)    :: fieldname
   type(file_desc_t), intent(inout) :: fh
   character(len=*),  intent(in)    :: dimname
   real(r8),          intent(inout) :: buffer(:, :)

   ! Local variables
   logical                  :: found
   real(r8)                 :: fillvalue
   !----------------------------------------------------------------------------

   buffer = 0.0_r8
   call cam_read_field(trim(fieldname), fh, buffer, found, &
                       gridname=ini_grid_name, fillvalue=fillvalue)
   if(.not. found) then
      call endrun('READ_DYN_FIELD_2D: Could not find '//trim(fieldname)//' field on input datafile')
   end if

   ! This code allows use of compiler option to set uninitialized values
   ! to NaN.  In that case cam_read_field can return NaNs where the element
   ! GLL points are not "unique columns".
   ! Set NaNs or fillvalue points to zero:
   where (shr_infnan_isnan(buffer))
      ! check for NaN first, as comparing NaN to fillvalue raises floating invalid.
      buffer = 0.0_r8
   end where

   if (.not. shr_infnan_isnan(fillvalue)) then
      ! only compare against fillvalue if fillvalue is not NaN, otherwise the comparison
      ! will raise floating invalid.
      where (buffer == fillvalue)
         buffer = 0.0_r8
      end where
   end if

end subroutine read_dyn_field_2d

!========================================================================================

subroutine read_dyn_field_3d(fieldname, fh, dimname, buffer)

   use pio,                only: file_desc_t
   use cam_field_read,     only: cam_read_field
   use cam_abortutils,     only: endrun
   use shr_infnan_mod,     only: shr_infnan_isnan
   use dyn_grid,           only: ini_grid_name

   ! SE dycore:
   use dimensions_mod,     only: nlev

   ! Dummy arguments
   character(len=*),  intent(in)    :: fieldname
   type(file_desc_t), intent(inout) :: fh
   character(len=*),  intent(in)    :: dimname
   real(r8),          intent(inout) :: buffer(:,:,:)

   ! Local variables
   logical                  :: found
   real(r8)                 :: fillvalue
   !----------------------------------------------------------------------------

   buffer = 0.0_r8
   call cam_read_field(trim(fieldname), fh, buffer, found, 'lev', (/1, nlev/), &
                       dim3_pos=2, gridname=ini_grid_name, fillvalue=fillvalue)
   if(.not. found) then
      call endrun('READ_DYN_FIELD_3D: Could not find '//trim(fieldname)//' field on input datafile')
   end if

   ! This code allows use of compiler option to set uninitialized values
   ! to NaN.  In that case cam_read_field can return NaNs where the element GLL
   ! points are not "unique columns".
   ! Set NaNs or fillvalue points to zero:
   where (shr_infnan_isnan(buffer))
      ! check for NaN first, as comparing NaN to fillvalue raises floating invalid.
      buffer = 0.0_r8
   end where

   if (.not. shr_infnan_isnan(fillvalue)) then
      ! only compare against fillvalue if fillvalue is not NaN, otherwise the comparison
      ! will raise floating invalid.
      where (buffer == fillvalue)
         buffer = 0.0_r8
      end where
   end if

end subroutine read_dyn_field_3d

!========================================================================================

subroutine read_phys_field_2d(fieldname, fh, dimname, buffer)

   use pio,                only: file_desc_t
   use cam_field_read,     only: cam_read_field
   use cam_abortutils,     only: endrun

   ! Dummy arguments
   character(len=*),  intent(in)    :: fieldname
   type(file_desc_t), intent(inout) :: fh
   character(len=*),  intent(in)    :: dimname
   real(r8),          intent(inout) :: buffer(:, :)

   ! Local variables
   logical                  :: found
   !----------------------------------------------------------------------------

   call cam_read_field(trim(fieldname), fh, buffer, found, gridname='physgrid_d')
   if(.not. found) then
      call endrun('READ_PHYS_FIELD_2D: Could not find '//trim(fieldname)//' field on input datafile')
   end if

end subroutine read_phys_field_2d

!========================================================================================

subroutine map_phis_from_physgrid_to_gll(fvm,elem,phis_phys_tmp,phis_tmp,pmask)

   use cam_abortutils,     only: check_allocate

   !SE dycore:
   use parallel_mod,       only: par
   use hybrid_mod,         only: hybrid_t, get_loop_ranges, config_thread_region
   use dimensions_mod,     only: fv_nphys, nhc_phys, nelemd, np, npsq
   use fvm_mapping,        only: phys2dyn
   use thread_mod,         only: horz_num_threads

   type(element_t),   intent(inout) :: elem(:)
   type (fvm_struct), intent(in)    :: fvm(:)
   real(r8)         , intent(in)    :: phis_phys_tmp(fv_nphys**2,nelemd) !physgrid phis
   real(r8)         , intent(inout) :: phis_tmp(npsq,nelemd)             !gll phis
   logical          , intent(in)    :: pmask(npsq*nelemd)

   type(hybrid_t)                   :: hybrid
   integer                          :: nets, nete, ie,i,j,indx, iret
   real(r8),            allocatable :: fld_phys(:,:,:,:,:),fld_gll(:,:,:,:,:)
   logical                          :: llimiter(1)
   character(len=cl)                :: errmsg

   character(len=*), parameter      :: subname = 'map_phis_from_physgrid_to_gll'

   !----------------------------------------------------------------------------

   !!$OMP PARALLEL NUM_THREADS(horz_num_threads), DEFAULT(SHARED), PRIVATE(hybrid,nets,nete,ie)
   !hybrid = config_thread_region(par,'horizontal')
   hybrid = config_thread_region(par,'serial')

   call get_loop_ranges(hybrid, ibeg=nets, iend=nete)

   allocate(fld_phys(1-nhc_phys:fv_nphys+nhc_phys,1-nhc_phys:fv_nphys+nhc_phys,1,1,nets:nete), &
            stat=iret, errmsg=errmsg)
   call check_allocate(iret, subname, &
                       'fld_phys(1-nhc_phys:fv_nphys+nhc_phys,1-nhc_phys:fv_nphys+nhc_phys,1,1,nets:nete)', &
                       file=__FILE__, line=__LINE__, errmsg=errmsg)

   allocate(fld_gll(np,np,1,1,nets:nete), stat=iret, errmsg=errmsg)
   call check_allocate(iret, subname, 'fld_gll(np,np,1,1,nets:nete)', &
                       file=__FILE__, line=__LINE__, errmsg=errmsg)

   fld_phys = 0.0_r8
   do ie = nets, nete
      fld_phys(1:fv_nphys,1:fv_nphys,1,1,ie) = RESHAPE(phis_phys_tmp(:,ie),(/fv_nphys,fv_nphys/))
   end do
   llimiter = .true.
   call phys2dyn(hybrid,elem,fld_phys,fld_gll,nets,nete,1,1,fvm,llimiter,halo_filled=.false.)
   do ie = nets,nete
      indx = 1
      do j = 1, np
         do i = 1, np
            if (pmask(((ie - 1) * npsq) + indx)) then
               phis_tmp(indx,ie) = fld_gll(i,j,1,1,ie)
            else
               phis_tmp(indx,ie) = 0.0_r8
            end if
            indx = indx + 1
         end do
      end do
   end do
   deallocate(fld_phys)
   deallocate(fld_gll)
   !!$OMP END PARALLEL
end subroutine map_phis_from_physgrid_to_gll

!========================================================================================

subroutine write_dyn_vars(dyn_out)

   use cam_history_support, only: fieldname_len
   use cam_history,         only: history_out_field
   use cam_constituents,    only: const_name
   use cam_constituents,    only: const_diag_name

   !SE dycore:
   use dimensions_mod,      only: nlev, ntrac, nc, nelemd, use_cslam

   type (dyn_export_t), intent(inout) :: dyn_out ! Dynamics export container

   character(len=fieldname_len) :: tfname
   integer                      :: ie, m
   !----------------------------------------------------------------------------

   if (use_cslam) then
      do ie = 1, nelemd
         call history_out_field('dp_fvm', RESHAPE(dyn_out%fvm(ie)%dp_fvm(1:nc,1:nc,:),   &
                                       (/nc*nc,nlev/)))
         call history_out_field('PSDRY_fvm', RESHAPE(dyn_out%fvm(ie)%psc(1:nc,1:nc),     &
                                          (/nc*nc/)))
         do m = 1, ntrac
            tfname = trim(const_diag_name(m))//'_fvm'
            call history_out_field(tfname, RESHAPE(dyn_out%fvm(ie)%c(1:nc,1:nc,:,m),     &
                                        (/nc*nc,nlev/)))

            tfname = 'F'//trim(const_diag_name(m))//'_fvm'
            call history_out_field(tfname, RESHAPE(dyn_out%fvm(ie)%fc(1:nc,1:nc,:,m),    &
                                        (/nc*nc,nlev/)))
         end do
      end do
   end if

end subroutine write_dyn_vars

!=========================================================================================
end module dyn_comp
