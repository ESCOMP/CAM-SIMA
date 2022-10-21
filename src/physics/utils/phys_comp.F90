module phys_comp

   use ccpp_kinds,   only: kind_phys
   use shr_kind_mod, only: SHR_KIND_CS, SHR_KIND_CL
   use runtime_obj,  only: unset_str

   implicit none
   private

   public :: phys_readnl
   public :: phys_init
   public :: phys_run1
   public :: phys_run2
   public :: phys_final

   ! Public module data
   ! suite_name: Suite we are running
   character(len=SHR_KIND_CS), public, protected :: phys_suite_name = unset_str

   ! Private module data
   character(len=SHR_KIND_CS), allocatable :: suite_names(:)
   character(len=SHR_KIND_CS), allocatable :: suite_parts(:)
   character(len=SHR_KIND_CL)              :: ncdata_check = unset_str
   character(len=SHR_KIND_CL)              :: cam_physics_mesh = unset_str
   character(len=SHR_KIND_CS)              :: cam_take_snapshot_before = unset_str
   character(len=SHR_KIND_CS)              :: cam_take_snapshot_after = unset_str
   real(kind_phys)                         :: min_difference = HUGE(1.0_kind_phys)
   real(kind_phys)                         :: min_relative_value = HUGE(1.0_kind_phys)

!==============================================================================
CONTAINS
!==============================================================================

   subroutine phys_readnl(nlfilename)
      ! Read physics options, such as suite to run
      use shr_kind_mod,    only: r8 => shr_kind_r8
      use shr_nl_mod,      only: find_group_name => shr_nl_find_group_name
      use shr_flux_mod,    only: shr_flux_adjust_constants
      use mpi,             only: mpi_character, mpi_real8, mpi_logical
      use spmd_utils,      only: masterproc, masterprocid, mpicom
      use cam_logfile,     only: iulog
      use cam_abortutils,  only: endrun
      use cam_initfiles,   only: unset_path_str
      use cam_ccpp_cap,    only: ccpp_physics_suite_list

      ! filepath for file containing namelist input
      character(len=*), intent(in) :: nlfilename

      ! Local variables
      character(len=SHR_KIND_CS)  :: physics_suite

      integer                     :: unitn, ierr, i
      character(len=*), parameter :: subname = 'phys_readnl'

      namelist /physics_nl/ ncdata_check, min_difference, min_relative_value,&
         cam_take_snapshot_before, cam_take_snapshot_after, cam_physics_mesh,&
         physics_suite

      ! Initialize namelist variables to invalid values
      min_difference           = HUGE(1.0_kind_phys)
      min_relative_value       = HUGE(1.0_kind_phys)
      cam_take_snapshot_after  = unset_path_str
      cam_take_snapshot_before = unset_path_str
      cam_physics_mesh         = unset_path_str
      ncdata_check             = unset_path_str
      physics_suite            = unset_str

      ! Read namelist
      if (masterproc) then
         open(newunit=unitn, file=trim(nlfilename), status='old')
         call find_group_name(unitn, 'physics_nl', status=ierr)
         if (ierr == 0) then
            read(unitn, physics_nl, iostat=ierr)
            if (ierr /= 0) then
               call endrun(subname // ':: ERROR reading namelist')
            end if
         end if
         close(unitn)
      end if
      ! Broadcast namelist variables
      call mpi_bcast(ncdata_check, len(ncdata_check), mpi_character,       &
         masterprocid, mpicom, ierr)
      call mpi_bcast(min_difference, 1, mpi_real8, masterprocid, mpicom,   &
         ierr)
      call mpi_bcast(min_relative_value, 1, mpi_real8, masterprocid,       &
         mpicom, ierr)
      call mpi_bcast(cam_physics_mesh, len(cam_physics_mesh),              &
         mpi_character, masterprocid, mpicom, ierr)
      call mpi_bcast(cam_take_snapshot_before,                             &
        len(cam_take_snapshot_before), mpi_character, masterprocid,        &
         mpicom, ierr)
      call mpi_bcast(cam_take_snapshot_after, len(cam_take_snapshot_after),&
        mpi_character, masterprocid, mpicom, ierr)
      call mpi_bcast(physics_suite, len(physics_suite),&
        mpi_character, masterprocid, mpicom, ierr)

      ! Check that the listed physics suite is actually present
      ! in the CCPP physics suite list:
      call ccpp_physics_suite_list(suite_names)
      do i = 1, size(suite_names)
         if (trim(physics_suite) == trim(suite_names(i))) then
            phys_suite_name = trim(physics_suite)
         end if
      end do

      ! If no match is found, then end run here:
      if (phys_suite_name == unset_str) then
         call endrun(subname//": Physics suite '"//trim(physics_suite)//"' not found.")
      end if

      ! Print out namelist variables
      if (masterproc) then
         write(iulog,*) subname, ' options:'
         if (trim(ncdata_check) /= trim(unset_path_str)) then
            write(iulog,*) '  Physics data check will be performed against: ',&
               trim(ncdata_check)
            write(iulog,*) 'Minimum Difference considered significant: ',     &
               min_difference
            write(iulog,*) 'Value Under Which Absolute Difference Calculated: ', &
               min_relative_value
         else
            write(iulog,*) '  Physics data check will not be performed'
         end if
         write(iulog, *) ' CCPP Physics suite chosen: ', phys_suite_name
      end if

   end subroutine phys_readnl

   subroutine phys_init(cam_runtime_opts, phys_state, phys_tend, cam_out)
      use pio,              only: file_desc_t
      use cam_abortutils,   only: endrun
      use runtime_obj,      only: runtime_options
      use physics_types,    only: physics_state, physics_tend
      use camsrfexch,       only: cam_out_t
      use physics_grid,     only: columns_on_task
      use vert_coord,       only: pver, pverp
      use cam_constituents, only: num_advected
      use cam_thermo,       only: cam_thermo_init
      use physics_types,    only: allocate_physics_types_fields
      use cam_ccpp_cap,     only: cam_ccpp_physics_initialize
      use cam_ccpp_cap,     only: ccpp_physics_suite_part_list

      ! Dummy arguments
      type(runtime_options), intent(in)    :: cam_runtime_opts
      type(physics_state),   intent(inout) :: phys_state
      type(physics_tend),    intent(inout) :: phys_tend
      type(cam_out_t),       intent(inout) :: cam_out

      ! Local variables
      real(kind_phys)            :: dtime_phys = 0.0_kind_phys ! Not set yet
      integer                    :: num_host_advected
      character(len=512)         :: errmsg
      integer                    :: errcode

      errcode = 0
      call cam_thermo_init(columns_on_task, pver, pverp)

      call allocate_physics_types_fields(columns_on_task, pver, pverp,        &
           set_init_val_in=.true., reallocate_in=.false.)
      call cam_ccpp_physics_initialize(phys_suite_name, dtime_phys,           &
           errmsg, errcode)
      if (errcode /= 0) then
         call endrun('cam_ccpp_physics_initialize: '//trim(errmsg))
      end if
      call ccpp_physics_suite_part_list(phys_suite_name, suite_parts,         &
           errmsg, errcode)
      if (errcode /= 0) then
         call endrun('cam_ccpp_suite_part_list: '//trim(errmsg))
      end if

   end subroutine phys_init

   subroutine phys_run1(dtime_phys, cam_runtime_opts, phys_state, phys_tend,  &
        cam_in, cam_out)
      use cam_abortutils, only: endrun
      use runtime_obj,    only: runtime_options
      use physics_types,  only: physics_state, physics_tend
      use camsrfexch,     only: cam_in_t, cam_out_t

      ! Dummy arguments
      real(kind_phys),       intent(in)    :: dtime_phys
      type(runtime_options), intent(in)    :: cam_runtime_opts
      type(physics_state),   intent(inout) :: phys_state
      type(physics_tend),    intent(inout) :: phys_tend
      type(cam_in_t),        intent(inout) :: cam_in
      type(cam_out_t),       intent(inout) :: cam_out

   end subroutine phys_run1

   !> \section arg_table_phys_run2  Argument Table
   !! \htmlinclude arg_table_phys_run2.html
   !!
   subroutine phys_run2(dtime_phys, cam_runtime_opts, phys_state, phys_tend,  &
        cam_in, cam_out)
      use pio,            only: file_desc_t
      use cam_abortutils, only: endrun
      use runtime_obj,    only: runtime_options
      use physics_types,  only: physics_state, physics_tend
      use physics_types,  only: physics_types_tstep_init
      use physics_grid,   only: columns_on_task
      use camsrfexch,     only: cam_in_t, cam_out_t
      use cam_ccpp_cap,   only: cam_ccpp_physics_timestep_initial
      use cam_ccpp_cap,   only: cam_ccpp_physics_run
      use cam_ccpp_cap,   only: cam_ccpp_physics_timestep_final
      use physics_inputs, only: physics_read_data
      use cam_initfiles,  only: initial_file_get_id, unset_path_str
      use time_manager,   only: get_nstep
      use time_manager,   only: is_first_step
      use time_manager,   only: is_first_restart_step
      use physics_inputs, only: physics_check_data

      ! Dummy arguments
      real(kind_phys),       intent(in)    :: dtime_phys
      type(runtime_options), intent(in)    :: cam_runtime_opts
      type(physics_state),   intent(inout) :: phys_state
      type(physics_tend),    intent(inout) :: phys_tend
      type(cam_out_t),       intent(inout) :: cam_out
      type(cam_in_t),        intent(inout) :: cam_in
      ! Local variables
      type(file_desc_t), pointer :: ncdata
      character(len=512) :: errmsg
      integer            :: errcode
      integer                            :: part_ind
      integer                            :: col_start
      integer                            :: col_end
      integer                            :: data_frame
      logical                            :: use_init_variables

      errcode = 0
      ! Physics needs to read in all data not read in by the dycore
      ncdata => initial_file_get_id()

      ! data_frame is the next input frame for physics input fields
      ! Frame 1 is skipped for snapshot files
      !!XXgoldyXX: This section needs to have better logic once we know if
      !!           this is a physics test bench run.
      data_frame = get_nstep() + 2

      ! Initialize host model variables that must be done each time step:
      call physics_types_tstep_init()

      ! Determine if we should read initialized variables from file
      use_init_variables = (.not. is_first_step()) .and.                      &
         (.not. is_first_restart_step())

      call physics_read_data(ncdata, suite_names, data_frame,                 &
           read_initialized_variables=use_init_variables)

      ! Initialize the physics time step
      call cam_ccpp_physics_timestep_initial(phys_suite_name, dtime_phys,     &
           errmsg, errcode)
      if (errcode /= 0) then
         call endrun('cam_ccpp_physics_timestep_initial: '//trim(errmsg))
      end if

      ! Threading vars
      col_start = 1
      col_end = columns_on_task

      ! Run CCPP suite
      do part_ind = 1, size(suite_parts, 1)
         call cam_ccpp_physics_run(phys_suite_name, suite_parts(part_ind),    &
              col_start, col_end, dtime_phys, errmsg, errcode)
         if (errcode /= 0) then
            call endrun('cam_ccpp_physics_run: '//trim(errmsg))
         end if
      end do

      ! Finalize the time step
      call cam_ccpp_physics_timestep_final(phys_suite_name, dtime_phys,       &
           errmsg, errcode)
      if (errcode /= 0) then
         call endrun('cam_ccpp_physics_timestep_final: '//trim(errmsg))
      end if

      ! Determine if physics_check should be run:
      if (trim(ncdata_check) /= trim(unset_path_str)) then
         call physics_check_data(ncdata_check, suite_names, data_frame,       &
            min_difference, min_relative_value)
      end if

   end subroutine phys_run2

   subroutine phys_final(cam_runtime_opts, phys_state, phys_tend)
      use cam_abortutils, only: endrun
      use runtime_obj,    only: runtime_options
      use physics_types,  only: physics_state, physics_tend
      use camsrfexch,     only: cam_in_t, cam_out_t
      use cam_ccpp_cap,   only: cam_ccpp_physics_finalize

      ! Dummy arguments
      type(runtime_options), intent(in)    :: cam_runtime_opts
      type(physics_state),   intent(inout) :: phys_state
      type(physics_tend),    intent(inout) :: phys_tend
      ! Local variables
      real(kind_phys)    :: dtime_phys = 0.0_kind_phys ! Not used
      character(len=512) :: errmsg
      integer            :: errcode

      errcode = 0

      call cam_ccpp_physics_finalize(phys_suite_name, dtime_phys,             &
           errmsg, errcode)
      if (errcode /= 0) then
         call endrun('cam_ccpp_physics_finalize: '//trim(errmsg))
      end if
      deallocate(suite_names)
      deallocate(suite_parts)

   end subroutine phys_final

end module phys_comp
