module phys_comp

   use ccpp_kinds,   only: kind_phys
   use shr_kind_mod, only: SHR_KIND_CS, SHR_KIND_CL

   implicit none
   private

   public :: phys_readnl
   public :: phys_init
   public :: phys_run1
   public :: phys_run2
   public :: phys_final

   ! Provate module data
   character(len=SHR_KIND_CS), allocatable :: suite_names(:)
   character(len=SHR_KIND_CS), allocatable :: suite_parts(:)
   ! suite_name: Suite we are running
   character(len=SHR_KIND_CS)              :: suite_name = ''
   logical                                 :: print_physics_check
   character(len=SHR_KIND_CL)              :: ncdata_check = ''
   character(len=SHR_KIND_CL)              :: cam_physics_mesh = ''
   character(len=SHR_KIND_CS)              :: cam_take_snapshot_before = ''
   character(len=SHR_KIND_CS)              :: cam_take_snapshot_after = ''
   real(kind_phys)                         :: min_difference
   real(kind_phys)                         :: min_relative_value

!==============================================================================
CONTAINS
!==============================================================================

   subroutine phys_readnl(nlfilename)
      ! Read physics options, such as suite to run
      use shr_kind_mod,    only: r8 => shr_kind_r8
      use shr_nl_mod,      only: find_group_name => shr_nl_find_group_name
      use shr_flux_mod,    only: shr_flux_adjust_constants
      use mpi,             only: mpi_char, mpi_real8, mpi_logical
      use spmd_utils,      only: masterproc, masterprocid, mpicom, npes
      use cam_logfile,     only: iulog
      use cam_abortutils,  only: endrun

      ! filepath for file containing namelist input
      character(len=*), intent(in) :: nlfilename

      ! Local variables
      integer :: unitn, ierr
      character(len=*), parameter :: subname = 'phys_readnl'

      namelist /physics_nl/ ncdata_check, print_physics_check,               &
         min_difference, min_relative_value, cam_take_snapshot_before,       &
         cam_take_snapshot_after, cam_physics_mesh

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
      write(iulog,*) ncdata_check
      ! Broadcast namelist variables
      if (npes > 1) then
         call mpi_bcast(ncdata_check, len(ncdata_check), mpi_char,            &
            masterprocid, mpicom, ierr)
         call mpi_bcast(print_physics_check, 1, mpi_logical, masterprocid,    &
            mpicom, ierr)
         call mpi_bcast(min_difference, 1, mpi_real8, masterprocid, mpicom,   &
            ierr)
         call mpi_bcast(min_relative_value, 1, mpi_real8, masterprocid,       &
            mpicom, ierr)
         call mpi_bcast(cam_physics_mesh, len(cam_physics_mesh), mpi_char,    &
            masterprocid, mpicom, ierr)
         call mpi_bcast(cam_take_snapshot_before,                             &
           len(cam_take_snapshot_before), mpi_char, masterprocid, mpicom,     &
           ierr)
         call mpi_bcast(cam_take_snapshot_after, len(cam_take_snapshot_after),&
           mpi_char, masterprocid, mpicom, ierr)
      end if

      ! Print out namelist variables
      if (masterproc) then
         write(iulog,*) subname, ' options:'
         if (print_physics_check) then
            write(iulog,*) '  Physics data check will be performed against: ',&
               ncdata_check
            write(iulog,*) 'Minimum Difference considered significant: ',     &
               min_difference
            write(iulog,*) 'Value Under Which Absolute Difference Calculated: ', &
               min_relative_value
         else
            write(iulog,*) '  Physics data check will not be performed'
         end if
      end if

   end subroutine phys_readnl

   subroutine phys_init(phys_state, phys_tend, cam_out)
      use pio,               only: file_desc_t
      use cam_abortutils,    only: endrun
      use physics_types,     only: physics_state, physics_tend
      use camsrfexch,        only: cam_out_t
      use physics_grid,      only: columns_on_task
      use vert_coord,        only: pver, pverp
      use physconst,         only: physconst_init
      use physics_types,     only: allocate_physics_types_fields
      use constituents,      only: pcnst
      use cam_ccpp_cap,      only: cam_ccpp_physics_initialize
      use cam_ccpp_cap,      only: ccpp_physics_suite_list
      use cam_ccpp_cap,      only: ccpp_physics_suite_part_list

      ! Dummy arguments
      type(physics_state), intent(inout) :: phys_state
      type(physics_tend),  intent(inout) :: phys_tend
      type(cam_out_t),     intent(inout) :: cam_out

      ! Local variables
      real(kind_phys)            :: dtime_phys = 0.0_kind_phys ! Not set yet
      character(len=512)         :: errmsg
      integer                    :: errflg

      errflg = 0
      call physconst_init(columns_on_task, pver, pverp)
      call allocate_physics_types_fields(columns_on_task, pver, pverp,        &
           pcnst, set_init_val_in=.true., reallocate_in=.false.)
      call ccpp_physics_suite_list(suite_names)
      suite_name = suite_names(1)
      call cam_ccpp_physics_initialize(suite_name, dtime_phys, errmsg, errflg)
      if (errflg /= 0) then
         call endrun('cam_ccpp_physics_initialize: '//trim(errmsg))
      end if
      call ccpp_physics_suite_part_list(suite_name, suite_parts, errmsg, errflg)
      if (errflg /= 0) then
         call endrun('cam_ccpp_suite_part_list: '//trim(errmsg))
      end if

   end subroutine phys_init

   !> \section arg_table_phys_run1  Argument Table
   !! \htmlinclude arg_table_phys_run1.html
   !!
   subroutine phys_run1(dtime_phys, phys_state, phys_tend, cam_in, cam_out)
      use cam_abortutils, only: endrun
      use physics_types,  only: physics_state, physics_tend
      use camsrfexch,     only: cam_in_t, cam_out_t

      ! Dummy arguments
      real(kind_phys),     intent(in)    :: dtime_phys
      type(physics_state), intent(inout) :: phys_state
      type(physics_tend),  intent(inout) :: phys_tend
      type(cam_in_t),      intent(inout) :: cam_in
      type(cam_out_t),     intent(inout) :: cam_out

   end subroutine phys_run1

   subroutine phys_run2(dtime_phys, phys_state, phys_tend, cam_in, cam_out)
      use pio,            only: file_desc_t
      use cam_abortutils, only: endrun
      use physics_types,  only: physics_state, physics_tend
      use physics_grid,   only: columns_on_task
      use camsrfexch,     only: cam_in_t, cam_out_t
      use cam_ccpp_cap,   only: cam_ccpp_physics_timestep_initial
      use cam_ccpp_cap,   only: cam_ccpp_physics_run
      use cam_ccpp_cap,   only: cam_ccpp_physics_timestep_final
      use physics_inputs, only: physics_read_data
      use cam_initfiles,  only: initial_file_get_id
      use time_manager,   only: get_nstep
      use time_manager,   only: is_first_step
      use time_manager,   only: is_first_restart_step
      use physics_inputs, only: physics_check_data

      ! Dummy arguments
      type(physics_state), intent(inout) :: phys_state
      real(kind_phys),     intent(in)    :: dtime_phys
      type(physics_tend),  intent(inout) :: phys_tend
      type(cam_out_t),     intent(inout) :: cam_out
      type(cam_in_t),      intent(inout) :: cam_in
      ! Local variables
      type(file_desc_t), pointer :: ncdata
      character(len=512) :: errmsg
      integer            :: errflg
      integer                            :: part_ind
      integer                            :: col_start
      integer                            :: col_end
      integer                            :: data_frame
      logical                            :: use_init_variables

      errflg = 0
      ! Physics needs to read in all data not read in by the dycore
      ncdata => initial_file_get_id()

      ! data_frame is the next input frame for physics input fields
      ! Frame 1 is skipped for snapshot files
      !!XXgoldyXX: This section needs to have better logic once we know if
      !!           this is a physics test bench run.
      data_frame = get_nstep() + 2

      ! Determine if we should read initialized variables from file
      use_init_variables = (.not. is_first_step()) .and.                      &
         (.not. is_first_restart_step())

      call physics_read_data(ncdata, suite_names, data_frame,                 &
           read_initialized_variables=use_init_variables)

      ! Initialize the physics time step
      call cam_ccpp_physics_timestep_initial(suite_name, dtime_phys,          &
           errmsg, errflg)
      if (errflg /= 0) then
         call endrun('cam_ccpp_physics_timestep_initial: '//trim(errmsg))
      end if

      ! Threading vars
      col_start = 1
      col_end = columns_on_task
      ! Run CCPP suite
      do part_ind = 1, size(suite_parts, 1)
         call cam_ccpp_physics_run(suite_name, suite_parts(part_ind),         &
              col_start, col_end, dtime_phys, errmsg, errflg)
         if (errflg /= 0) then
            call endrun('cam_ccpp_physics_run: '//trim(errmsg))
         end if
      end do
 
      ! Finalize the time step
      call cam_ccpp_physics_timestep_final(suite_name, dtime_phys,            &
           errmsg, errflg)
      if (errflg /= 0) then
         call endrun('cam_ccpp_physics_timestep_final: '//trim(errmsg))
      end if
 
      ! Determine if physics_check should be run:
      if (print_physics_check) then
         call physics_check_data(ncdata_check, suite_names, data_frame,       &
            min_difference, min_relative_value)
      end if

   end subroutine phys_run2

   subroutine phys_final(phys_state, phys_tend)
      use cam_abortutils, only: endrun
      use physics_types,  only: physics_state, physics_tend
      use camsrfexch,     only: cam_in_t, cam_out_t
      use cam_ccpp_cap,   only: cam_ccpp_physics_finalize

      ! Dummy arguments
      type(physics_state), intent(inout) :: phys_state
      type(physics_tend),  intent(inout) :: phys_tend
      ! Local variables
      real(kind_phys)    :: dtime_phys = 0.0_kind_phys ! Not used
      character(len=512) :: errmsg
      integer            :: errflg

      errflg = 0

      call cam_ccpp_physics_finalize(suite_name, dtime_phys, errmsg, errflg)
      if (errflg /= 0) then
         call endrun('cam_ccpp_physics_finalize: '//trim(errmsg))
      end if
      deallocate(suite_names)
      deallocate(suite_parts)

   end subroutine phys_final

end module phys_comp
