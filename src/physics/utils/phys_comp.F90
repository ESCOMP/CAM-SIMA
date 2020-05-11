module phys_comp

   use ccpp_kinds,   only: kind_phys
   use shr_kind_mod, only: SHR_KIND_CS

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

!==============================================================================
CONTAINS
!==============================================================================

   subroutine phys_readnl(nlfilename)
      ! Read physics options, such as suite to run

      ! Dummy argument
      character(len=*), intent(in) :: nlfilename

   end subroutine phys_readnl

   subroutine phys_init(phys_state, phys_tend, cam_out)
      use pio,            only: file_desc_t
      use cam_abortutils, only: endrun
      use cam_initfiles,  only: initial_file_get_id
      use physics_types,  only: physics_state, physics_tend
      use camsrfexch,     only: cam_out_t
      use physics_grid,   only: columns_on_task
      use phys_vert_coord,only: pver, pverp
      use physics_data,   only: physics_read_data
      use physconst,      only: physconst_init
      use physics_types,  only: allocate_physics_types_fields
      use physics_types,  only: lagrangian_vertical
      use constituents,   only: pcnst
      use cam_ccpp_cap,   only: cam_ccpp_physics_initialize
      use cam_ccpp_cap,   only: ccpp_physics_suite_list
      use cam_ccpp_cap,   only: ccpp_physics_suite_part_list

      ! Dummy arguments
      type(physics_state), intent(inout) :: phys_state
      type(physics_tend),  intent(inout) :: phys_tend
      type(cam_out_t),     intent(inout) :: cam_out

      ! Local variables
      type(file_desc_t), pointer :: ncdata
      real(kind_phys)            :: dtime_phys = 0.0_kind_phys ! Not set yet
      character(len=512)         :: errmsg
      integer                    :: errflg = 0

      ncdata => initial_file_get_id()
      call physconst_init(columns_on_task, pver, pverp)
      call allocate_physics_types_fields(columns_on_task, pver, pverp,        &
           pcnst, set_init_val_in=.true., reallocate_in=.false.)
      !!XXgoldyXX: This needs to be set based on the dycore
      !!XXgoldyXX: Set via namelist?
      lagrangian_vertical = .false.
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
      ! Physics needs to read in all data not read in by the dycore
      call physics_read_data(ncdata, 2) ! Skip first timestep of data

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
      use cam_abortutils, only: endrun
      use physics_types,  only: physics_state, physics_tend
      use physics_grid,   only: columns_on_task
      use camsrfexch,     only: cam_in_t, cam_out_t
      use cam_ccpp_cap,   only: cam_ccpp_physics_timestep_initial
      use cam_ccpp_cap,   only: cam_ccpp_physics_run
      use cam_ccpp_cap,   only: cam_ccpp_physics_timestep_final

      ! Dummy arguments
      type(physics_state), intent(inout) :: phys_state
      real(kind_phys),     intent(in)    :: dtime_phys
      type(physics_tend),  intent(inout) :: phys_tend
      type(cam_out_t),     intent(inout) :: cam_out
      type(cam_in_t),      intent(inout) :: cam_in
      ! Local variables
      character(len=512) :: errmsg
      integer            :: errflg = 0
      integer                            :: part_ind
      integer                            :: col_start
      integer                            :: col_end

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
      integer            :: errflg = 0

      call cam_ccpp_physics_finalize(suite_name, dtime_phys, errmsg, errflg)
      if (errflg /= 0) then
         call endrun('cam_ccpp_physics_finalize: '//trim(errmsg))
      end if
      deallocate(suite_names)
      deallocate(suite_parts)

   end subroutine phys_final

end module phys_comp
