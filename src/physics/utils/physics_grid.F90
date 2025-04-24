module physics_grid

!------------------------------------------------------------------------------
!
! The phys_grid module represents the host model physics decomposition.
!
!  phys_grid_init receives the physics column info (area, weight, centers)
!                 from the dycore.
!                 The routine then creates the physics decomposition which
!                 is the arrangement of columns across the atmosphere model's
!                 MPI tasks as well as the arrangement into groups to
!                 facilitate efficient threading.
!                 The routine then creates a grid object to allow for data
!                 to be read into and written from this decomposition.
! The phys_grid module also provides interfaces for retrieving information
! about the decomposition
!
! Note: This current implementation does not perform load balancing,
!       physics columns ae always on the same task as the corresponding
!       column received from the dycore.
!
!------------------------------------------------------------------------------

   use shr_kind_mod,        only: r8 => shr_kind_r8
   use ccpp_kinds,          only: kind_phys
   use physics_column_type, only: physics_column_t, assignment(=)
   use perf_mod,            only: t_adj_detailf, t_startf, t_stopf

   implicit none
   private
   save

   ! Physics grid management
   public :: phys_grid_init ! initialize the physics grid

   ! Local task interfaces
   public :: get_dlat_p     ! latitude of a physics column in degrees
   public :: get_dlon_p     ! longitude of a physics column in degrees
   public :: get_rlat_p     ! latitude of a physics column in radians
   public :: get_rlon_p     ! longitude of a physics column in radians
   public :: get_area_p     ! area of a physics column in radians squared
   public :: get_wght_p     ! weight of a physics column in radians squared
   public :: get_rlat_all_p ! latitudes of physics cols on task (radians)
   public :: get_rlon_all_p ! longitudes of physics cols on task (radians)
   public :: get_wght_all_p ! weights of physics cols on task
   public :: get_dyn_col_p  ! dynamics local blk number and blk offset(s)
   public :: global_index_p ! global column index of a physics column
   public :: local_index_p  ! local column index of a physics column
   public :: get_grid_dims  ! return grid dimensions

   ! Private subroutines
   private :: check_phys_input !Checks that physics grid is initialized and that
                               !provided physics column index is valid.

   ! The identifier for the physics grid
   integer, parameter, public          :: phys_decomp = 100

   ! dynamics field grid information
   ! hdim1_d and hdim2_d are dimensions of rectangular horizontal grid
   ! data structure, If 1D data structure, then hdim2_d == 1.
   integer, protected, public          :: hdim1_d, hdim2_d
   logical                             :: dycore_unstructured = .false.
   ! Dycore name and properties
   character(len=8), protected, public :: dycore_name = ''

   ! Physics decomposition information
   type(physics_column_t), protected, public, allocatable :: phys_columns(:)

   ! Memory debugging control
   logical :: calc_memory_increase = .false.

   ! These variables are last to provide a limited table to search

   !> \section arg_table_physics_grid  Argument Table
   !! \htmlinclude arg_table_physics_grid.html
   !!
   integer,          protected, public :: num_global_phys_cols = 0
   integer,          protected, public :: columns_on_task = 0
   integer,          protected, public :: col_start = 1
   integer,          protected, public :: col_end = 0
   logical,          protected, public :: phys_grid_initialized = .false.

   real(kind_phys), protected, allocatable, public :: lat_rad(:)
   real(kind_phys), protected, allocatable, public :: lon_rad(:)
   real(kind_phys), protected, allocatable, public :: lat_deg(:)
   real(kind_phys), protected, allocatable, public :: lon_deg(:)
   real(kind_phys), protected, allocatable, public :: area(:)
   real(kind_phys), protected, allocatable, public :: weight(:)

!==============================================================================
CONTAINS
!==============================================================================

   subroutine phys_grid_init(hdim1_d_in, hdim2_d_in, dycore_name_in, &
                             dyn_columns, dyn_gridname, dyn_attributes)

!      use mpi,              only: MPI_reduce ! XXgoldyXX: Should this work?
      use mpi,              only: MPI_INTEGER, MPI_REAL8, MPI_MIN, MPI_MAX
      use shr_mem_mod,      only: shr_mem_getusage
      use cam_abortutils,   only: endrun, check_allocate
      use cam_logfile,      only: iulog
      use spmd_utils,       only: npes, mpicom, masterprocid, masterproc
      use string_utils,     only: to_str
      use cam_map_utils,    only: iMap
      use cam_grid_support, only: cam_grid_register, cam_grid_attribute_register
      use cam_grid_support, only: horiz_coord_t, horiz_coord_create
      use cam_grid_support, only: cam_grid_attribute_copy, cam_grid_attr_exists

      ! Dummy (input) variables:
      integer, intent(in) :: hdim1_d_in            ! First dyn grid horizontal dimension
      integer, intent(in) :: hdim2_d_in            ! Second dyn grid horizontal dimension

      character(len=*), intent(in) :: dycore_name_in    ! Name of dycore
      character(len=*), intent(in) :: dyn_gridname      ! Name of dynamics grid
      character(len=*), intent(in) :: dyn_attributes(:) ! Dyanmics grid attributes

      type(physics_column_t), intent(in) :: dyn_columns(:) ! physics columns structure on dynamics grid

      ! Local variables
      integer                             :: index
      integer                             :: col_index
      integer                             :: first_dyn_column, last_dyn_column
      ! Maps and values for physics grid
      real(r8),               pointer     :: lonvals(:)
      real(r8),               pointer     :: latvals(:)
      real(r8)                            :: lonmin, latmin
      integer(iMap),          pointer     :: grid_map(:,:)
      integer(iMap),          allocatable :: coord_map(:)
      type(horiz_coord_t),    pointer     :: lat_coord
      type(horiz_coord_t),    pointer     :: lon_coord
      real(r8),               pointer     :: area_d(:)
      real(r8)                            :: mem_hw_beg, mem_hw_end
      real(r8)                            :: mem_beg, mem_end
      logical                             :: unstructured
      real(r8)                            :: temp ! For MPI
      integer                             :: ierr ! For error codes

      character(len=*),  parameter :: subname = 'phys_grid_init'

      nullify(lonvals)
      nullify(latvals)
      nullify(grid_map)
      nullify(lat_coord)
      nullify(lon_coord)
      nullify(area_d)

      if (calc_memory_increase) then
         call shr_mem_getusage(mem_hw_beg, mem_beg)
      end if

      ! Check that the physics grid is not already initialized:
      if (phys_grid_initialized) then
         call endrun(subname//": Physics grid is already initialized.")
      end if

      call t_adj_detailf(-2)
      call t_startf("phys_grid_init")

      ! Set public variables:
      hdim1_d            = hdim1_d_in
      hdim2_d            = hdim2_d_in
      dycore_name        = dycore_name_in

      unstructured     = hdim2_d <= 1

      ! Calculate total number of physics columns:
      num_global_phys_cols = hdim1_d * hdim2_d

      ! Calculate number of columns on tasks:
      columns_on_task = size(dyn_columns)
      col_end = columns_on_task

      ! Allocate phys_columns:
      allocate(phys_columns(columns_on_task), stat=ierr)
      call check_allocate(ierr, subname, 'phys_columns(columns_on_task)', &
                          file=__FILE__, line=__LINE__)

      ! Allocate public physics grid variables for CCPP:
      allocate(lat_rad(columns_on_task), stat=ierr)
      call check_allocate(ierr, subname, 'lat_rad(columns_on_task)', &
                          file=__FILE__, line=__LINE__)

      allocate(lon_rad(columns_on_task), stat=ierr)
      call check_allocate(ierr, subname, 'lon_rad(columns_on_task)', &
                          file=__FILE__, line=__LINE__)

      allocate(lat_deg(columns_on_task), stat=ierr)
      call check_allocate(ierr, subname, 'lat_deg(columns_on_task)', &
                          file=__FILE__, line=__LINE__)

      allocate(lon_deg(columns_on_task), stat=ierr)
      call check_allocate(ierr, subname, 'lon_deg(columns_on_task)', &
                          file=__FILE__, line=__LINE__)

      allocate(area(columns_on_task), stat=ierr)
      call check_allocate(ierr, subname, 'area(columns_on_task)', &
                          file=__FILE__, line=__LINE__)

      allocate(weight(columns_on_task), stat=ierr)
      call check_allocate(ierr, subname, 'weight(columns_on_task)', &
                          file=__FILE__, line=__LINE__)

      ! Set column index bounds:
      first_dyn_column = 1
      last_dyn_column  = columns_on_task

      ! Set up the physics decomposition and public variables:
      do index = first_dyn_column, last_dyn_column
         !physics columns:
         phys_columns(index) = dyn_columns(index)

         !public CCPP variables:
         lat_rad(index) = phys_columns(index)%lat_rad
         lon_rad(index) = phys_columns(index)%lon_rad
         lat_deg(index) = phys_columns(index)%lat_deg
         lon_deg(index) = phys_columns(index)%lon_deg
         area(index)    = phys_columns(index)%area
         weight(index)  = phys_columns(index)%weight
      end do

      ! Add physics-package grid to set of CAM grids
      ! physgrid always uses 'lat' and 'lon' as coordinate names; If dynamics
      !    grid is different, it will use different coordinate names

      ! First, create a map for the physics grid
      ! It's structure will depend on whether or not the physics grid is
      ! unstructured
      if (unstructured) then
         allocate(grid_map(3, columns_on_task), stat=ierr)
         call check_allocate(ierr, subname, 'grid_map(3, columns_on_task)', &
                             file=__FILE__, line=__LINE__)
      else
         allocate(grid_map(4, columns_on_task), stat=ierr)
         call check_allocate(ierr, subname, 'grid_map(4, columns_on_task)', &
                             file=__FILE__, line=__LINE__)
      end if
      grid_map = 0

      allocate(latvals(columns_on_task), stat=ierr)
      call check_allocate(ierr, subname, 'latvals(columns_on_task)', &
                          file=__FILE__, line=__LINE__)

      allocate(lonvals(columns_on_task), stat=ierr)
      call check_allocate(ierr, subname, 'lonvals(columns_on_task)', &
                          file=__FILE__, line=__LINE__)

      lonmin = 1000.0_r8 ! Out of longitude range
      latmin = 1000.0_r8 ! Out of latitude range
      do col_index = 1, columns_on_task
         latvals(col_index) = phys_columns(col_index)%lat_deg
         if (latvals(col_index) < latmin) then
            latmin = latvals(col_index)
         end if
         lonvals(col_index) = phys_columns(col_index)%lon_deg
         if (lonvals(col_index) < lonmin) then
            lonmin = lonvals(col_index)
         end if
         grid_map(1, col_index) = col_index
         grid_map(2, col_index) = 0 ! No chunking in physics anymore
         if (unstructured) then
            grid_map(3, col_index) = phys_columns(col_index)%global_col_num
         else
            ! lon
            grid_map(3, col_index) = phys_columns(col_index)%coord_indices(1)
            ! lat
            grid_map(4, col_index) = phys_columns(col_index)%coord_indices(2)
         end if
      end do

      ! Note that if the dycore is using the same points as the physics grid,
      !      it will have already set up 'lat' and 'lon' axes for
      !      the physics grid
      !      However, these will be in the dynamics decomposition

      if (unstructured) then
         lon_coord => horiz_coord_create('lon', 'ncol', num_global_phys_cols, &
              'longitude', 'degrees_east', 1, size(lonvals), lonvals,         &
              map=grid_map(3,:))
         lat_coord => horiz_coord_create('lat', 'ncol', num_global_phys_cols, &
              'latitude', 'degrees_north', 1, size(latvals), latvals,         &
              map=grid_map(3,:))
      else
         allocate(coord_map(columns_on_task), stat=ierr)
         call check_allocate(ierr, subname, 'coord_map(columns_on_task)', &
                          file=__FILE__, line=__LINE__)

         ! We need a global minimum longitude and latitude
         temp = lonmin
         call MPI_allreduce(temp, lonmin, 1, MPI_INTEGER, MPI_MIN,         &
              mpicom, ierr)
         temp = latmin
         call MPI_allreduce(temp, latmin, 1, MPI_INTEGER, MPI_MIN,         &
              mpicom, ierr)
         ! Create lon coord map which only writes from one of each unique lon
         where(latvals == latmin)
            coord_map(:) = grid_map(3, :)
         elsewhere
            coord_map(:) = 0_iMap
         end where
         lon_coord => horiz_coord_create('lon', 'lon', hdim1_d,            &
              'longitude', 'degrees_east', 1, size(lonvals), lonvals,      &
              map=coord_map)

         ! Create lat coord map which only writes from one of each unique lat
         where(lonvals == lonmin)
            coord_map(:) = grid_map(4, :)
         elsewhere
            coord_map(:) = 0_iMap
         end where
            lat_coord => horiz_coord_create('lat', 'lat', hdim2_d,            &
              'latitude', 'degrees_north', 1, size(latvals), latvals,      &
              map=coord_map)
         deallocate(coord_map)
      end if
      call cam_grid_register('physgrid', phys_decomp,                         &
           lat_coord, lon_coord, grid_map, src_in=(/ 1, 0 /),                 &
           unstruct=unstructured, block_indexed=.false.)

      ! Copy required attributes from the dynamics array
      do index = 1, size(dyn_attributes)
         call cam_grid_attribute_copy(dyn_gridname, 'physgrid',              &
              dyn_attributes(index))
      end do

      if ((.not. cam_grid_attr_exists('physgrid', 'area')) .and.              &
           unstructured) then
         ! Physgrid always needs an area attribute. If we did not inherit one
         !   from the dycore (i.e., physics and dynamics are on different
         !   grids), create that attribute here (Note, a separate physics
         !   grid is only supported for unstructured grids).
         allocate(area_d(columns_on_task), stat=ierr)
         call check_allocate(ierr, subname, 'area_d(columns_on_task)', &
                          file=__FILE__, line=__LINE__)

         do col_index = 1, columns_on_task
            area_d(col_index) = phys_columns(col_index)%area
         end do
         call cam_grid_attribute_register('physgrid', 'area',                 &
              'physics column areas', 'ncol', area_d, map=grid_map(3,:))
         nullify(area_d) ! Belongs to attribute now
      end if
      ! Cleanup pointers (they belong to the grid now)
      nullify(grid_map)
      deallocate(latvals)
      nullify(latvals)
      deallocate(lonvals)
      nullify(lonvals)

      ! Set flag indicating physics grid is now set
      phys_grid_initialized = .true.

      call t_stopf("phys_grid_init")
      call t_adj_detailf(+2)

      ! Calculate memory usage stats if requested:
      if (calc_memory_increase) then
         call shr_mem_getusage(mem_hw_end, mem_end)
         temp = mem_end - mem_beg
         call MPI_reduce(temp, mem_end, 1, MPI_REAL8, MPI_MAX, masterprocid,  &
              mpicom, ierr)
         if (masterproc) then
            write(iulog, *) 'phys_grid_init: Increase in memory usage = ',    &
                 mem_end, ' (MB)'
         end if
         temp = mem_hw_end - mem_hw_beg
         call MPI_reduce(temp, mem_hw_end, 1, MPI_REAL8, MPI_MAX,             &
              masterprocid, mpicom, ierr)
         if (masterproc) then
            write(iulog, *) subname, 'Increase in memory highwater = ',       &
                 mem_end, ' (MB)'
         end if
      end if

   end subroutine phys_grid_init

   !========================================================================

   real(r8) function get_dlat_p(index)
      ! latitude of a physics column in degrees

      ! Dummy argument
      integer, intent(in) :: index
      ! Local variables
      character(len=128)          :: errmsg
      character(len=*), parameter :: subname = 'get_dlat_p'

      ! Check that input is valid:
      call check_phys_input(subname, index)

      get_dlat_p = phys_columns(index)%lat_deg

   end function get_dlat_p

   !========================================================================

   real(r8) function get_dlon_p(index)
      ! longitude of a physics column in degrees

      ! Dummy argument
      integer, intent(in) :: index
      ! Local variables
      character(len=128)          :: errmsg
      character(len=*), parameter :: subname = 'get_dlon_p'

      ! Check that input is valid:
      call check_phys_input(subname, index)

      get_dlon_p = phys_columns(index)%lon_deg

   end function get_dlon_p

   !========================================================================

   real(r8) function get_rlat_p(index)
      ! latitude of a physics column in radians

      ! Dummy argument
      integer, intent(in) :: index
      ! Local variables
      character(len=128)          :: errmsg
      character(len=*), parameter :: subname = 'get_rlat_p'

      ! Check that input is valid:
      call check_phys_input(subname, index)

      get_rlat_p = phys_columns(index)%lat_rad

   end function get_rlat_p

   !========================================================================

   real(r8) function get_rlon_p(index)
      ! longitude of a physics column in radians

      ! Dummy argument
      integer, intent(in) :: index
      ! Local variables
      character(len=128)          :: errmsg
      character(len=*), parameter :: subname = 'get_rlon_p'

      ! Check that input is valid:
      call check_phys_input(subname, index)

      get_rlon_p = phys_columns(index)%lon_rad

   end function get_rlon_p

   !========================================================================

   real(r8) function get_area_p(index)
      ! area of a physics column in radians squared

      ! Dummy argument
      integer, intent(in) :: index
      ! Local variables
      character(len=128)          :: errmsg
      character(len=*), parameter :: subname = 'get_area_p'

      ! Check that input is valid:
      call check_phys_input(subname, index)

      get_area_p = phys_columns(index)%area

   end function get_area_p

   !========================================================================

   real(r8) function get_wght_p(index)
      ! weight of a physics column in radians squared

      ! Dummy argument
      integer, intent(in) :: index
      ! Local variables
      character(len=128)          :: errmsg
      character(len=*), parameter :: subname = 'get_wght_p'

      ! Check that input is valid:
      call check_phys_input(subname, index)

      get_wght_p = phys_columns(index)%weight

   end function get_wght_p

   !========================================================================

   subroutine get_rlat_all_p(rlatdim, rlats)
      !-----------------------------------------------------------------------
      !
      ! get_rlat_all_p: Return all latitudes (in radians) on task.
      !
      !-----------------------------------------------------------------------
      ! Dummy Arguments
      integer,  intent(in)  :: rlatdim        ! declared size of output array
      real(r8), intent(out) :: rlats(rlatdim) ! array of latitudes

      ! Local variables
      integer                     :: index ! loop index
      character(len=128)          :: errmsg
      character(len=*), parameter :: subname = 'get_rlat_all_p: '

      !-----------------------------------------------------------------------

      ! Check that input is valid:
      call check_phys_input(subname, rlatdim)

      do index = 1, rlatdim
         rlats(index) = phys_columns(index)%lat_rad
      end do

   end subroutine get_rlat_all_p

   !========================================================================

   subroutine get_rlon_all_p(rlondim, rlons)
      !-----------------------------------------------------------------------
      !
      ! get_rlon_all_p: Return all longitudes (in radians) on task.
      !
      !-----------------------------------------------------------------------
      ! Dummy Arguments
      integer,  intent(in)  :: rlondim        ! declared size of output array
      real(r8), intent(out) :: rlons(rlondim) ! array of longitudes

      ! Local variables
      integer                     :: index ! loop index
      character(len=128)          :: errmsg
      character(len=*), parameter :: subname = 'get_rlon_all_p: '

      !-----------------------------------------------------------------------

      ! Check that input is valid:
      call check_phys_input(subname, rlondim)

      do index = 1, rlondim
         rlons(index) = phys_columns(index)%lon_rad
      end do

   end subroutine get_rlon_all_p

   !========================================================================

   subroutine get_wght_all_p(wghtdim, wghts)
      !-----------------------------------------------------------------------
      !
      ! get_wght_all_p: Return all weights on task.
      !
      !-----------------------------------------------------------------------
      ! Dummy Arguments
      integer,  intent(in)  :: wghtdim        ! declared size of output array
      real(r8), intent(out) :: wghts(wghtdim) ! array of weights

      ! Local variables
      integer                     :: index ! loop index
      character(len=128)          :: errmsg
      character(len=*), parameter :: subname = 'get_wght_all_p: '

      !-----------------------------------------------------------------------

      ! Check that input is valid:
      call check_phys_input(subname, wghtdim)

      do index = 1, wghtdim
         wghts(index) = phys_columns(index)%weight
      end do

   end subroutine get_wght_all_p

   !========================================================================

   subroutine get_dyn_col_p(index, blk_num, blk_ind)
      use cam_abortutils, only: endrun
      ! Return the dynamics local block number and block offset(s) for
      ! the physics column indicated by <index>.

      ! Dummy arguments
      integer, intent(in)  :: index         ! index of local physics column
      integer, intent(out) :: blk_num       ! Local dynamics block index
      integer, intent(out) :: blk_ind(:)    ! Local dynamics block offset(s)
      ! Local variables
      integer                     :: off_size
      character(len=128)          :: errmsg
      character(len=*), parameter :: subname = 'get_dyn_col_p_index: '

      ! Check that input is valid:
      call check_phys_input(subname, index)

      off_size = SIZE(phys_columns(index)%dyn_block_index, 1)
      if (SIZE(blk_ind, 1) < off_size) then
         call endrun(subname//'blk_ind too small')
      end if
      blk_num = phys_columns(index)%local_dyn_block
      blk_ind(1:off_size) = phys_columns(index)%dyn_block_index(1:off_size)
      if (SIZE(blk_ind, 1) > off_size) then
         blk_ind(off_size+1:) = -1
      end if

   end subroutine get_dyn_col_p

   !========================================================================

   integer function global_index_p(index)
      ! global column index of a physics column

      ! Dummy argument
      integer, intent(in) :: index
      ! Local variables
      character(len=128)          :: errmsg
      character(len=*), parameter :: subname = 'global_index_p'

      ! Check that input is valid:
      call check_phys_input(subname, index)

      global_index_p = phys_columns(index)%global_col_num

   end function global_index_p

   integer function local_index_p(index)
      ! global column index of a physics column

      ! Dummy argument
      integer, intent(in) :: index
      ! Local variables
      character(len=128)          :: errmsg
      character(len=*), parameter :: subname = 'local_index_p'

      ! Check that input is valid:
      call check_phys_input(subname, index)

      local_index_p = phys_columns(index)%phys_chunk_index

   end function local_index_p

   subroutine get_grid_dims(hdim1_d_out, hdim2_d_out)
      use cam_abortutils, only: endrun
      ! retrieve dynamics field grid information
      ! hdim1_d and hdim2_d are dimensions of rectangular horizontal grid
      ! data structure, If 1D data structure, then hdim2_d == 1.
      integer, intent(out) :: hdim1_d_out
      integer, intent(out) :: hdim2_d_out

      if (.not. phys_grid_initialized) then
         call endrun('get_grid_dims: physics grid not initialized')
      end if
      hdim1_d_out = hdim1_d
      hdim2_d_out = hdim2_d

   end subroutine get_grid_dims

   !========================================================================

   subroutine check_phys_input(subname, index_val)
      use cam_abortutils, only: endrun
      use string_utils,   only: to_str
      ! Check that the physics grid is initialized, and that the
      ! user-provided index value is within an acceptable range.
      ! If either check fails then end the model simulation.

      character(len=*), intent(in) :: subname   !Calling subroutine name
      integer, intent(in)          :: index_val !User-specified index value

      ! Check if physics grid is initialized,
      ! if so, then check that index value is within bounds:
      if (.not. phys_grid_initialized) then
         call endrun(subname//': physics grid not initialized')
      else if ((index_val < 1) .or. (index_val > columns_on_task)) then
         call endrun(subname//': index ('//to_str(index_val)//&
                     ') out of range (1 to '//&
                     to_str(columns_on_task)//')')
      end if

   end subroutine check_phys_input

end module physics_grid
