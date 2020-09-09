module physics_grid

   use shr_kind_mod,        only: r8 => shr_kind_r8
   use physics_column_type, only: physics_column_t
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
   public :: get_rlat_all_p ! latitudes of physics cols on task (radians)
   public :: get_rlon_all_p ! longitudes of physics cols on task (radians)
   public :: global_index_p ! global column index of a physics column
   public :: local_index_p  ! local column index of a physics column
   public :: get_grid_dims  ! return grid dimensions

   ! The identifier for the physics grid
   integer, parameter, public          :: phys_decomp = 100

   ! dynamics field grid information
   ! hdim1_d and hdim2_d are dimensions of rectangular horizontal grid
   ! data structure, If 1D data structure, then hdim2_d == 1.
   integer                             :: hdim1_d, hdim2_d
   logical                             :: dycore_unstructured = .false.
   ! Dycore name and properties
   character(len=8), protected, public :: dycore_name = ''

   ! Physics decomposition information
   type(physics_column_t), pointer     :: phys_columns(:) => NULL()

   ! These variables are last to provide a limited table to search

   !> \section arg_table_physics_grid  Argument Table
   !! \htmlinclude arg_table_physics_grid.html
   !!
   integer,          protected, public :: pver = 0
   integer,          protected, public :: pverp = 0
   integer,          protected, public :: num_global_phys_cols = 0
   integer,          protected, public :: columns_on_task = 0
   integer,          protected, public :: index_top_layer = 0
   integer,          protected, public :: index_bottom_layer = 0
   integer,          protected, public :: index_top_interface = 1
   integer,          protected, public :: index_bottom_interface = 0
   logical,          protected, public :: phys_grid_initialized = .false.

!==============================================================================
CONTAINS
!==============================================================================

   subroutine phys_grid_init()
!      use mpi,              only: MPI_reduce ! XXgoldyXX: Should this work?
      use mpi,              only: MPI_INTEGER, MPI_MIN
      use cam_abortutils,   only: endrun
      use spmd_utils,       only: npes, mpicom
      use dyn_grid,         only: get_dyn_grid_info, physgrid_copy_attributes_d
      use cam_grid_support, only: cam_grid_register, cam_grid_attribute_register
      use cam_grid_support, only: iMap, hclen => max_hcoordname_len
      use cam_grid_support, only: horiz_coord_t, horiz_coord_create
      use cam_grid_support, only: cam_grid_attribute_copy, cam_grid_attr_exists

      ! Local variables
      integer                             :: index
      integer                             :: col_index
      integer                             :: first_dyn_column, last_dyn_column
      type(physics_column_t), pointer     :: dyn_columns(:) ! Dyn decomp
      ! Maps and values for physics grid
      real(r8),               pointer     :: lonvals(:)
      real(r8),               pointer     :: latvals(:)
      real(r8)                            :: lonmin, latmin
      integer(iMap),          pointer     :: grid_map(:,:)
      integer(iMap),          allocatable :: coord_map(:)
      type(horiz_coord_t),    pointer     :: lat_coord
      type(horiz_coord_t),    pointer     :: lon_coord
      real(r8),               pointer     :: area_d(:)
      logical                             :: unstructured
      real(r8)                            :: temp ! For MPI
      integer                             :: ierr ! For MPI
      character(len=hclen),   pointer     :: copy_attributes(:)
      character(len=hclen)                :: copy_gridname

      nullify(dyn_columns)
      nullify(lonvals)
      nullify(latvals)
      nullify(grid_map)
      nullify(lat_coord)
      nullify(lon_coord)
      nullify(area_d)
      nullify(copy_attributes)

      call t_adj_detailf(-2)
      call t_startf("phys_grid_init")

      ! Gather info from the dycore
      call get_dyn_grid_info(hdim1_d, hdim2_d, pver, dycore_name,             &
           index_top_layer, index_bottom_layer, dyn_columns)
      num_global_phys_cols = hdim1_d * hdim2_d
      pverp = pver + 1
      first_dyn_column = LBOUND(dyn_columns, 1)
      last_dyn_column = UBOUND(dyn_columns, 1)
      unstructured = hdim2_d <= 1
      !!XXgoldyXX: Can we enforce interface numbering separate from dycore?
      !!XXgoldyXX: This will work for both CAM and WRF/MPAS physics
      !!XXgoldyXX: This only has a 50% chance of working on a single level model
      if (index_top_layer < index_bottom_layer) then
         index_top_interface = index_top_layer
         index_bottom_interface = index_bottom_layer + 1
      else
         index_bottom_interface = index_bottom_layer
         index_top_interface = index_top_layer + 1
      end if

      ! Set up the physics decomposition
      columns_on_task = size(dyn_columns)
      phys_columns => dyn_columns

      ! Now that we are done settine up the physics decomposition, clean up
      if (.not. associated(phys_columns, target=dyn_columns)) then
         deallocate(dyn_columns)
      end if
      nullify(dyn_columns)

      ! Add physics-package grid to set of CAM grids
      ! physgrid always uses 'lat' and 'lon' as coordinate names; If dynamics
      !    grid is different, it will use different coordinate names

      ! First, create a map for the physics grid
      ! It's structure will depend on whether or not the physics grid is
      ! unstructured
      if (unstructured) then
         allocate(grid_map(3, columns_on_task))
      else
         allocate(grid_map(4, columns_on_task))
      end if
      grid_map = 0
      allocate(latvals(size(grid_map, 2)))
      allocate(lonvals(size(grid_map, 2)))

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
         allocate(coord_map(size(grid_map, 2)))
         ! We need a global minimum longitude and latitude
         if (npes > 1) then
            temp = lonmin
            call MPI_allreduce(temp, lonmin, 1, MPI_INTEGER, MPI_MIN,         &
                 mpicom, ierr)
            temp = latmin
            call MPI_allreduce(temp, latmin, 1, MPI_INTEGER, MPI_MIN,         &
                 mpicom, ierr)
         end if
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
      nullify(copy_attributes)
      call physgrid_copy_attributes_d(copy_gridname, copy_attributes)
      do index = 1, size(copy_attributes)
         call cam_grid_attribute_copy(copy_gridname, 'physgrid',              &
              copy_attributes(index))
      end do

      if ((.not. cam_grid_attr_exists('physgrid', 'area')) .and.              &
           unstructured) then
         ! Physgrid always needs an area attribute. If we did not inherit one
         !   from the dycore (i.e., physics and dynamics are on different
         !   grids), create that attribute here (Note, a separate physics
         !   grid is only supported for unstructured grids).
         allocate(area_d(size(grid_map, 2)))
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
      ! Cleanup, we are responsible for copy attributes
      if (associated(copy_attributes)) then
         deallocate(copy_attributes)
         nullify(copy_attributes)
      end if

      ! Set flag indicating physics grid is now set
      phys_grid_initialized = .true.

      call t_stopf("phys_grid_init")
      call t_adj_detailf(+2)

   end subroutine phys_grid_init

   !========================================================================

   real(r8) function get_dlat_p(index)
      use cam_logfile,    only: iulog
      use cam_abortutils, only: endrun
      ! latitude of a physics column in degrees

      ! Dummy argument
      integer, intent(in) :: index
      ! Local variables
      character(len=128)          :: errmsg
      character(len=*), parameter :: subname = 'get_dlat_p'

      if (.not. phys_grid_initialized) then
         call endrun(subname//': physics grid not initialized')
      else if ((index < 1) .or. (index > columns_on_task)) then
         write(errmsg, '(a,2(a,i0))') subname, ': index (', index,            &
              ') out of range (1 to ', columns_on_task
         write(iulog, *) errmsg
         call endrun(errmsg)
      else
         get_dlat_p = phys_columns(index)%lat_deg
      end if

   end function get_dlat_p

   !========================================================================

   real(r8) function get_dlon_p(index)
      use cam_logfile,    only: iulog
      use cam_abortutils, only: endrun
      ! longitude of a physics column in degrees

      ! Dummy argument
      integer, intent(in) :: index
      ! Local variables
      character(len=128)          :: errmsg
      character(len=*), parameter :: subname = 'get_dlon_p'

      if (.not. phys_grid_initialized) then
         call endrun(subname//': physics grid not initialized')
      else if ((index < 1) .or. (index > columns_on_task)) then
         write(errmsg, '(a,2(a,i0))') subname, ': index (', index,            &
              ') out of range (1 to ', columns_on_task
         write(iulog, *) errmsg
         call endrun(errmsg)
      else
         get_dlon_p = phys_columns(index)%lon_deg
      end if

   end function get_dlon_p

   !========================================================================

   real(r8) function get_rlat_p(index)
      use cam_logfile,    only: iulog
      use cam_abortutils, only: endrun
      ! latitude of a physics column in radians

      ! Dummy argument
      integer, intent(in) :: index
      ! Local variables
      character(len=128)          :: errmsg
      character(len=*), parameter :: subname = 'get_rlat_p'

      if (.not. phys_grid_initialized) then
         call endrun(subname//': physics grid not initialized')
      else if ((index < 1) .or. (index > columns_on_task)) then
         write(errmsg, '(a,2(a,i0))') subname, ': index (', index,            &
              ') out of range (1 to ', columns_on_task, ')'
         write(iulog, *) errmsg
         call endrun(errmsg)
      else
         get_rlat_p = phys_columns(index)%lat_rad
      end if

   end function get_rlat_p

   !========================================================================

   real(r8) function get_rlon_p(index)
      use cam_logfile,    only: iulog
      use cam_abortutils, only: endrun
      ! longitude of a physics column in radians

      ! Dummy argument
      integer, intent(in) :: index
      ! Local variables
      character(len=128)          :: errmsg
      character(len=*), parameter :: subname = 'get_rlon_p'

      if (.not. phys_grid_initialized) then
         call endrun(subname//': physics grid not initialized')
      else if ((index < 1) .or. (index > columns_on_task)) then
         write(errmsg, '(a,2(a,i0))') subname, ': index (', index,            &
              ') out of range (1 to ', columns_on_task, ')'
         write(iulog, *) errmsg
         call endrun(errmsg)
      else
         get_rlon_p = phys_columns(index)%lon_rad
      end if

   end function get_rlon_p

   !========================================================================

   real(r8) function get_area_p(index)
      use cam_logfile,    only: iulog
      use cam_abortutils, only: endrun
      ! area of a physics column in radians squared

      ! Dummy argument
      integer, intent(in) :: index
      ! Local variables
      character(len=128)          :: errmsg
      character(len=*), parameter :: subname = 'get_area_p'

      if (.not. phys_grid_initialized) then
         call endrun(subname//': physics grid not initialized')
      else if ((index < 1) .or. (index > columns_on_task)) then
         write(errmsg, '(a,2(a,i0))') subname, ': index (', index,            &
              ') out of range (1 to ', columns_on_task, ')'
         write(iulog, *) errmsg
         call endrun(errmsg)
      else
         get_area_p = phys_columns(index)%area
      end if

   end function get_area_p

   !========================================================================

   subroutine get_rlat_all_p(rlatdim, rlats)
      use cam_logfile,    only: iulog
      use cam_abortutils, only: endrun
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
      if (.not. phys_grid_initialized) then
         call endrun(subname//': physics grid not initialized')
      else if ((rlatdim < 1) .or. (rlatdim > columns_on_task)) then
         write(errmsg, '(a,3(a,i0))') subname, 'dimension provided (', rlatdim, &
              ') out of range (1 to ', columns_on_task, ')'
         write(iulog, *) trim(errmsg)
         call endrun(trim(errmsg))
      else
         do index = 1, rlatdim
            rlats(index) = phys_columns(index)%lat_rad
         end do
      end if

   end subroutine get_rlat_all_p

   !========================================================================

   subroutine get_rlon_all_p(rlondim, rlons)
      use cam_logfile,    only: iulog
      use cam_abortutils, only: endrun
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
      if (.not. phys_grid_initialized) then
         call endrun(subname//': physics grid not initialized')
      else if ((rlondim < 1) .or. (rlondim > columns_on_task)) then
         write(errmsg, '(a,3(a,i0))') subname, 'dimension provided (', rlondim, &
              ') out of range (1 to ', columns_on_task, ')'
         write(iulog, *) trim(errmsg)
         call endrun(trim(errmsg))
      else
         do index = 1, rlondim
            rlons(index) = phys_columns(index)%lon_rad
         end do
      end if

   end subroutine get_rlon_all_p

   !========================================================================

   integer function global_index_p(index)
      use cam_logfile,    only: iulog
      use cam_abortutils, only: endrun
      ! global column index of a physics column

      ! Dummy argument
      integer, intent(in) :: index
      ! Local variables
      character(len=128)          :: errmsg
      character(len=*), parameter :: subname = 'global_index_p'

      if (.not. phys_grid_initialized) then
         call endrun(subname//': physics grid not initialized')
      else if ((index < 1) .or. (index > columns_on_task)) then
         write(errmsg, '(a,2(a,i0))') subname, ': index (', index,            &
              ') out of range (1 to ', columns_on_task
         write(iulog, *) errmsg
         call endrun(errmsg)
      else
         global_index_p = phys_columns(index)%global_col_num
      end if

   end function global_index_p

   integer function local_index_p(index)
      use cam_logfile,    only: iulog
      use cam_abortutils, only: endrun
      ! global column index of a physics column

      ! Dummy argument
      integer, intent(in) :: index
      ! Local variables
      character(len=128)          :: errmsg
      character(len=*), parameter :: subname = 'local_index_p'

      if (.not. phys_grid_initialized) then
         call endrun(subname//': physics grid not initialized')
      else if ((index < 1) .or. (index > columns_on_task)) then
         write(errmsg, '(a,2(a,i0))') subname, ': index (', index,            &
              ') out of range (1 to ', columns_on_task
         write(iulog, *) errmsg
         call endrun(errmsg)
      else
         local_index_p = phys_columns(index)%phys_chunk_index
      end if

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

end module physics_grid
