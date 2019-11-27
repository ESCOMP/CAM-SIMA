module dyn_grid

   use shr_kind_mod,        only: r8 => shr_kind_r8
   use shr_sys_mod,         only: shr_sys_flush
   use cam_logfile,         only: iulog
   use spmd_utils,          only: masterproc
   use physics_column_type, only: physics_column_t

   implicit none
   private
   save

   public dyn_grid_init
   public get_dyn_grid_info
   public physgrid_copy_attributes_d

   type(physics_column_t), public, protected, allocatable :: local_columns(:)
   ! Private module variables
   integer               :: num_levels         = -1
   integer               :: num_global_columns = -1
   integer               :: num_lats           = -1 ! Global
   integer               :: num_lons           = -1 ! Global (or 1 for unstruct)
   integer               :: num_local_columns  = -1
   integer               :: global_col_offset  = -HUGE(1)
   integer,  allocatable :: lon_lat_indices(:, :)
   real(r8), allocatable :: local_lats_rad(:)
   real(r8), allocatable :: local_lons_rad(:)
   real(r8), allocatable :: local_lats_deg(:)
   real(r8), allocatable :: local_lons_deg(:)
   real(r8), allocatable :: local_areas(:)
   real(r8), allocatable :: local_weights(:)

   logical               :: grid_is_latlon = .true.
   character(len=8)      :: lev_name       = 'UNSET'
   integer               :: lev_dim_ind    = -1 ! lev location in 3D var
   character(len=8)      :: lev_dim_name   = 'UNSET'
   character(len=8)      :: lat_name       = 'UNSET'
   integer               :: lat_dim_ind    = -1 ! lat (or col) loc in 3D var
   character(len=8)      :: lat_dim_name   = 'UNSET'
   character(len=8)      :: lon_name       = 'UNSET'
   integer               :: lon_dim_ind    = -1 ! lon (or col) loc in 3D var
   character(len=8)      :: lon_dim_name   = 'UNSET'
   character(len=64)     :: fieldname

   integer, parameter    :: MAX_DIMS = 8
   character(len=*), parameter :: gridname = 'null_dycore_grid'

   ! Private module routines
   private :: find_units
   private :: find_dimension

!==============================================================================
CONTAINS
!==============================================================================

   subroutine dyn_grid_init()
      use pio,            only: file_desc_t, var_desc_t, io_desc_t
      use pio,            only: iMap=>PIO_OFFSET_KIND, PIO_DOUBLE
      use pio,            only: PIO_BCAST_ERROR, pio_seterrorhandling
      use pio,            only: pio_get_var, PIO_NOERR, pio_freedecomp
      use pio,            only: pio_read_darray
      use spmd_utils,     only: npes, iam
      use cam_pio_utils,  only: cam_pio_handle_error, cam_pio_find_var
      use cam_pio_utils,  only: cam_pio_var_info, pio_subsystem
      use cam_pio_utils,  only: cam_pio_newdecomp, cam_pio_handle_error
      use cam_abortutils, only: endrun
      use cam_initfiles,  only: initial_file_get_id

      ! Initialize a dynamics decomposition based on an input data file

      ! Local variables
      type(file_desc_t), pointer     :: fh_ini
      type(var_desc_t)               :: lat_vardesc
      type(var_desc_t)               :: lon_vardesc
      type(var_desc_t)               :: vardesc
      type(io_desc_t),   pointer     :: iodesc
      integer                        :: err_handling
      logical                        :: var_found
      logical                        :: is_degrees
      logical                        :: is_lat
      integer                        :: num_var_dims
      integer                        :: time_id
      integer                        :: lindex
      integer                        :: dimids(MAX_DIMS)
      integer                        :: dimlens(MAX_DIMS)
      integer                        :: num_local_cols
      integer                        :: col_mod ! Temp for calculating decomp
      integer                        :: col_start, col_end
      integer                        :: start(1), kount(1)
      integer                        :: iret
      integer(iMap),     allocatable :: ldof(:) ! For reading coordinates
      character(len=128)             :: var_name
      character(len=256)             :: dimnames(MAX_DIMS)
      character(len=8)               :: column_name
      character(len=8)               :: lat_dim_name
      character(len=8)               :: lon_dim_name
      character(len=128)             :: errormsg

      character(len=*),  parameter   :: subname = 'dyn_grid_init'

      nullify(iodesc)

      ! Get file handle for initial file to find coordinates
      fh_ini => initial_file_get_id()

      ! We will handle errors for this routine
      call pio_seterrorhandling(fh_ini, PIO_BCAST_ERROR, err_handling)
      ! Find the latitude variable and dimension(s)
      call cam_pio_find_var(fh_ini, (/ 'lat', 'lat_d' /), lat_name,           &
           lat_vardesc, var_found)
      if (var_found) then
         ! Find the variable latitude dimension info
         call cam_pio_var_info(fh_ini, lat_vardesc, num_var_dims, dimids,     &
              dimlens, dimnames=dimnames, unlimDimID=time_id)
         lat_dim_name = dimnames(1)
         if (index(lat_dim_name, 'ncol') > 0) then
            grid_is_latlon = .false. ! lat/lon dycore grid
            num_lats = 1
            num_lons = dimlens(1)
         else if (index(lat_dim_name, 'lat') > 0) then
            grid_is_latlon = .true. ! Unstructured dycore grid
            num_lats = dimlens(1)
         else
            write(errormsg, '(4a)') subname,                                  &
                 ": Unknown grid lat dimension, '", trim(lat_dim_name), "'"
            call endrun(errormsg)
         end if
      else
         write(errormsg, '(3a)') subname, ": Could not find latitude ",       &
              "on initial data file"
         call endrun(errormsg)
      end if
      ! Find the longitude variable and dimension(s)
      call cam_pio_find_var(fh_ini, (/ 'lon', 'lon_d' /), lon_name,           &
           lon_vardesc, var_found)
      if (var_found) then
         ! Find the longitude variable dimension info
         call cam_pio_var_info(fh_ini, lon_vardesc, num_var_dims, dimids,     &
              dimlens, dimnames=dimnames, unlimDimID=time_id)
         lon_dim_name = dimnames(1)
         if (grid_is_latlon) then
            num_lons = dimlens(1)
            if (index(lon_dim_name, 'lon') <= 0) then
               write(errormsg, '(5a)') subname, ": Bad Longitude variable ",  &
                    "dimension, '", trim(lon_dim_name), "'"
               call endrun(errormsg)
            end if ! No else needed, everything is fine
         else if (index(lat_dim_name, 'ncol') <= 0) then
            write(errormsg, '(8a)') subname, ": Longitude variable ",         &
                 "dimension, '", trim(lon_dim_name), "', does not match ",    &
                 "latitude, '", trim(lat_dim_name), "'"
               call endrun(errormsg)
         end if ! No else, we have a good dimension
      else
         write(errormsg, '(3a)') subname, ": Could not find longitude ",      &
              "on initial data file"
         call endrun(errormsg)
      end if
      ! Compute our decomposition
      num_global_columns = num_lons * num_lats
      num_local_columns = num_global_columns / npes
      col_mod = MOD(num_global_columns, npes)
      global_col_offset = (num_local_columns * iam) + MIN(iam, col_mod)
      col_start = global_col_offset + 1
      if (iam < col_mod) then
         num_local_columns = num_local_columns + 1
      end if
      col_end = col_start + num_local_columns - 1
      ! Find a 3D variable and get its dimensions
      call cam_pio_find_var(fh_ini, (/ 'U', 'u_snapshot' /), fieldname,       &
           vardesc, var_found)
      if (var_found) then
         ! Find the variable dimension info
         dimnames = ''
         dimids = -1
         call cam_pio_var_info(fh_ini, vardesc, num_var_dims, dimids,         &
              dimlens, dimnames=dimnames, unlimDimID=time_id)

         do lindex = 1, num_var_dims
            if (trim(dimnames(lindex)) == lat_dim_name) then
               lat_dim_ind = lindex
            else if (trim(dimnames(lindex)) == lon_dim_name) then
               lon_dim_ind = lindex
            else if (index(dimnames(lindex), 'lev') > 0) then ! Need uppercase?
               num_levels = dimlens(lindex)
               lev_dim_ind = lindex
            else if (index(dimnames(lindex), 'time') > 0) then
               ! Just ignore this dimension
            else
               write(errormsg, '(4a)') subname, "Unknown dimension, '",       &
                    trim(dimnames(lindex)), "'"
            end if
         end do
      else
         write(errormsg, '(3a)') subname, ": Could not find wind variable, ", &
              "U, on initial data file"
         call endrun(errormsg)
      end if
      ! Did we find a number of levels?
      if (num_levels < 1) then
         write(errormsg, '(3a)') subname, ": Could not find number of levels"
         call endrun(errormsg)
      end if
      ! Find the local column coordinates
      ! Check the units
      call find_units(fh_ini, lat_vardesc, is_degrees, is_lat)
      if (.not. is_lat) then
         call endrun(subname//': bad latitude units')
      end if
      if (.not. is_degrees) then
         call endrun(subname//': unsupported latitude units')
      end if
      call find_units(fh_ini, lon_vardesc, is_degrees, is_lat)
      if (is_lat) then
         call endrun(subname//': bad longitude units')
      end if
      if (.not. is_degrees) then
         ! NB: If radians becomes supported, additional check needed here
         call endrun(subname//': unsupported longitude units')
      end if
      if (grid_is_latlon) then
         ! Read in lat and lon and save local quantities
         start(1) = (col_start / num_lons) + 1
         kount(1) = (col_end / num_lons) + 1 - start(1) + 1 ! num local lats
         if (is_degrees) then
            allocate(local_lons_deg(lindex))
            allocate(local_lats_deg(num_lons))
            iret = pio_get_var(fh_ini, lat_vardesc, start, kount,             &
                 local_lats_deg)
            call cam_pio_handle_error(iret,                                   &
                 subname//': Unable to read latitude')
            ! Longitudes might cycle so just read them all in
            start(1) = 1
            kount(1) = num_lons
            iret = pio_get_var(fh_ini, lon_vardesc, start, kount,             &
                 local_lons_deg)
            call cam_pio_handle_error(iret,                                   &
                 subname//': Unable to read longitude')
         else
            call endrun(subname//': bad units error trap')
         end if
      else
         ! Do parallel read of lat and lon
         if (is_degrees) then
            allocate(local_lats_deg(num_local_columns))
            allocate(local_lons_deg(num_local_columns))
            allocate(ldof(num_local_columns))
            ldof = 0_iMap
            do lindex = 1, num_local_columns
               ldof(lindex) = col_start + lindex - 1
            end do
            allocate(iodesc)
            call cam_pio_newdecomp(iodesc, (/ num_global_columns /), ldof,    &
                 PIO_DOUBLE)
            call pio_read_darray(fh_ini, lat_vardesc, iodesc, local_lats_deg, &
                 iret)
            call cam_pio_handle_error(iret,                                   &
                 subname//': Unable to read latitude')
            call pio_read_darray(fh_ini, lon_vardesc, iodesc, local_lons_deg, &
                 iret)
            call cam_pio_handle_error(iret,                                   &
                 subname//': Unable to read longitude')
         else
            call endrun(subname//': bad units error trap')
         end if
      end if
      ! Find the grid area and / or weight terms
      call cam_pio_find_var(fh_ini, (/ 'gw  ', 'area' /), var_name,           &
           vardesc, var_found)
      if (var_found) then
         ! Find the variable dimension info
         call cam_pio_var_info(fh_ini, vardesc, num_var_dims, dimids, dimlens)
         if (num_var_dims /= 1) then
            write(errormsg, '(3a,i0)') subname, "Unhandled number of area ",  &
                 "dimensions, ", num_var_dims
            call endrun(errormsg)
         end if
         if ((num_lats > 1) .and. (dimlens(1) == num_lats)) then
            allocate(local_areas(num_lats))
            start(1) = 1
            kount(1) = num_lats
            iret = pio_get_var(fh_ini, vardesc, start, kount, local_areas)
            call cam_pio_handle_error(iret,                                   &
                 subname//': Unable to read '//trim(var_name))
         else if (dimlens(1) == num_global_columns) then
            allocate(local_areas(num_local_columns))
            call pio_read_darray(fh_ini, vardesc, iodesc, local_areas, iret)
            call cam_pio_handle_error(iret, subname//': Unable to read areas')
         else
            write(errormsg, '(a,3(a,i0))') subname,                           &
                 'Unsupported number of grid areas, ', dimlens(1),            &
                 ', num_lats = ', num_lats, ', num_cols = ', num_global_columns
         end if
      else
         call endrun(subname//'Unable to find grid areas')
      end if
      ! Cleanup
      if (.not. grid_is_latlon) then
         call pio_freedecomp(pio_subsystem, iodesc)
         deallocate(iodesc)
         nullify(iodesc)
      end if
      ! Back to old error handling
      call pio_seterrorhandling(fh_ini, err_handling)

   end subroutine dyn_grid_init

   !===========================================================================

   subroutine get_dyn_grid_info(hdim1_d, hdim2_d, num_lev,                    &
        dycore_name, dyn_columns)
      use shr_const_mod,  only: SHR_CONST_PI
      use cam_abortutils, only: endrun
      use spmd_utils,     only: iam
      ! Dummy arguments
      integer,          intent(out)   :: hdim1_d ! # longitudes or grid size
      integer,          intent(out)   :: hdim2_d ! # latitudes or 1
      integer,          intent(out)   :: num_lev ! # levels
      character(len=*), intent(out)   :: dycore_name
      type(physics_column_t), pointer :: dyn_columns(:) ! Phys col in Dyn decomp
      ! Local variables
      integer                         :: lindex
      integer                         :: gindex
      real(r8),         parameter     :: radtodeg = 180.0_r8 / SHR_CONST_PI
      real(r8),         parameter     :: degtorad = SHR_CONST_PI / 180.0_r8
      character(len=*), parameter     :: subname = 'get_dyn_grid_info'

      if (associated(dyn_columns)) then
         call endrun(subname//': dyn_columns must be unassociated pointer')
      end if
      allocate(dyn_columns(num_local_columns))
      hdim1_d = num_lons
      hdim2_d = num_lats
      num_lev = num_levels
      dycore_name = 'NULL'
      do lindex = 1, num_local_columns
         if (allocated(local_lats_rad)) then
            dyn_columns(lindex)%lat_rad = local_lats_rad(lindex)
            dyn_columns(lindex)%lat_deg = local_lats_rad(lindex) * radtodeg
         else if (allocated(local_lats_deg)) then
            dyn_columns(lindex)%lat_deg = local_lats_deg(lindex)
            dyn_columns(lindex)%lat_rad = local_lats_deg(lindex) * degtorad
         else
            call endrun(subname//': No column latitude info')
         end if
         if (allocated(local_lons_rad)) then
            dyn_columns(lindex)%lon_rad = local_lons_rad(lindex)
            dyn_columns(lindex)%lon_deg = local_lons_rad(lindex) * radtodeg
         else if (allocated(local_lons_deg)) then
            dyn_columns(lindex)%lon_deg = local_lons_deg(lindex)
            dyn_columns(lindex)%lon_rad = local_lons_deg(lindex) * degtorad
         else
            call endrun(subname//': No column longitude info')
         end if
         if (allocated(local_areas)) then
            dyn_columns(lindex)%area = local_areas(lindex)
         else if (allocated(local_weights)) then
            dyn_columns(lindex)%area = local_weights(lindex)
         else
            call endrun(subname//': No column area info')
         end if
         if (allocated(local_weights)) then
            dyn_columns(lindex)%weight = local_weights(lindex)
         else if (allocated(local_areas)) then
            dyn_columns(lindex)%weight = local_areas(lindex)
         else
            call endrun(subname//': No column weight info')
         end if
         ! File decomposition
         gindex = global_col_offset + lindex
         dyn_columns(lindex)%global_col_num = gindex
         dyn_columns(lindex)%coord_indices(1) = MOD(gindex - 1, num_lons) + 1
         dyn_columns(lindex)%coord_indices(2) = gindex / num_lons
         ! Dynamics decomposition
         dyn_columns(lindex)%dyn_task = iam
         dyn_columns(lindex)%local_dyn_block = 1
         dyn_columns(lindex)%global_dyn_block = iam + 1
         !  If there is more than one block lindex, they are in the same order
         !    as in the dynamics block structure
         allocate(dyn_columns(lindex)%dyn_block_index(1))
         dyn_columns(lindex)%dyn_block_index(1) = lindex
      end do

   end subroutine get_dyn_grid_info

   !===========================================================================

   subroutine physgrid_copy_attributes_d(gridname_out, grid_attribute_names)
      ! create list of attributes for the physics grid that should be copied
      ! from the corresponding grid object on the dynamics decomposition

      use cam_grid_support, only: hclen => max_hcoordname_len

      ! Dummy arguments
      character(len=hclen),          intent(out) :: gridname_out
      character(len=hclen), pointer, intent(out) :: grid_attribute_names(:)

      gridname_out = gridname
      allocate(grid_attribute_names(0))

   end subroutine physgrid_copy_attributes_d

   !===========================================================================

   subroutine find_units(file, vardesc, is_degrees, is_lat)
      use pio,            only: pio_inq_att, pio_get_att, PIO_OFFSET_KIND
      use pio,            only: file_desc_t, var_desc_t
      use cam_abortutils, only: endrun

      ! Dummy arguments
      type(file_desc_t), intent(inout) :: file
      type(var_desc_t),  intent(in)    :: vardesc
      logical,           intent(out)   :: is_degrees
      logical,           intent(out)   :: is_lat
      ! Local variables
      integer                      :: xtype
      integer(pio_offset_kind)     :: slen
      integer                      :: ierr
      integer, parameter           :: max_units_len = 32
      character(len=max_units_len) :: units

      ierr = pio_inq_att(file, vardesc, 'units', xtype, slen)
      if (slen > max_units_len) then
         call endrun('find_units: units string too long')
      end if
      ierr = pio_get_att(file, vardesc, 'units', units)
      units(slen+1:) = ' '
      is_degrees = .false.
      is_lat = .false.
      if (units(1:6) /= 'degree') then
         call endrun("find_units: Unsupported coordinate units, "//trim(units))
      end if
      if ( (trim(units) == 'degrees_north')    .or.                           &
           (trim(units) == 'degree_north')     .or.                           &
           (trim(units) == 'degree_N')         .or.                           &
           (trim(units) == 'degrees_N')        .or.                           &
           (trim(units) == 'degreeN')          .or.                           &
           (trim(units) == 'degreesN')) then
         is_degrees = .true.
         is_lat = .true.
      else if ((trim(units) == 'degrees_east') .or.                           &
           (trim(units) == 'degree_east')      .or.                           &
           (trim(units) == 'degree_E')         .or.                           &
           (trim(units) == 'degrees_E')        .or.                           &
           (trim(units) == 'degreeE')          .or.                           &
           (trim(units) == 'degreesE')) then
         is_degrees  = .true.
         is_lat = .false.
    else
      call endrun("find_units: unsupported units: '"//trim(units)//"'")
    end if

   end subroutine find_units

   !===========================================================================

   subroutine find_dimension(file, dim_names, found_name, dim_len)
      use pio,            only: file_desc_t
      use pio,            only: PIO_Inq_DimID, PIO_Inq_dimlen, PIO_NOERR
      use cam_abortutils, only: endrun
      use cam_pio_utils,  only: cam_pio_handle_error

      ! Find a dimension on <file> from the list in <dim_names>
      ! Return the first dimension found in <found_name> along with its
      !     length in <dim_len>
      ! Abort on error
      ! Find levels

      ! Dummy arguments
      type(file_desc_t), intent(inout) :: file
      character(len=*),  intent(in)    :: dim_names(:)
      character(len=*),  intent(out)   :: found_name
      integer,           intent(out)   :: dim_len

      ! Local variables
      integer                          :: index
      integer                          :: ierr
      integer                          :: num_dims
      integer                          :: dimid
      character(len=128)               :: errmsg
      character(len=*), parameter      :: subname = 'find_dimension'

      num_dims = len(dim_names)
      found_name = ''
      do index = 1, num_dims
         ierr = PIO_Inq_DimID(file, trim(dim_names(index)), dimid)
         if (ierr == PIO_NOERR) then
            found_name = dim_names(index)
            ierr = PIO_Inq_dimlen(file, dimid, dim_len)
            write(errmsg, '(5a)') subname, ": Could not read length of ",   &
                 "dimension, '", trim(found_name), "', on initial data file"
            call cam_pio_handle_error(ierr, errmsg)
            exit
         end if
      end do
      if (len_trim(found_name) == '') then
         write(errmsg, '(2a,20("  ",a))') subname,                            &
              ": Did not find any of these dimensions on initial data file:", &
              (trim(dim_names(index)), index=1, len(dim_names))
         call endrun(errmsg)
      end if
   end subroutine find_dimension

end module dyn_grid
