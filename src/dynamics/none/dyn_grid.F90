module dyn_grid

   use shr_kind_mod,        only: r8 => shr_kind_r8
   use shr_sys_mod,         only: shr_sys_flush
   use cam_logfile,         only: iulog
   use cam_logfile,         only: debug_output, DEBUGOUT_DEBUG, DEBUGOUT_NONE
   use spmd_utils,          only: masterproc
   use physics_column_type, only: physics_column_t
   use string_utils,        only: to_str

   implicit none
   private
   save

   public model_grid_init

   ! Private module variables

   type(physics_column_t), allocatable :: dyn_columns(:)

   integer               :: num_levels         = -1
   integer               :: num_global_columns = -1
   integer               :: num_lats           = -1 ! Global
   integer               :: num_lons           = -1 ! Global (or 1 for unstruct)
   integer               :: num_local_columns  = -1
   integer               :: global_col_offset  = -HUGE(1)
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
   private :: find_energy_formula

!==============================================================================
CONTAINS
!==============================================================================

   subroutine model_grid_init()
      use shr_kind_mod,     only: SHR_KIND_CL
      use pio,              only: file_desc_t, var_desc_t, io_desc_t
      use pio,              only: iMap=>PIO_OFFSET_KIND, PIO_DOUBLE
      use pio,              only: PIO_BCAST_ERROR, pio_seterrorhandling
      use pio,              only: pio_get_var, pio_freedecomp
      use pio,              only: pio_read_darray
      use spmd_utils,       only: npes, iam
      use cam_pio_utils,    only: cam_pio_handle_error, cam_pio_find_var
      use cam_pio_utils,    only: cam_pio_var_info, pio_subsystem
      use cam_pio_utils,    only: cam_pio_newdecomp
      use cam_abortutils,   only: endrun
      use cam_logfile,      only: cam_log_multiwrite
      use cam_initfiles,    only: initial_file_get_id
      use vert_coord,       only: vert_coord_init, pver
      use hycoef,           only: hycoef_init, hypi, hypm, nprlev, &
                                  hyam, hybm, hyai, hybi, ps0
      use ref_pres,         only: ref_pres_init
      use physics_grid,     only: phys_grid_init
      use cam_grid_support, only: hclen => max_hcoordname_len

      ! Initializes a dynamics decomposition based on an input data file,
      ! and then initializes the physics decomposition based on the dynamics
      ! grid.

      ! Local variables
      type(file_desc_t), pointer        :: fh_ini
      type(var_desc_t)                  :: lat_vardesc
      type(var_desc_t)                  :: lon_vardesc
      type(var_desc_t)                  :: vardesc
      type(io_desc_t),   pointer        :: iodesc
      integer                           :: err_handling
      logical                           :: var_found
      logical                           :: is_degrees
      logical                           :: is_lat
      integer                           :: num_var_dims
      integer                           :: time_id
      integer                           :: lindex
      integer                           :: dimids(MAX_DIMS)
      integer                           :: dimlens(MAX_DIMS)
      integer                           :: col_mod ! Temp for calculating decomp
      integer                           :: col_start, col_end
      integer                           :: start(1), kount(1)
      integer                           :: iret
      integer(iMap),     allocatable    :: ldof(:) ! For reading coordinates
      real(r8),          allocatable    :: temp_arr(:)
      character(len=128)                :: var_name
      character(len=hclen), allocatable :: grid_attribute_names(:)
      character(len=SHR_KIND_CL)        :: dimnames(MAX_DIMS)
      character(len=8)                  :: lat_dim_name
      character(len=8)                  :: lon_dim_name
      character(len=128)                :: errormsg

      character(len=*),  parameter :: subname = 'model_grid_init'


      nullify(iodesc)

      ! Get file handle for initial file to find coordinates
      fh_ini => initial_file_get_id()

      ! Set vertical coordinate information not provided by namelist:
      call vert_coord_init(1, pver)

      ! Initialize hybrid coordinate arrays
      call hycoef_init(fh_ini, psdry=.true.)

      ! Initialize reference pressures
      call ref_pres_init(hypi, hypm, nprlev)

      ! We will handle errors for this routine
      call pio_seterrorhandling(fh_ini, PIO_BCAST_ERROR, oldmethod=err_handling)

      ! Find the latitude variable and dimension(s)
      call cam_pio_find_var(fh_ini, (/ 'lat     ', 'lat_d   ', 'latitude' /), lat_name,         &
           lat_vardesc, var_found)
      if (var_found) then
         ! Find the variable latitude dimension info
         call cam_pio_var_info(fh_ini, lat_vardesc, num_var_dims, dimids,     &
              dimlens, dimnames=dimnames, unlimDimID=time_id)
         lat_dim_name = dimnames(1)
         if (index(lat_dim_name, 'ncol') > 0) then
            grid_is_latlon = .false. ! Unstructured dycore grid
            num_lats = 1
            num_lons = dimlens(1)
         else if (index(lat_dim_name, 'lat') > 0) then
            grid_is_latlon = .true. ! lat/lon dycore grid
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
      if (masterproc .and. (debug_output > DEBUGOUT_NONE)) then
         call shr_sys_flush(iulog) ! Make sure things line up
         if (grid_is_latlon) then
            write(iulog, *) subname, ': Grid is rectangular (lat / lon)'
         else
            write(iulog, *) subname, ': Grid is unstructured'
         end if
      end if

      ! Find the dynamical core from which snapshot was saved to populate energy formula used
      ! Some information about the grid is needed to determine this.
      call find_energy_formula(fh_ini, grid_is_latlon)

      ! Find the longitude variable and dimension(s)
      call cam_pio_find_var(fh_ini, (/ 'lon      ', 'lon_d    ', 'longitude' /), lon_name,         &
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
      if (masterproc .and. (debug_output > DEBUGOUT_NONE)) then
         if (grid_is_latlon) then
            write(iulog, '(2a,i0,a,i0,a)') subname, ': Grid has ', num_lats,  &
                 ' latitude coordinates, and ', num_lons,                     &
                 ' longitude coordinates'
         end if ! No else, we get a total column count below
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
      if (masterproc .and. (debug_output > DEBUGOUT_NONE)) then
         write(iulog, '(2a,i0,a)') subname, ': Grid has ',                    &
              num_global_columns, ' total columns'
         call shr_sys_flush(iulog)
      end if
      if (debug_output >= DEBUGOUT_DEBUG) then
         ! Expensive, print out decomp info from every task
         call cam_log_multiwrite(subname, ':  PE   # cols  start    end',     &
              '(a,i4,i9,2i7)', (/ num_local_columns, col_start, col_end/))
      end if
      ! Find a 3D variable and get its dimensions
      call cam_pio_find_var(fh_ini, (/ 'U            ',                       &
                                       'state_u      ',                       &
                                       'eastward_wind' /),                    &
                            fieldname, vardesc, var_found)
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
      if (masterproc .and. (debug_output > DEBUGOUT_NONE)) then
         write(iulog, '(2a,i0,a)') subname, ': Grid has ', num_levels, ' levels'
         call shr_sys_flush(iulog)
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
         ! We only have a subset of latitudes, figure out which ones.
         start(1) = ((col_start - 1) / num_lons) + 1
         kount(1) = ((col_end - 1) / num_lons) + 1 - start(1) + 1 ! # local lats
         if (debug_output > 2) then
            ! Expensive, print out lat/lon decomp info from every task
            call cam_log_multiwrite(subname, ':  PE   # lats  start    end',  &
                 '(a,i4,i9,2i7)', (/ kount(1), start(1), start(1)+kount(1)-1/))
         end if
         if (is_degrees) then
            allocate(local_lons_deg(num_lons), stat=iret)
            if (iret /= 0) then
               call endrun(subname//': allocate local_lons_deg(num_lons) failed with stat: '//&
                           to_str(iret))
            end if
            allocate(local_lats_deg(kount(1)), stat=iret)
            if (iret /= 0) then
               call endrun(subname//': allocate local_lats_deg(kount) failed with stat: '//&
                           to_str(iret))
            end if
            allocate(temp_arr(num_lats), stat=iret)
            if (iret /= 0) then
               call endrun(subname//': allocate temp_arr(num_lats) failed with stat: '//&
                           to_str(iret))
            end if
            iret = pio_get_var(fh_ini, lat_vardesc, (/ 1 /), (/ num_lats /),  &
                 temp_arr)
            call cam_pio_handle_error(iret,                                   &
                 subname//': Unable to read latitude')
            lindex = start(1) + kount(1) - 1
            local_lats_deg(1:kount(1)) = temp_arr(start(1):lindex)
            deallocate(temp_arr)
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
            allocate(local_lats_deg(num_local_columns), stat=iret)
            if (iret /= 0) then
               call endrun(subname//': allocate local_lats_deg(num_local_columns) '//&
                           'failed with stat: '//to_str(iret))
            end if
            allocate(local_lons_deg(num_local_columns), stat=iret)
            if (iret /= 0) then
               call endrun(subname//': allocate local_lons_deg(num_local_columns) '//&
                           'failed with stat: '//to_str(iret))
            end if
            allocate(ldof(num_local_columns), stat=iret)
            if (iret /= 0) then
               call endrun(subname//': allocate ldof(num_local_columns) '//&
                           'failed with stat: '//to_str(iret))
            end if
            ldof = 0_iMap
            do lindex = 1, num_local_columns
               ldof(lindex) = col_start + lindex - 1
            end do
            allocate(iodesc, stat=iret)
            if (iret /= 0) then
               call endrun(subname//': allocate iodesc failed with stat: '//&
                           to_str(iret))
            end if
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
      call cam_pio_find_var(fh_ini, (/ 'gw       ', 'area     ', 'cell_area' /), var_name,           &
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
            allocate(local_areas(num_lats), stat=iret)
            if (iret /= 0) then
               call endrun(subname//': allocate local_areas(num_lats) failed with stat: '//&
                           to_str(iret))
            end if
            start(1) = 1
            kount(1) = num_lats
            iret = pio_get_var(fh_ini, vardesc, start, kount, local_areas)
            call cam_pio_handle_error(iret,                                   &
                 subname//': Unable to read '//trim(var_name))
         else if (dimlens(1) == num_global_columns) then
            allocate(local_areas(num_local_columns), stat=iret)
            if (iret /= 0) then
               call endrun(subname//': allocate local_areas(num_local_columns) '//&
                           'failed with stat: '//to_str(iret))
            end if
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

      ! Allocate dyn_columns structure if not already allocated:
      if (.not.allocated(dyn_columns)) then
         allocate(dyn_columns(num_local_columns), stat=iret)
         if (iret /= 0) then
            call endrun(subname//': allocate dyn_columns(num_local_columns) '//&
                        'failed with stat: '//to_str(iret))
         end if
      end if

      ! Set dyn_columns values:
      call set_dyn_col_values()

      ! The null dycore has no grid attributes, so allocate to size zero.
      allocate(grid_attribute_names(0), stat=iret)
      if (iret /= 0) then
         call endrun(subname//': allocate grid_attribute_names(0) failed with stat: '//&
                     to_str(iret))
      end if

      ! Initialize physics grid decomposition:
      call phys_grid_init(num_lons, num_lats, 'NULL', &
                          dyn_columns, gridname, &
                          grid_attribute_names)

      ! Deallocate grid_attirbute_names, as it is no longer needed:
      deallocate(grid_attribute_names)

      ! Deallocate dyn_columns, as it is now stored in the
      ! global phys_columns structure:
      deallocate(dyn_columns)

   end subroutine model_grid_init

   !===========================================================================

   subroutine set_dyn_col_values()

      ! Sets the values stored in the "dyn_columns" structure,
      ! which are the physics columns as they exist on the
      ! dynamics decomposition.

      use shr_const_mod,  only: SHR_CONST_PI
      use cam_abortutils, only: endrun
      use spmd_utils,     only: iam

      ! Local variables:
      integer                         :: lindex
      integer                         :: gindex
      integer                         :: lat_index, lat1
      integer                         :: lon_index
      integer                         :: ierr

      real(r8),         parameter     :: radtodeg = 180.0_r8 / SHR_CONST_PI
      real(r8),         parameter     :: degtorad = SHR_CONST_PI / 180.0_r8
      character(len=*), parameter     :: subname = 'set_dyn_col_values'

      ! Calculate dyn_columns variable values:
      lat1 = global_col_offset / num_lons
      do lindex = 1, num_local_columns
         if (grid_is_latlon) then
            lat_index = ((global_col_offset + lindex - 1) / num_lons) + 1 - lat1
            lon_index = MOD(lindex - 1, num_lons) + 1
         else
            lat_index = lindex
            lon_index = lindex
         end if
         if (allocated(local_lats_rad)) then
            dyn_columns(lindex)%lat_rad = local_lats_rad(lat_index)
            dyn_columns(lindex)%lat_deg = local_lats_rad(lat_index) * radtodeg
         else if (allocated(local_lats_deg)) then
            dyn_columns(lindex)%lat_deg = local_lats_deg(lat_index)
            dyn_columns(lindex)%lat_rad = local_lats_deg(lat_index) * degtorad
         else
            call endrun(subname//': No column latitude info')
         end if
         if (allocated(local_lons_rad)) then
            dyn_columns(lindex)%lon_rad = local_lons_rad(lon_index)
            dyn_columns(lindex)%lon_deg = local_lons_rad(lon_index) * radtodeg
         else if (allocated(local_lons_deg)) then
            dyn_columns(lindex)%lon_deg = local_lons_deg(lon_index)
            dyn_columns(lindex)%lon_rad = local_lons_deg(lon_index) * degtorad
         else
            call endrun(subname//': No column longitude info')
         end if
         if (allocated(local_areas)) then
            dyn_columns(lindex)%area = local_areas(lat_index)
         else if (allocated(local_weights)) then
            dyn_columns(lindex)%area = local_weights(lat_index)
         else
            call endrun(subname//': No column area info')
         end if
         if (allocated(local_weights)) then
            dyn_columns(lindex)%weight = local_weights(lat_index)
         else if (allocated(local_areas)) then
            dyn_columns(lindex)%weight = local_areas(lat_index)
         else
            call endrun(subname//': No column weight info')
         end if
         ! File decomposition
         gindex = global_col_offset + lindex
         dyn_columns(lindex)%global_col_num = gindex
         dyn_columns(lindex)%coord_indices(1) = MOD(gindex - 1, num_lons) + 1
         dyn_columns(lindex)%coord_indices(2) = ((gindex - 1) / num_lons) + 1
         ! Dynamics decomposition
         dyn_columns(lindex)%dyn_task = iam
         dyn_columns(lindex)%local_dyn_block = 1
         dyn_columns(lindex)%global_dyn_block = iam + 1
         !  If there is more than one block lindex, they are in the same order
         !    as in the dynamics block structure
         allocate(dyn_columns(lindex)%dyn_block_index(1), stat=ierr)
         if (ierr /= 0) then
            call endrun(subname//': allocate dyn_columns('//&
                        to_str(lindex)//')%dyn_block_index(1)'//&
                        ' failed with stat: '//to_str(ierr))
         end if

         dyn_columns(lindex)%dyn_block_index(1) = lindex
      end do

   end subroutine set_dyn_col_values

   !===========================================================================

   subroutine find_units(file, vardesc, is_degrees, is_lat)
      use pio,            only: pio_inq_att, pio_get_att, PIO_OFFSET_KIND
      use pio,            only: file_desc_t, var_desc_t
      use cam_abortutils, only: endrun
      use cam_pio_utils,  only: cam_pio_handle_error

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
      call cam_pio_handle_error(ierr, 'find_units: Unable to find units attr')
      if (slen > max_units_len) then
         call endrun('find_units: units string too long')
      end if
      ierr = pio_get_att(file, vardesc, 'units', units)
      call cam_pio_handle_error(ierr, 'find_units: Unable to read units attr')
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
      if (len_trim(found_name) == 0) then
         write(errmsg, '(2a,20("  ",a))') subname,                            &
              ": Did not find any of these dimensions on initial data file:", &
              (trim(dim_names(index)), index=1, len(dim_names))
         call endrun(errmsg)
      end if
   end subroutine find_dimension

   !===========================================================================

   subroutine find_energy_formula(file, grid_is_latlon)
      use pio,                  only: file_desc_t
      use pio,                  only: pio_inq_att, pio_global, PIO_NOERR
      use cam_thermo_formula,   only: energy_formula_physics, energy_formula_dycore
      use cam_thermo_formula,   only: ENERGY_FORMULA_DYCORE_SE, ENERGY_FORMULA_DYCORE_FV, ENERGY_FORMULA_DYCORE_MPAS
      use physics_types,        only: dycore_energy_consistency_adjust
      use phys_vars_init_check, only: mark_as_initialized

      ! Find which dynamical core is used in <file> and set the energy formulation
      ! (also called vc_dycore in CAM)
      !
      ! This functionality is only used to recognize the originating dynamical core
      ! from the snapshot file in order to set the energy formulation when running
      ! with the null dycore. Other dynamical cores set energy_formula_dycore at their
      ! initialization.

      type(file_desc_t), intent(inout) :: file
      logical, intent(in)              :: grid_is_latlon

      ! Local variables
      integer                          :: ierr, xtype
      character(len=*), parameter      :: subname = 'find_energy_formula'

      energy_formula_dycore = -1

      ! Is FV dycore? (has lat lon dimension)
      if(grid_is_latlon) then
         energy_formula_dycore = ENERGY_FORMULA_DYCORE_FV
         dycore_energy_consistency_adjust = .false.
         if(masterproc) then
            write(iulog, *) subname, ': Null dycore will use FV dycore energy formula'
         endif
      else
         ! Is SE dycore?
         ierr = pio_inq_att(file, pio_global, 'ne', xtype)
         if (ierr == PIO_NOERR) then
            ! Has ne property - is SE dycore.
            ! if has fv_nphys then is physics grid (ne..pg..), but the energy formulation is the same.
            energy_formula_dycore = ENERGY_FORMULA_DYCORE_SE
            dycore_energy_consistency_adjust = .true.
            if(masterproc) then
               write(iulog, *) subname, ': Null dycore will use SE dycore energy formula'
            endif
         else
            ! Is unstructured and is MPAS dycore
            ! there are no global attributes to identify MPAS dycore, so this has to do for now.
            energy_formula_dycore = ENERGY_FORMULA_DYCORE_MPAS
            dycore_energy_consistency_adjust = .true.
            if(masterproc) then
               write(iulog, *) subname, ': Null dycore will use MPAS dycore energy formula'
            endif
         endif
      endif

      if(energy_formula_dycore /= -1) then
         call mark_as_initialized("total_energy_formula_for_dycore")
      endif
      call mark_as_initialized("flag_for_dycore_energy_consistency_adjustment")

      ! Mark other energy variables calculated by check_energy_timestep_init
      ! here since it will always run when required
      call mark_as_initialized("specific_heat_of_air_used_in_dycore")
      call mark_as_initialized("vertically_integrated_total_energy_using_physics_energy_formula_at_start_of_physics_timestep")
      call mark_as_initialized("vertically_integrated_total_energy_using_physics_energy_formula")
      call mark_as_initialized("vertically_integrated_total_energy_using_dycore_energy_formula_at_start_of_physics_timestep")
      call mark_as_initialized("vertically_integrated_total_energy_using_dycore_energy_formula")
      call mark_as_initialized("vertically_integrated_total_water_at_start_of_physics_timestep")
      call mark_as_initialized("vertically_integrated_total_water")
      call mark_as_initialized("vertically_integrated_total_energy_at_end_of_physics_timestep")

   end subroutine find_energy_formula

end module dyn_grid
