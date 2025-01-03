module tropopause_climo_read
  !-------------------------------------------------------------------
  ! Support module for CAM-SIMA tropopause_find to read in
  ! climatological tropopause data.
  !
  ! Remarks: This module is not CCPP-ized, but has been cleaned up
  ! for use within CAM-SIMA, particularly removal of chunk support.
  !-------------------------------------------------------------------

  use ccpp_kinds,     only: kind_phys
  use runtime_obj,    only: unset_str
  use shr_kind_mod,   only: shr_kind_cl

  implicit none
  private

  public :: tropopause_climo_readnl
  public :: tropopause_climo_read_file

  ! Private module data
  character(len=shr_kind_cl)           :: tropopause_climo_file = unset_str

!> \section arg_table_tropopause_climo_read  Argument Table
!! \htmlinclude tropopause_climo_read.html
  ! months in year for climatological tropopause pressure data
  integer,         public, parameter   :: tropp_slices = 12

  ! climatological tropopause pressures (ncol,ntimes)
  real(kind_phys), public, allocatable :: tropp_p_loc(:,:)

  ! monthly day-of-year times corresponding to climatological data (12)
  real(kind_phys), public, allocatable :: tropp_days(:)

contains
  ! Read namelist variable tropopause_climo_file.
  ! Containing this within CAM-SIMA instead of within scheme as otherwise the climo filepath
  ! has to be passed from physics -> here then the data from here -> physics.
  subroutine tropopause_climo_readnl(nlfile)
    use shr_nl_mod,      only: find_group_name => shr_nl_find_group_name
    use shr_kind_mod,    only: shr_kind_cm
    use mpi,             only: mpi_character
    use spmd_utils,      only: mpicom
    use cam_logfile,     only: iulog
    use cam_abortutils,  only: endrun
    use spmd_utils,      only: masterproc

    ! filepath for file containing namelist input
    character(len=*), intent(in) :: nlfile

    ! Local variables
    integer                      :: unitn, ierr
    character(len=*), parameter  :: subname = 'tropopause_climo_readnl'
    character(len=shr_kind_cm)   :: errmsg

    namelist /tropopause_nl/ tropopause_climo_file

    errmsg = ''

    if (masterproc) then
       open(newunit=unitn, file=trim(nlfile), status='old')
       call find_group_name(unitn, 'tropopause_nl', status=ierr)
       if (ierr == 0) then
          read(unitn, tropopause_nl, iostat=ierr, iomsg=errmsg)
          if (ierr /= 0) then
             call endrun(subname // ':: ERROR reading namelist:' // errmsg)
          end if
       end if
       close(unitn)
    end if

    ! Broadcast namelist variables
    call mpi_bcast(tropopause_climo_file, len(tropopause_climo_file), mpi_character, 0, mpicom, ierr)

    ! Print out namelist variables
    if (masterproc) then
      write(iulog,*) subname, ' options:'
      write(iulog,*) '  Tropopause climatology file will be read: ', trim(tropopause_climo_file)
    endif

  end subroutine tropopause_climo_readnl

  subroutine tropopause_climo_read_file()
    !------------------------------------------------------------------
    ! ... read tropopause climatology dataset file
    !------------------------------------------------------------------
    use shr_kind_mod,         only: shr_kind_cm
    use cam_logfile,          only: iulog
    use cam_abortutils,       only: endrun, check_allocate
    use spmd_utils,           only: masterproc
    use interpolate_data,     only: lininterp_init, lininterp, interp_type, lininterp_finish
    use physics_grid,         only: get_rlat_all_p, get_rlon_all_p
    use physics_grid,         only: pcols => columns_on_task
    use physconst,            only: pi
    use time_manager,         only: get_calday
    use ioFileMod,            only: cam_get_file
    use cam_pio_utils,        only: cam_pio_openfile
    use pio,                  only: file_desc_t, var_desc_t, pio_inq_dimid, pio_inq_dimlen
    use pio,                  only: pio_inq_varid, pio_get_var, pio_closefile, pio_nowrite
    use phys_vars_init_check, only: mark_as_initialized

    !------------------------------------------------------------------
    ! ... local variables
    !------------------------------------------------------------------
    integer :: i, j, n
    integer :: ierr
    type(file_desc_t) :: pio_id
    integer :: dimid
    type(var_desc_t) :: vid
    integer :: nlon, nlat, ntimes
    integer :: start(3)
    integer :: count(3)

    ! YMD (01-16, 02-14, ...) corresponding to the time slices of the climatological
    ! tropopause dataset. Will be converted to day-of-year and stored in tropp_days.
    integer, parameter :: dates(tropp_slices) = (/ 116, 214, 316, 415, 516, 615, &
         716, 816, 915, 1016, 1115, 1216 /)
    type(interp_type) :: lon_wgts, lat_wgts
    real(kind_phys), allocatable :: tropp_p_in(:,:,:)
    real(kind_phys), allocatable :: lat(:)
    real(kind_phys), allocatable :: lon(:)
    real(kind_phys) :: to_lats(pcols), to_lons(pcols)
    real(kind_phys), parameter :: d2r=pi/180._kind_phys, zero=0._kind_phys, twopi=pi*2._kind_phys
    character(len=shr_kind_cl) :: locfn
    character(len=shr_kind_cl) :: errmsg
    character(len=*), parameter :: subname = "tropopause_climo_read_file"

    errmsg = ''

    !-----------------------------------------------------------------------
    !       ... open netcdf file
    !-----------------------------------------------------------------------
    call cam_get_file (tropopause_climo_file, locfn, allow_fail=.false.)
    call cam_pio_openfile(pio_id, trim(locfn), PIO_NOWRITE)

    !-----------------------------------------------------------------------
    !       ... get time dimension
    !-----------------------------------------------------------------------
    ierr = pio_inq_dimid( pio_id, 'time', dimid )
    ierr = pio_inq_dimlen( pio_id, dimid, ntimes )
    if( ntimes /= tropp_slices ) then
       write(iulog,*) 'tropopause_climo_read_file: number of months = ',ntimes,'; expecting ',tropp_slices
       call endrun('tropopause_climo_read_file: number of months in file incorrect')
    end if
    !-----------------------------------------------------------------------
    !       ... get latitudes
    !-----------------------------------------------------------------------
    ierr = pio_inq_dimid( pio_id, 'lat', dimid )
    ierr = pio_inq_dimlen( pio_id, dimid, nlat )
    allocate( lat(nlat), stat=ierr, errmsg=errmsg )
    call check_allocate(ierr, subname, 'lat(nlat)', &
                        file=__FILE__, line=__LINE__, errmsg=errmsg)

    ierr = pio_inq_varid( pio_id, 'lat', vid )
    ierr = pio_get_var( pio_id, vid, lat )
    lat(:nlat) = lat(:nlat) * d2r
    !-----------------------------------------------------------------------
    !       ... get longitudes
    !-----------------------------------------------------------------------
    ierr = pio_inq_dimid( pio_id, 'lon', dimid )
    ierr = pio_inq_dimlen( pio_id, dimid, nlon )
    allocate( lon(nlon), stat=ierr, errmsg=errmsg )
    call check_allocate(ierr, subname, 'lon(nlon)', &
                        file=__FILE__, line=__LINE__, errmsg=errmsg)

    ierr = pio_inq_varid( pio_id, 'lon', vid )
    ierr = pio_get_var( pio_id, vid, lon )
    lon(:nlon) = lon(:nlon) * d2r

    !------------------------------------------------------------------
    !  ... allocate arrays
    !------------------------------------------------------------------
    allocate( tropp_p_in(nlon,nlat,ntimes), stat=ierr, errmsg=errmsg )
    call check_allocate(ierr, subname, 'tropp_p_in(nlon,nlat,ntimes)', &
                        file=__FILE__, line=__LINE__, errmsg=errmsg)

    !------------------------------------------------------------------
    !  ... read in the tropopause pressure
    !------------------------------------------------------------------
    ierr = pio_inq_varid( pio_id, 'trop_p', vid )
    start = (/ 1, 1, 1 /)
    count = (/ nlon, nlat, ntimes /)
    ierr = pio_get_var( pio_id, vid, start, count, tropp_p_in )

    !------------------------------------------------------------------
    !  ... close the netcdf file
    !------------------------------------------------------------------
    call pio_closefile( pio_id )

    !--------------------------------------------------------------------
    !  ... regrid
    !--------------------------------------------------------------------
    allocate( tropp_p_loc(pcols,ntimes), stat=ierr, errmsg=errmsg )
    call check_allocate(ierr, subname, 'tropp_p_loc(pcols,ntimes)', &
                        file=__FILE__, line=__LINE__, errmsg=errmsg)

    call get_rlat_all_p(pcols, to_lats)
    call get_rlon_all_p(pcols, to_lons)
    call lininterp_init(lon, nlon, to_lons, pcols, 2, lon_wgts, zero, twopi)
    call lininterp_init(lat, nlat, to_lats, pcols, 1, lat_wgts)
    do n=1,ntimes
       call lininterp(tropp_p_in(:,:,n), nlon, nlat, tropp_p_loc(1:pcols,n), pcols, lon_wgts, lat_wgts)
    end do
    call lininterp_finish(lon_wgts)
    call lininterp_finish(lat_wgts)
    deallocate(lon)
    deallocate(lat)
    deallocate(tropp_p_in)

    !--------------------------------------------------------
    ! ... initialize the monthly day of year times
    !--------------------------------------------------------

    allocate( tropp_days(tropp_slices), stat=ierr, errmsg=errmsg )
    call check_allocate(ierr, subname, 'tropp_days(tropp_slices)', &
                        file=__FILE__, line=__LINE__, errmsg=errmsg)

    do n = 1,tropp_slices
       tropp_days(n) = get_calday( dates(n), 0 )
    end do
    if (masterproc) then
       write(iulog,*) 'tropopause_climo_read_file: tropp_days (fractional day-of-year in climatology dataset) ='
       write(iulog,'(1p,5g15.8)') tropp_days(:)
    endif

    !--------------------------------------------------------
    ! Mark variables as initialized so they are not read from initial conditions
    !--------------------------------------------------------
    call mark_as_initialized('tropopause_air_pressure_from_tropopause_climatology_dataset')
    call mark_as_initialized('tropopause_calendar_days_from_tropopause_climatology')

  end subroutine tropopause_climo_read_file
end module tropopause_climo_read
