module tropopause_read_file
  !-------------------------------------------------------------------
  ! Support module for CAM-SIMA tropopause_find to read in
  ! climatological tropopause data.
  !
  ! Remarks: This module is not CCPP-ized, but has been cleaned up
  ! for use within CAM-SIMA, particularly removal of chunk support.
  !-------------------------------------------------------------------

  ! climatological tropopause pressures (pcols,1,ntimes) and monthly day-of-year times (12)
  use physics_types,      only: tropp_p_loc, tropp_days

  implicit none
  private

  public :: tropopause_read_file

contains
  subroutine tropopause_read_file(tropopause_climo_file)
    !------------------------------------------------------------------
    ! ... initialize upper boundary values
    !------------------------------------------------------------------
    use interpolate_data,  only : lininterp_init, lininterp, interp_type, lininterp_finish
    use physics_grid, only : get_rlat_all_p, get_rlon_all_p
    use physics_grid, only : pcols => columns_on_task
    use physconst,    only : pi
    use time_manager, only : get_calday
    use ioFileMod,    only : getfil
    use cam_pio_utils, only: cam_pio_openfile
    use pio,          only : file_desc_t, var_desc_t, pio_inq_dimid, pio_inq_dimlen, &
         pio_inq_varid, pio_get_var, pio_closefile, pio_nowrite

    character(len=256), intent(in) :: tropopause_climo_file  ! absolute path of climatology file

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
    integer, parameter :: dates(12) = (/ 116, 214, 316, 415,  516,  615, &
         716, 816, 915, 1016, 1115, 1216 /)
    type(interp_type) :: lon_wgts, lat_wgts
    real(kind_phys), allocatable :: tropp_p_in(:,:,:)
    real(kind_phys), allocatable :: lat(:)
    real(kind_phys), allocatable :: lon(:)
    real(kind_phys) :: to_lats(pcols), to_lons(pcols)
    real(kind_phys), parameter :: d2r=pi/180._kind_phys, zero=0._kind_phys, twopi=pi*2._kind_phys
    character(len=256) :: locfn

    !-----------------------------------------------------------------------
    !       ... open netcdf file
    !-----------------------------------------------------------------------
    call getfil (tropopause_climo_file, locfn, 0)
    call cam_pio_openfile(pio_id, trim(locfn), PIO_NOWRITE)

    !-----------------------------------------------------------------------
    !       ... get time dimension
    !-----------------------------------------------------------------------
    ierr = pio_inq_dimid( pio_id, 'time', dimid )
    ierr = pio_inq_dimlen( pio_id, dimid, ntimes )
    if( ntimes /= 12 )then
       write(iulog,*) 'tropopause_find_init: number of months = ',ntimes,'; expecting 12'
       call endrun
    end if
    !-----------------------------------------------------------------------
    !       ... get latitudes
    !-----------------------------------------------------------------------
    ierr = pio_inq_dimid( pio_id, 'lat', dimid )
    ierr = pio_inq_dimlen( pio_id, dimid, nlat )
    allocate( lat(nlat), stat=ierr )
    if( ierr /= 0 ) then
       write(iulog,*) 'tropopause_find_init: lat allocation error = ',ierr
       call endrun
    end if
    ierr = pio_inq_varid( pio_id, 'lat', vid )
    ierr = pio_get_var( pio_id, vid, lat )
    lat(:nlat) = lat(:nlat) * d2r
    !-----------------------------------------------------------------------
    !       ... get longitudes
    !-----------------------------------------------------------------------
    ierr = pio_inq_dimid( pio_id, 'lon', dimid )
    ierr = pio_inq_dimlen( pio_id, dimid, nlon )
    allocate( lon(nlon), stat=ierr )
    if( ierr /= 0 ) then
       write(iulog,*) 'tropopause_find_init: lon allocation error = ',ierr
       call endrun
    end if
    ierr = pio_inq_varid( pio_id, 'lon', vid )
    ierr = pio_get_var( pio_id, vid, lon )
    lon(:nlon) = lon(:nlon) * d2r

    !------------------------------------------------------------------
    !  ... allocate arrays
    !------------------------------------------------------------------
    allocate( tropp_p_in(nlon,nlat,ntimes), stat=ierr )
    if( ierr /= 0 ) then
       write(iulog,*) 'tropopause_find_init: tropp_p_in allocation error = ',ierr
       call endrun
    end if
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

    ! tropp_p_loc is allocated with dimensions (pcols, begchunk:endchunk, ntimes) in CAM.
    ! in CAM-SIMA, the chunk dimension is collapsed as it is unused.
    allocate( tropp_p_loc(pcols,ntimes), stat=ierr )

    if( ierr /= 0 ) then
      write(iulog,*) 'tropopause_find_init: tropp_p_loc allocation error = ',ierr
      call endrun
    end if

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

    do n = 1,12
       tropp_days(n) = get_calday( dates(n), 0 )
    end do
    if (masterproc) then
       write(iulog,*) 'tropopause_find_init : tropp_days'
       write(iulog,'(1p,5g15.8)') tropp_days(:)
    endif

  end subroutine tropopause_read_file
end module tropopause_read_file