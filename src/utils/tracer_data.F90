! Module used to read (and interpolate) offline tracer data (sources and mixing ratios)
! Created by: Francis Vitt -- 2 May 2006
! Modified by: Jim Edwards -- 10 March 2009
! Modified by: Cheryl Craig and Chih-Chieh (Jack) Chen  -- February 2010
! Dechunkized and ported to SIMA -- Haipeng Lin, October 2025
!
! Notes on port to SIMA:
! - chunking support has been removed
! - pbuf support is removed, so in_pbuf is no longer supported.
!   the previous behavior of in_pbuf meant that if the data was
!   already in pbuf, the data arrays in tracer_data were not allocated
!   and would directly use the pbuf pointer. Because pbuf is retired in
!   SIMA, this is no longer available. The caller should handle the data
!   flow in and out of physics schemes instead of using the pbuf, by
!   retrieving the data from trfld%data.
! - file removal functionality was removed. If needed, it can be a script
!   that runs alongside job submission.
! - FV dycore features: latitude weighting which requires a structured dycore
!   is untested as no such dycore is available in SIMA. Polar averaging is
!   commented out as only used for FV.
module tracer_data
  use perf_mod,       only: t_startf, t_stopf
  use shr_kind_mod,   only: r8 => shr_kind_r8, shr_kind_cl
  use spmd_utils,     only: masterproc
  use cam_abortutils, only: endrun
  use cam_logfile,    only: iulog

  use vert_coord,     only: pver, pverp
  use physics_grid,   only: pcols => columns_on_task

  use time_manager,   only: get_curr_date, get_step_size
  use time_manager,   only: set_time_float_from_date, set_date_from_time_float

  use runtime_obj,    only: unset_real

  use pio, only: file_desc_t, var_desc_t, &
                 pio_seterrorhandling, pio_internal_error, pio_bcast_error, &
                 pio_char, pio_noerr, &
                 pio_inq_dimid, pio_inq_varid, &
                 pio_def_dim, pio_def_var, &
                 pio_put_att, pio_put_var, &
                 pio_get_var, pio_get_att, pio_nowrite, pio_inq_dimlen, &
                 pio_inq_vardimid, pio_inq_dimlen, pio_closefile, &
                 pio_inquire_variable

  implicit none
  private

  public :: trfld, input3d, input2d, trfile
  public :: trcdata_init
  public :: advance_trcdata
  public :: get_fld_data
  public :: get_fld_ndx
  public :: write_trc_restart
  public :: read_trc_restart
  public :: init_trc_restart
  public :: incr_filename

  type input3d
    real(r8), dimension(:, :), pointer :: data => null() ! ncol, lev
  end type input3d

  type input2d
    real(r8), dimension(:),    pointer :: data => null() ! ncol
  end type input2d

  type trfld
    real(r8), dimension(:, :), pointer :: data => null() ! ncol, lev
    type(input3d), dimension(4)        :: input
    character(len=32)                  :: srcnam
    character(len=32)                  :: fldnam
    character(len=32)                  :: units
    type(var_desc_t)                   :: var_id
    integer :: coords(4) ! LATDIM | LONDIM | LEVDIM | TIMDIM
    integer :: order(4)  ! LATDIM | LONDIM | LEVDIM | TIMDIM
    logical :: srf_fld = .false.
  end type trfld

  type trfile
    type(input2d), dimension(4) :: ps_in
    character(len=shr_kind_cl)  :: pathname = ' '
    character(len=shr_kind_cl)  :: curr_filename = ' '
    character(len=shr_kind_cl)  :: next_filename = ' '
    type(file_desc_t) :: curr_fileid
    type(file_desc_t) :: next_fileid

    type(var_desc_t), pointer :: currfnameid => null() ! pio restart file var id
    type(var_desc_t), pointer :: nextfnameid => null() ! pio restart file var id

    character(len=shr_kind_cl) :: filenames_list = ''
    real(r8) :: datatimem = -unset_real     ! time of prv. values read in
    real(r8) :: datatimep = -unset_real     ! time of nxt. values read in
    real(r8) :: datatimes(4)
    integer  :: interp_recs
    real(r8), pointer, dimension(:) :: curr_data_times => null()
    real(r8), pointer, dimension(:) :: next_data_times => null()
    real(r8) :: offset_time
    integer  :: cyc_ndx_beg
    integer  :: cyc_ndx_end
    integer  :: cyc_yr = 0
    real(r8) :: one_yr = 0
    real(r8) :: curr_mod_time ! model time - calendar day
    real(r8) :: next_mod_time ! model time - calendar day - next time step
    integer  :: nlon = 0
    integer  :: nlat = 0
    integer  :: nlev = 0
    integer  :: nilev = 0
    integer  :: ps_coords(3) ! LATDIM | LONDIM | TIMDIM
    integer  :: ps_order(3)  ! LATDIM | LONDIM | TIMDIM
    real(r8), pointer, dimension(:) :: lons => null()
    real(r8), pointer, dimension(:) :: lats => null()
    real(r8), pointer, dimension(:) :: levs => null()
    real(r8), pointer, dimension(:) :: ilevs => null()
    real(r8), pointer, dimension(:) :: hyam => null()
    real(r8), pointer, dimension(:) :: hybm => null()
    real(r8), pointer, dimension(:) :: hyai => null()
    real(r8), pointer, dimension(:) :: hybi => null()
    real(r8), pointer, dimension(:, :) :: weight_x => null(), weight_y => null()
    integer,  pointer, dimension(:)    :: count_x => null(), count_y => null()
    integer,  pointer, dimension(:, :) :: index_x => null(), index_y => null()

    real(r8), pointer, dimension(:, :) :: weight0_x => null(), weight0_y => null()
    integer,  pointer, dimension(:)    :: count0_x => null(), count0_y => null()
    integer,  pointer, dimension(:, :) :: index0_x => null(), index0_y => null()
    logical :: dist

    real(r8) :: p0
    type(var_desc_t) :: ps_id
    logical :: has_ps = .false.
    logical :: zonal_ave = .false.
    logical :: unstructured = .false.
    logical :: alt_data = .false.
    logical :: geop_alt = .false.
    logical :: cyclical = .false.
    logical :: cyclical_list = .false.
    logical :: weight_by_lat = .false.
    logical :: conserve_column = .false.
    logical :: fill_in_months = .false.
    logical :: fixed = .false.
    logical :: initialized = .false.
    logical :: top_bndry = .false.
    logical :: top_layer = .false.
    logical :: stepTime = .false.  ! Do not interpolate in time, but use stepwise times
  end type trfile

  integer, parameter :: LONDIM = 1
  integer, parameter :: LATDIM = 2
  integer, parameter :: LEVDIM = 3
  integer, parameter :: TIMDIM = 4

  integer, parameter :: PS_TIMDIM = 3

  integer, parameter :: ZA_LATDIM = 1
  integer, parameter :: ZA_LEVDIM = 2
  integer, parameter :: ZA_TIMDIM = 3

  integer, parameter :: nm = 1    ! array index for previous (minus) data
  integer, parameter :: np = 2    ! array index for next (plus) data

  integer :: plon, plat

  integer, allocatable :: lon_global_grid_ndx(:) ! (ncol)
  integer, allocatable :: lat_global_grid_ndx(:) ! (ncol)

contains

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
  subroutine trcdata_init(specifier, filename, filelist, datapath, flds, file, &
                          data_cycle_yr, data_fixed_ymd, data_fixed_tod, data_type)
    use physconst, only: pi
    use string_utils, only: to_lower
    use cam_abortutils, only: check_allocate

    ! For latitude weighting functionality
    !use dyn_grid, only: get_horiz_grid_int
    !use physics_grid, only: get_rlat_all_p, get_rlon_all_p
    !use spmd_utils, only: mpicom, masterprocid, mpi_real8, mpi_integer
    !use horizontal_interpolate, only: xy_interp_init
    use physics_grid, only: dycore_unstructured
    use physics_grid, only: plon => hdim1_d, plat => hdim2_d

    character(len=*), intent(in)            :: specifier(:)
    character(len=*), intent(in)            :: filename
    character(len=*), intent(in)            :: filelist
    character(len=*), intent(in)            :: datapath
    type(trfld),      dimension(:), pointer :: flds
    type(trfile),     intent(inout)         :: file
    integer,          intent(in)            :: data_cycle_yr
    integer,          intent(in)            :: data_fixed_ymd
    integer,          intent(in)            :: data_fixed_tod
    character(len=*), intent(in)            :: data_type

    character(len=*), parameter :: sub = 'trcdata_init'
    real(r8),         parameter :: d2r = pi/180._r8

    integer :: f, mxnflds, astat
    integer :: str_yr, str_mon, str_day
    integer :: lon_dimid, lat_dimid, lev_dimid, tim_dimid, old_dimid
    integer :: dimids(4), did
    type(var_desc_t) :: varid
    integer :: idx
    integer :: ierr
    integer :: errcode
    real(r8) :: start_time, time1, time2
    integer :: i1, i2, j1, j2
    integer :: nvardims, vardimids(4)

    character(len=256) :: data_units
    real(r8), allocatable :: lam(:), phi(:)
    real(r8):: rlats(pcols), rlons(pcols)
    integer :: ncol, icol, i, j
    logical :: found
    integer :: aircraft_cnt
    integer :: err_handling
    character(len=512) :: errmsg

    call specify_fields(specifier, flds)

    file%datatimep = -unset_real
    file%datatimem = -unset_real

    mxnflds = 0
    if (associated(flds)) mxnflds = size(flds)

    if (mxnflds < 1) return

    file%pathname = trim(datapath)
    file%filenames_list = trim(filelist)

    file%fill_in_months = .false.
    file%cyclical = .false.
    file%cyclical_list = .false.

    select case (data_type)
    case ('FIXED')
      file%fixed = .true.
    case ('INTERP_MISSING_MONTHS')
      file%fill_in_months = .true.
    case ('CYCLICAL')
      file%cyclical = .true.
      file%cyc_yr = data_cycle_yr
    case ('CYCLICAL_LIST')
      file%cyclical_list = .true.
      file%cyc_yr = data_cycle_yr
    case ('SERIAL')
      ! nothing needs to be done here.
    case default
      write (iulog, *) 'trcdata_init: invalid data type: '//trim(data_type)//' file: '//trim(filename)
      write (iulog, *) 'trcdata_init: valid data types: SERIAL | CYCLICAL | CYCLICAL_LIST | FIXED | INTERP_MISSING_MONTHS '
      call endrun('trcdata_init: invalid data type: '//trim(data_type)//' file: '//trim(filename))
    end select

    if ((.not. file%fixed) .and. ((data_fixed_ymd > 0._r8) .or. (data_fixed_tod > 0._r8))) then
      call endrun('trcdata_init: Cannot specify data_fixed_ymd or data_fixed_tod if data type is not FIXED')
    end if
    if ((.not. file%cyclical) .and. (data_cycle_yr > 0._r8)) then
      call endrun('trcdata_init: Cannot specify data_cycle_yr if data type is not CYCLICAL')
    end if

    if (file%top_bndry .and. file%top_layer) then
      call endrun('trcdata_init: Cannot set both file%top_bndry and file%top_layer to TRUE.')
    end if

    if (masterproc) then
      write (iulog, *) 'trcdata_init: data type: '//trim(data_type)//' file: '//trim(filename)
    end if

    ! if there is no list of files (len_trim(file%filenames_list)<1) then
    !  -> set curr_filename from namelist rather than restart data
    if (len_trim(file%curr_filename) < 1 .or. len_trim(file%filenames_list) < 1 .or. file%fixed) then ! initial run
      file%curr_filename = trim(filename)

      call get_model_time(file)

      if (file%fixed) then
        str_yr = data_fixed_ymd/10000
        str_mon = (data_fixed_ymd - str_yr*10000)/100
        str_day = data_fixed_ymd - str_yr*10000 - str_mon*100
        call set_time_float_from_date(start_time, str_yr, str_mon, str_day, data_fixed_tod)
        file%offset_time = start_time - file%curr_mod_time
      else
        file%offset_time = 0
      end if
    end if

    call set_time_float_from_date(time2, 2, 1, 1, 0)
    call set_time_float_from_date(time1, 1, 1, 1, 0)
    file%one_yr = time2 - time1

    if (file%cyclical .or. file%cyclical_list) then
      file%cyc_ndx_beg = -1
      file%cyc_ndx_end = -1
      if (file%cyc_yr /= 0) then
        call set_time_float_from_date(time1, file%cyc_yr, 1, 1, 0)
        call set_time_float_from_date(time2, file%cyc_yr + 1, 1, 1, 0)
        file%one_yr = time2 - time1
      end if

      if (file%cyclical) then
        call open_trc_datafile(file%curr_filename, file%pathname, file%curr_fileid, file%curr_data_times, &
                               cyc_ndx_beg=file%cyc_ndx_beg, cyc_ndx_end=file%cyc_ndx_end, cyc_yr=file%cyc_yr)
      else
        call open_trc_datafile(file%curr_filename, file%pathname, file%curr_fileid, file%curr_data_times)
      end if
    else
      call open_trc_datafile(file%curr_filename, file%pathname, file%curr_fileid, file%curr_data_times)
      file%curr_data_times = file%curr_data_times - file%offset_time
    end if

    call pio_seterrorhandling(File%curr_fileid, PIO_BCAST_ERROR, oldmethod=err_handling)
    ierr = pio_inq_dimid(file%curr_fileid, 'ncol', idx)
    file%unstructured = (ierr == PIO_NOERR)
    if (.not. file%unstructured) then
      ierr = pio_inq_dimid(file%curr_fileid, 'lon', idx)
      file%zonal_ave = (ierr /= PIO_NOERR)
    end if
    call pio_seterrorhandling(File%curr_fileid, err_handling)

    if (file%zonal_ave) then
      file%nlon = 1
    else
      if (.not. file%unstructured) then
        call get_dimension(file%curr_fileid, 'lon', file%nlon, dimid=old_dimid, data=file%lons)

        file%lons = file%lons*d2r

        lon_dimid = old_dimid
      end if
    end if

    ierr = pio_inq_dimid(file%curr_fileid, 'time', old_dimid)

    if (.not. file%unstructured) then
      ! Hack to work with weird netCDF and old gcc or NAG bug.
      tim_dimid = old_dimid

      call get_dimension(file%curr_fileid, 'lat', file%nlat, dimid=old_dimid, data=file%lats)
      file%lats = file%lats*d2r

      lat_dimid = old_dimid
    end if

    call pio_seterrorhandling(File%curr_fileid, PIO_BCAST_ERROR, oldmethod=err_handling)
    ierr = pio_inq_varid(file%curr_fileid, 'PS', file%ps_id)
    file%has_ps = (ierr == PIO_NOERR)
    ierr = pio_inq_dimid(file%curr_fileid, 'altitude', idx)
    file%alt_data = (ierr == PIO_NOERR)

    call pio_seterrorhandling(File%curr_fileid, err_handling)

    if (file%has_ps .and. .not. file%unstructured) then
      if (file%zonal_ave) then
        ierr = pio_inq_vardimid(file%curr_fileid, file%ps_id, dimids(1:2))
        do did = 1, 2
          if (dimids(did) == lat_dimid) then
            file%ps_coords(LATDIM) = did
            file%ps_order(did) = LATDIM
          else if (dimids(did) == tim_dimid) then
            file%ps_coords(PS_TIMDIM) = did
            file%ps_order(did) = PS_TIMDIM
          end if
        end do
      else
        ierr = pio_inq_vardimid(file%curr_fileid, file%ps_id, dimids(1:3))
        do did = 1, 3
          if (dimids(did) == lon_dimid) then
            file%ps_coords(LONDIM) = did
            file%ps_order(did) = LONDIM
          else if (dimids(did) == lat_dimid) then
            file%ps_coords(LATDIM) = did
            file%ps_order(did) = LATDIM
          else if (dimids(did) == tim_dimid) then
            file%ps_coords(PS_TIMDIM) = did
            file%ps_order(did) = PS_TIMDIM
          end if
        end do
      end if
    end if

    if (masterproc) then
      write (iulog, *) 'trcdata_init: file%has_ps = ', file%has_ps
    end if ! masterproc

    if (file%alt_data) then
      call get_dimension(file%curr_fileid, 'altitude_int', file%nilev, data=file%ilevs)
      call get_dimension(file%curr_fileid, 'altitude', file%nlev, dimid=old_dimid, data=file%levs)
    else
      call get_dimension(file%curr_fileid, 'lev', file%nlev, dimid=old_dimid, data=file%levs)
      if (old_dimid > 0) then
        file%levs = file%levs*100._r8 ! mbar->pascals
      end if
    end if

    ! For some bizarre reason, netCDF with older gcc is keeping a pointer to the dimid, and overwriting it later!
    ! Hackish workaround is to make a copy...
    lev_dimid = old_dimid

    if (file%has_ps) then

      allocate (file%hyam(file%nlev), file%hybm(file%nlev), stat=astat, errmsg=errmsg)
      call check_allocate(astat, sub, 'file%hyam(file%nlev), file%hybm(file%nlev)',                   &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)

      allocate (file%hyai(file%nlev + 1), file%hybi(file%nlev + 1), stat=astat, errmsg=errmsg)
      call check_allocate(astat, sub, 'file%hyai(file%nlev + 1), file%hybi(file%nlev + 1)',                   &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)

      call pio_seterrorhandling(File%curr_fileid, PIO_BCAST_ERROR, oldmethod=err_handling)
      ierr = pio_inq_varid(file%curr_fileid, 'P0', varid)
      call pio_seterrorhandling(File%curr_fileid, err_handling)

      if (ierr == PIO_NOERR) then
        ierr = pio_get_var(file%curr_fileid, varid, file%p0)
      else
        file%p0 = 100000._r8
      end if
      ierr = pio_inq_varid(file%curr_fileid, 'hyam', varid)
      ierr = pio_get_var(file%curr_fileid, varid, file%hyam)
      ierr = pio_inq_varid(file%curr_fileid, 'hybm', varid)
      ierr = pio_get_var(file%curr_fileid, varid, file%hybm)
      if (file%conserve_column) then
        ierr = pio_inq_varid(file%curr_fileid, 'hyai', varid)
        ierr = pio_get_var(file%curr_fileid, varid, file%hyai)
        ierr = pio_inq_varid(file%curr_fileid, 'hybi', varid)
        ierr = pio_get_var(file%curr_fileid, varid, file%hybi)
      end if

      allocate (file%ps_in(1)%data(pcols), stat=astat, errmsg=errmsg)
      call check_allocate(astat, sub, 'file%ps_in(1)%data(pcols)',                   &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)

      allocate (file%ps_in(2)%data(pcols), stat=astat, errmsg=errmsg)
      call check_allocate(astat, sub, 'file%ps_in(2)%data(pcols)',                   &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)

      if (file%fill_in_months) then
        allocate (file%ps_in(3)%data(pcols), stat=astat, errmsg=errmsg)
        call check_allocate(astat, sub, 'file%ps_in(3)%data(pcols)',                   &
                            file=__FILE__, line=__LINE__, errmsg=errmsg)

        allocate (file%ps_in(4)%data(pcols), stat=astat, errmsg=errmsg)
        call check_allocate(astat, sub, 'file%ps_in(4)%data(pcols)',                   &
                            file=__FILE__, line=__LINE__, errmsg=errmsg)
      end if
    end if

    call pio_seterrorhandling(File%curr_fileid, PIO_BCAST_ERROR, oldmethod=err_handling)

    flds_loop: do f = 1, mxnflds

      ! get netcdf variable id for the field
      ierr = pio_inq_varid(file%curr_fileid, flds(f)%srcnam, flds(f)%var_id)
      if (ierr /= pio_noerr) then
        call endrun('trcdata_init: Cannot find var "'//trim(flds(f)%srcnam)// &
                    '" in file "'//trim(file%curr_filename)//'"')
      end if

      ! determine if the field has a vertical dimension

      if (lev_dimid > 0) then
        ierr = pio_inquire_variable(file%curr_fileid, flds(f)%var_id, ndims=nvardims)
        ierr = pio_inquire_variable(file%curr_fileid, flds(f)%var_id, dimids=vardimids(:nvardims))
        flds(f)%srf_fld = .not. any(vardimids(:nvardims) == lev_dimid)
      else
        flds(f)%srf_fld = .true.
      end if

      ! allocate memory for data in container.
      if (flds(f)%srf_fld .or. file%top_bndry .or. file%top_layer) then
        ! surface/top boundary/top layer field.
        allocate (flds(f)%data(pcols, 1), stat=astat, errmsg=errmsg)
      else
        allocate (flds(f)%data(pcols, pver), stat=astat, errmsg=errmsg)
      end if
      call check_allocate(astat, sub, 'flds(f)%data(pcols, nlev)',                   &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)

      if (flds(f)%srf_fld) then
        allocate (flds(f)%input(1)%data(pcols, 1), stat=astat, errmsg=errmsg)
      else
        allocate (flds(f)%input(1)%data(pcols, file%nlev), stat=astat, errmsg=errmsg)
      end if
      call check_allocate(astat, sub, 'flds(f)%input(1)%data(pcols, file%nlev)',                   &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)

      if (flds(f)%srf_fld) then
        allocate (flds(f)%input(2)%data(pcols, 1), stat=astat, errmsg=errmsg)
      else
        allocate (flds(f)%input(2)%data(pcols, file%nlev), stat=astat, errmsg=errmsg)
      end if
      call check_allocate(astat, sub, 'flds(f)%input(2)%data(pcols, file%nlev)',                   &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)

      if (file%fill_in_months) then
        if (flds(f)%srf_fld) then
          allocate (flds(f)%input(3)%data(pcols, 1), stat=astat, errmsg=errmsg)
        else
          allocate (flds(f)%input(3)%data(pcols, file%nlev), stat=astat, errmsg=errmsg)
        end if
        call check_allocate(astat, sub, 'flds(f)%input(3)%data(pcols, file%nlev)',                   &
                            file=__FILE__, line=__LINE__, errmsg=errmsg)

        if (flds(f)%srf_fld) then
          allocate (flds(f)%input(4)%data(pcols, 1), stat=astat, errmsg=errmsg)
        else
          allocate (flds(f)%input(4)%data(pcols, file%nlev), stat=astat, errmsg=errmsg)
        end if
        call check_allocate(astat, sub, 'flds(f)%input(4)%data(pcols, file%nlev)',                   &
                            file=__FILE__, line=__LINE__, errmsg=errmsg)
      end if

      if (file%zonal_ave) then
        ierr = pio_inq_vardimid(file%curr_fileid, flds(f)%var_id, dimids(1:3))
        do did = 1, 3
          if (dimids(did) == lat_dimid) then
            flds(f)%coords(ZA_LATDIM) = did
            flds(f)%order(did) = ZA_LATDIM
          else if (dimids(did) == lev_dimid) then
            flds(f)%coords(ZA_LEVDIM) = did
            flds(f)%order(did) = ZA_LEVDIM
          else if (dimids(did) == tim_dimid) then
            flds(f)%coords(ZA_TIMDIM) = did
            flds(f)%order(did) = ZA_TIMDIM
          end if
        end do
      else if (flds(f)%srf_fld .and. .not. file%unstructured) then
        ierr = pio_inq_vardimid(file%curr_fileid, flds(f)%var_id, dimids(1:3))
        do did = 1, 3
          if (dimids(did) == lon_dimid) then
            flds(f)%coords(LONDIM) = did
            flds(f)%order(did) = LONDIM
          else if (dimids(did) == lat_dimid) then
            flds(f)%coords(LATDIM) = did
            flds(f)%order(did) = LATDIM
          else if (dimids(did) == tim_dimid) then
            flds(f)%coords(PS_TIMDIM) = did
            flds(f)%order(did) = PS_TIMDIM
          end if
        end do
      else if (.not. file%unstructured) then
        ierr = pio_inq_vardimid(file%curr_fileid, flds(f)%var_id, dimids)
        do did = 1, 4
          if (dimids(did) == lon_dimid) then
            flds(f)%coords(LONDIM) = did
            flds(f)%order(did) = LONDIM
          else if (dimids(did) == lat_dimid) then
            flds(f)%coords(LATDIM) = did
            flds(f)%order(did) = LATDIM
          else if (dimids(did) == lev_dimid) then
            flds(f)%coords(LEVDIM) = did
            flds(f)%order(did) = LEVDIM
          else if (dimids(did) == tim_dimid) then
            flds(f)%coords(TIMDIM) = did
            flds(f)%order(did) = TIMDIM
          end if
        end do
      end if

      ! retrieve units from file (used by downstream code to potentially
      ! perform unit conversions on read data)
      !
      ! convert units to lowercase to facilitate comparisons
      ierr = pio_get_att(file%curr_fileid, flds(f)%var_id, 'units', data_units)
      flds(f)%units = trim(to_lower(data_units(1:32)))

    end do flds_loop

    call pio_seterrorhandling(File%curr_fileid, err_handling)

    ! if weighting by latitude, compute weighting for horizontal interpolation
    if (file%weight_by_lat) then
      if (dycore_unstructured) then
        call endrun('trcdata_init: weighting by latitude not implemented for unstructured grids')
      end if

      call endrun('trcdata_init: weighting by latitude (used by aircraft emis) is untested in SIMA; uncomment this line for testing.')
      ! WARNING: in SIMA, currently implemented dycores are unstructured.
      ! The below code has been ported to the best of ability,
      ! but is completely untested. (hplin, 10/9/25)

      ! allocate (lam(plon), phi(plat))
      ! call get_horiz_grid_int(plat, clat_d_out=phi)
      ! call get_horiz_grid_int(plon, clon_d_out=lam)

      ! if (.not. allocated(lon_global_grid_ndx)) allocate (lon_global_grid_ndx(pcols))
      ! if (.not. allocated(lat_global_grid_ndx)) allocate (lat_global_grid_ndx(pcols))
      ! lon_global_grid_ndx = -huge(1)
      ! lat_global_grid_ndx = -huge(1)

      ! ncol = pcols ! active columns
      ! call get_rlat_all_p(ncol, rlats(:ncol))
      ! call get_rlon_all_p(ncol, rlons(:ncol))
      ! do icol = 1, ncol
      !   found = .false.
      !   find_col: do j = 1, plat
      !     do i = 1, plon
      !       if (rlats(icol) == phi(j) .and. rlons(icol) == lam(i)) then
      !         found = .true.
      !         exit find_col
      !       end if
      !     end do
      !   end do find_col

      !   if (.not. found) call endrun('trcdata_init: not able to find physics column coordinate')
      !   lon_global_grid_ndx(icol) = i
      !   lat_global_grid_ndx(icol) = j
      ! end do

      ! deallocate (phi, lam)

      ! ! weight_x & weight_y are weighting function for x & y interpolation
      ! allocate (file%weight_x(plon, file%nlon), stat=astat)
      ! if (astat /= 0) then
      !   write (iulog, *) 'trcdata_init: file%weight_x allocation error = ', astat
      !   call endrun('trcdata_init: failed to allocate weight_x array')
      ! end if
      ! allocate (file%weight_y(plat, file%nlat), stat=astat)
      ! if (astat /= 0) then
      !   write (iulog, *) 'trcdata_init: file%weight_y allocation error = ', astat
      !   call endrun('trcdata_init: failed to allocate weight_y array')
      ! end if
      ! allocate (file%count_x(plon), stat=astat)
      ! if (astat /= 0) then
      !   write (iulog, *) 'trcdata_init: file%count_x allocation error = ', astat
      !   call endrun('trcdata_init: failed to allocate count_x array')
      ! end if
      ! allocate (file%count_y(plat), stat=astat)
      ! if (astat /= 0) then
      !   write (iulog, *) 'trcdata_init: file%count_y allocation error = ', astat
      !   call endrun('trcdata_init: failed to allocate count_y array')
      ! end if
      ! allocate (file%index_x(plon, file%nlon), stat=astat)
      ! if (astat /= 0) then
      !   write (iulog, *) 'trcdata_init: file%index_x allocation error = ', astat
      !   call endrun('trcdata_init: failed to allocate index_x array')
      ! end if
      ! allocate (file%index_y(plat, file%nlat), stat=astat)
      ! if (astat /= 0) then
      !   write (iulog, *) 'trcdata_init: file%index_y allocation error = ', astat
      !   call endrun('trcdata_init: failed to allocate index_y array')
      ! end if
      ! file%weight_x(:, :) = 0.0_r8
      ! file%weight_y(:, :) = 0.0_r8
      ! file%count_x(:) = 0
      ! file%count_y(:) = 0
      ! file%index_x(:, :) = 0
      ! file%index_y(:, :) = 0

      ! if (file%dist) then
      !   allocate (file%weight0_x(plon, file%nlon), stat=astat)
      !   if (astat /= 0) then
      !     write (iulog, *) 'trcdata_init: file%weight0_x allocation error = ', astat
      !     call endrun('trcdata_init: failed to allocate weight0_x array')
      !   end if
      !   allocate (file%weight0_y(plat, file%nlat), stat=astat)
      !   if (astat /= 0) then
      !     write (iulog, *) 'trcdata_init: file%weight0_y allocation error = ', astat
      !     call endrun('trcdata_init: failed to allocate weight0_y array')
      !   end if
      !   allocate (file%count0_x(plon), stat=astat)
      !   if (astat /= 0) then
      !     write (iulog, *) 'trcdata_init: file%count0_x allocation error = ', astat
      !     call endrun('trcdata_init: failed to allocate count0_x array')
      !   end if
      !   allocate (file%count0_y(plat), stat=astat)
      !   if (astat /= 0) then
      !     write (iulog, *) 'trcdata_init: file%count0_y allocation error = ', astat
      !     call endrun('trcdata_init: failed to allocate count0_y array')
      !   end if
      !   allocate (file%index0_x(plon, file%nlon), stat=astat)
      !   if (astat /= 0) then
      !     write (iulog, *) 'trcdata_init: file%index0_x allocation error = ', astat
      !     call endrun('trcdata_init: failed to allocate index0_x array')
      !   end if
      !   allocate (file%index0_y(plat, file%nlat), stat=astat)
      !   if (astat /= 0) then
      !     write (iulog, *) 'trcdata_init: file%index0_y allocation error = ', astat
      !     call endrun('trcdata_init: failed to allocate index0_y array')
      !   end if
      !   file%weight0_x(:, :) = 0.0_r8
      !   file%weight0_y(:, :) = 0.0_r8
      !   file%count0_x(:) = 0
      !   file%count0_y(:) = 0
      !   file%index0_x(:, :) = 0
      !   file%index0_y(:, :) = 0
      ! end if

      ! if (masterproc) then
      !   ! compute weighting.  NOTE: we always set
      !   ! use_flight_distance=.false. for this path since these
      !   ! weights are used to inerpolate field values like PS even
      !   ! when the file contains other data which should be treated
      !   ! as per-cell totals.
      !   call xy_interp_init(file%nlon, file%nlat, file%lons, file%lats, &
      !                       plon, plat, file%weight_x, file%weight_y, .false.)

      !   do i2 = 1, plon
      !     file%count_x(i2) = 0
      !     do i1 = 1, file%nlon
      !       if (file%weight_x(i2, i1) > 0.0_r8) then
      !         file%count_x(i2) = file%count_x(i2) + 1
      !         file%index_x(i2, file%count_x(i2)) = i1
      !       end if
      !     end do
      !   end do

      !   do j2 = 1, plat
      !     file%count_y(j2) = 0
      !     do j1 = 1, file%nlat
      !       if (file%weight_y(j2, j1) > 0.0_r8) then
      !         file%count_y(j2) = file%count_y(j2) + 1
      !         file%index_y(j2, file%count_y(j2)) = j1
      !       end if
      !     end do
      !   end do

      !   if (file%dist) then
      !     call xy_interp_init(file%nlon, file%nlat, file%lons, file%lats, &
      !                         plon, plat, file%weight0_x, file%weight0_y, .true.)

      !     do i2 = 1, plon
      !       file%count0_x(i2) = 0
      !       do i1 = 1, file%nlon
      !         if (file%weight0_x(i2, i1) > 0.0_r8) then
      !           file%count0_x(i2) = file%count0_x(i2) + 1
      !           file%index0_x(i2, file%count0_x(i2)) = i1
      !         end if
      !       end do
      !     end do

      !     do j2 = 1, plat
      !       file%count0_y(j2) = 0
      !       do j1 = 1, file%nlat
      !         if (file%weight0_y(j2, j1) > 0.0_r8) then
      !           file%count0_y(j2) = file%count0_y(j2) + 1
      !           file%index0_y(j2, file%count0_y(j2)) = j1
      !         end if
      !       end do
      !     end do

      !   end if
      ! end if

      ! call mpi_bcast(file%weight_x, plon*file%nlon, mpi_real8, masterprocid, mpicom, ierr)
      ! if (ierr /= 0) call endrun(sub//": FATAL: mpi_bcast: file%weight_x")
      ! call mpi_bcast(file%weight_y, plat*file%nlat, mpi_real8, masterprocid, mpicom, ierr)
      ! if (ierr /= 0) call endrun(sub//": FATAL: mpi_bcast: file%weight_y")
      ! call mpi_bcast(file%count_x, plon, mpi_integer, masterprocid, mpicom, ierr)
      ! if (ierr /= 0) call endrun(sub//": FATAL: mpi_bcast: file%count_x")
      ! call mpi_bcast(file%count_y, plat, mpi_integer, masterprocid, mpicom, ierr)
      ! if (ierr /= 0) call endrun(sub//": FATAL: mpi_bcast: file%count_y")
      ! call mpi_bcast(file%index_x, plon*file%nlon, mpi_integer, masterprocid, mpicom, ierr)
      ! if (ierr /= 0) call endrun(sub//": FATAL: mpi_bcast: file%index_x")
      ! call mpi_bcast(file%index_y, plat*file%nlat, mpi_integer, masterprocid, mpicom, ierr)
      ! if (ierr /= 0) call endrun(sub//": FATAL: mpi_bcast: file%index_y")
      ! if (file%dist) then
      !   call mpi_bcast(file%weight0_x, plon*file%nlon, mpi_real8, masterprocid, mpicom, ierr)
      !   if (ierr /= 0) call endrun(sub//": FATAL: mpi_bcast: file%weight0_x")
      !   call mpi_bcast(file%weight0_y, plat*file%nlat, mpi_real8, masterprocid, mpicom, ierr)
      !   if (ierr /= 0) call endrun(sub//": FATAL: mpi_bcast: file%weight0_y")
      !   call mpi_bcast(file%count0_x, plon, mpi_integer, masterprocid, mpicom, ierr)
      !   if (ierr /= 0) call endrun(sub//": FATAL: mpi_bcast: file%count0_x")
      !   call mpi_bcast(file%count0_y, plat, mpi_integer, masterprocid, mpicom, ierr)
      !   if (ierr /= 0) call endrun(sub//": FATAL: mpi_bcast: file%count0_y")
      !   call mpi_bcast(file%index0_x, plon*file%nlon, mpi_integer, masterprocid, mpicom, ierr)
      !   if (ierr /= 0) call endrun(sub//": FATAL: mpi_bcast: file%index0_x")
      !   call mpi_bcast(file%index0_y, plat*file%nlat, mpi_integer, masterprocid, mpicom, ierr)
      !   if (ierr /= 0) call endrun(sub//": FATAL: mpi_bcast: file%index0_y")
      ! end if

    end if
  end subroutine trcdata_init

  !-----------------------------------------------------------------------
  ! Reads more data if needed and interpolates data to current model time
  !-----------------------------------------------------------------------
  subroutine advance_trcdata( &
             flds, file, &
             pmid, pint, phis, zi)

    use ccpp_kinds,   only: kind_phys

    ! dimensions of the grid can be retrieved directly
    use vert_coord,   only: pver, pverp
    use physics_grid, only: ncol => columns_on_task

    type(trfile), intent(inout) :: file
    type(trfld),  intent(inout) :: flds(:)

    ! state inputs for interpolating to model grid.
    real(kind_phys), intent(in) :: pmid(:,:) ! air pressure [Pa]
    real(kind_phys), intent(in) :: pint(:,:) ! air pressure at interfaces [Pa]
    real(kind_phys), intent(in) :: phis(:)   ! surface geopotential [m2 s-2]
    real(kind_phys), intent(in) :: zi(:,:)   ! height above surface, interfaces [m]

    real(r8) :: data_time

    call t_startf('advance_trcdata')
    if (.not. (file%fixed .and. file%initialized)) then

      call get_model_time(file)

      data_time = file%datatimep

      if (file%cyclical .or. file%cyclical_list) then
        ! wrap around
        if ((file%datatimep < file%datatimem) .and. (file%curr_mod_time > file%datatimem)) then
          data_time = data_time + file%one_yr
        end if
      end if

      ! For stepTime need to advance if the times are equal
      ! Should not impact other runs?
      if (file%curr_mod_time >= data_time) then
        call t_startf('read_next_trcdata')
        call read_next_trcdata(flds, file)
        call t_stopf('read_next_trcdata')
        if (masterproc) write (iulog, *) 'READ_NEXT_TRCDATA: ', flds%fldnam
      end if

    end if

    ! need to interpolate the data, regardless
    ! each mpi task needs to interpolate
    call t_startf('interpolate_trcdata')
    call interpolate_trcdata(ncol  = ncol, &
                             pver  = pver, &
                             pverp = pverp, &
                             pmid  = pmid(:ncol,:pver), &
                             pint  = pint(:ncol,:pverp), &
                             phis  = phis(:ncol), &
                             zi    = zi(:ncol, :pverp), &
                             flds  = flds(:), &
                             file  = file)
    call t_stopf('interpolate_trcdata')

    file%initialized = .true.

    call t_stopf('advance_trcdata')

  end subroutine advance_trcdata

  pure subroutine get_fld_data(flds, field_name, data, ncol)
    type(trfld), intent(inout) :: flds(:)
    character(len=*), intent(in) :: field_name
    real(r8), intent(out) :: data(:, :)
    integer, intent(in) :: ncol

    integer :: f, nflds
    real(r8), pointer  :: tmpptr(:, :)

    data(:, :) = 0._r8
    nflds = size(flds)

    do f = 1, nflds
      if (trim(flds(f)%fldnam) == trim(field_name)) then
        data(:ncol, :) = flds(f)%data(:ncol, :)
      end if
    end do

  end subroutine get_fld_data

  pure subroutine get_fld_ndx(flds, field_name, idx)

    type(trfld), intent(in) :: flds(:)
    character(len=*), intent(in) :: field_name
    integer, intent(out) :: idx
    integer :: f, nflds

    idx = -1
    nflds = size(flds)

    do f = 1, nflds
      if (trim(flds(f)%fldnam) == trim(field_name)) then
        idx = f
        return
      end if
    end do

  end subroutine get_fld_ndx

  subroutine get_model_time(file)
    type(trfile), intent(inout) :: file

    integer yr, mon, day, ncsec  ! components of a date

    call get_curr_date(yr, mon, day, ncsec)

    if (file%cyclical .or. file%cyclical_list) yr = file%cyc_yr
    call set_time_float_from_date(file%curr_mod_time, yr, mon, day, ncsec)
    file%next_mod_time = file%curr_mod_time + get_step_size()/86400._r8

  end subroutine get_model_time

  subroutine check_files(file, fids, itms, times_found)
    type(trfile), intent(inout) :: file
    type(file_desc_t), intent(out)   :: fids(2) ! ids of files that contains these recs
    integer, optional, intent(out)   :: itms(2)
    logical, optional, intent(inout) :: times_found

    logical            :: list_cycled

    list_cycled = .false.

    !-----------------------------------------------------------------------
    !        If next time beyond the end of the time list,
    !        then increment the filename and move on to the next file
    !-----------------------------------------------------------------------
    if ((file%next_mod_time > file%curr_data_times(size(file%curr_data_times))) .or. file%cyclical_list) then
      if (file%cyclical_list) then
        if (associated(file%next_data_times)) then
          if ((file%curr_mod_time > file%datatimep)) then

            call advance_file(file)

          end if
        end if

      end if

      if (.not. associated(file%next_data_times)) then
        ! open next file if not already opened...
        if (file%cyclical_list) then
          file%next_filename = incr_filename(file%curr_filename, filenames_list=file%filenames_list, datapath=file%pathname, &
                                             cyclical_list=file%cyclical_list, list_cycled=list_cycled)
        else
          file%next_filename = incr_filename(file%curr_filename, filenames_list=file%filenames_list, datapath=file%pathname)
        end if
        call open_trc_datafile(file%next_filename, file%pathname, file%next_fileid, file%next_data_times)
        file%next_data_times = file%next_data_times - file%offset_time
      end if
    end if

    !-----------------------------------------------------------------------
    !        If using next_data_times and the current is greater than or equal to the next, then
    !        close the current file, and set up for next file.
    !-----------------------------------------------------------------------
    if (associated(file%next_data_times)) then
      if (file%cyclical_list .and. list_cycled) then    ! special case - list cycled

        file%datatimem = file%curr_data_times(size(file%curr_data_times))
        itms(1) = size(file%curr_data_times)
        fids(1) = file%curr_fileid

        file%datatimep = file%next_data_times(1)
        itms(2) = 1
        fids(2) = file%next_fileid

        times_found = .true.

      else if (file%curr_mod_time >= file%next_data_times(1)) then

        call advance_file(file)

      end if
    end if

  end subroutine check_files

  ! Increment or decrement a date string withing a filename
  ! the filename date section is assumed to be of the form yyyy-dd-mm
  function incr_filename(filename, filenames_list, datapath, cyclical_list, list_cycled, abort)
    use string_utils, only: increment_string
    use shr_file_mod, only: shr_file_getunit, shr_file_freeunit

    character(len=*), intent(in)    :: filename ! present dynamical dataset filename
    character(len=*), optional, intent(in)    :: filenames_list
    character(len=*), optional, intent(in)    :: datapath
    logical, optional, intent(in)    :: cyclical_list  ! If true, allow list to cycle
    logical, optional, intent(out)   :: list_cycled
    logical, optional, intent(in)    :: abort

    character(len=shr_kind_cl)                :: incr_filename         ! next filename in the sequence

    !-----------------------------------------------------------------------
    !   ... local variables
    !-----------------------------------------------------------------------
    integer :: pos, istat
    character(len=shr_kind_cl) :: fn_new, line, filepath
    integer :: ios, unitnumber
    logical :: abort_run

    if (present(abort)) then
      abort_run = abort
    else
      abort_run = .true.
    end if

    if (present(list_cycled)) list_cycled = .false.

    if ((.not. present(filenames_list)) .or. (len_trim(filenames_list) == 0)) then
      !-----------------------------------------------------------------------
      !    ... ccm type filename
      !-----------------------------------------------------------------------
      pos = len_trim(filename)
      fn_new = filename(:pos)
      if (masterproc) write (iulog, *) 'incr_flnm: old filename = ', trim(fn_new)
      if (fn_new(pos - 2:) == '.nc') then
        pos = pos - 3
      end if
      istat = increment_string(fn_new(:pos), 1)
      if (istat /= 0) then
        write (iulog, *) 'incr_flnm: increment_string returned ', istat
        write (iulog, *) '           while trying to decrement ', trim(fn_new)
        call endrun('incr_flnm: increment_string failure')
      end if

    else

      !-------------------------------------------------------------------
      !  ... open filenames_list
      !-------------------------------------------------------------------
      if (masterproc) write(iulog, *) 'incr_flnm: old filename = ', trim(filename)
      if (masterproc) write(iulog, *) 'incr_flnm: open filenames_list : ', trim(filenames_list)
      unitnumber = shr_file_getUnit()
      if (present(datapath)) then
        filepath = trim(datapath)//'/'//trim(filenames_list)
      else
        filepath = trim(filenames_list)
      end if

      open (unit=unitnumber, file=filepath, iostat=ios, status="OLD")
      if (ios /= 0) then
        call endrun('not able to open file: '//trim(filepath))
      end if

      !-------------------------------------------------------------------
      !  ...  read file names
      !-------------------------------------------------------------------
      read (unit=unitnumber, fmt='(A)', iostat=ios) line
      if (ios /= 0) then
        if (abort_run) then
          call endrun('not able to increment file name from filenames_list file: '//trim(filenames_list))
        else
          fn_new = 'NOT_FOUND'
          incr_filename = trim(fn_new)
          return
        end if
      end if

      !-------------------------------------------------------------------
      !      If current filename is '', then initialize with the first filename read in
      !      and skip this section.
      !-------------------------------------------------------------------
      if (filename /= '') then

        !-------------------------------------------------------------------
        !       otherwise read until find current filename
        !-------------------------------------------------------------------
        do while (trim(line) /= trim(filename))
          read (unit=unitnumber, fmt='(A)', iostat=ios) line
          if (ios /= 0) then
            if (abort_run) then
              call endrun('not able to increment file name from filenames_list file: '//trim(filenames_list))
            else
              fn_new = 'NOT_FOUND'
              incr_filename = trim(fn_new)
              return
            end if
          end if
        end do

        !-------------------------------------------------------------------
        !      Read next filename
        !-------------------------------------------------------------------
        read (unit=unitnumber, fmt='(A)', iostat=ios) line

        !---------------------------------------------------------------------------------
        !       If cyclical_list, then an end of file is not an error, but rather
        !       a signal to rewind and start over
        !---------------------------------------------------------------------------------

        if (ios /= 0) then
          if (present(cyclical_list)) then
            if (cyclical_list) then
              list_cycled = .true.
              rewind (unitnumber)
              read (unit=unitnumber, fmt='(A)', iostat=ios) line
              ! Error here should never happen, but check just in case
              if (ios /= 0) then
                call endrun('not able to increment file name from filenames_list file: '//trim(filenames_list))
              end if
            else
              call endrun('not able to increment file name from filenames_list file: '//trim(filenames_list))
            end if
          else
            if (abort_run) then
              call endrun('not able to increment file name from filenames_list file: '//trim(filenames_list))
            else
              fn_new = 'NOT_FOUND'
              incr_filename = trim(fn_new)
              return
            end if
          end if
        end if

      end if

      !---------------------------------------------------------------------------------
      !     Assign the current filename and close the filelist
      !---------------------------------------------------------------------------------
      fn_new = trim(line)

      close (unit=unitnumber)
      call shr_file_freeUnit(unitnumber)
    end if

    !---------------------------------------------------------------------------------
    !      return the current filename
    !---------------------------------------------------------------------------------
    incr_filename = trim(fn_new)
    if (masterproc) write (iulog, *) 'incr_flnm: new filename = ', trim(incr_filename)

  end function incr_filename

  subroutine find_times(itms, fids, time, file, datatimem, datatimep, times_found)
    use cam_abortutils, only: check_allocate

    type(trfile), intent(in) :: file
    real(r8), intent(out) :: datatimem, datatimep

    integer, intent(out) :: itms(2) ! record numbers that bracket time
    type(file_desc_t), intent(out) :: fids(2) ! ids of files that contains these recs

    real(r8), intent(in) :: time    ! time of interest
    logical, intent(inout)  :: times_found

    integer :: np1        ! current forward time index of dataset
    integer :: n, i      !
    integer :: curr_tsize, next_tsize, all_tsize
    integer :: astat
    integer :: cyc_tsize

    real(r8), allocatable, dimension(:):: all_data_times

    character(len=512) :: errmsg
    character(len=*), parameter :: subname = "find_times"

    curr_tsize = size(file%curr_data_times)
    next_tsize = 0
    if (associated(file%next_data_times)) next_tsize = size(file%next_data_times)

    all_tsize = curr_tsize + next_tsize

    allocate (all_data_times(all_tsize), stat=astat, errmsg=errmsg)
    call check_allocate(astat, subname, 'all_data_times(all_tsize)',                   &
                        file=__FILE__, line=__LINE__, errmsg=errmsg)

    all_data_times(:curr_tsize) = file%curr_data_times(:)
    if (next_tsize > 0) all_data_times(curr_tsize + 1:all_tsize) = file%next_data_times(:)

    if (.not. file%cyclical) then
      if (all(all_data_times(:) > time)) then
        write (iulog, *) subname//': ALL data times are after ', time
        write (iulog, *) subname//': file: ', trim(file%curr_filename)
        write (iulog, *) subname//': time: ', time
        call endrun(subname//': all(all_data_times(:) > time) '//trim(file%curr_filename))
      end if

      ! find bracketing times
      find_times_loop: do n = 1, all_tsize - 1
        np1 = n + 1
        datatimem = all_data_times(n)   !+ file%offset_time
        datatimep = all_data_times(np1) !+ file%offset_time
        ! When stepTime, datatimep may not equal the time (as only datatimem is used)
        ! Should not break other runs?
        if ((time >= datatimem) .and. (time < datatimep)) then
          times_found = .true.
          exit find_times_loop
        end if
      end do find_times_loop

    else  ! file%cyclical

      cyc_tsize = file%cyc_ndx_end - file%cyc_ndx_beg + 1

      if (cyc_tsize > 1) then

        call findplb(all_data_times(file%cyc_ndx_beg:file%cyc_ndx_end), cyc_tsize, time, n)

        if (n == cyc_tsize) then
          np1 = 1
        else
          np1 = n + 1
        end if

        datatimem = all_data_times(n + file%cyc_ndx_beg - 1)
        datatimep = all_data_times(np1 + file%cyc_ndx_beg - 1)
        times_found = .true.

      end if
    end if

    if (.not. times_found) then
      if (masterproc) then
        write (iulog, *) subname//': Failed to find dates bracketing desired time =', time
        write (iulog, *) 'filename = '//trim(file%curr_filename)
        write (iulog, *) ' datatimem = ', file%datatimem
        write (iulog, *) ' datatimep = ', file%datatimep
      end if
      return
    end if

    deallocate (all_data_times, stat=astat)
    if (astat /= 0) then
      write (iulog, *) subname//': failed to deallocate all_data_times array; error = ', astat
      call endrun(subname//': failed to deallocate all_data_times array')
    end if

    if (.not. file%cyclical) then
      itms(1) = n
      itms(2) = np1
    else
      itms(1) = n + file%cyc_ndx_beg - 1
      itms(2) = np1 + file%cyc_ndx_beg - 1
    end if

    fids(:) = file%curr_fileid

    do i = 1, 2
      if (itms(i) > curr_tsize) then
        itms(i) = itms(i) - curr_tsize
        fids(i) = file%next_fileid
      end if
    end do

  end subroutine find_times

  subroutine read_next_trcdata(flds, file)

    type(trfile), intent(inout) :: file
    type(trfld), intent(inout) :: flds(:)

    integer :: recnos(4), i, f, nflds      !
    integer :: cnt4(4)            ! array of counts for each dimension
    integer :: strt4(4)           ! array of starting indices
    integer :: cnt3(3)            ! array of counts for each dimension
    integer :: strt3(3)           ! array of starting indices
    type(file_desc_t) :: fids(4)
    logical :: times_found

    integer :: cur_yr, cur_mon, cur_day, cur_sec, yr1, yr2, mon, date, sec
    real(r8) :: series1_time, series2_time
    type(file_desc_t) :: fid1, fid2

    nflds = size(flds)
    times_found = .false.

    do while (.not. times_found)
      call find_times(recnos, fids, file%curr_mod_time, file, file%datatimem, file%datatimep, times_found)
      if (.not. times_found) then
        call check_files(file, fids, recnos, times_found)
      end if
    end do

    !--------------------------------------------------------------
    !       If stepTime, then no time interpolation is to be done
    !--------------------------------------------------------------
    if (file%stepTime) then
      file%interp_recs = 1
    else
      file%interp_recs = 2
    end if

    if (file%fill_in_months) then

      if (file%datatimep - file%datatimem > file%one_yr) then

        call get_curr_date(cur_yr, cur_mon, cur_day, cur_sec)

        call set_date_from_time_float(file%datatimem, yr1, mon, date, sec)
        call set_date_from_time_float(file%datatimep, yr2, mon, date, sec)

        call set_time_float_from_date(series1_time, yr1, cur_mon, cur_day, cur_sec)
        call set_time_float_from_date(series2_time, yr2, cur_mon, cur_day, cur_sec)

        fid1 = fids(1)
        fid2 = fids(2)
        file%cyclical = .true.
        call set_cycle_indices(fid1, file%cyc_ndx_beg, file%cyc_ndx_end, yr1)
        call find_times(recnos(1:2), fids(1:2), series1_time, file, file%datatimes(1), file%datatimes(2), times_found)

        if (.not. times_found) then
          call endrun('read_next_trcdata: time not found for series1_time')
        end if
        call set_cycle_indices(fid2, file%cyc_ndx_beg, file%cyc_ndx_end, yr2)

        if (fid1%fh /= fid2%fh) then
          file%cyc_ndx_beg = file%cyc_ndx_beg + size(file%curr_data_times)
          file%cyc_ndx_end = file%cyc_ndx_end + size(file%curr_data_times)
        end if
        call find_times(recnos(3:4), fids(3:4), series2_time, file, file%datatimes(3), file%datatimes(4), times_found)
        if (.not. times_found) then
          call endrun('read_next_trcdata: time not found for series2_time')
        end if
        file%cyclical = .false.
        file%interp_recs = 4

        call set_date_from_time_float(file%datatimes(1), yr1, mon, date, sec)
        call set_time_float_from_date(file%datatimem, cur_yr, mon, date, sec)
        if (file%datatimes(1) > file%datatimes(2)) then ! wrap around
          if (cur_mon == 1) then
            call set_time_float_from_date(file%datatimem, cur_yr - 1, mon, date, sec)
          end if
        end if

        call set_date_from_time_float(file%datatimes(2), yr1, mon, date, sec)
        call set_time_float_from_date(file%datatimep, cur_yr, mon, date, sec)
        if (file%datatimes(1) > file%datatimes(2)) then ! wrap around
          if (cur_mon == 12) then
            call set_time_float_from_date(file%datatimep, cur_yr + 1, mon, date, sec)
          end if
        end if

      end if

    end if

    !
    ! Set up hyperslab corners
    !
    do i = 1, file%interp_recs

      strt4(:) = 1
      strt3(:) = 1

      do f = 1, nflds
        if (file%zonal_ave) then
          cnt3(flds(f)%coords(ZA_LATDIM)) = file%nlat
          if (flds(f)%srf_fld) then
            cnt3(flds(f)%coords(ZA_LEVDIM)) = 1
          else
            cnt3(flds(f)%coords(ZA_LEVDIM)) = file%nlev
          end if
          cnt3(flds(f)%coords(ZA_TIMDIM)) = 1
          strt3(flds(f)%coords(ZA_TIMDIM)) = recnos(i)
          call read_za_trc(fids(i), flds(f)%var_id, flds(f)%input(i)%data, strt3, cnt3, file, &
                           (/flds(f)%order(ZA_LATDIM), flds(f)%order(ZA_LEVDIM)/))
        else if (flds(f)%srf_fld) then
          if (file%unstructured) then
            ! read data directly onto the unstructureed phys grid -- assumes input data is on same grid as phys
            call read_physgrid_2d(fids(i), flds(f)%fldnam, recnos(i), flds(f)%input(i)%data(:, 1))
          else
            cnt3(flds(f)%coords(LONDIM)) = file%nlon
            cnt3(flds(f)%coords(LATDIM)) = file%nlat
            cnt3(flds(f)%coords(PS_TIMDIM)) = 1
            strt3(flds(f)%coords(PS_TIMDIM)) = recnos(i)
            call read_2d_trc(fids(i), flds(f)%var_id, flds(f)%input(i)%data(:, 1), strt3, cnt3, file, &
                             (/flds(f)%order(LONDIM), flds(f)%order(LATDIM)/))
          end if
        else
          if (file%unstructured) then
            ! read data directly onto the unstructureed phys grid -- assumes input data is on same grid as phys
            if (file%alt_data) then
              call read_physgrid_3d(fids(i), flds(f)%fldnam, 'altitude', file%nlev, recnos(i), flds(f)%input(i)%data(:, :))
            else
              call read_physgrid_3d(fids(i), flds(f)%fldnam, 'lev', file%nlev, recnos(i), flds(f)%input(i)%data(:, :))
            end if
          else
            cnt4(flds(f)%coords(LONDIM)) = file%nlon
            cnt4(flds(f)%coords(LATDIM)) = file%nlat
            cnt4(flds(f)%coords(LEVDIM)) = file%nlev
            cnt4(flds(f)%coords(TIMDIM)) = 1
            strt4(flds(f)%coords(TIMDIM)) = recnos(i)
            call read_3d_trc(fids(i), flds(f)%var_id, flds(f)%input(i)%data, strt4, cnt4, file, &
                             (/flds(f)%order(LONDIM), flds(f)%order(LATDIM), flds(f)%order(LEVDIM)/))
          end if

        end if

      end do

      if (file%has_ps) then
        if (file%unstructured) then
          call read_physgrid_2d(fids(i), 'PS', recnos(i), file%ps_in(i)%data)
        else
          cnt3 = 1
          strt3 = 1
          if (.not. file%zonal_ave) then
            cnt3(file%ps_coords(LONDIM)) = file%nlon
          end if
          cnt3(file%ps_coords(LATDIM)) = file%nlat
          cnt3(file%ps_coords(PS_TIMDIM)) = 1
          strt3(file%ps_coords(PS_TIMDIM)) = recnos(i)
          if (file%zonal_ave) then
            call read_2d_trc(fids(i), file%ps_id, file%ps_in(i)%data, strt3(1:2), cnt3(1:2), file, &
                             (/1, 2/))
          else
            call read_2d_trc(fids(i), file%ps_id, file%ps_in(i)%data, strt3, cnt3, file, &
                             (/file%ps_order(LONDIM), file%ps_order(LATDIM)/))
          end if
        end if
      end if

    end do

  end subroutine read_next_trcdata

!------------------------------------------------------------------------

  subroutine read_2d_trc(fid, vid, loc_arr, strt, cnt, file, order)
    use interpolate_data, only: lininterp_init, lininterp, interp_type, lininterp_finish
    use horizontal_interpolate, only: xy_interp

    use physics_grid, only: get_rlat_all_p, get_rlon_all_p
    use physconst, only: pi
    use cam_abortutils, only: check_allocate

    type(file_desc_t), intent(in) :: fid
    type(var_desc_t), intent(in) :: vid
    integer, intent(in) :: strt(:), cnt(:), order(2)
    real(r8), intent(out)  :: loc_arr(:) ! (ncol)
    type(trfile), intent(in) :: file

    real(r8) :: to_lats(pcols), to_lons(pcols)
    real(r8), allocatable, target :: wrk2d(:, :) ! (cnt(1), cnt(2))
    real(r8), pointer :: wrk2d_in(:, :) ! (file%nlon, file%nlat)

    integer :: ierr
    real(r8), parameter :: zero = 0_r8, twopi = 2_r8*pi
    type(interp_type) :: lon_wgts, lat_wgts
    integer :: lons(pcols), lats(pcols)
    real(r8) :: file_lats(file%nlat)

    character(len=512) :: errmsg
    character(len=*), parameter :: subname = "read_2d_trc"

    nullify (wrk2d_in)
    allocate (wrk2d(cnt(1), cnt(2)), stat=ierr, errmsg=errmsg)
    call check_allocate(ierr, subname, 'wrk2d(cnt(1), cnt(2))', &
                        file=__FILE__, line=__LINE__, errmsg=errmsg)

    if (order(1) /= 1 .or. order(2) /= 2 .or. cnt(1) /= file%nlon .or. cnt(2) /= file%nlat) then
      allocate (wrk2d_in(file%nlon, file%nlat), stat=ierr)

      call check_allocate(ierr, subname, 'wrk2d_in(file%nlon, file%nlat)', &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)
    end if

    ierr = pio_get_var(fid, vid, strt, cnt, wrk2d)
    if (associated(wrk2d_in)) then
      wrk2d_in = reshape(wrk2d(:, :), (/file%nlon, file%nlat/), order=order)
      deallocate (wrk2d)
    else
      wrk2d_in => wrk2d
    end if

    ! PGI 13.9 bug workaround.
    file_lats = file%lats

    ! For zonal average, only interpolate along latitude.
    if (file%zonal_ave) then
      call get_rlat_all_p(pcols, to_lats)

      call lininterp_init(file_lats, file%nlat, to_lats, pcols, 1, lat_wgts)

      call lininterp(wrk2d_in(1, :), file%nlat, loc_arr(1:pcols), pcols, lat_wgts)

      call lininterp_finish(lat_wgts)
    else
      ! if weighting by latitude, the perform horizontal interpolation by using weight_x, weight_y

      if (file%weight_by_lat) then

        call t_startf('xy_interp')

        lons(:pcols) = lon_global_grid_ndx(:pcols)
        lats(:pcols) = lat_global_grid_ndx(:pcols)

        ! NOTE: This uses weight_[xy] instead of weight0_[xy] and
        ! hence treats the values as a field rather than per-cell
        ! totals.  When file%dist == TRUE, this path only appears
        ! to be used to interpolate PS, which is probably the
        ! correct behavior.
        !
        ! @reviewers: The control flow is convoluted here, so
        ! this merits some additional scrutiny.
        !
        ! in SIMA: pcols (size of array) and ncols (loop dim) here now equal
        call xy_interp(file%nlon, file%nlat, 1, plon, plat, pcols, pcols, &
                       file%weight_x, file%weight_y, wrk2d_in, loc_arr(:), &
                       lons, lats, file%count_x, file%count_y, file%index_x, file%index_y)

        call t_stopf('xy_interp')

      else
        call get_rlat_all_p(pcols, to_lats)
        call get_rlon_all_p(pcols, to_lons)

        call lininterp_init(file%lons, file%nlon, to_lons, pcols, 2, lon_wgts, zero, twopi)
        call lininterp_init(file%lats, file%nlat, to_lats, pcols, 1, lat_wgts)

        call lininterp(wrk2d_in, file%nlon, file%nlat, loc_arr(1:pcols), pcols, lon_wgts, lat_wgts)

        call lininterp_finish(lon_wgts)
        call lininterp_finish(lat_wgts)
      end if

    end if

    if (allocated(wrk2d)) then
      deallocate(wrk2d)
    else
      deallocate(wrk2d_in)
    end if

    ! FV only: commented out for SIMA
    !call polar_average(loc_arr)
  end subroutine read_2d_trc

!------------------------------------------------------------------------

  ! Read zonal average data
  subroutine read_za_trc(fid, vid, loc_arr, strt, cnt, file, order)
    use interpolate_data, only: lininterp_init, lininterp, interp_type, lininterp_finish
    use physics_grid, only: get_rlat_all_p
    use cam_abortutils, only: check_allocate

    type(file_desc_t), intent(in) :: fid
    type(var_desc_t), intent(in) :: vid
    integer, intent(in) :: strt(:), cnt(:)
    integer, intent(in) :: order(2)
    real(r8), intent(out):: loc_arr(:, :)
    type(trfile), intent(in) :: file

    type(interp_type) :: lat_wgts
    real(r8) :: to_lats(pcols), wrk(pcols)
    real(r8), allocatable, target :: wrk2d(:, :)
    real(r8), pointer :: wrk2d_in(:, :)
    integer :: k, ierr
    character(len=512) :: errmsg
    character(len=*), parameter :: subname = "read_za_trc"

    nullify (wrk2d_in)
    allocate (wrk2d(cnt(1), cnt(2)), stat=ierr, errmsg=errmsg)
    call check_allocate(ierr, subname, 'wrk2d(cnt(1), cnt(2))', &
                        file=__FILE__, line=__LINE__, errmsg=errmsg)

    if (order(1) /= 1 .or. order(2) /= 2 .or. cnt(1) /= file%nlat .or. cnt(2) /= file%nlev) then
      allocate (wrk2d_in(file%nlat, file%nlev), stat=ierr, errmsg=errmsg)

      call check_allocate(ierr, subname, 'wrk2d_in(file%nlat, file%nlev)', &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)
    end if

    ierr = pio_get_var(fid, vid, strt, cnt, wrk2d)
    if (associated(wrk2d_in)) then
      wrk2d_in = reshape(wrk2d(:, :), (/file%nlat, file%nlev/), order=order)
      deallocate (wrk2d)
    else
      wrk2d_in => wrk2d
    end if

    call get_rlat_all_p(pcols, to_lats)

    call lininterp_init(file%lats, file%nlat, to_lats, pcols, 1, lat_wgts)
    do k = 1, file%nlev
      call lininterp(wrk2d_in(:, k), file%nlat, wrk(1:pcols), pcols, lat_wgts)
      loc_arr(1:pcols, k) = wrk(1:pcols)
    end do
    call lininterp_finish(lat_wgts)

    if (allocated(wrk2d)) then
      deallocate (wrk2d)
    else
      deallocate (wrk2d_in)
    end if
  end subroutine read_za_trc

  ! this assumes the input data is gridded to match the physics grid
  subroutine read_physgrid_2d(ncid, varname, recno, data)
    use cam_field_read, only: cam_read_field

    type(file_desc_t), intent(inout) :: ncid
    character(len=*),  intent(in)  :: varname
    integer,           intent(in)  :: recno
    real(r8),          intent(out) :: data(1:pcols)

    logical :: found

    call cam_read_field(varname=varname, ncid=ncid, &
                        field=data(:pcols), readvar=found, &
                        gridname='physgrid', &
                        timelevel=recno)

    if (.not. found) then
      call endrun('tracer_data::read_physgrid_2d: Could not find '//trim(varname)//' field in input datafile')
    end if

  end subroutine read_physgrid_2d

  ! this assumes the input data is gridded to match the physics grid
  subroutine read_physgrid_3d(ncid, varname, vrt_coord_name, nlevs, recno, data)
    use cam_field_read, only: cam_read_field

    type(file_desc_t), intent(inout) :: ncid
    character(len=*),  intent(in)  :: varname
    character(len=*),  intent(in)  :: vrt_coord_name
    integer,           intent(in)  :: nlevs
    integer,           intent(in)  :: recno
    real(r8),          intent(out) :: data(1:pcols, 1:nlevs)

    logical :: found

    call cam_read_field(varname=varname, ncid=ncid, &
                        field=data(:pcols,:nlevs), readvar=found, &
                        gridname='physgrid', &
                        timelevel=recno, &
                        dim3name=vrt_coord_name, dim3_bnds=[1, nlevs])

    if (.not. found) then
      call endrun('tracer_data::read_physgrid_3d: Could not find '//trim(varname)//' field in input datafile')
    end if

  end subroutine read_physgrid_3d

  !------------------------------------------------------------------------

  subroutine read_3d_trc(fid, vid, loc_arr, strt, cnt, file, order)
    ! Interpolation utils
    use interpolate_data, only: lininterp_init, lininterp, interp_type, lininterp_finish
    use horizontal_interpolate, only: xy_interp

    use physics_grid, only: get_rlat_all_p, get_rlon_all_p
    use physconst, only: pi

    use cam_abortutils, only: check_allocate

    type(file_desc_t), intent(in) :: fid
    type(var_desc_t), intent(in) :: vid
    integer, intent(in) :: strt(:), cnt(:), order(3)
    real(r8), intent(out)  :: loc_arr(:, :)

    type(trfile), intent(in) :: file

    integer :: astat
    integer :: lons(pcols), lats(pcols)

    integer :: ierr

    real(r8), allocatable, target :: wrk3d(:, :, :)
    real(r8), pointer :: wrk3d_in(:, :, :)
    real(r8) :: to_lons(pcols), to_lats(pcols)
    real(r8), parameter :: zero = 0_r8, twopi = 2_r8*pi
    type(interp_type) :: lon_wgts, lat_wgts

    character(len=512) :: errmsg
    character(len=*), parameter :: subname = "read_3d_trc"

    loc_arr(:, :) = 0._r8
    nullify (wrk3d_in)
    allocate (wrk3d(cnt(1), cnt(2), cnt(3)), stat=ierr, errmsg=errmsg)
    call check_allocate(ierr, subname, 'wrk3d(cnt(1), cnt(2), cnt(3))', &
                        file=__FILE__, line=__LINE__, errmsg=errmsg)

    ierr = pio_get_var(fid, vid, strt, cnt, wrk3d)

    if (order(1) /= 1 .or. order(2) /= 2 .or. order(3) /= 3 .or. &
        cnt(1) /= file%nlon .or. cnt(2) /= file%nlat .or. cnt(3) /= file%nlev) then
      allocate (wrk3d_in(file%nlon, file%nlat, file%nlev), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'wrk3d_in(file%nlon, file%nlat, file%nlev)', &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)

      wrk3d_in = reshape(wrk3d(:, :, :), (/file%nlon, file%nlat, file%nlev/), order=order)
      deallocate (wrk3d)
    else
      wrk3d_in => wrk3d
    end if

    ! If weighting by latitude, then perform horizontal interpolation by using weight_x, weight_y
    if (file%weight_by_lat) then

      call t_startf('xy_interp')
      if (file%dist) then
        lons(:pcols) = lon_global_grid_ndx(:pcols)
        lats(:pcols) = lat_global_grid_ndx(:pcols)

        call xy_interp(file%nlon, file%nlat, file%nlev, plon, plat, pcols, pcols, &
                       file%weight0_x, file%weight0_y, wrk3d_in, loc_arr(:, :), &
                       lons, lats, file%count0_x, file%count0_y, file%index0_x, file%index0_y)
      else
        lons(:pcols) = lon_global_grid_ndx(:pcols)
        lats(:pcols) = lat_global_grid_ndx(:pcols)

        call xy_interp(file%nlon, file%nlat, file%nlev, plon, plat, pcols, pcols, &
                       file%weight_x, file%weight_y, wrk3d_in, loc_arr(:, :), &
                       lons, lats, file%count_x, file%count_y, file%index_x, file%index_y)
      end if
      call t_stopf('xy_interp')

    else
      call get_rlat_all_p(pcols, to_lats)
      call get_rlon_all_p(pcols, to_lons)

      call lininterp_init(file%lons, file%nlon, to_lons(1:pcols), pcols, 2, lon_wgts, zero, twopi)
      call lininterp_init(file%lats, file%nlat, to_lats(1:pcols), pcols, 1, lat_wgts)

      call lininterp(wrk3d_in, file%nlon, file%nlat, file%nlev, loc_arr(:, :), pcols, pcols, lon_wgts, lat_wgts)

      call lininterp_finish(lon_wgts)
      call lininterp_finish(lat_wgts)
    end if

    if (allocated(wrk3d)) then
      deallocate(wrk3d, stat=astat)
    else
      deallocate(wrk3d_in, stat=astat)
    end if
    if (astat /= 0) then
      write(iulog, *) 'read_3d_trc: failed to deallocate wrk3d array; error = ', astat
      call endrun('read_3d_trc: failed to deallocate wrk3d array')
    end if

    ! FV only: commented out for SIMA
    !call polar_average(file%nlev, loc_arr)
  end subroutine read_3d_trc

!------------------------------------------------------------------------------

  subroutine interpolate_trcdata( &
             ncol, pver, pverp, &
             pmid, pint, phis, zi, &
             flds, file)
    use ccpp_kinds, only: kind_phys
    use physconst, only: cday, rga

    integer,         intent(in)    :: ncol
    integer,         intent(in)    :: pver
    integer,         intent(in)    :: pverp
    ! state variables used for interpolation
    real(kind_phys), intent(in)    :: pmid(:, :)
    real(kind_phys), intent(in)    :: pint(:, :)
    real(kind_phys), intent(in)    :: phis(:)
    real(kind_phys), intent(in)    :: zi(:, :)

    type(trfld),     intent(inout) :: flds(:)
    type(trfile),    intent(inout) :: file

    real(r8) :: fact1, fact2
    real(r8) :: deltat
    integer :: f, nflds, i, k
    real(r8) :: ps(pcols)
    real(r8) :: datain(pcols, file%nlev)
    real(r8) :: pin(pcols, file%nlev)
    real(r8)            :: model_z(pverp)
    real(r8), parameter :: m2km = 1.e-3_r8
    real(r8), pointer :: data_out(:, :)
    real(r8) :: data_col(pver)

    nflds = size(flds)

    if (file%interp_recs == 4) then
      deltat = file%datatimes(3) - file%datatimes(1)
      fact1 = (file%datatimes(3) - file%datatimem)/deltat
      fact2 = 1._r8 - fact1

      if (file%has_ps) then
        file%ps_in(1)%data(:ncol) = fact1*file%ps_in(1)%data(:ncol) + fact2*file%ps_in(3)%data(:ncol)
      end if
      do f = 1, nflds
        flds(f)%input(1)%data(:ncol, :) = fact1*flds(f)%input(1)%data(:ncol, :) + fact2*flds(f)%input(3)%data(:ncol, :)
      end do

      deltat = file%datatimes(4) - file%datatimes(2)
      fact1 = (file%datatimes(4) - file%datatimep)/deltat
      fact2 = 1._r8 - fact1

      if (file%has_ps) then
        file%ps_in(2)%data(:ncol) = fact1*file%ps_in(2)%data(:ncol) + fact2*file%ps_in(4)%data(:ncol)
      end if
      do f = 1, nflds
        flds(f)%input(2)%data(:ncol, :) = fact1*flds(f)%input(2)%data(:ncol, :) + fact2*flds(f)%input(4)%data(:ncol, :)
      end do

    end if
    !-------------------------------------------------------------------------
    !       If file%interp_recs=1 then no time interpolation -- set
    !       fact1=1 and fact2=0 and will just use first value unmodified
    !-------------------------------------------------------------------------

    if (file%interp_recs == 1) then
      fact1 = 1._r8
      fact2 = 0._r8
    else
      file%interp_recs = 2

      deltat = file%datatimep - file%datatimem

      if (file%cyclical .and. (deltat < 0._r8)) then
        deltat = deltat + file%one_yr
        if (file%datatimep >= file%curr_mod_time) then
          fact1 = (file%datatimep - file%curr_mod_time)/deltat
        else
          fact1 = (file%datatimep + file%one_yr - file%curr_mod_time)/deltat
        end if
      else
        fact1 = (file%datatimep - file%curr_mod_time)/deltat
      end if

      ! this assures that FIXED data are b4b on restarts
      if (file%fixed) then
        fact1 = dble(int(fact1*cday + .5_r8))/dble(cday)
      end if
      fact2 = 1._r8 - fact1
    end if

    fld_loop: do f = 1, nflds
      data_out => flds(f)%data(:, :)

      if (file%alt_data) then
        if (fact2 == 0) then  ! This needed as %data is not set if fact2=0 (and lahey compiler core dumps)
          datain(:ncol, :) = fact1*flds(f)%input(nm)%data(:ncol, :)
        else
          datain(:ncol, :) = fact1*flds(f)%input(nm)%data(:ncol, :) + fact2*flds(f)%input(np)%data(:ncol, :)
        end if
        do i = 1, ncol
          model_z(1:pverp) = m2km*zi(i, pverp:1:-1)
          if (file%geop_alt) then
            model_z(1:pverp) = model_z(1:pverp) + m2km*phis(i)*rga
          end if
          if (file%conserve_column) then
            call interpz_conserve(file%nlev, pver, file%ilevs, model_z, datain(i, :), data_col(:))
          else
            call rebin(file%nlev, pver, file%ilevs, model_z, datain(i, :), data_col(:))
          end if
          data_out(i, :) = data_col(pver:1:-1)
        end do
      else ! .not. alt_data
        if (file%nlev > 1) then
          if (file%has_ps) then
            if (fact2 == 0) then  ! This needed as %data is not set if fact2=0 (and lahey compiler core dumps)
              ps(:ncol) = fact1*file%ps_in(nm)%data(:ncol)
            else
              ps(:ncol) = fact1*file%ps_in(nm)%data(:ncol) + fact2*file%ps_in(np)%data(:ncol)
            end if
            do i = 1, ncol
              do k = 1, file%nlev
                pin(i, k) = file%p0*file%hyam(k) + ps(i)*file%hybm(k)
              end do
            end do
          else
            do k = 1, file%nlev
              pin(:, k) = file%levs(k)
            end do
          end if
        end if

        if (flds(f)%srf_fld) then
          do i = 1, ncol
            if (fact2 == 0) then  ! This needed as %data is not set if fact2=0 (and lahey compiler core dumps)
              data_out(i, 1) = &
                fact1*flds(f)%input(nm)%data(i, 1)
            else
              data_out(i, 1) = &
                fact1*flds(f)%input(nm)%data(i, 1) + fact2*flds(f)%input(np)%data(i, 1)
            end if
          end do
        else
          if (fact2 == 0) then  ! This needed as %data is not set if fact2=0 (and lahey compiler core dumps)
            datain(:ncol, :) = fact1*flds(f)%input(nm)%data(:ncol, :)
          else
            datain(:ncol, :) = fact1*flds(f)%input(nm)%data(:ncol, :) + fact2*flds(f)%input(np)%data(:ncol, :)
          end if
          if (file%top_bndry) then
            call vert_interp_ub(ncol, file%nlev, file%levs, datain(:ncol, :), data_out(:ncol, 1))
          else if (file%top_layer) then
            call vert_interp_ub_var(ncol, file%nlev, file%levs, pmid(:ncol, 1), datain(:ncol, :), data_out(:ncol, 1))
          else if (file%conserve_column) then
            call vert_interp_mixrat(ncol, file%nlev, pver, pint, &
                                    datain, data_out(:, :), &
                                    file%p0, ps, file%hyai, file%hybi, file%dist)
          else
            call vert_interp(ncol, file%nlev, pin, pmid, datain, data_out(:, :))
          end if
        end if

      end if

    end do fld_loop

  end subroutine interpolate_trcdata

  subroutine get_dimension(fid, dname, dsize, dimid, data)
    type(file_desc_t), intent(inout) :: fid
    character(*),      intent(in)    :: dname
    integer,           intent(out)   :: dsize

    integer,  optional, intent(out)  :: dimid
    real(r8), optional, pointer, dimension(:) :: data

    integer :: vid, ierr, id
    integer :: err_handling

    call pio_seterrorhandling(fid, PIO_BCAST_ERROR, oldmethod=err_handling)
    ierr = pio_inq_dimid(fid, dname, id)
    call pio_seterrorhandling(fid, err_handling)

    if (ierr == PIO_NOERR) then
      ierr = pio_inq_dimlen(fid, id, dsize)

      if (present(dimid)) then
        dimid = id
      end if

      if (present(data)) then
        if (associated(data)) then
          deallocate (data, stat=ierr)
          if (ierr /= 0) then
            write (iulog, *) 'get_dimension: data deallocation error = ', ierr
            call endrun('get_dimension: failed to deallocate data array')
          end if
        end if
        allocate (data(dsize), stat=ierr)
        if (ierr /= 0) then
          write (iulog, *) 'get_dimension: data allocation error = ', ierr
          call endrun('get_dimension: failed to allocate data array')
        end if

        ierr = pio_inq_varid(fid, dname, vid)
        ierr = pio_get_var(fid, vid, data)
      end if
    else
      dsize = 1
      if (present(dimid)) then
        dimid = -1
      end if
    end if

  end subroutine get_dimension

  subroutine set_cycle_indices(fileid, cyc_ndx_beg, cyc_ndx_end, cyc_yr)
    use cam_abortutils, only: check_allocate

    type(file_desc_t), intent(inout)  :: fileid
    integer, intent(out) :: cyc_ndx_beg
    integer, intent(out) :: cyc_ndx_end
    integer, intent(in)  :: cyc_yr

    character(len=512) :: errmsg
    character(len=*), parameter :: subname = "set_cycle_indices"

    integer, allocatable, dimension(:) :: dates
    integer :: timesize, i, errflg, year, ierr
    type(var_desc_T) :: dateid
    call get_dimension(fileid, 'time', timesize)
    cyc_ndx_beg = -1

    allocate (dates(timesize), stat=errflg, errmsg=errmsg)
    call check_allocate(errflg, subname, 'dates(timesize)',                   &
                                 file=__FILE__, line=__LINE__, errmsg=errmsg)

    ierr = pio_inq_varid(fileid, 'date', dateid)
    ierr = pio_get_var(fileid, dateid, dates)

    do i = 1, timesize
      year = dates(i)/10000
      if (year == cyc_yr) then
        if (cyc_ndx_beg < 0) then
          cyc_ndx_beg = i
        end if
        cyc_ndx_end = i
      end if
    end do
    deallocate (dates, stat=errflg)
    if (errflg /= 0) then
      call endrun(subname // ': failed to deallocate dates array')
    end if
    if (cyc_ndx_beg < 0) then
      write (*, *) subname // ': cycle year not found : ', cyc_yr
      call endrun(subname // ': cycle year not found')
    end if

  end subroutine set_cycle_indices

  subroutine open_trc_datafile(fname, path, piofile, times, cyc_ndx_beg, cyc_ndx_end, cyc_yr)
    use ioFileMod,     only: cam_get_file
    use cam_pio_utils, only: cam_pio_openfile
    use cam_abortutils, only: check_allocate

    character(*),      intent(in)    :: fname
    character(*),      intent(in)    :: path
    type(file_desc_t), intent(inout) :: piofile
    real(r8),          pointer       :: times(:)

    integer, optional, intent(out) :: cyc_ndx_beg
    integer, optional, intent(out) :: cyc_ndx_end
    integer, optional, intent(in)  :: cyc_yr

    character(len=shr_kind_cl) :: filen, filepath
    integer :: year, month, day, i, timesize
    integer :: dateid, secid
    integer, allocatable, dimension(:) :: dates, datesecs
    integer :: ierr
    logical :: need_first_ndx
    integer :: err_handling

    character(len=512) :: errmsg
    character(len=*), parameter :: subname = "open_trc_datafile"

    if (len_trim(path) == 0) then
      filepath = trim(fname)
    else
      filepath = trim(path)//'/'//trim(fname)
    end if
    !
    ! open file and get fileid
    !
    call cam_get_file(filepath, filen, allow_fail=.false.)
    call cam_pio_openfile(piofile, filen, PIO_NOWRITE)
    if (masterproc) write (iulog, *) 'open_trc_datafile: ', trim(filen)

    call get_dimension(piofile, 'time', timesize)

    if (associated(times)) then
      deallocate (times, stat=ierr)
      if (ierr /= 0) then
        write (iulog, *) 'open_trc_datafile: data deallocation error = ', ierr
        call endrun('open_trc_datafile: failed to deallocate data array')
      end if
    end if

    allocate (times(timesize), stat=ierr, errmsg=errmsg)
    call check_allocate(ierr, subname, 'times(timesize)',                   &
                              file=__FILE__, line=__LINE__, errmsg=errmsg)

    allocate (dates(timesize), stat=ierr, errmsg=errmsg)
    call check_allocate(ierr, subname, 'dates(timesize)',                   &
                              file=__FILE__, line=__LINE__, errmsg=errmsg)

    allocate (datesecs(timesize), stat=ierr, errmsg=errmsg)
    call check_allocate(ierr, subname, 'datesecs(timesize)',                   &
                              file=__FILE__, line=__LINE__, errmsg=errmsg)

    ierr = pio_inq_varid(piofile, 'date', dateid)
    call pio_seterrorhandling(piofile, PIO_BCAST_ERROR, oldmethod=err_handling)
    ierr = pio_inq_varid(piofile, 'datesec', secid)
    call pio_seterrorhandling(piofile, err_handling)

    if (ierr == PIO_NOERR) then
      ierr = pio_get_var(piofile, secid, datesecs)
    else
      datesecs = 0
    end if

    ierr = pio_get_var(piofile, dateid, dates)
    need_first_ndx = .true.

    do i = 1, timesize
      year = dates(i)/10000
      month = mod(dates(i), 10000)/100
      day = mod(dates(i), 100)
      call set_time_float_from_date(times(i), year, month, day, datesecs(i))
      if (present(cyc_yr)) then
        if (year == cyc_yr) then
          if (present(cyc_ndx_beg) .and. need_first_ndx) then
            cyc_ndx_beg = i
            need_first_ndx = .false.
          end if
          if (present(cyc_ndx_end)) then
            cyc_ndx_end = i
          end if
        end if
      end if
    end do

    deallocate (dates, stat=ierr)
    if (ierr /= 0) then
      if (masterproc) write (iulog, *) subname //': failed to deallocate dates array; error = ', ierr
      call endrun(subname //': failed to deallocate dates array')
    end if
    deallocate (datesecs, stat=ierr)
    if (ierr /= 0) then
      if (masterproc) write (iulog, *) subname //': failed to deallocate datesec array; error = ', ierr
      call endrun(subname //': failed to deallocate datesec array')
    end if

    if (present(cyc_yr) .and. present(cyc_ndx_beg)) then
      if (cyc_ndx_beg < 0) then
        write (iulog, *) subname //': cycle year not found : ', cyc_yr
        call endrun(subname //': cycle year not found '//trim(filepath))
      end if
    end if

  end subroutine open_trc_datafile

  subroutine specify_fields(specifier, fields)
    use cam_abortutils, only: check_allocate

    character(len=*), intent(in) :: specifier(:)
    type(trfld), pointer, dimension(:) :: fields

    integer :: fld_cnt, astat
    integer :: i, j
    character(len=shr_kind_cl) :: str1, str2
    character(len=32), allocatable, dimension(:) :: fld_name, src_name
    integer :: nflds

    nflds = size(specifier)

    allocate (fld_name(nflds), src_name(nflds), stat=astat)
    if (astat /= 0) then
      write (iulog, *) 'specify_fields: failed to allocate fld_name, src_name arrays; error = ', astat
      call endrun('specify_fields: failed to allocate fld_name, src_name arrays')
    end if

    fld_cnt = 0

    count_cnst: do i = 1, nflds

      if (len_trim(specifier(i)) == 0) then
        exit count_cnst
      end if

      j = scan(specifier(i), ':')

      if (j > 0) then
        str1 = trim(adjustl(specifier(i) (:j - 1)))
        str2 = trim(adjustl(specifier(i) (j + 1:)))
        fld_name(i) = trim(adjustl(str1))
        src_name(i) = trim(adjustl(str2))
      else
        fld_name(i) = trim(adjustl(specifier(i)))
        src_name(i) = trim(adjustl(specifier(i)))
      end if

      fld_cnt = fld_cnt + 1

    end do count_cnst

    if (fld_cnt < 1) then
      nullify (fields)
      return
    end if

    !-----------------------------------------------------------------------
    !   ... allocate field type array
    !-----------------------------------------------------------------------
    allocate (fields(fld_cnt), stat=astat)
    if (astat /= 0) then
      write (iulog, *) 'specify_fields: failed to allocate fields array; error = ', astat
      call endrun('specify_fields: failed to allocate fields array')
    end if

    do i = 1, fld_cnt
      fields(i)%fldnam = fld_name(i)
      fields(i)%srcnam = src_name(i)
    end do

    deallocate (fld_name, src_name)

  end subroutine specify_fields

  ! This routine advances to the next file
  subroutine advance_file(file)
    type(trfile), intent(inout) :: file

    !-----------------------------------------------------------------------
    !   local variables
    !-----------------------------------------------------------------------
    character(len=shr_kind_cl) :: ctmp
    character(len=shr_kind_cl) :: loc_fname
    integer            :: istat, astat

    !-----------------------------------------------------------------------
    !   close current file ...
    !-----------------------------------------------------------------------
    call pio_closefile(file%curr_fileid)

    !-----------------------------------------------------------------------
    !   Advance the filename and file id
    !-----------------------------------------------------------------------
    file%curr_filename = file%next_filename
    file%curr_fileid = file%next_fileid

    !-----------------------------------------------------------------------
    !   Advance the curr_data_times
    !-----------------------------------------------------------------------
    deallocate (file%curr_data_times, stat=astat)
    if (astat /= 0) then
      write (iulog, *) 'advance_file: failed to deallocate file%curr_data_times array; error = ', astat
      call endrun('advance_file: failed to deallocate file%curr_data_times array')
    end if
    allocate (file%curr_data_times(size(file%next_data_times)), stat=astat)
    if (astat /= 0) then
      write (iulog, *) 'advance_file: failed to allocate file%curr_data_times array; error = ', astat
      call endrun('advance_file: failed to allocate file%curr_data_times array')
    end if
    file%curr_data_times(:) = file%next_data_times(:)

    !-----------------------------------------------------------------------
    !   delete information about next file (as was just assigned to current)
    !-----------------------------------------------------------------------
    file%next_filename = ''

    deallocate (file%next_data_times, stat=astat)
    if (astat /= 0) then
      write (iulog, *) 'advance_file: failed to deallocate file%next_data_times array; error = ', astat
      call endrun('advance_file: failed to deallocate file%next_data_times array')
    end if
    nullify (file%next_data_times)

  end subroutine advance_file

!------------------------------------------------------------------------------

  subroutine init_trc_restart(whence, piofile, tr_file)

    character(len=*), intent(in) :: whence
    type(file_desc_t), intent(inout) :: piofile
    type(trfile), intent(inout) :: tr_file

    character(len=32) :: name
    integer :: ioerr, mcdimid, maxlen
    integer :: err_handling

    ! Dimension should already be defined in restart file
    call pio_seterrorhandling(pioFile, PIO_BCAST_ERROR, oldmethod=err_handling)
    ioerr = pio_inq_dimid(pioFile, 'max_chars', mcdimid)
    call pio_seterrorhandling(pioFile, err_handling)
    ! but define it if nessasary
    if (ioerr /= PIO_NOERR) then
      ioerr = pio_def_dim(pioFile, 'max_chars', SHR_KIND_CL, mcdimid)
    end if

    if (len_trim(tr_file%curr_filename) > 1) then
      allocate (tr_file%currfnameid)
      name = trim(whence)//'_curr_fname'
      ioerr = pio_def_var(pioFile, name, pio_char, (/mcdimid/), tr_file%currfnameid)
      ioerr = pio_put_att(pioFile, tr_file%currfnameid, 'offset_time', tr_file%offset_time)
      maxlen = len_trim(tr_file%curr_filename)
      ioerr = pio_put_att(pioFile, tr_file%currfnameid, 'actual_len', maxlen)
    else
      nullify (tr_file%currfnameid)
    end if

    if (len_trim(tr_file%next_filename) > 1) then
      allocate (tr_file%nextfnameid)
      name = trim(whence)//'_next_fname'
      ioerr = pio_def_var(pioFile, name, pio_char, (/mcdimid/), tr_file%nextfnameid)
      maxlen = len_trim(tr_file%next_filename)
      ioerr = pio_put_att(pioFile, tr_file%nextfnameid, 'actual_len', maxlen)
    else
      nullify (tr_file%nextfnameid)
    end if
  end subroutine init_trc_restart

  ! writes file names to restart file
  subroutine write_trc_restart(piofile, tr_file)

    type(file_desc_t), intent(inout) :: piofile
    type(trfile), intent(inout) :: tr_file

    integer :: ioerr   ! error status
    if (associated(tr_file%currfnameid)) then
      ioerr = pio_put_var(pioFile, tr_file%currfnameid, tr_file%curr_filename)
      deallocate (tr_file%currfnameid)
      nullify (tr_file%currfnameid)
    end if
    if (associated(tr_file%nextfnameid)) then
      ioerr = pio_put_var(pioFile, tr_file%nextfnameid, tr_file%next_filename)
      deallocate (tr_file%nextfnameid)
      nullify (tr_file%nextfnameid)
    end if
  end subroutine write_trc_restart

  ! reads file names from restart file
  subroutine read_trc_restart(whence, piofile, tr_file)
    character(len=*), intent(in) :: whence
    type(file_desc_t), intent(inout) :: piofile
    type(trfile), intent(inout) :: tr_file
    type(var_desc_t) :: vdesc
    character(len=64) :: name
    integer :: ioerr   ! error status
    integer :: slen
    integer :: err_handling

    call PIO_SetErrorHandling(piofile, PIO_BCAST_ERROR, oldmethod=err_handling)
    name = trim(whence)//'_curr_fname'
    ioerr = pio_inq_varid(piofile, name, vdesc)
    if (ioerr == PIO_NOERR) then
      tr_file%curr_filename = ' '
      ioerr = pio_get_att(piofile, vdesc, 'offset_time', tr_file%offset_time)
      ioerr = pio_get_att(piofile, vdesc, 'actual_len', slen)
      ioerr = pio_get_var(piofile, vdesc, tr_file%curr_filename)
      if (slen < SHR_KIND_CL) tr_file%curr_filename(slen + 1:) = ' '
    end if

    name = trim(whence)//'_next_fname'
    ioerr = pio_inq_varid(piofile, name, vdesc)
    if (ioerr == PIO_NOERR) then
      tr_file%next_filename = ' '
      ioerr = pio_get_att(piofile, vdesc, 'actual_len', slen)
      ioerr = pio_get_var(piofile, vdesc, tr_file%next_filename)
      if (slen < SHR_KIND_CL) tr_file%next_filename(slen + 1:) = ' '
    end if
    call PIO_SetErrorHandling(piofile, err_handling)

  end subroutine read_trc_restart

  !------------------------------------------------------------------------------
  ! Various utility subroutines below:
  !------------------------------------------------------------------------------
  pure subroutine interpz_conserve(nsrc, ntrg, src_x, trg_x, src, trg)

    integer, intent(in)   :: nsrc                  ! dimension source array
    integer, intent(in)   :: ntrg                  ! dimension target array
    real(r8), intent(in)  :: src_x(nsrc + 1)         ! source coordinates
    real(r8), intent(in)  :: trg_x(ntrg + 1)         ! target coordinates
    real(r8), intent(in)  :: src(nsrc)             ! source array
    real(r8), intent(out) :: trg(ntrg)             ! target array

    !---------------------------------------------------------------
    !   ... local variables
    !---------------------------------------------------------------
    integer  :: i, j
    integer  :: sil
    real(r8) :: tl, y
    real(r8) :: bot, top

    do i = 1, ntrg
      tl = trg_x(i)
      if ((tl < src_x(nsrc + 1)) .and. (trg_x(i + 1) > src_x(1))) then
        do sil = 1, nsrc
          if ((tl - src_x(sil))*(tl - src_x(sil + 1)) <= 0.0_r8) then
            exit
          end if
        end do

        if (tl < src_x(1)) sil = 1

        y = 0.0_r8
        bot = max(tl, src_x(1))
        top = trg_x(i + 1)
        do j = sil, nsrc
          if (top > src_x(j + 1)) then
            y = y + (src_x(j + 1) - bot)*src(j)/(src_x(j + 1) - src_x(j))
            bot = src_x(j + 1)
          else
            y = y + (top - bot)*src(j)/(src_x(j + 1) - src_x(j))
            exit
          end if
        end do
        trg(i) = y
      else
        trg(i) = 0.0_r8
      end if
    end do

    if (trg_x(1) > src_x(1)) then
      top = trg_x(1)
      bot = src_x(1)
      y = 0.0_r8
      do j = 1, nsrc
        if (top > src_x(j + 1)) then
          y = y + (src_x(j + 1) - bot)*src(j)/(src_x(j + 1) - src_x(j))
          bot = src_x(j + 1)
        else
          y = y + (top - bot)*src(j)/(src_x(j + 1) - src_x(j))
          exit
        end if
      end do
      trg(1) = trg(1) + y
    end if

  end subroutine interpz_conserve

  subroutine vert_interp_mixrat(ncol, nsrc, ntrg, trg_x, src, trg, p0, ps, hyai, hybi, use_flight_distance)

    implicit none

    integer, intent(in)   :: ncol
    integer, intent(in)   :: nsrc                         ! dimension source array
    integer, intent(in)   :: ntrg                         ! dimension target array
    real(r8)              :: src_x(nsrc + 1)              ! source coordinates
    real(r8), intent(in)  :: trg_x(pcols, ntrg + 1)   ! target coordinates
    real(r8), intent(in)  :: src(pcols, nsrc)         ! source array
    logical,  intent(in)  :: use_flight_distance      ! .true. = flight distance, .false. = mixing ratio
    real(r8), intent(out) :: trg(pcols, ntrg)         ! target array

    real(r8) :: ps(pcols), p0, hyai(nsrc + 1), hybi(nsrc + 1)
    !---------------------------------------------------------------
    !   ... local variables
    !---------------------------------------------------------------
    integer  :: i, j, n
    real(r8)     :: y, trg_lo, trg_hi, src_lo, src_hi, overlap, outside

    do n = 1, ncol   ! loop over columns

      trg(n, :) = 0.0_r8   ! probably not needed

      ! calculate source pressure levels from hybrid coords
      do i = 1, nsrc + 1
        src_x(i) = p0*hyai(i) + ps(n)*hybi(i)
      end do

      ! Check the invariant that src_x and trg_x values are
      ! ascending.  This could also be checked at an earlier stage to
      ! avoid doing so for every interpolation call.
      if (.not. ALL(src_x(1:nsrc) < src_x(2:nsrc + 1))) then
        call endrun('src_x values are not ascending')
      end if
      if (.not. ALL(trg_x(n, 1:ntrg) < trg_x(n, 2:ntrg + 1))) then
        call endrun('trg_x values are not ascending')
      end if

      do i = 1, ntrg
        y = 0.0_r8

        trg_lo = trg_x(n, i)
        trg_hi = trg_x(n, i + 1)

        do j = 1, nsrc
          src_lo = src_x(j)
          src_hi = src_x(j + 1)

          overlap = min(src_hi, trg_hi) - max(src_lo, trg_lo)
          if (overlap > 0.0_r8) then
            if (use_flight_distance) then
              ! add input based on the overlap fraction
              y = y + src(n, j)*overlap/(src_hi - src_lo)
            else
              ! convert to mass by multiplying by dp
              y = y + src(n, j)*overlap
            end if
          end if
        end do
        trg(n, i) = y
      end do

      ! Handle source values outside the target range.  Since we want
      ! to preserve the total amount, add these to the first/last
      ! target bucket.
      trg_lo = trg_x(n, 1)
      y = 0.0_r8
      do j = 1, nsrc
        src_lo = src_x(j)
        src_hi = src_x(j + 1)

        if (src_lo < trg_lo) then
          if (src_hi <= trg_lo) then
            ! whole source interval is outside the target range
            outside = src_hi - src_lo
          else
            ! There was some overlap, which would have been added
            ! previously.  Only add the parts outside the target
            ! range.
            outside = trg_lo - src_lo
          end if
          if (use_flight_distance) then
            ! add the input scaled by the fraction outside
            y = y + src(n, j)*outside/(src_hi - src_lo)
          else
            ! convert to mass by multiplying by dp
            y = y + src(n, j)*outside
          end if
        else
          exit
        end if
      end do
      trg(n, 1) = trg(n, 1) + y

      trg_hi = trg_x(n, ntrg + 1)
      y = 0.0_r8
      do j = nsrc, 1, -1
        src_lo = src_x(j)
        src_hi = src_x(j + 1)

        if (src_hi > trg_hi) then
          if (src_lo >= trg_hi) then
            ! whole source interval is outside the target range
            outside = src_hi - src_lo
          else
            ! There was some overlap, which would have been added
            ! previously.  Only add the parts outside the target
            ! range.
            outside = src_hi - trg_hi
          end if
          if (use_flight_distance) then
            ! add the full input
            y = y + src(n, j)*outside/(src_hi - src_lo)
          else
            ! convert to mass by multiplying by dp
            y = y + src(n, j)*outside
          end if
        else
          exit
        end if
      end do
      trg(n, ntrg) = trg(n, ntrg) + y

      ! turn mass into mixing ratio
      if (.not. use_flight_distance) then
        do i = 1, ntrg
          trg(n, i) = trg(n, i)/(trg_x(n, i + 1) - trg_x(n, i))
        end do
      end if

    end do

  end subroutine vert_interp_mixrat

  ! Interpolate data from current time-interpolated values to model levels
  pure subroutine vert_interp(ncol, levsiz, pin, pmid, datain, dataout)
    integer,  intent(in)  :: ncol
    integer,  intent(in)  :: levsiz
    real(r8), intent(in)  :: pin(pcols, levsiz)
    real(r8), intent(in)  :: pmid(pcols, pver)
    real(r8), intent(in)  :: datain(pcols, levsiz)
    real(r8), intent(out) :: dataout(pcols, pver)

    ! local storage
    integer ::  i                   ! longitude index
    integer ::  k, kk, kkstart      ! level indices
    integer ::  kupper(pcols)       ! Level indices for interpolation
    real(r8) :: dpu                ! upper level pressure difference
    real(r8) :: dpl                ! lower level pressure difference

    !--------------------------------------------------------------------------
    !
    ! Initialize index array
    !
    do i = 1, ncol
      kupper(i) = 1
    end do

    do k = 1, pver
      !
      ! Top level we need to start looking is the top level for the previous k
      ! for all column points
      !
      kkstart = levsiz
      do i = 1, ncol
        kkstart = min0(kkstart, kupper(i))
      end do
      !
      ! Store level indices for interpolation
      !
      do kk = kkstart, levsiz - 1
        do i = 1, ncol
          if (pin(i, kk) < pmid(i, k) .and. pmid(i, k) <= pin(i, kk + 1)) then
            kupper(i) = kk
          end if
        end do
      end do
      ! interpolate or extrapolate...
      do i = 1, ncol
        if (pmid(i, k) < pin(i, 1)) then
          dataout(i, k) = datain(i, 1)*pmid(i, k)/pin(i, 1)
        else if (pmid(i, k) > pin(i, levsiz)) then
          dataout(i, k) = datain(i, levsiz)
        else
          dpu = pmid(i, k) - pin(i, kupper(i))
          dpl = pin(i, kupper(i) + 1) - pmid(i, k)
          dataout(i, k) = (datain(i, kupper(i))*dpl + &
                           datain(i, kupper(i) + 1)*dpu)/(dpl + dpu)
        end if
      end do
    end do

  end subroutine vert_interp

  ! Interpolate data from current time-interpolated values to top interface pressure
  pure subroutine vert_interp_ub(ncol, nlevs, plevs, datain, dataout)
    use ref_pres, only: ptop_ref

    integer,  intent(in)  :: ncol
    integer,  intent(in)  :: nlevs
    real(r8), intent(in)  :: plevs(nlevs)
    real(r8), intent(in)  :: datain(ncol, nlevs)
    real(r8), intent(out) :: dataout(ncol)

    !
    ! local variables
    !
    integer  :: i, ku, kl, kk
    real(r8) :: pinterp, delp

    pinterp = ptop_ref

    if (pinterp <= plevs(1)) then
      kl = 1
      ku = 1
      delp = 0._r8
    else if (pinterp >= plevs(nlevs)) then
      kl = nlevs
      ku = nlevs
      delp = 0._r8
    else

      do kk = 2, nlevs
        if (pinterp <= plevs(kk)) then
          ku = kk
          kl = kk - 1
          delp = log(pinterp/plevs(kk))/log(plevs(kk - 1)/plevs(kk))
          exit
        end if
      end do

    end if

    do i = 1, ncol
      dataout(i) = datain(i, kl) + delp*(datain(i, ku) - datain(i, kl))
    end do

  end subroutine vert_interp_ub

  ! Interpolate data from current time-interpolated values to press
  pure subroutine vert_interp_ub_var(ncol, nlevs, plevs, press, datain, dataout)

    integer, intent(in)  :: ncol
    integer, intent(in)  :: nlevs
    real(r8), intent(in)  :: plevs(nlevs)
    real(r8), intent(in)  :: press(ncol)
    real(r8), intent(in)  :: datain(ncol, nlevs)
    real(r8), intent(out) :: dataout(ncol)

    !
    ! local variables
    !
    integer  :: i, k
    integer  :: ku, kl
    real(r8) :: delp

    do i = 1, ncol

      if (press(i) <= plevs(1)) then
        kl = 1
        ku = 1
        delp = 0._r8
      else if (press(i) >= plevs(nlevs)) then
        kl = nlevs
        ku = nlevs
        delp = 0._r8
      else

        do k = 2, nlevs
          if (press(i) <= plevs(k)) then
            ku = k
            kl = k - 1
            delp = log(press(i)/plevs(k))/log(plevs(k - 1)/plevs(k))
            exit
          end if
        end do

      end if

      dataout(i) = datain(i, kl) + delp*(datain(i, ku) - datain(i, kl))
    end do

  end subroutine vert_interp_ub_var

  ! Purpose: "find periodic lower bound"
  ! Search the input array for the lower bound of the interval that
  ! contains the input value.  The returned index satifies:
  ! x(index) .le. xval .lt. x(index+1)
  ! Assume the array represents values in one cycle of a periodic coordinate.
  ! So, if xval .lt. x(1), or xval .ge. x(nx), then the index returned is nx.
  !
  ! Author: B. Eaton
  pure subroutine findplb(x, nx, xval, index)
    integer, intent(in) ::   nx         ! size of x
    real(r8), intent(in) ::  x(nx)      ! strictly increasing array
    real(r8), intent(in) ::  xval       ! value to be searched for in x

    integer, intent(out) ::  index

    ! Local variables:
    integer i
    !-----------------------------------------------------------------------

    if (xval .lt. x(1) .or. xval .ge. x(nx)) then
      index = nx
      return
    end if

    do i = 2, nx
      if (xval .lt. x(i)) then
        index = i - 1
        return
      end if
    end do

  end subroutine findplb

  ! rebin src (source) to trg (target).
  ! originally from mo_util
  pure subroutine rebin(nsrc, ntrg, src_x, trg_x, src, trg)
    !---------------------------------------------------------------
    ! ... dummy arguments
    !---------------------------------------------------------------
    integer,  intent(in)   :: nsrc                  ! dimension source array
    integer,  intent(in)   :: ntrg                  ! dimension target array
    real(r8), intent(in)   :: src_x(nsrc + 1)       ! source coordinates
    real(r8), intent(in)   :: trg_x(ntrg + 1)       ! target coordinates
    real(r8), intent(in)   :: src(nsrc)             ! source array
    real(r8), intent(out)  :: trg(ntrg)             ! target array

    !---------------------------------------------------------------
    ! ... local variables
    !---------------------------------------------------------------
    integer  :: i
    integer  :: si, si1
    integer  :: sil, siu
    real(r8) :: y
    real(r8) :: sl, su
    real(r8) :: tl, tu

    !---------------------------------------------------------------
    ! ... check interval overlap
    !---------------------------------------------------------------
    !     if( trg_x(1) < src_x(1) .or. trg_x(ntrg+1) > src_x(nsrc+1) ) then
    !        write(iulog,*) 'rebin: target grid is outside source grid'
    !        write(iulog,*) '       target grid from ',trg_x(1),' to ',trg_x(ntrg+1)
    !        write(iulog,*) '       source grid from ',src_x(1),' to ',src_x(nsrc+1)
    !        call endrun
    !     end if

    do i = 1, ntrg
      tl = trg_x(i)
      if (tl < src_x(nsrc + 1)) then
        do sil = 1, nsrc + 1
          if (tl <= src_x(sil)) then
            exit
          end if
        end do
        tu = trg_x(i + 1)
        do siu = 1, nsrc + 1
          if (tu <= src_x(siu)) then
            exit
          end if
        end do
        y = 0._r8
        sil = max(sil, 2)
        siu = min(siu, nsrc + 1)
        do si = sil, siu
          si1 = si - 1
          sl = max(tl, src_x(si1))
          su = min(tu, src_x(si))
          y = y + (su - sl)*src(si1)
        end do
        trg(i) = y/(trg_x(i + 1) - trg_x(i))
      else
        trg(i) = 0._r8
      end if
    end do

  end subroutine rebin

end module tracer_data
