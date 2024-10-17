!------------------------------------------------------------------------------
!   time_coordinate -- manages the time coordinate of input data sets
!------------------------------------------------------------------------------
module cam_time_coord
   use shr_kind_mod,   only: r8=>shr_kind_r8, cl=>shr_kind_cl, cs=>shr_kind_cs
   use cam_abortutils, only: endrun
   use cam_logfile,    only: iulog
   use pio,            only: file_desc_t, pio_inq_dimid, pio_inq_dimlen
   use pio,            only: pio_get_att, pio_seterrorhandling, pio_get_var
   use pio,            only: pio_inq_varid, PIO_NOWRITE, PIO_BCAST_ERROR
   use pio,            only: PIO_INTERNAL_ERROR, PIO_NOERR
   use time_manager,   only: timemgr_get_calendar_cf, set_time_float_from_date
   use time_manager,   only: get_curr_date
   use spmd_utils,     only: masterproc

   implicit none
   private

   type, public :: time_coordinate
      integer               :: ntimes
      real(r8)              :: wghts(2) = -HUGE(1.0_r8)
      integer               :: indxs(2) = -HUGE(1)
      real(r8), allocatable :: times(:)
      real(r8), allocatable :: time_bnds(:,:)
      logical               :: time_interp = .true.
      logical               :: fixed = .false.
      integer               :: fixed_ymd = -HUGE(1)
      integer               :: fixed_tod = -HUGE(1)
      real(r8)              :: dtime ! time shift in interpolation point (days)
      character(len=:), allocatable :: filename
   contains
      procedure :: initialize
      procedure :: advance
      procedure :: read_more
      procedure :: copy
      procedure :: destroy
   end type time_coordinate

CONTAINS

   !---------------------------------------------------------------------------
   ! initializer
   !---------------------------------------------------------------------------
   subroutine initialize(this, filepath, fixed, fixed_ymd, fixed_tod,         &
        force_time_interp, set_weights, try_dates, delta_days)
      use shr_string_mod, only: to_upper => shr_string_toUpper
      use cam_abortutils, only: check_allocate
      use ioFileMod,      only: cam_get_file
      use cam_pio_utils,  only: cam_pio_openfile, cam_pio_closefile
      use cam_pio_utils,  only: cam_pio_handle_error

      class(time_coordinate), intent(inout) :: this
      character(len=*),       intent(in)    :: filepath
      logical,  optional,     intent(in)    :: fixed
      integer,  optional,     intent(in)    :: fixed_ymd
      integer,  optional,     intent(in)    :: fixed_tod
      logical,  optional,     intent(in)    :: force_time_interp
      logical,  optional,     intent(in)    :: set_weights
      logical,  optional,     intent(in)    :: try_dates
      ! delta_days is a time shift in interpolation point (days)
      !            for previous day set delta_days to -1.
      real(r8), optional,     intent(in)    :: delta_days

      character(len=cl) :: filen
      character(len=cl) :: time_units, err_str
      character(len=cs) :: time_calendar, model_calendar
      character(len=4)  :: yr_str
      character(len=2)  :: mon_str, day_str, hr_str, min_str, sec_str
      integer           :: ref_yr, ref_mon, ref_day, ref_hr
      integer           :: ref_min, ref_sec, tod
      integer           :: varid, ierr
      real(r8)          :: ref_time

      integer,  allocatable       :: dates(:)
      integer,  allocatable       :: datesecs(:)
      real(r8), allocatable       :: times_file(:)
      real(r8), allocatable       :: times_modl(:)
      real(r8), allocatable       :: time_bnds_file(:,:)
      type(file_desc_t)           :: fileid
      logical                     :: force_interp
      logical                     :: set_wghts
      logical                     :: use_time, adj_times, use_time_bnds
      integer                     :: index
      integer                     :: err_handling
      character(len=*), parameter :: subname = 'time_coordinate%initialize: '

      if (present(fixed)) then
         this%fixed = fixed
      end if
      if (present(fixed_ymd)) then
         this%fixed_ymd = fixed_ymd
      end if
      if (present(fixed_tod)) then
         this%fixed_tod = fixed_tod
      end if

      if (present(delta_days)) then
         this%dtime = delta_days
      else
         this%dtime = 0._r8
      end if

      if (present(force_time_interp)) then
         force_interp = force_time_interp
      else
         force_interp = .false.
      end if

      if (present(set_weights)) then
         set_wghts = set_weights
      else
         set_wghts = .true.
      end if

      this%filename = trim(filepath)

      call cam_get_file(filepath, filen, allow_fail=.false.)
      call cam_pio_openfile(fileid, filen, PIO_NOWRITE)

      call pio_seterrorhandling(fileid, PIO_BCAST_ERROR, oldmethod=err_handling)

      call get_dimension(fileid, 'time', this%ntimes)
      allocate(times_file(this%ntimes), stat=ierr)
      call check_allocate(ierr, subname, 'times_file')
      allocate(times_modl(this%ntimes), stat=ierr)
      call check_allocate(ierr, subname, 'times_modl')
      allocate(this%times(this%ntimes), stat=ierr)
      call check_allocate(ierr, subname, 'this%times')

      ierr =  pio_inq_varid(fileid, 'time', varid)
      use_time = ierr == PIO_NOERR
      ierr = pio_get_att(fileid, varid, 'calendar', time_calendar)
      use_time = (ierr == PIO_NOERR) .and. use_time
      ierr = pio_get_att(fileid, varid, 'units', time_units)
      use_time = (ierr == PIO_NOERR) .and. use_time
      if (use_time) then
         use_time = time_units(1:10) == 'days since'
      end if

      if (present(try_dates)) then
         if (try_dates) then
            ierr = pio_inq_varid(fileid, 'date', varid )
            use_time = ierr /= PIO_NOERR
         end if
      end if

      adj_times = .false.
      use_time_bnds =  .false.

      if (use_time) then
         ! check the calendar attribute - must match model calendar
         model_calendar = timemgr_get_calendar_cf()

         if (this%ntimes>2) then
            ! If only 2 time records then it is assumed that the input has 2
            !   identical time records -- climatological or solar-cycle avaraged

            adj_times = (to_upper(time_calendar(1:6)) /=                      &
                 to_upper(model_calendar(1:6)))

            if (adj_times .and. masterproc) then
               write(iulog,*) 'time_coordinate%initialize: model calendar ',  &
                    trim(model_calendar),                                     &
                    ' does not match input data calendar ', trim(time_calendar)
               write(iulog,*) ' -- will try to use date and datesec in the ', &
                    'input file to adjust the time coordinate.'
            end if
         end if

         ! parse out ref date and time
         !  time:units = "days since YYYY-MM-DD hh:mm:ss" ;

         yr_str  = time_units(12:15)
         mon_str = time_units(17:18)
         day_str = time_units(20:21)
         hr_str  = time_units(23:24)
         min_str = time_units(26:27)

         read(yr_str,  *) ref_yr
         read(mon_str, *) ref_mon
         read(day_str, *) ref_day
         read(hr_str,  *) ref_hr
         read(min_str, *) ref_min
         if (len_trim(time_units) >= 30) then
            sec_str = time_units(29:30)
            read(sec_str, *) ref_sec
         else
            ref_sec = 0
         end if

         tod = ref_hr*3600 + ref_min*60 + ref_sec
         call set_time_float_from_date(ref_time, ref_yr, ref_mon, ref_day, tod)

         ierr = pio_get_var(fileid, varid, times_file)
         if (ierr /= PIO_NOERR) then
            call endrun(subname//'not able to read times')
         end if

         times_file = times_file + ref_time

         ierr =  pio_inq_varid(fileid, 'time_bnds', varid)

         use_time_bnds = ((ierr == PIO_NOERR) .and. (.not. force_interp))

         if (use_time_bnds) then
            allocate(this%time_bnds(2, this%ntimes), stat=ierr)
            call check_allocate(ierr, subname, 'this%time_bnds')
            allocate(time_bnds_file(2, this%ntimes), stat=ierr)
            call check_allocate(ierr, subname, 'time_bnds_file')
            ierr =  pio_get_var(fileid, varid, time_bnds_file)
            call cam_pio_handle_error(ierr, subname//': Error with get_var time_bnds_file')
            time_bnds_file = time_bnds_file + ref_time
            this%time_interp = .false.
            do index = 1,this%ntimes
               if (.not. (time_bnds_file(1,index) < times_file(index)         &
                    .and. time_bnds_file(2,index) > times_file(index))) then
                  write(err_str, '(a,i0,2a)')                                 &
                       'incorrect time_bnds -- time index: ', index,          &
                       ' file: ', trim(filepath)
                  call endrun(subname//trim(err_str))
               end if
            end do
         else
            this%time_interp = .true.
         end if
      else
         this%time_interp = .true.
      end if

      if (adj_times .or. (.not. use_time)) then

         ! try using date and datesec
         allocate(dates(this%ntimes), stat=ierr)
         call check_allocate(ierr, subname, 'dates')

         allocate(datesecs(this%ntimes), stat=ierr)
         call check_allocate(ierr, subname, 'datesecs')

         ierr = pio_inq_varid(fileid, 'date', varid)
         if (ierr /= PIO_NOERR) then
            call endrun(subname//'input file must contain time or date '//    &
                 'variable '//trim(filepath))
         end if
         ierr = pio_get_var(fileid, varid, dates)
         call cam_pio_handle_error(ierr, subname//': Error with get_var date')
         ierr = pio_inq_varid(fileid, 'datesec', varid)
         if (ierr == PIO_NOERR) then
            ierr = pio_get_var(fileid, varid, datesecs)
            call cam_pio_handle_error(ierr, subname//': Error with get_var datesecs')
         else
            datesecs(:) = 0
         end if

         call convert_dates(dates, datesecs, times_modl)

         deallocate(dates, datesecs)

      end if

      if (adj_times) then
         ! time_bnds_modl - time_bnds_file = times_modl - times_file
         ! time_bnds_modl = time_bnds_file + times_modl - times_file
         this%times(:) = times_modl(:)
         if (use_time_bnds) then
            this%time_bnds(1,:) = time_bnds_file(1,:) + times_modl(:) -       &
                 times_file(:)
            this%time_bnds(2,:) = time_bnds_file(2,:) + times_modl(:) -       &
                 times_file(:)
         end if
      else if (use_time) then
         this%times(:) = times_file(:)
         if (use_time_bnds) then
            this%time_bnds(1,:) = time_bnds_file(1,:)
            this%time_bnds(2,:) = time_bnds_file(2,:)
         end if
      else
         this%times(:) = times_modl(:)
      end if

      deallocate(times_modl, times_file)
      if (use_time_bnds) deallocate(time_bnds_file)

      call pio_seterrorhandling(fileid, err_handling)

      call cam_pio_closefile(fileid)

      this%indxs(1) = 1
      if (set_wghts) then
         call set_wghts_indices(this)
      end if

   end subroutine initialize

   !---------------------------------------------------------------------------
   ! advance the time coordinate
   !---------------------------------------------------------------------------
   subroutine advance(this)
      class(time_coordinate) :: this

      if (.not. this%fixed) then
         call set_wghts_indices(this)
      end if

   end subroutine advance

   !---------------------------------------------------------------------------
   ! determine if need to read more data from input data set
   !---------------------------------------------------------------------------
   logical function read_more(this) result(check)
      class(time_coordinate), intent(in) :: this

      real(r8) :: model_time

      model_time = get_model_time() + this%dtime

      if (.not. this%fixed) then
         if (allocated(this%time_bnds)) then
            check = model_time > this%time_bnds(2,this%indxs(1))
         else
            check = model_time > this%times(this%indxs(2))
         end if
      else
         check = .false.
      end if

   end function read_more

   !---------------------------------------------------------------------------
   ! destroy method -- deallocate memory and revert to default settings
   !---------------------------------------------------------------------------
   subroutine destroy(this)
      class(time_coordinate), intent(inout) :: this

      if (allocated(this%times)) then
         deallocate(this%times)
      end if
      if (allocated(this%time_bnds)) then
         deallocate(this%time_bnds)
      end if
      this%ntimes = 0
      this%filename = 'NONE'
      this%time_interp = .true.
      this%fixed = .false.

   end subroutine destroy

   !---------------------------------------------------------------------------
   ! produce a duplicate time coordinate object
   !---------------------------------------------------------------------------
   subroutine copy(this, obj)
      use cam_abortutils, only: check_allocate

      class(time_coordinate), intent(inout) :: this
      class(time_coordinate), intent(in) :: obj

      integer :: ierr

      call this%destroy()

      this%ntimes = obj%ntimes
      this%fixed  = obj%fixed
      this%fixed_ymd = obj%fixed_ymd
      this%fixed_tod = obj%fixed_tod

      allocate (this%times(this%ntimes), stat=ierr)
      call check_allocate(ierr, 'copy', 'this%times', file=__FILE__, line=__LINE__)
      this%times = obj%times

      if (allocated(obj%time_bnds)) then
         allocate(this%time_bnds(2, this%ntimes), stat=ierr)
         call check_allocate(ierr, 'copy', 'this%time_bnds',                  &
              file=__FILE__, line=__LINE__)
         this%time_bnds = obj%time_bnds
      end if
      this%filename = obj%filename

   end subroutine copy

   ! private methods

   !-----------------------------------------------------------------------
   ! set time interpolation weights
   !-----------------------------------------------------------------------
   subroutine set_wghts_indices(obj)

      class(time_coordinate), intent(inout) :: obj

      real(r8)          :: model_time
      real(r8)          :: datatm, datatp
      integer           :: yr, mon, day
      integer           :: index, ind
      character(len=cl) :: errmsg
      character(len=*), parameter :: subname = 'time_coordinate:set_wghts_indices: '

      ! set time indices and time-interpolation weights
      if (obj%fixed) then
         yr = obj%fixed_ymd/10000
         mon = (obj%fixed_ymd-yr*10000) / 100
         day = obj%fixed_ymd-yr*10000-mon*100
         call set_time_float_from_date(model_time, yr, mon, day, obj%fixed_tod)
         model_time = model_time + obj%dtime
      else
         model_time = get_model_time() + obj%dtime
      end if

      index = -1

      do ind = obj%indxs(1), obj%ntimes
         if (allocated(obj%time_bnds)) then
            datatm = obj%time_bnds(1,ind)
            datatp = obj%time_bnds(2,ind)
         else
            if (ind >= obj%ntimes) then
               errmsg = subname//'cannot not find model time in: '//          &
                    trim(obj%filename)
               write(iulog, *) trim(errmsg)
               call endrun(trim(errmsg))
            end if
            datatm = obj%times(ind)
            datatp = obj%times(ind+1)
         end if
         if (model_time < datatm) then
            errmsg = subname//'cannot find model time in: '//trim(obj%filename)
            write(iulog, *) trim(errmsg)
            call endrun(trim(errmsg))
         end if
         if ((model_time >= datatm) .and. (model_time <= datatp)) then
            index = ind
            obj%indxs(1) = ind
            obj%indxs(2) = ind + 1
            exit
         end if
      end do

      if (allocated(obj%time_bnds) .and. (ind < obj%ntimes)) then
         if (.not. (obj%time_bnds(1,ind+1) > obj%time_bnds(1,ind))) then
            obj%indxs = obj%indxs+1  ! skip 29 Feb when calendar is noleap
         end if
      end if

      if (.not. ((index > 0) .and. (index <= obj%ntimes))) then
         errmsg = subname//'cannot not find time indices for input file: '//  &
              trim(obj%filename)
         write(iulog,*) trim(errmsg)
         call endrun(trim(errmsg))
      end if

      if (obj%time_interp) then
         obj%wghts(2) = (model_time - obj%times(index)) /                     &
              (obj%times(index+1) - obj%times(index))
         obj%wghts(1) = 1._r8 - obj%wghts(2)
      else
         obj%wghts(1) = 1._r8
         obj%wghts(2) = 0._r8
      end if

   end subroutine set_wghts_indices

   !-----------------------------------------------------------------------
   ! returns dimension size
   !-----------------------------------------------------------------------
   subroutine get_dimension(fid, dname, dsize)
      use cam_pio_utils, only: cam_pio_handle_error

      type(file_desc_t), intent(in)  :: fid
      character(*),      intent(in)  :: dname
      integer,           intent(out) :: dsize

      integer :: dimid, ierr
      character(len=*), parameter :: subname = 'time_coord:get_dimension'

      ierr = pio_inq_dimid(fid, dname, dimid)
      call cam_pio_handle_error(ierr, subname//': Error with inq_dimid dname')
      ierr = pio_inq_dimlen(fid, dimid, dsize)
      call cam_pio_handle_error(ierr, subname//': Error with inq_dimlen dname')

   end subroutine get_dimension

   !-----------------------------------------------------------------------
   ! returns a real which represents the current model time
   !-----------------------------------------------------------------------
   real(r8) function get_model_time() result(time)

      integer yr, mon, day, ncsec  ! components of a date

      call get_curr_date(yr, mon, day, ncsec)

      call set_time_float_from_date(time, yr, mon, day, ncsec)

   end function get_model_time

   !---------------------------------------------------------------------------
   ! convert a collection of dates and times to reals
   !---------------------------------------------------------------------------
   subroutine convert_dates(dates, secs, times)

      integer,  intent(in)  :: dates(:)
      integer,  intent(in)  :: secs(:)
      real(r8), intent(out) :: times(:)

      integer :: year, month, day, sec, ndates, index

      ndates = size(dates)

      do index = 1, ndates
         year = dates(index) / 10000
         month = (dates(index) - (year*10000)) / 100
         day = dates(index) - (year*10000) - (month*100)
         sec = secs(index)
         call set_time_float_from_date(times(index), year, month, day, sec)
      end do

   end subroutine convert_dates

end module cam_time_coord
