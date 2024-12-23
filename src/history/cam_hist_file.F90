module cam_hist_file
   ! Module to define and read CAM history configuration namelist entries
   !    and associated history files

   use ISO_FORTRAN_ENV,     only: REAL64
   use pio,                 only: file_desc_t, var_desc_t
   use cam_history_support, only: max_fldlen=>max_fieldname_len
   use cam_history_support, only: fieldname_suffix_len
   use cam_history_support, only: interp_info_t
   use shr_kind_mod,        only: r8 => shr_kind_r8, CL => SHR_KIND_CL
   use hist_field,          only: hist_field_info_t
   use hist_hash_table,     only: hist_hash_table_t
   use cam_grid_support,    only: max_split_files
   use runtime_obj,         only: UNSET_I => unset_int
   use runtime_obj,         only: UNSET_C => unset_str
   use runtime_obj,         only: UNSET_R8 => unset_real

   implicit none
   private

   public :: hist_file_t
   public :: hist_read_namelist_config
   public :: AvgflagToString
   public :: strip_suffix

   character(len=*), parameter :: hist_nl_group_name = 'hist_file_config_nl'
   integer,          parameter :: nl_gname_len = len(hist_nl_group_name)
   integer, public,  parameter :: instantaneous_file_index = 1
   integer, public,  parameter :: accumulated_file_index   = 2
   character(len=fieldname_suffix_len ) :: fieldname_suffix = '&IC' ! Suffix appended to field names for IC file

   logical, parameter                      :: PATCH_DEF = .true.
   integer, parameter                      :: OUTPUT_DEF = REAL64
   integer, parameter                      :: vlen = 8
   integer, parameter                      :: flen = 16
   integer, parameter                      :: tlen = 16

   integer, parameter                      :: hfile_type_default    = -1
   integer, parameter                      :: hfile_type_history    =  1
   integer, parameter                      :: hfile_type_init_value =  2
   integer, parameter                      :: hfile_type_sat_track  =  3
   integer, parameter                      :: hfile_type_restart    =  4

   type :: hist_file_t
      ! History file configuration information
      character(len=vlen),           private :: volume = UNSET_C
      type(file_desc_t),             private :: hist_files(max_split_files) ! PIO file ids
      integer,                       private :: rl_kind = OUTPUT_DEF
      integer,                       private :: max_frames = UNSET_I
      integer,                       private :: output_freq_mult = UNSET_I
      character(len=8),              private :: output_freq_type = UNSET_C
      integer,                       private :: num_samples = 0
      real(r8),                      private :: beg_time = UNSET_R8
      real(r8),                      private :: end_time = UNSET_R8
      character(len=:), allocatable, private :: filename_spec
      character(len=max_fldlen), allocatable, private :: field_names(:)
      character(len=3), allocatable, private :: accumulate_types(:)
      type(var_desc_t), allocatable, private :: file_varids(:,:)
      integer, allocatable,          private :: grids(:)
      integer,                       private :: hfile_type = hfile_type_default
      logical,                       private :: collect_patch_output = PATCH_DEF
      logical,                       private :: has_instantaneous = .false.
      logical,                       private :: has_accumulated = .false.
      logical,                       private :: write_nstep0 = .false.
      integer,                       private :: last_month_written
      integer,                       private :: last_year_written
      logical,                       private :: files_open = .false.
      type(interp_info_t), pointer,  private :: interp_info => NULL()
      character(len=CL), allocatable, private :: file_names(:)
      ! PIO IDs
      type(var_desc_t),              private :: timeid
      type(var_desc_t),              private :: dateid
      type(var_desc_t),              private :: bdateid
      type(var_desc_t),              private :: datesecid
      type(var_desc_t),              private :: tbndid
      type(var_desc_t),              private :: date_writtenid
      type(var_desc_t),              private :: time_writtenid
      type(var_desc_t),              private :: ndbaseid
      type(var_desc_t),              private :: nsbaseid
      type(var_desc_t),              private :: nbdateid
      type(var_desc_t),              private :: nbsecid
      type(var_desc_t),              private :: mdtid
      type(var_desc_t),              private :: ndcurid
      type(var_desc_t),              private :: nscurid
      type(var_desc_t),              private :: tsecid
      type(var_desc_t),              private :: nstephid


      ! Field list
      type(hist_field_info_t), allocatable, private :: field_list(:)
      type(hist_hash_table_t),          private :: field_list_hash_table
   contains
      ! Accessors
      procedure :: filename => config_filename
      procedure :: get_volume => config_volume
      procedure :: get_filenames => config_get_filenames
      procedure :: get_filename_spec => config_get_filename_spec
      procedure :: get_last_month_written => config_get_last_month_written
      procedure :: get_last_year_written => config_get_last_year_written
      procedure :: precision => config_precision
      procedure :: max_frame => config_max_frame
      procedure :: get_num_samples => config_get_num_samples
      procedure :: get_beg_time => config_get_beg_time
      procedure :: output_freq => config_output_freq
      procedure :: output_freq_separate => config_output_freq_separate
      procedure :: is_history_file => config_history_file
      procedure :: is_initial_value_file => config_init_value_file
      procedure :: is_satellite_file => config_satellite_file
      procedure :: is_hist_restart_file => config_restart_file
      procedure :: do_write_nstep0 => config_do_write_nstep0
      procedure :: file_is_setup => config_file_is_setup
      procedure :: are_files_open => config_files_open
      ! Actions
      procedure :: reset        => config_reset
      procedure :: configure    => config_configure
      procedure :: print_config => config_print_config
      procedure :: set_beg_time => config_set_beg_time
      procedure :: set_end_time => config_set_end_time
      procedure :: set_filenames => config_set_filenames
      procedure :: set_last_month_written => config_set_last_month_written
      procedure :: set_last_year_written => config_set_last_year_written
      procedure :: set_up_fields => config_set_up_fields
      procedure :: find_in_field_list => config_find_in_field_list
      procedure :: define_file => config_define_file
      procedure :: write_time_dependent_variables => config_write_time_dependent_variables
      procedure :: write_field => config_write_field
      procedure :: close_files => config_close_files
      procedure :: clear_buffers => config_clear_buffers
   end type hist_file_t

   private :: count_array         ! Number of non-blank strings in array
   private :: read_namelist_entry ! Read a namelist group and create config

CONTAINS

   ! ========================================================================

   function config_filename(this) result(cfiles)
      use cam_filenames,  only: interpret_filename_spec
      use cam_abortutils, only: check_allocate
      ! Dummy arguments
      class(hist_file_t), intent(in) :: this
      character(len=CL), allocatable :: cfiles(:)

      character(len=1) :: accum_types(max_split_files)
      integer :: file_idx, ierr
      character(len=*), parameter :: subname = 'config_filename: '

      accum_types(instantaneous_file_index) = 'i'
      accum_types(accumulated_file_index)   = 'a'
      allocate(cfiles(max_split_files), stat=ierr)
      call check_allocate(ierr, subname, 'cfiles',             &
           file=__FILE__, line=__LINE__-1)

      do file_idx = 1, size(cfiles)
         cfiles(file_idx) = interpret_filename_spec(this%filename_spec, &
           unit=this%volume, accum_type=accum_types(file_idx),          &
           incomplete_ok=.false.)
      end do

   end function config_filename

   ! ========================================================================

   subroutine config_set_filenames(this)
      use cam_filenames,  only: interpret_filename_spec
      use cam_abortutils, only: check_allocate
      ! Dummy argument
      class(hist_file_t), intent(inout) :: this
      
      character(len=1) :: accum_types(max_split_files)
      integer          :: file_idx, ierr
      character(len=*), parameter :: subname = 'config_set_filenames: '

      if (allocated(this%file_names)) then
         return
      end if
      accum_types(instantaneous_file_index) = 'i'
      accum_types(accumulated_file_index)   = 'a'
      allocate(this%file_names(max_split_files), stat=ierr)
      call check_allocate(ierr, subname, 'this%file_names',   &
           file=__FILE__, line=__LINE__-1)
      do file_idx = 1, size(this%file_names)
         this%file_names(file_idx) = interpret_filename_spec(this%filename_spec, &
           unit=this%volume, accum_type=accum_types(file_idx),          &
           incomplete_ok=.false.)
      end do

   end subroutine config_set_filenames

   ! ========================================================================

   subroutine config_set_last_month_written(this, last_month_written)
      ! Dummy arguments
      class(hist_file_t), intent(inout) :: this
      integer,               intent(in) :: last_month_written

      this%last_month_written = last_month_written

   end subroutine config_set_last_month_written

   ! ========================================================================

   subroutine config_set_last_year_written(this, last_year_written)
      ! Dummy arguments
      class(hist_file_t), intent(inout) :: this
      integer,               intent(in) :: last_year_written

      this%last_year_written = last_year_written

   end subroutine config_set_last_year_written

   ! ========================================================================

   pure function config_get_filenames(this) result(cfiles)
      ! Dummy arguments
      class(hist_file_t), intent(in) :: this
      character(len=CL)              :: cfiles(max_split_files)

      cfiles = this%file_names

   end function config_get_filenames

   ! ========================================================================

   pure function config_get_filename_spec(this) result(filename_spec)
      ! Dummy arguments
      class(hist_file_t), intent(in) :: this
      character(len=:), allocatable  :: filename_spec

      filename_spec = this%filename_spec

   end function config_get_filename_spec

   ! ========================================================================

   pure function config_get_last_month_written(this) result(last_month_written)
      ! Dummy arguments
      class(hist_file_t), intent(in) :: this
      integer                        :: last_month_written

      last_month_written = this%last_month_written

   end function config_get_last_month_written

   ! ========================================================================

   pure function config_get_last_year_written(this) result(last_year_written)
      ! Dummy arguments
      class(hist_file_t), intent(in) :: this
      integer                        :: last_year_written

      last_year_written = this%last_year_written

   end function config_get_last_year_written

   ! ========================================================================

   pure function config_precision(this) result(cprec)
      use ISO_FORTRAN_ENV,     only: REAL32
      ! Dummy arguments
      class(hist_file_t), intent(in) :: this
      character(len=vlen)                   :: cprec

      if (this%rl_kind == REAL32) then
         cprec = "REAL32"
      else if (this%rl_kind == REAL64) then
         cprec = "REAL64"
      else
         write(cprec, '(i0)') this%rl_kind
      end if
   end function config_precision

   ! ========================================================================

   pure function config_volume(this) result(cvol)
      ! Dummy arguments
      class(hist_file_t), intent(in) :: this
      character(len=vlen)            :: cvol

      cvol = this%volume

   end function config_volume

   ! ========================================================================

   pure function config_max_frame(this) result(max_frame)
      ! Dummy arguments
      class(hist_file_t), intent(in) :: this
      integer                        :: max_frame

      max_frame = this%max_frames
   end function config_max_frame

   ! ========================================================================

   pure function config_get_num_samples(this) result(num_samples)
      ! Dummy arguments
      class(hist_file_t), intent(in) :: this
      integer                        :: num_samples

      num_samples = this%num_samples
   end function config_get_num_samples

   ! ========================================================================

   pure function config_get_beg_time(this) result(beg_time)
      ! Dummy arguments
      class(hist_file_t), intent(in) :: this
      real(r8)                       :: beg_time

      beg_time = this%beg_time
   end function config_get_beg_time

   ! ========================================================================

   function config_output_freq(this) result(out_freq)
      use shr_kind_mod,   only: CS => SHR_KIND_CS
      use shr_string_mod, only: to_lower => shr_string_toLower
      ! Dummy arguments
      class(hist_file_t), intent(in) :: this
      character(len=CL)                     :: out_freq
      ! Local variable
      character(len=CS) :: out_opt
      character(len=1)  :: plural

      select case(to_lower(trim(this%output_freq_type)))
      case ("step")
         out_opt = "time step"
      case default
         out_opt = trim(this%output_freq_type)
      end select
      if (this%output_freq_mult > 1) then
         plural = "s"
      else
         plural = ""
      end if
      write(out_freq, '(i0,1x,2a)') this%output_freq_mult, trim(out_opt), plural

   end function config_output_freq

   ! ========================================================================

   subroutine config_output_freq_separate(this, out_freq_mult, out_freq_type)
      use shr_string_mod, only: to_lower => shr_string_toLower
      ! Dummy arguments
      class(hist_file_t), intent(in)  :: this
      integer,            intent(out) :: out_freq_mult
      character(len=8),   intent(out) :: out_freq_type

      out_freq_mult = this%output_freq_mult
      out_freq_type = this%output_freq_type

   end subroutine config_output_freq_separate

   ! ========================================================================

   pure function config_history_file(this) result(history_file)
      ! Dummy arguments
      class(hist_file_t), intent(in) :: this
      logical                        :: history_file

      history_file = this%hfile_type == hfile_type_history

   end function config_history_file

   ! ========================================================================

   pure function config_init_value_file(this) result(init_value_file)
      ! Dummy arguments
      class(hist_file_t), intent(in) :: this
      logical                        :: init_value_file

      init_value_file = this%hfile_type == hfile_type_init_value

   end function config_init_value_file

   ! ========================================================================

   pure function config_satellite_file(this) result(satellite_file)
      ! Dummy arguments
      class(hist_file_t), intent(in) :: this
      logical                        :: satellite_file

      satellite_file = this%hfile_type == hfile_type_sat_track

   end function config_satellite_file

   ! ========================================================================

   pure function config_restart_file(this) result(restart_file)
      ! Dummy arguments
      class(hist_file_t), intent(in) :: this
      logical                        :: restart_file

      restart_file = this%hfile_type == hfile_type_restart

   end function config_restart_file

   ! ========================================================================

   pure function config_do_write_nstep0(this) result(write_nstep0)
      ! Dummy arguments
      class(hist_file_t), intent(in) :: this
      logical                        :: write_nstep0

      write_nstep0 = this%write_nstep0

   end function config_do_write_nstep0

   ! ========================================================================

   pure function config_file_is_setup(this) result(file_is_setup)
      ! Dummy arguments
      class(hist_file_t), intent(in) :: this
      logical                        :: file_is_setup

      file_is_setup = allocated(this%grids)

   end function config_file_is_setup

   ! ========================================================================

   pure function config_files_open(this) result(files_open)
      ! Dummy arguments
      class(hist_file_t), intent(in) :: this
      logical                        :: files_open

      files_open = this%files_open

   end function config_files_open

   ! ========================================================================

   subroutine config_reset(this)
      ! Dummy argument
      class(hist_file_t), intent(inout) :: this

      this%collect_patch_output = PATCH_DEF
      this%rl_kind = OUTPUT_DEF
      this%max_frames = UNSET_I
      this%output_freq_mult = UNSET_I
      this%output_freq_type = UNSET_C
      this%num_samples = 0
      this%beg_time = UNSET_R8
      this%end_time = UNSET_R8
      this%hfile_type = hfile_type_default
      if (allocated(this%file_names)) then
         deallocate(this%file_names)
      end if
      if (associated(this%interp_info)) then
         deallocate(this%interp_info)
         nullify(this%interp_info)
      end if
   end subroutine config_reset

   ! ========================================================================

   subroutine config_configure(this, volume, out_prec, max_frames,            &
        output_freq, file_type, filename_spec, collect_patch_out,             &
        inst_fields, avg_fields, min_fields, max_fields, var_fields,          &
        write_nstep0, interp_out, interp_nlat, interp_nlon, interp_grid,      &
        interp_type)
      use shr_string_mod,      only: to_lower => shr_string_toLower
      use cam_history_support, only: parse_multiplier
      use cam_abortutils,      only: endrun, check_allocate
      use shr_kind_mod,        only: CM => shr_kind_cm
      use time_manager,        only: get_step_size
      use string_utils,        only: stringify
      ! Dummy arguments
      class(hist_file_t),         intent(inout) :: this
      character(len=*),           intent(in)    :: volume
      integer,                    intent(in)    :: out_prec
      integer,                    intent(in)    :: max_frames
      character(len=*),           intent(in)    :: output_freq
      integer,                    intent(in)    :: file_type
      character(len=*),           intent(in)    :: filename_spec
      logical,                    intent(in)    :: collect_patch_out
      character(len=*),           intent(in)    :: inst_fields(:)
      character(len=*),           intent(in)    :: avg_fields(:)
      character(len=*),           intent(in)    :: min_fields(:)
      character(len=*),           intent(in)    :: max_fields(:)
      character(len=*),           intent(in)    :: var_fields(:)
      logical,                    intent(in)    :: write_nstep0
      logical,          optional, intent(in)    :: interp_out
      integer,          optional, intent(in)    :: interp_nlat
      integer,          optional, intent(in)    :: interp_nlon
      character(len=*), optional, intent(in)    :: interp_grid
      character(len=*), optional, intent(in)    :: interp_type
      ! Local variables
      character(len=CM) :: errmsg
      integer           :: last_char
      integer           :: ierr
      integer           :: num_fields
      integer           :: num_inst_fields
      integer           :: num_avg_fields
      integer           :: num_min_fields
      integer           :: num_max_fields
      integer           :: num_var_fields
      integer           :: field_index
      integer           :: idx
      integer           :: dtime
      integer           :: freq_in_seconds
      integer           :: filename_len
      character(len=*), parameter :: freq_types_to_check(3) = (/'second', 'minute', 'hour  '/)
      integer,          parameter :: seconds_in_minute = 60
      integer,          parameter :: seconds_in_hour = 3600
      character(len=*), parameter :: subname = 'config_configure: '
      character(len=*), parameter :: default_filename_spec = '%c.cam.%u%f.%y-%m-%d-%s.nc'

      call this%reset()

      this%volume = volume
      this%rl_kind = out_prec
      this%max_frames = max_frames
      dtime = get_step_size()
      ! Parse output frequency spec into multiplier and type
      ! Note, the allowed_set should match __TIME_PERIODS in hist_config.py
      call parse_multiplier(output_freq, this%output_freq_mult,               &
           this%output_freq_type, errmsg=errmsg,                              &
           allowed_set=(/ 'nsteps  ', 'nstep   ', 'nseconds', 'nsecond ',     &
           'nminutes', 'nminute ', 'nhours  ', 'nhour   ', 'ndays   ',        &
           'nday    ', 'nmonths ', 'nmonth  ', 'nyears  ', 'nyear   ',        &
           'steps   ', 'seconds ', 'minutes ', 'hours   ', 'days    ',        &
           'months  ', 'years   ', 'step    ', 'second  ', 'minute  ',        &
           'hour    ', 'day     ', 'month   ', 'year    '/))
      if (this%output_freq_mult < 1) then
         call endrun(subname//trim(errmsg), file=__FILE__, line=__LINE__-6)
      end if
      ! Standardize frequency type
      if (to_lower(this%output_freq_type(1:1)) == "n") then
         this%output_freq_type = this%output_freq_type(2:)
      end if
      last_char = len_trim(this%output_freq_type)
      if (to_lower(this%output_freq_type(last_char:last_char)) == "s") then
         this%output_freq_type = this%output_freq_type(1:last_char-1)
      end if
      ! It's an error to have a frequency < the timestep size
      if (any(freq_types_to_check == trim(this%output_freq_type))) then
         freq_in_seconds = this%output_freq_mult
         if (trim(this%output_freq_type) == 'hour') then
            freq_in_seconds = this%output_freq_mult * seconds_in_hour
         else if (trim(this%output_freq_type) == 'minute') then
            freq_in_seconds = this%output_freq_mult * seconds_in_minute
         end if
         if (freq_in_seconds < dtime) then
            write(errmsg,*) subname//' ERROR: output frequency "', trim(output_freq), &
                    '" cannot be less than timestep size (', stringify((/dtime/)),    &
                    ' seconds)'
            call endrun(errmsg, file=__FILE__)
         end if
      end if

      this%hfile_type = file_type
      this%collect_patch_output = collect_patch_out
      this%write_nstep0 = write_nstep0
      ! Append accumulation flag to user-specified filename specifier
      if (trim(filename_spec) /= default_filename_spec) then
         filename_len = len_trim(filename_spec)
         this%filename_spec = filename_spec(:filename_len-3)  // '%f.nc'
      else
         this%filename_spec = filename_spec
      end if

      if (present(interp_out)) then
         if (interp_out) then
            allocate(this%interp_info)
            ! To do: write and call interp object creator
         end if
      end if

      num_inst_fields = count_array(inst_fields)
      num_avg_fields = count_array(avg_fields)
      num_min_fields = count_array(min_fields)
      num_max_fields = count_array(max_fields)
      num_var_fields = count_array(var_fields)

      num_fields = num_inst_fields + num_avg_fields + num_min_fields + &
         num_max_fields + num_var_fields

      if (num_inst_fields > 0) then
         this%has_instantaneous = .true.
      end if

      if (num_fields - num_inst_fields > 0) then
         this%has_accumulated = .true.
      end if

      allocate(this%field_names(num_fields), stat=ierr)
      call check_allocate(ierr, subname, 'this%field_names',             &
           file=__FILE__, line=__LINE__-1)
      allocate(this%accumulate_types(num_fields), stat=ierr)
      call check_allocate(ierr, subname, 'this%accumulate_types',             &
           file=__FILE__, line=__LINE__-1)

      call this%field_list_hash_table%initialize(num_fields)
      allocate(this%field_list(num_fields), stat=ierr)
      call check_allocate(ierr, subname, 'this%field_list',             &
           file=__FILE__, line=__LINE__-1)

      field_index = 1
      ! Add the field names and associated accumulate types to the object
      do idx = 1, num_inst_fields
         this%accumulate_types(field_index) = 'lst'
         this%field_names(field_index) = inst_fields(idx)
         field_index = field_index + 1
      end do
      do idx = 1, num_avg_fields
         this%accumulate_types(field_index) = 'avg'
         this%field_names(field_index) = avg_fields(idx)
         field_index = field_index + 1
      end do
      do idx = 1, num_min_fields
         this%accumulate_types(field_index) = 'min'
         this%field_names(field_index) = min_fields(idx)
         field_index = field_index + 1
      end do
      do idx = 1, num_max_fields
         this%accumulate_types(field_index) = 'max'
         this%field_names(field_index) = max_fields(idx)
         field_index = field_index + 1
      end do
      do idx = 1, num_var_fields
         this%accumulate_types(field_index) = 'var'
         this%field_names(field_index) = var_fields(idx)
         field_index = field_index + 1
      end do

   end subroutine config_configure

   ! ========================================================================

   subroutine config_print_config(this)
      use string_utils,   only: stringify
      use spmd_utils,     only: masterproc
      use cam_logfile,    only: iulog
      use cam_abortutils, only: endrun
      use ISO_FORTRAN_ENV,only: REAL32
      ! Dummy argument
      class(hist_file_t), intent(in) :: this

      if (masterproc) then
         if (this%has_instantaneous) then
            write(iulog, '(3a)') "Instanteous history configuration for volume = ",   &
               trim(this%volume), 'i'
         end if
         if (this%has_accumulated) then
            write(iulog, '(3a)') "Accumulated history configuration for volume = ",   &
               trim(this%volume), 'a'
         end if
         select case(this%hfile_type)
         case (hfile_type_history)
            write(iulog, *) "File will contain model history (diagnostics) output"
         case (hfile_type_init_value)
            write(iulog, *) "File will contain values for model initialization"
         case (hfile_type_sat_track)
            write(iulog, *) "File will contain satellite track values"
         case (hfile_type_restart)
            write(iulog, *) "File contains history restart information"
         case default
            call endrun("ERROR: Unknown CAM history file type, "//            &
                 stringify((/this%hfile_type/)))
         end select
         if (this%rl_kind == REAL64) then
            write(iulog, '(a)') "  Output precision, 64 bits"
         else if (this%rl_kind == REAL32) then
            write(iulog, '(a)') "  Output precision, 32 bits"
         else
            call endrun("ERROR: Unknown output precision, "//                 &
                    stringify((/this%rl_kind/)))
         end if
         write(iulog, '(a,i0)') "  Maximum number of output frames per file = ",  &
              this%max_frames
         if (this%output_freq_mult == 1) then
            write(iulog, *) "  Writing output once per ", trim(this%output_freq_type)
         else
            write(iulog, '(a,i0,3a)') "  Writing output every ",                  &
                 this%output_freq_mult, " ", trim(this%output_freq_type), "s"
         end if
         !!XXgoldyXX: Fix this when patch output is known
         if (this%collect_patch_output) then
            write(iulog, '(2a)') "  Output from all patches will be collected ",  &
                 "into a single variable"
         else
            write(iulog, '(2a)') "  Output from each patch will be written ",     &
                 "as a separate variable"
         end if
         ! peverwhee - add interpolation info
      end if
   end subroutine config_print_config

   ! ========================================================================

   subroutine config_set_beg_time(this, day, sec)
      ! Dummy arguments
      class(hist_file_t), intent(inout) :: this
      integer, intent(in) :: day
      integer, intent(in) :: sec
      integer, parameter :: seconds_per_day = 86400._r8

      this%beg_time = day + (sec/seconds_per_day)

   end subroutine config_set_beg_time

   ! ========================================================================

   subroutine config_set_end_time(this, day, sec)
      ! Dummy arguments
      class(hist_file_t), intent(inout) :: this
      integer, intent(in) :: day
      integer, intent(in) :: sec

      this%end_time = day + (sec/86400._r8)

   end subroutine config_set_end_time

   ! ========================================================================

   subroutine config_set_up_fields(this, possible_field_list)
      use hist_api,            only: hist_new_field, hist_new_buffer
      use hist_hashable,       only: hist_hashable_t
      use cam_grid_support,    only: cam_grid_num_grids
      use hist_msg_handler,    only: hist_log_messages
      use cam_history_support, only: max_chars
      use cam_logfile,         only: iulog
      use spmd_utils,          only: masterproc
      use cam_abortutils,      only: check_allocate, endrun
      use shr_kind_mod,        only: CM => shr_kind_cm

      ! Dummy arguments
      class(hist_file_t),        intent(inout) :: this
      type(hist_hash_table_t),   intent(in)    :: possible_field_list

      integer :: idx
      integer :: ierr
      integer :: num_grids, grid_idx
      integer, allocatable :: possible_grids(:)
      class(hist_field_info_t), pointer :: field_info
      class(hist_field_info_t), pointer :: field_ptr
      class(hist_hashable_t), pointer :: field_ptr_entry
      character(len=*), parameter :: subname = 'hist:config_set_up_fields: '
      integer, allocatable :: dimensions(:)
      integer, allocatable :: field_shape(:)
      integer, allocatable :: beg_dim(:)
      integer, allocatable :: end_dim(:)
      character(len=CM)    :: errmsg
      type(hist_log_messages) :: errors

      allocate(possible_grids(cam_grid_num_grids() + 1), stat=ierr)
      call check_allocate(ierr, subname, 'possible_grids',             &
           file=__FILE__, line=__LINE__-1)
      possible_grids = -1
      num_grids = 0
      errmsg = ''
      do idx = 1, size(this%field_names)
         ! Find the relevant field in the possible field list
         field_ptr_entry => possible_field_list%table_value(this%field_names(idx))
         select type(field_ptr_entry)
         type is (hist_field_info_t)
            field_ptr => field_ptr_entry
         class default
            write(errmsg,'(3a)') 'ERROR Field : ',trim(this%field_names(idx)),' not available'
            call endrun(subname//errmsg, file=__FILE__, line=__LINE__)
         end select
         !call field_ptr%dimensions(dimensions)
         dimensions = field_ptr%dimensions()
         field_shape = field_ptr%shape()
         beg_dim = field_ptr%beg_dims()
         end_dim = field_ptr%end_dims()
         field_info => hist_new_field(this%field_names(idx),     &
            field_ptr%standard_name(), field_ptr%long_name(),    &
            field_ptr%units(), field_ptr%type(), field_ptr%decomp(), &
            dimensions, this%accumulate_types(idx), field_ptr%num_levels(), &
            field_shape, beg_dims=beg_dim, end_dims=end_dim,     &
            mixing_ratio=field_ptr%mixing_ratio())
         call hist_new_buffer(field_info, field_shape, &
            this%rl_kind, 1, this%accumulate_types(idx), 1, errors=errors)
         if (masterproc) then
            call errors%output(iulog)
         end if
         ! peverwhee - TODO: create additional buffer(s) for other accum types
!         call hist_new_buffer(field_info, field_shape, &
!            this%rl_kind, 1, this%accumulate_types(idx), 1)
         ! Add to field list array and hash table
         this%field_list(idx) = field_info
         call this%field_list_hash_table%add_hash_key(field_info)
         ! Add grid to possible grids if it's not already there
         if (.not. any(possible_grids(:) == field_ptr%decomp())) then
            grid_idx = findloc(possible_grids(:), -1, dim=1)
            possible_grids(grid_idx) = field_ptr%decomp()
            num_grids = num_grids + 1
         end if
         deallocate(dimensions)
         deallocate(field_shape)
         if (allocated(beg_dim)) then
            deallocate(beg_dim)
         end if
         if (allocated(end_dim)) then
            deallocate(end_dim)
         end if
      end do
      ! Finish set-up of grids for this volume
      allocate(this%grids(num_grids), stat=ierr)
      call check_allocate(ierr, subname, 'this%grids',             &
           file=__FILE__, line=__LINE__-1)
      this%grids(1:num_grids) = possible_grids(1:num_grids)
      ! We don't need the user-set fields arrays anymore
      deallocate(this%accumulate_types)
      deallocate(this%field_names)

   end subroutine config_set_up_fields

   ! ========================================================================

   subroutine config_find_in_field_list(this, diagnostic_name, field_info, errmsg)
      use hist_hashable,       only: hist_hashable_t
      ! Dummy arguments
      class(hist_file_t), intent(in)                 :: this
      character(len=*),   intent(in)                 :: diagnostic_name
      class(hist_field_info_t), pointer, intent(out) :: field_info
      character(len=*),   intent(out)                :: errmsg

      ! Local variables
      class(hist_hashable_t),   pointer :: field_ptr_entry
      integer                           :: field_idx
      logical                           :: found_field
      character(len=*), parameter       :: subname = 'hist:find_in_field_list: '

      nullify(field_info) 
      errmsg = ''
      found_field = .false.
      ! Loop over fields
      do field_idx = 1, size(this%field_list)
         if (trim(this%field_list(field_idx)%diag_name()) == trim(diagnostic_name)) then
            ! Grab the associated accumulate flag
            found_field = .true.
         end if
      end do
      if (.not. found_field) then
         ! Not an error - return with the null field_info pointer
         return
      end if
      found_field = .false.
      
      ! Grab the field info pointer from the hash table
      field_ptr_entry => this%field_list_hash_table%table_value(diagnostic_name)
      select type(field_ptr_entry)
      type is (hist_field_info_t)
         field_info => field_ptr_entry
         found_field = .true.
      class default
         write(errmsg,*) subname//'Unknown field type for "'//trim(diagnostic_name)//'"'
         return
      end select

      if (.not. found_field) then
         ! Field not found - return an error
         write(errmsg,*) subname//"Field not found in field list, '"//      &
            trim(diagnostic_name)//"'"
         return
      end if

   end subroutine config_find_in_field_list

   !#######################################################################

   subroutine AvgflagToString(avgflag, time_op)
      use cam_history_support, only: max_chars
      use cam_abortutils,      only: endrun
      ! Dummy arguments
       character(len=3),           intent(in)  :: avgflag ! averaging flag
       character(len=max_chars),   intent(out) :: time_op ! time op (e.g. max)

       ! Local variable
       character(len=*), parameter             :: subname = 'AvgflagToString'

       select case (avgflag)
       case ('avg')
         time_op(:) = 'mean'
       case ('B')
         time_op(:) = 'mean00z'
       case ('N')
         time_op(:) = 'mean_over_nsteps'
       case ('lst')
         time_op(:) = 'point'
       case ('max')
         time_op(:) = 'maximum'
       case ('min')
         time_op(:) = 'minimum'
       case('L')
         time_op(:) = 'mean (over local time)'
       case ('var')
         time_op(:) = 'standard_deviation'
       case default
         call endrun(subname//': unknown avgflag = '//avgflag)
    end select
  end subroutine AvgflagToString

  !#######################################################################

   ! ========================================================================

   subroutine config_define_file(this, restart, logname, host, model_doi_url)
      use pio,                 only: PIO_CLOBBER, pio_file_is_open, pio_unlimited
      use pio,                 only: pio_double, pio_def_var, pio_put_att, pio_int
      use pio,                 only: PIO_GLOBAL, pio_char, pio_real, PIO_NOERR, pio_enddef
      use pio,                 only: pio_put_var
      use cam_pio_utils,       only: cam_pio_createfile, cam_pio_def_var
      use cam_pio_utils,       only: cam_pio_def_dim, cam_pio_handle_error
      use cam_grid_support,    only: cam_grid_header_info_t, cam_grid_write_attr
      use cam_grid_support,    only: cam_grid_write_var
      use cam_history_support, only: write_hist_coord_attrs
      use cam_history_support, only: write_hist_coord_vars
      use cam_history_support, only: max_chars
      use time_manager,        only: get_ref_date, timemgr_get_calendar_cf
      use time_manager,        only: get_step_size
      use string_utils,        only: date2yyyymmdd, sec2hms, stringify
      use cam_control_mod,     only: caseid
      use cam_initfiles,       only: ncdata, bnd_topo
      use cam_abortutils,      only: check_allocate, endrun
      use cam_logfile,         only: iulog
      use spmd_utils,          only: masterproc
      use shr_kind_mod,        only: CM => shr_kind_cm
      ! Define the metadata for the file(s) for this volume
      ! Dummy arguments
      class(hist_file_t), intent(inout) :: this
      logical,            intent(in)    :: restart
      character(len=*),   intent(in)    :: logname
      character(len=*),   intent(in)    :: host
      character(len=*),   intent(in)    :: model_doi_url

      ! Local variables
      integer :: amode, ierr
      integer :: grid_index, split_file_index, field_index, idx, jdx
      integer :: dtime              ! timestep size
      integer :: yr, mon, day       ! year, month, day components of a date
      integer :: nbsec              ! time of day component of base date [seconds]
      integer :: nbdate             ! base date in yyyymmdd format
      integer :: nsbase = 0         ! seconds component of base time
      integer :: ndbase = 0         ! days component of base time
      integer :: ncreal             ! real data type for output
      integer :: grd
      integer :: mdimsize, num_hdims, fdims
      integer :: num_patches
      integer, allocatable :: mdims(:)

      logical                  :: is_satfile
      logical                  :: is_initfile
      logical                  :: varid_set
      character(len=16)        :: time_per_freq
      character(len=max_chars) :: str       ! character temporary
      character(len=max_chars) :: calendar  ! Calendar type
      character(len=max_chars) :: cell_methods ! For cell_methods attribute
      character(len=max_chars) :: fname_tmp ! local copy of field name
      character(len=CM)        :: errmsg
      type(var_desc_t)         :: varid
      ! NetCDF dimensions
      integer :: timdim             ! unlimited dimension id
      integer :: bnddim             ! bounds dimension id
      integer :: chardim            ! character dimension id
      integer :: dimenchar(2)       ! character dimension ids
      integer :: nacsdims(2)        ! dimension ids for nacs (used in restart file)
      integer :: max_mdims          ! maximum number of middle dimensions
      integer :: max_hdims          ! maximum number of grid dimensions

      integer, allocatable :: dimindex(:)    ! dimension ids for variable declaration
      ! A structure to hold the horizontal dimension and coordinate info
      type(cam_grid_header_info_t), allocatable :: header_info(:)
      integer,          allocatable    :: mdimids(:)
      integer,          parameter :: max_netcdf_len = 256
      character(len=*), parameter :: subname = 'config_define_file: '

      is_initfile = (this%hfile_type == hfile_type_init_value)
      is_satfile = (this%hfile_type == hfile_type_sat_track)
      
      ! Log what we're doing
      if (masterproc) then
         if (this%has_accumulated) then
            write(iulog,*) 'Opening netcdf history file for accumulated output ', &
               trim(this%file_names(accumulated_file_index))
         end if
         if (this%has_instantaneous) then
            write(iulog,*) 'Opening netcdf history file for instantaneous output ', &
               trim(this%file_names(instantaneous_file_index))
         end if
      end if

      amode = PIO_CLOBBER

      if (this%has_instantaneous) then
         call cam_pio_createfile(this%hist_files(instantaneous_file_index),   &
            this%file_names(instantaneous_file_index), amode)
      end if

      if (this%has_accumulated) then
         call cam_pio_createfile(this%hist_files(accumulated_file_index),  &
            this%file_names(accumulated_file_index), amode)
      end if

      this%files_open = .true.

      allocate(header_info(size(this%grids)), stat=ierr)
      call check_allocate(ierr, subname, 'header_info',             &
           file=__FILE__, line=__LINE__-1)

      max_hdims = 0
      do grid_index = 1, size(this%grids)
         do split_file_index = 1, max_split_files
            if (pio_file_is_open(this%hist_files(split_file_index))) then
               call cam_grid_write_attr(this%hist_files(split_file_index), &
                  this%grids(grid_index), header_info(grid_index),         &
                  file_index=split_file_index)
               max_hdims = max(max_hdims, header_info(grid_index)%num_hdims())
            end if
         end do
      end do
      ! Determine the maximum number of dimensions
      max_mdims = 0
      do field_index = 1, size(this%field_list)
         max_mdims = max(max_mdims, size(this%field_list(field_index)%dimensions()))
      end do
      ! Allocate dimindex to the maximum possible dimensions (plus 1 for time)
      allocate(dimindex(max_hdims + max_mdims + 1), stat=ierr)
      call check_allocate(ierr, subname, 'dimindex', file=__FILE__, line=__LINE__-1)

      call get_ref_date(yr, mon, day, nbsec)
      nbdate = yr*10000 + mon*100 + day
      calendar = timemgr_get_calendar_cf()
      dtime = get_step_size()
      ! v peverwhee - remove when patch output is set up
      num_patches = 1
      ! ^ peverwhee - remove when patch output is set up
      varid_set = .true.
      ! Allocate the varid array
      if (.not. allocated(this%file_varids)) then
         allocate(this%file_varids(size(this%field_list), num_patches), stat=ierr)
         call check_allocate(ierr, subname, 'this%file_varids',             &
              file=__FILE__, line=__LINE__-1)
         varid_set = .false.
      end if

      ! Format frequency
      write(time_per_freq,999) trim(this%output_freq_type), '_', this%output_freq_mult
999   format(2a,i0)

      do split_file_index = 1, max_split_files
         if (.not. pio_file_is_open(this%hist_files(split_file_index))) then
            cycle
         end if
         ! Define the unlimited time dim
         call cam_pio_def_dim(this%hist_files(split_file_index), 'time', pio_unlimited, timdim)
         call cam_pio_def_dim(this%hist_files(split_file_index), 'nbnd', 2, bnddim, existOK=.true.)
         call cam_pio_def_dim(this%hist_files(split_file_index), 'chars', 8, chardim)
         ! Populate the history coordinate (well, mdims anyway) attributes
         ! This routine also allocates the mdimids array
         call write_hist_coord_attrs(this%hist_files(split_file_index), bnddim, mdimids, restart)
         ! Define time variable
         ierr=pio_def_var (this%hist_files(split_file_index),'time',pio_double,(/timdim/),this%timeid)
         call cam_pio_handle_error(ierr, 'config_define_file: failed to define "time" variable')
         ierr=pio_put_att (this%hist_files(split_file_index), this%timeid, 'long_name', 'time')
         call cam_pio_handle_error(ierr, 'config_define_file: failed to add "long_name" attribute to "time" variable')
         str = 'days since ' // date2yyyymmdd(nbdate) // ' ' // sec2hms(nbsec)
         ierr=pio_put_att (this%hist_files(split_file_index), this%timeid, 'units', trim(str))
         call cam_pio_handle_error(ierr, 'config_define_file: failed to add "units" attribtue to "time" variable')
         ierr=pio_put_att (this%hist_files(split_file_index), this%timeid, 'calendar', trim(calendar))
         call cam_pio_handle_error(ierr, 'config_define_file: failed to add "calendar" attribute to "time" variable')

         ! Define date variable
         ierr=pio_def_var (this%hist_files(split_file_index),'date    ',pio_int,(/timdim/),this%dateid)
         call cam_pio_handle_error(ierr, 'config_define_file: failed to define "date" variable')
         str = 'current date (YYYYMMDD)'
         ierr=pio_put_att (this%hist_files(split_file_index), this%dateid, 'long_name', trim(str))
         call cam_pio_handle_error(ierr, 'config_define_file: failed to add "long_name" attribute to "date" variable')

         ! Define datesec variable
         ierr=pio_def_var (this%hist_files(split_file_index),'datesec ',pio_int,(/timdim/), this%datesecid)
         call cam_pio_handle_error(ierr, 'config_define_file: failed to define "datesec" variable')
         str = 'current seconds of current date'
         ierr=pio_put_att (this%hist_files(split_file_index), this%datesecid, 'long_name', trim(str))
         call cam_pio_handle_error(ierr, 'config_define_file: failed to add "long_name" attribute to "datesec" variable')

         !
         ! Character header information
         !
         str = 'CF-1.0'
         ierr=pio_put_att (this%hist_files(split_file_index), PIO_GLOBAL, 'Conventions', trim(str))
         call cam_pio_handle_error(ierr, 'config_define_file: failed to add "Conventions" attribtue to file')
         ierr=pio_put_att (this%hist_files(split_file_index), PIO_GLOBAL, 'source', 'CAM-SIMA')
         call cam_pio_handle_error(ierr, 'config_define_file: failed to add "source" attribute to file')
         ierr=pio_put_att (this%hist_files(split_file_index), PIO_GLOBAL, 'case', caseid)
         call cam_pio_handle_error(ierr, 'config_define_file: failed to add "case" attribute to file')
         ierr=pio_put_att (this%hist_files(split_file_index), PIO_GLOBAL, 'logname',trim(logname))
         call cam_pio_handle_error(ierr, 'config_define_file: failed to add "logname" attribute to file')
         ierr=pio_put_att (this%hist_files(split_file_index), PIO_GLOBAL, 'host', trim(host))
         call cam_pio_handle_error(ierr, 'config_define_file: failed to add "host" attribute to file')

         ierr=pio_put_att (this%hist_files(split_file_index), PIO_GLOBAL, 'initial_file', ncdata)
         call cam_pio_handle_error(ierr, 'config_define_file: failed to add "initial_file" attribute to file')
         ierr=pio_put_att (this%hist_files(split_file_index), PIO_GLOBAL, 'topography_file', bnd_topo)
         call cam_pio_handle_error(ierr, 'config_define_file: failed to add "topography_file" attribute to file')
         if (len_trim(model_doi_url) > 0) then
            ierr=pio_put_att (this%hist_files(split_file_index), PIO_GLOBAL, 'model_doi_url', model_doi_url)
            call cam_pio_handle_error(ierr, 'config_define_file: failed to add "model_doi_url" attribute to file')
         end if

         ierr=pio_put_att (this%hist_files(split_file_index), PIO_GLOBAL, 'time_period_freq', trim(time_per_freq))
         call cam_pio_handle_error(ierr, 'config_define_file: failed to add "time_period_freq" attribute to file')

         if (.not. is_satfile) then
            ! Define time_bounds variable
            ierr=pio_put_att (this%hist_files(split_file_index), this%timeid, 'bounds', 'time_bounds')
            call cam_pio_handle_error(ierr, 'config_define_file: failed to add "bounds" attribute to file')
            ierr=pio_def_var (this%hist_files(split_file_index),'time_bounds',pio_double,(/bnddim,timdim/),this%tbndid)
            call cam_pio_handle_error(ierr, 'config_define_file: failed to define "time_bounds" variable')
            ierr=pio_put_att (this%hist_files(split_file_index), this%tbndid, 'long_name', 'time interval endpoints')
            call cam_pio_handle_error(ierr, 'config_define_file: failed to add "long_name" attribute to "time_bounds" variable')
            str = 'days since ' // date2yyyymmdd(nbdate) // ' ' // sec2hms(nbsec)
            ierr=pio_put_att (this%hist_files(split_file_index), this%tbndid, 'units', trim(str))
            call cam_pio_handle_error(ierr, 'config_define_file: failed to add "units" attribute to "time_bounds" variable')
            ierr=pio_put_att (this%hist_files(split_file_index), this%tbndid, 'calendar', trim(calendar))
            call cam_pio_handle_error(ierr, 'config_define_file: failed to add "calendar" attribute to "time_bounds" variable')

            !
            ! Character
            !
            dimenchar(1) = chardim
            dimenchar(2) = timdim
            ierr=pio_def_var (this%hist_files(split_file_index),'date_written',pio_char,dimenchar,this%date_writtenid)
            call cam_pio_handle_error(ierr, 'config_define_file: failed to define "date_written" variable')
            ierr=pio_def_var (this%hist_files(split_file_index),'time_written',pio_char,dimenchar,this%time_writtenid)
            call cam_pio_handle_error(ierr, 'config_define_file: failed to define "time_written" variable')

            !
            ! Integer header
            !
            ! Define base day variables
            ierr=pio_def_var (this%hist_files(split_file_index),'ndbase',PIO_INT,this%ndbaseid)
            call cam_pio_handle_error(ierr, 'config_define_file: failed to define "ndbase" variable')
            str = 'base day'
            ierr=pio_put_att (this%hist_files(split_file_index), this%ndbaseid, 'long_name', trim(str))
            call cam_pio_handle_error(ierr, 'config_define_file: failed to add "long_name" attribute to "ndbase" variable')

            ierr=pio_def_var (this%hist_files(split_file_index),'nsbase',PIO_INT,this%nsbaseid)
            call cam_pio_handle_error(ierr, 'config_define_file: failed to define "nsbase" variable')
            str = 'seconds of base day'
            ierr=pio_put_att (this%hist_files(split_file_index), this%nsbaseid, 'long_name', trim(str))
            call cam_pio_handle_error(ierr, 'config_define_file: failed to add "long_name" attribute to "nsbase" variable')

            ierr=pio_def_var (this%hist_files(split_file_index),'nbdate',PIO_INT,this%nbdateid)
            call cam_pio_handle_error(ierr, 'config_define_file: failed to define "nbdate" variable')
            str = 'base date (YYYYMMDD)'
            ierr=pio_put_att (this%hist_files(split_file_index), this%nbdateid, 'long_name', trim(str))
            call cam_pio_handle_error(ierr, 'config_define_file: failed to add "long_name" attribtue to "nbdate" variable')

            ierr=pio_def_var (this%hist_files(split_file_index),'nbsec',PIO_INT,this%nbsecid)
            call cam_pio_handle_error(ierr, 'config_define_file: failed to define "nbsec" variable')
            str = 'seconds of base date'
            ierr=pio_put_att (this%hist_files(split_file_index), this%nbsecid, 'long_name', trim(str))
            call cam_pio_handle_error(ierr, 'config_define_file: failed to add "long_name" attribute to "nbsec" variable')

            ierr=pio_def_var (this%hist_files(split_file_index),'mdt',PIO_INT,this%mdtid)
            call cam_pio_handle_error(ierr, 'config_define_file: failed to define "mdt" variable')
            ierr=pio_put_att (this%hist_files(split_file_index), this%mdtid, 'long_name', 'timestep')
            call cam_pio_handle_error(ierr, 'config_define_file: failed to add "long_name" attribute to "mdt" variable')
            ierr=pio_put_att (this%hist_files(split_file_index), this%mdtid, 'units', 's')
            call cam_pio_handle_error(ierr, 'config_define_file: failed to add "units" attribute to "mdt" variable')

            !
            ! Create variables for model timing and header information
            !
            if (split_file_index == instantaneous_file_index) then
               ierr=pio_def_var (this%hist_files(split_file_index),'ndcur   ',pio_int,(/timdim/),this%ndcurid)
               call cam_pio_handle_error(ierr, 'config_define_file: failed to define "ndcur" variable')
               str = 'current day (from base day)'
               ierr=pio_put_att (this%hist_files(split_file_index), this%ndcurid, 'long_name', trim(str))
               call cam_pio_handle_error(ierr, 'config_define_file: failed to add "long_name" attribute to "ndcur" variable')

               ierr=pio_def_var (this%hist_files(split_file_index),'nscur   ',pio_int,(/timdim/),this%nscurid)
               call cam_pio_handle_error(ierr, 'config_define_file: failed to define "nscur" variable')
               str = 'current seconds of current day'
               ierr=pio_put_att (this%hist_files(split_file_index), this%nscurid, 'long_name', trim(str))
               call cam_pio_handle_error(ierr, 'config_define_file: failed to add "long_name" attribute to "nscur" variable')
               ierr=pio_def_var (this%hist_files(split_file_index),'nsteph',pio_int,(/timdim/),this%nstephid)
               call cam_pio_handle_error(ierr, 'config_define_file: failed to define "nsteph" variable')
               str = 'current timestep'
               ierr=pio_put_att (this%hist_files(split_file_index), this%nstephid, 'long_name', trim(str))
               call cam_pio_handle_error(ierr, 'config_define_file: failed to add "long_name" attribute to "nsteph" variable')
            end if

         end if ! .not. satfile

         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         !
         ! Create variables and attributes for field list
         !
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         do field_index = 1, size(this%field_list)
            if (.not. is_satfile .and. .not. restart .and. .not. is_initfile) then
               if (split_file_index == accumulated_file_index) then
                  ! this is the accumulated file of a potentially split
                  ! history tape - skip instantaneous fields
                  if (this%field_list(field_index)%accumulate_type() == 'lst') then
                     cycle
                  end if
               else
                  ! this is the instantaneous file of a potentially split
                  ! history tape - skip accumulated fields
                  if (this%field_list(field_index)%accumulate_type() /= 'lst') then
                     cycle
                  end if
               end if
            end if

            if (this%precision() == 'REAL32') then
               ncreal = pio_real
            else if (this%precision() == 'REAL64') then
               ncreal = pio_double
            end if
            mdims = this%field_list(field_index)%dimensions()
            mdimsize = size(mdims)
            fname_tmp = strip_suffix(this%field_list(field_index)%diag_name())
            ! Ensure that fname_tmp is not longer than the maximum length for a
            !  netcdf file
            if (len_trim(fname_tmp) > max_netcdf_len) then
               ! Endrun if the name is too long
               write(errmsg, *) 'config_define_file: variable name ', trim(fname_tmp), &
                  ' too long for NetCDF file (len=', stringify((/len(trim(fname_tmp))/)), ' > ', &
                  stringify((/max_netcdf_len/)), ')'
               call endrun(errmsg, file=__FILE__, line=__LINE__)
            end if
            !
            !  Create variables and atributes for fields written out as columns
            !
            !  Find appropriate grid in header_info
            if (.not. allocated(header_info)) then
               ! Safety check
               call endrun('config_define_file: header_info not allocated', file=__FILE__, line=__LINE__)
            end if
            grd = -1
            do idx = 1, size(header_info)
              if (header_info(idx)%get_gridid() == this%field_list(field_index)%decomp()) then
                 grd = idx
                 exit
              end if
            end do
            if (grd < 0) then
               write(errmsg, '(a,i0,2a)') 'grid, ',this%field_list(field_index)%decomp(),', not found for ', &
                  trim(fname_tmp)
               call endrun('config_define_file: '//errmsg, file=__FILE__, line=__LINE__)
            end if
            num_hdims = header_info(grd)%num_hdims()
            do idx = 1, num_hdims
               dimindex(idx) = header_info(grd)%get_hdimid(idx)
               nacsdims(idx) = header_info(grd)%get_hdimid(idx)
            end do
            do idx = 1, num_patches
               varid = this%file_varids(field_index, idx)
               ! Figure the dimension ID array for this field
               ! We have defined the horizontal grid dimensions in dimindex
               fdims = num_hdims
               do jdx = 1, mdimsize
                  fdims = fdims + 1
                  dimindex(fdims) = mdimids(mdims(jdx))
               end do
               if(.not. restart) then
                  ! Only add time dimension if this is not a restart history tape
                  fdims = fdims + 1
                  dimindex(fdims) = timdim
               end if
               ! peverwhee - TODO: enable patch output
               ! Define the variable
               call cam_pio_def_var(this%hist_files(split_file_index), trim(fname_tmp), ncreal,           &
                    dimindex(1:fdims), varid)
               if (.not. varid_set) then
                  this%file_varids(field_index, idx) = varid
               end if
               if (mdimsize > 0) then
                  ierr = pio_put_att(this%hist_files(split_file_index), varid, 'mdims', mdims(1:mdimsize))
                  call cam_pio_handle_error(ierr, 'config_define_file: cannot define mdims for '//trim(fname_tmp))
               end if
               str = this%field_list(field_index)%sampling_sequence()
               if (len_trim(str) > 0) then
                  ierr = pio_put_att(this%hist_files(split_file_index), varid, 'Sampling_Sequence', trim(str))
                  call cam_pio_handle_error(ierr, 'config_define_file: cannot define Sampling_Sequence for '//trim(fname_tmp))
               end if
               if (this%field_list(field_index)%flag_xyfill()) then
                  ! peverwhee - TODO: implement fill values!
                  call endrun('config_define_file: fill values not yet implemented!', file=__FILE__, line=__LINE__)
               end if
               str = this%field_list(field_index)%units()
               if (len_trim(str) > 0) then
                  ierr=pio_put_att (this%hist_files(split_file_index), varid, 'units', trim(str))
                  call cam_pio_handle_error(ierr, 'config_define_file: cannot define units for '//trim(fname_tmp))
               end if
               str = this%field_list(field_index)%mixing_ratio()
               if (len_trim(str) > 0) then
                  ierr=pio_put_att (this%hist_files(split_file_index), varid, 'mixing_ratio', trim(str))
                  call cam_pio_handle_error(ierr, 'config_define_file: cannot define mixing_ratio for '//trim(fname_tmp))
               end if
               str = this%field_list(field_index)%long_name()
               ierr=pio_put_att (this%hist_files(split_file_index), varid, 'long_name', trim(str))
               call cam_pio_handle_error(ierr, 'config_define_file: cannot define long_name for '//trim(fname_tmp))
               ! Assign field attributes defining valid levels and averaging info
               cell_methods = ''
               if (len_trim(this%field_list(field_index)%cell_methods()) > 0) then
                  if (len_trim(cell_methods) > 0) then
                     cell_methods = trim(cell_methods)//' '//trim(this%field_list(field_index)%cell_methods())
                  else
                     cell_methods = trim(this%field_list(field_index)%cell_methods())
                  end if
               end if
               ! Time cell methods is after field method because time averaging is
               ! applied later (just before output) than field method which is applied
               ! before outfld call.
               call AvgflagToString(this%field_list(field_index)%accumulate_type(), str)
               cell_methods = adjustl(trim(cell_methods)//' '//'time: '//str)
               ierr = pio_put_att(this%hist_files(split_file_index), varid, 'cell_methods', trim(cell_methods))
               call cam_pio_handle_error(ierr, 'config_define_file: cannot define cell_methods for '//trim(fname_tmp))
            end do ! end loop over patches
            deallocate(mdims)
         end do ! end loop over fields
         deallocate(mdimids)
         ierr = pio_enddef(this%hist_files(split_file_index))
         if (ierr /= PIO_NOERR) then
            call endrun('config_define_file: ERROR exiting define mode in PIO', file=__FILE__, line=__LINE__)
         end if
         if(masterproc) then
            write(iulog,*)'config_define_file: Successfully opened netcdf file '//trim(this%file_names(split_file_index))
         end if

         !
         ! Write time-invariant portion of history header

         if (.not. is_satfile) then
            do idx = 1, size(this%grids)
               call cam_grid_write_var(this%hist_files(split_file_index), this%grids(idx), &
                  file_index=split_file_index)
            end do
            ierr = pio_put_var(this%hist_files(split_file_index), this%mdtid, (/dtime/))
            call cam_pio_handle_error(ierr, 'config_define_file: cannot put mdt')

            !
            ! Model date info
            !
            ierr = pio_put_var(this%hist_files(split_file_index), this%ndbaseid, (/ndbase/))
            call cam_pio_handle_error(ierr, 'config_define_file: cannot put ndbase')
            ierr = pio_put_var(this%hist_files(split_file_index), this%nsbaseid, (/nsbase/))
            call cam_pio_handle_error(ierr, 'config_define_file: cannot put nsbase')

            ierr = pio_put_var(this%hist_files(split_file_index), this%nbdateid, (/nbdate/))
            call cam_pio_handle_error(ierr, 'config_define_file: cannot put nbdate')
            ierr = pio_put_var(this%hist_files(split_file_index), this%nbsecid, (/nbsec/))
            call cam_pio_handle_error(ierr, 'config_define_file: cannot put nbsec')
         end if

         ! Write the mdim variable data
         call write_hist_coord_vars(this%hist_files(split_file_index), restart)

      end do ! end loop over files

      if (allocated(header_info)) then
         do idx = 1, size(header_info)
            call header_info(idx)%deallocate()
         end do
         deallocate(header_info)
      end if

   end subroutine config_define_file

   ! ========================================================================

   subroutine config_write_time_dependent_variables(this, restart)
      use pio,           only: pio_put_var, pio_file_is_open
      use time_manager,  only: get_nstep, get_curr_date, get_curr_time
      use time_manager,  only: set_date_from_time_float, get_step_size
      use datetime_mod,  only: datetime
      use spmd_utils,    only: masterproc
      use cam_logfile,   only: iulog
      use perf_mod,      only: t_startf, t_stopf
      use cam_pio_utils, only: cam_pio_handle_error
      ! Dummy arguments
      class(hist_file_t), intent(inout) :: this
      logical,            intent(in)    :: restart

      ! Local variables
      integer :: yr, mon, day      ! year, month, and day components of a date
      integer :: yr_mid, mon_mid, day_mid ! year, month, and day components of midpoint date
      integer :: nstep             ! current timestep number
      integer :: ncdate(max_split_files) ! current (or midpoint) date in integer format [yyyymmdd]
      integer :: ncsec(max_split_files)  ! current (or midpoint) time of day [seconds]
      integer :: ndcur             ! day component of current time
      integer :: nscur             ! seconds component of current time
      real(r8) :: time             ! current (or midpoint) time
      real(r8) :: time_interval(2) ! time interval boundaries
      integer :: ierr
      integer :: split_file_index, field_idx
      integer :: start, count1
      integer :: startc(2)          ! start values required by nf_put_vara (character)
      integer :: countc(2)          ! count values required by nf_put_vara (character)
      logical :: is_initfile, is_satfile
      character(len=8) :: cdate  ! system date
      character(len=8) :: ctime  ! system time

      nstep = get_nstep()
      call get_curr_date(yr, mon, day, ncsec(instantaneous_file_index))
      ncdate(instantaneous_file_index) = yr*10000 + mon*100 + day
      call get_curr_time(ndcur, nscur)
      time = ndcur + nscur/86400._r8
      time_interval(1) = this%beg_time
      time_interval(2) = time
      call set_date_from_time_float((time_interval(1) + time_interval(2)) / 2._r8, &
                                    yr_mid, mon_mid, day_mid, ncsec(accumulated_file_index))
      ncdate(accumulated_file_index) = yr_mid*10000 + mon_mid*100 + day_mid

      ! Increment samples
      this%num_samples = this%num_samples + 1

      start = mod(this%num_samples, this%max_frames) + 1
      count1 = 1

      is_initfile = (this%hfile_type == hfile_type_init_value)
      is_satfile = (this%hfile_type == hfile_type_sat_track)

      startc(1) = 1
      startc(2) = start
      countc(2) = 1

      if(.not.restart) this%beg_time = time  ! update beginning time of next interval
      call datetime (cdate, ctime)

      ! peverwhee - TODO handle composed fields

      call t_startf ('write_field')
      do split_file_index = 1, max_split_files
         if (.not. pio_file_is_open(this%hist_files(split_file_index))) then
            cycle
         end if
         if (masterproc) then
            if (split_file_index == instantaneous_file_index) then
               write(iulog,200) this%num_samples,'instantaneous',trim(this%volume),yr,mon,day,ncsec(split_file_index)
            else
               write(iulog,200) this%num_samples,'accumulated',trim(this%volume),yr_mid,mon_mid,day_mid,ncsec(split_file_index)
            end if
200         format('config_write_*: writing time sample ',i3,' to ', a, ' h-file ', &
                 a,' DATE=',i4.4,'/',i2.2,'/',i2.2,' NCSEC=',i6)
         end if
         if (split_file_index == instantaneous_file_index) then
            ierr = pio_put_var (this%hist_files(split_file_index),this%ndcurid,(/start/),(/count1/),(/ndcur/))
            call cam_pio_handle_error(ierr, 'config_write_time_dependent_variables: cannot write "ndcur" variable')
            ierr = pio_put_var (this%hist_files(split_file_index),this%nscurid,(/start/),(/count1/),(/nscur/))
            call cam_pio_handle_error(ierr, 'config_write_time_dependent_variables: cannot write "nscur" variable')
            ierr = pio_put_var (this%hist_files(split_file_index),this%nstephid,(/start/),(/count1/),(/nstep/))
            call cam_pio_handle_error(ierr, 'config_write_time_dependent_variables: cannot write "nstephid" variable')
         end if
         ierr = pio_put_var (this%hist_files(split_file_index),this%dateid,(/start/),(/count1/),(/ncdate(split_file_index)/))
         call cam_pio_handle_error(ierr, 'config_write_time_dependent_variables: cannot write "ncdate" variable')
         ierr = pio_put_var (this%hist_files(split_file_index),this%datesecid,(/start/),(/count1/),(/ncsec(split_file_index)/))
         call cam_pio_handle_error(ierr, 'config_write_time_dependent_variables: cannot write "ncsec" variable')
         countc(1) = 2
         if (split_file_index == accumulated_file_index .and. .not. restart .and. .not. is_initfile) then
            ! accumulated tape - time is midpoint of time_bounds

            ierr=pio_put_var (this%hist_files(split_file_index), this%timeid, (/start/),(/count1/), &
               (/(time_interval(1) + time_interval(2)) / 2._r8/))
            call cam_pio_handle_error(ierr, 'config_write_time_dependent_variables: cannot write midpoint "time" variable')
         else
            ! not an accumulated history tape - time is current time
            ierr=pio_put_var (this%hist_files(split_file_index), this%timeid, (/start/),(/count1/),(/time/))
            call cam_pio_handle_error(ierr, 'config_write_time_dependent_variables: cannot write instantaneous "time" variable')
         end if
         ierr=pio_put_var (this%hist_files(split_file_index), this%tbndid, startc, countc, time_interval)
         call cam_pio_handle_error(ierr, 'config_write_time_dependent_variables: cannot write "time_bounds" variable')
         countc(1) = 8
         ierr = pio_put_var (this%hist_files(split_file_index), this%date_writtenid, startc, countc, (/cdate/))
         call cam_pio_handle_error(ierr, 'config_write_time_dependent_variables: cannot write "cdate" variable')
         ierr = pio_put_var (this%hist_files(split_file_index), this%time_writtenid, startc, countc, (/ctime/))
         call cam_pio_handle_error(ierr, 'config_write_time_dependent_variables: cannot write "ctime" variable')
         do field_idx = 1, size(this%field_list)
            ! we may have a history split, conditionally skip fields that are
            ! for the other file
            if ((this%field_list(field_idx)%accumulate_type() .eq. 'lst') .and. &
               split_file_index == accumulated_file_index .and. .not. restart) then
               cycle
            else if ((this%field_list(field_idx)%accumulate_type() .ne. 'lst') .and. &
               split_file_index == instantaneous_file_index .and. .not. restart) then
               cycle
            end if
            call this%write_field(this%field_list(field_idx), split_file_index, restart, start, field_idx)
         end do
      end do
      call t_stopf  ('write_field')

   end subroutine config_write_time_dependent_variables

   ! ========================================================================

   subroutine config_write_field(this, field, split_file_index, restart, &
      sample_index, field_index)
      use pio,                 only: PIO_OFFSET_KIND, pio_setframe
      use hist_buffer,         only: hist_buffer_t
      use hist_api,            only: hist_buffer_norm_value
      use cam_grid_support,    only: cam_grid_write_dist_array
      use cam_abortutils,      only: check_allocate, endrun
      use hist_field,          only: hist_field_info_t
      ! Dummy arguments
      class(hist_file_t),      intent(inout) :: this
      type(hist_field_info_t), intent(inout) :: field
      integer,                 intent(in)    :: split_file_index
      logical,                 intent(in)    :: restart
      integer,                 intent(in)    :: sample_index
      integer,                 intent(in)    :: field_index

      ! Local variables
      integer, allocatable           :: field_shape(:) ! Field file dim sizes
      integer                        :: frank          ! Field file rank
      integer                        :: field_shape_temp
      integer, allocatable           :: dimind(:)
      integer, allocatable           :: dim_sizes(:)
      integer, allocatable           :: beg_dims(:)
      integer, allocatable           :: end_dims(:)
      integer                        :: patch_idx, num_patches, ierr
      type(var_desc_t)               :: varid
      integer                        :: field_decomp
      integer                        :: idx
      real(r8), allocatable          :: field_data(:,:)
      class(hist_buffer_t), pointer  :: buff_ptr
      character(len=*), parameter    :: subname = 'config_write_field: '

      !!! Get the field's shape and decomposition
      ! Shape on disk
      field_shape = field%shape()
      beg_dims = field%beg_dims()
      end_dims = field%end_dims()
      frank = size(field_shape)
      if (frank == 1) then
         allocate(field_data(end_dims(1) - beg_dims(1) + 1, 1), stat=ierr)
         call check_allocate(ierr, subname, 'field_data', file=__FILE__, line=__LINE__-1)
      else
         allocate(field_data(end_dims(1) - beg_dims(1) + 1, field_shape(2)), stat=ierr)
         call check_allocate(ierr, subname, 'field_data', file=__FILE__, line=__LINE__-1)
      end if
      ! Shape of array
      dimind = field%dimensions()

      allocate(dim_sizes(size(beg_dims)), stat=ierr)
      call check_allocate(ierr, subname, 'dim_sizes', file=__FILE__, line=__LINE__-1)
      do idx = 1, size(beg_dims)
         dim_sizes(idx) = end_dims(idx) - beg_dims(idx) + 1
      end do
      field_decomp = field%decomp()

      num_patches = 1

      do patch_idx = 1, num_patches
         varid = this%file_varids(field_index, patch_idx)
         call pio_setframe(this%hist_files(split_file_index), varid, int(sample_index,kind=PIO_OFFSET_KIND))
         buff_ptr => field%buffers
         if (frank == 1) then
            call hist_buffer_norm_value(buff_ptr, field_data(:,1))
            call cam_grid_write_dist_array(this%hist_files(split_file_index), field_decomp, dim_sizes(1: frank), &
                 field_shape, field_data(:,1), varid)
         else
            call hist_buffer_norm_value(buff_ptr, field_data)
            call cam_grid_write_dist_array(this%hist_files(split_file_index), field_decomp, dim_sizes(1: frank), &
                 field_shape, field_data, varid)
         end if
      end do

   end subroutine config_write_field

   ! ========================================================================

   subroutine config_close_files(this)
      use pio,           only: pio_file_is_open
      use cam_pio_utils, only: cam_pio_closefile
      use spmd_utils,    only: masterproc
      use cam_logfile,   only: iulog
      ! Dummy arguments
      class(hist_file_t), intent(inout) :: this

      ! Local variables
      integer :: split_file_index

      if(pio_file_is_open(this%hist_files(accumulated_file_index)) .or. &
                 pio_file_is_open(this%hist_files(instantaneous_file_index))) then
         deallocate(this%file_varids)
      end if

      do split_file_index = 1, max_split_files
         if (pio_file_is_open(this%hist_files(split_file_index))) then
            if (masterproc) then
               write(iulog,*)'config_close_files: nf_close(', trim(this%volume),')=',&
                  this%file_names(split_file_index)
            end if
            call cam_pio_closefile(this%hist_files(split_file_index))
         end if
      end do

      if (allocated(this%file_names)) then
         deallocate(this%file_names)
      end if
      this%files_open = .false.

   end subroutine config_close_files

   ! ========================================================================

   subroutine config_clear_buffers(this)
      use hist_msg_handler,    only: hist_log_messages
      use spmd_utils,          only: masterproc
      use cam_logfile,         only: iulog
      ! Dummy arguments
      class(hist_file_t), intent(inout) :: this
      ! Local variables
      integer :: field_idx
      type(hist_log_messages) :: errors


      do field_idx = 1, size(this%field_list)
         call this%field_list(field_idx)%clear_buffers(logger=errors)
         if (masterproc) then
            call errors%output(iulog)
         end if
      end do

   end subroutine config_clear_buffers

   ! ========================================================================

   pure function count_array(arr_in) result(arr_count)
      ! Dummy arguments
      character(len=*), intent(in) :: arr_in(:)
      integer                      :: arr_count
      ! Local variable
      integer :: index

      arr_count = 0
      do index = 1, size(arr_in)
         if (len_trim(arr_in(index)) > 0) then
            arr_count = arr_count + 1
         else
            exit
         end if
      end do
   end function count_array

   ! ========================================================================

   subroutine read_namelist_entry(unitn, hfile_config, hist_inst_fields,      &
        hist_avg_fields, hist_min_fields, hist_max_fields, hist_var_fields)
      use mpi,            only: MPI_CHARACTER, MPI_INTEGER, MPI_LOGICAL
      use string_utils,   only: stringify
      use spmd_utils,     only: masterproc, masterprocid, mpicom
      use shr_nl_mod,     only: shr_nl_find_group_name
      use cam_abortutils, only: endrun
      use shr_kind_mod,   only: CM => shr_kind_cm
      use ISO_FORTRAN_ENV,only: REAL32
      ! Read a history file configuration from <unitn> and process it into
      ! <hfile_config>.
      ! <hist_inst_fields>, <avg_fields>, <min_fields>, <max_fields>, & <var_fields>
      !    are provided to ensure enough room to read all namelist field entries.
      ! This routine assumes that <unitn> is positioned at the beginning of
      !    a history file configuration namelist entry
      ! Dummy arguments
      integer,           intent(inout) :: unitn
      type(hist_file_t), intent(inout) :: hfile_config
      character(len=max_fldlen), allocatable, intent(inout) :: hist_inst_fields(:)
      character(len=max_fldlen), allocatable, intent(inout) :: hist_avg_fields(:)
      character(len=max_fldlen), allocatable, intent(inout) :: hist_min_fields(:)
      character(len=max_fldlen), allocatable, intent(inout) :: hist_max_fields(:)
      character(len=max_fldlen), allocatable, intent(inout) :: hist_var_fields(:)
      ! Local variables (namelist)
      character(len=vlen) :: hist_volume
      character(len=vlen) :: hist_precision
      integer             :: hist_max_frames
      character(len=flen) :: hist_output_frequency
      logical             :: hist_collect_patch_output
      character(len=flen) :: hist_file_type
      character(len=CL)   :: hist_filename_spec
      logical             :: hist_write_nstep0
      ! Local variables (other)
      integer             :: ierr
      character(len=CM)   :: errmsg
      integer             :: num_fields_inst
      integer             :: num_fields_avg
      integer             :: num_fields_min
      integer             :: num_fields_max
      integer             :: num_fields_var
      integer             :: file_type
      integer             :: rl_kind
      ! XXgoldyXX: Add patch information
      logical             :: hist_interp_out
      integer             :: hist_interp_nlat
      integer             :: hist_interp_nlon
      character(len=flen) :: hist_interp_grid
      character(len=flen) :: hist_interp_type
      character(len=*), parameter :: subname = 'hist:read_namelist_entry: '

      namelist /hist_file_config_nl/ hist_inst_fields, hist_avg_fields,       &
           hist_min_fields, hist_max_fields, hist_var_fields, hist_volume,    &
           hist_precision, hist_max_frames, hist_output_frequency,            &
           hist_file_type, hist_collect_patch_output,                         &
           hist_interp_out, hist_interp_nlat, hist_interp_nlon,               &
           hist_interp_grid, hist_interp_type, hist_filename_spec,            &
           hist_write_nstep0

      ! Initialize namelist entries to default values
      hist_inst_fields(:) = ''
      hist_avg_fields(:) = ''
      hist_min_fields(:) = ''
      hist_max_fields(:) = ''
      hist_var_fields(:) = ''
      hist_volume = UNSET_C
      hist_precision = UNSET_C
      hist_max_frames = UNSET_I
      hist_output_frequency = UNSET_C
      hist_collect_patch_output = .true.
      hist_file_type = UNSET_C
      hist_interp_out = .false.
      hist_interp_nlat = 0
      hist_interp_nlon = 0
      hist_interp_grid = UNSET_C
      hist_interp_type = UNSET_C
      file_type = hfile_type_default
      hist_filename_spec = UNSET_C
      hist_write_nstep0 = .false.

      ! Read namelist entry
      if (masterproc) then
         read(unitn, hist_file_config_nl, iostat=ierr, iomsg=errmsg)
         if (ierr /= 0) then
            call endrun(subname//"ERROR "//trim(errmsg)//' (code= '//trim(stringify((/ierr/)))// &
                 ") reading namelist", file=__FILE__, line=__LINE__)
         end if
         ! Translate <file_type>
         select case(trim(hist_file_type))
         case(UNSET_C, 'history')
            file_type = hfile_type_history
         case('initial_value')
            file_type = hfile_type_init_value
         case('restart')
            file_type = hfile_type_restart
         case('satellite')
            file_type = hfile_type_sat_track
         case default
            call endrun(subname//"ERROR, Invalid history file type, '"//      &
                 trim(hist_file_type)//"'", file=__FILE__, line=__LINE__)
         end select
         ! Translate precision into rl_kind
         rl_kind = UNSET_I
         select case(trim(hist_precision))
         case('REAL32')
            rl_kind = REAL32
         case('REAL64')
            rl_kind = REAL64
         case default
            call endrun(subname//"ERROR, Invalid precision, '"//              &
                 trim(hist_precision)//"'", file=__FILE__, line=__LINE__)
         end select
         num_fields_inst = count_array(hist_inst_fields)
         num_fields_avg = count_array(hist_avg_fields)
         num_fields_min = count_array(hist_min_fields)
         num_fields_max = count_array(hist_max_fields)
         num_fields_var = count_array(hist_var_fields)
      end if
      call MPI_Bcast(num_fields_inst, 1, MPI_INTEGER, masterprocid, mpicom, ierr)
      call MPI_Bcast(num_fields_avg, 1, MPI_INTEGER, masterprocid, mpicom, ierr)
      call MPI_Bcast(num_fields_min, 1, MPI_INTEGER, masterprocid, mpicom, ierr)
      call MPI_Bcast(num_fields_max, 1, MPI_INTEGER, masterprocid, mpicom, ierr)
      call MPI_Bcast(num_fields_var, 1, MPI_INTEGER, masterprocid, mpicom, ierr)
      ! Broadcast namelist data
      if (num_fields_inst > 0) then
         call MPI_Bcast(hist_inst_fields(:), max_fldlen*num_fields_inst, MPI_CHARACTER,       &
              masterprocid, mpicom, ierr)
      end if
      if (num_fields_avg > 0) then
         call endrun(subname//"ERROR, average fields not yet implemented",     &
               file=__FILE__, line=__LINE__)
         call MPI_Bcast(hist_avg_fields(:), max_fldlen*num_fields_avg, MPI_CHARACTER,        &
              masterprocid, mpicom, ierr)
      end if
      if (num_fields_min > 0) then
         call endrun(subname//"ERROR, minimum fields not yet implemented",     &
               file=__FILE__, line=__LINE__)
         call MPI_Bcast(hist_min_fields(:), max_fldlen*num_fields_min, MPI_CHARACTER,        &
              masterprocid, mpicom, ierr)
      end if
      if (num_fields_max > 0) then
         call endrun(subname//"ERROR, maximum fields not yet implemented",     &
               file=__FILE__, line=__LINE__)
         call MPI_Bcast(hist_max_fields(:), max_fldlen*num_fields_max, MPI_CHARACTER,        &
              masterprocid, mpicom, ierr)
      end if
      if (num_fields_var > 0) then
         call endrun(subname//"ERROR, standard deviation fields not yet implemented",     &
               file=__FILE__, line=__LINE__)
         call MPI_Bcast(hist_var_fields(:), max_fldlen*num_fields_var, MPI_CHARACTER,        &
              masterprocid, mpicom, ierr)
      end if
      call MPI_Bcast(hist_volume, vlen, MPI_CHARACTER, masterprocid,          &
           mpicom, ierr)

      call MPI_Bcast(rl_kind, 1, MPI_INTEGER, masterprocid, mpicom, ierr)
      call MPI_Bcast(hist_max_frames, 1, MPI_INTEGER, masterprocid,           &
           mpicom, ierr)
      call MPI_Bcast(hist_output_frequency, flen, MPI_CHARACTER,              &
           masterprocid, mpicom, ierr)
      call MPI_Bcast(hist_collect_patch_output, 1, MPI_LOGICAL,               &
           masterprocid, mpicom, ierr)
      call MPI_Bcast(hist_write_nstep0, 1, MPI_LOGICAL,                       &
           masterprocid, mpicom, ierr)
      call MPI_Bcast(file_type, 1, MPI_INTEGER, masterprocid, mpicom, ierr)
      call MPI_Bcast(hist_interp_grid, flen, MPI_CHARACTER,                   &
           masterprocid, mpicom, ierr)
      call MPI_Bcast(hist_interp_type, flen, MPI_CHARACTER,                   &
           masterprocid, mpicom, ierr)
      call MPI_Bcast(hist_filename_spec, CL, MPI_CHARACTER,                   &
           masterprocid, mpicom, ierr)
      ! Configure the history file
      call hfile_config%configure(hist_volume, rl_kind, hist_max_frames,      &
           hist_output_frequency, file_type, hist_filename_spec,              &
           hist_collect_patch_output, hist_inst_fields, hist_avg_fields,      &
           hist_min_fields, hist_max_fields, hist_var_fields,                 &
           hist_write_nstep0, interp_out=hist_interp_out,  &
           interp_nlat=hist_interp_nlat, interp_nlon=hist_interp_nlon,        &
           interp_grid=hist_interp_grid, interp_type=hist_interp_type)
      call hfile_config%print_config()

   end subroutine read_namelist_entry

   ! ========================================================================

   subroutine allocate_field_arrays(unitn, hist_inst_fields,                  &
        hist_avg_fields, hist_min_fields, hist_max_fields, hist_var_fields)
      use mpi,            only: MPI_INTEGER
      use shr_nl_mod,     only: shr_nl_find_group_name
      use cam_logfile,    only: iulog
      use spmd_utils,     only: mpicom, masterproc, masterprocid
      use cam_abortutils, only: check_allocate, endrun
      use shr_kind_mod,   only: CM => shr_kind_cm
      ! Read the maximum sizes of field arrays from namelist file and allocate
      !  field arrays
      ! Dummy arguments
      integer,                   intent(inout)            :: unitn
      character(len=max_fldlen), intent(out), allocatable :: hist_inst_fields(:)
      character(len=max_fldlen), intent(out), allocatable :: hist_avg_fields(:)
      character(len=max_fldlen), intent(out), allocatable :: hist_min_fields(:)
      character(len=max_fldlen), intent(out), allocatable :: hist_max_fields(:)
      character(len=max_fldlen), intent(out), allocatable :: hist_var_fields(:)
      ! Local variables
      integer                     :: ierr
      integer                     :: hist_num_inst_fields
      integer                     :: hist_num_avg_fields
      integer                     :: hist_num_min_fields
      integer                     :: hist_num_max_fields
      integer                     :: hist_num_var_fields
      character(len=CM)           :: errmsg
      character(len=CM)           :: io_errmsg
      character(len=*), parameter :: subname = 'allocate_field_arrays'

      namelist /hist_config_arrays_nl/ hist_num_inst_fields,                  &
           hist_num_avg_fields, hist_num_min_fields, hist_num_max_fields,     &
           hist_num_var_fields

      ! Initialize data
      hist_num_inst_fields = 0
      hist_num_avg_fields = 0
      hist_num_min_fields = 0
      hist_num_max_fields = 0
      hist_num_var_fields = 0
      if (allocated(hist_inst_fields)) then
         deallocate(hist_inst_fields)
      end if
      if (allocated(hist_avg_fields)) then
         deallocate(hist_avg_fields)
      end if
      if (allocated(hist_min_fields)) then
         deallocate(hist_min_fields)
      end if
      if (allocated(hist_max_fields)) then
         deallocate(hist_max_fields)
      end if
      if (allocated(hist_var_fields)) then
         deallocate(hist_var_fields)
      end if
      if (masterproc) then
         rewind(unitn)
         call shr_nl_find_group_name(unitn, 'hist_config_arrays_nl', ierr)
         if (ierr == 0) then
            read(unitn, hist_config_arrays_nl, iostat=ierr, iomsg=io_errmsg)
            if (ierr /= 0) then
               write(errmsg, '(2a,i0,4a)') subname, ": ERROR ", ierr,          &
                    " reading namelist, hist_config_arrays_nl",                &
                    " (error message= '", trim(io_errmsg), "')"
               call endrun(trim(errmsg))
            end if
         else
            write(iulog, *) subname, ": WARNING, no hist_config_arrays_nl ",  &
                 "namelist found"
         end if
      end if
      ! Broadcast data
      call MPI_Bcast(hist_num_inst_fields, 1, MPI_INTEGER, masterprocid,      &
           mpicom, ierr)
      call MPI_Bcast(hist_num_avg_fields,  1, MPI_INTEGER, masterprocid,      &
           mpicom, ierr)
      call MPI_Bcast(hist_num_min_fields,  1, MPI_INTEGER, masterprocid,      &
           mpicom, ierr)
      call MPI_Bcast(hist_num_max_fields,  1, MPI_INTEGER, masterprocid,      &
           mpicom, ierr)
      call MPI_Bcast(hist_num_var_fields,  1, MPI_INTEGER, masterprocid,      &
           mpicom, ierr)
      ! Allocate arrays
      allocate(hist_inst_fields(hist_num_inst_fields), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'hist_inst_fields',                  &
           file=__FILE__, line=__LINE__-1)
      allocate(hist_avg_fields(hist_num_avg_fields), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'hist_avg_fields',                   &
           file=__FILE__, line=__LINE__-1)
      allocate(hist_min_fields(hist_num_min_fields), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'hist_min_fields',                   &
           file=__FILE__, line=__LINE__-1)
      allocate(hist_max_fields(hist_num_max_fields), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'hist_max_fields',                   &
           file=__FILE__, line=__LINE__-1)
      allocate(hist_var_fields(hist_num_var_fields), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'hist_var_fields',                   &
           file=__FILE__, line=__LINE__-1)

   end subroutine allocate_field_arrays

   ! ========================================================================

   subroutine hist_read_namelist_config(filename, config_arr)
      use mpi,            only: MPI_CHARACTER, MPI_INTEGER
      use shr_kind_mod,   only: max_str =>SHR_KIND_CXX, CM => shr_kind_cm
      use shr_nl_mod,     only: shr_nl_find_group_name
      use spmd_utils,     only: masterproc, masterprocid, mpicom
      use cam_abortutils, only: check_allocate, endrun
      ! Read all the history configuration namelist groups from  <filename>
      !    and return an array of config objects
      ! Note: File operations are done on the root task with results
      !       broadcast to other tasks.

      ! Dummy arguments
      character(len=*), intent(in) :: filename
      type(hist_file_t), allocatable, intent(inout) :: config_arr(:)
      ! Local variables
      integer                                :: unitn
      integer                                :: read_status
      integer                                :: ierr
      integer                                :: line_num
      integer                                :: lindex
      integer                                :: num_configs
      logical                                :: filefound
      character(len=max_fldlen), allocatable :: hist_inst_fields(:)
      character(len=max_fldlen), allocatable :: hist_avg_fields(:)
      character(len=max_fldlen), allocatable :: hist_min_fields(:)
      character(len=max_fldlen), allocatable :: hist_max_fields(:)
      character(len=max_fldlen), allocatable :: hist_var_fields(:)
      character(len=max_str)                 :: config_line
      character(len=CM)                      :: errmsg
      character(len=CM)                      :: io_errmsg
      character(len=*),          parameter   :: subname = 'read_config_file'

      ! Variables for reading a namelist entry
      unitn = -1 ! Prevent reads on error or wrong tasks
      ierr = 0

      errmsg = ''
      if (masterproc) then
         inquire(file=trim(filename), exist=filefound)
         if (.not. filefound) then
            write(config_line, *)                                             &
                 ": ERROR: could not find history config file '",               &
                 trim(filename), "'"
            call endrun(subname//trim(config_line))
         else
            open(newunit=unitn, file=trim(filename), status='old', iostat=ierr)
            if (ierr /= 0) then
               write(errmsg, '(a,i0,2a)') ": Error ", ierr, " opening ",      &
                     trim(filename)
            end if
            line_num = 0
         end if
      end if
      ! First, count up the number of history configs in this file
      num_configs = 0
      line_num = 0
      if (masterproc .and. filefound) then
         do
            ! Look for an instance of the history configure group
            call shr_nl_find_group_name(unitn, hist_nl_group_name, read_status)
            if (read_status == 0) then
               ! We found a history config, count it
               num_configs = num_configs + 1
               ! shr_nl_find_group_name leaves the file pointer at the beginning
               !      of the namelist, move past for the next search
               read(unitn, '(a)', iostat=read_status, iomsg=io_errmsg) config_line
               ! Check that the read did not cause trouble
               if (read_status > 0) then
                  write(errmsg, '(a,i0,5a)') ": Error (", read_status,        &
                       " - '", trim(io_errmsg), "') from '", trim(filename), "'"
                  close(unitn)
                  call endrun(subname//trim(errmsg))
               else if (read_status < 0) then
                  ! We reached the end of the file, just quit
                  exit
               end if ! No else, we just look for the next group
            else
               ! We are done with this file
               exit
            end if
         end do
      end if
      ! All tasks allocate the history config file objects
      call MPI_bcast(num_configs, 1, MPI_INTEGER, masterprocid, mpicom, ierr)
      allocate(config_arr(num_configs), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'config_arr',                        &
           file=__FILE__, line=__LINE__-1)
      ! Allocate the config field name arrays
      call allocate_field_arrays(unitn, hist_inst_fields, hist_avg_fields,    &
           hist_min_fields, hist_max_fields, hist_var_fields)
      ! Now, step through each config file namelist entry, read, and process
      if (masterproc) then
         ! Start from beginning of file
         rewind(unit=unitn)
      end if
      do lindex = 1, num_configs
         if (masterproc) then
            ! Look for an instance of the history configure group
            call shr_nl_find_group_name(unitn, hist_nl_group_name, read_status)
            if (read_status /= 0) then
               write(errmsg, '(2a,i0,3a)') subname,                           &
                    ": ERROR finding history config namelist #", lindex,      &
                    " in '", trim(filename), "'"
               close(unitn)
               call endrun(trim(errmsg))
            end if
         end if
         call read_namelist_entry(unitn, config_arr(lindex),                  &
              hist_inst_fields, hist_avg_fields, hist_min_fields,             &
              hist_max_fields, hist_var_fields)
      end do
      !
      ! Cleanup
      !
      ! Close unitn if it is still open
      inquire(unit=unitn, opened=filefound, iostat=ierr)
      if ((ierr == 0) .and. filefound) then
         close(unitn)
      end if
      if (allocated(hist_inst_fields)) then
         deallocate(hist_inst_fields)
      end if
      if (allocated(hist_avg_fields)) then
         deallocate(hist_avg_fields)
      end if
      if (allocated(hist_min_fields)) then
         deallocate(hist_min_fields)
      end if
      if (allocated(hist_max_fields)) then
         deallocate(hist_max_fields)
      end if
      if (allocated(hist_var_fields)) then
         deallocate(hist_var_fields)
      end if
   end subroutine hist_read_namelist_config

   pure function strip_suffix(name) result(stripped)
      use cam_history_support, only: fieldname_len
      !
      !----------------------------------------------------------
      !
      ! Purpose:  Strip "&IC" suffix from fieldnames if it exists
      !
      !----------------------------------------------------------
      !
      ! Arguments
      !
      character(len=*), intent(in) :: name
      character(len=max_fldlen)    :: stripped
      !
      ! Local workspace
      !
      integer :: n
      !
      !-----------------------------------------------------------------------
      !
      stripped = ' '

      do n = 1,fieldname_len
         stripped(n:n) = name(n:n)
         if(name(n+1:n+1         ) == ' '                       ) return
         if(name(n+1:n+fieldname_suffix_len) == fieldname_suffix) return
      end do

      stripped(fieldname_len+1:max_fldlen) = name(fieldname_len+1:max_fldlen)

  end function strip_suffix

  !#######################################################################

end module cam_hist_file
