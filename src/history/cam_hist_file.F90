module cam_hist_file
   ! Module to define and read CAM history configuration namelist entries
   !    and associated history files
   ! Note: In test mode, endrun does not abort so there are a few lines
   !          of special code to cleanly return after an endrun call.

   use ISO_FORTRAN_ENV,     only: REAL64, REAL32
   use pio,                 only: file_desc_t
   use cam_history_support, only: max_fldlen=>max_fieldname_len
   use cam_interp_mod,      only: interp_info_t=>hist_interp_info_t

   implicit none
   private

   public :: hist_file_t
   public :: hist_read_namelist_config

   character(len=*), parameter :: hist_nl_group_name = 'hist_file_config_nl'
   integer,          parameter :: nl_gname_len = len(hist_nl_group_name)

   logical, parameter, private             :: PATCH_DEF = .true.
   integer, parameter, private             :: OUTPUT_DEF = REAL64
   integer, parameter, private             :: vlen = 8
   integer, parameter, private             :: flen = 16
   integer, parameter, private             :: tlen = 16
   integer, parameter, private             :: UNSET_I = -1
   character(len=vlen), parameter, private :: UNSET_C = 'UNSET'

   integer, parameter, private             :: hfile_type_default    = -1
   integer, parameter, private             :: hfile_type_history    =  1
   integer, parameter, private             :: hfile_type_init_value =  2
   integer, parameter, private             :: hfile_type_sat_track  =  3
   integer, parameter, private             :: hfile_type_restart    =  4

   type :: hist_file_t
      ! History file configuration information
      character(len=vlen),           private :: volume = UNSET_C
      integer,                       private :: rl_kind = OUTPUT_DEF
      integer,                       private :: max_frames = UNSET_I
      integer,                       private :: output_freq_mult = UNSET_I
      character(len=8),              private :: output_freq_type = UNSET_C
      character(len=*), allocatable, private :: filename_spec
      integer,                       private :: hfile_type = hfile_type_default
      logical,                       private :: collect_patch_output = PATCH_DEF
      type(interp_info_t), pointer,  private :: interp_info => NULL()
      ! History file information
      type(file_desc_t),            private  :: hist_file
   contains
      ! Accessors
      procedure :: filename => config_filename
      procedure :: precision => config_precision
      procedure :: max_frame => config_max_frame
      procedure :: output_freq => config_output_freq
      procedure :: is_history_file => config_history_file
      procedure :: is_initial_value_file => config_init_value_file
      procedure :: is_satellite_file => config_satellite_file
      procedure :: is_hist_restart_file => config_restart_file
      ! Actions
      procedure :: reset        => config_reset
      procedure :: configure    => config_configure
      procedure :: print_config => config_print_config
   end type hist_file_t

   private :: count_array         ! Number of non-blank strings in array
   private :: read_namelist_entry ! Read a namelist group and create config

CONTAINS
   !
   ! Filename specifiers for history, initial files and restart history files
   !  %c = caseid,
   !  %y = year,
   !  %m = month,
   !  %d = day,
   !  %s = seconds in day,
   !  %u = unit number (e.g., h0, i)
   !
   ! rhfilename_spec is the templdate for history restart files
   character(len=*), parameter :: rhfilename_spec = '%c.cam.r%u.%y-%m-%d-%s.nc'
   ! hfilename_spec is the template for each history file
   character(len=*), parameter :: hfilename_spec = '%c.cam.%u.%y-%m-%d-%s.nc'

   ! ========================================================================

   function config_filename(this, inst_suffix) result(cfile)
      use shr_kind_mod,  only: CL => SHR_KIND_CL
      use cam_filenames, only: interpret_filename_spec
      ! Dummy arguments
      class(hist_file_t), intent(in) :: this
      character(len=*),   intent(in) :: inst_suffix
      character(len=CL)              :: cfile

      cfile = interpret_filename_spec(this%filename_spec, unit=this%volume)

   end function config_filename

   ! ========================================================================

   function config_precision(this) result(cprec)
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

   integer function config_max_frame(this)
      ! Dummy argument
      class(hist_file_t), intent(in) :: this

      config_max_frame = this%max_frames
   end function config_max_frame

   ! ========================================================================

   function config_output_freq(this) result(out_freq)
      use shr_kind_mod,   only: CL => SHR_KIND_CL, CS => SHR_KIND_CS
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
      case ("monthly")
         out_opt = "month"
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

   logical function config_history_file(this)
      ! Dummy argument
      class(hist_file_t), intent(in) :: this

      config_init_value_file = this%hfile_type == hfile_type_history

   end function config_history_file

   ! ========================================================================

   logical function config_init_value_file(this)
      ! Dummy argument
      class(hist_file_t), intent(in) :: this

      config_init_value_file = this%hfile_type == hfile_type_init_value

   end function config_init_value_file

   ! ========================================================================

   logical function config_satellite_file(this)
      ! Dummy argument
      class(hist_file_t), intent(in) :: this

      config_satellite_file = this%hfile_type == hfile_type_sat_track

   end function config_satellite_file

   ! ========================================================================

   logical function config_restart_file(this)
      ! Dummy argument
      class(hist_file_t), intent(in) :: this

      config_satellite_file = this%hfile_type == hfile_type_restart

   end function config_restart_file

   ! ========================================================================

   subroutine config_reset(this)
      ! Dummy argument
      class(hist_file_t), intent(inout) :: this

      this%collect_patch_output = PATCH_DEF
      this%rl_kind = OUTPUT_DEF
      this%max_frames = UNSET_I
      this%output_freq_mult = UNSET_I
      this%output_freq_type = UNSET_C
      this%is_init_val_file = .false.
      this%is_sat_track_file = .false.
      if (associated(this%interp_info)) then
         call this%interp_info%reset()
         deallocate(this%interp_info)
         nullify(this%interp_info)
      end if
   end subroutine config_reset

   ! ========================================================================

   subroutine config_configure(this, volume, out_prec, max_frames,            &
        output_freq, file_type, collect_patch_out, filename_spec,             &
        interp_out, interp_nlat, interp_nlon, interp_grid, interp_type)
      use shr_kind_mod,   only: CL=>SHR_KIND_CL
      use shr_string_mod, only: to_lower => shr_string_toLower
      use cam_abortutils, only: endrun
      use string_utils,   only: parse_multiplier
      ! Dummy arguments
      class(hist_file_t),         intent(inout) :: this
      character(len=*),           intent(in)    :: volume
      integer,                    intent(in)    :: out_prec
      integer,                    intent(in)    :: max_frames
      character(len=*),           intent(in)    :: output_freq
      integer,                    intent(in)    :: file_type
      logical,                    intent(in)    :: collect_patch_out
      character(len*),            intent(in)    :: filename_spec
      logical,          optional, intent(in)    :: interp_out
      integer,          optional, intent(in)    :: interp_nlat
      integer,          optional, intent(in)    :: interp_nlon
      character(len=*), optional, intent(in)    :: interp_grid
      character(len=*), optional, intent(in)    :: interp_type
      ! Local variables
      character(len=CL) :: errmsg
      integer           :: last_char
      character(len=*), parameter :: subname = 'config_configure: '

      call this%reset()

      this%volume = volume
      this%rl_kind = out_prec
      this%max_frames = max_frames
      ! Parse output frequency spec into multiplier and type
      ! Note, the allowed_set should match __TIME_PERIODS in hist_config.py
      call parse_multiplier(output_freq, this%output_freq_mult,               &
           this%output_freq_type, errmsg=errmsg,                              &
           allowed_set=(/ 'nsteps  ', 'nstep   ', 'nseconds', 'nsecond ',     &
           'nminutes', 'nminute ', 'nhours  ', 'nhour   ', 'ndays   ',        &
           'nday    ', 'monthly ', 'nmonths ', 'nmonth  ', 'nyears  ',        &
           'nyear   ', 'steps   ', 'seconds ', 'minutes ', 'hours   ',        &
           'days    ', 'months  ', 'years   ' /))
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
      this%hfile_type = file_type
      this%collect_patch_output = collect_patch_out
      this%filename_spec = filename_spec
      if (present(interp_out)) then
         if (interp_out) then
            allocate(this%interp_info)
            ! To do: write and call interp object creator
         end if
      end if
   end subroutine config_configure

   ! ========================================================================

   subroutine config_print_config(this)
      use spmd_utils,  only: masterproc
      use cam_logfile, only: iulog
      ! Dummy argument
      class(hist_file_t), intent(in) :: this

      if (masterproc) then
         write(iulog, '(2a)') "History configuration for volume = ",           &
              trim(this%volume)
         if (this%is_init_val_file) then
            write(6, '(a)') "  File will contain initial values"
         end if
         if (this%is_sat_track_file) then
            write(6, '(a)') "  File will contain satellite track values"
         end if
         if (this%rl_kind == REAL64) then
            write(iulog, '(a)') "  Ouput precision, 64 bits"
         else if (this%rl_kind == REAL32) then
            write(iulog, '(a)') "  Ouput precision, 32 bits"
         else
            write(iulog, '(a,i0)') "  Unknown output precision, ", this%rl_kind
         end if
         write(6, '(a,i0)') "  Maximum number of output frames per file = ",  &
              this%max_frames
         if (this%output_freq_mult == 1) then
            write(6, *) "  Writing output once per ", trim(this%output_freq_type)
         else
            write(6, '(a,i0,3a)') "  Writing output every ",                  &
                 this%output_freq_mult, " ", trim(this%output_freq_type), "s"
         end if
         !!XXgoldyXX: Fix this when patch output is known
         if (this%collect_patch_output) then
            write(6, '(2a)') "  Output from all patches will be collected ",  &
                 "into a single variable"
         else
            write(6, '(2a)') "  Output from each patch will be written ",     &
                 "as a separate variable"
         end if
         if (associated(this%interp_info)) then
            !!XXgoldyXX: Add interp info
         end if
      end if
   end subroutine config_print_config

   ! ========================================================================

   integer function count_array(arr_in)
      ! Dummy argument
      character(len=*), intent(in) :: arr_in(:)
      ! Local variable
      integer :: index

      count_array = 0
      do index = 1, size(arr_in)
         if (len_trim(arr_in(index)) > 0) then
            count_array = count_array + 1
         else
            exit
         end if
      end do
   end function count_array

   ! ========================================================================

   subroutine read_namelist_entry(unitn, hfile_config, hist_inst_fields,      &
        hist_avg_fields, hist_min_fields, hist_max_fields, hist_var_fields)
      use mpi,            only: MPI_CHARACTER, MPI_INTEGER, MPI_LOGICAL
      use shr_kind_mod,   only: CL=>SHR_KIND_CL
      use string_utils,   only: to_str
      use cam_logfile,    only: iulog
      use cam_abortutils, only: endrun
      use spmd_utils,     only: masterproc, masterprocid, mpicom
      ! Read a history file configuration from <unitn> and process it into
      ! <hfile_config>.
      ! <hist_inst_fields>, <avg_fields>, <min_fields>, <max_fields>, & <var_fields>
      !    are provided to ensure enough room to read all namelist field entries.
      ! This routine assumes that <unitn> is positioned at the beginning of
      !    a history file configuration namelist entry
      ! Dummy arguments
      integer,           intent(inout) :: unitn
      type(hist_file_t), intent(inout) :: hfile_config
      character(len=*),  intent(inout) :: hist_inst_fields(:)
      character(len=*),  intent(inout) :: hist_avg_fields(:)
      character(len=*),  intent(inout) :: hist_min_fields(:)
      character(len=*),  intent(inout) :: hist_max_fields(:)
      character(len=*),  intent(inout) :: hist_var_fields(:)
      ! Local variables (namelist)
      character(len=vlen) :: hist_volume ! h# ir i, not config number
      character(len=vlen) :: hist_precision
      integer             :: hist_max_frames
      character(len=flen) :: hist_output_frequency
      logical             :: hist_collect_patch_output
      character(len=flen) :: hist_file_type
      character(len=CL)   :: hist_filename_spec
      ! Local variables (other)
      integer             :: ierr
      integer             :: num_fields
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
           hist_interp_grid, hist_interp_type, hist_filename_spec

      ! Initialize namelist entries to default values
      hist_inst_fields(:) = ''
      hist_avg_fields(:) = ''
      hist_min_fields(:) = ''
      hist_max_fields(:) = ''
      hist_var_fields(:) = ''
      hist_volume = UNSET_C ! h# ir i, not config number =
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
      ! Read namelist entry
      if (masterproc) then
         read(unitn, hist_file_config_nl, iostat=ierr)
         if (ierr /= 0) then
            call endrun(subname//"ERROR "//trim(to_str(ierr))//               &
                 " reading namelist", file=__FILE__, line=__LINE__)
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
         ! Translat<e precision into rl_kind
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
      end if
      ! Broadcast namelist data
      num_fields = count_array(hist_inst_fields)
      if (num_fields > 0) then
         call MPI_Bcast(hist_inst_fields(:), num_fields, MPI_CHARACTER,       &
              masterprocid, mpicom, ierr)
      end if
      num_fields = count_array(hist_avg_fields)
      if (num_fields > 0) then
         call MPI_Bcast(hist_avg_fields(:), num_fields, MPI_CHARACTER,        &
              masterprocid, mpicom, ierr)
      end if
      num_fields = count_array(hist_min_fields)
      if (num_fields > 0) then
         call MPI_Bcast(hist_min_fields(:), num_fields, MPI_CHARACTER,        &
              masterprocid, mpicom, ierr)
      end if
      num_fields = count_array(hist_max_fields)
      if (num_fields > 0) then
         call MPI_Bcast(hist_max_fields(:), num_fields, MPI_CHARACTER,        &
              masterprocid, mpicom, ierr)
      end if
      num_fields = count_array(hist_var_fields)
      if (num_fields > 0) then
         call MPI_Bcast(hist_var_fields(:), num_fields, MPI_CHARACTER,        &
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
      call MPI_Bcast(file_type, 1, MPI_INTEGER, masterprocid, mpicom, ierr)
      call MPI_Bcast(hist_interp_grid, flen, MPI_CHARACTER,                   &
           masterprocid, mpicom, ierr)
      call MPI_Bcast(hist_interp_type, flen, MPI_CHARACTER,                   &
           masterprocid, mpicom, ierr)
      call MPI_Bcast(hist_filename_spec, CL, MPI_CHARACTER,                   &
           masterprocid, mpicom, ierr)
      ! Configure the history file
      call hfile_config%configure(hist_volume, rl_kind, hist_max_frames,      &
           hist_output_frequency, file_type,                                  &
           hist_collect_patch_output, hist_interp_out, hist_interp_nlat,      &
           hist_interp_nlon, hist_interp_grid, hist_interp_type,              &
           hist_filename_spec)
      call hfile_config%print_config()

   end subroutine read_namelist_entry

   ! ========================================================================

   subroutine allocate_field_arrays(unitn, hist_inst_fields,                  &
        hist_avg_fields, hist_min_fields, hist_max_fields, hist_var_fields)
      use mpi,            only: MPI_INTEGER
      use shr_kind_mod,   only: SHR_KIND_CL
      use shr_nl_mod,     only: shr_nl_find_group_name
      use cam_abortutils, only: endrun
      use string_utils,   only: to_str
      use spmd_utils,     only: mpicom, masterproc, masterprocid
      use cam_abortutils, only: endrun, check_allocate
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
      character(len=SHR_KIND_CL)  :: errmsg
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
            read(unitn, hist_config_arrays_nl, iostat=ierr)
            if (ierr /= 0) then
               write(errmsg, '(2a,i0,a)') subname, ": ERROR ", ierr,         &
                    " reading namelist, hist_config_arrays_nl"
               call endrun(trim(errmsg))
               return ! For testing
            end if
         else
            write(6, *) subname, ": WARNING, no hist_config_arrays_nl ",      &
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
      call check_allocate(ierr, subname, 'hist_inst_fields', errmsg=errmsg,   &
           file=__FILE__, line=__LINE__-1)
      allocate(hist_avg_fields(hist_num_avg_fields), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'hist_avg_fields', errmsg=errmsg,    &
           file=__FILE__, line=__LINE__-1)
      allocate(hist_min_fields(hist_num_min_fields), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'hist_min_fields', errmsg=errmsg,    &
           file=__FILE__, line=__LINE__-1)
      allocate(hist_max_fields(hist_num_max_fields), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'hist_max_fields', errmsg=errmsg,    &
           file=__FILE__, line=__LINE__-1)
      allocate(hist_var_fields(hist_num_var_fields), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'hist_var_fields', errmsg=errmsg,    &
           file=__FILE__, line=__LINE__-1)

   end subroutine allocate_field_arrays

   ! ========================================================================

   function hist_read_namelist_config(filename) result(config_arr)
      use mpi,            only: MPI_CHARACTER, MPI_INTEGER
      use shr_kind_mod,   only: max_str =>SHR_KIND_CXX, SHR_KIND_CS, SHR_KIND_CL
      use shr_nl_mod,     only: shr_nl_find_group_name
      use cam_abortutils, only: endrun, check_allocate
      use spmd_utils,     only: masterproc, masterprocid, mpicom
      use string_utils,   only: to_str
      ! Read all the history configuration namelist groups from  <filename>
      !    and return an array of config objects
      ! Note: File operations are done on the root task with results
      !       broadcast to other tasks.

      ! Dummy arguments
      character(len=*), intent(in) :: filename
      type(hist_file_t), pointer   :: config_arr(:)
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
      character(len=SHR_KIND_CL)             :: errmsg
      character(len=*),          parameter   :: subname = 'read_config_file'
      ! Variables for reading a namelist entry

      nullify(config_arr)
      unitn = -1 ! Prevent reads on error or wrong tasks
      ierr = 0
      errmsg = ''
      if (masterproc) then
         inquire(file=trim(filename), exist=filefound)
         if (.not. filefound) then
            write(config_line, *)                                             &
                 "ERROR: could not find history config file '",               &
                 trim(filename), "'"
            call endrun(subname//trim(config_line))
            return ! Needed for testing
         else
            open(newunit=unitn, file=trim(filename), status='old', iostat=ierr)
            line_num = 0
         end if
      end if
      call MPI_bcast(ierr, 1, MPI_INTEGER, masterprocid, mpicom, ierr)
      if (ierr /= 0) then
         write(errmsg, '(a,i0,2a)') ": Error ", ierr, " opening ",            &
              trim(filename)
         call endrun(subname//trim(errmsg))
         return ! Needed for testing
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
               read(unitn, '(a)', iostat=read_status) config_line
               ! Check that the read did not cause trouble
               if (read_status > 0) then
                  write(errmsg, '(a,i0,3a)') ": Error (", read_status,        &
                       ") from '", trim(filename), "'"
                  close(unitn)
                  num_configs = - read_status ! Used for testing
                  call endrun(subname//trim(errmsg))
                  return ! Neede for testing
               else if (read_status < 0) then
                  ! We reached the end of the file, just quit
                  exit
               end if ! No else, we just look for the next group
            else
               ! We are done with this file
               exit
            end if
         end do
      else
         config_line = '' ! For testing
      end if
      ! All tasks allocate the history config file objects
      call MPI_bcast(num_configs, 1, MPI_INTEGER, masterprocid, mpicom, ierr)
      ! This block is used for testing
      if ((num_configs < 0) .or. (LEN_TRIM(errmsg) > 0)) then
         call endrun(subname//trim(errmsg))
         return ! Needed for testing
      end if
      allocate(config_arr(num_configs), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'config_arr', errmsg=errmsg,         &
           file=__FILE__, line=__LINE__-2)
      ! This block is needed for testing
      if (ierr /= 0) then
         return
      end if ! End test
      ! This block is needed for testing
      if (masterproc) then
         inquire(unit=unitn, opened=filefound, iostat=ierr)
         if ((ierr > 0) .or. (.not. filefound)) then
            return
         end if
      end if ! End test
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
      ! Special block for testing
      call MPI_bcast(read_status, 1, MPI_INTEGER, masterprocid, mpicom, ierr)
      if (read_status /= 0) then
         return
      end if
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
   end function hist_read_namelist_config

end module cam_hist_file
