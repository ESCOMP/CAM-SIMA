module cam_hist_restart
   use pio,                 only: var_desc_t
   use cam_history_support, only: max_fieldname_len
   use shr_kind_mod,        only: r4 => shr_kind_r4
   use shr_kind_mod,        only: r8 => shr_kind_r8
   use cam_hist_file,       only: hist_file_t

   implicit none
   private

   integer, parameter :: max_dimensions = 4

   type restart_variable_t
      type(var_desc_t), pointer        :: vdesc => null()
      integer                          :: var_type
      integer                          :: number_of_dimensions
      integer                          :: dimension_ids(max_dimensions)
      character(len=max_fieldname_len) :: var_name
      logical                          :: fill_set = .false.
      integer                          :: integer_fill
      real(r4)                         :: real_fill
      real(r8)                         :: double_fill
    end type restart_variable_t

    type restart_dimension_t
      integer                          :: dimension_length
      integer                          :: dimension_id
      character(len=max_fieldname_len) :: dimension_name
    end type restart_dimension_t

    !
    !   The size of these parameters should match the assignments in restart_vars_setnames and restart_dims_setnames below
    !
    integer, parameter :: num_restart_vars = 21
    integer, parameter :: num_restart_dims =  9
    type(restart_variable_t)  :: restart_vars(num_restart_vars)
    type(restart_dimension_t) :: restart_dims(num_restart_dims)

!    integer, parameter :: max_num_fields = 1000

    integer, parameter :: num_configs_dim_ind         =  1
    integer, parameter :: max_string_len_dim_ind      =  2
    integer, parameter :: max_fieldname_len_dim_ind   =  3
    integer, parameter :: max_num_fields_dim_ind      =  4
    integer, parameter :: max_chars_dim_ind           =  5
    integer, parameter :: max_dims_dim_ind            =  6
    integer, parameter :: registeredmdims_dim_ind     =  7
    integer, parameter :: max_hcoordname_len_dim_ind  =  8
    integer, parameter :: max_num_split_files_dim_ind =  9

    private :: set_restart_variable_names
    private :: set_restart_dimension_names

    public :: hist_restart_init
    public :: hist_restart_write
    public :: hist_restart_read

CONTAINS

   subroutine hist_restart_init(restart_file, num_hist_configs, max_fields)
      ! Initialize history restart fields in the overall CAM restart file (.r.)
      use pio,           only: file_desc_t, pio_def_var
      use cam_pio_utils, only: cam_pio_handle_error, cam_pio_def_dim
      type(file_desc_t), intent(inout) :: restart_file
      integer,           intent(in)    :: num_hist_configs
      integer,           intent(in)    :: max_fields

      ! Local variables
      integer :: idx, ndims, kdx, ierr
      integer :: dimids(4)

      ! Set the restart dimensions and variables for writing to the file
      call set_restart_variable_names()
      call set_restart_dimension_names(num_hist_configs, max_fields)

      do idx = 1, num_restart_dims
        ! it's possible that one or more of these have been defined elsewhere
        call cam_pio_def_dim(restart_file, restart_dims(idx)%dimension_name, restart_dims(idx)%dimension_length, &
             restart_dims(idx)%dimension_id, existOK=.true.)
      end do

      do idx = 1, num_restart_vars
        ndims= restart_vars(idx)%number_of_dimensions
        do kdx = 1, ndims
          dimids(kdx)=restart_dims(restart_vars(idx)%dimension_ids(kdx))%dimension_id
        end do
        allocate(restart_vars(idx)%vdesc)
        ierr = pio_def_var(restart_file, restart_vars(idx)%var_name, restart_vars(idx)%var_type, dimids(1:ndims), restart_vars(idx)%vdesc)
        call cam_pio_handle_error(ierr, 'INIT_RESTART_HISTORY: Error defining '//trim(restart_vars(idx)%var_name))
      end do

   end subroutine hist_restart_init

   subroutine hist_restart_write(restart_file, hist_configs, max_fields, just_written)
      ! Write history restart fields to the .r. file
      use pio,                 only: file_desc_t, pio_put_var
      use cam_hist_file,       only: hist_file_t
      use cam_history_support, only: max_chars, max_string_len, get_hist_coord_names, registeredmdims
      use cam_grid_support,    only: max_split_files, max_hcoordname_len
      use cam_abortutils,      only: endrun
      type(file_desc_t), intent(inout) :: restart_file
      type(hist_file_t), intent(in)    :: hist_configs(:)
      integer,           intent(in)    :: max_fields
      logical,           intent(in)    :: just_written(:)

      ! Local variables
      integer :: idx, jdx, ierr
      integer :: has_rh_int(size(hist_configs))
      integer :: num_fields(size(hist_configs))
      integer :: num_frames(size(hist_configs))
      integer :: max_frames(size(hist_configs))
      integer :: ndims(max_fields)
      integer :: decomp(max_fields, size(hist_configs))
      integer :: num_levels(max_fields, size(hist_configs))
      integer :: fill_flag(max_fields, size(hist_configs))
      integer :: dimensions(max_dimensions, max_fields, size(hist_configs))
      character(len=max_fieldname_len) :: field_list(max_fields, size(hist_configs))
      character(len=max_fieldname_len), allocatable :: field_list_config(:)
      character(len=max_chars) :: output_freq(size(hist_configs))
      character(len=max_string_len) :: current_files(size(hist_configs), max_split_files)
      character(len=max_chars) :: hist_precision(size(hist_configs))
      character(len=max_chars) :: avg_flag(max_fields, size(hist_configs))
      character(len=max_chars) :: long_name(max_fields, size(hist_configs))
      character(len=max_chars) :: cell_methods(max_fields, size(hist_configs))
      character(len=max_chars) :: units(max_fields, size(hist_configs))
      character(len=max_chars) :: volume(size(hist_configs))
      character(len=max_hcoordname_len) :: dim_names(registeredmdims)
      character(len=max_string_len) :: restart_file_paths(size(hist_configs))
      real(r8) :: beg_time(size(hist_configs))
      real(r8) :: fill_value(max_fields, size(hist_configs))
      logical :: has_accum

      field_list = ''
      has_rh_int = 0
      avg_flag = ''
      decomp = 0
      num_levels = 0
      cell_methods = ''
      long_name = ''
      units = ''
      fill_flag = 0
      fill_value = 0
      dimensions = 0
      restart_file_paths = ''
      ! Compile all the necessary info for the restart file from the hist_configs array
      do idx = 1, size(hist_configs)
          ! Grab the field list
          field_list_config = hist_configs(idx)%get_field_list()
          num_fields(idx) = hist_configs(idx)%get_num_fields()
          field_list(1:num_fields(idx), idx) = field_list_config(1:num_fields(idx))
          ! Determine whether or not there will be an rh file
          if (hist_configs(idx)%has_accumulated_fields() .and. .not. just_written(idx)) then
             has_rh_int(idx) = 1
             restart_file_paths(idx) = hist_configs(idx)%get_restart_filename()
          end if
          volume(idx) = hist_configs(idx)%get_volume()
          ! Get the output frequency
          output_freq(idx) = hist_configs(idx)%output_freq()
          ! Get the number of samples written to the hist file
          num_frames(idx) = hist_configs(idx)%get_num_samples()
          ! Get the maximum number of samples written to the hist file
          max_frames(idx) = hist_configs(idx)%max_frame()
          ! Get the file names
          current_files(idx,:) = hist_configs(idx)%get_filenames()
          ! Get the precision
          hist_precision(idx) = hist_configs(idx)%precision()
          ! Get the interval start time
          beg_time(idx) = hist_configs(idx)%get_beg_time()
          ! Get field-specific info vv
          ! Get accumulated flags for each field
          avg_flag(1:num_fields(idx), idx) = hist_configs(idx)%get_averaging_flags()
          ! Get field decompositions
          decomp(1:num_fields(idx), idx) = hist_configs(idx)%get_decompositions()
          ! Get num vertical levels
          num_levels(1:num_fields(idx), idx) = hist_configs(idx)%get_num_levels()
          ! Get cell methods
          cell_methods(1:num_fields(idx), idx) = hist_configs(idx)%get_cell_methods()
          ! Get long names
          long_name(1:num_fields(idx), idx) = hist_configs(idx)%get_long_names()
          ! Get units
          units(1:num_fields(idx), idx) = hist_configs(idx)%get_units()
          ! Get fill value flag and fill value
          fill_flag(1:num_fields(idx), idx) = hist_configs(idx)%get_fill_flags()
          fill_value(1:num_fields(idx), idx) = hist_configs(idx)%get_fill_values()
          ! Get field dimension indices
          ndims(1:num_fields(idx)) = hist_configs(idx)%get_num_dimensions()
          do jdx = 1, num_fields(idx)
             dimensions(1:ndims(jdx), 1:num_fields(idx), idx) = hist_configs(idx)%get_dimension_indices()
          end do
          ! End field specific info ^^
      end do

      ! Grab the dimension names
      dim_names = get_hist_coord_names()

      ! Loop over the restart vars and write them to the file
      do idx = 1, num_restart_vars
         select case(trim(restart_vars(idx)%var_name))
         case ('has_rh_file')
            ierr = pio_put_var(restart_file, restart_vars(idx)%vdesc, has_rh_int)
         case ('volume')
            ierr = pio_put_var(restart_file, restart_vars(idx)%vdesc, volume)
         case ('output_frequency')
            ierr = pio_put_var(restart_file, restart_vars(idx)%vdesc, output_freq)
         case ('field_list')
            ierr = pio_put_var(restart_file, restart_vars(idx)%vdesc, field_list)
         case ('number_of_fields')
            ierr = pio_put_var(restart_file, restart_vars(idx)%vdesc, num_fields)
         case ('number_of_frames')
            ierr = pio_put_var(restart_file, restart_vars(idx)%vdesc, num_frames)
         case ('max_frames')
            ierr = pio_put_var(restart_file, restart_vars(idx)%vdesc, max_frames)
         case ('current_file_name')
            ierr = pio_put_var(restart_file, restart_vars(idx)%vdesc, current_files)
         case ('precision')
            ierr = pio_put_var(restart_file, restart_vars(idx)%vdesc, hist_precision)
         case ('interval_start_time')
            ierr = pio_put_var(restart_file, restart_vars(idx)%vdesc, beg_time)
         case ('field_average_flag')
            ierr = pio_put_var(restart_file, restart_vars(idx)%vdesc, avg_flag)
         case ('field_decomposition_type')
            ierr = pio_put_var(restart_file, restart_vars(idx)%vdesc, decomp)
         case ('field_num_vertical_levels')
            ierr = pio_put_var(restart_file, restart_vars(idx)%vdesc, num_levels)
         case ('field_cell_methods')
            ierr = pio_put_var(restart_file, restart_vars(idx)%vdesc, cell_methods)
         case ('field_long_name')
            ierr = pio_put_var(restart_file, restart_vars(idx)%vdesc, long_name)
         case ('field_units')
            ierr = pio_put_var(restart_file, restart_vars(idx)%vdesc, units)
         case ('field_fill_flag')
            ierr = pio_put_var(restart_file, restart_vars(idx)%vdesc, fill_flag)
         case ('field_fill_value')
            ierr = pio_put_var(restart_file, restart_vars(idx)%vdesc, fill_value)
         case ('field_dimensions')
            ierr = pio_put_var(restart_file, restart_vars(idx)%vdesc, dimensions)
         case ('dimension_names')
            ierr = pio_put_var(restart_file, restart_vars(idx)%vdesc, dim_names)
         case ('rh_file_path')
            ierr = pio_put_var(restart_file, restart_vars(idx)%vdesc, restart_file_paths)
         case default
         end select
         if (ierr /= 0) then
            call endrun('hist_restart_write: failed to write variable '//trim(restart_vars(idx)%var_name))
         end if
      end do
   end subroutine hist_restart_write

   subroutine hist_restart_read(restart_file, hist_configs, has_rh, restart_file_paths)
      ! Read history fields from the .r. file
      use pio,            only: file_desc_t, pio_inq_varid, pio_seterrorhandling, pio_get_var
      use pio,            only: PIO_BCAST_ERROR, pio_inq_dimid, PIO_INTERNAL_ERROR, pio_inq_dimlen
      use cam_hist_file,  only: hist_file_t
      use cam_abortutils, only: endrun, check_allocate
      use cam_logfile,    only: iulog
      use spmd_utils,     only: masterproc
      use cam_history_support, only: max_chars, max_string_len, registeredmdims
      use cam_grid_support,    only: max_split_files, max_hcoordname_len
      use, intrinsic :: ISO_FORTRAN_ENV, only: REAL32, REAL64
      type(file_desc_t), intent(inout)  :: restart_file
      type(hist_file_t), intent(inout)  :: hist_configs(:)
      logical, allocatable, intent(out) :: has_rh(:)
      character(len=max_string_len), allocatable, intent(out) :: restart_file_paths(:)
      ! Local variables
      type(hist_file_t) :: rest_config
      integer :: idx, fld_idx, ierr
      integer :: num_configs_id, max_fields_id
      integer :: rl_kind
      type(var_desc_t) :: vdesc
      integer, allocatable :: has_rh_int(:)
      integer, allocatable :: num_fields(:)
      integer, allocatable :: num_frames(:)
      integer, allocatable :: max_frames(:)
      integer, allocatable :: ndims(:)
      integer, allocatable :: decomp(:,:)
      integer, allocatable :: num_levels(:,:)
      integer, allocatable :: fill_flag(:,:)
      integer, allocatable :: dimensions(:,:,:)
      character(len=max_fieldname_len), allocatable :: field_list(:,:)
      character(len=max_fieldname_len), allocatable :: field_list_config(:)
      character(len=max_chars), allocatable :: output_freq(:)
      character(len=max_string_len), allocatable :: current_files(:,:)
      character(len=max_chars), allocatable :: hist_precision(:)
      character(len=max_chars), allocatable :: avg_flag(:,:)
      character(len=max_chars), allocatable :: long_name(:,:)
      character(len=max_chars), allocatable :: cell_methods(:,:)
      character(len=max_chars), allocatable :: units(:,:)
      character(len=max_chars), allocatable :: volume(:)
      character(len=max_hcoordname_len), allocatable :: dim_names(:)
      real(r8), allocatable :: beg_time(:)
      real(r8), allocatable :: fill_value(:,:)
      integer :: max_fields
      integer :: num_configs
      character(len=256) :: errmsg
      character(len=max_fieldname_len), allocatable :: inst_fields(:)
      character(len=max_fieldname_len), allocatable :: avg_fields(:)
      character(len=max_fieldname_len), allocatable :: var_fields(:)
      character(len=max_fieldname_len), allocatable :: min_fields(:)
      character(len=max_fieldname_len), allocatable :: max_fields_list(:)
      integer :: inst_fields_idx
      integer :: avg_fields_idx
      integer :: var_fields_idx
      integer :: min_fields_idx
      integer :: max_fields_idx
      character(len=*), parameter :: subname = 'hist_restart_read'

      ! Check if the restart (.r.) file has history variables on it
      ! If not, no action needed (original run had no history variables)
      call pio_seterrorhandling(restart_file, PIO_BCAST_ERROR)

      ierr = pio_inq_varid(restart_file, 'has_rh_file', vdesc)
      if (ierr /= 0) then
         if (masterproc) then
            write(iulog,*) 'Not reading history info from the restart file.'
         end if ! No history info in restart (.r.) file
         return
      end if

      call pio_seterrorhandling(restart_file, PIO_INTERNAL_ERROR)

      ! Grab dimensions from file
      ierr = pio_inq_dimid(restart_file, 'number_of_hist_configs', num_configs_id)
      ierr = pio_inq_dimlen(restart_file, num_configs_id, num_configs)

      ierr = pio_inq_dimid(restart_file, 'max_fields_per_configuration', max_fields_id)
      ierr = pio_inq_dimlen(restart_file, max_fields_id, max_fields)

      ! Set the restart dimensions and variables for reading from the file
      call set_restart_variable_names()
      call set_restart_dimension_names(num_configs, max_fields)

      ! Allocate variables for reading
      allocate(has_rh_int(num_configs), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'has_rh_int', file=__FILE__, &
            line=__LINE__-1, errmsg=errmsg)
      allocate(has_rh(num_configs), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'has_rh', file=__FILE__, &
            line=__LINE__-1, errmsg=errmsg)
      allocate(num_fields(num_configs), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'num_fields', file=__FILE__, &
            line=__LINE__-1, errmsg=errmsg)
      allocate(num_frames(num_configs), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'num_frames', file=__FILE__, &
            line=__LINE__-1, errmsg=errmsg)
      allocate(max_frames(num_configs), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'max_frames', file=__FILE__, &
            line=__LINE__-1, errmsg=errmsg)
      allocate(dimensions(max_dimensions, max_fields, num_configs), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'dimensions', file=__FILE__, &
            line=__LINE__-1, errmsg=errmsg)
      allocate(volume(num_configs), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'volume', file=__FILE__, &
            line=__LINE__-1, errmsg=errmsg)
      allocate(output_freq(num_configs), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'output_freq', file=__FILE__, &
            line=__LINE__-1, errmsg=errmsg)
      allocate(field_list(max_fields, num_configs), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'field_list', file=__FILE__, &
            line=__LINE__-1, errmsg=errmsg)
      allocate(current_files(num_configs, max_split_files), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'current_files', file=__FILE__, &
            line=__LINE__-1, errmsg=errmsg)
      allocate(hist_precision(num_configs), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'hist_precision', file=__FILE__, &
            line=__LINE__-1, errmsg=errmsg)
      allocate(beg_time(num_configs), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'beg_time', file=__FILE__, &
            line=__LINE__-1, errmsg=errmsg)
      allocate(avg_flag(max_fields, num_configs), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'avg_flag', file=__FILE__, &
            line=__LINE__-1, errmsg=errmsg)
      allocate(decomp(max_fields, num_configs), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'decomp', file=__FILE__, &
            line=__LINE__-1, errmsg=errmsg)
      allocate(num_levels(max_fields, num_configs), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'num_levels', file=__FILE__, &
            line=__LINE__-1, errmsg=errmsg)
      allocate(cell_methods(max_fields, num_configs), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'cell_methods', file=__FILE__, &
            line=__LINE__-1, errmsg=errmsg)
      allocate(long_name(max_fields, num_configs), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'long_name', file=__FILE__, &
            line=__LINE__-1, errmsg=errmsg)
      allocate(units(max_fields, num_configs), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'units', file=__FILE__, &
            line=__LINE__-1, errmsg=errmsg)
      allocate(fill_flag(max_fields, num_configs), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'fill_flag', file=__FILE__, &
            line=__LINE__-1, errmsg=errmsg)
      allocate(fill_value(max_fields, num_configs), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'fill_value', file=__FILE__, &
            line=__LINE__-1, errmsg=errmsg)
      allocate(dim_names(registeredmdims), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'dim_names', file=__FILE__, &
            line=__LINE__-1, errmsg=errmsg)
      allocate(restart_file_paths(num_configs), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'restart_file_paths', file=__FILE__, &
            line=__LINE__-1, errmsg=errmsg)

      ! Loop over the restart vars and read them from the file
      do idx = 1, num_restart_vars
         ! Confirm the variable is on the .r. file
         ierr = pio_inq_varid(restart_file, restart_vars(idx)%var_name, vdesc)
         select case(trim(restart_vars(idx)%var_name))
         case ('has_rh_file')
            ierr = pio_get_var(restart_file, vdesc, has_rh_int)
            has_rh = (has_rh_int == 1)
         case ('volume')
            ierr = pio_get_var(restart_file, vdesc, volume)
         case ('output_frequency')
            ierr = pio_get_var(restart_file, vdesc, output_freq)
         case ('field_list')
            ierr = pio_get_var(restart_file, vdesc, field_list)
         case('number_of_fields')
            ierr = pio_get_var(restart_file, vdesc, num_fields)
         case('number_of_frames')
            ierr = pio_get_var(restart_file, vdesc, num_frames)
         case('max_frames')
            ierr = pio_get_var(restart_file, vdesc, max_frames)
         case('current_file_name')
            ierr = pio_get_var(restart_file, vdesc, current_files)
         case('precision')
            ierr = pio_get_var(restart_file, vdesc, hist_precision)
         case('interval_start_time')
            ierr = pio_get_var(restart_file, vdesc, beg_time)
         case('field_average_flag')
            ierr = pio_get_var(restart_file, vdesc, avg_flag)
         case('field_decomposition_type')
            ierr = pio_get_var(restart_file, vdesc, decomp)
         case('field_num_vertical_levels')
            ierr = pio_get_var(restart_file, vdesc, num_levels)
         case('field_cell_methods')
            ierr = pio_get_var(restart_file, vdesc, cell_methods)
         case('field_long_name')
            ierr = pio_get_var(restart_file, vdesc, long_name)
         case('field_units')
            ierr = pio_get_var(restart_file, vdesc, units)
         case('field_fill_flag')
            ierr = pio_get_var(restart_file, vdesc, fill_flag)
         case('field_fill_value')
            ierr = pio_get_var(restart_file, vdesc, fill_value)
         case('field_dimensions')
            ierr = pio_get_var(restart_file, vdesc, dimensions)
         case('dimension_names')
            ierr = pio_get_var(restart_file, vdesc, dim_names)
         case('rh_file_path')
            ierr = pio_get_var(restart_file, vdesc, restart_file_paths)
         case default
            write(errmsg,*) subname, ': Missing history restart variable ', trim(restart_vars(idx)%var_name)
            call endrun(errmsg)
         end select
      end do

      allocate(inst_fields(max_fields), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'inst_fields', file=__FILE__, &
            line=__LINE__-1, errmsg=errmsg)
      allocate(avg_fields(max_fields), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'avg_fields', file=__FILE__, &
            line=__LINE__-1, errmsg=errmsg)
      allocate(var_fields(max_fields), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'var_fields', file=__FILE__, &
            line=__LINE__-1, errmsg=errmsg)
      allocate(min_fields(max_fields), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'min_fields', file=__FILE__, &
            line=__LINE__-1, errmsg=errmsg)
      allocate(max_fields_list(max_fields), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'max_fields_list', file=__FILE__, &
            line=__LINE__-1, errmsg=errmsg)
      inst_fields_idx = 1
      avg_fields_idx = 1
      var_fields_idx = 1
      min_fields_idx = 1
      max_fields_idx = 1

      do idx = 1, size(hist_configs)
         if (trim(hist_precision(idx)) == 'REAL32') then
            rl_kind = REAL32
         else if (trim(hist_precision(idx)) == 'REAL64') then
            rl_kind = REAL64
         else
            write(errmsg,*) subname, ': Invalid precision for volume ', trim(volume(idx)), ': ', trim(hist_precision(idx))
            call endrun(errmsg)
         end if
         ! Grab field lists by flag
         do fld_idx = 1, num_fields(idx)
            if (trim(avg_flag(fld_idx, idx)) == 'avg') then
               avg_fields(avg_fields_idx) = field_list(fld_idx, idx)
               avg_fields_idx = avg_fields_idx + 1
            else if (trim(avg_flag(fld_idx, idx)) == 'lst') then
               inst_fields(inst_fields_idx) = field_list(fld_idx, idx)
               inst_fields_idx = inst_fields_idx + 1
            else if (trim(avg_flag(fld_idx, idx)) == 'min') then
               min_fields(min_fields_idx) = field_list(fld_idx, idx)
               min_fields_idx = min_fields_idx + 1
            else if (trim(avg_flag(fld_idx, idx)) == 'max') then
               max_fields_list(max_fields_idx) = field_list(fld_idx, idx)
               max_fields_idx = max_fields_idx + 1
            else if (trim(avg_flag(fld_idx, idx)) == 'var') then
               var_fields(var_fields_idx) = field_list(fld_idx, idx)
               var_fields_idx = var_fields_idx + 1
            else
               write(errmsg,*) subname, ': Invalid averaging flag for field ', trim(field_list(fld_idx,idx)), ' flag = ', trim(avg_flag(fld_idx,idx))
               call endrun(errmsg)
            end if
         end do

         ! Make sure no configs have changed from previous run
         call rest_config%configure(volume(idx), rl_kind, max_frames(idx), output_freq(idx), &
             4, '', .false., inst_fields(:inst_fields_idx - 1), avg_fields(:avg_fields_idx - 1), &
             min_fields(:min_fields_idx - 1), max_fields_list(:max_fields_idx - 1), var_fields(:var_fields_idx - 1), .false.)
         call rest_config%check_restart_consistency(hist_configs(idx))

         ! Overwrite current stats from restart file
         call hist_configs(idx)%overwrite_restart_info(num_frames(idx), current_files(idx,:))

         call rest_config%reset()

         min_fields_idx = 1
         max_fields_idx = 1
         avg_fields_idx = 1
         var_fields_idx = 1
         inst_fields_idx = 1

      end do

   end subroutine hist_restart_read

   subroutine set_restart_variable_names()
      use pio, only: pio_int, pio_double, pio_char
      integer :: rvar_index

      rvar_index = 1
      restart_vars(rvar_index)%var_name = 'has_rh_file'
      restart_vars(rvar_index)%var_type = pio_int
      restart_vars(rvar_index)%number_of_dimensions = 1
      restart_vars(rvar_index)%dimension_ids(1) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'output_frequency'
      restart_vars(rvar_index)%var_type = pio_char
      restart_vars(rvar_index)%number_of_dimensions = 2
      restart_vars(rvar_index)%dimension_ids(1) = max_chars_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'volume'
      restart_vars(rvar_index)%var_type = pio_char
      restart_vars(rvar_index)%number_of_dimensions = 2
      restart_vars(rvar_index)%dimension_ids(1) = max_chars_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'field_list'
      restart_vars(rvar_index)%var_type = pio_char
      restart_vars(rvar_index)%number_of_dimensions = 3
      restart_vars(rvar_index)%dimension_ids(1) = max_fieldname_len_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = max_num_fields_dim_ind
      restart_vars(rvar_index)%dimension_ids(3) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'number_of_fields'
      restart_vars(rvar_index)%var_type = pio_int
      restart_vars(rvar_index)%number_of_dimensions = 1
      restart_vars(rvar_index)%dimension_ids(1) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'number_of_frames'
      restart_vars(rvar_index)%var_type = pio_int
      restart_vars(rvar_index)%number_of_dimensions = 1
      restart_vars(rvar_index)%dimension_ids(1) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'max_frames'
      restart_vars(rvar_index)%var_type = pio_int
      restart_vars(rvar_index)%number_of_dimensions = 1
      restart_vars(rvar_index)%dimension_ids(1) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'current_file_name'
      restart_vars(rvar_index)%var_type = pio_char
      restart_vars(rvar_index)%number_of_dimensions = 3
      restart_vars(rvar_index)%dimension_ids(1) = max_string_len_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = num_configs_dim_ind
      restart_vars(rvar_index)%dimension_ids(3) = max_num_split_files_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'precision'
      restart_vars(rvar_index)%var_type = pio_char
      restart_vars(rvar_index)%number_of_dimensions = 2
      restart_vars(rvar_index)%dimension_ids(1) = max_chars_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'interval_start_time'
      restart_vars(rvar_index)%var_type = pio_double
      restart_vars(rvar_index)%number_of_dimensions = 1
      restart_vars(rvar_index)%dimension_ids(1) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'field_average_flag'
      restart_vars(rvar_index)%var_type = pio_char
      restart_vars(rvar_index)%number_of_dimensions = 3
      restart_vars(rvar_index)%dimension_ids(1) = max_chars_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = max_num_fields_dim_ind
      restart_vars(rvar_index)%dimension_ids(3) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'field_decomposition_type'
      restart_vars(rvar_index)%var_type = pio_int
      restart_vars(rvar_index)%number_of_dimensions = 2
      restart_vars(rvar_index)%dimension_ids(1) = max_num_fields_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = num_configs_dim_ind
      restart_vars(rvar_index)%fill_set = .true.
      restart_vars(rvar_index)%integer_fill = 0

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'field_num_vertical_levels'
      restart_vars(rvar_index)%var_type = pio_int
      restart_vars(rvar_index)%number_of_dimensions = 2
      restart_vars(rvar_index)%dimension_ids(1) = max_num_fields_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = num_configs_dim_ind
      restart_vars(rvar_index)%fill_set = .true.
      restart_vars(rvar_index)%integer_fill = 0

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'field_cell_methods'
      restart_vars(rvar_index)%var_type = pio_char
      restart_vars(rvar_index)%number_of_dimensions = 3
      restart_vars(rvar_index)%dimension_ids(1) = max_chars_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = max_num_fields_dim_ind
      restart_vars(rvar_index)%dimension_ids(3) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'field_long_name'
      restart_vars(rvar_index)%var_type = pio_char
      restart_vars(rvar_index)%number_of_dimensions = 3
      restart_vars(rvar_index)%dimension_ids(1) = max_chars_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = max_num_fields_dim_ind
      restart_vars(rvar_index)%dimension_ids(3) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'field_units'
      restart_vars(rvar_index)%var_type = pio_char
      restart_vars(rvar_index)%number_of_dimensions = 3
      restart_vars(rvar_index)%dimension_ids(1) = max_chars_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = max_num_fields_dim_ind
      restart_vars(rvar_index)%dimension_ids(3) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'field_fill_flag'
      restart_vars(rvar_index)%var_type = pio_int
      restart_vars(rvar_index)%number_of_dimensions = 2
      restart_vars(rvar_index)%dimension_ids(1) = max_num_fields_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'field_fill_value'
      restart_vars(rvar_index)%var_type = pio_double
      restart_vars(rvar_index)%number_of_dimensions = 2
      restart_vars(rvar_index)%dimension_ids(1) = max_num_fields_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = num_configs_dim_ind
      restart_vars(rvar_index)%fill_set = .true.
      restart_vars(rvar_index)%double_fill = 0.0_r8

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'field_dimensions'
      restart_vars(rvar_index)%var_type = pio_int
      restart_vars(rvar_index)%number_of_dimensions = 3
      restart_vars(rvar_index)%dimension_ids(1) = max_dims_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = max_num_fields_dim_ind
      restart_vars(rvar_index)%dimension_ids(3) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'dimension_names'
      restart_vars(rvar_index)%var_type = pio_char
      restart_vars(rvar_index)%number_of_dimensions = 2
      restart_vars(rvar_index)%dimension_ids(1) = max_hcoordname_len_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = registeredmdims_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'rh_file_path'
      restart_vars(rvar_index)%var_type = pio_char
      restart_vars(rvar_index)%number_of_dimensions = 2
      restart_vars(rvar_index)%dimension_ids(1) = max_string_len_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = num_configs_dim_ind

   end subroutine set_restart_variable_names

   subroutine set_restart_dimension_names(num_hist_configs, max_fields)
      use cam_history_support, only: max_string_len, max_chars, max_fieldname_len, registeredmdims
      use cam_grid_support,    only: max_hcoordname_len, max_split_files
      integer, intent(in) :: num_hist_configs
      integer, intent(in) :: max_fields

      restart_dims(num_configs_dim_ind)%dimension_name = 'number_of_hist_configs'
      restart_dims(num_configs_dim_ind)%dimension_length = num_hist_configs

      restart_dims(max_string_len_dim_ind)%dimension_name = 'max_string_length'
      restart_dims(max_string_len_dim_ind)%dimension_length = max_string_len

      restart_dims(max_fieldname_len_dim_ind)%dimension_name = 'max_fieldname_length'
      restart_dims(max_fieldname_len_dim_ind)%dimension_length = max_fieldname_len

      restart_dims(max_num_fields_dim_ind)%dimension_name = 'max_fields_per_configuration'
      restart_dims(max_num_fields_dim_ind)%dimension_length = max_fields

      restart_dims(max_chars_dim_ind)%dimension_name = 'max_chars'
      restart_dims(max_chars_dim_ind)%dimension_length = max_chars

      restart_dims(max_dims_dim_ind)%dimension_name = 'max_variable_mdims'
      restart_dims(max_dims_dim_ind)%dimension_length = max_dimensions

      restart_dims(registeredmdims_dim_ind)%dimension_name = 'registered_mdims'
      restart_dims(registeredmdims_dim_ind)%dimension_length = registeredmdims

      restart_dims(max_hcoordname_len_dim_ind)%dimension_name = 'max_hcoordname_len'
      restart_dims(max_hcoordname_len_dim_ind)%dimension_length = max_hcoordname_len

      restart_dims(max_num_split_files_dim_ind)%dimension_name = 'max_num_split_files'
      restart_dims(max_num_split_files_dim_ind)%dimension_length = max_split_files

   end subroutine set_restart_dimension_names

end module cam_hist_restart
