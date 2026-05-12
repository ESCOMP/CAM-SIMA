module cam_hist_restart
   use pio,                 only: var_desc_t
   use cam_history_support, only: max_fieldname_len
   use shr_kind_mod,        only: r4 => shr_kind_r4
   use shr_kind_mod,        only: r8 => shr_kind_r8
   use cam_logfile,         only: iulog

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
    integer, parameter :: num_restart_vars = 20
    integer, parameter :: num_restart_dims =  9
    type(restart_variable_t)  :: restart_vars(num_restart_vars)
    type(restart_dimension_t) :: restart_dims(num_restart_dims)

    integer, parameter :: max_num_fields = 1000

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

CONTAINS

   subroutine hist_restart_init(restart_file, num_hist_configs, max_fields)
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

   subroutine hist_restart_write(restart_file, hist_configs, max_num_fields, just_written)
      use pio,                 only: file_desc_t, pio_put_var
      use cam_hist_file,       only: hist_file_t
      use cam_history_support, only: max_chars, max_string_len, get_hist_coord_names
      use cam_grid_support,    only: max_split_files, max_hcoordname_len
      use cam_abortutils,      only: endrun
      type(file_desc_t), intent(inout) :: restart_file
      type(hist_file_t), intent(in)    :: hist_configs(:)
      integer,           intent(in)    :: max_num_fields
      logical,           intent(in)    :: just_written(:)

      ! Local variables
      integer :: idx, jdx, ierr
      integer :: has_rh_int(size(hist_configs))
      integer :: num_fields(size(hist_configs))
      integer :: num_frames(size(hist_configs))
      integer :: max_frames(size(hist_configs))
      integer :: ndims(max_num_fields)
      integer :: decomp(max_num_fields, size(hist_configs))
      integer :: num_levels(max_num_fields, size(hist_configs))
      integer :: fill_flag(max_num_fields, size(hist_configs))
      integer :: dimensions(max_dimensions, max_num_fields, size(hist_configs))
      character(len=max_fieldname_len) :: field_list(max_num_fields, size(hist_configs))
      character(len=max_fieldname_len), allocatable :: field_list_config(:)
      character(len=max_chars) :: output_freq(size(hist_configs))
      character(len=max_string_len) :: current_files(size(hist_configs), max_split_files)
      character(len=max_chars) :: hist_precision(size(hist_configs))
      character(len=max_chars) :: avg_flag(max_num_fields, size(hist_configs))
      character(len=max_chars) :: long_name(max_num_fields, size(hist_configs))
      character(len=max_chars) :: cell_methods(max_num_fields, size(hist_configs))
      character(len=max_chars) :: units(max_num_fields, size(hist_configs))
      character(len=max_hcoordname_len) :: dim_names(max_dimensions)
      real(r8) :: beg_time(size(hist_configs))
      real(r8) :: fill_value(max_num_fields, size(hist_configs))
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
      ! Compile all the necessary info for the restart file from the hist_configs array
      do idx = 1, size(hist_configs)
          ! Grab the field list
          field_list_config = hist_configs(idx)%get_field_list()
          num_fields(idx) = hist_configs(idx)%get_num_fields()
          field_list(1:num_fields(idx), idx) = field_list_config(1:num_fields(idx))
          ! Determine whether or not there will be an rh file
          if (hist_configs(idx)%has_accumulated_fields() .and. .not. just_written(idx)) then
             has_rh_int(idx) = 1
          end if
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
      write(iulog,*) 'peverwhee - dim_names'
      write(iulog,*) dim_names

      ! Loop over the restart vars and write them to the file
      do idx = 1, num_restart_vars
         select case(trim(restart_vars(idx)%var_name))
         case ('has_rh_file')
            ierr = pio_put_var(restart_file, restart_vars(idx)%vdesc, has_rh_int)            
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
         case ('history_restart_path')
         case default
         end select
         if (ierr /= 0) then
            call endrun('hist_restart_write: failed to write variable '//restart_vars(idx)%var_name)
         end if
      end do
   end subroutine hist_restart_write

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
      restart_vars(rvar_index)%var_name = 'history_restart_path'
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
