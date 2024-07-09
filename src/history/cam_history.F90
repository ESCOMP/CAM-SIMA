module cam_history
   !----------------------------------------------------------------------------
   !
   ! The cam_history module provides the user interface for CAM's history
   !   output capabilities.
   !
   ! It maintains the list of possible fields and provides interfaces
   !   to cam_hist_file hist_file_t object
   !
   !-----------------------------------------------------------------------

   use shr_kind_mod,         only: cl=>SHR_KIND_CL, cxx=>SHR_KIND_CXX
   use cam_hist_file,        only: hist_file_t
   use cam_history_support,  only: pfiles
   use hist_field,           only: hist_field_info_t
   use hist_hash_table,      only: hist_hash_table_t

   implicit none
   private
   save

   character(len=cl) :: model_doi_url = '' ! Model DOI
   character(len=cl) :: caseid = ''        ! case ID
   character(len=cl) :: ctitle = ''        ! case title
   character(len=16) :: logname            ! user name
   character(len=16) :: host               ! host name

   ! Functions
   public :: history_readnl         ! Namelist reader for CAM history
   public :: history_write_files    ! Write files out
   public :: history_init_files     ! Initialization
   public :: history_add_field      ! Write to list of possible history fields for this run
   public :: history_out_field      ! Accumulate field if its in use by one or more tapes
   public :: history_wrap_up        ! Process history files at end of timestep or run

   interface history_out_field
      module procedure history_out_field_1d
      module procedure history_out_field_2d
      module procedure history_out_field_3d
   end interface history_out_field

   interface history_add_field
      module procedure history_add_field_1d
      module procedure history_add_field_nd
   end interface history_add_field

   ! Private data
   type(hist_file_t), allocatable   :: hist_configs(:)
   type(hist_field_info_t), pointer :: possible_field_list_head
   type(hist_hash_table_t)          :: possible_field_list
   integer                          :: num_possible_fields

CONTAINS

   subroutine history_readnl(nlfile)
      !-----------------------------------------------------------------------
      !
      ! Purpose: Read in history namelist and set hist_configs
      !
      !-----------------------------------------------------------------------
      use spmd_utils,    only: masterproc, masterprocid, mpicom
      use mpi,           only: mpi_integer, mpi_logical, mpi_character
      use cam_hist_file, only: hist_read_namelist_config
      use time_manager,  only: get_step_size

      ! Dummy argument
      character(len=*), intent(in) :: nlfile ! path of namelist input file

      !
      ! Local variables
      integer                      :: dtime  ! Step time in seconds
      integer                      :: out_unit, err_cnt
      character(len=8)             :: ctemp  ! Temporary character string
      character(len=512)           :: test_msg

      ! Read in CAM history configuration
      call hist_read_namelist_config(nlfile, hist_configs)

   end subroutine history_readnl

   !===========================================================================

   subroutine history_write_files()
      !-----------------------------------------------------------------------
      !
      ! Purpose: Check if it's time to write any files and Write any active
      !          fields to those files
      !
      !-----------------------------------------------------------------------
      use time_manager,     only: get_curr_date, get_nstep
      use time_manager,     only: get_step_size
      use cam_grid_support, only: max_split_files
      use cam_logfile,      only: iulog
      use cam_abortutils,   only: endrun
      use spmd_utils,       only: masterproc
      use shr_kind_mod,     only: r8 => shr_kind_r8
      character(len=cl) :: file_names(max_split_files)
      character(len=cl) :: prev_file_names(max_split_files)
      integer           :: yr, mon, day
      integer           :: nstep, dtime, nstep_freq
      integer           :: ncsec
      integer           :: num_samples
      integer           :: file_idx, prev_file_idx, idx
      integer           :: out_frq_mult
      integer           :: last_month_written
      integer           :: last_year_written
      character(len=8)  :: out_frq_type
      logical           :: write_history, write_nstep0, duplicate
      character(len=cl) :: filename_spec, prev_filename_spec
      logical           :: restart
      logical           :: month_changed

      ! Get nstep
      nstep = get_nstep()

      ! Get timestep size (in seconds)
      dtime = get_step_size()

      ! Get current time
      call get_curr_date(yr, mon, day, ncsec)

      ! peverwhee - TODO: remove when restarts are implemented
      restart = .false.

      ! Loop over history volumes
      do file_idx = 1, size(hist_configs)
         ! Determine if it's time to write!
         if (nstep == 0) then
            call hist_configs(file_idx)%set_last_year_written(yr)
            call hist_configs(file_idx)%set_last_month_written(mon)
         end if
         write_history = .false.
         month_changed = .false.
         call hist_configs(file_idx)%output_freq_separate(out_frq_mult, out_frq_type)
         select case(trim(out_frq_type))
            case('step')
               if (mod(nstep, out_frq_mult) == 0) then
                  write_history = .true.
               end if
            case('second')
               nstep_freq = out_frq_mult / dtime
               if (mod(nstep, nstep_freq) == 0) then
                  write_history = .true.
               end if
            case('minute')
               nstep_freq = nint((out_frq_mult * 60._r8) / dtime)
               if (mod(nstep, nstep_freq) == 0) then
                  write_history = .true.
               end if
            case('hour')
               nstep_freq = nint((out_frq_mult * 3600._r8) / dtime)
               if (mod(nstep, nstep_freq) == 0) then
                  write_history = .true.
               end if
            case('day')
               nstep_freq = nint((out_frq_mult * 86400._r8) / dtime)
               if (mod(nstep, nstep_freq) == 0) then
                  write_history = .true.
               end if
            case('month')
               ! Determine if it has been out_frq_mult months since the
               !  last write
               last_month_written = hist_configs(file_idx)%get_last_month_written()
               if (mon < last_month_written) then
                  mon = mon + 12
                  month_changed = .true.
               end if
               if ((mon - last_month_written == out_frq_mult) .and. ncsec == 0 .and. day == 1) then
                  write_history = .true.
                  if (month_changed) then
                     last_month_written = mon - 12
                  else
                     last_month_written = mon
                  end if
                  call hist_configs(file_idx)%set_last_month_written(last_month_written)
               end if
            case('year')
               ! Determine if it has been out_frq_mult years since the
               !  last write
               last_year_written = hist_configs(file_idx)%get_last_year_written()
               if ((yr - last_year_written == out_frq_mult) .and. ncsec == 0 .and. day == 1 .and. &
                  mon == 1) then
                  write_history = .true.
                  call hist_configs(file_idx)%set_last_year_written(yr)
               end if
         end select
         write_nstep0 = hist_configs(file_idx)%do_write_nstep0()
         if (nstep == 0) then
            if (write_nstep0) then
               write_history = .true.
            else
               write_history = .false.
            end if
         end if
         if (.not. write_history) then
            ! Don't write this volume!
            cycle
         end if
         num_samples = hist_configs(file_idx)%get_num_samples()
         if (mod(num_samples, hist_configs(file_idx)%max_frame()) == 0) then
            ! This if the first write to this file - set up volume
            call hist_configs(file_idx)%set_filenames()
            file_names = hist_configs(file_idx)%get_filenames()
            duplicate = .false.
            do prev_file_idx = 1, file_idx - 1
               prev_file_names = hist_configs(prev_file_idx)%filename()
               do idx = 1, max_split_files
                  if (prev_file_names(idx) == file_names(idx)) then
                     duplicate = .true.
                     if (masterproc) then
                        write(iulog,*)'history_write_files: New filename same as old file = ', trim(file_names(idx))
                     end if
                  end if
               end do
            end do
            if (duplicate) then
               filename_spec = hist_configs(file_idx)%get_filename_spec()
               prev_filename_spec = hist_configs(prev_file_idx)%get_filename_spec()
               if (masterproc) then
                  write(iulog,*)'Is there an error in your filename specifiers?'
                  write(iulog,*)'filename_spec(', file_idx, ') = ', trim(filename_spec)
                  if ( prev_file_idx /= file_idx )then
                    write(iulog,*)'filename_spec(', prev_file_idx, ') = ', trim(prev_filename_spec)
                  end if
               end if
               call endrun('history_write_files: ERROR - see atm log file for information')
            end if
            call hist_configs(file_idx)%define_file(restart, logname, host, model_doi_url)
         end if
         call hist_configs(file_idx)%write_time_dependent_variables(restart)
      end do

   end subroutine history_write_files

   !===========================================================================

   subroutine history_init_files(model_doi_url_in, caseid_in, ctitle_in)

      !-----------------------------------------------------------------------
      !
      ! Purpose: Print master field list and initialize history files
      !
      !-----------------------------------------------------------------------
      use shr_sys_mod,      only: shr_sys_getenv
      use time_manager,     only: get_prev_time, get_curr_time
      use spmd_utils,       only: mpicom, masterprocid, masterproc
      use mpi,              only: mpi_character
      !
      !-----------------------------------------------------------------------
      !
      ! Dummy argument
      !
      character(len=cl), intent(in) :: model_doi_url_in
      character(len=cl), intent(in) :: caseid_in
      character(len=cl), intent(in) :: ctitle_in
      !
      ! Local workspace
      !
      integer :: file_idx          ! file, field indices
      integer :: day, sec          ! day and seconds from base date
      integer :: rcode             ! shr_sys_getenv return code

      !
      ! Save the DOI
      !
      model_doi_url = trim(model_doi_url_in)
      caseid        = caseid_in
      ctitle        = ctitle_in

      ! Print out the list of possible fields
      call print_field_list()

      ! Set up possible field list hash table
      call possible_field_list%initialize(num_possible_fields)
      call set_up_field_list_hash_table()

      !
      ! Get users logname and machine hostname
      !
      if (masterproc) then
         logname = ' '
         call shr_sys_getenv ('LOGNAME', logname, rcode)
         host = ' '
         call shr_sys_getenv ('HOST', host, rcode)
      end if
      ! PIO requires netcdf attributes have consistant values on all tasks
      call mpi_bcast(logname, len(logname), mpi_character,                    &
           masterprocid, mpicom, rcode)
      call mpi_bcast(host,    len(host),    mpi_character,                    &
           masterprocid, mpicom, rcode)

      call get_curr_time(day, sec)  ! elapased time since reference date

      ! Set up hist fields on each user-specified file
      do file_idx = 1, size(hist_configs, 1)
         ! Time at beginning of current averaging interval.
         call hist_configs(file_idx)%set_beg_time(day, sec)

         ! Set up fields and buffers
         call hist_configs(file_idx)%set_up_fields(possible_field_list)
      end do


   end subroutine history_init_files

   !===========================================================================

   subroutine print_field_list()
      !-----------------------------------------------------------------------
      !
      ! Purpose: Print master field list
      !
      !-----------------------------------------------------------------------
      use cam_logfile, only: iulog
      use spmd_utils,  only: masterproc
      ! Local variables
      class(hist_field_info_t), pointer :: field_ptr

      character(len=4) :: avgflag

      field_ptr => possible_field_list_head

      if (masterproc) then
         write(iulog,*) ' '
         write(iulog,*)' ***************** HISTORY FIELD LIST ******************'
      end if
      do
         if (associated(field_ptr)) then
            avgflag = field_ptr%accumulate_type()
            if (avgflag == 'lst') then
               avgflag = 'inst'
            end if
            if (masterproc) then
               write(iulog, 9000) trim(field_ptr%diag_name()), &
                  field_ptr%units(), avgflag, &
                  field_ptr%standard_name()
9000           format(a16, 1x, a12, 2x, a3, 2x, a)
            end if
            field_ptr => field_ptr%next
         else
            exit
         end if
      end do
      if (masterproc) then
         write(iulog,*)' *************** END HISTORY FIELD LIST ****************'
         write(iulog,*) ' '
      end if

   end subroutine print_field_list

!===========================================================================

   subroutine set_up_field_list_hash_table()
      !-----------------------------------------------------------------------
      !
      ! Purpose: Populate field list hash table from linked list
      !
      !-----------------------------------------------------------------------
      ! Local variables
      class(hist_field_info_t), pointer :: field_ptr

      field_ptr => possible_field_list_head

      if (associated(field_ptr)) then
         ! Add to end of field list
         do
            call possible_field_list%add_hash_key(field_ptr)
            if (associated(field_ptr%next)) then
               field_ptr => field_ptr%next
            else
               exit
            end if
         end do
      end if

   end subroutine set_up_field_list_hash_table

!===========================================================================

   subroutine history_add_field_1d(diagnostic_name, standard_name, vdim_name, &
      avgflag, units, gridname, flag_xyfill, mixing_ratio)
      use cam_history_support, only: get_hist_coord_index, max_chars, horiz_only
      use cam_abortutils,      only: endrun
      !-----------------------------------------------------------------------
      !
      ! Purpose: Add a field to the master field list
      !
      ! Method: Put input arguments of field name, units, number of levels,
      !         averaging flag, and standard name into an entry in the global
      !         field linked list (possible_field_list_head).
      !
      !-----------------------------------------------------------------------

      !
      ! Arguments
      !
      character(len=*), intent(in) :: diagnostic_name ! field name (max_fieldname_len)
      character(len=*), intent(in) :: standard_name ! field standard name
      character(len=*), intent(in) :: vdim_name  ! NetCDF dimension name (or scalar coordinate)
      character(len=*), intent(in) :: avgflag    ! averaging flag
      character(len=*), intent(in) :: units      ! units of fname (max_chars)
      character(len=*), optional, intent(in) :: gridname
      logical,          optional, intent(in) :: flag_xyfill
      character(len=*), optional, intent(in) :: mixing_ratio

      !
      ! Local workspace
      !
      character(len=max_chars), allocatable :: dimnames(:)
      integer                               :: index

      if (trim(vdim_name) == trim(horiz_only)) then
         allocate(dimnames(0))
      else
         index = get_hist_coord_index(trim(vdim_name))
         if (index < 1) then
           call endrun('history_add_field_1d: Invalid coordinate, '//trim(vdim_name))
         end if
         allocate(dimnames(1))
         dimnames(1) = trim(vdim_name)
       end if
       call history_add_field(diagnostic_name, standard_name, dimnames, avgflag, units, &
            gridname=gridname, flag_xyfill=flag_xyfill, mixing_ratio=mixing_ratio)
    
   end subroutine history_add_field_1d

!===========================================================================

   subroutine history_add_field_nd(diagnostic_name, standard_name, dimnames, avgflag, &
      units, gridname, flag_xyfill, mixing_ratio)
      !-----------------------------------------------------------------------
      !
      ! Purpose: Add a field to the master field list - generic; called from
      !          history_add_field_1d
      !
      ! Method: Put input arguments of field name, units, number of levels,
      !         averaging flag, and standard name into an entry in the global
      !         field linked list (possible_field_list_head).
      !
      !-----------------------------------------------------------------------
      use hist_api,            only: hist_new_field
      use cam_grid_support,    only: cam_grid_get_coord_names
      use cam_grid_support,    only: cam_grid_dimensions
      use cam_grid_support,    only: cam_grid_id, cam_grid_is_zonal
      use cam_grid_support,    only: cam_grid_get_array_bounds
      use cam_history_support, only: lookup_hist_coord_indices
      use cam_history_support, only: hist_coord_find_levels, hist_coords
      use cam_history_support, only: max_fldlen=>max_fieldname_len, max_chars, fieldname_len
      use cam_hist_file,       only: strip_suffix
      use cam_logfile,         only: iulog
      use cam_abortutils,      only: endrun, check_allocate
      use spmd_utils,          only: masterproc

      character(len=*), intent(in) :: diagnostic_name
      character(len=*), intent(in) :: standard_name
      character(len=*), intent(in) :: dimnames(:)
      character(len=*), intent(in) :: avgflag    ! averaging flag
      character(len=*), intent(in) :: units      ! units of fname (max_chars)
      character(len=*), optional, intent(in) :: gridname
      logical,          optional, intent(in) :: flag_xyfill
      character(len=*), optional, intent(in) :: mixing_ratio

      ! Local variables
      class(hist_field_info_t), pointer :: field_ptr
      class(hist_field_info_t), pointer :: listentry
      integer                           :: grid_decomp, rank, pos
      integer                           :: grid_dims(2)
      integer                           :: num_levels
      integer,          allocatable     :: mdim_indices(:)
      integer,          allocatable     :: mdim_sizes(:)
      integer,          allocatable     :: field_shape(:)
      integer                           :: ierr, idx
      integer                           :: dimbounds(2,2)
      character(len=512)                :: errmsg
      character(len=max_fldlen)         :: fname_tmp ! local copy of fname
      character(len=max_fldlen)         :: coord_name ! for cell_methods
      character(len=max_fldlen)         :: cell_methods
      character(len=3)                  :: mixing_ratio_loc
      character(len=*), parameter       :: subname = 'history_add_field_nd: '

      if (size(hist_configs) > 0 .and. hist_configs(1)%file_is_setup()) then
         call endrun ('history_add_field_nd: Attempt to add field '//trim(diagnostic_name)//' after history files set')
      end if

      ! Some checks for diagnostic_name
      !
      ! Ensure that the diagnostic name is not blank
      !
      if (len_trim(diagnostic_name)==0) then
         call endrun('history_add_field_nd: blank field name not allowed')
      end if
      !
      ! Ensure that new field name is not longer than allowed
      ! (strip "&IC" suffix if it exists)
      !
      fname_tmp = diagnostic_name
      fname_tmp = strip_suffix(fname_tmp)

      if (len_trim(fname_tmp) > fieldname_len) then
         if (masterproc) then
            write(iulog,*)'history_add_field_nd: field name cannot be longer than ', fieldname_len,' characters long'
            write(iulog,*)'Field name:  ',diagnostic_name
            write(errmsg, *) 'Field name, "', trim(diagnostic_name), '" is too long'
         end if
         call endrun('history_add_field_nd: '//trim(errmsg))
      end if

      !
      ! Ensure that new field doesn't already exist
      !
      listentry => get_entry_by_name(possible_field_list_head, diagnostic_name)
      if(associated(listentry)) then
         call endrun ('history_add_field_nd:  '//diagnostic_name//' already on list')
      end if

      if (present(mixing_ratio)) then
         mixing_ratio_loc = mixing_ratio
      else
         mixing_ratio_loc = ''
      end if

      if (present(gridname)) then
         grid_decomp = cam_grid_id(trim(gridname))
      else
         ! default to physics grid
         grid_decomp = cam_grid_id('physgrid')
      end if
      if (grid_decomp < 0) then
         write(errmsg, *) 'Invalid grid name, "', trim(gridname), '" for ', trim(diagnostic_name)
             call endrun('history_add_field_nd: '//trim(errmsg))
      end if

      ! Indicate if some field pre-processing occurred (e.g., zonal mean)
      if (cam_grid_is_zonal(grid_decomp)) then
         call cam_grid_get_coord_names(grid_decomp, coord_name, errmsg)
         ! Zonal method currently hardcoded to 'mean'.
         cell_methods = trim(coord_name)//': mean'
      else
         cell_methods = ''
      end if

      ! peverwhee - TODO: handle fill values

      allocate(mdim_indices(size(dimnames, 1)), stat=ierr)
      call check_allocate(ierr, subname, 'mdim_indices', file=__FILE__, line=__LINE__-1)

      call lookup_hist_coord_indices(dimnames, mdim_indices)

      ! levels
      num_levels = hist_coord_find_levels(dimnames)
      if (num_levels < 0) then
         num_levels = 1
      end if

      call cam_grid_get_array_bounds(grid_decomp, dimbounds)

      call cam_grid_dimensions(grid_decomp, grid_dims, rank)
      pos = rank
      if (size(mdim_indices) > 0) then
         rank = rank + size(mdim_indices)
      end if
      allocate(field_shape(rank), stat=ierr)
      call check_allocate(ierr, subname, 'field_shape', file=__FILE__, line=__LINE__-1)
      allocate(mdim_sizes(size(mdim_indices)), stat=ierr)
      call check_allocate(ierr, subname, 'mdim_sizes', file=__FILE__, line=__LINE__-1)
      field_shape(1:pos) = grid_dims(1:pos)
      if (rank > pos) then
         do idx = 1, size(mdim_indices)
            pos = pos + 1
            mdim_sizes(idx) = hist_coords(mdim_indices(idx))%dimsize
            field_shape(pos) = mdim_sizes(idx)
         end do
      end if

      field_ptr => possible_field_list_head      
      if (associated(field_ptr)) then
         ! Add to end of field list
         do
            if (associated(field_ptr%next)) then
               field_ptr => field_ptr%next
            else
               field_ptr%next => hist_new_field(diagnostic_name, &
                  standard_name, standard_name, units, 'real', grid_decomp, &
                  mdim_indices, avgflag, num_levels, field_shape, mixing_ratio=mixing_ratio_loc, &
                  dim_bounds=dimbounds, mdim_sizes=mdim_sizes, cell_methods=cell_methods,    &
                  flag_xyfill=flag_xyfill)
               exit
            end if
         end do
      else
         possible_field_list_head => hist_new_field(diagnostic_name, &
            standard_name, standard_name, units, 'real', grid_decomp,   &
            mdim_indices, avgflag, num_levels, field_shape, mixing_ratio=mixing_ratio_loc, &
            dim_bounds=dimbounds, mdim_sizes=mdim_sizes, cell_methods=cell_methods,    &
            flag_xyfill=flag_xyfill)
      end if

      num_possible_fields = num_possible_fields + 1

   end subroutine history_add_field_nd

!===========================================================================

   subroutine history_out_field_1d(diagnostic_name, field_values)
      !-----------------------------------------------------------------------
      !
      ! Purpose: Accumulate active fields - 1d fields
      !
      !-----------------------------------------------------------------------
      use hist_api,         only: hist_field_accumulate
      use hist_msg_handler, only: hist_log_messages
      use cam_logfile,      only: iulog
      use cam_abortutils,   only: endrun
      use spmd_utils,       only: masterproc
      use shr_kind_mod,     only: r8 => shr_kind_r8
      ! Dummy variables
      character(len=*), intent(in) :: diagnostic_name
      real(r8),         intent(in) :: field_values(:)

      ! Local variables
      integer :: file_idx, field_idx
      character(len=3)  :: flag
      logical :: found
      character(len=cl) :: errmsg
      type(hist_log_messages) :: logger
      character(len=*), parameter :: subname = 'history_out_field_1d: '
      class(hist_field_info_t), pointer :: field_info

      errmsg = ''

      do file_idx = 1, size(hist_configs, 1)
         ! Check if the field is on the current file
         call hist_configs(file_idx)%find_in_field_list(diagnostic_name, field_info, errmsg)
         if (len_trim(errmsg) /= 0) then
            call endrun('ERROR: '//subname//errmsg,file=__FILE__, line=__LINE__)
         end if
         if (.not. associated(field_info)) then
            ! field is not active on this tape - do nothing!
            cycle
         end if
         ! Field is active on this file - accumulate!
         call hist_field_accumulate(field_info, field_values, 1, logger=logger)
         if (masterproc) then
            call logger%output(iulog)
         end if
            
      end do

   end subroutine history_out_field_1d

!===========================================================================

   subroutine history_out_field_2d(diagnostic_name, field_values)
      !-----------------------------------------------------------------------
      !
      ! Purpose: Accumulate active fields - 2d fields
      !
      !-----------------------------------------------------------------------
      use hist_api,         only: hist_field_accumulate
      use hist_msg_handler, only: hist_log_messages
      use cam_logfile,      only: iulog
      use cam_abortutils,   only: endrun
      use spmd_utils,       only: masterproc
      use shr_kind_mod,     only: r8 => shr_kind_r8
      ! Dummy variables
      character(len=*), intent(in) :: diagnostic_name
      real(r8),         intent(in) :: field_values(:,:)

      ! Local variables
      integer :: file_idx, field_idx
      character(len=3)  :: flag
      logical :: found
      character(len=cl) :: errmsg
      type(hist_log_messages) :: logger
      character(len=*), parameter :: subname = 'history_out_field_2d: '
      class(hist_field_info_t), pointer :: field_info

      errmsg = ''

      do file_idx = 1, size(hist_configs, 1)
         ! Check if the field is on the current file
         call hist_configs(file_idx)%find_in_field_list(diagnostic_name, field_info, errmsg)
         if (len_trim(errmsg) /= 0) then
            call endrun('ERROR: '//subname//errmsg,file=__FILE__, line=__LINE__)
         end if
         if (.not. associated(field_info)) then
            ! field is not active on this tape - do nothing!
            cycle
         end if
         ! Field is active on this file - accumulate!
         call hist_field_accumulate(field_info, field_values, 1, logger=logger)
         if (masterproc) then
            call logger%output(iulog)
         end if
      end do

   end subroutine history_out_field_2d

!===========================================================================

   subroutine history_out_field_3d(diagnostic_name, field_values)
      !-----------------------------------------------------------------------
      !
      ! Purpose: Accumulate active fields - 3d fields
      !
      !-----------------------------------------------------------------------
      use hist_api,       only: hist_field_accumulate
      use cam_logfile,    only: iulog
      use cam_abortutils, only: endrun
      use spmd_utils,     only: masterproc
      use shr_kind_mod,   only: r8 => shr_kind_r8
      ! Dummy variables
      character(len=*), intent(in) :: diagnostic_name
      real(r8),         intent(in) :: field_values(:,:,:)

      ! Local variables
      integer :: file_idx, field_idx
      character(len=3)  :: flag
      logical :: found
      character(len=cl) :: errmsg
      character(len=*), parameter :: subname = 'history_out_field_3d: '
      class(hist_field_info_t), pointer :: field_info

      errmsg = ''
      call endrun('ERROR: '//subname//'3d history fields not implemented', file=__FILE__, line=__LINE__)

      do file_idx = 1, size(hist_configs, 1)
         ! Check if the field is on the current file
         call hist_configs(file_idx)%find_in_field_list(diagnostic_name, field_info, errmsg)
         if (len_trim(errmsg) /= 0) then
            call endrun('ERROR: '//subname//errmsg,file=__FILE__, line=__LINE__)
         end if
         if (.not. associated(field_info)) then
            ! field is not active on this tape - do nothing!
            cycle
         end if
         ! Field is active on this file - accumulate!
         !call hist_field_accumulate(field_info, real(field_values, REAL64), 1)
         !if (masterproc) then
         !   call logger%output(iulog)
         !end if
            
      end do

   end subroutine history_out_field_3d

!==========================================================================

   subroutine history_wrap_up(restart_write, last_timestep)
      !-----------------------------------------------------------------------
      !
      ! Purpose: Close files we're done with (either last timestep
      !          or we've reached the max_frames cap for the file)
      !
      !-----------------------------------------------------------------------
      use time_manager,   only: get_curr_date, get_curr_time, get_nstep
      use cam_logfile,    only: iulog
      use spmd_utils,     only: masterproc
      use shr_kind_mod,   only: r8 => shr_kind_r8
      !
      !-----------------------------------------------------------------------
      !
      ! Purpose:
      ! Close history files.
      !
      ! Method:
      ! This routine will close any full hist. files
      ! or any hist. file that has data on it when restart files are being
      ! written.
      ! If a partially full history file was disposed (for restart
      ! purposes), then wrapup will open that unit back up and position
      ! it for appending new data.
      !
      ! Original version: CCM2
      !
      !-----------------------------------------------------------------------
      ! 
      ! Dummy arguments
      logical, intent(in) :: restart_write
      logical, intent(in) :: last_timestep

      ! Local variables
      integer  :: yr, mon, day, ncsec
      integer  :: ndcur, nscur, nstep
      integer  :: file_idx
      integer  :: num_samples, max_frames
      logical  :: full
      real(r8) :: tday ! Model day number for printout

      nstep = get_nstep()
      call get_curr_date(yr, mon, day, ncsec)
      call get_curr_time(ndcur, nscur)
      !
      !-----------------------------------------------------------------------
      ! Dispose history files.
      !-----------------------------------------------------------------------
      !
      ! Begin loop over hist_configs (the no. of declared history files - primary
      ! and auxiliary).  This loop disposes a history file to Mass Store
      ! when appropriate.
      !

      do file_idx = 1, size(hist_configs)
         !
         ! Find out if file is full
         !
         full = .false.
         num_samples = hist_configs(file_idx)%get_num_samples()
         max_frames = hist_configs(file_idx)%max_frame()
         if (mod(num_samples, max_frames) == 0 .and. num_samples > 0) then
            full = .true.
         end if
         if ((full .or. (last_timestep .and. num_samples >= 1)) .and. &
            hist_configs(file_idx)%are_files_open()) then
            !
            ! Dispose history file
            !
            call hist_configs(file_idx)%close_files()

            !
            ! Print information concerning model output.
            ! Model day number = iteration number of history file data * delta-t / (seconds per day)
            !
            tday = ndcur + nscur/86400._r8
            if(masterproc) then
               if (trim(hist_configs(file_idx)%get_volume()) == 'h0') then
                  write(iulog,*)'   Primary history file'
               else
                  write(iulog,*)'   Auxiliary history file ', hist_configs(file_idx)%get_volume()
               end if
               if (full) then
                  write(iulog,9003) nstep, max_frames, tday
               else
                  write(iulog,9003) nstep, mod(num_samples, max_frames), tday
               end if
               write(iulog,9004)
            end if
         end if
         call hist_configs(file_idx)%clear_buffers()
      end do
9003  format('    Output at NSTEP     = ',i10,/, &
             '    Number of time samples on this file = ',i10,/, &
             '    Model Day           = ',f10.2)
9004  format('---------------------------------------')

   end subroutine history_wrap_up

!#######################################################################

   recursive function get_entry_by_name(listentry, name) result(entry)
     type(hist_field_info_t),  pointer :: listentry
     character(len=*), intent(in) :: name ! variable name
     type(hist_field_info_t), pointer :: entry

     if(associated(listentry)) then
        if(listentry%diag_name() .eq. name) then
           entry => listentry
        else
           entry=>get_entry_by_name(listentry%next, name)
        end if
     else
        nullify(entry)
     end if
   end function get_entry_by_name

end module cam_history
