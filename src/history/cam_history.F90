module cam_history
   !----------------------------------------------------------------------------
   !
   ! The cam_history module provides the user interface for CAM's history
   !   output capabilities.
   ! It maintains the lists of fields that are written to each history file,
   !   and the associated metadata for those fields such as descriptive names,
   !   physical units, time axis properties, etc.
   !
   ! Public functions/subroutines:
   !   cam_hist_init_files
   !   cam_hist_write_history_state
   !   cam_hist_write_restart
   !   cam_hist_read_restart
   !   cam_hist_capture_field
   !   cam_hist_write_history_files
   !-----------------------------------------------------------------------

   use ISO_FORTRAN_ENV,      only: REAL64, REAL32, INT32, INT64
   use shr_kind_mod,         only: r8 => shr_kind_r8, r4 => shr_kind_r4
   use shr_kind_mod,         only: cl=>SHR_KIND_CL, cxx=>SHR_KIND_CXX
   use shr_sys_mod,          only: shr_sys_flush
   use perf_mod,             only: t_startf, t_stopf
   use spmd_utils,           only: masterproc
!   use cam_filenames,        only: interpret_filename_spec
   use cam_instance,         only: inst_suffix
!   use cam_initfiles,        only: ncdata, bnd_topo
   use cam_abortutils,       only: endrun
   use cam_logfile,          only: iulog
   use cam_hist_file,        only: hist_file_t
   use cam_grid_support,     only: max_split_files
   use cam_hist_file,        only: instantaneous_file_index, accumulated_file_index
   use cam_hist_file,        only: strip_suffix
   use cam_history_support,  only: pfiles, horiz_only
   use cam_history_support,  only: max_fldlen=>max_fieldname_len, max_chars, fieldname_len
   use hist_field,           only: hist_field_info_t
   use hist_hash_table,      only: hist_hash_table_t
   use hist_hashable,        only: hist_hashable_t
   use time_manager,         only: get_nstep

   implicit none
   private
   save

   integer           :: idx                ! index for nhtfrq initialization
   character(len=cl) :: model_doi_url = '' ! Model DOI
   character(len=cl) :: caseid = ''        ! case ID
   character(len=cl) :: ctitle = ''        ! case title
   ! NB: history_namelist value must match the group name in namelist_definition.xml
   character(len=*), parameter   :: history_namelist = 'cam_history_nl'
   ! hrestpath:  Full history restart pathnames
   character(len=cxx) :: hrestpath(pfiles) = (/(' ',idx=1,pfiles)/)
   character(len=cxx) :: cpath(pfiles) ! Array of current pathnames
   character(len=cxx) :: nhfil(pfiles) ! Array of current file names
   character(len=16)  :: logname             ! user name
   character(len=16)  :: host                ! host name
!!XXgoldyXX: Change inithist to use same values as any other history file
   character(len=8)   :: inithist = 'YEARLY' ! If set to '6-HOURLY, 'DAILY', 'MONTHLY' or
   ! 'YEARLY' then write IC file

!!XXgoldyXX: Do we need maxvarmdims anymore?
   integer, private :: maxvarmdims = 1
   !

   integer :: lcltod_start(pfiles) ! start time of day for local time averaging (sec)
   integer :: lcltod_stop(pfiles)  ! stop time of day for local time averaging, stop > start is wrap around (sec)

   ! Functions
   public :: history_readnl         ! Namelist reader for CAM history
!   public :: history_init_restart   ! Write restart history data
!   public :: history_write_restart  ! Write restart history data
!   public :: history_read_restart   ! Read restart history data
   public :: history_write_files    ! Write files out
   public :: history_init_files     ! Initialization
   public :: history_add_field      ! Write to list of possible history fields for this run
   public :: history_out_field      ! Accumulate field if its in use by one or more tapes
   public :: history_wrap_up        ! Process history files at end of timestep or run
!   public :: history_finalize       ! process history files at end of run
!   public :: history_write_IC       ! flag to dump of IC to IC file
!   public :: history_define_fld     ! Add a field to history file
!   public :: history_capture_fld    ! Capture current state of a model field
!   public :: history_fld_active     ! .true. if a field is active on any history file
!   public :: history_fld_col_active ! .true. for each column where a field is active on any history file
!   public :: register_vector_field  ! Register vector field set for interpolated output

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

      ! Setup the interpolate_info structures
      !do t = 1, size(interpolate_info)
      !   interpolate_info(fil_idx)%interp_type = interpolate_type(fil_idx)
      !   interpolate_info(fil_idx)%interp_gridtype = interpolate_gridtype(fil_idx)
      !   interpolate_info(fil_idx)%interp_nlat = interpolate_nlat(fil_idx)
      !   interpolate_info(fil_idx)%interp_nlon = interpolate_nlon(fil_idx)
      !end do

      ! separate namelist reader for the satellite history file
      !call sat_hist_readnl(nlfile, hfilename_spec, mfilt, fincl, hist_freq, avgflag_perfile)

   end subroutine history_readnl

   !===========================================================================

   subroutine history_write_files()
      use time_manager,     only: set_date_from_time_float
      character(len=cl) :: file_names(max_split_files)
      character(len=cl) :: prev_file_names(max_split_files)
      integer           :: yr, mon, day
      integer           :: yr_mid, mon_mid, day_mid
      integer           :: nstep
      integer           :: ncdate, ncdate_mid
      integer           :: ncsec, ncsec_mid
      integer           :: ndcur, nscur
      integer           :: num_samples
      real(r8)          :: time, beg_time
      real(r8)          :: time_interval(2)
      integer           :: file_idx, split_file_idx, prev_file_idx, idx
      integer           :: out_frq_mult
      character(len=8)  :: out_frq_type
      logical           :: write_history, write_nstep0, duplicate
      character(len=cl) :: filename_spec, prev_filename_spec
      integer           :: start, count1
      logical           :: restart

      ! Get nstep
      nstep = get_nstep()

      ! peverwhee - TODO: remove when restarts are implemented
      restart = .false.

      ! Loop over history volumes
      do file_idx = 1, size(hist_configs)
         ! Determine if it's time to write!
         write_history = .false.
         call hist_configs(file_idx)%output_freq_separate(out_frq_mult, out_frq_type)
         select case(trim(out_frq_type))
            case('step')
               if (mod(nstep, out_frq_mult) == 0) then
                  write_history = .true.
               end if
            case('second')
               if (mod(ncsec, out_frq_mult) == 0) then
                  write_history = .true.
               end if
            case('minute')
               if (mod(ncsec, out_frq_mult * 60) == 0) then
                  write_history = .true.
               end if
            case('hour')
               if (mod(ncsec, out_frq_mult * 3600) == 0) then
                  write_history = .true.
               end if
            case('day')
               if (mod(day, out_frq_mult) == 0 .and. ncsec == 0) then
                  write_history = .true.
               end if
            case('month')
               if (mod(mon, out_frq_mult) == 0 .and. ncsec == 0 .and. day == 1) then
                  write_history = .true.
               end if
            case('year')
               if (mod(yr, out_frq_mult) == 0 .and. ncsec == 0 .and. day == 1 .and. &
                  mon == 1) then
                  write_history = .true.
               end if
         end select
         if (.not. write_history) then
            ! Don't write this volume!
            cycle
         end if
         write_nstep0 = hist_configs(file_idx)%do_write_nstep0()
         if (nstep == 0 .and. .not. write_nstep0) then
            ! Don't write the first step
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
                     write(iulog,*)'hist_write_files: New filename same as old file = ', trim(file_names(idx))
                  end if
               end do
            end do
            if (duplicate) then
               filename_spec = hist_configs(file_idx)%get_filename_spec()
               prev_filename_spec = hist_configs(prev_file_idx)%get_filename_spec()
               write(iulog,*)'Is there an error in your filename specifiers?'
               write(iulog,*)'filename_spec(', file_idx, ') = ', trim(filename_spec)
               if ( prev_file_idx /= file_idx )then
                 write(iulog,*)'filename_spec(', prev_file_idx, ') = ', trim(prev_filename_spec)
               end if
               call endrun('hist_write_files: ERROR - see atm log file for information')
            end if
            call hist_configs(file_idx)%define_file(restart, logname, host, model_doi_url)
        !    call hist_configs(file_idx)%write_time_dependent_variables(file_idx, restart)
         end if
         call hist_configs(file_idx)%write_time_dependent_variables(file_idx, restart)
      end do

   end subroutine history_write_files

   !===========================================================================

   subroutine history_init_files(model_doi_url_in, caseid_in, ctitle_in)

      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: Initialize history file handler for initial or continuation
      !          run.
      !          For example, on an initial run, this routine initializes
      !          the configured history files. On a restart run, this routine
      !          only initializes history files declared beyond what existed
      !          on the previous run.  Files which already existed on the
      !          previous run have already been initialized (i.e. named and
      !          opened) in routine, hist_initialize_restart
      !
      !-----------------------------------------------------------------------
      use shr_sys_mod,      only: shr_sys_getenv
      use time_manager,     only: get_prev_time, get_curr_time
!      use cam_control_mod,  only: restart_run, branch_run
!      use sat_hist,         only: sat_hist_init
      use spmd_utils,       only: mpicom, masterprocid
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
      integer :: fil_idx, fld_ind  ! file, field indices
      integer :: begdim1           ! on-node dim1 start index
      integer :: enddim1           ! on-node dim1 end index
      integer :: begdim2           ! on-node dim2 start index
      integer :: enddim2           ! on-node dim2 end index
      integer :: begdim3           ! on-node chunk or lat start index
      integer :: enddim3           ! on-node chunk or lat end index
      integer :: day, sec          ! day and seconds from base date
      integer :: rcode             ! shr_sys_getenv return code
!      type(master_entry), pointer :: listentry
      character(len=32) :: fldname

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

      ! peverwhee - override averaging flag if specified?

!      if (branch_run) then
!         call get_prev_time(day, sec)  ! elapased time since reference date
!      else
       call get_curr_time(day, sec)  ! elapased time since reference date
!      end if

      do fil_idx = 1, size(hist_configs, 1)
         ! Time at beginning of current averaging interval.
         call hist_configs(fil_idx)%set_beg_time(day, sec)

         ! Set up fields and buffers
         call hist_configs(fil_idx)%set_up_fields(possible_field_list)
      end do


   end subroutine history_init_files

   !===========================================================================

   subroutine print_field_list()
      ! Local variables
      class(hist_hashable_t),   pointer :: field_ptr_value
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
      avgflag, units, gridname)
      use cam_history_support, only: get_hist_coord_index
      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: Add a field to the master field list
      !
      ! Method: Put input arguments of field name, units, number of levels,
      !         averaging flag, and long name into a type entry in the global
      !         master field list (masterlist).
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
       call history_add_field(diagnostic_name, standard_name, dimnames, avgflag, units, gridname)
    
   end subroutine history_add_field_1d

!===========================================================================

   subroutine history_add_field_nd(diagnostic_name, standard_name, dimnames, avgflag, &
      units, gridname, flag_xyfill)
      ! Add field to possible field linked list
      use hist_api, only: hist_new_field
      use hist_hashable, only: hist_hashable_char_t
      use hist_hashable, only: hist_hashable_int_t
      use cam_grid_support, only: cam_grid_get_coord_names
      use cam_grid_support, only: cam_grid_dimensions
      use cam_grid_support, only: cam_grid_id, cam_grid_is_zonal
      use cam_grid_support, only: cam_grid_get_array_bounds
      use cam_history_support, only: lookup_hist_coord_indices
      use cam_history_support, only: hist_coord_find_levels, hist_coords
      !use cam_ccpp_cap, only: cam_const_get_index

      character(len=*), intent(in) :: diagnostic_name
      character(len=*), intent(in) :: standard_name
      character(len=*), intent(in) :: dimnames(:)
      character(len=*), intent(in) :: avgflag    ! averaging flag
      character(len=*), intent(in) :: units      ! units of fname (max_chars)
      character(len=*), optional, intent(in) :: gridname
      logical,          optional, intent(in) :: flag_xyfill

      ! Local variables
      class(hist_field_info_t), pointer :: field_ptr
      class(hist_field_info_t), pointer :: listentry
      integer                           :: grid_decomp, rank, pos
      integer                           :: grid_dims(2)
      integer                           :: dimcnt, num_levels
      integer,          allocatable     :: mdim_indices(:)
      integer,          allocatable     :: mdim_sizes(:)
      integer,          allocatable     :: field_shape(:)
      integer                           :: const_index
      integer                           :: errcode
      integer                           :: dimbounds(2,2)
      character(len=512)                :: errmsg
      character(len=max_fldlen)         :: fname_tmp ! local copy of fname
      character(len=max_fldlen)         :: coord_name ! for cell_methods
      character(len=max_fldlen)         :: cell_methods
      character(len=3)                  :: mixing_ratio

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
      fname_tmp  = strip_suffix(fname_tmp)

      if (len_trim(fname_tmp) > fieldname_len) then
         write(iulog,*)'history_add_field_nd: field name cannot be longer than ', fieldname_len,' characters long'
         write(iulog,*)'Field name:  ',diagnostic_name
         write(errmsg, *) 'Field name, "', trim(diagnostic_name), '" is too long'
         call endrun('history_add_field_nd: '//trim(errmsg))
      end if

      !
      ! Ensure that new field doesn't already exist
      !
      listentry => get_entry_by_name(possible_field_list_head, diagnostic_name)
      if(associated(listentry)) then
         call endrun ('history_add_field_nd:  '//diagnostic_name//' already on list')
      end if

      ! If the field is an advected constituent determine whether its concentration
      ! is based on dry or wet air.
      ! peverwhee - TODO: constituents handling requires SIMA  and/or framework update
      !call cam_const_get_index(standard_name, const_index, errcode, errmsg)
      !if (errcode /= 0) then
      !   write(iulog,*) errmsg
      !   call endrun('history_add_field_nd:  '//diagnostic_name//' failed in const_get_index')
      !end if
      mixing_ratio = ''
      !if (const_index > 0) then
      !   mixing_ratio = cnst_get_type_byind(idx)
      !end if


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

      allocate(mdim_indices(size(dimnames, 1)))

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
      allocate(field_shape(rank))
      allocate(mdim_sizes(size(mdim_indices)))
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
                  mdim_indices, avgflag, num_levels, field_shape, mixing_ratio=mixing_ratio, &
                  dim_bounds=dimbounds, mdim_sizes=mdim_sizes, cell_methods=cell_methods,    &
                  flag_xyfill=flag_xyfill)
               exit
            end if
         end do
      else
         possible_field_list_head => hist_new_field(diagnostic_name, &
            standard_name, standard_name, units, 'real', grid_decomp,   &
            mdim_indices, avgflag, num_levels, field_shape, mixing_ratio=mixing_ratio, &
            dim_bounds=dimbounds, mdim_sizes=mdim_sizes, cell_methods=cell_methods,    &
            flag_xyfill=flag_xyfill)
      end if

      num_possible_fields = num_possible_fields + 1

   end subroutine history_add_field_nd

!===========================================================================

   subroutine history_out_field_1d(diagnostic_name, field_values, idim)
      use hist_api, only: hist_field_accumulate
      use hist_msg_handler, only: hist_log_messages
      ! Dummy variables
      character(len=*), intent(in) :: diagnostic_name
      integer,          intent(in) :: idim
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

      ! peverwhee - TODO
      !  - fill values
      !  - different dimensions

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
         ! Accumulate the field
         if (hist_configs(file_idx)%precision() == 'REAL32') then
            call hist_field_accumulate(field_info, real(field_values, REAL32), 1, logger=logger)
         else
            call hist_field_accumulate(field_info, real(field_values, REAL64), 1, logger=logger)
         end if
            
      end do

   end subroutine history_out_field_1d

!===========================================================================

   subroutine history_out_field_2d(diagnostic_name, field_values, idim)
      use hist_api, only: hist_field_accumulate
      use hist_msg_handler, only: hist_log_messages
      ! Dummy variables
      character(len=*), intent(in) :: diagnostic_name
      integer,          intent(in) :: idim
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

      ! peverwhee - TODO
      !  - fill values
      !  - different dimensions

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
         ! Accumulate the field
         if (hist_configs(file_idx)%precision() == 'REAL32') then
            call hist_field_accumulate(field_info, real(field_values, REAL32), 1, logger=logger)
         else
            call hist_field_accumulate(field_info, real(field_values, REAL64), 1, logger=logger)
         end if
         call logger%output(iulog)
      end do

   end subroutine history_out_field_2d

!===========================================================================

   subroutine history_out_field_3d(diagnostic_name, field_values, idim)
      use hist_api, only: hist_field_accumulate
      ! Dummy variables
      character(len=*), intent(in) :: diagnostic_name
      integer,          intent(in) :: idim
      real(r8),         intent(in) :: field_values(:,:,:)

      ! Local variables
      integer :: file_idx, field_idx
      character(len=3)  :: flag
      logical :: found
      character(len=cl) :: errmsg
      character(len=*), parameter :: subname = 'history_out_field_3d: '
      class(hist_field_info_t), pointer :: field_info

      errmsg = ''

      ! peverwhee - TODO
      !  - fill values
      !  - different dimensions

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
         ! Accumulate the field
         !call hist_field_accumulate(field_info, real(field_values, REAL64), 1)
            
      end do

   end subroutine history_out_field_3d

!==========================================================================

   subroutine history_wrap_up(restart_write, last_timestep)
     use time_manager,         only: get_curr_date, get_curr_time
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
      integer  :: file_idx, split_file_idx, field_idx
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
         if (mod(num_samples, max_frames) == 0) then
            full = .true.
         end if
         if (full .or. (last_timestep .and. num_samples >= 1)) then
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
               if (file_idx == 1) then
                  write(iulog,*)'   Primary history file'
               else
                  write(iulog,*)'   Auxiliary history file number ', file_idx-1
               end if
               write(iulog,9003)nstep,mod(num_samples, max_frames),tday
               write(iulog,9004)
            end if
         end if
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
