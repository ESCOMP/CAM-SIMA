module cam_history
   !----------------------------------------------------------------------------
   !
   ! The cam_history module provides the user interface for CAM's history
   !   output capabilities.
   ! It maintains the lists of fields that are written to each history file,
   !   and the associated metadata for those fields such as descriptive names,
   !   physical units, time axis properties, etc.
   ! It also contains the programmer interface which provides routines that
   !   are called from the physics and dynamics initialization routines to
   !   define the fields that are produced by the model and are available for
   !   output, and the routine that is called from the corresponding run
   !   method to add the field values into a history buffer so that they
   !   may be output to disk.
   !
   ! There are two special history files.  The initial file and the
   !   satellite track file.
   !
   ! Public functions/subroutines:
   !   addfld, add_default
   !   hist_init_files
   !   history_initialized
   !   write_restart_history
   !   read_restart_history
   !   outfld
   !   wshist
   !-----------------------------------------------------------------------

   use shr_kind_mod,    only: r8 => shr_kind_r8, r4 => shr_kind_r4
   use shr_kind_mod,    only: cl=>SHR_KIND_CL
   use shr_sys_mod,     only: shr_sys_flush
   use perf_mod,        only: t_startf, t_stopf
   use spmd_utils,      only: masterproc
   use cam_filenames,   only: interpret_filename_spec
   use cam_instance,    only: inst_suffix
   use cam_initfiles,   only: ncdata, bnd_topo
   use cam_abortutils,  only: endrun
   use cam_logfile,     only: iulog

   use cam_history_support, only: max_fieldname_len
   use cam_history_support, only: fieldname_suffix_len
   use cam_history_support, only: max_chars
   use cam_history_support, only: pfiles
   use cam_history_support, only: fieldname_len
   use cam_history_support, only: max_string_len
   use cam_history_support, only: date2yyyymmdd
   use cam_history_support, only: pflds
   use cam_history_support, only: fieldname_lenp2
   use cam_history_support, only: sec2hms
   use cam_history_support, only: field_info
   use cam_history_support, only: active_entry
   use cam_history_support, only: hentry
   use cam_history_support, only: horiz_only
   use cam_history_support, only: write_hist_coord_attrs
   use cam_history_support, only: write_hist_coord_vars
   use cam_history_support, only: interp_info_t
   use cam_history_support, only: lookup_hist_coord_indices
   use cam_history_support, only: get_hist_coord_index
   use sat_hist,            only: is_satfile
   use solar_parms_data,    only: solar_parms_define, solar_parms_write
   use solar_wind_data,     only: solar_wind_define, solar_wind_write
   use epotential_params,   only: epot_active, epot_crit_colats

   implicit none
   private
   save

   character(len=cl) :: model_doi_url = '' ! Model DOI
   character(len=cl) :: caseid = ''        ! case ID
   character(len=cl) :: ctitle = ''        ! case title
   ! NB: This name must match the group name in namelist_definition.xml
   character(len=*), parameter   :: history_namelist = 'cam_history_nl'
   ! hrestpath:  Full history restart pathnames
   character(len=max_string_len) :: hrestpath(pfiles) = (/(' ',idx=1,pfiles)/)
   character(len=max_string_len) :: cpath(pfiles) ! Array of current pathnames
   character(len=max_string_len) :: nhfil(pfiles) ! Array of current file names
   character(len=16)  :: logname             ! user name
   character(len=16)  :: host                ! host name
   character(len=8)   :: inithist = 'YEARLY' ! If set to '6-HOURLY, 'DAILY', 'MONTHLY' or
   ! 'YEARLY' then write IC file
   logical            :: inithist_all = .false. ! Flag to indicate set of fields to be
   ! included on IC file
   !  .false.  include only required fields
   !  .true.   include required *and* optional fields

   integer :: maxvarmdims=1
   !

   !
   ! Filename specifiers for history, initial files and restart history files
   ! (%c = caseid, $y = year, $m = month, $d = day, $s = seconds in day, %t = file number)
   !
   character(len=max_string_len) :: rhfilename_spec = '%c.cam.rh%t.%y-%m-%d-%s.nc' ! history restart
   character(len=max_string_len) :: hfilename_spec(pfiles) = (/ (' ', idx=1, pfiles) /) ! filename specifyer


   ! Needed by cam_diagnostics
   public :: inithist_all

   integer :: lcltod_start(pfiles) ! start time of day for local time averaging (sec)
   integer :: lcltod_stop(pfiles)  ! stop time of day for local time averaging, stop > start is wrap around (sec)

   ! Functions
   public :: history_readnl         ! Namelist reader for CAM history
   public :: history_init_restart   ! Write restart history data
   public :: history_write_restart  ! Write restart history data
   public :: history_read_restart   ! Read restart history data
   public :: history_write_files    ! Write files out
!   public :: outfld                 ! Output a field
   public :: history_init_files     ! Initialization
   public :: history_initialized    ! .true. iff cam history initialized
   public :: history_finalize       ! process history files at end of run
   public :: history_write_IC       ! flag to dump of IC to IC file
   public :: history_addfld         ! Add a field to history file
   public :: history_fld_active     ! .true. if a field is active on any history file
   public :: history_fld_col_active ! .true. for each column where a field is active on any history file
   public :: register_vector_field  ! Register vector field set for interpolated output

CONTAINS

   subroutine history_readnl(nlfile)

      use namelist_utils, only: find_group_name
      use units,          only: getunit, freeunit
      use spmd_utils,     only: masterproc, masterprocid, mpicom
      use spmd_utils,     only: mpi_integer, mpi_logical, mpi_character
      use shr_string_mod, only: shr_string_toUpper
      use time_manager,   only: get_step_size
      use sat_hist,       only: sat_hist_readnl

      ! Dummy argument
      character(len=*), intent(in)   :: nlfile  ! filepath of namelist input file

      !
      ! Local variables
      integer                        :: dtime   ! Step time in seconds
      integer                        :: unitn, ierr, f, t
      character(len=8)               :: ctemp      ! Temporary character string

      ! History namelist items
      namelist /cam_history_nl/ &
           diag_file1, diag_file2, diag_file3, diag_file4, diag_file5,        &
           diag_file6, diag_file7, diag_file8, diag_file9, diag_file10,       &
           lcltod_start, lcltod_stop,          &
           inithist, inithist_all,                                            &
           hfilename_spec,                             &
           interpolate_nlat, interpolate_nlon,                                &
           interpolate_gridtype, interpolate_type, interpolate_output

      ! Set namelist defaults (these should match initial values if given)
      fincl(:,:)               = ' '
      fincllonlat(:,:)         = ' '
      fexcl(:,:)               = ' '
      fout_prec(:,:)           = ' '
      collect_column_output(:) = .false.
      avgflag_perfile(:)       = ' '
      ndens                    = 2
      hist_freq(1)             = 0
      hist_freq(2:)            = -24
      mfilt                    = 30
      inithist                 = 'YEARLY'
      inithist_all             = .false.
      empty_hfiles             = .false.
      lcltod_start(:)          = 0
      lcltod_stop(:)           = 0
      hfilename_spec(:)        = ' '
      interpolate_nlat(:)      = 0
      interpolate_nlon(:)      = 0
      interpolate_gridtype(:)  = 1
      interpolate_type(:)      = 1
      interpolate_output(:)    = .false.

      if (trim(history_namelist) /= 'cam_history_nl') then
         call endrun('HISTORY_READNL: CAM history namelist mismatch')
      end if
      if (masterproc) then
         write(iulog, *) 'Read in ',history_namelist,' namelist from: ',trim(nlfile)
         unitn = getunit()
         open(unitn, file=trim(nlfile), status='old')
         call find_group_name(unitn, history_namelist, status=ierr)
         if (ierr == 0) then
            read(unitn, cam_history_nl, iostat=ierr)
            if (ierr /= 0) then
               call endrun('history_readnl: ERROR reading namelist, '//trim(history_namelist))
            end if
         end if
         close(unitn)
         call freeunit(unitn)

         do f = 1, pflds
            fincl(f, 1) = fincl1(fld_idx)
            fincl(f, 2) = fincl2(fld_idx)
            fincl(f, 3) = fincl3(fld_idx)
            fincl(f, 4) = fincl4(fld_idx)
            fincl(f, 5) = fincl5(fld_idx)
            fincl(f, 6) = fincl6(fld_idx)
            fincl(f, 7) = fincl7(fld_idx)
            fincl(f, 8) = fincl8(fld_idx)
            fincl(f, 9) = fincl9(fld_idx)
            fincl(f,10) = fincl10(fld_idx)

            fincllonlat(f, 1) = fincl1lonlat(fld_idx)
            fincllonlat(f, 2) = fincl2lonlat(fld_idx)
            fincllonlat(f, 3) = fincl3lonlat(fld_idx)
            fincllonlat(f, 4) = fincl4lonlat(fld_idx)
            fincllonlat(f, 5) = fincl5lonlat(fld_idx)
            fincllonlat(f, 6) = fincl6lonlat(fld_idx)
            fincllonlat(f, 7) = fincl7lonlat(fld_idx)
            fincllonlat(f, 8) = fincl8lonlat(fld_idx)
            fincllonlat(f, 9) = fincl9lonlat(fld_idx)
            fincllonlat(f,10) = fincl10lonlat(fld_idx)

            fexcl(f, 1) = fexcl1(fld_idx)
            fexcl(f, 2) = fexcl2(fld_idx)
            fexcl(f, 3) = fexcl3(fld_idx)
            fexcl(f, 4) = fexcl4(fld_idx)
            fexcl(f, 5) = fexcl5(fld_idx)
            fexcl(f, 6) = fexcl6(fld_idx)
            fexcl(f, 7) = fexcl7(fld_idx)
            fexcl(f, 8) = fexcl8(fld_idx)
            fexcl(f, 9) = fexcl9(fld_idx)
            fexcl(f,10) = fexcl10(fld_idx)

            fout_prec(f, 1) = fwrtpr1(fld_idx)
            fout_prec(f, 2) = fwrtpr2(fld_idx)
            fout_prec(f, 3) = fwrtpr3(fld_idx)
            fout_prec(f, 4) = fwrtpr4(fld_idx)
            fout_prec(f, 5) = fwrtpr5(fld_idx)
            fout_prec(f, 6) = fwrtpr6(fld_idx)
            fout_prec(f, 7) = fwrtpr7(fld_idx)
            fout_prec(f, 8) = fwrtpr8(fld_idx)
            fout_prec(f, 9) = fwrtpr9(fld_idx)
            fout_prec(f,10) = fwrtpr10(fld_idx)
         end do

         !
         ! If generate an initial conditions history file as an auxillary file:
         !
         ctemp = shr_string_toUpper(inithist)
         inithist = trim(ctemp)
         if ( (inithist /= '6-HOURLY') .and. (inithist /= 'DAILY')  .and.        &
              (inithist /= 'MONTHLY')  .and. (inithist /= 'YEARLY') .and.        &
              (inithist /= 'CAMIOP')   .and. (inithist /= 'ENDOFRUN')) then
            inithist = 'NONE'
         end if
         !
         ! History file write times
         ! Convert write freq. of hist files from hours to timesteps if necessary.
         !
         dtime = get_step_size()
         do t = 1, pfiles
            if (hist_freq(fil_idx) < 0) then
               hist_freq(fil_idx) = nint((-hist_freq(fil_idx) * 3600._r8) / dtime)
            end if
         end do
         !
         ! Initialize the filename specifier if not already set
         ! This is the format for the history filenames:
         ! %c= caseid, %t=file no., %y=year, %m=month, %d=day, %s=second, %%=%
         ! See the filenames module for more information
         !
         do t = 1, pfiles
            if ( len_trim(hfilename_spec(fil_idx)) == 0 )then
               if ( hist_freq(fil_idx) == 0 )then
                  ! Monthly files
                  hfilename_spec(fil_idx) = '%c.cam' // trim(inst_suffix) // '.h%t.%y-%m.nc'
               else
                  hfilename_spec(fil_idx) = '%c.cam' // trim(inst_suffix) // '.h%t.%y-%m-%d-%s.nc'
               end if
            end if
            !
            ! Only one time sample allowed per monthly average file
            !
            if (hist_freq(fil_idx) == 0) then
               mfilt(fil_idx) = 1
            end if
         end do
      end if ! masterproc

      ! Print per-file averaging flags
      if (masterproc) then
         do t = 1, pfiles
            if (avgflag_perfile(fil_idx) /= ' ') then
               write(iulog,*)'Unless overridden by namelist input on a per-field basis (FINCL),'
               write(iulog,*)'All fields on history file ',t,' will have averaging flag ',avgflag_perfile(fil_idx)
            end if
            ! Enforce no interpolation for satellite files
            if (is_satfile(fil_idx) .and. interpolate_output(fil_idx)) then
               write(iulog, *) 'WARNING: Interpolated output not supported for a satellite history file, ignored'
               interpolate_output(fil_idx) = .false.
            end if
            ! Enforce no interpolation for IC files
            if (is_initfile(fil_idx) .and. interpolate_output(fil_idx)) then
               write(iulog, *) 'WARNING: Interpolated output not supported for an initial data (IC) history file, ignored'
               interpolate_output(fil_idx) = .false.
            end if
         end do
      end if

      ! Write out inithist info
      if (masterproc) then
         if (inithist == '6-HOURLY' ) then
            write(iulog,*)'Initial conditions history files will be written 6-hourly.'
         else if (inithist == 'DAILY' ) then
            write(iulog,*)'Initial conditions history files will be written daily.'
         else if (inithist == 'MONTHLY' ) then
            write(iulog,*)'Initial conditions history files will be written monthly.'
         else if (inithist == 'YEARLY' ) then
            write(iulog,*)'Initial conditions history files will be written yearly.'
         else if (inithist == 'CAMIOP' ) then
            write(iulog,*)'Initial conditions history files will be written for IOP.'
         else if (inithist == 'ENDOFRUN' ) then
            write(iulog,*)'Initial conditions history files will be written at end of run.'
         else
            write(iulog,*)'Initial conditions history files will not be created'
         end if
      end if

      ! Print out column-output information
      do t = 1, size(fincllonlat, 2)
         if (ANY(len_trim(fincllonlat(:,t)) > 0)) then
            if (collect_column_output(fil_idx)) then
               write(iulog, '(a,i2,a)') 'History file, ',t,', has patch output, columns will be collected into ncol dimension'
            else
               write(iulog, '(a,i2,a)') 'History file, ',t,', has patch output, patches will be written to individual variables'
            end if
         end if
      end do

      ! Broadcast namelist variables
      call mpi_bcast(ndens, pfiles, mpi_integer, masterprocid, mpicom, ierr)
      call mpi_bcast(hist_freq, pfiles, mpi_integer, masterprocid, mpicom, ierr)
      call mpi_bcast(mfilt, pfiles, mpi_integer, masterprocid, mpicom, ierr)
      call mpi_bcast(inithist,len(inithist), mpi_character, masterprocid, mpicom, ierr)
      call mpi_bcast(inithist_all,1, mpi_logical, masterprocid, mpicom, ierr)
      call mpi_bcast(lcltod_start, pfiles, mpi_integer, masterprocid, mpicom, ierr)
      call mpi_bcast(lcltod_stop,  pfiles, mpi_integer, masterprocid, mpicom, ierr)
      call mpi_bcast(collect_column_output, pfiles, mpi_logical, masterprocid, mpicom, ierr)
      call mpi_bcast(empty_hfiles,1, mpi_logical, masterprocid, mpicom, ierr)
      call mpi_bcast(avgflag_perfile, pfiles, mpi_character, masterprocid, mpicom, ierr)
      call mpi_bcast(hfilename_spec, len(hfilename_spec(1))*pfiles, mpi_character, masterprocid, mpicom, ierr)
      call mpi_bcast(fincl, len(fincl (1,1))*pflds*pfiles, mpi_character, masterprocid, mpicom, ierr)
      call mpi_bcast(fexcl, len(fexcl (1,1))*pflds*pfiles, mpi_character, masterprocid, mpicom, ierr)

      call mpi_bcast(fincllonlat, len(fincllonlat (1,1))*pflds*pfiles, mpi_character, masterprocid, mpicom, ierr)

      call mpi_bcast(fout_prec, len(fout_prec(1,1))*pflds*pfiles,             &
           mpi_character, masterprocid, mpicom, ierr)
      t = size(interpolate_nlat, 1)
      call mpi_bcast(interpolate_nlat, t, mpi_integer, masterprocid, mpicom, ierr)
      call mpi_bcast(interpolate_nlon, t, mpi_integer, masterprocid, mpicom, ierr)
      call mpi_bcast(interpolate_gridtype, t, mpi_integer, masterprocid, mpicom, ierr)
      call mpi_bcast(interpolate_type, t, mpi_integer, masterprocid, mpicom, ierr)
      call mpi_bcast(interpolate_output, pfiles, mpi_logical, masterprocid, mpicom, ierr)

      ! Setup the interpolate_info structures
      do t = 1, size(interpolate_info)
         interpolate_info(fil_idx)%interp_type = interpolate_type(fil_idx)
         interpolate_info(fil_idx)%interp_gridtype = interpolate_gridtype(fil_idx)
         interpolate_info(fil_idx)%interp_nlat = interpolate_nlat(fil_idx)
         interpolate_info(fil_idx)%interp_nlon = interpolate_nlon(fil_idx)
      end do

      ! separate namelist reader for the satellite history file
      call sat_hist_readnl(nlfile, hfilename_spec, mfilt, fincl, hist_freq, avgflag_perfile)

   end subroutine history_readnl

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
      use cam_control_mod,  only: restart_run, branch_run
      use sat_hist,         only: sat_hist_init
      use spmd_utils,       only: mpicom, masterprocid, mpi_character
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
      integer :: file, fld_ind     ! file, field indices
      integer :: begdim1           ! on-node dim1 start index
      integer :: enddim1           ! on-node dim1 end index
      integer :: begdim2           ! on-node dim2 start index
      integer :: enddim2           ! on-node dim2 end index
      integer :: begdim3           ! on-node chunk or lat start index
      integer :: enddim3           ! on-node chunk or lat end index
      integer :: day, sec          ! day and seconds from base date
      integer :: rcode             ! shr_sys_getenv return code
      type(master_entry), pointer :: listentry
      character(len=32) :: fldname

      !
      ! Save the DOI
      !
      model_doi_url = trim(model_doi_url_in)
      caseid        = caseid_in
      ctitle        = ctitle_in

      !
      ! Print master field list
      !

      if (masterproc) then
         write(iulog,*) ' '
         write(iulog,*)' ******* MASTER FIELD LIST *******'
      end if
      listentry=>masterlinkedlist
      fld_ind = 0
      do while(associated(listentry))
         fld_ind = fld_ind + 1
         if(masterproc) then
            fldname = listentry%field%name
            write(iulog,9000) fld_ind, fldname, listentry%field%units,        &
                 listentry%field%numlev, listentry%avgflag(1),                &
                 trim(listentry%field%long_name)
9000        format(i5, 1x, a32, 1x, a16, 1x, i4, 1x, a1, 2x, a)
         end if
         listentry=>listentry%next_entry
      end do
      nfmaster = fld_ind
      if (masterproc) then
         write(iulog,*) 'hist_init_files: nfmaster=', nfmaster
      end if

      !
      !  Now that masterlinkedlist is defined and we are performing a
      !  restart run (after all addfld calls), construct primary and
      !  secondary hashing tables.
      !
      if (restart_run) then
         call print_active_field_list()
         call bld_outfld_hash_tbls()
         call bld_hfilefld_indices()
         return
      end if
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
      !
      ! Override averaging flag for all fields on a particular file if namelist input so specifies
      !
      do fil_idx = 1, pfiles
         if (avgflag_perfile(fil_idx) /= ' ') then
            call h_override (fil_idx)
         end if
      end do
      !
      ! Define field list information for all history files.
      !
      call create_field_list()
      !
      ! Loop over max. no. of history files permitted
      !
      if (branch_run) then
         call get_prev_time(day, sec)  ! elapased time since reference date
      else
         call get_curr_time(day, sec)  ! elapased time since reference date
      end if
      do fil_idx = 1, pfiles
         ! nfils: Number of time samples in history file file, fil_idx
         nfils(fil_idx) = 0
         ! Time at beginning of current averaging interval.
         beg_time(fil_idx) = day + (sec/86400._r8)
      end do

      !
      ! Initialize history variables
      !
      do fil_idx = 1, pfiles
         do fld_idx = 1, nflds(fil_idx)
            begdim1  = file(fil_idx)%hlist(fld_idx)%field%begdim1
            enddim1  = file(fil_idx)%hlist(fld_idx)%field%enddim1
            begdim2  = file(fil_idx)%hlist(fld_idx)%field%begdim2
            enddim2  = file(fil_idx)%hlist(fld_idx)%field%enddim2
            begdim3  = file(fil_idx)%hlist(fld_idx)%field%begdim3
            enddim3  = file(fil_idx)%hlist(fld_idx)%field%enddim3
            allocate(file(fil_idx)%hlist(fld_idx)%hbuf(begdim1:enddim1,begdim2:enddim2,begdim3:enddim3))
            file(fil_idx)%hlist(fld_idx)%hbuf = 0._r8
            if (file(fil_idx)%hlist(fld_idx)%avgflag .eq. 'S') then ! allocate the variance buffer for standard dev
               allocate(file(fil_idx)%hlist(fld_idx)%sbuf(begdim1:enddim1,begdim2:enddim2,begdim3:enddim3))
               file(fil_idx)%hlist(fld_idx)%sbuf = 0._r8
            endif
            if(file(fil_idx)%hlist(fld_idx)%field%flag_xyfill .or. (avgflag_perfile(fil_idx) .eq. 'L')) then
               allocate (file(fil_idx)%hlist(fld_idx)%nacs(begdim1:enddim1,begdim3:enddim3))
            else
               allocate (file(fil_idx)%hlist(fld_idx)%nacs(1,begdim3:enddim3))
            end if
            file(fil_idx)%hlist(fld_idx)%nacs(:,:) = 0
            file(fil_idx)%hlist(fld_idx)%field%meridional_complement = -1
            file(fil_idx)%hlist(fld_idx)%field%zonal_complement = -1
         end do
      end do
      ! Setup vector pairs for unstructured grid interpolation
      call setup_interpolation_and_define_vector_complements()
      !  Initialize the sat following history subsystem
      call sat_hist_init()

      return
   end subroutine hist_init_files

   logical function history_initialized()
      history_initialized = associated(masterlist)
   end function history_initialized

   !===========================================================================

   subroutine set_field_dimensions(field)
      use cam_history_support, only: hist_coord_size
      use cam_grid_support,    only: cam_grid_get_array_bounds, cam_grid_is_block_indexed
      ! Dummy arguments
      type(field_info), intent(inout) :: field

      ! Local variables
      integer                         :: i
      integer                         :: msize
      integer                         :: dimbounds(2,2)

      call cam_grid_get_array_bounds(field%decomp_type, dimbounds)
      field%begdim1  = dimbounds(1,1)
      field%enddim1  = dimbounds(1,2)
      field%begdim2  = 1
      if (associated(field%mdims)) then
         if (size(field%mdims) > 0) then
            field%enddim2  = 1
            do i = 1, size(field%mdims)
               msize = hist_coord_size(field%mdims(i))
               if (msize <= 0) then
                  call endrun('set_field_dimensions: mdim size must be > 0')
               end if
               field%enddim2 = field%enddim2 * msize
            end do
         else
            if (field%numlev < 1) then
               if (masterproc) then
                  write(iulog, *) 'SET_FIELD_DIMENSIONS WARNING: illegal numlev for ', trim(field%name)
               end if
               field%numlev = 1
            end if
            field%enddim2 = field%numlev
         end if
      else
         if (field%numlev < 1) then
            if (masterproc) then
               write(iulog, *) 'SET_FIELD_DIMENSIONS WARNING: illegal numlev for ', trim(field%name)
            end if
            field%numlev = 1
         end if
         field%enddim2 = field%numlev
      end if
      field%begdim3  = dimbounds(2,1)
      field%enddim3  = dimbounds(2,2)
      field%colperchunk = cam_grid_is_block_indexed(field%decomp_type)

   end subroutine set_field_dimensions

   subroutine setup_interpolation_and_define_vector_complements()
      use interp_mod, only: setup_history_interpolation

      ! Local variables
      integer :: hf, f, ff
      logical :: interp_ok
      character(len=max_fieldname_len) :: fname
      character(len=max_fieldname_len) :: mname
      character(len=max_fieldname_len) :: zname
      character(len=*), parameter      :: subname='setup_interpolation_and_define_vector_complements'

      ! Do not interpolate IC history and sat hist files
      if (any(interpolate_output)) then
         call setup_history_interpolation(interp_ok, pfiles-2,                &
              interpolate_output, interpolate_info)
         do hf = 1, pfiles - 2
            if((.not. is_satfile(hf)) .and. (.not. is_initfile(hf))) then
               do fld_idx = 1, nflds(hf)
                  fname = trim(file(hf)%hlist(fld_idx)%field%name)
                  if (field_part_of_vector(fname, mname, zname)) then
                     if (len_trim(mname) > 0) then
                        ! This field is a zonal part of a set,
                        !   find the meridional partner
                        do ff = 1, nflds(hf)
                           if ( trim(mname) ==                                &
                                trim(file(hf)%hlist(ff)%field%name)) then
                              file(hf)%hlist(fld_idx)%field%meridional_complement = ff
                              file(hf)%hlist(ff)%field%zonal_complement = f
                              exit
                           end if
                           if (ff == nflds(hf)) then
                              call endrun(subname//': No meridional match for '//fname)
                           end if
                        end do
                     else if (len_trim(zname) > 0) then
                        ! This field is a meridional part of a set,
                        !   find the zonal partner
                        do ff = 1, nflds(hf)
                           if ( trim(zname) ==                                &
                                trim(file(hf)%hlist(ff)%field%name)) then
                              file(hf)%hlist(fld_idx)%field%zonal_complement = ff
                              file(hf)%hlist(ff)%field%meridional_complement = f
                              exit
                           end if
                           if (ff == nflds(hf)) then
                              call endrun(subname//': No zonal match for '//fname))
                           end if
                        end do
                     else
                        call endrun(subname//': INTERNAL ERROR, bad vector field')
                     end if
                  end if
               end do
            end if
         end do
      end if
   end subroutine setup_interpolation_and_define_vector_complements

   !#######################################################################

   subroutine init_restart_history (fil_idx)
      use cam_pio_utils,  only: cam_pio_def_dim
      use cam_pio_utils,  only: cam_pio_handle_error

      !------------------------------------------------------------------------
      !
      ! Arguments
      !
      type(file_desc_t), intent(inout) :: File               ! Pio file Handle
      !
      ! Local
      !
      integer :: dimids(4), ndims
      integer :: ierr, i, k

      ! Don't need to write restart data if we have written the file this step
      where (write_file(:))
         regen_hist_file(:) = .false.
      elsewhere
         regen_hist_file(:) = .true.
      end where

   end subroutine init_restart_history

   !#######################################################################

   subroutine write_restart_history(File,                                     &
        yr_spec, mon_spec, day_spec, sec_spec)

      !------------------------------------------------------------------------
      !
      ! Arguments
      !
      type(file_desc_t), intent(inout) :: file  ! PIO restart file pointer
      integer, intent(in), optional :: yr_spec  ! Simulation year
      integer, intent(in), optional :: mon_spec ! Simulation month
      integer, intent(in), optional :: day_spec ! Simulation day
      integer, intent(in), optional :: sec_spec ! Seconds into current sim. day
      !
      ! Local workspace
      !
      integer :: ierr              ! PIO return valud
      integer :: fil_idx           ! history file index
      integer :: fld_idx           ! index of field on history file
      integer :: regen_hist_int(pfiles) ! For writing out regen_hist_file

      integer, allocatable      ::  interp_output(:)

      integer                   ::  maxnflds


      maxnflds = maxval(nflds)
      allocate(interp_output(pfiles))
      interp_output = 0

      !
      !-----------------------------------------------------------------------
      ! Write the history restart data if necessary
      !-----------------------------------------------------------------------

      regen_hist_int(:) = 0

      if( .not. allocated(restarthistory_files)) then
         allocate(restarthistory_files(pfiles))
      end if

      do fil_idx = 1, pfiles
         ! No need to write history IC restart because it is always
         !   instantaneous
         if (is_initfile(file_index=fil_idx)) then
            regen_hist_file(fil_idx) = .false.
         end if
         ! No need to write restart data for empty files
         if (nflds(fil_idx) == 0) then
            regen_hist_file(fil_idx) = .false.
         end if
         if(regen_hist_file(fil_idx)) then
            regen_hist_int(fil_idx) = 1
            restarthistory_files(fil_idx)%hlist => history_file(fil_idx)%hlist
         end if
      end do

      if(maxval(nflds) <= 0) then
         ! There are no history restart files to write
         return
      end if

      call wshist(regen_hist_file)

      file => history_file

   end subroutine write_restart_history


   !#######################################################################

   subroutine read_restart_history (fil_idx)
      use pio,                 only: pio_inq_dimid, pio_seterrorhandling
      use pio,                 only: pio_inq_varid, pio_inq_dimname
      use cam_pio_utils,       only: cam_pio_openfile, cam_pio_closefile
      use cam_pio_utils,       only: cam_pio_var_info
      use ioFileMod,           only: getfil
      use sat_hist,            only: sat_hist_define, sat_hist_init
      use cam_grid_support,    only: cam_grid_read_dist_array
      use cam_grid_support,    only: cam_grid_num_grids
      use cam_history_support, only: get_hist_coord_index, add_hist_coord
      use constituents,        only: cnst_get_ind, cnst_get_type_byind

      use shr_sys_mod,         only: shr_sys_getenv
      use spmd_utils,          only: mpicom, mpi_character, masterprocid
      !
      !-----------------------------------------------------------------------
      !
      ! Arguments
      !
      type(file_desc_t), intent(inout) :: File            ! unit number
      !
      ! Local workspace
      !
      integer t, f, ff                 ! file, field indices
      integer begdim2                  ! on-node vert start index
      integer enddim2                  ! on-node vert end index
      integer begdim1                  ! on-node dim1 start index
      integer enddim1                  ! on-node dim1 end index
      integer begdim3                  ! on-node chunk or lat start index
      integer enddim3                  ! on-node chunk or lat end index


      integer regen_hist_int(pfiles)
      integer :: ierr

      character(len=max_string_len)  :: locfn       ! Local filename
      character(len=max_fieldname_len), allocatable :: tmpname(:,:)
      integer, allocatable :: decomp(:,:), tmpnumlev(:,:)
      integer, pointer :: nacs(:,:)    ! accumulation counter
      character(len=max_fieldname_len) :: fname_tmp ! local copy of field name
      character(len=max_fieldname_len) :: dname_tmp ! local copy of dim name

      integer :: i, pfiles_dimid

      type(var_desc_t)                 :: vdesc
      integer                          :: ndims, dimids(8)
      integer                          :: tmpdims(8), dimcnt
      integer                          :: dimlens(7)
      integer                          :: mfiles, mdimcnt
      integer                          :: fdims(3)         ! Field dims
      integer                          :: nfdims           ! 2 or 3 (for 2D,3D)
      integer                          :: fdecomp          ! Grid ID for field
      integer                          :: idx
      integer                          :: err_handling
      character(len=3)                 :: mixing_ratio

      !
      ! Get users logname and machine hostname
      !
      if ( masterproc )then
         logname = ' '
         call shr_sys_getenv ('LOGNAME',logname,ierr)
         host = ' '
         call shr_sys_getenv ('HOST',host,ierr)
      end if
      ! PIO requires netcdf attributes have consistant values on all tasks
      call mpi_bcast(logname, len(logname), mpi_character, masterprocid,      &
           mpicom, ierr)
      call mpi_bcast(host,    len(host),    mpi_character, masterprocid,      &
           mpicom, ierr)

      call pio_seterrorhandling(File, PIO_BCAST_ERROR, oldmethod=err_handling)

      ierr = pio_inq_dimid(File, 'pfiles', pfiles_dimid)
      if(ierr /= PIO_NOERR) then
         if(masterproc) then
            write(iulog,*) 'Not reading history info from restart file', ierr
         end if
         return   ! no history info in restart file
      end if
      call pio_seterrorhandling(File, err_handling)

      ierr = pio_inq_dimlen(File, pfiles_dimid, mfiles)

      ierr = pio_inq_dimid(File, 'maxnflds', dimid)
      ierr = pio_inq_dimlen(File, dimid, maxnflds)

      ierr = pio_inq_dimid(File, 'maxvarmdims', dimid)
      ierr = pio_inq_dimlen(File, dimid, maxvarmdims)

      ierr = pio_inq_varid(File, 'regen_hist_file', vdesc)
      ierr = pio_get_var(File, vdesc, regen_hist_int(1:mfiles))

      ierr = pio_inq_varid(File, 'nflds', vdesc)
      ierr = pio_get_var(File, vdesc, nflds(1:mfiles))
      ierr = pio_inq_varid(File, 'nfils', vdesc)
      ierr = pio_get_var(File, vdesc, nfils(1:mfiles))
      ierr = pio_inq_varid(File, 'mfilt', vdesc)
      ierr = pio_get_var(File, vdesc, mfilt(1:mfiles))

      ierr = pio_inq_varid(File, 'cpath', vdesc)
      ierr = pio_get_var(File, vdesc, cpath(1:mfiles))
      ierr = pio_inq_varid(File, 'nhfil', vdesc)
      ierr = pio_get_var(File, vdesc, nhfil(1:mfiles))
      ierr = pio_inq_varid(File, 'hrestpath', vdesc)
      ierr = pio_get_var(File, vdesc, hrestpath(1:mfiles))


      ierr = pio_inq_varid(File, 'ndens', vdesc)
      ierr = pio_get_var(File, vdesc, ndens(1:mfiles))
      ierr = pio_inq_varid(File, 'ncprec', vdesc)
      ierr = pio_get_var(File, vdesc, ncprec(1:mfiles))
      ierr = pio_inq_varid(File, 'beg_time', vdesc)
      ierr = pio_get_var(File, vdesc, beg_time(1:mfiles))


      ierr = pio_inq_varid(File, 'fincl', vdesc)
      ierr = pio_get_var(File, vdesc, fincl(:,1:mfiles))

      ierr = pio_inq_varid(File, 'fincllonlat', vdesc)
      ierr = pio_get_var(File, vdesc, fincllonlat(:,1:mfiles))

      ierr = pio_inq_varid(File, 'fexcl', vdesc)
      ierr = pio_get_var(File, vdesc, fexcl(:,1:mfiles))

      ierr = pio_inq_varid(File, 'lcltod_start', vdesc)
      ierr = pio_get_var(File, vdesc, lcltod_start(1:mfiles))

      ierr = pio_inq_varid(File, 'lcltod_stop', vdesc)
      ierr = pio_get_var(File, vdesc, lcltod_stop(1:mfiles))




      allocate(tmpname(maxnflds, mfiles), decomp(maxnflds, mfiles), tmpnumlev(maxnflds,mfiles))
      ierr = pio_inq_varid(File, 'field_name', vdesc)
      ierr = pio_get_var(File, vdesc, tmpname)

      ierr = pio_inq_varid(File, 'decomp_type', vdesc)
      ierr = pio_get_var(File, vdesc, decomp)
      ierr = pio_inq_varid(File, 'numlev', vdesc)
      ierr = pio_get_var(File, vdesc, tmpnumlev)

      allocate(tmpprec(maxnflds,mfiles))
      ierr = pio_inq_varid(File, 'hwrt_prec',vdesc)
      ierr = pio_get_var(File, vdesc, tmpprec(:,:))

      allocate(xyfill(maxnflds,mfiles))
      ierr = pio_inq_varid(File, 'xyfill', vdesc)
      ierr = pio_get_var(File, vdesc, xyfill)

      allocate(is_subcol(maxnflds,mfiles))
      ierr = pio_inq_varid(File, 'is_subcol', vdesc)
      ierr = pio_get_var(File, vdesc, is_subcol)

      !! interpolated output
      allocate(interp_output(mfiles))
      ierr = pio_inq_varid(File, 'interpolate_output', vdesc)
      ierr = pio_get_var(File, vdesc, interp_output)
      interpolate_output(1:mfiles) = interp_output(1:mfiles) > 0
      if (pfiles > mfiles) then
         interpolate_output(mfiles+1:pfiles) = .false.
      end if
      ierr = pio_inq_varid(File, 'interpolate_type', vdesc)
      ierr = pio_get_var(File, vdesc, interp_output)
      do t = 1, mfiles
         if (interpolate_output(fil_idx)) then
            interpolate_info(fil_idx)%interp_type = interp_output(fil_idx)
         end if
      end do
      ierr = pio_inq_varid(File, 'interpolate_gridtype', vdesc)
      ierr = pio_get_var(File, vdesc, interp_output)
      do t = 1, mfiles
         if (interpolate_output(fil_idx)) then
            interpolate_info(fil_idx)%interp_gridtype = interp_output(fil_idx)
         end if
      end do
      ierr = pio_inq_varid(File, 'interpolate_nlat', vdesc)
      ierr = pio_get_var(File, vdesc, interp_output)
      do t = 1, mfiles
         if (interpolate_output(fil_idx)) then
            interpolate_info(fil_idx)%interp_nlat = interp_output(fil_idx)
         end if
      end do
      ierr = pio_inq_varid(File, 'interpolate_nlon', vdesc)
      ierr = pio_get_var(File, vdesc, interp_output)
      do t = 1, mfiles
         if (interpolate_output(fil_idx)) then
            interpolate_info(fil_idx)%interp_nlon = interp_output(fil_idx)
         end if
      end do

      !! mdim indices
      allocate(allmdims(maxvarmdims,maxnflds,mfiles))
      ierr = pio_inq_varid(File, 'mdims', vdesc)
      ierr = pio_get_var(File, vdesc, allmdims)

      !! mdim names
      ! Read the hist coord names to make sure they are all registered
      ierr = pio_inq_varid(File, 'mdimnames', vdesc)
      call cam_pio_var_info(File, vdesc, ndims, dimids, dimlens)
      mdimcnt = dimlens(2)
      allocate(mdimnames(mdimcnt))
      ierr = pio_get_var(File, vdesc, mdimnames)
      do f = 1, mdimcnt
         ! Check to see if the mdim is registered
         if (get_hist_coord_index(trim(mdimnames(fld_idx))) <= 0) then
            ! We need to register this mdim (hist_coord)
            call add_hist_coord(trim(mdimnames(fld_idx)))
         end if
      end do

      regen_hist_file(:) = .false.

      allocate(history_file(mfiles))

      file => history_file

      do t = 1, mfiles

         if(regen_hist_int(fil_idx) == 1) then
            regen_hist_file(fil_idx) = .true.
         end if

         call strip_null(cpath(fil_idx))
         call strip_null(hrestpath(fil_idx))
         allocate(file(fil_idx)%hlist(nflds(fil_idx)))

         do f = 1,nflds(fil_idx)

      allocate(gridsonfile(cam_grid_num_grids() + 1, pfiles))
      gridsonfile = -1
      do t = 1, pfiles
         do f = 1, nflds(fil_idx)
            call set_field_dimensions(file(fil_idx)%hlist(fld_idx)%field)

            begdim1 = file(fil_idx)%hlist(fld_idx)%field%begdim1
            enddim1 = file(fil_idx)%hlist(fld_idx)%field%enddim1
            begdim2 = file(fil_idx)%hlist(fld_idx)%field%begdim2
            enddim2 = file(fil_idx)%hlist(fld_idx)%field%enddim2
            begdim3 = file(fil_idx)%hlist(fld_idx)%field%begdim3
            enddim3 = file(fil_idx)%hlist(fld_idx)%field%enddim3

            allocate(file(fil_idx)%hlist(fld_idx)%hbuf(begdim1:enddim1,begdim2:enddim2,begdim3:enddim3))
            if (file(fil_idx)%hlist(fld_idx)%avgflag .eq. 'S') then ! allocate the variance buffer for standard dev
               allocate(file(fil_idx)%hlist(fld_idx)%sbuf(begdim1:enddim1,begdim2:enddim2,begdim3:enddim3))
            endif

            if (associated(file(fil_idx)%hlist(fld_idx)%varid)) then
               deallocate(file(fil_idx)%hlist(fld_idx)%varid)
            end if
            nullify(file(fil_idx)%hlist(fld_idx)%varid)
            if (associated(file(fil_idx)%hlist(fld_idx)%nacs)) then
               deallocate(file(fil_idx)%hlist(fld_idx)%nacs)
            end if
            nullify(file(fil_idx)%hlist(fld_idx)%nacs)
            if(file(fil_idx)%hlist(fld_idx)%field%flag_xyfill .or. (avgflag_perfile(fil_idx) == 'L')) then
               allocate (file(fil_idx)%hlist(fld_idx)%nacs(begdim1:enddim1,begdim3:enddim3))
            else
               allocate(file(fil_idx)%hlist(fld_idx)%nacs(1,begdim3:enddim3))
            end if
            ! initialize all buffers to zero - this will be overwritten later by the
            ! data in the history restart file if it exists.
            call h_zero(f,t)

            ! Make sure this field's decomp is listed on the file
            fdecomp = file(fil_idx)%hlist(fld_idx)%field%decomp_type
            do ff = 1, size(gridsonfile, 1)
               if (fdecomp == gridsonfile(ff, t)) then
                  exit
               else if (gridsonfile(ff, t) < 0) then
                  gridsonfile(ff, t) = fdecomp
                  exit
               end if
            end do

         end do
      end do
      !
      !-----------------------------------------------------------------------
      ! Read history restart files
      !-----------------------------------------------------------------------
      !
      ! Loop over the total number of history files declared and
      ! read the pathname for any history restart files
      ! that are present (if any). Test to see if the run is a restart run
      ! AND if any history buffer regen files exist (regen_hist_file = .T.).
      ! NOTE: regen_hist_file is preset to false, reset to true earlier in
      !      this routine if hbuf restart files are written and saved in the
      !      master restart file. Each history buffer restart file is then read.
      ! NOTE: some f90 compilers (e.g. SGI) complain about I/O of
      !      derived types which have pointer components, so explicitly read
      !       each one.
      !
      do t = 1, mfiles
         if (regen_hist_file(fil_idx)) then
            !
            ! Open history restart file
            !
            call getfil (hrestpath(fil_idx), locfn)
            call cam_pio_openfile(file(fil_idx)%File, locfn, 0)
            !
            ! Read history restart file
            !
            do f = 1, nflds(fil_idx)

               fname_tmp = strip_suffix(file(fil_idx)%hlist(fld_idx)%field%name)
               if(masterproc) write(iulog, *) 'Reading history variable ',fname_tmp
               ierr = pio_inq_varid(file(fil_idx)%File, fname_tmp, vdesc)

               call cam_pio_var_info(file(fil_idx)%File, vdesc, ndims, dimids, dimlens)
               if(.not. associated(file(fil_idx)%hlist(fld_idx)%field%mdims)) then
                  dimcnt = 0
                  do i = 1,ndims
                     ierr = pio_inq_dimname(file(fil_idx)%File, dimids(i), dname_tmp)
                     dimid = get_hist_coord_index(dname_tmp)
                     if(dimid >= 1) then
                        dimcnt = dimcnt + 1
                        tmpdims(dimcnt) = dimid
                        ! No else, just looking for mdims (grid dims won't be hist coords)
                     end if
                  end do
                  if(dimcnt > 0) then
                     allocate(file(fil_idx)%hlist(fld_idx)%field%mdims(dimcnt))
                     file(fil_idx)%hlist(fld_idx)%field%mdims(:) = tmpdims(1:dimcnt)
                     if(dimcnt > maxvarmdims) maxvarmdims = dimcnt
                  end if
               end if
               call set_field_dimensions(file(fil_idx)%hlist(fld_idx)%field)
               begdim1 = file(fil_idx)%hlist(fld_idx)%field%begdim1
               enddim1 = file(fil_idx)%hlist(fld_idx)%field%enddim1
               fdims(1) = enddim1 - begdim1 + 1
               begdim2 = file(fil_idx)%hlist(fld_idx)%field%begdim2
               enddim2 = file(fil_idx)%hlist(fld_idx)%field%enddim2
               fdims(2) = enddim2 - begdim2 + 1
               begdim3 = file(fil_idx)%hlist(fld_idx)%field%begdim3
               enddim3 = file(fil_idx)%hlist(fld_idx)%field%enddim3
               fdims(3) = enddim3 - begdim3 + 1
               if (fdims(2) > 1) then
                  nfdims = 3
               else
                  nfdims = 2
                  fdims(2) = fdims(3)
               end if
               fdecomp = file(fil_idx)%hlist(fld_idx)%field%decomp_type
               if (nfdims > 2) then
                  call cam_grid_read_dist_array(file(fil_idx)%File, fdecomp,              &
                       fdims(1:nfdims), dimlens(1:ndims), file(fil_idx)%hlist(fld_idx)%hbuf, vdesc)
               else
                  call cam_grid_read_dist_array(file(fil_idx)%File, fdecomp,              &
                       fdims(1:nfdims), dimlens(1:ndims), file(fil_idx)%hlist(fld_idx)%hbuf(:,1,:), vdesc)
               end if

               if ( associated(file(fil_idx)%hlist(fld_idx)%sbuf) ) then
                  ! read in variance for standard deviation
                  ierr = pio_inq_varid(file(fil_idx)%File, trim(fname_tmp)//'_var', vdesc)
                  if (nfdims > 2) then
                     call cam_grid_read_dist_array(file(fil_idx)%File, fdecomp,              &
                          fdims(1:nfdims), dimlens(1:ndims), file(fil_idx)%hlist(fld_idx)%sbuf, vdesc)
                  else
                     call cam_grid_read_dist_array(file(fil_idx)%File, fdecomp,              &
                          fdims(1:nfdims), dimlens(1:ndims), file(fil_idx)%hlist(fld_idx)%sbuf(:,1,:), vdesc)
                  end if
               endif

               ierr = pio_inq_varid(file(fil_idx)%File, trim(fname_tmp)//'_nacs', vdesc)
               call cam_pio_var_info(file(fil_idx)%File, vdesc, nacsdimcnt, dimids, dimlens)

               if(nacsdimcnt > 0) then
                  if (nfdims > 2) then
                     ! nacs only has 2 dims (no levels)
                     fdims(2) = fdims(3)
                  end if
                  allocate(file(fil_idx)%hlist(fld_idx)%nacs(begdim1:enddim1,begdim3:enddim3))
                  nacs => file(fil_idx)%hlist(fld_idx)%nacs(:,:)
                  call cam_grid_read_dist_array(file(fil_idx)%File, fdecomp, fdims(1:2),  &
                       dimlens(1:nacsdimcnt), nacs, vdesc)
               else
                  allocate(file(fil_idx)%hlist(fld_idx)%nacs(1,begdim3:enddim3))
                  ierr = pio_get_var(file(fil_idx)%File, vdesc, nacsval)
                  file(fil_idx)%hlist(fld_idx)%nacs(1,:) = nacsval
               end if

            end do
            !
            ! Done reading this history restart file
            !
            call cam_pio_closefile(file(fil_idx)%File)

         end if  ! regen_hist_file(fil_idx)

         ! (re)create the master list of grid IDs
         ff = 0
         do f = 1, size(gridsonfile, 1)
            if (gridsonfile(f, t) > 0) then
               ff = ff + 1
            end if
         end do
         allocate(file(fil_idx)%grid_ids(ff))
         ff = 1
         do f = 1, size(gridsonfile, 1)
            if (gridsonfile(f, t) > 0) then
               file(fil_idx)%grid_ids(ff) = gridsonfile(f, t)
               ff = ff + 1
            end if
         end do
         call patch_init(fil_idx)
      end do     ! end of do mfiles loop

      !
      ! If the history files are partially complete (contain less than
      ! mfilt(fil_idx) time samples, then get the files and open them.)
      !
      ! NOTE:  No need to perform this operation for IC history files or empty files
      !

      do t = 1,mfiles
         if (is_initfile(file_index = t)) then
            ! Initialize filename specifier for IC file
            hfilename_spec(fil_idx) = '%c.cam' // trim(inst_suffix) // '.i.%y-%m-%d-%s.nc'
            nfils(fil_idx) = 0
         else if (nflds(fil_idx) == 0) then
            nfils(fil_idx) = 0
         else
            if (nfils(fil_idx) > 0) then
               call getfil (cpath(fil_idx), locfn)
               call cam_pio_openfile(file(fil_idx)%File, locfn, PIO_WRITE)
               call h_inquire (fil_idx)
               if(is_satfile(fil_idx)) then
                  !  Initialize the sat following history subsystem
                  call sat_hist_init()
                  call sat_hist_define(file(fil_idx)%File)
               end if
            end if
            !
            ! If the history file is full, close the current unit
            !
            if (nfils(fil_idx) >= mfilt(fil_idx)) then
               if (masterproc) then
                  write(iulog,*)'READ_RESTART_HISTORY: nf_close(',t,') = ',nhfil(fil_idx), mfilt(fil_idx)
               end if
               do f = 1,nflds(fil_idx)
                  deallocate(file(fil_idx)%hlist(fld_idx)%varid)
                  nullify(file(fil_idx)%hlist(fld_idx)%varid)
               end do
               call cam_pio_closefile(file(fil_idx)%File)
               nfils(fil_idx) = 0
            end if
         end if
      end do

      ! Setup vector pairs for unstructured grid interpolation
      call setup_interpolation_and_define_vector_complements()

      if(mfiles /= pfiles .and. masterproc) then
         write(iulog,*) ' WARNING: Restart file pfiles setting ',mfiles,' not equal to model setting ',pfiles
      end if

      return
   end subroutine read_restart_history

   !#######################################################################

   recursive function get_entry_by_name(listentry, name) result(entry)
      type(master_entry),  pointer :: listentry
      character(len=*), intent(in) :: name ! variable name
      type(master_entry), pointer :: entry

      if(associated(listentry)) then
         if(listentry%field%name .eq. name) then
            entry => listentry
         else
            entry =>get_entry_by_name(listentry%next_entry, name)
         end if
      else
         nullify(entry)
      end if
   end function get_entry_by_name

   !#######################################################################

   subroutine AvgflagToString(avgflag, time_op)
      ! Dummy arguments
      character(len=1),           intent(in)  :: avgflag ! averaging flag
      character(len=max_chars),   intent(out) :: time_op ! time op (e.g. max)

      ! Local variable
      character(len=*), parameter             :: subname = 'AvgflagToString'

      select case (avgflag)
      case ('A')
         time_op(:) = 'mean'
      case ('B')
         time_op(:) = 'mean00z'
      case ('I')
         time_op(:) = ' '
      case ('X')
         time_op(:) = 'maximum'
      case ('M')
         time_op(:) = 'minimum'
      case('L')
         time_op(:) = LT_DESC
      case ('S')
         time_op(:) = 'standard_deviation'
      case default
         call endrun(subname//': unknown avgflag = '//avgflag)
      end select
   end subroutine AvgflagToString

   !#######################################################################

   subroutine create_field_list()

      use cam_grid_support, only: cam_grid_num_grids
      use spmd_utils,       only: mpicom
      use dycore,           only: dycore_is

      !-----------------------------------------------------------------------
      !
      ! Purpose: Define the contents of each history file based on namelist
      !         input for initial or branch run, and restart data if a
      !         restart run.
      !
      ! Method: Use arrays fincl and fexcl to modify default history file
      !         contents.
      !         Then sort the result alphanumerically for later use by OUTFLD to
      !         allow an n log n search time.
      !
      !---------------------------Local variables-----------------------------
      !
      integer fil_idx                ! file index
      integer fld_idx                ! field index
      integer ff                     ! index into include, exclude and fprec list
      integer :: i
      ! name: field name portion of fincl (i.e. no avgflag separator)
      character(len=fieldname_len) :: name
      ! mastername: name from masterlist field
      character(len=max_fieldname_len) :: mastername
      character(len=max_chars) :: errormsg ! error output field
      character(len=1) :: avgflag    ! averaging flag
      character(len=1) :: prec_wrt   ! history buffer write precision flag

      type (hentry) :: tmp           ! temporary used for swapping

      type(master_entry), pointer :: listentry
      logical                     :: fieldonfile      ! .true. iff field on file
      integer                     :: errors_found

      ! List of active grids (first dim) for each file (second dim)
      ! An active grid is one for which there is a least one field being output
      !    on that grid.
      integer, allocatable        :: gridsonfile(:,:)
      ! The following list of field names are only valid for the FV dycore.
      !     They appear in fincl settings of WACCM use case files which are
      !     not restricted to the FV dycore.
      ! To avoid duplicating long fincl lists in use case files to provide
      !     both FV and non-FV versions this short list of fields is checked
      !     for and removed from fincl lists when the dycore is not FV.
      integer, parameter :: n_fv_only = 10
      character(len=6) :: fv_only_flds(n_fv_only) = &
           [ 'VTHzm ', 'WTHzm ', 'UVzm  ', 'UWzm  ', 'Uzm   ',                &
           'Vzm   ', 'Wzm   ', 'THzm  ', 'TH    ', 'MSKtem' ]

      integer :: n_vec_comp, add_fincl_idx
      integer, parameter :: nvecmax = 50 ! max number of vector components in a fincl list
      character(len=2) :: avg_suffix
      character(len=max_fieldname_len) :: vec_comp_names(nvecmax)
      character(len=1)                 :: vec_comp_avgflag(nvecmax)
      character(len=*), parameter      :: subname = 'create_field_list'
      !--------------------------------------------------------------------------

      ! First ensure contents of fincl, fexcl, and fout_prec are all valid names
      !
      errors_found = 0
      do fil_idx = 1, pfiles

         n_vec_comp = 0
         vec_comp_names = ' '
         vec_comp_avgflag = ' '
         do fld_idx = 1, pflds
            if  (len_trim(fincl(fld_idx, fil_idx)) == 0) then
               exit ! No more fields on this file
            else
               add_fincl_idx = fld_idx ! Last used index
            end if
            name = getname(fincl(fld_idx, fil_idx))

            if (.not. dycore_is('FV')) then
               ! filter out fields only provided by FV dycore
               fieldonfile = .false.
               do i = 1, n_fv_only
                  if (name == fv_only_flds(i)) then
                     write(errormsg,'(4a,2(i0,a))') subname, ': ',            &
                          trim(name), ' in fincl(', fil_idx, ', ', fld_idx,   &
                          ') only available with FV dycore'
                     if (masterproc) then
                        write(iulog, *) trim(errormsg)
                     end if
                     fieldonfile = .true.
                     exit
                  end if
               end do
               if (fieldonfile) then
                  cycle ! We are excluding this FV-only field
               end if
            end if

            mastername = ''
            listentry => get_entry_by_name(masterlinkedlist, name)
            if (associated(listentry)) then
               mastername = listentry%field%name
            end if
            if (name /= mastername) then
               write(errormsg,'(4a,2(i0,a))') subname, ': ', trim(name),      &
                    ' in fincl(', fld_idx, ', ', fil_idx, ') not found'
               if (masterproc) then
                  write(iulog, *) trim(errormsg)
               end if
               errors_found = errors_found + 1
            else
               if ( (len_trim(mastername) > 0) .and.                          &
                    interpolate_output(fil_idx)) then
                  if (n_vec_comp >= nvecmax) then
                     call endrun(subname//': need to increase nvecmax')
                  end if
                  ! If this is a vector component, save name of the complement
                  avgflag = getflag(fincl(fld_idx, fil_idx))
                  if (len_trim(listentry%meridional_field) > 0) then
                     n_vec_comp = n_vec_comp + 1
                     vec_comp_names(n_vec_comp) = listentry%meridional_field
                     vec_comp_avgflag(n_vec_comp) = avgflag
                  else if (len_trim(listentry%zonal_field) > 0) then
                     n_vec_comp = n_vec_comp + 1
                     vec_comp_names(n_vec_comp) = listentry%zonal_field
                     vec_comp_avgflag(n_vec_comp) = avgflag
                  end if
               end if
            end if
         end do

         ! Interpolation of vector components requires that both be present.
         ! If the fincl specifier contains any vector components, then the
         ! complement was saved in the array vec_comp_names.  Next ensure
         ! (for interpolated output only) that all complements
         ! are also present in the fincl array.
         ! The first empty slot in the current fincl array is index fld_idx
         !     from loop above.
         add_fincl_idx = add_fincl_idx + 1
         if ((add_fincl_idx > 1) .and. interpolate_output(fil_idx)) then
            do i = 1, n_vec_comp
               call list_index(fincl(:, fil_idx), vec_comp_names(i), ff)
               if (ff == 0) then

                  ! Add vector component to fincl.  Don't need to check
                  !    whether its in the master list since this was done at
                  !    the time of registering the vector components.
                  avg_suffix = '  '
                  if (len_trim(vec_comp_avgflag(i)) > 0) then
                     avg_suffix = ':' // vec_comp_avgflag(i)
                  end if
                  fincl(add_fincl_idx, fil_idx) =                             &
                       trim(vec_comp_names(i)) // avg_suffix
                  add_fincl_idx = add_fincl_idx + 1

                  write(errormsg,'(4a,i0,2a))') subname, ': ',                &
                       trim(vec_comp_names(i)), ' added to fincl', fil_idx,   &
                       '.  Both vector components are required for '          &
                       'interpolated output.'
                  if (masterproc) then
                     write(iulog,*) trim(errormsg)
                  end if
               end if
            end do
         end if

         ! Check for non-existant excluded fields
         fld_idx = 1
         do while ((fld_idx < pflds) .and.                                    &
              (len_trim(fexcl(fld_idx, fil_idx)) > ' '))
            mastername = ''
            listentry => get_entry_by_name(masterlinkedlist,                  &
                 fexcl(fld_idx, fil_idx))
            if(associated(listentry)) then
               mastername = listentry%field%name
            end if

            if (fexcl(fld_idx, fil_idx) /= mastername) then
               write(errormsg,'(4a,2(i0,a))') subname, ': ',                  &
                    trim(fexcl(fld_idx, fil_idx)), ' in fexcl(', fld_idx,     &
                    ', ', fil_idx, ') not found'
               if (masterproc) then
                  write(iulog,*) trim(errormsg)
               end if
               errors_found = errors_found + 1
            end if
            fld_idx = fld_idx + 1
         end do

         fld_idx = 1
         do while ((fld_idx < pflds) .and.                                    &
              (len_trim(fout_prec(fld_idx,fil_idx)) > 0))
            name = getname(fout_prec(fld_idx,fil_idx))
            mastername = ''
            listentry => get_entry_by_name(masterlinkedlist, name)
            if(associated(listentry)) then
               mastername = listentry%field%name
            end if
            if (name /= mastername) then
               write(errormsg,'(4a,i0,a)') subname, ': ', trim(name),         &
                    ' in fout_prec(', fld_idx, ') not found'
               if (masterproc) then
                  write(iulog,*) trim(errormsg)
               end if
               errors_found = errors_found + 1
            end if
            do ff = 1, fld_idx - 1       ! If duplicate entry is found, stop
               if (trim(name) == trim(getname(fout_prec(ff,t)))) then
                  write(errormsg,'(4a)') subname, ': Duplicate field ',       &
                       trim(name), ' in fout_prec'
                  if (masterproc) then
                     write(iulog,*) trim(errormsg)
                  end if
                  errors_found = errors_found + 1
               end if
            end do
            f = f + 1
         end do
      end do

      if (errors_found > 0) then
         ! Give masterproc a chance to write all the log messages
         call mpi_barrier(mpicom, t)
         write(errormsg, '(2a,i0,a)') subname, ': ',errors_found,             &
              ' errors found, see log'
         call endrun(trim(errormsg))
      end if

      nflds(:) = 0
      ! IC history file is to be created, set properties
      if(is_initfile()) then
         hfilename_spec(pfiles) = '%c.cam' // trim(inst_suffix) // '.i.%y-%m-%d-%s.nc'

         ncprec(pfiles) = pio_double
         ndens (pfiles) = 1
         mfilt (pfiles) = 1
      end if

      allocate(gridsonfile(cam_grid_num_grids() + 1, pfiles))
      gridsonfile = -1
      do fil_idx = 1, pfiles
         !
         ! Add the field to the file if specified via namelist
         ! (FINCL[1-pfiles]), or if it is on by default and was not
         ! excluded via namelist (FEXCL[1-pfiles]).
         ! Also set history buffer accumulation and output precision
         ! values according to the values specified via namelist
         ! (FWRTPR[1-pfiles] in namelist, FOUT_PREC)
         ! or, if not on the list, to the default values given by
         !  ndens(fil_idx).
         !
         listentry => masterlinkedlist
         do while(associated(listentry))
            mastername = listentry%field%name
            call list_index (fincl(1,t), mastername, ff)
            fieldonfile = .false.
            if (ff > 0) then
               fieldonfile = .true.
            else if ((.not. empty_hfiles) .or.                                &
                 (is_initfile(file_index = t))) then
               call list_index (fexcl(1,t), mastername, ff)
               if ((ff == 0) .and. listentry%actflag(fil_idx)) then
                  fieldonfile = .true.
               end if
            end if
            if (fieldonfile) then
               ! The field is active so increment the number fo fields and add
               ! its decomp type to the list of decomp types on this file
               nflds(fil_idx) = nflds(fil_idx) + 1
               do ff = 1, size(gridsonfile, 1)
                  if (listentry%field%decomp_type == gridsonfile(ff, t)) then
                     exit
                  else if (gridsonfile(ff, t) < 0) then
                     gridsonfile(ff, t) = listentry%field%decomp_type
                     exit
                  end if
               end do
            end if
            listentry => listentry%next_entry
         end do
      end do
      !
      ! Determine total number of active history files
      !
      if (masterproc) then
         do fil_idx = 1, pfiles
            if (nflds(fil_idx) == 0) then
               write(iulog, '(2a,i0,a)') subname, ': File ', t, ' is empty'
            end if
         end do
      endif
      allocate(history_file(pfiles))
      file = >history_file


      do fil_idx = 1, pfiles
         nullify(file(fil_idx)%hlist)
         ! Now we have a field count and can allocate
         if(nflds(fil_idx) > 0) then
            ! Allocate the correct number of hentry slots
            allocate(file(fil_idx)%hlist(nflds(fil_idx)))
            ! Count up the number of grids output on this file
            ff = 0
            do f = 1, size(gridsonfile, 1)
               if (gridsonfile(f, t) > 0) then
                  ff = ff + 1
               end if
            end do
            allocate(file(fil_idx)%grid_ids(ff))
            ff = 1
            do f = 1, size(gridsonfile, 1)
               if (gridsonfile(f, t) > 0) then
                  file(fil_idx)%grid_ids(ff) = gridsonfile(f, t)
                  ff = ff + 1
               end if
            end do
         end if
         do ff = 1,nflds(fil_idx)
            nullify(file(fil_idx)%hlist(ff)%hbuf)
            nullify(file(fil_idx)%hlist(ff)%sbuf)
            nullify(file(fil_idx)%hlist(ff)%nacs)
            nullify(file(fil_idx)%hlist(ff)%varid)
         end do


         nflds(fil_idx) = 0 ! recount to support array based method
         listentry => masterlinkedlist
         do while(associated(listentry))
            mastername = listentry%field%name

            call list_index (fout_prec(1,t), mastername, ff)
            if (ff > 0) then
               prec_wrt = getflag(fout_prec(ff,t))
            else
               prec_wrt = ' '
            end if

            call list_index (fincl(1,t), mastername, ff)

            if (ff > 0) then
               avgflag = getflag (fincl(ff,t))
               call inifld (t, listentry, avgflag,  prec_wrt)
            else if ((.not. empty_hfiles) .or. (is_initfile(file_index = t))) then
               call list_index (fexcl(1,t), mastername, ff)
               if (ff == 0 .and. listentry%actflag(fil_idx)) then
                  call inifld (t, listentry, ' ', prec_wrt)
               else
                  listentry%actflag(fil_idx) = .false.
               end if
            else
               listentry%actflag(fil_idx) = .false.
            end if
            listentry =>listentry%next_entry

         end do
         !
         ! If column output is specified make sure there are some fields defined
         ! for that file
         !
         if (nflds(fil_idx) .eq. 0 .and. fincllonlat(1,t) .ne. ' ') then
            write(errormsg,'(a,i2,a)') 'FLDLST: Column output is specified for file ',t,' but no fields defined for that file.'
            call endrun(errormsg)
         else
            call patch_init(fil_idx)
         end if
         !
         ! Specification of file contents now complete.  Sort each list of active
         ! entries for efficiency in OUTFLD.  Simple bubble sort.
         !
         !!XXgoldyXX: v In the future, we will sort according to decomp to speed I/O
         do f = nflds(fil_idx)-1,1,-1
            do ff = 1,f

               if (file(fil_idx)%hlist(ff)%field%name > file(fil_idx)%hlist(ff+1)%field%name) then

                  tmp = file(fil_idx)%hlist(ff)
                  file(fil_idx)%hlist(ff  ) = file(fil_idx)%hlist(ff+1)
                  file(fil_idx)%hlist(ff+1) = tmp

               else if (file(fil_idx)%hlist(ff  )%field%name == file(fil_idx)%hlist(ff+1)%field%name) then

                  write(errormsg,'(2a,2(a,i3))') 'FLDLST: Duplicate field: ', &
                       trim(file(fil_idx)%hlist(ff)%field%name),', file = ', t, ', ff = ', ff
                  call endrun(errormsg)

               end if

            end do
         end do

      end do    ! do fil_idx = 1, pfiles
      deallocate(gridsonfile)

      call print_active_field_list()

      !
      ! Packing density, ndens: With netcdf, only 1 (nf_double) and 2 (pio_real)
      ! are allowed
      !
      do fil_idx = 1, pfiles
         if (ndens(fil_idx) == 1) then
            ncprec(fil_idx) = pio_double
         else if (ndens(fil_idx) == 2) then
            ncprec(fil_idx) = pio_real
         else
            call endrun ('FLDLST: ndens must be 1 or 2')
         end if

      end do
      ! Flush any waiting output
      if (masterproc) then
         call shr_sys_flush(iulog)
      end if

      !
      !  Now that masterlinkedlist is defined, construct primary and
      !     secondary hashing tables.
      !
      call bld_outfld_hash_tbls()
      call bld_hfilefld_indices()

   end subroutine create_field_list

   !###########################################################################

   subroutine print_active_field_list()

      integer :: f, ff, i, t
      integer :: num_patches

      character(len=6) :: prec_str
      character(len=max_chars) :: fldname, fname_tmp

      type(active_entry), pointer :: hfile(:) => null()  ! history files

      character(len=*), parameter :: subname = 'print_active_field_list'

      if (masterproc) then
         hfile => history_file
         do fil_idx = 1, pfiles
            if (nflds(fil_idx) > 0) then
               write(iulog, *) ' '
               write(iulog, '(a,i2,a,i4,a)') 'FLDLST: History file ',         &
                    fil_idx,' contains ', nflds(fil_idx), ' fields'

               if (is_initfile(file_index=fil_idx)) then
                  write(iulog, '(3a)') ' Write frequency:                 ',  &
                       inithist,' (INITIAL CONDITIONS)'
               else
                  if (nhtfrq(fil_idx) == 0) then
                     write(iulog, *) ' Write frequency:                  MONTHLY'
                  else
                     write(iulog, *) ' Write frequency:                 ',    &
                          nhtfrq(fil_idx)
                  end if
               end if

               write(iulog, *) ' Filename specifier:              ',          &
                    trim(hfilename_spec(fil_idx))

               prec_str = 'double'
               if (ndens(fil_idx) == 2) then
                  prec_str = 'single'
               end if
               write(iulog,*) ' Output precision:                ', prec_str
               write(iulog,*) ' Number of time samples per file: ', mfilt(fil_idx)

               ! grid info
               if (associated(hfile(fil_idx)%patches)) then
                  write(iulog,*) ' Fields are represented on columns (FIELD_LON_LAT)'
               else if (associated(hfile(fil_idx)%grid_ids)) then
                  write(iulog,*) ' Fields are represented on global grids:'
                  do i = 1, size(hfile(fil_idx)%grid_ids)
                     write(iulog,*) ' ', hfile(fil_idx)%grid_ids(i)
                  end do
               else
                  call endrun(subname//': error in active_entry object')
               end if
               write(iulog,*)' Included fields are:'
            end if

            do f = 1, nflds(fil_idx)
               if (associated(hfile(fil_idx)%patches)) then
                  num_patches = size(hfile(fil_idx)%patches)
                  fldname = strip_suffix(hfile(fil_idx)%hlist(fld_idx)%field%name)
                  do i = 1, num_patches
                     ff = (f-1)*num_patches + i
                     fname_tmp = trim(fldname)
                     call hfile(fil_idx)%patches(i)%field_name(fname_tmp)
                     write(iulog, 9000) ff, fname_tmp,                        &
                          hfile(fil_idx)%hlist(fld_idx)%field%units,          &
                          hfile(fil_idx)%hlist(fld_idx)%field%numlev,         &
                          hfile(fil_idx)%hlist(fld_idx)%avgflag,   &
                          trim(hfile(fil_idx)%hlist(fld_idx)%field%long_name)
                  end do
               else
                  fldname = hfile(fil_idx)%hlist(fld_idx)%field%name
                  write(iulog,9000) f, fldname,                               &
                       hfile(fil_idx)%hlist(fld_idx)%field%units,             &
                       hfile(fil_idx)%hlist(fld_idx)%field%numlev,            &
                       hfile(fil_idx)%hlist(fld_idx)%avgflag,                 &
                       trim(hfile(fil_idx)%hlist(fld_idx)%field%long_name)
               end if
            end do
         end do
      end if

9000  format(i5, 1x, a32, 1x, a16, 1x, i4, 1x, a1, 2x, 256a)

   end subroutine print_active_field_list

   !###########################################################################

   subroutine inifld (t, listentry, avgflag, prec_wrt)
      use cam_grid_support, only: cam_grid_is_zonal
      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: Add a field to the active list for a history file
      !
      ! Method: Copy the data from the master field list to the active list for the file
      !         Also: define mapping arrays from (col,chunk) -> (lon,lat)
      !
      ! Author: CCM Core Group
      !
      !-----------------------------------------------------------------------


      !
      ! Arguments
      !
      integer, intent(in)          :: t         ! history file index

      type(master_entry), pointer  :: listentry

      character(len=1), intent(in) :: avgflag   ! averaging flag
      character(len=1), intent(in) :: prec_wrt  ! history output precision flag
      !
      ! Local workspace
      !
      integer :: n                  ! field index on defined file


      !
      ! Ensure that it is not to late to add a field to the history file
      !
      if (hfiles_defined) then
         call endrun ('INIFLD: Attempt to add field '//listentry%field%name//' after history files set')
      end if

      nflds(fil_idx) = nflds(fil_idx) + 1
      n = nflds(fil_idx)
      !
      ! Copy field info.
      !
      if(n > size(file(fil_idx)%hlist)) then
         write(iulog,*) 'file field miscount error ', n, size(file(fil_idx)%hlist)
         call endrun()
      end if

      file(fil_idx)%hlist(n)%field = listentry%field

      select case (prec_wrt)
      case (' ')
         if (ndens(fil_idx) == 1) then
            file(fil_idx)%hlist(n)%hwrt_prec = 8
         else
            file(fil_idx)%hlist(n)%hwrt_prec = 4
         end if
      case ('4')
         file(fil_idx)%hlist(n)%hwrt_prec = 4
         if (masterproc) then
            write(iulog,*) 'INIFLD: Output data type for ', file(fil_idx)%hlist(n)%field%name, &
                 ' is real*4'
         end if
      case ('8')
         file(fil_idx)%hlist(n)%hwrt_prec = 8
         if (masterproc) then
            write(iulog,*) 'INIFLD: Output data type for ', file(fil_idx)%hlist(n)%field%name, &
                 ' is real*8'
         end if
      case default
         call endrun ('INIFLD: unknown prec_wrt = '//prec_wrt)
      end select
      !
      ! Override the default averaging (masterlist) averaging flag if non-blank
      !
      if (avgflag == ' ') then
         file(fil_idx)%hlist(n)%avgflag = listentry%avgflag(fil_idx)
         file(fil_idx)%hlist(n)%time_op = listentry%time_op(fil_idx)
      else
         file(fil_idx)%hlist(n)%avgflag = avgflag
         call AvgflagToString(avgflag, file(fil_idx)%hlist(n)%time_op)
      end if

      ! Some things can't be done with zonal fields
      if (cam_grid_is_zonal(listentry%field%decomp_type)) then
         if (file(fil_idx)%hlist(n)%avgflag == 'L') then
            call endrun("Cannot perform local time processing on zonal data ("//trim(listentry%field%name)//")")
         else if (is_satfile(fil_idx)) then
            call endrun("Zonal data not valid for satellite history ("//trim(listentry%field%name)//")")
         end if
      end if

#ifdef HDEBUG
      if (masterproc) then
         write(iulog,'(a,i0,3a,i0,a,i2)')'HDEBUG: ',__LINE__,' field ',          &
              trim(file(fil_idx)%hlist(n)%field%name), ' added as field number ', n,   &
              ' on file ', t
         write(iulog,'(2a)')'  units = ',trim(file(fil_idx)%hlist(n)%field%units)
         write(iulog,'(a,i0)')'  numlev = ',file(fil_idx)%hlist(n)%field%numlev
         write(iulog,'(2a)')'  avgflag = ',file(fil_idx)%hlist(n)%avgflag
         write(iulog,'(3a)')'  time_op = "',trim(file(fil_idx)%hlist(n)%time_op),'"'
         write(iulog,'(a,i0)')'  hwrt_prec = ',file(fil_idx)%hlist(n)%hwrt_prec
      end if
#endif

      return
   end subroutine inifld


   subroutine patch_init(fil_idx)
      use cam_history_support, only: history_patch_t
      use cam_grid_support,    only: cam_grid_compute_patch

      ! Dummy arguments
      integer, intent(in)               :: t     ! Current file

      ! Local variables
      integer                           :: ff    ! Loop over fincllonlat entries
      integer                           :: i     ! General loop index
      integer                           :: npatches
      type(history_patch_t), pointer    :: patchptr

      character(len=max_chars)          :: errormsg
      character(len=max_chars)          :: lonlatname(pflds)
      real(r8)                          :: beglon, beglat, endlon, endlat

      !
      ! Setup column information if this field will be written as group
      ! First verify the column information in the namelist
      ! Duplicates are an error, but we can just ignore them
      !

      ! I know, this shouldn't happen . . . yet: (better safe than sorry)
      if (associated(file(fil_idx)%patches)) then
         do i = 1, size(file(fil_idx)%patches)
            call file(fil_idx)%patches(i)%deallocate()
         end do
         deallocate(file(fil_idx)%patches)
         nullify(file(fil_idx)%patches)
      end if

      ! First, count the number of patches and check for duplicates
      ff = 1  ! Index of fincllonlat entry
      npatches = 0   ! Number of unique patches in namelist entry
      do while (len_trim(fincllonlat(ff, t)) > 0)
         npatches = npatches + 1
         lonlatname(npatches) = trim(fincllonlat(ff, t))
         ! Check for duplicates
         do i = 1, npatches - 1
            if (trim(lonlatname(i)) == trim(lonlatname(npatches))) then
               write(errormsg, '(a,i0,3a)') 'Duplicate fincl', t, 'lonlat entry.', &
                    'Duplicate entry is ', trim(lonlatname(i))
               write(iulog, *) 'patch_init: WARNING: '//errormsg
               ! Remove the new entry
               lonlatname(npatches) = ''
               npatches = npatches - 1
               exit
            end if
         end do
         ff = ff + 1
      end do

      ! Now we know how many patches, allocate space
      if (npatches > 0) then
         if (collect_column_output(fil_idx)) then
            allocate(file(fil_idx)%patches(1))
         else
            allocate(file(fil_idx)%patches(npatches))
         end if

         ! For each lat/lon specification, parse and create a patch for each grid
         do ff = 1, npatches
            if (collect_column_output(fil_idx)) then
               ! For colleccted column output, we only have one patch
               patchptr => file(fil_idx)%patches(1)
            else
               patchptr => file(fil_idx)%patches(ff)
               patchptr%namelist_entry = trim(lonlatname(ff))
            end if
            ! We need to set up one patch per (active) grid
            patchptr%collected_output = collect_column_output(fil_idx)
            call parseLonLat(lonlatname(ff),                                      &
                 beglon, endlon, patchptr%lon_axis_name,                          &
                 beglat, endlat, patchptr%lat_axis_name)
            if (associated(patchptr%patches)) then
               ! One last sanity check
               if (.not. collect_column_output(fil_idx)) then
                  write(errormsg, '(a,i0,2a)') 'Attempt to overwrite fincl', t,     &
                       'lonlat entry, ', trim(patchptr%namelist_entry)
                  call endrun('patch_init: '//errormsg)
               end if
            else
               allocate(patchptr%patches(size(file(fil_idx)%grid_ids)))
            end if
            do i = 1, size(file(fil_idx)%grid_ids)
               call cam_grid_compute_patch(file(fil_idx)%grid_ids(i), patchptr%patches(i),&
                    beglon, endlon, beglat, endlat, collect_column_output(fil_idx))
            end do
            nullify(patchptr)
         end do
      end if
      ! We are done processing this file's fincl#lonlat entries. Now,
      ! compact each patch so that the output variables have no holes
      ! We wait until now for when collect_column_output(fil_idx) is .true. since
      !    all the fincl#lonlat entries are concatenated
      if (associated(file(fil_idx)%patches)) then
         do ff = 1, size(file(fil_idx)%patches)
            call file(fil_idx)%patches(ff)%compact()
         end do
      end if

   end subroutine patch_init

   !#######################################################################

   subroutine strip_null(str)
      character(len=*), intent(inout) :: str
      integer :: i
      do i = 1,len(str)
         if(ichar(str(i:i)) == 0) str(i:i) = ' '
      end do
   end subroutine strip_null

   character(len=max_fieldname_len) function strip_suffix (name)
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
      !
      ! Local workspace
      !
      integer :: n
      !
      !-----------------------------------------------------------------------
      !
      strip_suffix = ' '

      do n = 1,fieldname_len
         strip_suffix(n:n) = name(n:n)
         if(name(n+1:n+1         ) == ' '                       ) return
         if(name(n+1:n+fieldname_suffix_len) == fieldname_suffix) return
      end do

      strip_suffix(fieldname_len+1:max_fieldname_len) = name(fieldname_len+1:max_fieldname_len)

      return

   end function strip_suffix

   !#######################################################################

   character(len=fieldname_len) function getname (inname)
      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: retrieve name portion of inname
      !
      ! Method:  If an averaging flag separater character is present (":") in inname,
      !          lop it off
      !
      !-------------------------------------------------------------------------------
      !
      ! Arguments
      !
      character(len=*), intent(in) :: inname
      !
      ! Local workspace
      !
      integer :: length
      integer :: i

      length = len (inname)

      if (length < fieldname_len .or. length > fieldname_lenp2) then
         write(iulog,*) 'GETNAME: bad length = ',length
         call endrun
      end if

      getname = ' '
      do i = 1,fieldname_len
         if (inname(i:i) == ':') exit
         getname(i:i) = inname(i:i)
      end do

      return
   end function getname

   !#######################################################################

   ! parseRangeString: Parse either a coordinate descriptor (e.g., 10S) or a
   !                   coordinate range (e.g., 10e:20e)
   !                   chars represents the allowed coordinate character.
   !                   NB: Does not validate numerical values (e.g., lat <= 90)
   subroutine parseRangeString(rangestr, chars, begval, begchar, begname,     &
        endval, endchar, endname)

      ! Dummy arguments
      character(len=*),       intent(in)    :: rangestr
      character(len=*),       intent(in)    :: chars
      real(r8),               intent(out)   :: begval
      character,              intent(out)   :: begchar
      character(len=*),       intent(out)   :: begname
      real(r8),               intent(out)   :: endval
      character,              intent(out)   :: endchar
      character(len=*),       intent(out)   :: endname

      ! Local variables
      character(len=128)                    :: errormsg
      integer                               :: colonpos
      integer                               :: beglen, endlen

      ! First, see if we have a position or a range
      colonpos = scan(rangestr, ':')
      if (colonpos == 0) then
         begname = trim(rangestr)
         beglen = len_trim(begname)
         endname = trim(begname)
      else
         beglen = colonpos - 1
         begname = rangestr(1:beglen)
         endname = trim(rangestr(colonpos+1:))
         endlen = len_trim(endname)
      end if
      ! begname should be a number (integer or real) followed by a character
      if (verify(begname, '0123456789.') /= beglen) then
         write(errormsg, *) 'Coordinate range must begin with number, ', begname
         call endrun('parseRangeString: '//errormsg)
      end if
      if (verify(begname(beglen:beglen), chars) /= 0) then
         write(errormsg, *) 'Coordinate range must end with character in the ',  &
              'set [', trim(chars), '] ', begname
         call endrun('parseRangeString: '//errormsg)
      end if
      ! begname parses so collect the values
      read(begname(1:beglen-1), *) begval
      begchar = begname(beglen:beglen)
      if (colonpos /= 0) then
         ! endname should be a number (integer or real) followed by a character
         if (verify(endname, '0123456789.') /= endlen) then
            write(errormsg, *) 'Coordinate range must begin with number, ', endname
            call endrun('parseRangeString: '//errormsg)
         end if
         if (verify(endname(endlen:endlen), chars) /= 0) then
            write(errormsg, *) 'Coordinate range must end with character in the ',&
                 'set [', trim(chars), '] ', endname
            call endrun('parseRangeString: '//errormsg)
         end if
         ! endname parses so collect the values
         read(endname(1:endlen-1), *) endval
         endchar = endname(endlen:endlen)
      else
         endval = begval
         endchar = begchar
      end if

   end subroutine parseRangeString

   ! parseLonLat: Parse a lon_lat description allowed by the fincllonlat(n)
   !              namelist entries. Returns the starting and ending values of
   !              the point or range specified.
   !              NB: Does not validate the range against any particular grid
   subroutine parseLonLat(lonlatname, beglon, endlon, lonname, beglat, endlat, latname)

      ! Dummy arguments
      character(len=*),       intent(in)    :: lonlatname
      real(r8),               intent(out)   :: beglon
      real(r8),               intent(out)   :: endlon
      character(len=*),       intent(out)   :: lonname
      real(r8),               intent(out)   :: beglat
      real(r8),               intent(out)   :: endlat
      character(len=*),       intent(out)   :: latname

      ! Local variables
      character(len=128)                    :: errormsg
      character(len=MAX_CHARS)              :: lonstr, latstr
      character(len=MAX_CHARS)              :: begname, endname
      character                             :: begchar, endchar
      integer                               :: underpos

      !
      ! make sure _ separator is present
      !
      underpos = scan(lonlatname, '_')
      if (underpos == 0) then
         write(errormsg,*) 'Improperly formatted fincllonlat string. ',          &
              'Missing underscore character (xxxE_yyyS) ', lonlatname
         call endrun('parseLonLat: '//errormsg)
      end if

      ! Break out the longitude and latitude sections
      lonstr = lonlatname(:underpos-1)
      latstr = trim(lonlatname(underpos+1:))

      ! Parse the longitude section
      call parseRangeString(lonstr, 'eEwW', beglon, begchar, begname, endlon, endchar, endname)
      ! Convert longitude to degrees East
      if ((begchar == 'w') .or. (begchar == 'W')) then
         if (beglon > 0.0_r8) then
            beglon = 360._r8 - beglon
         end if
      end if
      if ((beglon < 0._r8) .or. (beglon > 360._r8)) then
         write(errormsg, *) 'Longitude specification out of range, ', trim(begname)
         call endrun('parseLonLat: '//errormsg)
      end if
      if ((endchar == 'w') .or. (endchar == 'W')) then
         if (endlon > 0.0_r8) then
            endlon = 360._r8 - endlon
         end if
      end if
      if ((endlon < 0._r8) .or. (endlon > 360._r8)) then
         write(errormsg, *) 'Longitude specification out of range, ', trim(endname)
         call endrun('parseLonLat: '//errormsg)
      end if
      if (beglon == endlon) then
         lonname = trim(begname)
      else
         lonname = trim(begname)//'_to_'//trim(endname)
      end if

      ! Parse the latitude section
      call parseRangeString(latstr, 'nNsS', beglat, begchar, begname, endlat, endchar, endname)
      ! Convert longitude to degrees East
      if ((begchar == 's') .or. (begchar == 'S')) then
         beglat = (-1._r8) * beglat
      end if
      if ((beglat < -90._r8) .or. (beglat > 90._r8)) then
         write(errormsg, *) 'Latitude specification out of range, ', trim(begname)
         call endrun('parseLonLat: '//errormsg)
      end if
      if ((endchar == 's') .or. (endchar == 'S')) then
         endlat = (-1._r8) * endlat
      end if
      if ((endlat < -90._r8) .or. (endlat > 90._r8)) then
         write(errormsg, *) 'Latitude specification out of range, ', trim(endname)
         call endrun('parseLonLat: '//errormsg)
      end if
      if (beglat == endlat) then
         latname = trim(begname)
      else
         latname = trim(begname)//'_to_'//trim(endname)
      end if

   end subroutine parseLonLat


   !#######################################################################

   character(len=1) function getflag (inname)
      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: retrieve flag portion of inname
      !
      ! Method:  If an averaging flag separater character is present (":") in inname,
      !          return the character after it as the flag
      !
      !-------------------------------------------------------------------------------
      !
      ! Arguments
      !
      character(len=*), intent(in) :: inname   ! character string
      !
      ! Local workspace
      !
      integer :: length         ! length of inname
      integer :: i              ! loop index

      length = len(inname)

      if (length /= fieldname_lenp2) then
         write(iulog,*) 'GETFLAG: bad length = ',length
         call endrun
      end if

      getflag = ' '
      do i = 1,fieldname_lenp2-1
         if (inname(i:i) == ':') then
            getflag = inname(i+1:i+1)
            exit
         end if
      end do

      return
   end function getflag

   !#######################################################################

   subroutine list_index (list, name, index)
      !
      ! Input arguments
      !
      character(len=*), intent(in) :: list(pflds) ! input list of names, possibly ":" delimited
      character(len=max_fieldname_len), intent(in) :: name ! name to be searched for
      !
      ! Output arguments
      !
      integer, intent(out) :: index               ! index of "name" in "list"
      !
      ! Local workspace
      !
      character(len=fieldname_len) :: listname    ! input name with ":" stripped off.
      integer f                       ! field index

      index = 0
      do f=1,pflds
         !
         ! Only list items
         !
         listname = getname (list(fld_idx))
         if (listname == ' ') exit
         if (listname == name) then
            index = f
            exit
         end if
      end do

      return
   end subroutine list_index

   !#######################################################################

   recursive subroutine outfld (fname, field, idim, c, avg_subcol_field)
      use cam_history_buffers, only: hbuf_accum_inst, hbuf_accum_add, hbuf_accum_variance,  &
           hbuf_accum_add00z, hbuf_accum_max, hbuf_accum_min,          &
           hbuf_accum_addlcltime
      use cam_history_support, only: dim_index_2d
      use subcol_pack_mod,     only: subcol_unpack
      use cam_grid_support,    only: cam_grid_id

      interface
         subroutine subcol_field_avg_handler(idim, field_in, c, field_out)
            use shr_kind_mod, only: r8 => shr_kind_r8
            integer,  intent(in)  :: idim
            real(r8), intent(in)  :: field_in(idim, *)
            integer,  intent(in)  :: c
            real(r8), intent(out) :: field_out(:,:)
         end subroutine subcol_field_avg_handler
      end interface

      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: Accumulate (or take min, max, etc. as appropriate) input field
      !          into its history buffer for appropriate files
      !
      ! Method: Check 'masterlist' whether the requested field 'fname' is active
      !         on one or more history files, and if so do the accumulation.
      !         If not found, return silently.
      !         subcol_field_avg_handler:
      !            An interface into subcol_field_avg without creating a dependency as
      !            this would cause a dependency loop. See subcol.F90
      ! Note: We cannot know a priori if field is a grid average field or a subcolumn
      !       field because many fields passed to outfld are defined on ncol rather
      !       than pcols or psetcols. Therefore, we use the avg_subcol_field input
      !       to determine whether to average the field input before accumulation.
      !       NB: If output is on a subcolumn grid (requested in addfle), it is
      !           an error to use avg_subcol_field. A subcolumn field is assumed and
      !           subcol_unpack is called before accumulation.
      !
      ! Author: CCM Core Group
      !
      !-----------------------------------------------------------------------
      !
      ! Arguments
      !
      character(len=*), intent(in) :: fname ! Field name--should be 8 chars long

      ! For structured grids, idim is the local longitude dimension.
      ! For unstructured grids, idim is the local column dimension
      ! For phys_decomp, it should be pcols or pcols*psubcols
      integer, intent(in)           :: idim
      real(r8), intent(in)          :: field(idim,*) ! Array containing field values
      integer, intent(in)           :: c             ! chunk (physics) or latitude (dynamics) index
      logical, optional, intent(in) :: avg_subcol_field
      !
      ! Local variables
      !
      integer               :: t, f          ! file, field indices

      character*1           :: avgflag       ! averaging flag

      type (active_entry), pointer :: ofile(:) ! Local history_file pointer
      real(r8),pointer      :: hbuf(:,:)     ! history buffer
      real(r8),pointer      :: sbuf(:,:)     ! variance buffer
      integer, pointer      :: nacs(:)       ! accumulation counter
      integer               :: begdim2, enddim2, endi
      integer               :: phys_decomp
      type (dim_index_2d)   :: dimind        ! 2-D dimension index
      logical               :: flag_xyfill   ! non-applicable xy points flagged with fillvalue
      real(r8)              :: fillvalue
      real(r8), allocatable :: afield(:,:)   ! Averaged field values
      real(r8), allocatable :: ufield(:,:,:) ! Unpacked field values
      integer               :: ff            ! masterlist index pointer
      integer               :: i, j
      logical               :: found
      logical               :: avg_subcols   ! average subcols before accum
      !-----------------------------------------------------------------------

      call get_field_properties(fname, found, file_out=ofile, ff_out=ff)
      phys_decomp = cam_grid_id('physgrid')

      ! If this field is not active, return now
      if (.not. found) then
         return
      end if

      !
      ! Note, the field may be on any or all of the history files (primary
      ! and auxiliary).
      !
      !      write(iulog,*)'fname_loc=',fname_loc
      do t = 1, pfiles
         if ( .not. masterlist(ff)%thisentry%actflag(fil_idx)) cycle
         f = masterlist(ff)%thisentry%hfileindx(fil_idx)
         !
         ! Update history buffer
         !
         flag_xyfill = ofile(fil_idx)%hlist(fld_idx)%field%flag_xyfill
         fillvalue = ofile(fil_idx)%hlist(fld_idx)%field%fillvalue
         avgflag = ofile(fil_idx)%hlist(fld_idx)%avgflag
         nacs   => ofile(fil_idx)%hlist(fld_idx)%nacs(:,c)
         hbuf => ofile(fil_idx)%hlist(fld_idx)%hbuf(:,:,c)
         if (associated(file(fil_idx)%hlist(fld_idx)%sbuf)) then
            sbuf => ofile(fil_idx)%hlist(fld_idx)%sbuf(:,:,c)
         endif
         dimind = ofile(fil_idx)%hlist(fld_idx)%field%get_dims(c)

         ! See notes above about validity of avg_subcol_field
         if (ofile(fil_idx)%hlist(fld_idx)%field%is_subcol) then
            if (present(avg_subcol_field)) then
               call endrun('OUTFLD: Cannot average '//trim(fname)//', subcolumn output was requested in addfld')
            end if
            avg_subcols = .false.
         else if (ofile(fil_idx)%hlist(fld_idx)%field%decomp_type == phys_decomp) then
            if (present(avg_subcol_field)) then
               avg_subcols = avg_subcol_field
            else
               avg_subcols = .false.
            end if
         else ! Any dynamics decomposition
            if (present(avg_subcol_field)) then
               call endrun('OUTFLD: avg_subcol_field only valid for physgrid')
            else
               avg_subcols = .false.
            end if
         end if

         begdim2 = ofile(fil_idx)%hlist(fld_idx)%field%begdim2
         enddim2 = ofile(fil_idx)%hlist(fld_idx)%field%enddim2
         if (avg_subcols) then
            allocate(afield(pcols, begdim2:enddim2))
            call subcol_field_avg_handler(idim, field, c, afield)
            ! Hack! Avoid duplicating select statement below
            call outfld(fname, afield, pcols, c)
            deallocate(afield)
         else if (ofile(fil_idx)%hlist(fld_idx)%field%is_subcol) then
            ! We have to assume that using mdimnames (e.g., psubcols) is
            ! incompatible with the begdimx, enddimx usage (checked in addfld)
            ! Since psubcols is included in levels, take that out
            endi = (enddim2 - begdim2 + 1) / psubcols
            allocate(ufield(pcols, psubcols, endi))
            allocate(afield(pcols*psubcols, endi))
            do j = 1, endi
               do i = 1, idim
                  afield(i, j) = field(i, j)
               end do
            end do
            ! Initialize unused aray locations.
            if (idim < pcols*psubcols) then
               if (flag_xyfill) then
                  afield(idim+1:pcols*psubcols, :) = fillvalue
               else
                  afield(idim+1:pcols*psubcols, :) = 0.0_r8
               end if
            end if
            if (flag_xyfill) then
               call subcol_unpack(c, afield, ufield, fillvalue)
            else
               call subcol_unpack(c, afield, ufield)
            end if
            deallocate(afield)
            select case (avgflag)

            case ('I') ! Instantaneous
               call hbuf_accum_inst(hbuf, ufield, nacs, dimind, pcols,        &
                    flag_xyfill, fillvalue)

            case ('A') ! Time average
               call hbuf_accum_add(hbuf, ufield, nacs, dimind, pcols,         &
                    flag_xyfill, fillvalue)

            case ('B') ! Time average only 00z values
               call hbuf_accum_add00z(hbuf, ufield, nacs, dimind, pcols,      &
                    flag_xyfill, fillvalue)

            case ('X') ! Maximum over time
               call hbuf_accum_max (hbuf, ufield, nacs, dimind, pcols,        &
                    flag_xyfill, fillvalue)

            case ('M') ! Minimum over time
               call hbuf_accum_min(hbuf, ufield, nacs, dimind, pcols,         &
                    flag_xyfill, fillvalue)

            case ('L')
               call hbuf_accum_addlcltime(hbuf, ufield, nacs, dimind, pcols,  &
                    flag_xyfill, fillvalue, c,                                &
                    ofile(fil_idx)%hlist(fld_idx)%field%decomp_type,                      &
                    lcltod_start(fil_idx), lcltod_stop(fil_idx))

            case ('S') ! Standard deviation
               call hbuf_accum_variance(hbuf, sbuf, ufield, nacs, dimind, pcols,&
                    flag_xyfill, fillvalue)

            case default
               call endrun ('OUTFLD: invalid avgflag='//avgflag)

            end select
            deallocate(ufield)
         else
            select case (avgflag)

            case ('I') ! Instantaneous
               call hbuf_accum_inst(hbuf, field, nacs, dimind, idim,          &
                    flag_xyfill, fillvalue)

            case ('A') ! Time average
               call hbuf_accum_add(hbuf, field, nacs, dimind, idim,           &
                    flag_xyfill, fillvalue)

            case ('B') ! Time average only 00z values
               call hbuf_accum_add00z(hbuf, field, nacs, dimind, idim,        &
                    flag_xyfill, fillvalue)

            case ('X') ! Maximum over time
               call hbuf_accum_max (hbuf, field, nacs, dimind, idim,          &
                    flag_xyfill, fillvalue)

            case ('M') ! Minimum over time
               call hbuf_accum_min(hbuf, field, nacs, dimind, idim,           &
                    flag_xyfill, fillvalue)

            case ('L')
               call hbuf_accum_addlcltime(hbuf, field, nacs, dimind, idim,    &
                    flag_xyfill, fillvalue, c,                                &
                    ofile(fil_idx)%hlist(fld_idx)%field%decomp_type,                      &
                    lcltod_start(fil_idx), lcltod_stop(fil_idx))

            case ('S') ! Standard deviation
               call hbuf_accum_variance(hbuf, sbuf, field, nacs, dimind, idim,&
                    flag_xyfill, fillvalue)

            case default
               call endrun ('OUTFLD: invalid avgflag='//avgflag)

            end select
         end if

      end do

      return
   end subroutine outfld

   !#######################################################################

   subroutine get_field_properties(fname, found, file_out, ff_out, no_file_check_in)

      implicit none
      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: If fname is active, lookup and return field information
      !
      ! Method: Check 'masterlist' whether the requested field 'fname' is active
      !         on one or more history files, and if so, return the requested
      !         field information
      !
      ! Author: goldy
      !
      !-----------------------------------------------------------------------
      !
      ! Arguments
      !
      character(len=*),   intent(in)  :: fname ! Field name--should be 8 chars long
      logical,            intent(out) :: found ! Set to true if fname is active
      type(active_entry), pointer, optional :: file_out(:)
      integer,            intent(out), optional :: ff_out
      logical,            intent(in), optional  :: no_file_check_in

      !
      ! Local variables
      !
      character*(max_fieldname_len) :: fname_loc  ! max-char equivalent of fname
      integer :: t, ff          ! file, masterindex indices
      logical :: no_file_check
      !-----------------------------------------------------------------------

      ! Need to re-cast the field name so that the hashing works #hackalert
      fname_loc = fname
      ff = get_masterlist_indx(fname_loc)

      ! Set the no_file_check to false, unless is passed in
      if (present(no_file_check_in)) then
         no_file_check = no_file_check_in
      else
         no_file_check = .false.
      end if

      ! Set found to .false. so we can return early if fname is not active
      found = .false.
      if (present(file_out)) then
         nullify(file_out)
      end if
      if (present(ff_out)) then
         ff_out = -1
      end if

      !
      !  If ( ff < 0 ), the field is not defined on the masterlist. This check
      !  is necessary because of coding errors calling outfld without first defining
      !  the field on masterlist.
      !
      if ( ff < 0 ) then
         return
      end if
      !
      !  Next, check to see whether this field is active on one or more history
      !  files.
      !
      if (no_file_check) then
         if (present(ff_out)) ff_out   =  ff  ! Set the output index and return without checking files
         return
      else if ( .not. masterlist(ff)%thisentry%act_somefile )  then
         return
      end if
      !
      ! Note, the field may be on any or all of the history files (primary
      ! and auxiliary).
      !

      do t=1, pfiles
         if (masterlist(ff)%thisentry%actflag(fil_idx)) then
            found    =  .true.
            if (present(file_out)) then
               file_out => history_file
            end if
            if (present(ff_out)) then
               ff_out   =  ff
            end if
            ! We found the info so we are done with the loop
            exit
         end if
      end do

   end subroutine get_field_properties

   !#######################################################################

   logical function is_initfile(file_index)
      !
      !------------------------------------------------------------------------
      !
      ! Purpose: to determine:
      !
      !   a) if an IC file is active in this model run at all
      !       OR,
      !   b) if it is active, is the current file index referencing the IC file
      !      (IC file is always at pfiles)
      !
      !------------------------------------------------------------------------
      !
      ! Arguments
      !
      integer, intent(in), optional :: file_index ! index of file in question

      is_initfile = .false.

      if (present(file_index)) then
         if ((inithist /= 'NONE') .and. (file_index == pfiles)) then
            is_initfile = .true.
         end if
      else
         if (inithist /= 'NONE') then
            is_initfile = .true.
         end if
      end if

   end function is_initfile

   !#######################################################################

   integer function strcmpf (name1, name2)
      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: Return the lexical difference between two strings
      !
      ! Method: Use ichar() intrinsic as we loop through the names
      !
      !-----------------------------------------------------------------------
      !
      ! Arguments
      !
      character(len=max_fieldname_len), intent(in) :: name1 ! string to compare
      character(len=max_fieldname_len), intent(in) :: name2 ! string to compare
      integer                                      :: n     ! loop index

      do n = 1, max_fieldname_len
         strcmpf = ichar(name1(n:n)) - ichar(name2(n:n))
         if (strcmpf /= 0) then
            exit
         end if
      end do

   end function strcmpf

   !#######################################################################

   subroutine h_inquire (fil_idx)
      use pio,           only: pio_inq_varid, pio_inq_attlen
      use cam_pio_utils, only: cam_pio_handle_error
      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: Ensure that the proper variables are on a history file
      !
      ! Method: Issue the appropriate netcdf wrapper calls
      !
      !-----------------------------------------------------------------------
      !
      ! Arguments
      !
      integer, intent(in) :: t   ! file index
      !
      ! Local workspace
      !
      integer                  :: f            ! field index
      integer                  :: ierr
      integer                  :: i
      integer                  :: num_patches
      integer(pio_offset_kind) :: mdimsize
      character(len=max_chars) :: fldname, fname_tmp, basename

      !
      !
      ! Dimension id's
      !
      file => history_file



      !
      ! Create variables for model timing and header information
      !
      if(.not. is_satfile(fil_idx)) then
         ierr=pio_inq_varid (file(fil_idx)%File,'ndcur   ',    file(fil_idx)%ndcurid)
         ierr=pio_inq_varid (file(fil_idx)%File,'nscur   ',    file(fil_idx)%nscurid)
         ierr=pio_inq_varid (file(fil_idx)%File,'nsteph  ',    file(fil_idx)%nstephid)

         ierr=pio_inq_varid (file(fil_idx)%File,'time_bnds',   file(fil_idx)%tbndid)
         ierr=pio_inq_varid (file(fil_idx)%File,'date_written',file(fil_idx)%date_writtenid)
         ierr=pio_inq_varid (file(fil_idx)%File,'time_written',file(fil_idx)%time_writtenid)
#if ( defined BFB_CAM_SCAM_IOP )
         ierr=pio_inq_varid (file(fil_idx)%File,'tsec    ',file(fil_idx)%tsecid)
         ierr=pio_inq_varid (file(fil_idx)%File,'bdate   ',file(fil_idx)%bdateid)
#endif
         if (.not. is_initfile(file_index=t) ) then
            ! Don't write the GHG/Solar forcing data to the IC file.  It is never
            ! read from that file so it's confusing to have it there.
            ierr=pio_inq_varid (file(fil_idx)%File,'co2vmr  ',    file(fil_idx)%co2vmrid)
            ierr=pio_inq_varid (file(fil_idx)%File,'ch4vmr  ',    file(fil_idx)%ch4vmrid)
            ierr=pio_inq_varid (file(fil_idx)%File,'n2ovmr  ',    file(fil_idx)%n2ovmrid)
            ierr=pio_inq_varid (file(fil_idx)%File,'f11vmr  ',    file(fil_idx)%f11vmrid)
            ierr=pio_inq_varid (file(fil_idx)%File,'f12vmr  ',    file(fil_idx)%f12vmrid)
            ierr=pio_inq_varid (file(fil_idx)%File,'sol_tsi ',    file(fil_idx)%sol_tsiid)
            if (solar_parms_on) then
               ierr=pio_inq_varid (file(fil_idx)%File,'f107    ',    file(fil_idx)%f107id)
               ierr=pio_inq_varid (file(fil_idx)%File,'f107a   ',    file(fil_idx)%f107aid)
               ierr=pio_inq_varid (file(fil_idx)%File,'f107p   ',    file(fil_idx)%f107pid)
               ierr=pio_inq_varid (file(fil_idx)%File,'kp      ',    file(fil_idx)%kpid)
               ierr=pio_inq_varid (file(fil_idx)%File,'ap      ',    file(fil_idx)%apid)
            endif
            if (solar_wind_on) then
               ierr=pio_inq_varid (file(fil_idx)%File,'byimf', file(fil_idx)%byimfid)
               ierr=pio_inq_varid (file(fil_idx)%File,'bzimf', file(fil_idx)%bzimfid)
               ierr=pio_inq_varid (file(fil_idx)%File,'swvel', file(fil_idx)%swvelid)
               ierr=pio_inq_varid (file(fil_idx)%File,'swden', file(fil_idx)%swdenid)
            endif
            if (epot_active) then
               ierr=pio_inq_varid (file(fil_idx)%File,'colat_crit1', file(fil_idx)%colat_crit1_id)
               ierr=pio_inq_varid (file(fil_idx)%File,'colat_crit2', file(fil_idx)%colat_crit2_id)
            endif
         end if
      end if
      ierr=pio_inq_varid (file(fil_idx)%File,'date    ',    file(fil_idx)%dateid)
      ierr=pio_inq_varid (file(fil_idx)%File,'datesec ',    file(fil_idx)%datesecid)
      ierr=pio_inq_varid (file(fil_idx)%File,'time    ',    file(fil_idx)%timeid)


      !
      ! Obtain variable name from ID which was read from restart file
      !
      do f=1,nflds(fil_idx)
         if(.not. associated(file(fil_idx)%hlist(fld_idx)%varid)) then
            if (associated(file(fil_idx)%patches)) then
               allocate(file(fil_idx)%hlist(fld_idx)%varid(size(file(fil_idx)%patches)))
            else
               allocate(file(fil_idx)%hlist(fld_idx)%varid(1))
            end if
         end if
         !
         ! If this field will be put out as columns then get column names for field
         !
         if (associated(file(fil_idx)%patches)) then
            num_patches = size(file(fil_idx)%patches)
            fldname = strip_suffix(file(fil_idx)%hlist(fld_idx)%field%name)
            do i = 1, num_patches
               fname_tmp = trim(fldname)
               call file(fil_idx)%patches(i)%field_name(fname_tmp)
               ierr = pio_inq_varid(file(fil_idx)%File, trim(fname_tmp), file(fil_idx)%hlist(fld_idx)%varid(i))
               call cam_pio_handle_error(ierr, 'H_INQUIRE: Error getting ID for '//trim(fname_tmp))
               ierr = pio_get_att(file(fil_idx)%File, file(fil_idx)%hlist(fld_idx)%varid(i), 'basename', basename)
               call cam_pio_handle_error(ierr, 'H_INQUIRE: Error getting basename for '//trim(fname_tmp))
               if (trim(fldname) /= trim(basename)) then
                  call endrun('H_INQUIRE: basename ('//trim(basename)//') does not match fldname ('//trim(fldname)//')')
               end if
            end do
         else
            fldname = file(fil_idx)%hlist(fld_idx)%field%name
            ierr = pio_inq_varid(file(fil_idx)%File, trim(fldname), file(fil_idx)%hlist(fld_idx)%varid(1))
            call cam_pio_handle_error(ierr, 'H_INQUIRE: Error getting ID for '//trim(fldname))
         end if
         if(file(fil_idx)%hlist(fld_idx)%field%numlev>1) then
            ierr = pio_inq_attlen(file(fil_idx)%File,file(fil_idx)%hlist(fld_idx)%varid(1),'mdims', mdimsize)
            if(.not. associated(file(fil_idx)%hlist(fld_idx)%field%mdims)) then
               allocate(file(fil_idx)%hlist(fld_idx)%field%mdims(mdimsize))
            end if
            ierr=pio_get_att(file(fil_idx)%File,file(fil_idx)%hlist(fld_idx)%varid(1),'mdims', &
                 file(fil_idx)%hlist(fld_idx)%field%mdims(1:mdimsize))
            if(mdimsize>maxvarmdims) maxvarmdims=mdimsize
         end if

      end do

      if(masterproc) then
         write(iulog,*)'H_INQUIRE: Successfully opened netcdf file '
      end if

      return
   end subroutine h_inquire

   !#######################################################################

   subroutine add_default (name, tindex, flag)
      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: Add a field to the default "on" list for a given history file
      !
      ! Method:
      !
      !-----------------------------------------------------------------------
      !
      ! Arguments
      !
      character(len=*), intent(in) :: name  ! field name
      character(len=1), intent(in) :: flag  ! averaging flag

      integer, intent(in) :: tindex         ! history file index
      !
      ! Local workspace
      !
      integer :: t            ! file index
      type(master_entry), pointer :: listentry

      if (hfiles_defined) then
         call endrun ('ADD_DEFAULT: Attempt to add hist default '//trim(name)//' after history files set')
      end if
      !
      ! Check validity of input arguments
      !
      if (tindex > pfiles) then
         write(iulog,*)'ADD_DEFAULT: file index=', tindex, ' is too big'
         call endrun
      end if

      ! Add to IC file if tindex = 0, reset to pfiles
      if (tindex == 0) then
         t = pfiles
         if ( .not. is_initfile(file_index=t) ) return
      else
         t = tindex
      end if

      if (verify(flag, HIST_AVG_FLAGS) /= 0) then
         call endrun ('ADD_DEFAULT: unknown averaging flag='//flag)
      end if
      !
      ! Look through master list for input field name.  When found, set active
      ! flag for that file to true.  Also set averaging flag if told to use other
      ! than default.
      !
      listentry => get_entry_by_name(masterlinkedlist, trim(name))
      if(.not.associated(listentry)) then
         call endrun ('ADD_DEFAULT: field = "'//trim(name)//'" not found')
      end if
      listentry%actflag(fil_idx) = .true.
      if (flag /= ' ') then
         listentry%avgflag(fil_idx) = flag
         call AvgflagToString(flag, listentry%time_op(fil_idx))
      end if

      return
   end subroutine add_default

   !#######################################################################

   subroutine h_override (fil_idx)
      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: Override default history file contents for a specific file
      !
      ! Method: Copy the flag into the master field list
      !
      !-----------------------------------------------------------------------
      !
      ! Arguments
      !
      integer, intent(in) :: t         ! history file index
      !
      ! Local workspace
      !
      character(len=1) :: avgflg       ! lcl equiv of avgflag_perfile(fil_idx) (to address xlf90 compiler bug)

      type(master_entry), pointer :: listentry


      avgflg = avgflag_perfile(fil_idx)


      listentry=>masterlinkedlist
      do while(associated(listentry))
         call AvgflagToString(avgflg, listentry%time_op(fil_idx))
         listentry%avgflag(fil_idx) = avgflag_perfile(fil_idx)
         listentry=>listentry%next_entry
      end do

   end subroutine h_override

   !#######################################################################

   subroutine h_define (t, restart)
      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: Define contents of history file t
      !
      ! Method: Issue the required netcdf wrapper calls to define the history file contents
      !
      !-----------------------------------------------------------------------
      use cam_grid_support, only: cam_grid_header_info_t
      use cam_grid_support, only: cam_grid_write_attr, cam_grid_write_var
      use time_manager,     only: get_step_size, get_ref_date, timemgr_get_calendar_cf
      use cam_abortutils,   only: endrun
      use cam_pio_utils,    only: vdesc_ptr, cam_pio_handle_error, cam_pio_def_dim
      use cam_pio_utils,    only: cam_pio_createfile, cam_pio_def_var
      use sat_hist,         only: sat_hist_define

      !-----------------------------------------------------------------------

      !
      ! Input arguments
      !
      integer, intent(in) :: t   ! file index
      logical, intent(in) :: restart
      !
      ! Local workspace
      !
      integer :: i, j            ! longitude, latitude indices
      integer :: grd             ! indices for looping through grids
      integer :: f               ! field index
      integer :: ncreal          ! real data type for output
      integer :: dtime           ! timestep size
      integer :: sec_nhtfrq      ! nhtfrq converted to seconds
      integer :: ndbase = 0      ! days component of base time
      integer :: nsbase = 0      ! seconds component of base time
      integer :: nbdate          ! base date in yyyymmdd format
      integer :: nbsec           ! time of day component of base date [seconds]
      integer :: yr, mon, day    ! year, month, day components of a date

      character(len=max_chars) :: str       ! character temporary
      character(len=max_chars) :: fname_tmp ! local copy of field name
      character(len=max_chars) :: calendar  ! Calendar type
      character(len=max_chars) :: cell_methods ! For cell_methods attribute
      character(len=16)        :: time_per_freq
      character(len=128)       :: errormsg

      integer :: ret                        ! function return value

      !
      ! netcdf dimensions
      !
      integer :: chardim            ! character dimension id
      integer :: dimenchar(2)       ! character dimension ids
      integer :: nacsdims(2)        ! dimension ids for nacs (used in restart file)
      integer :: bnddim             ! bounds dimension id
      integer :: timdim             ! unlimited dimension id

      integer :: dimindex(8)        ! dimension ids for variable declaration
      integer :: dimids_tmp(8)      ! dimension ids for variable declaration

      !
      ! netcdf variables
      !
      ! A structure to hold the horizontal dimension and coordinate info
      type(cam_grid_header_info_t), allocatable :: header_info(:)
      ! For satellite files and column output
      type(vdesc_ptr), allocatable :: latvar(:)    ! latitude variable ids
      type(vdesc_ptr), allocatable :: lonvar(:)    ! longitude variable ids

      type(var_desc_t), pointer        :: varid => NULL() ! temporary variable descriptor
      integer                          :: num_hdims, fdims
      integer                          :: num_patches ! How many entries for a field on this file?
      integer,          pointer        :: mdims(:) => NULL()
      integer                          :: mdimsize
      integer                          :: ierr
      integer,          allocatable    :: mdimids(:)
      integer                          :: amode
      logical                          :: interpolate
      logical                          :: patch_output

      if(restart) then
         file => restarthistory_files
         if(masterproc) write(iulog,*)'Opening netcdf history restart file ', trim(hrestpath(fil_idx))
      else
         file => history_file
         if(masterproc) write(iulog,*)'Opening netcdf history file ', trim(nhfil(fil_idx))
      end if

      amode = PIO_CLOBBER

      if(restart) then
         call cam_pio_createfile (file(fil_idx)%File, hrestpath(fil_idx), amode)
      else
         call cam_pio_createfile (file(fil_idx)%File, nhfil(fil_idx), amode)
      end if
      if(is_satfile(fil_idx)) then
         interpolate = .false. ! !!XXgoldyXX: Do we ever want to support this?
         patch_output = .false.
         call cam_pio_def_dim(file(fil_idx)%File, 'ncol', pio_unlimited, timdim)
         call cam_pio_def_dim(file(fil_idx)%File, 'nbnd', 2, bnddim)

         allocate(latvar(1), lonvar(1))
         allocate(latvar(1)%vd, lonvar(1)%vd)
         call cam_pio_def_var(file(fil_idx)%File, 'lat', pio_double, (/timdim/),       &
              latvar(1)%vd)
         ierr=pio_put_att (file(fil_idx)%File, latvar(1)%vd, 'long_name', 'latitude')
         ierr=pio_put_att (file(fil_idx)%File, latvar(1)%vd, 'units', 'degrees_north')

         call cam_pio_def_var(file(fil_idx)%File, 'lon', pio_double, (/timdim/),       &
              lonvar(1)%vd)
         ierr=pio_put_att (file(fil_idx)%File, lonvar(1)%vd,'long_name','longitude')
         ierr=pio_put_att (file(fil_idx)%File, lonvar(1)%vd,'units','degrees_east')

      else
         !
         ! Setup netcdf file - create the dimensions of lat,lon,time,level
         !
         ! interpolate is only supported for unstructured dycores
         interpolate = (interpolate_output(fil_idx) .and. (.not. restart))
         patch_output = (associated(file(fil_idx)%patches) .and. (.not. restart))

         ! First define the horizontal grid dims
         ! Interpolation is special in that we ignore the native grids
         if(interpolate) then
            allocate(header_info(1))
            call cam_grid_write_attr(file(fil_idx)%File, interpolate_info(fil_idx)%grid_id, header_info(1))
         else if (patch_output) then
            ! We are doing patch (column) output
            if (allocated(header_info)) then
               ! We shouldn't have any header_info yet
               call endrun('H_DEFINE: header_info should not be allocated for patch output')
            end if
            do i = 1, size(file(fil_idx)%patches)
               call file(fil_idx)%patches(i)%write_attrs(file(fil_idx)%File)
            end do
         else
            allocate(header_info(size(file(fil_idx)%grid_ids)))
            do i = 1, size(file(fil_idx)%grid_ids)
               call cam_grid_write_attr(file(fil_idx)%File, file(fil_idx)%grid_ids(i), header_info(i))
            end do
         end if   ! interpolate

         ! Define the unlimited time dim
         call cam_pio_def_dim(file(fil_idx)%File, 'time', pio_unlimited, timdim)
         call cam_pio_def_dim(file(fil_idx)%File, 'nbnd', 2, bnddim, existOK=.true.)
         call cam_pio_def_dim(file(fil_idx)%File, 'chars', 8, chardim)
      end if   ! is satfile

      ! Populate the history coordinate (well, mdims anyway) attributes
      ! This routine also allocates the mdimids array
      call write_hist_coord_attrs(file(fil_idx)%File, bnddim, mdimids, restart)

      call get_ref_date(yr, mon, day, nbsec)
      nbdate = yr*10000 + mon*100 + day
      ierr=pio_def_var (file(fil_idx)%File,'time',pio_double,(/timdim/),file(fil_idx)%timeid)
      ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%timeid, 'long_name', 'time')
      str = 'days since ' // date2yyyymmdd(nbdate) // ' ' // sec2hms(nbsec)
      ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%timeid, 'units', trim(str))

      calendar = timemgr_get_calendar_cf()
      ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%timeid, 'calendar', trim(calendar))


      ierr=pio_def_var (file(fil_idx)%File,'date    ',pio_int,(/timdim/),file(fil_idx)%dateid)
      str = 'current date (YYYYMMDD)'
      ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%dateid, 'long_name', trim(str))


      ierr=pio_def_var (file(fil_idx)%File,'datesec ',pio_int,(/timdim/), file(fil_idx)%datesecid)
      str = 'current seconds of current date'
      ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%datesecid, 'long_name', trim(str))

      !
      ! Character header information
      !
      str = 'CF-1.0'
      ierr=pio_put_att (file(fil_idx)%File, PIO_GLOBAL, 'Conventions', trim(str))
      ierr=pio_put_att (file(fil_idx)%File, PIO_GLOBAL, 'source', 'CAM')
#if ( defined BFB_CAM_SCAM_IOP )
      ierr=pio_put_att (file(fil_idx)%File, PIO_GLOBAL, 'CAM_GENERATED_FORCING','create SCAM IOP dataset')
#endif
      ierr=pio_put_att (file(fil_idx)%File, PIO_GLOBAL, 'case',caseid)
      ierr=pio_put_att (file(fil_idx)%File, PIO_GLOBAL, 'logname',logname)
      ierr=pio_put_att (file(fil_idx)%File, PIO_GLOBAL, 'host', host)

      ! Put these back in when they are filled properly
      !    ierr=pio_put_att (file(fil_idx)%File, PIO_GLOBAL, 'title',ctitle)
      !    ierr= pio_put_att (file(fil_idx)%File, PIO_GLOBAL, 'Version', &
      !         '$Name$')
      !    ierr= pio_put_att (file(fil_idx)%File, PIO_GLOBAL, 'revision_Id', &
      !         '$Id$')

      ierr=pio_put_att (file(fil_idx)%File, PIO_GLOBAL, 'initial_file', ncdata)
      ierr=pio_put_att (file(fil_idx)%File, PIO_GLOBAL, 'topography_file', bnd_topo)
      if (len_trim(model_doi_url) > 0) then
         ierr=pio_put_att (file(fil_idx)%File, PIO_GLOBAL, 'model_doi_url', model_doi_url)
      end if

      ! Determine what time period frequency is being output for each file
      ! Note that nhtfrq is now in timesteps

      sec_nhtfrq = nhtfrq(fil_idx)

      ! If nhtfrq is in hours, convert to seconds
      if (nhtfrq(fil_idx) < 0) then
         sec_nhtfrq = abs(nhtfrq(fil_idx))*3600
      end if

      dtime = get_step_size()
      if (sec_nhtfrq == 0) then                                !month
         time_per_freq = 'month_1'
      else if (mod(sec_nhtfrq*dtime,86400) == 0) then          ! day
         write(time_per_freq,999) 'day_',sec_nhtfrq*dtime/86400
      else if (mod(sec_nhtfrq*dtime,3600) == 0) then           ! hour
         write(time_per_freq,999) 'hour_',(sec_nhtfrq*dtime)/3600
      else if (mod(sec_nhtfrq*dtime,60) == 0) then             ! minute
         write(time_per_freq,999) 'minute_',(sec_nhtfrq*dtime)/60
      else                                                     ! second
         write(time_per_freq,999) 'second_',sec_nhtfrq*dtime
      end if
999   format(a,i0)

      ierr=pio_put_att (file(fil_idx)%File, PIO_GLOBAL, 'time_period_freq', trim(time_per_freq))

      if(.not. is_satfile(fil_idx)) then

         ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%timeid, 'bounds', 'time_bnds')

         ierr=pio_def_var (file(fil_idx)%File,'time_bnds',pio_double,(/bnddim,timdim/),file(fil_idx)%tbndid)
         ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%tbndid, 'long_name', 'time interval endpoints')
         !
         ! Character
         !
         dimenchar(1) = chardim
         dimenchar(2) = timdim
         ierr=pio_def_var (file(fil_idx)%File,'date_written',PIO_CHAR,dimenchar, file(fil_idx)%date_writtenid)
         ierr=pio_def_var (file(fil_idx)%File,'time_written',PIO_CHAR,dimenchar, file(fil_idx)%time_writtenid)
         !
         ! Integer Header
         !

         ierr=pio_def_var (file(fil_idx)%File,'ndbase',PIO_INT,file(fil_idx)%ndbaseid)
         str = 'base day'
         ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%ndbaseid, 'long_name', trim(str))

         ierr=pio_def_var (file(fil_idx)%File,'nsbase',PIO_INT,file(fil_idx)%nsbaseid)
         str = 'seconds of base day'
         ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%nsbaseid, 'long_name', trim(str))

         ierr=pio_def_var (file(fil_idx)%File,'nbdate',PIO_INT,file(fil_idx)%nbdateid)
         str = 'base date (YYYYMMDD)'
         ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%nbdateid, 'long_name', trim(str))

#if ( defined BFB_CAM_SCAM_IOP )
         ierr=pio_def_var (file(fil_idx)%File,'bdate',PIO_INT,file(fil_idx)%bdateid)
         str = 'base date (YYYYMMDD)'
         ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%bdateid, 'long_name', trim(str))
#endif
         ierr=pio_def_var (file(fil_idx)%File,'nbsec',PIO_INT,file(fil_idx)%nbsecid)
         str = 'seconds of base date'
         ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%nbsecid, 'long_name', trim(str))

         ierr=pio_def_var (file(fil_idx)%File,'mdt',PIO_INT,file(fil_idx)%mdtid)
         ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%mdtid, 'long_name', 'timestep')
         ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%mdtid, 'units', 's')

         !
         ! Create variables for model timing and header information
         !

         ierr=pio_def_var (file(fil_idx)%File,'ndcur   ',pio_int,(/timdim/),file(fil_idx)%ndcurid)
         str = 'current day (from base day)'
         ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%ndcurid, 'long_name', trim(str))

         ierr=pio_def_var (file(fil_idx)%File,'nscur   ',pio_int,(/timdim/),file(fil_idx)%nscurid)
         str = 'current seconds of current day'
         ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%nscurid, 'long_name', trim(str))


         if (.not. is_initfile(file_index=t)) then
            ! Don't write the GHG/Solar forcing data to the IC file.
            ierr=pio_def_var (file(fil_idx)%File,'co2vmr  ',pio_double,(/timdim/),file(fil_idx)%co2vmrid)
            str = 'co2 volume mixing ratio'
            ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%co2vmrid, 'long_name', trim(str))

            ierr=pio_def_var (file(fil_idx)%File,'ch4vmr  ',pio_double,(/timdim/),file(fil_idx)%ch4vmrid)
            str = 'ch4 volume mixing ratio'
            ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%ch4vmrid, 'long_name', trim(str))

            ierr=pio_def_var (file(fil_idx)%File,'n2ovmr  ',pio_double,(/timdim/),file(fil_idx)%n2ovmrid)
            str = 'n2o volume mixing ratio'
            ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%n2ovmrid, 'long_name', trim(str))

            ierr=pio_def_var (file(fil_idx)%File,'f11vmr  ',pio_double,(/timdim/),file(fil_idx)%f11vmrid)
            str = 'f11 volume mixing ratio'
            ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%f11vmrid, 'long_name', trim(str))

            ierr=pio_def_var (file(fil_idx)%File,'f12vmr  ',pio_double,(/timdim/),file(fil_idx)%f12vmrid)
            str = 'f12 volume mixing ratio'
            ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%f12vmrid, 'long_name', trim(str))

            ierr=pio_def_var (file(fil_idx)%File,'sol_tsi ',pio_double,(/timdim/),file(fil_idx)%sol_tsiid)
            str = 'total solar irradiance'
            ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%sol_tsiid, 'long_name', trim(str))
            str = 'W/m2'
            ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%sol_tsiid, 'units', trim(str))

            if (solar_parms_on) then
               ! solar / geomagetic activity indices...
               ierr=pio_def_var (file(fil_idx)%File,'f107',pio_double,(/timdim/),file(fil_idx)%f107id)
               str = '10.7 cm solar radio flux (F10.7)'
               ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%f107id, 'long_name', trim(str))
               str = '10^-22 W m^-2 Hz^-1'
               ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%f107id, 'units', trim(str))

               ierr=pio_def_var (file(fil_idx)%File,'f107a',pio_double,(/timdim/),file(fil_idx)%f107aid)
               str = '81-day centered mean of 10.7 cm solar radio flux (F10.7)'
               ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%f107aid, 'long_name', trim(str))

               ierr=pio_def_var (file(fil_idx)%File,'f107p',pio_double,(/timdim/),file(fil_idx)%f107pid)
               str = 'Pervious day 10.7 cm solar radio flux (F10.7)'
               ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%f107pid, 'long_name', trim(str))

               ierr=pio_def_var (file(fil_idx)%File,'kp',pio_double,(/timdim/),file(fil_idx)%kpid)
               str = 'Daily planetary K geomagnetic index'
               ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%kpid, 'long_name', trim(str))

               ierr=pio_def_var (file(fil_idx)%File,'ap',pio_double,(/timdim/),file(fil_idx)%apid)
               str = 'Daily planetary A geomagnetic index'
               ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%apid, 'long_name', trim(str))
            endif
            if (solar_wind_on) then

               ierr=pio_def_var (file(fil_idx)%File,'byimf',pio_double,(/timdim/),file(fil_idx)%byimfid)
               str = 'Y component of the interplanetary magnetic field'
               ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%byimfid, 'long_name', trim(str))
               str = 'nT'
               ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%byimfid, 'units', trim(str))

               ierr=pio_def_var (file(fil_idx)%File,'bzimf',pio_double,(/timdim/),file(fil_idx)%bzimfid)
               str = 'Z component of the interplanetary magnetic field'
               ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%bzimfid, 'long_name', trim(str))
               str = 'nT'
               ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%bzimfid, 'units', trim(str))

               ierr=pio_def_var (file(fil_idx)%File,'swvel',pio_double,(/timdim/),file(fil_idx)%swvelid)
               str = 'Solar wind speed'
               ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%swvelid, 'long_name', trim(str))
               str = 'km/sec'
               ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%swvelid, 'units', trim(str))

               ierr=pio_def_var (file(fil_idx)%File,'swden',pio_double,(/timdim/),file(fil_idx)%swdenid)
               str = 'Solar wind ion number density'
               ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%swdenid, 'long_name', trim(str))
               str = 'cm-3'
               ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%swdenid, 'units', trim(str))

            endif
            if (epot_active) then
               ierr=pio_def_var (file(fil_idx)%File,'colat_crit1',pio_double,(/timdim/),file(fil_idx)%colat_crit1_id)
               ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%colat_crit1_id, 'long_name', &
                    'First co-latitude of electro-potential critical angle')
               ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%colat_crit1_id, 'units', 'degrees')

               ierr=pio_def_var (file(fil_idx)%File,'colat_crit2',pio_double,(/timdim/),file(fil_idx)%colat_crit2_id)
               ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%colat_crit2_id, 'long_name',&
                    'Second co-latitude of electro-potential critical angle')
               ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%colat_crit2_id, 'units', 'degrees')
            endif
         end if


#if ( defined BFB_CAM_SCAM_IOP )
         ierr=pio_def_var (file(fil_idx)%File,'tsec ',pio_int,(/timdim/), file(fil_idx)%tsecid)
         str = 'current seconds of current date needed for scam'
         ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%tsecid, 'long_name', trim(str))
#endif
         ierr=pio_def_var (file(fil_idx)%File,'nsteph  ',pio_int,(/timdim/),file(fil_idx)%nstephid)
         str = 'current timestep'
         ierr=pio_put_att (file(fil_idx)%File, file(fil_idx)%nstephid, 'long_name', trim(str))
      end if ! .not. is_satfile

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !
      ! Create variables and attributes for field list
      !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      do f = 1, nflds(fil_idx)

         !! Collect some field properties
         call AvgflagToString(file(fil_idx)%hlist(fld_idx)%avgflag, file(fil_idx)%hlist(fld_idx)%time_op)

         if ((file(fil_idx)%hlist(fld_idx)%hwrt_prec == 8) .or. restart) then
            ncreal = pio_double
         else
            ncreal = pio_real
         end if

         if(associated(file(fil_idx)%hlist(fld_idx)%field%mdims)) then
            mdims => file(fil_idx)%hlist(fld_idx)%field%mdims
            mdimsize = size(mdims)
         else if(file(fil_idx)%hlist(fld_idx)%field%numlev > 1) then
            call endrun('mdims not defined for variable '//trim(file(fil_idx)%hlist(fld_idx)%field%name))
         else
            mdimsize=0
         end if

         ! num_patches will loop through the number of patches (or just one
         !             for the whole grid) for this field for this file
         if (patch_output) then
            num_patches = size(file(fil_idx)%patches)
         else
            num_patches = 1
         end if
         if(.not.associated(file(fil_idx)%hlist(fld_idx)%varid)) then
            allocate(file(fil_idx)%hlist(fld_idx)%varid(num_patches))
         end if
         fname_tmp = strip_suffix(file(fil_idx)%hlist(fld_idx)%field%name)

         if(is_satfile(fil_idx)) then
            num_hdims=0
            nfils(fil_idx)=1
            call sat_hist_define(file(fil_idx)%File)
         else if (interpolate) then
            ! Interpolate can't use normal grid code since we are forcing fields
            ! to use interpolate decomp
            if (.not. allocated(header_info)) then
               ! Safety check
               call endrun('h_define: header_info not allocated')
            end if
            num_hdims = 2
            do i = 1, num_hdims
               dimindex(i) = header_info(1)%get_hdimid(i)
               nacsdims(i) = header_info(1)%get_hdimid(i)
            end do
         else if (patch_output) then
            ! All patches for this variable should be on the same grid
            num_hdims = file(fil_idx)%patches(1)%num_hdims(file(fil_idx)%hlist(fld_idx)%field%decomp_type)
         else
            ! Normal grid output
            ! Find appropriate grid in header_info
            if (.not. allocated(header_info)) then
               ! Safety check
               call endrun('h_define: header_info not allocated')
            end if
            grd = -1
            do i = 1, size(header_info)
               if (header_info(i)%get_gridid() == file(fil_idx)%hlist(fld_idx)%field%decomp_type) then
                  grd = i
                  exit
               end if
            end do
            if (grd < 0) then
               write(errormsg, '(a,i0,2a)') 'grid, ',file(fil_idx)%hlist(fld_idx)%field%decomp_type,', not found for ',trim(fname_tmp)
               call endrun('H_DEFINE: '//errormsg)
            end if
            num_hdims = header_info(grd)%num_hdims()
            do i = 1, num_hdims
               dimindex(i) = header_info(grd)%get_hdimid(i)
               nacsdims(i) = header_info(grd)%get_hdimid(i)
            end do
         end if     ! is_satfile

         !
         !  Create variables and atributes for fields written out as columns
         !

         do i = 1, num_patches
            fname_tmp = strip_suffix(file(fil_idx)%hlist(fld_idx)%field%name)
            varid => file(fil_idx)%hlist(fld_idx)%varid(i)
            dimids_tmp = dimindex
            ! Figure the dimension ID array for this field
            ! We have defined the horizontal grid dimensions in dimindex
            fdims = num_hdims
            do j = 1, mdimsize
               fdims = fdims + 1
               dimids_tmp(fdims) = mdimids(mdims(j))
            end do
            if(.not. restart) then
               ! Only add time dimension if this is not a restart history file
               fdims = fdims + 1
               dimids_tmp(fdims) = timdim
            end if
            if (patch_output) then
               ! For patch output, we need new dimension IDs and a different name
               call file(fil_idx)%patches(i)%get_var_data(fname_tmp,                     &
                    dimids_tmp(1:fdims), file(fil_idx)%hlist(fld_idx)%field%decomp_type)
            end if
            ! Define the variable
            call cam_pio_def_var(file(fil_idx)%File, trim(fname_tmp), ncreal,           &
                 dimids_tmp(1:fdims), varid)
            if (mdimsize > 0) then
               ierr = pio_put_att(file(fil_idx)%File, varid, 'mdims', mdims(1:mdimsize))
               call cam_pio_handle_error(ierr, 'h_define: cannot define mdims for '//trim(fname_tmp))
            end if
            str = file(fil_idx)%hlist(fld_idx)%field%sampling_seq
            if (len_trim(str) > 0) then
               ierr = pio_put_att(file(fil_idx)%File, varid, 'Sampling_Sequence', trim(str))
               call cam_pio_handle_error(ierr, 'h_define: cannot define Sampling_Sequence for '//trim(fname_tmp))
            end if

            if (file(fil_idx)%hlist(fld_idx)%field%flag_xyfill) then
               ! Add both _FillValue and missing_value to cover expectations
               !     of various applications.
               ! The attribute type must match the data type.
               if ((file(fil_idx)%hlist(fld_idx)%hwrt_prec == 8) .or. restart) then
                  ierr = pio_put_att(file(fil_idx)%File, varid, '_FillValue',             &
                       file(fil_idx)%hlist(fld_idx)%field%fillvalue)
                  call cam_pio_handle_error(ierr,                                   &
                       'h_define: cannot define _FillValue for '//trim(fname_tmp))
                  ierr = pio_put_att(file(fil_idx)%File, varid, 'missing_value',          &
                       file(fil_idx)%hlist(fld_idx)%field%fillvalue)
                  call cam_pio_handle_error(ierr,                                   &
                       'h_define: cannot define missing_value for '//trim(fname_tmp))
               else
                  ierr = pio_put_att(file(fil_idx)%File, varid, '_FillValue',             &
                       REAL(file(fil_idx)%hlist(fld_idx)%field%fillvalue,r4))
                  call cam_pio_handle_error(ierr,                                   &
                       'h_define: cannot define _FillValue for '//trim(fname_tmp))
                  ierr = pio_put_att(file(fil_idx)%File, varid, 'missing_value',          &
                       REAL(file(fil_idx)%hlist(fld_idx)%field%fillvalue,r4))
                  call cam_pio_handle_error(ierr,                                   &
                       'h_define: cannot define missing_value for '//trim(fname_tmp))
               end if
            end if

            str = file(fil_idx)%hlist(fld_idx)%field%units
            if (len_trim(str) > 0) then
               ierr=pio_put_att (file(fil_idx)%File, varid, 'units', trim(str))
               call cam_pio_handle_error(ierr,                                     &
                    'h_define: cannot define units for '//trim(fname_tmp))
            end if

            str = file(fil_idx)%hlist(fld_idx)%field%mixing_ratio
            if (len_trim(str) > 0) then
               ierr=pio_put_att (file(fil_idx)%File, varid, 'mixing_ratio', trim(str))
               call cam_pio_handle_error(ierr,                                     &
                    'h_define: cannot define mixing_ratio for '//trim(fname_tmp))
            end if

            str = file(fil_idx)%hlist(fld_idx)%field%long_name
            ierr=pio_put_att (file(fil_idx)%File, varid, 'long_name', trim(str))
            call cam_pio_handle_error(ierr,                                       &
                 'h_define: cannot define long_name for '//trim(fname_tmp))

            ! Assign field attributes defining valid levels and averaging info

            cell_methods = ''
            if (len_trim(file(fil_idx)%hlist(fld_idx)%field%cell_methods) > 0) then
               if (len_trim(cell_methods) > 0) then
                  cell_methods = trim(cell_methods)//' '//trim(file(fil_idx)%hlist(fld_idx)%field%cell_methods)
               else
                  cell_methods = trim(cell_methods)//trim(file(fil_idx)%hlist(fld_idx)%field%cell_methods)
               end if
            end if
            ! Time cell methods is after field method because time averaging is
            ! applied later (just before output) than field method which is applied
            ! before outfld call.
            str = file(fil_idx)%hlist(fld_idx)%time_op
            select case (str)
            case ('mean', 'maximum', 'minimum', 'standard_deviation')
               if (len_trim(cell_methods) > 0) then
                  cell_methods = trim(cell_methods)//' '//'time: '//str
               else
                  cell_methods = trim(cell_methods)//'time: '//str
               end if
            end select
            if (len_trim(cell_methods) > 0) then
               ierr = pio_put_att(file(fil_idx)%File, varid, 'cell_methods', trim(cell_methods))
               call cam_pio_handle_error(ierr,                                     &
                    'h_define: cannot define cell_methods for '//trim(fname_tmp))
            end if
            if (patch_output) then
               ierr = pio_put_att(file(fil_idx)%File, varid, 'basename',                 &
                    file(fil_idx)%hlist(fld_idx)%field%name)
               call cam_pio_handle_error(ierr,                                     &
                    'h_define: cannot define basename for '//trim(fname_tmp))
            end if

            if (restart) then
               ! For restart history files, we need to save accumulation counts
               fname_tmp = trim(fname_tmp)//'_nacs'
               if (.not. associated(file(fil_idx)%hlist(fld_idx)%nacs_varid)) then
                  allocate(file(fil_idx)%hlist(fld_idx)%nacs_varid)
               end if
               if (size(file(fil_idx)%hlist(fld_idx)%nacs, 1) > 1) then
                  call cam_pio_def_var(file(fil_idx)%File, trim(fname_tmp), pio_int,      &
                       nacsdims(1:num_hdims), file(fil_idx)%hlist(fld_idx)%nacs_varid)
               else
                  ! Save just one value representing all chunks
                  call cam_pio_def_var(file(fil_idx)%File, trim(fname_tmp), pio_int,      &
                       file(fil_idx)%hlist(fld_idx)%nacs_varid)
               end if
               ! for standard deviation
               if (associated(file(fil_idx)%hlist(fld_idx)%sbuf)) then
                  fname_tmp = strip_suffix(file(fil_idx)%hlist(fld_idx)%field%name)
                  fname_tmp = trim(fname_tmp)//'_var'
                  if ( .not.associated(file(fil_idx)%hlist(fld_idx)%sbuf_varid)) then
                     allocate(file(fil_idx)%hlist(fld_idx)%sbuf_varid)
                  endif
                  call cam_pio_def_var(file(fil_idx)%File, trim(fname_tmp), pio_double,      &
                       dimids_tmp(1:fdims), file(fil_idx)%hlist(fld_idx)%sbuf_varid)
               endif
            end if
         end do ! Loop over output patches
      end do   ! Loop over fields
      !
      deallocate(mdimids)
      ret = pio_enddef(file(fil_idx)%File)

      if(masterproc) then
         write(iulog,*)'H_DEFINE: Successfully opened netcdf file '
      endif
      !
      ! Write time-invariant portion of history header
      !
      if(.not. is_satfile(fil_idx)) then
         if(interpolate) then
            call cam_grid_write_var(file(fil_idx)%File, interpolate_info(fil_idx)%grid_id)
         else if((.not. patch_output) .or. restart) then
            do i = 1, size(file(fil_idx)%grid_ids)
               call cam_grid_write_var(file(fil_idx)%File, file(fil_idx)%grid_ids(i))
            end do
         else
            ! Patch output
            do i = 1, size(file(fil_idx)%patches)
               call file(fil_idx)%patches(i)%write_vals(file(fil_idx)%File)
            end do
         end if ! interpolate
         if (allocated(lonvar)) then
            deallocate(lonvar)
         end if
         if (allocated(latvar)) then
            deallocate(latvar)
         end if

         dtime = get_step_size()
         ierr = pio_put_var(file(fil_idx)%File, file(fil_idx)%mdtid, (/dtime/))
         call cam_pio_handle_error(ierr, 'h_define: cannot put mdt')
         !
         ! Model date info
         !
         ierr = pio_put_var(file(fil_idx)%File, file(fil_idx)%ndbaseid, (/ndbase/))
         call cam_pio_handle_error(ierr, 'h_define: cannot put ndbase')
         ierr = pio_put_var(file(fil_idx)%File, file(fil_idx)%nsbaseid, (/nsbase/))
         call cam_pio_handle_error(ierr, 'h_define: cannot put nsbase')

         ierr = pio_put_var(file(fil_idx)%File, file(fil_idx)%nbdateid, (/nbdate/))
         call cam_pio_handle_error(ierr, 'h_define: cannot put nbdate')
#if ( defined BFB_CAM_SCAM_IOP )
         ierr = pio_put_var(file(fil_idx)%File, file(fil_idx)%bdateid, (/nbdate/))
         call cam_pio_handle_error(ierr, 'h_define: cannot put bdate')
#endif
         ierr = pio_put_var(file(fil_idx)%File, file(fil_idx)%nbsecid, (/nbsec/))
         call cam_pio_handle_error(ierr, 'h_define: cannot put nbsec')
         !
         ! Reduced grid info
         !

      end if ! .not. is_satfile

      if (allocated(header_info)) then
         do i = 1, size(header_info)
            call header_info(i)%deallocate()
         end do
         deallocate(header_info)
      end if

      ! Write the mdim variable data
      call write_hist_coord_vars(file(fil_idx)%File, restart)

   end subroutine h_define

   !#######################################################################

   subroutine h_normalize (f, t)

      use cam_history_support, only: dim_index_2d

      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: Normalize fields on a history file by the number of accumulations
      !
      ! Method: Loop over fields on the file.  Need averaging flag and number of
      !         accumulations to perform normalization.
      !
      !-----------------------------------------------------------------------
      !
      ! Input arguments
      !
      integer, intent(in) :: f       ! field index
      integer, intent(in) :: t       ! file index
      !
      ! Local workspace
      !
      type (dim_index_2d) :: dimind  ! 2-D dimension index
      integer     :: c               ! chunk (or lat) index
      integer     :: ib, ie    ! beginning and ending indices of first dimension
      integer     :: jb, je    ! beginning and ending indices of second dimension
      integer     :: begdim3, enddim3 ! Chunk or block bounds
      integer     :: k         ! level
      integer     :: i, ii
      real(r8) :: variance, tmpfill

      logical     :: flag_xyfill ! non-applicable xy points flagged with fillvalue
      character*1 :: avgflag     ! averaging flag

      call t_startf ('h_normalize')

      call file(fil_idx)%hlist(fld_idx)%field%get_bounds(3, begdim3, enddim3)

      !
      ! normalize by number of accumulations for averaged case
      !
      flag_xyfill = file(fil_idx)%hlist(fld_idx)%field%flag_xyfill
      avgflag = file(fil_idx)%hlist(fld_idx)%avgflag

      do c = begdim3, enddim3
         dimind = file(fil_idx)%hlist(fld_idx)%field%get_dims(c)

         ib = dimind%beg1
         ie = dimind%end1
         jb = dimind%beg2
         je = dimind%end2

         if (flag_xyfill) then
            do k = jb, je
               where (file(fil_idx)%hlist(fld_idx)%nacs(ib:ie, c) == 0)
                  file(fil_idx)%hlist(fld_idx)%hbuf(ib:ie,k,c) = file(fil_idx)%hlist(fld_idx)%field%fillvalue
               endwhere
            end do
         end if

         if (avgflag == 'A' .or. avgflag == 'B' .or. avgflag == 'L') then
            if (size(file(fil_idx)%hlist(fld_idx)%nacs, 1) > 1) then
               do k = jb, je
                  where (file(fil_idx)%hlist(fld_idx)%nacs(ib:ie,c) /= 0)
                     file(fil_idx)%hlist(fld_idx)%hbuf(ib:ie,k,c) = &
                          file(fil_idx)%hlist(fld_idx)%hbuf(ib:ie,k,c) &
                          / file(fil_idx)%hlist(fld_idx)%nacs(ib:ie,c)
                  endwhere
               end do
            else if(file(fil_idx)%hlist(fld_idx)%nacs(1,c) > 0) then
               do k=jb,je
                  file(fil_idx)%hlist(fld_idx)%hbuf(ib:ie,k,c) = &
                       file(fil_idx)%hlist(fld_idx)%hbuf(ib:ie,k,c) &
                       / file(fil_idx)%hlist(fld_idx)%nacs(1,c)
               end do
            end if
         end if
         if (avgflag == 'S') then
            ! standard deviation ...
            ! from http://www.johndcook.com/blog/standard_deviation/
            tmpfill = merge(file(fil_idx)%hlist(fld_idx)%field%fillvalue,0._r8,flag_xyfill)
            do k=jb,je
               do i = ib,ie
                  ii = merge(i,1,flag_xyfill)
                  if (file(fil_idx)%hlist(fld_idx)%nacs(ii,c) > 1) then
                     variance = file(fil_idx)%hlist(fld_idx)%sbuf(i,k,c)/(file(fil_idx)%hlist(fld_idx)%nacs(ii,c)-1)
                     file(fil_idx)%hlist(fld_idx)%hbuf(i,k,c) = sqrt(variance)
                  else
                     file(fil_idx)%hlist(fld_idx)%hbuf(i,k,c) = tmpfill
                  endif
               end do
            end do
         endif
      end do

      call t_stopf ('h_normalize')

      return
   end subroutine h_normalize

   !#######################################################################

   subroutine h_zero (f, t)
      use cam_history_support, only: dim_index_2d
      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: Zero out accumulation buffers for a file
      !
      ! Method: Loop through fields on the file
      !
      !-----------------------------------------------------------------------
      !
      integer, intent(in) :: f     ! field index
      integer, intent(in) :: t     ! file index
      !
      ! Local workspace
      !
      type (dim_index_2d) :: dimind   ! 2-D dimension index
      integer :: c                    ! chunk index
      integer :: begdim3              ! on-node chunk or lat start index
      integer :: enddim3              ! on-node chunk or lat end index

      call t_startf ('h_zero')

      call file(fil_idx)%hlist(fld_idx)%field%get_bounds(3, begdim3, enddim3)

      do c = begdim3, enddim3
         dimind = file(fil_idx)%hlist(fld_idx)%field%get_dims(c)
         file(fil_idx)%hlist(fld_idx)%hbuf(dimind%beg1:dimind%end1,dimind%beg2:dimind%end2,c)=0._r8
         if (associated(file(fil_idx)%hlist(fld_idx)%sbuf)) then ! zero out variance buffer for standard deviation
            file(fil_idx)%hlist(fld_idx)%sbuf(dimind%beg1:dimind%end1,dimind%beg2:dimind%end2,c)=0._r8
         endif
      end do
      file(fil_idx)%hlist(fld_idx)%nacs(:,:) = 0

      call t_stopf ('h_zero')

      return
   end subroutine h_zero

   !#######################################################################

   subroutine dump_field (f, t, restart)
      use cam_history_support, only: history_patch_t, dim_index_3d
      use cam_grid_support,    only: cam_grid_write_dist_array, cam_grid_dimensions
      use interp_mod,       only : write_interpolated

      ! Dummy arguments
      integer,     intent(in)    :: f
      integer,     intent(in)    :: t
      logical,     intent(in)    :: restart
      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: Write a variable to a history file using PIO
      !          For restart files, also write the accumulation buffer (nacs)
      !
      !-----------------------------------------------------------------------
      ! Local variables
      integer                          :: ierr
      type(var_desc_t),      pointer   :: varid      ! PIO ID for var
      type(var_desc_t),      pointer   :: compid     ! PIO ID for vector comp.
      integer                          :: compind    ! index of vector comp.
      integer                          :: fdims(8)   ! Field file dim sizes
      integer                          :: frank      ! Field file rank
      integer                          :: nacsrank   ! Field file rank for nacs
      type(dim_index_3d)               :: dimind     ! 3-D dimension index
      integer                          :: adims(3)   ! Field array dim sizes
      integer                          :: nadims     ! # of used adims
      integer                          :: fdecomp
      integer                          :: num_patches
      integer                          :: mdimsize   ! Total # on-node elements
      logical                          :: interpolate
      logical                          :: patch_output
      type(history_patch_t), pointer   :: patchptr
      integer :: i

      interpolate = (interpolate_output(fil_idx) .and. (.not. restart))
      patch_output = (associated(file(fil_idx)%patches) .and. (.not. restart))

!!! Get the field's shape and decomposition

      ! Shape on disk
      call file(fil_idx)%hlist(fld_idx)%field%get_shape(fdims, frank)

      ! Shape of array
      dimind = file(fil_idx)%hlist(fld_idx)%field%get_dims()
      call dimind%dim_sizes(adims)
      if (adims(2) <= 1) then
         adims(2) = adims(3)
         nadims = 2
      else
         nadims = 3
      end if
      fdecomp = file(fil_idx)%hlist(fld_idx)%field%decomp_type

      ! num_patches will loop through the number of patches (or just one
      !             for the whole grid) for this field for this file
      if (patch_output) then
         num_patches = size(file(fil_idx)%patches)
      else
         num_patches = 1
      end if

      do i = 1, num_patches
         varid => file(fil_idx)%hlist(fld_idx)%varid(i)

         if (restart) then
            call pio_setframe(file(fil_idx)%File, varid, int(-1,kind=PIO_OFFSET_KIND))
         else
            call pio_setframe(file(fil_idx)%File, varid, int(max(1,nfils(fil_idx)),kind=PIO_OFFSET_KIND))
         end if
         if (patch_output) then
            ! We are outputting patches
            patchptr => file(fil_idx)%patches(i)
            if (interpolate) then
               call endrun('dump_field: interpolate incompatible with regional output')
            end if
            call patchptr%write_var(file(fil_idx)%File, fdecomp, adims(1:nadims),       &
                 pio_double, file(fil_idx)%hlist(fld_idx)%hbuf, varid)
         else
            ! We are doing output via the field's grid
            if (interpolate) then
               mdimsize = file(fil_idx)%hlist(fld_idx)%field%enddim2 - file(fil_idx)%hlist(fld_idx)%field%begdim2 + 1
               if (mdimsize == 0) then
                  mdimsize = file(fil_idx)%hlist(fld_idx)%field%numlev
               end if
               if (file(fil_idx)%hlist(fld_idx)%field%meridional_complement > 0) then
                  compind = file(fil_idx)%hlist(fld_idx)%field%meridional_complement
                  compid => file(fil_idx)%hlist(compind)%varid(i)
                  ! We didn't call set frame on the meridional complement field
                  call pio_setframe(file(fil_idx)%File, compid, int(max(1,nfils(fil_idx)),kind=PIO_OFFSET_KIND))
                  call write_interpolated(file(fil_idx)%File, varid, compid,              &
                       file(fil_idx)%hlist(fld_idx)%hbuf, file(fil_idx)%hlist(compind)%hbuf,          &
                       mdimsize, PIO_DOUBLE, fdecomp)
               else if (file(fil_idx)%hlist(fld_idx)%field%zonal_complement > 0) then
                  ! We don't want to double write so do nothing here
                  !            compind = file(fil_idx)%hlist(fld_idx)%field%zonal_complement
                  !            compid => file(fil_idx)%hlist(compind)%varid(i)
                  !            call write_interpolated(file(fil_idx)%File, compid, varid,              &
                  !                 file(fil_idx)%hlist(compind)%hbuf, file(fil_idx)%hlist(fld_idx)%hbuf,          &
                  !                 mdimsize, PIO_DOUBLE, fdecomp)
               else
                  ! Scalar field
                  call write_interpolated(file(fil_idx)%File, varid,                      &
                       file(fil_idx)%hlist(fld_idx)%hbuf, mdimsize, PIO_DOUBLE, fdecomp)
               end if
            else if (nadims == 2) then
               ! Special case for 2D field (no levels) due to hbuf structure
               call cam_grid_write_dist_array(file(fil_idx)%File, fdecomp,               &
                    adims(1:nadims), fdims(1:frank), file(fil_idx)%hlist(fld_idx)%hbuf(:,1,:), varid)
            else
               call cam_grid_write_dist_array(file(fil_idx)%File, fdecomp, adims,        &
                    fdims(1:frank), file(fil_idx)%hlist(fld_idx)%hbuf, varid)
            end if
         end if
      end do
      !! write accumulation counter and variance to hist restart file
      if(restart) then
         if (associated(file(fil_idx)%hlist(fld_idx)%sbuf) ) then
            ! write variance data to restart file for standard deviation calc
            if (nadims == 2) then
               ! Special case for 2D field (no levels) due to sbuf structure
               call cam_grid_write_dist_array(file(fil_idx)%File, fdecomp, adims(1:nadims), &
                    fdims(1:frank), file(fil_idx)%hlist(fld_idx)%sbuf(:,1,:), file(fil_idx)%hlist(fld_idx)%sbuf_varid)
            else
               call cam_grid_write_dist_array(file(fil_idx)%File, fdecomp, adims,        &
                    fdims(1:frank), file(fil_idx)%hlist(fld_idx)%sbuf, file(fil_idx)%hlist(fld_idx)%sbuf_varid)
            endif
         endif
         !! NACS
         if (size(file(fil_idx)%hlist(fld_idx)%nacs, 1) > 1) then
            if (nadims > 2) then
               adims(2) = adims(3)
               nadims = 2
            end if
            call cam_grid_dimensions(fdecomp, fdims(1:2), nacsrank)
            call cam_grid_write_dist_array(file(fil_idx)%File, fdecomp, adims(1:nadims), &
                 fdims(1:nacsrank), file(fil_idx)%hlist(fld_idx)%nacs, file(fil_idx)%hlist(fld_idx)%nacs_varid)
         else
            ierr = pio_put_var(file(fil_idx)%File, file(fil_idx)%hlist(fld_idx)%nacs_varid,     &
                 file(fil_idx)%hlist(fld_idx)%nacs(:, file(fil_idx)%hlist(fld_idx)%field%begdim3:file(fil_idx)%hlist(fld_idx)%field%enddim3))
         end if
      end if

      return
   end subroutine dump_field

   !#######################################################################

   logical function write_inithist ()
      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: Set flags that will initiate dump to IC file when OUTFLD and
      ! WSHIST are called
      !
      !-----------------------------------------------------------------------
      !
      use time_manager, only: get_nstep, get_curr_date, get_step_size, is_last_step
      !
      ! Local workspace
      !
      integer :: yr, mon, day      ! year, month, and day components of
      ! a date
      integer :: nstep             ! current timestep number
      integer :: ncsec             ! current time of day [seconds]
      integer :: dtime             ! timestep size

      !-----------------------------------------------------------------------

      write_inithist  = .false.

      if(is_initfile()) then

         nstep = get_nstep()
         call get_curr_date(yr, mon, day, ncsec)

         if    (inithist == '6-HOURLY') then
            dtime  = get_step_size()
            write_inithist = nstep /= 0 .and. mod( nstep, nint((6._r8*3600._r8)/dtime) ) == 0
         elseif(inithist == 'DAILY'   ) then
            write_inithist = nstep /= 0 .and. ncsec == 0
         elseif(inithist == 'MONTHLY' ) then
            write_inithist = nstep /= 0 .and. ncsec == 0 .and. day == 1
         elseif(inithist == 'YEARLY'  ) then
            write_inithist = nstep /= 0 .and. ncsec == 0 .and. day == 1 .and. mon == 1
         elseif(inithist == 'CAMIOP'  ) then
            write_inithist = nstep == 0
         elseif(inithist == 'ENDOFRUN'  ) then
            write_inithist = nstep /= 0 .and. is_last_step()
         end if
      end if

      return
   end function write_inithist

   !#######################################################################

   subroutine wshist(regen_hist_file_in)
      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: Driver routine to write fields on history file t
      !
      !
      !-----------------------------------------------------------------------
      use time_manager,  only: get_nstep, get_curr_date, get_curr_time
      use time_manager,  only: get_step_size
      use chem_surfvals, only: chem_surfvals_get, chem_surfvals_co2_rad
      use solar_irrad_data, only: sol_tsi
      use sat_hist,      only: sat_hist_write
      use interp_mod,    only: set_interp_hfile
      use datetime_mod,  only: datetime
      use cam_pio_utils, only: cam_pio_closefile

      logical, intent(in), optional :: regen_hist_file_in(pfiles)
      !
      ! Local workspace
      !
      character(len=8) :: cdate  ! system date
      character(len=8) :: ctime  ! system time

      logical :: regen_hist_file(pfiles), restart
      integer :: fil_idx ! file index
      integer :: fld_idx ! field idx
      integer :: start      ! starting index required by nf_put_vara
      integer :: count1     ! count values required by nf_put_vara
      integer :: startc(2)  ! start values required by nf_put_vara (character)
      integer :: countc(2)  ! count values required by nf_put_vara (character)

      integer :: yr, mon, day      ! year, month, and day components of a date
      integer :: nstep             ! current timestep number
      integer :: ncdate            ! current date in integer format [yyyymmdd]
      integer :: ncsec             ! current time of day [seconds]
      integer :: ndcur             ! day component of current time
      integer :: nscur             ! seconds component of current time
      real(r8) :: time             ! current time
      real(r8) :: tdata(2)         ! time interval boundaries
      character(len=max_string_len) :: fname ! Filename
      logical :: prev              ! Label file with previous date rather than current
      integer :: ierr
#if ( defined BFB_CAM_SCAM_IOP )
      integer :: tsec             ! day component of current time
      integer :: dtime            ! seconds component of current time
#endif

      if(present(regen_hist_file_in)) then
         regen_hist_file = regen_hist_file_in
         restart=.true.
         file => restarthistory_files
      else
         regen_hist_file = .false.
         restart=.false.
         file => history_file
      end if

      nstep = get_nstep()
      call get_curr_date(yr, mon, day, ncsec)
      ncdate = yr*10000 + mon*100 + day
      call get_curr_time(ndcur, nscur)
      !
      ! Write time-varying portion of history file header
      !
      do fil_idx = 1, pfiles
         if ( (nflds(fil_idx) == 0) .or.                                      &
              (restart .and.(.not.regen_hist_file(fil_idx))))) cycle
         !
         ! Check if this is the IC file and if it's time to write.
         ! Else, use "nhtfrq" to determine if it's time to write
         ! the other history files.
         !
         if((.not. restart) .or. regen_hist_file(fil_idx)) then
            if( is_initfile(file_index=t) ) then
               write_file(fil_idx) =  write_inithist()
               prev     = .false.
            else
               if (nhtfrq(fil_idx) == 0) then
                  write_file(fil_idx) = nstep /= 0 .and. day == 1 .and. ncsec == 0
                  prev     = .true.
               else
                  write_file(fil_idx) = mod(nstep, nhtfrq(fil_idx)) == 0
                  prev     = .false.
               end if
            end if
         end if
         if (write_file(fil_idx) .or. (restart .and. regen_hist_file(fil_idx))) then
            if(masterproc) then
               if(is_initfile(file_index=t)) then
                  write(iulog,100) yr,mon,day,ncsec
100               format('WSHIST: writing time sample to Initial Conditions h-file', &
                       ' DATE=',i4.4,'/',i2.2,'/',i2.2,' NCSEC=',i6)
               else if(is_satfile(fil_idx)) then
                  write(iulog,150) nfils(fil_idx),t,yr,mon,day,ncsec
150               format('WSHIST: writing sat columns ',i6,' to h-file ', &
                       i1,' DATE=',i4.4,'/',i2.2,'/',i2.2,' NCSEC=',i6)
               else if(write_file(fil_idx)) then
                  write(iulog,200) nfils(fil_idx),t,yr,mon,day,ncsec
200               format('WSHIST: writing time sample ',i3,' to h-file ', &
                       i1,' DATE=',i4.4,'/',i2.2,'/',i2.2,' NCSEC=',i6)
               else if(restart .and. regen_hist_file(fil_idx)) then
                  write(iulog,300) nfils(fil_idx),t,yr,mon,day,ncsec
300               format('WSHIST: writing history restart ',i3,' to hr-file ', &
                       i1,' DATE=',i4.4,'/',i2.2,'/',i2.2,' NCSEC=',i6)
               end if
               write(iulog,*)
            end if
            !
            ! Starting a new volume => define the metadata
            !
            if ( (nfils(fil_idx) == 0) .or. &
                 (restart .and. regen_hist_file(fil_idx))) then
               if(restart) then
                  rhfilename_spec = '%c.cam' // trim(inst_suffix) // '.rh%t.%y-%m-%d-%s.nc'
                  fname = interpret_filename_spec(rhfilename_spec, unit=(t-1))
                  hrestpath(fil_idx)=fname
               else if(is_initfile(file_index=t)) then
                  fname = interpret_filename_spec( hfilename_spec(fil_idx) )
               else
                  fname = interpret_filename_spec( hfilename_spec(fil_idx), unit=(t-1), &
                       prev=prev )
               end if
               !
               ! Check that this new filename isn't the same as a previous or current filename
               !
               do f = 1, pfiles
                  if (masterproc.and. trim(fname) == trim(nhfil(fld_idx)) )then
                     write(iulog,*)'WSHIST: New filename same as old file = ', trim(fname)
                     write(iulog,*)'Is there an error in your filename specifiers?'
                     write(iulog,*)'hfilename_spec(', t, ') = ', hfilename_spec(fil_idx)
                     if ( t /= f )then
                        write(iulog,*)'hfilename_spec(', f, ') = ', hfilename_spec(fld_idx)
                     end if
                     call endrun
                  end if
               end do
               if(.not. restart) then
                  nhfil(fil_idx) = fname
                  if(masterproc) write(iulog,*)'WSHIST: nhfil(',t,')=',trim(nhfil(fil_idx))
                  cpath(fil_idx) = nhfil(fil_idx)
               end if
               call h_define (t, restart)
            end if

            if(is_satfile(fil_idx)) then
               call sat_hist_write( file(fil_idx), nflds(fil_idx), nfils(fil_idx))
            else
               if(restart) then
                  start=1
               else
                  nfils(fil_idx) = nfils(fil_idx) + 1
                  start = nfils(fil_idx)
               end if
               count1 = 1
               ! Setup interpolation data if history file is interpolated
               if (interpolate_output(fil_idx) .and. (.not. restart)) then
                  call set_interp_hfile(t, interpolate_info)
               end if

               ierr = pio_put_var (file(fil_idx)%File, file(fil_idx)%ndcurid,(/start/), (/count1/),(/ndcur/))
               ierr = pio_put_var (file(fil_idx)%File, file(fil_idx)%nscurid,(/start/), (/count1/),(/nscur/))
               ierr = pio_put_var (file(fil_idx)%File, file(fil_idx)%dateid,(/start/), (/count1/),(/ncdate/))

               if (.not. is_initfile(file_index=t)) then
                  ! Don't write the GHG/Solar forcing data to the IC file.
                  ierr=pio_put_var (file(fil_idx)%File, file(fil_idx)%co2vmrid,(/start/), (/count1/),(/chem_surfvals_co2_rad(vmr_in=.true.)/))
                  ierr=pio_put_var (file(fil_idx)%File, file(fil_idx)%ch4vmrid,(/start/), (/count1/),(/chem_surfvals_get('CH4VMR')/))
                  ierr=pio_put_var (file(fil_idx)%File, file(fil_idx)%n2ovmrid,(/start/), (/count1/),(/chem_surfvals_get('N2OVMR')/))
                  ierr=pio_put_var (file(fil_idx)%File, file(fil_idx)%f11vmrid,(/start/), (/count1/),(/chem_surfvals_get('F11VMR')/))
                  ierr=pio_put_var (file(fil_idx)%File, file(fil_idx)%f12vmrid,(/start/), (/count1/),(/chem_surfvals_get('F12VMR')/))
                  ierr=pio_put_var (file(fil_idx)%File, file(fil_idx)%sol_tsiid,(/start/), (/count1/),(/sol_tsi/))

                  if (solar_parms_on) then
                     ierr=pio_put_var (file(fil_idx)%File, file(fil_idx)%f107id, (/start/), (/count1/),(/ f107 /) )
                     ierr=pio_put_var (file(fil_idx)%File, file(fil_idx)%f107aid,(/start/), (/count1/),(/ f107a /) )
                     ierr=pio_put_var (file(fil_idx)%File, file(fil_idx)%f107pid,(/start/), (/count1/),(/ f107p /) )
                     ierr=pio_put_var (file(fil_idx)%File, file(fil_idx)%kpid,   (/start/), (/count1/),(/ kp /) )
                     ierr=pio_put_var (file(fil_idx)%File, file(fil_idx)%apid,   (/start/), (/count1/),(/ ap /) )
                  endif
                  if (solar_wind_on) then
                     ierr=pio_put_var (file(fil_idx)%File, file(fil_idx)%byimfid, (/start/), (/count1/),(/ byimf /) )
                     ierr=pio_put_var (file(fil_idx)%File, file(fil_idx)%bzimfid, (/start/), (/count1/),(/ bzimf /) )
                     ierr=pio_put_var (file(fil_idx)%File, file(fil_idx)%swvelid, (/start/), (/count1/),(/ swvel /) )
                     ierr=pio_put_var (file(fil_idx)%File, file(fil_idx)%swdenid, (/start/), (/count1/),(/ swden /) )
                  endif
                  if (epot_active) then
                     ierr=pio_put_var (file(fil_idx)%File, file(fil_idx)%colat_crit1_id, (/start/), (/count1/),(/ epot_crit_colats(1) /) )
                     ierr=pio_put_var (file(fil_idx)%File, file(fil_idx)%colat_crit2_id, (/start/), (/count1/),(/ epot_crit_colats(2) /) )
                  endif
               end if

               ierr = pio_put_var (file(fil_idx)%File, file(fil_idx)%datesecid,(/start/),(/count1/),(/ncsec/))
#if ( defined BFB_CAM_SCAM_IOP )
               dtime = get_step_size()
               tsec=dtime*nstep
               ierr = pio_put_var (file(fil_idx)%File, file(fil_idx)%tsecid,(/start/),(/count1/),(/tsec/))
#endif
               ierr = pio_put_var (file(fil_idx)%File, file(fil_idx)%nstephid,(/start/),(/count1/),(/nstep/))
               time = ndcur + nscur/86400._r8
               ierr=pio_put_var (file(fil_idx)%File, file(fil_idx)%timeid, (/start/),(/count1/),(/time/))

               startc(1) = 1
               startc(2) = start
               countc(1) = 2
               countc(2) = 1
               if (is_initfile(file_index=t)) then
                  tdata = time   ! Inithist file is always instantanious data
               else
                  tdata(1) = beg_time(fil_idx)
                  tdata(2) = time
               end if
               ierr=pio_put_var (file(fil_idx)%File, file(fil_idx)%tbndid, startc, countc, tdata)
               if(.not.restart) beg_time(fil_idx) = time  ! update beginning time of next interval
               startc(1) = 1
               startc(2) = start
               countc(1) = 8
               countc(2) = 1
               call datetime (cdate, ctime)
               ierr = pio_put_var (file(fil_idx)%File, file(fil_idx)%date_writtenid, startc, countc, (/cdate/))
               ierr = pio_put_var (file(fil_idx)%File, file(fil_idx)%time_writtenid, startc, countc, (/ctime/))

               if(.not. restart) then
                  !$OMP PARALLEL DO PRIVATE (fld_idx)
                  do f=1,nflds(fil_idx)
                     ! Normalized averaged fields
                     if (file(fil_idx)%hlist(fld_idx)%avgflag /= 'I') then
                        call h_normalize (f, t)
                     end if
                  end do
               end if
               !
               ! Write field to history file.  Note that this is NOT threaded due to netcdf limitations
               !
               call t_startf ('dump_field')
               do f=1,nflds(fil_idx)
                  call dump_field(f, t, restart)
               end do
               call t_stopf ('dump_field')
               !
               ! Zero history buffers and accumulators now that the fields have been written.
               !



               if(restart) then
                  do f=1,nflds(fil_idx)
                     if(associated(file(fil_idx)%hlist(fld_idx)%varid)) then
                        deallocate(file(fil_idx)%hlist(fld_idx)%varid)
                        nullify(file(fil_idx)%hlist(fld_idx)%varid)
                     end if
                  end do
                  call cam_pio_closefile(file(fil_idx)%File)
               else
                  !$OMP PARALLEL DO PRIVATE (fld_idx)
                  do f=1,nflds(fil_idx)
                     call h_zero (f, t)
                  end do
               end if
            end if
         end if

      end do

      return
   end subroutine wshist

   !#######################################################################

   subroutine addfld_1d(fname, vdim_name, avgflag, units, long_name,           &
        gridname, flag_xyfill, sampling_seq, standard_name, fill_value)

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
      character(len=*), intent(in)  :: fname      ! field name (max_fieldname_len)
      character(len=*), intent(in)  :: vdim_name  ! NetCDF dimension name (or scalar coordinate)
      character(len=1), intent(in)  :: avgflag    ! averaging flag
      character(len=*), intent(in)  :: units      ! units of fname (max_chars)
      character(len=*), intent(in)  :: long_name  ! long name of field (max_chars)

      character(len=*), intent(in), optional :: gridname    ! decomposition type
      logical, intent(in), optional :: flag_xyfill ! non-applicable xy points flagged with fillvalue
      character(len=*), intent(in), optional :: sampling_seq ! sampling sequence - if not every timestep,
      ! how often field is sampled:
      ! every other; only during LW/SW radiation calcs, etc.
      character(len=*), intent(in), optional :: standard_name  ! CF standard name (max_chars)
      real(r8),         intent(in), optional :: fill_value

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
            call endrun('ADDFLD: Invalid coordinate, '//trim(vdim_name))
         end if
         allocate(dimnames(1))
         dimnames(1) = trim(vdim_name)
      end if
      call addfld(fname, dimnames, avgflag, units, long_name, gridname,         &
           flag_xyfill, sampling_seq, standard_name, fill_value)

   end subroutine addfld_1d

   subroutine addfld_nd(fname, dimnames, avgflag, units, long_name,            &
        gridname, flag_xyfill, sampling_seq, standard_name, fill_value)

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
      use cam_history_support, only: fillvalue, hist_coord_find_levels
      use cam_grid_support,    only: cam_grid_id, cam_grid_is_zonal
      use cam_grid_support,    only: cam_grid_get_coord_names
      use constituents,        only: pcnst, cnst_get_ind, cnst_get_type_byind

      !
      ! Arguments
      !
      character(len=*), intent(in)  :: fname      ! field name (max_fieldname_len)
      character(len=*), intent(in)  :: dimnames(:) ! NetCDF dimension names (except grid dims)
      character(len=1), intent(in)  :: avgflag    ! averaging flag
      character(len=*), intent(in)  :: units      ! units of fname (max_chars)
      character(len=*), intent(in)  :: long_name  ! long name of field (max_chars)

      character(len=*), intent(in), optional :: gridname    ! decomposition type
      logical, intent(in), optional :: flag_xyfill ! non-applicable xy points flagged with fillvalue
      character(len=*), intent(in), optional :: sampling_seq ! sampling sequence - if not every timestep,
      ! how often field is sampled:
      ! every other; only during LW/SW radiation calcs, etc.
      character(len=*), intent(in), optional :: standard_name  ! CF standard name (max_chars)
      real(r8),         intent(in), optional :: fill_value

      !
      ! Local workspace
      !
      character(len=max_fieldname_len) :: fname_tmp ! local copy of fname
      character(len=max_fieldname_len) :: coord_name ! for cell_methods
      character(len=128)               :: errormsg
      character(len=3)                 :: mixing_ratio
      type(master_entry), pointer      :: listentry

      integer :: dimcnt
      integer :: idx

      if (hfiles_defined) then
         call endrun ('ADDFLD: Attempt to add field '//trim(fname)//' after history files set')
      end if

      !
      ! Ensure that new field name is not all blanks
      !
      if (len_trim(fname)==0) then
         call endrun('ADDFLD: blank field name not allowed')
      end if
      !
      ! Ensure that new field name is not longer than allowed
      ! (strip "&IC" suffix if it exists)
      !
      fname_tmp  = fname
      fname_tmp  = strip_suffix(fname_tmp)

      if (len_trim(fname_tmp) > fieldname_len) then
         write(iulog,*)'ADDFLD: field name cannot be longer than ',fieldname_len,' characters long'
         write(iulog,*)'Field name:  ',fname
         write(errormsg, *) 'Field name, "', trim(fname), '" is too long'
         call endrun('ADDFLD: '//trim(errormsg))
      end if
      !
      ! Ensure that new field doesn't already exist
      !
      listentry => get_entry_by_name(masterlinkedlist, fname)
      if(associated(listentry)) then
         call endrun ('ADDFLD:  '//fname//' already on list')
      end if

      ! If the field is an advected constituent determine whether its concentration
      ! is based on dry or wet air.
      call cnst_get_ind(fname_tmp, idx, abort=.false.)
      mixing_ratio = ''
      if (idx > 0) then
         mixing_ratio = cnst_get_type_byind(idx)
      end if

      ! Add field to Master Field List arrays fieldn and iflds
      !
      allocate(listentry)
      listentry%field%name         = fname
      listentry%field%long_name    = long_name
      listentry%field%numlev       = 1        ! Will change if lev or ilev in shape
      listentry%field%units        = units
      listentry%field%mixing_ratio = mixing_ratio
      listentry%field%meridional_complement = -1
      listentry%field%zonal_complement      = -1
      listentry%hfileindx(:) = -1
      listentry%act_somefile = .false.
      listentry%actflag(:) = .false.

      ! Make sure we have a valid gridname
      if (present(gridname)) then
         listentry%field%decomp_type = cam_grid_id(trim(gridname))
      else
         listentry%field%decomp_type = cam_grid_id('physgrid')
      end if
      if (listentry%field%decomp_type < 0) then
         write(errormsg, *) 'Invalid grid name, "', trim(gridname), '" for ',    &
              trim(fname)
         call endrun('ADDFLD: '//trim(errormsg))
      end if

      !
      ! Indicate sampling sequence of field (i.e., how often "outfld" is called)
      ! If not every timestep (default), then give a descriptor indicating the
      ! sampling pattern.  Currently, the only valid value is "rad_lwsw" for sampling
      ! during LW/SW radiation timesteps only
      !
      if (present(sampling_seq)) then
         listentry%field%sampling_seq = sampling_seq
      else
         listentry%field%sampling_seq = ' '
      end if
      ! Indicate if some field pre-processing occurred (e.g., zonal mean)
      if (cam_grid_is_zonal(listentry%field%decomp_type)) then
         call cam_grid_get_coord_names(listentry%field%decomp_type, coord_name, errormsg)
         ! Zonal method currently hardcoded to 'mean'.
         listentry%field%cell_methods = trim(coord_name)//': mean'
      else
         listentry%field%cell_methods = ''
      end if
      !
      ! Whether to apply xy fillvalue: default is false
      !
      if (present(flag_xyfill)) then
         listentry%field%flag_xyfill = flag_xyfill
      else
         listentry%field%flag_xyfill = .false.
      end if

      !
      !    Allow external packages to have fillvalues different than default
      !

      if(present(fill_value)) then
         listentry%field%fillvalue = fill_value
      else
         listentry%field%fillvalue = fillvalue
      endif

      !
      ! Process shape
      !

      if (associated(listentry%field%mdims)) then
         deallocate(listentry%field%mdims)
      end if
      nullify(listentry%field%mdims)
      dimcnt = size(dimnames)
      allocate(listentry%field%mdims(dimcnt))
      call lookup_hist_coord_indices(dimnames, listentry%field%mdims)
      if(dimcnt > maxvarmdims) then
         maxvarmdims = dimcnt
      end if
      ! Check for subcols (currently limited to first dimension)
      listentry%field%is_subcol = .false.
      if (size(dimnames) > 0) then
         if (trim(dimnames(1)) == 'psubcols') then
            if (listentry%field%decomp_type /= cam_grid_id('physgrid')) then
               write(errormsg, *) "Cannot add ", trim(fname),                      &
                    "Subcolumn history output only allowed on physgrid"
               call endrun("ADDFLD: "//errormsg)
               listentry%field%is_subcol = .true.
            end if
         end if
      end if
      ! Levels
      listentry%field%numlev = hist_coord_find_levels(dimnames)
      if (listentry%field%numlev <= 0) then
         listentry%field%numlev = 1
      end if

      !
      ! Dimension history info based on decomposition type (grid)
      !
      call set_field_dimensions(listentry%field)

      !
      ! These 2 fields are used only in master field list, not runtime field list
      !
      listentry%avgflag(:) = avgflag
      listentry%actflag(:) = .false.

      do dimcnt = 1, pfiles
         call AvgflagToString(avgflag, listentry%time_op(dimcnt))
      end do

      nullify(listentry%next_entry)

      call add_entry_to_master(listentry)
      return
   end subroutine addfld_nd

   !#######################################################################

   ! field_part_of_vector: Determinie if fname is part of a vector set
   !       Optionally fill in the names of the vector set fields
   logical function field_part_of_vector(fname, meridional_name, zonal_name)

      ! Dummy arguments
      character(len=*),           intent(in)  :: fname
      character(len=*), optional, intent(out) :: meridional_name
      character(len=*), optional, intent(out) :: zonal_name

      ! Local variables
      type(master_entry), pointer             :: listentry

      listentry => get_entry_by_name(masterlinkedlist, fname)
      if (associated(listentry)) then
         if ( (len_trim(listentry%meridional_field) > 0) .or.                     &
              (len_trim(listentry%zonal_field) > 0)) then
            field_part_of_vector = .true.
            if (present(meridional_name)) then
               meridional_name = listentry%meridional_field
            end if
            if (present(zonal_name)) then
               zonal_name = listentry%zonal_field
            end if
         else
            field_part_of_vector = .false.
         end if
      else
         field_part_of_vector = .false.
      end if
      if (.not. field_part_of_vector) then
         if (present(meridional_name)) then
            meridional_name = ''
         end if
         if (present(zonal_name)) then
            zonal_name = ''
         end if
      end if

   end function field_part_of_vector


   ! register_vector_field: Register a pair of history field names as
   !           being a vector complement set.
   !           This information is used to set up interpolated history output.
   ! NB: register_vector_field must be called after both fields are defined
   !     with addfld
   subroutine register_vector_field(zonal_field_name, meridional_field_name)

      ! Dummy arguments
      character(len=*),             intent(in) :: zonal_field_name
      character(len=*),             intent(in) :: meridional_field_name

      ! Local variables
      type(master_entry), pointer      :: mlistentry
      type(master_entry), pointer      :: zlistentry
      character(len=*),   parameter    :: subname = 'REGISTER_VECTOR_FIELD'
      character(len=max_chars)         :: errormsg

      if (hfiles_defined) then
         write(errormsg, '(5a)') ': Attempt to register vector field (',      &
              trim(zonal_field_name), ', ', trim(meridional_field_name),      &
              ') after history files set'
         call endrun (trim(subname)//errormsg)
      end if

      ! Look for the field IDs
      zlistentry => get_entry_by_name(masterlinkedlist, zonal_field_name)
      mlistentry => get_entry_by_name(masterlinkedlist, meridional_field_name)
      ! Has either of these fields been previously registered?
      if (associated(mlistentry)) then
         if (len_trim(mlistentry%meridional_field) > 0) then
            write(errormsg, '(9a)') ': ERROR attempting to register vector ', &
                 'field (', trim(zonal_field_name), ', ',                     &
                 trim(meridional_field_name), '), ',                          &
                 trim(meridional_field_name),                                 &
                 ' has been registered as part of a vector field with ',      &
                 trim(mlistentry%meridional_field)
            call endrun (trim(subname)//errormsg)
         else if (len_trim(mlistentry%zonal_field) > 0) then
            write(errormsg, '(9a)') ': ERROR attempting to register vector ', &
                 'field (', trim(zonal_field_name), ', ',                     &
                 trim(meridional_field_name), '), ',                          &
                 trim(meridional_field_name),                                 &
                 ' has been registered as part of a vector field with ',      &
                 trim(mlistentry%zonal_field)
            call endrun (trim(subname)//errormsg)
         end if
      end if
      if (associated(zlistentry)) then
         if (len_trim(zlistentry%meridional_field) > 0) then
            write(errormsg, '(9a)') ': ERROR attempting to register vector ', &
                 'field (', trim(zonal_field_name), ', ',                     &
                 trim(meridional_field_name), '), ', trim(zonal_field_name),  &
                 ' has been registered as part of a vector field with ',      &
                 trim(zlistentry%meridional_field)
            call endrun (trim(subname)//errormsg)
         else if (len_trim(zlistentry%zonal_field) > 0) then
            write(errormsg, '(9a)') ': ERROR attempting to register vector ', &
                 'field (', trim(zonal_field_name), ', ',                     &
                 trim(meridional_field_name), '), ', trim(zonal_field_name),  &
                 ' has been registered as part of a vector field with ',      &
                 trim(zlistentry%meridional_field)
            call endrun (trim(subname)//errormsg)
         end if
      end if
      if(associated(mlistentry) .and. associated(zlistentry)) then
         zlistentry%meridional_field = mlistentry%field%name
         zlistentry%zonal_field      = ''
         mlistentry%meridional_field = ''
         mlistentry%zonal_field      = zlistentry%field%name
      else if (associated(mlistentry)) then
         write(errormsg, '(8a)') ': ERROR attempting to register vector',     &
              ' field (', trim(zonal_field_name), ', ',                       &
              trim(meridional_field_name), '), ',                             &
              trim(zonal_field_name), ' is not defined'
         call endrun (trim(subname)//errormsg)
      else if (associated(zlistentry)) then
         write(errormsg, '(8a)') ': ERROR attempting to register vector',     &
              ' field (', trim(zonal_field_name), ', ',                       &
              trim(meridional_field_name), '), ',                             &
              trim(meridional_field_name), ' is not defined'
         call endrun (trim(subname)//errormsg)
      else
         write(errormsg, '(6a)') ': ERROR attempting to register vector',     &
              ' field (', trim(zonal_field_name), ', ',                       &
              trim(meridional_field_name), '), neither field is defined'
         call endrun (trim(subname)//errormsg)
      end if
   end subroutine register_vector_field

   subroutine add_entry_to_master( newentry)
      type(master_entry), target, intent(in) :: newentry
      type(master_entry), pointer :: listentry

      if(associated(masterlinkedlist)) then
         listentry => masterlinkedlist
         do while(associated(listentry%next_entry))
            listentry=>listentry%next_entry
         end do
         listentry%next_entry=>newentry
      else
         masterlinkedlist=>newentry
      end if

   end subroutine add_entry_to_master

   !#######################################################################

   subroutine wrapup (rstwr, nlend)
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
      use pio, only : pio_file_is_open
      use shr_kind_mod,  only: r8 => shr_kind_r8
      use ioFileMod
      use time_manager,  only: get_nstep, get_curr_date, get_curr_time
      use cam_pio_utils, only: cam_pio_openfile, cam_pio_closefile

      !
      ! Input arguments
      !
      logical, intent(in) :: rstwr   ! true => restart files are written this timestep
      logical, intent(in) :: nlend   ! Flag if time to end

      !
      ! Local workspace
      !
      integer  :: nstep            ! current timestep number
      integer  :: ncsec            ! time of day relative to current date [secs]
      integer  :: ndcur            ! days component of current time
      integer  :: nscur            ! seconds component of current time
      integer  :: yr, mon, day     ! year, month, day components of a date

      logical  :: lfill   (pfiles) ! Is history file ready to dispose?
      logical  :: lhdisp           ! true => history file is disposed
      logical  :: lhfill           ! true => history file is full

      integer  :: t                ! History file number
      integer  :: f
      real(r8) :: tday             ! Model day number for printout
      !-----------------------------------------------------------------------

      file => history_file

      nstep = get_nstep()
      call get_curr_date(yr, mon, day, ncsec)
      call get_curr_time(ndcur, nscur)
      !
      !-----------------------------------------------------------------------
      ! Dispose history files.
      !-----------------------------------------------------------------------
      !
      ! Begin loop over pfiles (the no. of declared history files - primary
      ! and auxiliary).  This loop disposes a history file to Mass Store
      ! when appropriate.
      !
      do fil_idx = 1, pfiles
         if (nflds(fil_idx) == 0) cycle

         lfill(fil_idx) = .false.
         !
         ! Find out if file is full
         !
         if (write_file(fil_idx) .and. nfils(fil_idx) >= mfilt(fil_idx)) then
            lfill(fil_idx) = .true.
         endif
         !
         ! Dispose history file if
         !    1) file is filled or
         !    2) this is the end of run and file has data on it or
         !    3) restarts are being put out and history file has data on it
         !
         if (lfill(fil_idx) .or. (nlend .and. nfils(fil_idx) >= 1) .or. (rstwr .and. nfils(fil_idx) >= 1)) then
            !
            ! Dispose history file
            !
            !
            ! Is this the 0 timestep data of a monthly run?
            ! If so, just close primary unit do not dispose.
            !
            if (masterproc) write(iulog,*)'WRAPUP: nf_close(',t,')=',trim(nhfil(fil_idx))
            if(pio_file_is_open(file(fil_idx)%File)) then
               if (nlend .or. lfill(fil_idx)) then
                  do f=1,nflds(fil_idx)
                     if (associated(file(fil_idx)%hlist(fld_idx)%varid)) then
                        deallocate(file(fil_idx)%hlist(fld_idx)%varid)
                        nullify(file(fil_idx)%hlist(fld_idx)%varid)
                     end if
                  end do
               end if
               call cam_pio_closefile(file(fil_idx)%File)
            end if
            if (nhtfrq(fil_idx) /= 0 .or. nstep > 0) then

               !
               ! Print information concerning model output.
               ! Model day number = iteration number of history file data * delta-t / (seconds per day)
               !
               tday = ndcur + nscur/86400._r8
               if(masterproc) then
                  if (t==1) then
                     write(iulog,*)'   Primary history file'
                  else
                     write(iulog,*)'   Auxiliary history file number ', t-1
                  end if
                  write(iulog,9003)nstep,nfils(fil_idx),tday
                  write(iulog,9004)
               end if
               !
               ! Auxilary files may have been closed and saved off without being full.
               ! We must reopen the files and position them for more data.
               ! Must position auxiliary files if not full
               !
               if (.not.nlend .and. .not.lfill(fil_idx)) then
                  call cam_PIO_openfile (file(fil_idx)%File, nhfil(fil_idx), PIO_WRITE)
                  call h_inquire(fil_idx)
               end if
            endif                 ! if 0 timestep of montly run****
         end if                      ! if time dispose history fiels***
      end do                         ! do pfiles
      !
      ! Reset number of files on each history file
      !
      do fil_idx = 1, pfiles
         if (nflds(fil_idx) == 0) cycle
         lhfill = write_file(fil_idx) .and. nfils(fil_idx) >= mfilt(fil_idx)
         lhdisp = lhfill .or. (nlend .and. nfils(fil_idx) >= 1) .or. &
              (rstwr .and. nfils(fil_idx) >= 1)
         if (lhfill.and.lhdisp) then
            nfils(fil_idx) = 0
         endif
      end do
      return
9003  format('    Output at NSTEP     = ',i10,/, &
           '    Number of time samples on this file = ',i10,/, &
           '    Model Day           = ',f10.2)
9004  format('---------------------------------------')
   end subroutine wrapup

   !#######################################################################

   subroutine bld_outfld_hash_tbls()
      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: Build primary and overflow hash tables for outfld processing.
      !
      ! Steps:
      !  1) Foreach field on masterlist, find all collisions.
      !  2) Given the number of collisions, verify overflow table has sufficient
      !     space.
      !  3) Build primary and overflow indices.
      !
      !-----------------------------------------------------------------------
      !
      !  Local.
      !
      integer :: ff
      integer :: ii
      integer :: itemp
      integer :: ncollisions
      integer :: hash_key
      type(master_entry), pointer :: listentry
      !
      !  1) Find all collisions.
      !
      tbl_hash_pri = 0

      ff=0
      allocate(masterlist(nfmaster))
      listentry=>masterlinkedlist
      do while(associated(listentry))
         ff=ff+1
         masterlist(ff)%thisentry=>listentry
         listentry=>listentry%next_entry
      end do
      if(ff /= nfmaster) then
         write(iulog,*) 'nfmaster = ',nfmaster, ' ff=',ff
         call endrun('mismatch in expected size of nfmaster')
      end if


      do ff = 1, nfmaster
         hash_key = gen_hash_key(masterlist(ff)%thisentry%field%name)
         tbl_hash_pri(hash_key) = tbl_hash_pri(hash_key) + 1
      end do

      !
      !  2) Count number of collisions and define start of a individual
      !     collision's chain in overflow table. A collision is defined to be any
      !     location in tbl_hash_pri that has a value > 1.
      !
      ncollisions = 0
      do ii = 0, tbl_hash_pri_sz-1
         if ( tbl_hash_pri(ii) > 1 ) then  ! Define start of chain in O.F. table
            itemp = tbl_hash_pri(ii)
            tbl_hash_pri(ii) = -(ncollisions + 1)
            ncollisions = ncollisions + itemp + 1
         end if
      end do

      if ( ncollisions > tbl_hash_oflow_sz ) then
         write(iulog,*) 'BLD_OUTFLD_HASH_TBLS: ncollisions > tbl_hash_oflow_sz', &
              ncollisions, tbl_hash_oflow_sz
         call endrun()
      end if

      !
      !  3) Build primary and overflow tables.
      !     i - set collisions in tbl_hash_pri to point to their respective
      !         chain in the overflow table.
      !
      tbl_hash_oflow = 0

      do ff = 1, nfmaster
         hash_key = gen_hash_key(masterlist(ff)%thisentry%field%name)
         if ( tbl_hash_pri(hash_key) < 0 ) then
            ii = abs(tbl_hash_pri(hash_key))
            tbl_hash_oflow(ii) = tbl_hash_oflow(ii) + 1
            tbl_hash_oflow(ii+tbl_hash_oflow(ii)) = ff
         else
            tbl_hash_pri(hash_key) = ff
         end if
      end do

      !
      !  Dump out primary and overflow hashing tables.
      !
      !   if ( masterproc ) then
      !      do ii = 0, tbl_hash_pri_sz-1
      !         if ( tbl_hash_pri(ii) /= 0 ) write(iulog,666) 'tbl_hash_pri', ii, tbl_hash_pri(ii)
      !      end do
      !
      !      do ii = 1, tbl_hash_oflow_sz
      !         if ( tbl_hash_oflow(ii) /= 0 ) write(iulog,666) 'tbl_hash_oflow', ii, tbl_hash_oflow(ii)
      !      end do
      !
      !      itemp = 0
      !      ii = 1
      !      do
      !         if ( tbl_hash_oflow(ii) == 0 ) exit
      !         itemp = itemp + 1
      !         write(iulog,*) 'Overflow chain ', itemp, ' has ', tbl_hash_oflow(ii), ' entries:'
      !         do ff = 1, tbl_hash_oflow(ii)  ! dump out colliding names on this chain
      !            write(iulog,*) '     ', ff, ' = ', tbl_hash_oflow(ii+ff), &
      !                       ' ', masterlist(tbl_hash_oflow(ii+ff))%thisentry%field%name
      !         end do
      !         ii = ii + tbl_hash_oflow(ii) +1 !advance pointer to start of next chain
      !      end do
      !   end if

      return
666   format(1x, a, '(', i4, ')', 1x, i6)

   end subroutine bld_outfld_hash_tbls

   !#######################################################################

   subroutine bld_hfilefld_indices
      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: Set history file field indicies in masterlist for each
      !          field defined on every file.
      !
      ! Note: because of restart processing, the actflag field is cleared and
      !       then set only for active output fields on the different history
      !       files.
      !
      !-----------------------------------------------------------------------
      !
      !  Arguments:
      !

      !
      !  Local.
      !
      integer :: f
      integer :: t

      !
      !  Initialize hfileindx to an invalid value.
      !
      type(master_entry), pointer :: listentry

      ! reset all the active flags to false
      ! this is needed so that restarts work properly -- fvitt
      listentry=>masterlinkedlist
      do while(associated(listentry))
         listentry%actflag(:) = .false.
         listentry%act_somefile = .false.
         listentry=>listentry%next_entry
      end do

      do t = 1, pfiles
         do f = 1, nflds(fil_idx)
            listentry => get_entry_by_name(masterlinkedlist, file(fil_idx)%hlist(fld_idx)%field%name)
            if(.not.associated(listentry)) then
               write(iulog,*) 'BLD_HFILEFLD_INDICES: something wrong, field not found on masterlist'
               write(iulog,*) 'BLD_HFILEFLD_INDICES: t, f, ff = ', t, f
               write(iulog,*) 'BLD_HFILEFLD_INDICES: file%name = ', file(fil_idx)%hlist(fld_idx)%field%name
               call endrun
            end if
            listentry%act_somefile = .true.
            listentry%actflag(fil_idx) = .true.
            listentry%hfileindx(fil_idx) = f
         end do
      end do

      !
      ! set flag indicating h-file contents are now defined (needed by addfld)
      !
      hfiles_defined = .true.

      return
   end subroutine bld_hfilefld_indices

   !#######################################################################

   logical function hist_fld_active(fname)
      !
      !------------------------------------------------------------------------
      !
      ! Purpose: determine if a field is active on any history file
      !
      !------------------------------------------------------------------------
      !
      ! Arguments
      !
      character(len=*), intent(in) :: fname ! Field name
      !
      ! Local variables
      !
      character*(max_fieldname_len) :: fname_loc  ! max-char equivalent of fname
      integer :: ff                  ! masterlist index pointer
      !-----------------------------------------------------------------------

      fname_loc = fname
      ff = get_masterlist_indx(fname_loc)
      if ( ff < 0 ) then
         hist_fld_active = .false.
      else
         hist_fld_active = masterlist(ff)%thisentry%act_somefile
      end if

   end function hist_fld_active

   !#######################################################################

   function hist_fld_col_active(fname, lchnk, numcols)
      use cam_history_support, only: history_patch_t

      ! Determine whether each column in a field is active on any history file.
      ! The purpose of this routine is to provide information which would allow
      ! a diagnostic physics parameterization to only be run on a subset of
      ! columns in the case when only column or regional output is requested.
      !
      ! **N.B.** The field is assumed to be using the physics decomposition.

      ! Arguments
      character(len=*), intent(in) :: fname   ! Field name
      integer,          intent(in) :: lchnk   ! chunk ID
      integer,          intent(in) :: numcols ! Size of return array

      ! Return value
      logical :: hist_fld_col_active(numcols)

      ! Local variables
      integer                         :: ff          ! masterlist index pointer
      integer                         :: i
      integer                         :: t           ! history file (fil_idx) index
      integer                         :: f           ! field index
      integer                         :: decomp
      logical                         :: activeloc(numcols)
      integer                         :: num_patches
      logical                         :: patch_output
      logical                         :: found
      type(history_patch_t), pointer  :: patchptr

      type (active_entry),   pointer  :: file(:)

      !-----------------------------------------------------------------------

      ! Initialize to false.  Then look to see if and where active.
      hist_fld_col_active = .false.

      ! Check for name in the master list.
      call get_field_properties(fname, found, file_out=file, ff_out=ff)

      ! If not in master list then return.
      if (.not. found) return

      ! If in master list, but not active on any file then return
      if (.not. masterlist(ff)%thisentry%act_somefile) return

      ! Loop over history files and check for the field/column in each one
      do t = 1, pfiles

         ! Is the field active in this file?  If not the cycle to next file.
         if (.not. masterlist(ff)%thisentry%actflag(fil_idx)) cycle

         f = masterlist(ff)%thisentry%hfileindx(fil_idx)
         decomp = file(fil_idx)%hlist(fld_idx)%field%decomp_type
         patch_output = associated(file(fil_idx)%patches)

         ! Check whether this file has patch (column) output.
         if (patch_output) then
            num_patches = size(file(fil_idx)%patches)

            do i = 1, num_patches
               patchptr => file(fil_idx)%patches(i)
               activeloc = .false.
               call patchptr%active_cols(decomp, lchnk, activeloc)
               hist_fld_col_active = hist_fld_col_active .or. activeloc
            end do
         else

            ! No column output has been requested.  In that case the field has
            ! global output which implies all columns are active.  No need to
            ! check any other history files.
            hist_fld_col_active = .true.
            exit

         end if

      end do ! history files

   end function hist_fld_col_active

end module cam_history
