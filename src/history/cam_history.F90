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
   use cam_history_support,  only: pfiles

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
!   public :: history_write_files    ! Write files out
!   public :: cam_hist_init_files    ! Initialization
!   public :: history_finalize       ! process history files at end of run
!   public :: history_write_IC       ! flag to dump of IC to IC file
!   public :: history_define_fld     ! Add a field to history file
!   public :: history_capture_fld    ! Capture current state of a model field
!   public :: history_fld_active     ! .true. if a field is active on any history file
!   public :: history_fld_col_active ! .true. for each column where a field is active on any history file
!   public :: register_vector_field  ! Register vector field set for interpolated output

   ! Private data
   type(hist_file_t), pointer :: hist_configs(:)

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
      hist_configs => hist_read_namelist_config(nlfile)
      write(iulog,*) 'peverwhee'
      write(iulog,*) hist_configs(1)%filename()
      !if (check_endrun(test_desc=test_msg, output=out_unit)) then
      !   err_cnt = err_cnt + 1
      !end if
      !

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

end module cam_history
