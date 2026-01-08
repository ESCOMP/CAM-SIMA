module radiation_namelist
!--------------------------------------------------------------------------
!
! This module provides namelist functionality for the radiation physics package(s).
!
!--------------------------------------------------------------------------

  use ccpp_kinds,   only: kind_phys

  implicit none
  private

  ! public routines
  public :: radiation_readnl

  ! public variables
  !> \section arg_table_radiation_namelist Argument Table
  !! \htmlinclude arg_table_radiation_namelist.html
  !!
  integer, public, protected :: iradsw = -1     ! freq. of shortwave radiation calc in time steps (positive)
                                                ! or hours (negative).
  integer, public, protected :: iradlw = -1     ! frequency of longwave rad. calc. in time steps (positive)
                                                ! or hours (negative).

  integer, public, protected :: irad_always = 0 ! Specifies length of time in timesteps (positive)
                                                ! or hours (negative) SW/LW radiation will be
                                                ! run continuously from the start of an
                                                ! initial or restart run
  logical, public, protected :: use_rad_uniform_angle = .false. ! if true, use the namelist rad_uniform_angle for the coszrs calculation
  real(kind_phys), public, protected :: rad_uniform_angle = -99._kind_phys ! radians

!==============================================================================
contains
!==============================================================================

  subroutine radiation_readnl(nlfile)

    use shr_nl_mod,     only: find_group_name => shr_nl_find_group_name
    use mpi,            only: mpi_logical, mpi_integer, mpi_real8
    use spmd_utils,     only: mpicom
    use cam_logfile,    only: iulog
    use cam_abortutils, only: endrun
    use spmd_utils,     only: masterproc, masterprocid
    use time_manager,   only: get_step_size
    use shr_kind_mod,   only: shr_kind_cm

    !-----------------------------------------------------------------------
    !
    ! Read CAM-SIMA's radiation namelist
    !
    !-----------------------------------------------------------------------

    ! filepath for file containing namelist input
    character(len=*), intent(in) :: nlfile

    ! Local variables
    integer                      :: unitn, ierr
    character(len=*), parameter  :: sub = 'radiation_readnl'
    character(len=shr_kind_cm)   :: errmsg
    integer                      :: dtime

    namelist /radiation_nl/ iradsw, iradlw, irad_always, use_rad_uniform_angle, &
                            rad_uniform_angle

    errmsg = ''

    if (masterproc) then
       open(newunit=unitn, file=trim(nlfile), status='old')
       call find_group_name(unitn, 'radiation_nl', status=ierr)
       if (ierr == 0) then
          read(unitn, radiation_nl, iostat=ierr, iomsg=errmsg)
          if (ierr /= 0) then
             call endrun(sub // ':: ERROR reading namelist: ' // errmsg)
          end if
       end if
       close(unitn)
    end if

    ! Broadcast namelist variable
    call mpi_bcast(use_rad_uniform_angle, 1, mpi_logical, masterprocid, mpicom, ierr)
    if (ierr /= 0) then
       call endrun(sub//": FATAL: mpi_bcast: use_rad_uniform_angle",      &
            file=__FILE__, line=__LINE__)
    end if
    call mpi_bcast(iradsw, 1, mpi_integer, masterprocid, mpicom, ierr)
    if (ierr /= 0) then
       call endrun(sub//": FATAL: mpi_bcast: iradsw",                     &
            file=__FILE__, line=__LINE__)
    end if
    call mpi_bcast(iradlw, 1, mpi_integer, masterprocid, mpicom, ierr)
    if (ierr /= 0) then
       call endrun(sub//": FATAL: mpi_bcast: iradlw",                     &
            file=__FILE__, line=__LINE__)
    end if
    call mpi_bcast(irad_always, 1, mpi_integer, masterprocid, mpicom, ierr)
    if (ierr /= 0) then
       call endrun(sub//": FATAL: mpi_bcast: irad_always",                &
            file=__FILE__, line=__LINE__)
    end if
    call mpi_bcast(rad_uniform_angle, 1, mpi_real8, masterprocid, mpicom, ierr)
    if (ierr /= 0) then
       call endrun(sub//": FATAL: mpi_bcast: rad_uniform_angle",                &
            file=__FILE__, line=__LINE__)
    end if

    if (use_rad_uniform_angle .and. rad_uniform_angle == -99._kind_phys) then
       call endrun(sub//': ERROR - use_rad_uniform_angle is set to .true,' &
                      //' but rad_uniform_angle is not set ')
    end if

    ! Convert iradsw, iradlw and irad_always from hours to timesteps if necessary
    dtime  = get_step_size()
    if (iradsw      < 0) iradsw      = nint((-iradsw     *3600._kind_phys)/dtime)
    if (iradlw      < 0) iradlw      = nint((-iradlw     *3600._kind_phys)/dtime)
    if (irad_always < 0) irad_always = nint((-irad_always*3600._kind_phys)/dtime)

    !-----------------------------------------------------------------------
    ! Print runtime options to log.
    !-----------------------------------------------------------------------

    if (masterproc) then
       write(iulog,*) 'RRTMGP radiation scheme parameters:'
       write(iulog,10) iradsw, iradlw, irad_always, use_rad_uniform_angle
    end if

10 format('  Frequency (timesteps) of Shortwave radiation calc:  ',i5/, &
          '  Frequency (timesteps) of Longwave Radiation calc:   ',i5/, &
          '  SW/LW calc done every timestep for first N steps. N=',i5/, &
          '  Use average zenith angle:                           ',l5/)

  end subroutine radiation_readnl

end module radiation_namelist
