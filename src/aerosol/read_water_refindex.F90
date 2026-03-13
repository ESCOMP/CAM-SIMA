!-------------------------------------------------------------------
! Read water refractive indices for modal aerosol optics.
!-------------------------------------------------------------------
! Note (hplin 3/14/26): I suppose this could be a ccpp scheme or
! collapsed into the aerosol_optics scheme init phase and use the
! CCPP i/o reader instead.
module read_water_refindex
  use ccpp_kinds,   only: kind_phys
  use shr_kind_mod, only: shr_kind_cl
  use runtime_obj,  only: unset_str

  implicit none
  private

  public :: read_water_refindex_readnl
  public :: read_water_refindex_file

  ! Private module data
  character(len=shr_kind_cl) :: water_refindex_file = unset_str

!> \section arg_table_read_water_refindex  Argument Table
!! \htmlinclude read_water_refindex.html
  ! Complex refractive indices for water (shortwave and longwave)
  complex(kind_phys), public, allocatable, target :: crefwsw(:)  ! dimension(nswbands)
  complex(kind_phys), public, allocatable, target :: crefwlw(:)  ! dimension(nlwbands)

contains

  subroutine read_water_refindex_readnl(nlfile)
    use shr_nl_mod,     only: find_group_name => shr_nl_find_group_name
    use shr_kind_mod,   only: shr_kind_cm
    use mpi,            only: mpi_character
    use spmd_utils,     only: mpicom
    use cam_logfile,    only: iulog
    use cam_abortutils, only: endrun
    use spmd_utils,     only: masterproc

    character(len=*), intent(in) :: nlfile

    integer                     :: unitn, ierr
    character(len=*), parameter :: subname = 'read_water_refindex_readnl'
    character(len=shr_kind_cm)  :: errmsg

    namelist /aerosol_optics_nl/ water_refindex_file

    errmsg = ''

    if (masterproc) then
       open(newunit=unitn, file=trim(nlfile), status='old')
       call find_group_name(unitn, 'aerosol_optics_nl', status=ierr)
       if (ierr == 0) then
          read(unitn, aerosol_optics_nl, iostat=ierr, iomsg=errmsg)
          if (ierr /= 0) then
             call endrun(subname // ':: ERROR reading namelist: ' // errmsg)
          end if
       end if
       close(unitn)
    end if

    ! Broadcast namelist variables
    call mpi_bcast(water_refindex_file, len(water_refindex_file), mpi_character, 0, mpicom, ierr)

    if (masterproc) then
       write(iulog,*) subname, ' options:'
       write(iulog,*) '  Water refindex file: ', trim(water_refindex_file)
    end if

  end subroutine read_water_refindex_readnl

  subroutine read_water_refindex_file()
    !------------------------------------------------------------------
    ! Read water refractive index dataset.
    ! File contains real and imaginary parts of the refractive index
    ! for shortwave and longwave bands.
    !------------------------------------------------------------------
    use cam_logfile,          only: iulog
    use cam_abortutils,       only: endrun, check_allocate
    use spmd_utils,           only: masterproc
    use ioFileMod,            only: cam_get_file
    use cam_pio_utils,        only: cam_pio_openfile
    use pio,                  only: file_desc_t, var_desc_t, pio_inq_dimid, pio_inq_dimlen
    use pio,                  only: pio_inq_varid, pio_get_var, pio_closefile, pio_nowrite
    use radiation_namelist,   only: nswbands, nlwbands
    use phys_vars_init_check, only: mark_as_initialized

    ! Local variables
    integer :: ierr
    type(file_desc_t) :: pio_id
    integer :: dimid, nsw_file, nlw_file
    type(var_desc_t) :: vid
    real(kind_phys), allocatable :: refr(:), refi(:)
    character(len=shr_kind_cl) :: locfn
    character(len=*), parameter :: subname = 'read_water_refindex_file'
    character(len=256) :: errmsg

    ! Guard: skip if no file specified
    if (trim(water_refindex_file) == trim(unset_str) .or. &
        trim(water_refindex_file) == 'UNSET_PATH') then
       if (masterproc) then
          write(iulog,*) subname, ': no water refindex file specified, skipping'
       end if
       return
    end if

    !-----------------------------------------------------------------------
    ! Open file
    !-----------------------------------------------------------------------
    call cam_get_file(water_refindex_file, locfn, allow_fail=.false.)
    call cam_pio_openfile(pio_id, trim(locfn), PIO_NOWRITE)

    !-----------------------------------------------------------------------
    ! Validate shortwave dimension
    !-----------------------------------------------------------------------
    ierr = pio_inq_dimid(pio_id, 'sw_band', dimid)
    ierr = pio_inq_dimlen(pio_id, dimid, nsw_file)
    if (nsw_file /= nswbands) then
       write(iulog,*) subname, ': nswbands mismatch: file=', nsw_file, ' expected=', nswbands
       call endrun(subname//': nswbands mismatch in water refindex file')
    end if

    !-----------------------------------------------------------------------
    ! Validate longwave dimension
    !-----------------------------------------------------------------------
    ierr = pio_inq_dimid(pio_id, 'lw_band', dimid)
    ierr = pio_inq_dimlen(pio_id, dimid, nlw_file)
    if (nlw_file /= nlwbands) then
       write(iulog,*) subname, ': nlwbands mismatch: file=', nlw_file, ' expected=', nlwbands
       call endrun(subname//': nlwbands mismatch in water refindex file')
    end if

    !-----------------------------------------------------------------------
    ! Read shortwave refractive indices
    !-----------------------------------------------------------------------
    allocate(refr(nswbands), refi(nswbands), stat=ierr, errmsg=errmsg)
    call check_allocate(ierr, subname, 'refr(nswbands), refi(nswbands)', errmsg=errmsg, &
                        file=__FILE__, line=__LINE__)

    ierr = pio_inq_varid(pio_id, 'refindex_real_water_sw', vid)
    ierr = pio_get_var(pio_id, vid, refr)

    ierr = pio_inq_varid(pio_id, 'refindex_im_water_sw', vid)
    ierr = pio_get_var(pio_id, vid, refi)

    allocate(crefwsw(nswbands), stat=ierr, errmsg=errmsg)
    call check_allocate(ierr, subname, 'crefwsw(nswbands)', errmsg=errmsg, &
                        file=__FILE__, line=__LINE__)
    crefwsw(:) = cmplx(refr(:), refi(:), kind=kind_phys)

    deallocate(refr, refi)

    !-----------------------------------------------------------------------
    ! Read longwave refractive indices
    !-----------------------------------------------------------------------
    allocate(refr(nlwbands), refi(nlwbands), stat=ierr, errmsg=errmsg)
    call check_allocate(ierr, subname, 'refr(nlwbands), refi(nlwbands)', errmsg=errmsg, &
                        file=__FILE__, line=__LINE__)

    ierr = pio_inq_varid(pio_id, 'refindex_real_water_lw', vid)
    ierr = pio_get_var(pio_id, vid, refr)

    ierr = pio_inq_varid(pio_id, 'refindex_im_water_lw', vid)
    ierr = pio_get_var(pio_id, vid, refi)

    allocate(crefwlw(nlwbands), stat=ierr, errmsg=errmsg)
    call check_allocate(ierr, subname, 'crefwlw(nlwbands)', errmsg=errmsg, &
                        file=__FILE__, line=__LINE__)
    crefwlw(:) = cmplx(refr(:), refi(:), kind=kind_phys)

    deallocate(refr, refi)

    !-----------------------------------------------------------------------
    ! Close file
    !-----------------------------------------------------------------------
    call pio_closefile(pio_id)

    if (masterproc) then
       write(iulog,*) subname, ': successfully read water refractive indices'
       write(iulog,*) '  nswbands=', nswbands, '  nlwbands=', nlwbands
    end if

    !--------------------------------------------------------
    ! Mark variables as initialized so they are not read from initial conditions
    !--------------------------------------------------------
    call mark_as_initialized('water_refractive_index_for_shortwave_radiation')
    call mark_as_initialized('water_refractive_index_for_longwave_radiation')

  end subroutine read_water_refindex_file

end module read_water_refindex
