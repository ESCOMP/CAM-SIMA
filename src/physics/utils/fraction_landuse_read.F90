! Support module for aerosol (and eventually gas) dry deposition to read
! the land use class fractions from the dry deposition surface dataset
! (namelist drydep_srf_file, the "atmsrf" file).
!
! Remarks: this module is not CCPP-ized but is written specifically for
! grid decomposition-aware I/O in CAM-SIMA, following the
! gravity_wave_drag_ridge_read pattern: the field is read on the physics
! grid decomposition with a custom non-grid dimension (class = n_land_type)
! and provided to underlying CCPP schemes via the argument table below.
!
! In CAM, fraction_landuse is owned by mo_drydep and read via infld on the
! physics grid; the values here are the same PIO read of the same file
! variable, so they are bitwise identical to CAM's on the same grid and
! need not be captured in physics snapshots.
!
! If drydep_srf_file is not set, the fractions are left at zero with a
! warning (CAM's lenient structured-grid fallback); a set but unreadable
! file or a missing file variable is a hard error, matching CAM's
! requirement that unstructured grids provide this dataset.
module fraction_landuse_read
  use ccpp_kinds,   only: kind_phys
  use shr_kind_mod, only: shr_kind_cl

  implicit none
  private

  public :: fraction_landuse_readnl
  public :: fraction_landuse_read_file

  ! Dry deposition surface dataset (rel pathname).
  character(len=shr_kind_cl) :: drydep_srf_file     = 'UNSET_PATH'
  ! Resolved pathname.
  character(len=shr_kind_cl) :: drydep_srf_file_loc = 'UNSET_PATH'

  ! Below data is provided externally to CCPP schemes.
!> \section arg_table_fraction_landuse_read  Argument Table
!! \htmlinclude fraction_landuse_read.html
  ! Number of land use classes in the Wesely dry deposition scheme
  ! (CAM mo_drydep n_land_type).
  integer,         parameter,   public :: n_land_type = 11

  real(kind_phys), allocatable, public :: fraction_landuse(:,:) ! land use class fraction (ncol, n_land_type) [1]

contains

  subroutine fraction_landuse_readnl(nlfile)
    use shr_nl_mod,      only: find_group_name => shr_nl_find_group_name
    use shr_kind_mod,    only: shr_kind_cm
    use mpi,             only: mpi_character
    use spmd_utils,      only: mpicom
    use cam_logfile,     only: iulog
    use cam_abortutils,  only: endrun
    use spmd_utils,      only: masterproc
    use cam_initfiles,   only: unset_path_str

    ! filepath for file containing namelist input
    character(len=*), intent(in) :: nlfile

    ! Local variables
    integer                      :: unitn, errflg
    character(len=*), parameter  :: subname = 'fraction_landuse_readnl'
    character(len=shr_kind_cm)   :: errmsg

    namelist /drydep_input_nl/ drydep_srf_file

    errmsg = ''
    errflg = 0

    if (masterproc) then
       open(newunit=unitn, file=trim(nlfile), status='old')
       call find_group_name(unitn, 'drydep_input_nl', status=errflg)
       if (errflg == 0) then
          read(unitn, drydep_input_nl, iostat=errflg, iomsg=errmsg)
          if (errflg /= 0) then
             call endrun(subname // ':: ERROR reading namelist:' // errmsg)
          end if
       end if
       close(unitn)
    end if

    ! Broadcast namelist variables
    call mpi_bcast(drydep_srf_file, len(drydep_srf_file), mpi_character, 0, mpicom, errflg)

    ! Print out namelist variables
    if (masterproc) then
      write(iulog,*) subname, ' options:'
      if(drydep_srf_file /= unset_path_str) then
        write(iulog,*) '  Dry deposition surface dataset: ', trim(drydep_srf_file)
      else
        write(iulog,*) '  Dry deposition surface dataset unavailable; land use fractions set to zero.'
      endif
    endif
  end subroutine fraction_landuse_readnl

  subroutine fraction_landuse_read_file()
    use spmd_utils,     only: masterproc
    use cam_logfile,    only: iulog
    use cam_abortutils, only: endrun, check_allocate
    use pio,            only: file_desc_t, pio_nowrite
    use cam_pio_utils,  only: cam_pio_openfile, cam_pio_closefile
    use ioFileMod,      only: cam_get_file
    use cam_field_read, only: cam_read_field
    use cam_initfiles,  only: unset_path_str
    use physics_grid,   only: ncol => columns_on_task
    use phys_vars_init_check, only: mark_as_initialized

    ! Local variables
    type(file_desc_t)             :: fh_srf
    integer                       :: errflg
    character(len=512)            :: errmsg
    character(len=*), parameter   :: subname = 'fraction_landuse_read_file'

    logical                       :: found

    errmsg = ''
    errflg = 0

    call mark_as_initialized('number_of_land_use_classes')

    ! Allocate and initialize data to zeros.
    allocate(fraction_landuse(ncol, n_land_type), stat=errflg, errmsg=errmsg)
    call check_allocate(errflg, subname, 'fraction_landuse', errmsg=errmsg)
    fraction_landuse(:,:) = 0._kind_phys

    if(drydep_srf_file /= unset_path_str) then
      call cam_get_file(drydep_srf_file, drydep_srf_file_loc)
      call cam_pio_openfile(fh_srf, drydep_srf_file_loc, pio_nowrite)

      if(masterproc) then
        write (iulog,*) trim(subname)//': Reading land use class fractions from ', trim(drydep_srf_file_loc)
      endif

      ! Read required 2D field: fraction_landuse (ncol, class)
      call cam_read_field('fraction_landuse', fh_srf, fraction_landuse, found, &
                          dim3name='class', dim3_bnds=(/1, n_land_type/))
      if(.not. found) then
        call endrun(trim(subname) // ': fraction_landuse not found in drydep_srf_file')
      endif

      call cam_pio_closefile(fh_srf)
    else
      if(masterproc) then
        write(iulog,*) trim(subname) // ': drydep_srf_file not set; land use fractions set to zero.'
      endif
    endif

    ! Mark variable as initialized so it is not read from ic file.
    call mark_as_initialized('land_use_class_area_fraction')
  end subroutine fraction_landuse_read_file

end module fraction_landuse_read
