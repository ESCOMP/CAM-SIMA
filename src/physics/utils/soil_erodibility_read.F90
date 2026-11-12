! Support module for the MAM dust emissions scheme to read and regrid the
! Zender soil erodibility factor dataset (namelist soil_erod_file).
!
! Remarks: this module is not CCPP-ized but is written specifically for
! host-side input in CAM-SIMA, following the fraction_landuse_read pattern:
! the field is provided to underlying CCPP schemes via the argument table
! below.
!
! In CAM this field is owned by soil_erod_mod (soil_erod_init), which reads
! the lat-lon erodibility dataset and horizontally interpolates it to the
! physics columns with interpolate_data's lininterp. CAM-SIMA's
! interpolate_data is the same code and the interpolation weights are
! computed per column, so the same read + interpolation on the same grid is
! bitwise identical to CAM's and the field needs no snapshot capture.
!
! If soil_erod_file is not set, the erodibility stays zero with a warning;
! dust emissions on the Zender-in-atmosphere path then vanish, so runs with
! zender_soil_erod_from_atm = .true. must provide the dataset (CAM aborts in
! getfil in that situation). The Leung_2023 path never uses this field.
module soil_erodibility_read
  use ccpp_kinds,   only: kind_phys
  use shr_kind_mod, only: shr_kind_cl

  implicit none
  private

  public :: soil_erodibility_readnl
  public :: soil_erodibility_read_file

  ! Soil erodibility dataset (rel pathname).
  character(len=shr_kind_cl) :: soil_erod_file     = 'UNSET_PATH'
  ! Resolved pathname.
  character(len=shr_kind_cl) :: soil_erod_file_loc = 'UNSET_PATH'

  ! Below data is provided externally to CCPP schemes.
!> \section arg_table_soil_erodibility_read  Argument Table
!! \htmlinclude soil_erodibility_read.html
  real(kind_phys), allocatable, public :: soil_erodibility(:) ! soil erodibility factor (ncol) [1]

contains

  subroutine soil_erodibility_readnl(nlfile)
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
    character(len=*), parameter  :: subname = 'soil_erodibility_readnl'
    character(len=shr_kind_cm)   :: errmsg

    namelist /dust_input_nl/ soil_erod_file

    errmsg = ''
    errflg = 0

    if (masterproc) then
       open(newunit=unitn, file=trim(nlfile), status='old')
       call find_group_name(unitn, 'dust_input_nl', status=errflg)
       if (errflg == 0) then
          read(unitn, dust_input_nl, iostat=errflg, iomsg=errmsg)
          if (errflg /= 0) then
             call endrun(subname // ':: ERROR reading namelist:' // errmsg)
          end if
       end if
       close(unitn)
    end if

    ! Broadcast namelist variables
    call mpi_bcast(soil_erod_file, len(soil_erod_file), mpi_character, 0, mpicom, errflg)

    ! Print out namelist variables
    if (masterproc) then
      write(iulog,*) subname, ' options:'
      if(soil_erod_file /= unset_path_str) then
        write(iulog,*) '  Soil erodibility dataset: ', trim(soil_erod_file)
      else
        write(iulog,*) '  Soil erodibility dataset unavailable; soil erodibility set to zero.'
      endif
    endif
  end subroutine soil_erodibility_readnl

  subroutine soil_erodibility_read_file()
    use spmd_utils,     only: masterproc
    use cam_logfile,    only: iulog
    use cam_abortutils, only: check_allocate
    use pio,            only: file_desc_t, pio_nowrite, pio_inq_dimid, &
                              pio_inq_dimlen, pio_inq_varid, pio_get_var
    use cam_pio_utils,  only: cam_pio_openfile, cam_pio_closefile
    use ioFileMod,      only: cam_get_file
    use cam_initfiles,  only: unset_path_str
    use physics_grid,   only: ncol => columns_on_task, &
                              get_rlat_all_p, get_rlon_all_p
    use phys_vars_init_check, only: mark_as_initialized
    use interpolate_data, only: lininterp_init, lininterp, lininterp_finish, &
                                interp_type
    use physconst,      only: pi

    ! Local variables
    type(file_desc_t)             :: ncid
    integer                       :: errflg
    character(len=512)            :: errmsg
    character(len=*), parameter   :: subname = 'soil_erodibility_read_file'

    real(kind_phys), allocatable  :: soil_erodibility_in(:,:)  ! temporary input array
    real(kind_phys), allocatable  :: dst_lons(:)
    real(kind_phys), allocatable  :: dst_lats(:)
    integer                       :: did, vid, nlat, nlon
    integer                       :: ierr

    type(interp_type)             :: lon_wgts, lat_wgts
    real(kind_phys)               :: to_lats(ncol), to_lons(ncol)
    ! CAM soil_erod_init takes d2r/twopi from mo_constants, whose pi is
    ! physconst's; formed the same way here so the interpolation weights
    ! match bitwise.
    real(kind_phys), parameter    :: zero = 0._kind_phys
    real(kind_phys)               :: d2r, twopi

    errmsg = ''
    errflg = 0

    d2r   = pi/180._kind_phys
    twopi = 2._kind_phys*pi

    ! Allocate and initialize data to zeros.
    allocate(soil_erodibility(ncol), stat=errflg, errmsg=errmsg)
    call check_allocate(errflg, subname, 'soil_erodibility', errmsg=errmsg)
    soil_erodibility(:) = 0._kind_phys

    if(soil_erod_file /= unset_path_str) then
      call cam_get_file(soil_erod_file, soil_erod_file_loc)
      call cam_pio_openfile(ncid, soil_erod_file_loc, pio_nowrite)

      if(masterproc) then
        write (iulog,*) trim(subname)//': Reading soil erodibility factor from ', trim(soil_erod_file_loc)
      endif

      ! Get input data resolution (CAM soil_erod_init; the replicated global
      ! read of the lat-lon dataset, then per-column interpolation).
      ierr = pio_inq_dimid( ncid, 'lon', did )
      ierr = pio_inq_dimlen( ncid, did, nlon )

      ierr = pio_inq_dimid( ncid, 'lat', did )
      ierr = pio_inq_dimlen( ncid, did, nlat )

      allocate(dst_lons(nlon), stat=errflg, errmsg=errmsg)
      call check_allocate(errflg, subname, 'dst_lons', errmsg=errmsg)
      allocate(dst_lats(nlat), stat=errflg, errmsg=errmsg)
      call check_allocate(errflg, subname, 'dst_lats', errmsg=errmsg)
      allocate(soil_erodibility_in(nlon,nlat), stat=errflg, errmsg=errmsg)
      call check_allocate(errflg, subname, 'soil_erodibility_in', errmsg=errmsg)

      ierr = pio_inq_varid( ncid, 'lon', vid )
      ierr = pio_get_var( ncid, vid, dst_lons  )

      ierr = pio_inq_varid( ncid, 'lat', vid )
      ierr = pio_get_var( ncid, vid, dst_lats  )

      ierr = pio_inq_varid( ncid, 'mbl_bsn_fct_geo', vid )
      ierr = pio_get_var( ncid, vid, soil_erodibility_in )

      ! ... convert to radians and setup regridding
      dst_lats(:) = d2r * dst_lats(:)
      dst_lons(:) = d2r * dst_lons(:)

      ! ... regrid (CAM loops over chunks; the weights and interpolation are
      ! per column, so the single columns-on-task pass is bitwise the same)
      call get_rlat_all_p(ncol, to_lats)
      call get_rlon_all_p(ncol, to_lons)

      call lininterp_init(dst_lons, nlon, to_lons, ncol, 2, lon_wgts, zero, twopi)
      call lininterp_init(dst_lats, nlat, to_lats, ncol, 1, lat_wgts)

      call lininterp(soil_erodibility_in(:,:), nlon, nlat, soil_erodibility(:), ncol, lon_wgts, lat_wgts)

      call lininterp_finish(lat_wgts)
      call lininterp_finish(lon_wgts)

      deallocate(soil_erodibility_in)
      deallocate(dst_lats)
      deallocate(dst_lons)

      call cam_pio_closefile(ncid)
    else
      if(masterproc) then
        write(iulog,*) trim(subname) // ': soil_erod_file not set; soil erodibility set to zero.'
      endif
    endif

    ! Mark variable as initialized so it is not read from ic file.
    call mark_as_initialized('soil_erodibility_factor')
  end subroutine soil_erodibility_read_file

end module soil_erodibility_read
