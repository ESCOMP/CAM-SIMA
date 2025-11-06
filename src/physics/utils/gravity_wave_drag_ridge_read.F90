! Support module for CAM-SIMA gravity_wave_drag_ridge parameterization
! to read in ridge data from the topo file.
!
! Remarks: this module is not CCPP-ized but is written specifically for
! grid decomposition-aware I/O in CAM-SIMA; it also has variables that
! are not a grid dimension (prdg = 16) that are read by specifying the
! custom dimension. It also makes that dimension available to the CCPP
! framework (prdg).
! This module can be a useful reference for how to provide
! gridded data to underlying CCPP schemes via the CAM PIO decomposition.
! (hplin, 8/28/25)
module gravity_wave_drag_ridge_read
  use ccpp_kinds,   only: kind_phys
  use shr_kind_mod, only: shr_kind_cl

  implicit none
  private

  public :: gravity_wave_drag_ridge_read_readnl
  public :: gravity_wave_drag_ridge_read_file

  ! Topography file (rel pathname) for meso-Beta ridge data.
  character(len=shr_kind_cl) :: bnd_topo      = 'UNSET_PATH'
  ! Resolved pathname.
  character(len=shr_kind_cl) :: bnd_topo_loc  = 'UNSET_PATH'

  ! Topography file (rel pathname) for meso-Gamma ridge data.
  character(len=shr_kind_cl) :: bnd_rdggm     = 'UNSET_PATH'
  ! Resolved pathname.
  character(len=shr_kind_cl) :: bnd_rdggm_loc = 'UNSET_PATH'

  ! Below data is provided externally to CCPP schemes.
!> \section arg_table_gravity_wave_drag_ridge_read  Argument Table
!! \htmlinclude gravity_wave_drag_ridge_read.html
  integer,         parameter,   public :: prdg = 16         ! # of ridges

  ! Meso-Beta ridges:
  real(kind_phys), allocatable, public :: rdg_gbxar (:)     ! Grid box area (ncol) [km2]
  real(kind_phys), allocatable, public :: rdg_isovar(:)     ! (B) Isotropic variance (ncol) [m]
  real(kind_phys), allocatable, public :: rdg_isowgt(:)     ! (B) Isotropic weight (ncol) [1]
  real(kind_phys), allocatable, public :: rdg_hwdth (:, :)  ! (B) Ridge Half-widths (ncol,prdg) [km]
  real(kind_phys), allocatable, public :: rdg_clngt (:, :)  ! (B) Ridge length (ncol,prdg) [km]
  real(kind_phys), allocatable, public :: rdg_mxdis (:, :)  ! (B) Ridge/obstacle height (ncol,prdg) [m]
  real(kind_phys), allocatable, public :: rdg_anixy (:, :)  ! (B) Ridge anisotropy (ncol,prdg) [1]
  real(kind_phys), allocatable, public :: rdg_angll (:, :)  ! (B) Ridge clockwise angle w.r.t. N-S direction (ncol,prdg) [degrees]

  ! Meso-Gamma ridges:
  real(kind_phys), allocatable, public :: rdg_gbxarg(:)     ! Grid box area (ncol) [km2]
  real(kind_phys), allocatable, public :: rdg_hwdthg(:, :)  ! (G) Ridge Half-widths (ncol,prdg) [km]
  real(kind_phys), allocatable, public :: rdg_clngtg(:, :)  ! (G) Ridge length (ncol,prdg) [km]
  real(kind_phys), allocatable, public :: rdg_mxdisg(:, :)  ! (G) Ridge/obstacle height (ncol,prdg) [m]
  real(kind_phys), allocatable, public :: rdg_anixyg(:, :)  ! (G) Ridge anisotropy (ncol,prdg) [1]
  real(kind_phys), allocatable, public :: rdg_angllg(:, :)  ! (G) Ridge clockwise angle w.r.t. N-S direction (ncol,prdg) [degrees]

contains

  subroutine gravity_wave_drag_ridge_read_readnl(nlfile)
    use shr_nl_mod,      only: find_group_name => shr_nl_find_group_name
    use shr_kind_mod,    only: shr_kind_cm
    use mpi,             only: mpi_character
    use spmd_utils,      only: mpicom
    use cam_logfile,     only: iulog
    use cam_abortutils,  only: endrun
    use spmd_utils,      only: masterproc
    use cam_initfiles,   only: unset_path_str

    ! topography dataset is available in this module:
    use cam_initfiles,   only: cam_bnd_topo => bnd_topo
    ! and does not need to be re-read from namelist.

    ! filepath for file containing namelist input
    character(len=*), intent(in) :: nlfile

    ! Local variables
    integer                      :: unitn, errflg
    character(len=*), parameter  :: subname = 'gravity_wave_drag_ridge_read_readnl'
    character(len=shr_kind_cm)   :: errmsg

    namelist /gw_drag_input_nl/ bnd_rdggm

    errmsg = ''
    errflg = 0

    if (masterproc) then
       open(newunit=unitn, file=trim(nlfile), status='old')
       call find_group_name(unitn, 'gw_drag_input_nl', status=errflg)
       if (errflg == 0) then
          read(unitn, gw_drag_input_nl, iostat=errflg, iomsg=errmsg)
          if (errflg /= 0) then
             call endrun(subname // ':: ERROR reading namelist:' // errmsg)
          end if
       end if
       close(unitn)
    end if

    ! Broadcast namelist variables
    call mpi_bcast(bnd_rdggm, len(bnd_rdggm), mpi_character, 0, mpicom, errflg)

    ! Retrieve topo file location:
    bnd_topo = cam_bnd_topo

    ! Print out namelist variables
    if (masterproc) then
      write(iulog,*) subname, ' options:'
      if(bnd_topo /= unset_path_str) then
        write(iulog,*) '  Gravity wave meso-Beta ridge topo file: ', trim(bnd_topo)
      else
        write(iulog,*) '  Gravity wave meso-Beta ridge input data unavailable.'
      endif

      if(bnd_rdggm /= unset_path_str) then
        write(iulog,*) '  Gravity wave meso-Gamma ridge input file: ', trim(bnd_rdggm)
      else
        write(iulog,*) '  Gravity wave meso-Gamma ridge input data unavailable.'
      endif
    endif
  end subroutine gravity_wave_drag_ridge_read_readnl

  subroutine gravity_wave_drag_ridge_read_file()
    use spmd_utils,     only: masterproc
    use cam_logfile,    only: iulog
    use cam_abortutils, only: endrun, check_allocate
    use pio,            only: file_desc_t, pio_nowrite
    use cam_pio_utils,  only: cam_pio_openfile, cam_pio_closefile
    use ioFileMod,      only: cam_get_file
    use cam_initfiles,  only: topo_file_get_id
    use cam_field_read, only: cam_read_field
    use physconst,      only: rearth
    use cam_initfiles,  only: unset_path_str
    use physics_grid,   only: ncol => columns_on_task
    use phys_vars_init_check, only: mark_as_initialized

    ! Local variables
    type(file_desc_t), pointer    :: fh_topo
    type(file_desc_t), pointer    :: fh_rdggm
    integer                       :: errflg
    character(len=512)            :: errmsg
    character(len=*), parameter   :: subname = 'gravity_wave_drag_ridge_read_file'

    logical                       :: found
    logical                       :: has_gbxar_from_topo

    errmsg = ''
    errflg = 0

    has_gbxar_from_topo = .false.
    call mark_as_initialized('number_of_ridges_in_ridge_gravity_wave_drag')

    ! Do we have meso-Beta file?
    if(bnd_topo /= unset_path_str) then
      call cam_get_file(bnd_topo, bnd_topo_loc)

      ! Try getting from initfiles first.
      fh_topo => topo_file_get_id()
      if(.not. associated(fh_topo)) then
        ! I think this case will never be hit in SIMA.
        ! There was a fallback in CAM.
        call endrun(trim(subname) // ": fh_topo from cam_initfiles is not available and this is not implemented.")
      endif

      if(masterproc) then
        write (iulog,*) trim(subname)//': Reading meso-Beta ridge data from ', trim(bnd_topo_loc)
      endif

      ! Allocate and initialize data to zeros.
      allocate(rdg_gbxar(ncol), stat=errflg, errmsg=errmsg)
      call check_allocate(errflg, subname, 'rdg_gbxar', errmsg=errmsg)
      allocate(rdg_isovar(ncol), stat=errflg, errmsg=errmsg)
      call check_allocate(errflg, subname, 'rdg_isovar', errmsg=errmsg)
      allocate(rdg_isowgt(ncol), stat=errflg, errmsg=errmsg)
      call check_allocate(errflg, subname, 'rdg_isowgt', errmsg=errmsg)
      allocate(rdg_hwdth(ncol, prdg), stat=errflg, errmsg=errmsg)
      call check_allocate(errflg, subname, 'rdg_hwdth', errmsg=errmsg)
      allocate(rdg_clngt(ncol, prdg), stat=errflg, errmsg=errmsg)
      call check_allocate(errflg, subname, 'rdg_clngt', errmsg=errmsg)
      allocate(rdg_mxdis(ncol, prdg), stat=errflg, errmsg=errmsg)
      call check_allocate(errflg, subname, 'rdg_mxdis', errmsg=errmsg)
      allocate(rdg_anixy(ncol, prdg), stat=errflg, errmsg=errmsg)
      call check_allocate(errflg, subname, 'rdg_anixy', errmsg=errmsg)
      allocate(rdg_angll(ncol, prdg), stat=errflg, errmsg=errmsg)
      call check_allocate(errflg, subname, 'rdg_angll', errmsg=errmsg)

      rdg_gbxar(:) = 0._kind_phys
      rdg_isovar(:) = 0._kind_phys
      rdg_isowgt(:) = 0._kind_phys
      rdg_hwdth(:,:) = 0._kind_phys
      rdg_clngt(:,:) = 0._kind_phys
      rdg_mxdis(:,:) = 0._kind_phys
      rdg_anixy(:,:) = 0._kind_phys
      rdg_angll(:,:) = 0._kind_phys

      ! Use cam_field_read to read the file data:

      ! Read required 1D field: GBXAR (grid box area)
      call cam_read_field('GBXAR', fh_topo, rdg_gbxar, found)
      if(.not. found) then
        call endrun(trim(subname) // ': GBXAR not found in input file')
      endif
      ! Convert from m2 to km2
      rdg_gbxar = rdg_gbxar * (rearth/1000._kind_phys) * (rearth/1000._kind_phys)
      has_gbxar_from_topo = .true.

      ! Read optional 1D field: ISOVAR (isotropic variance)
      call cam_read_field('ISOVAR', fh_topo, rdg_isovar, found)
      if(.not. found) then
        if(masterproc) then
          write(iulog,*) trim(subname) // ': ISOVAR not found in topo file, using zero values'
        endif
        ! rdg_isovar already initialized to zero above
      endif

      ! Read optional 1D field: ISOWGT (isotropic weight)
      call cam_read_field('ISOWGT', fh_topo, rdg_isowgt, found)
      if(.not. found) then
        if(masterproc) then
          write(iulog,*) trim(subname) // ': ISOWGT not found in topo file, using zero values'
        endif
        ! rdg_isowgt already initialized to zero above
      endif

      ! Read required 2D field: HWDTH (ridge half-widths)
      call cam_read_field('HWDTH', fh_topo, rdg_hwdth, found, dim3name='nrdg', dim3_bnds=(/1, prdg/))
      if(.not. found) then
        call endrun(trim(subname) // ': HWDTH not found in input file')
      endif

      ! Read required 2D field: CLNGT (ridge length)
      call cam_read_field('CLNGT', fh_topo, rdg_clngt, found, dim3name='nrdg', dim3_bnds=(/1, prdg/))
      if(.not. found) then
        call endrun(trim(subname) // ': CLNGT not found in input file')
      endif

      ! Read required 2D field: MXDIS (ridge/obstacle height)
      call cam_read_field('MXDIS', fh_topo, rdg_mxdis, found, dim3name='nrdg', dim3_bnds=(/1, prdg/))
      if(.not. found) then
        call endrun(trim(subname) // ': MXDIS not found in input file')
      endif

      ! Read required 2D field: ANIXY (ridge anisotropy)
      call cam_read_field('ANIXY', fh_topo, rdg_anixy, found, dim3name='nrdg', dim3_bnds=(/1, prdg/))
      if(.not. found) then
        call endrun(trim(subname) // ': ANIXY not found in input file')
      endif

      ! Read required 2D field: ANGLL (ridge clockwise angle w.r.t. N-S direction)
      call cam_read_field('ANGLL', fh_topo, rdg_angll, found, dim3name='nrdg', dim3_bnds=(/1, prdg/))
      if(.not. found) then
        call endrun(trim(subname) // ': ANGLL not found in input file')
      endif

      ! Mark variables as initialized so they are not read from ic file.
      call mark_as_initialized('grid_box_area_for_beta_ridge_gravity_wave_drag')
      call mark_as_initialized('isotropic_variance_for_beta_ridge_gravity_wave_drag')
      call mark_as_initialized('isotropic_weight_for_beta_ridge_gravity_wave_drag')
      call mark_as_initialized('ridge_half_width_for_beta_ridge_gravity_wave_drag')
      call mark_as_initialized('ridge_length_for_beta_ridge_gravity_wave_drag')
      call mark_as_initialized('ridge_obstacle_height_for_beta_ridge_gravity_wave_drag')
      call mark_as_initialized('ridge_anisotropy_for_beta_ridge_gravity_wave_drag')
      call mark_as_initialized('ridge_clockwise_angle_from_north_for_beta_ridge_gravity_wave_drag')
    endif

    ! Do we have meso-Gamma file?
    if(bnd_rdggm /= unset_path_str) then
      call cam_get_file(bnd_rdggm, bnd_rdggm_loc)
      call cam_pio_openfile(fh_rdggm, bnd_rdggm_loc, pio_nowrite)

      if(masterproc) then
        write (iulog,*) trim(subname)//': Reading meso-Gamma ridge data from ', trim(bnd_topo_loc)
      endif

      ! Allocate meso-Gamma ridge data arrays
      allocate(rdg_gbxarg(ncol), stat=errflg, errmsg=errmsg)
      call check_allocate(errflg, subname, 'rdg_gbxarg', errmsg=errmsg)
      allocate(rdg_hwdthg(ncol, prdg), stat=errflg, errmsg=errmsg)
      call check_allocate(errflg, subname, 'rdg_hwdthg', errmsg=errmsg)
      allocate(rdg_clngtg(ncol, prdg), stat=errflg, errmsg=errmsg)
      call check_allocate(errflg, subname, 'rdg_clngtg', errmsg=errmsg)
      allocate(rdg_mxdisg(ncol, prdg), stat=errflg, errmsg=errmsg)
      call check_allocate(errflg, subname, 'rdg_mxdisg', errmsg=errmsg)
      allocate(rdg_anixyg(ncol, prdg), stat=errflg, errmsg=errmsg)
      call check_allocate(errflg, subname, 'rdg_anixyg', errmsg=errmsg)
      allocate(rdg_angllg(ncol, prdg), stat=errflg, errmsg=errmsg)
      call check_allocate(errflg, subname, 'rdg_angllg', errmsg=errmsg)

      ! Initialize all gamma fields to zero first
      rdg_gbxarg(:) = 0._kind_phys
      rdg_hwdthg(:,:) = 0._kind_phys
      rdg_clngtg(:,:) = 0._kind_phys
      rdg_mxdisg(:,:) = 0._kind_phys
      rdg_anixyg(:,:) = 0._kind_phys
      rdg_angllg(:,:) = 0._kind_phys

      if(.not. has_gbxar_from_topo) then
        call cam_read_field('GBXAR', fh_rdggm, rdg_gbxarg, found)
        if(.not. found) then
          call endrun(trim(subname) // ': GBXAR not found in neither topo or gamma ridge file')
        endif
        ! Convert from m2 to km2
        rdg_gbxarg = rdg_gbxarg * (rearth/1000._kind_phys) * (rearth/1000._kind_phys)
      else
        if(masterproc) then
          write(iulog,*) trim(subname) // ': Using GBXAR from topo file, skipping gamma file GBXAR'
        endif
      endif

      ! ISOVAR and ISOWGT are intentionally not read from gamma file,
      ! because (1) they are unavailable, and (2) the original code did not read them in
      ! and left them as dangling pointers. See gw_drag_cam CAM interface,
      ! gravity_wave_drag_ridge_gamma_run implementation.

      ! Read required 2D field: HWDTH (ridge half-widths gamma)
      call cam_read_field('HWDTH', fh_rdggm, rdg_hwdthg, found, dim3name='nrdg', dim3_bnds=(/1, prdg/))
      if(.not. found) then
        call endrun(trim(subname) // ': HWDTH not found in gamma ridge file')
      endif

      ! Read required 2D field: CLNGT (ridge length gamma)
      call cam_read_field('CLNGT', fh_rdggm, rdg_clngtg, found, dim3name='nrdg', dim3_bnds=(/1, prdg/))
      if(.not. found) then
        call endrun(trim(subname) // ': CLNGT not found in gamma ridge file')
      endif

      ! Read required 2D field: MXDIS (ridge/obstacle height gamma)
      call cam_read_field('MXDIS', fh_rdggm, rdg_mxdisg, found, dim3name='nrdg', dim3_bnds=(/1, prdg/))
      if(.not. found) then
        call endrun(trim(subname) // ': MXDIS not found in gamma ridge file')
      endif

      ! Apply negative value correction for gamma ridge maximum displacement
      where (rdg_mxdisg < 0._kind_phys)
        rdg_mxdisg = 0._kind_phys
      end where

      ! Read required 2D field: ANIXY (ridge anisotropy gamma)
      call cam_read_field('ANIXY', fh_rdggm, rdg_anixyg, found, dim3name='nrdg', dim3_bnds=(/1, prdg/))
      if(.not. found) then
        call endrun(trim(subname) // ': ANIXY not found in gamma ridge file')
      endif

      ! Read required 2D field: ANGLL (ridge clockwise angle w.r.t. N-S direction gamma)
      call cam_read_field('ANGLL', fh_rdggm, rdg_angllg, found, dim3name='nrdg', dim3_bnds=(/1, prdg/))
      if(.not. found) then
        call endrun(trim(subname) // ': ANGLL not found in gamma ridge file')
      endif

      call cam_pio_closefile(fh_rdggm)
      deallocate(fh_rdggm)
      nullify(fh_rdggm)

      ! Mark variables as initialized so they are not read from ic file.
      call mark_as_initialized('grid_box_area_for_gamma_ridge_gravity_wave_drag')
      call mark_as_initialized('ridge_half_width_for_gamma_ridge_gravity_wave_drag')
      call mark_as_initialized('ridge_length_for_gamma_ridge_gravity_wave_drag')
      call mark_as_initialized('ridge_obstacle_height_for_gamma_ridge_gravity_wave_drag')
      call mark_as_initialized('ridge_anisotropy_for_gamma_ridge_gravity_wave_drag')
      call mark_as_initialized('ridge_clockwise_angle_from_north_for_gamma_ridge_gravity_wave_drag')
    endif
  end subroutine gravity_wave_drag_ridge_read_file

end module gravity_wave_drag_ridge_read
