module atm_import_export

  use NUOPC             , only : NUOPC_CompAttributeGet, NUOPC_Advertise, NUOPC_IsConnected
  use NUOPC_Model       , only : NUOPC_ModelGet
  use ESMF              , only : ESMF_GridComp, ESMF_State, ESMF_Mesh, ESMF_StateGet, ESMF_Field
  use ESMF              , only : ESMF_SUCCESS, ESMF_LOGMSG_INFO
  use ESMF              , only : ESMF_LogWrite
  use ESMF              , only : operator(/=), operator(==)
  use shr_kind_mod      , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs, cx=>shr_kind_cx

  implicit none
  private ! except

  public  :: read_surface_fields_namelists
  public  :: advertise_fields
  public  :: realize_fields
  public  :: import_fields
  public  :: export_fields

  private :: fldlist_add
  private :: fldlist_realize
  private :: state_getfldptr

  type fldlist_type
     character(len=128) :: stdname
     integer :: ungridded_lbound = 0
     integer :: ungridded_ubound = 0
  end type fldlist_type

  integer             , parameter         :: fldsMax = 100
  integer             , public, protected :: fldsToAtm_num = 0
  integer             , public, protected :: fldsFrAtm_num = 0
  type (fldlist_type) , public, protected :: fldsToAtm(fldsMax)
  type (fldlist_type) , public, protected :: fldsFrAtm(fldsMax)

  ! area correction factors for fluxes send and received from mediator
  real(r8), allocatable :: mod2med_areacor(:)
  real(r8), allocatable :: med2mod_areacor(:)

  character(len=cx)      :: carma_fields = ' '      ! list of CARMA fields from lnd->atm
  integer                :: drydep_nflds = -huge(1) ! number of dry deposition velocity fields lnd-> atm
  integer                :: megan_nflds = -huge(1)  ! number of MEGAN voc fields from lnd-> atm
  integer                :: emis_nflds = -huge(1)   ! number of fire emission fields from lnd-> atm
  integer, public        :: ndep_nflds = -huge(1)   ! number of nitrogen deposition fields from atm->lnd/ocn
  logical                :: atm_provides_lightning = .false. ! cld to grnd lightning flash freq (min-1)
  character(*),parameter :: F01 = "('(cam_import_export) ',a,i8,2x,i8,2x,d21.14)"
  character(*),parameter :: F02 = "('(cam_import_export) ',a,i8,2x,i8,2x,i8,2x,d21.14)"
  character(*),parameter :: u_FILE_u = __FILE__

!===============================================================================
contains
!===============================================================================

  !-----------------------------------------------------------
  ! read mediator fields namelist file
  !-----------------------------------------------------------
  subroutine read_surface_fields_namelists()

    use shr_drydep_mod    , only : shr_drydep_readnl
    use shr_megan_mod     , only : shr_megan_readnl
    use shr_fire_emis_mod , only : shr_fire_emis_readnl
    use shr_carma_mod     , only : shr_carma_readnl
    use shr_ndep_mod      , only : shr_ndep_readnl
    use shr_lightning_coupling_mod, only : shr_lightning_coupling_readnl

    character(len=*), parameter :: nl_file_name = 'drv_flds_in'

    ! read mediator fields options
    call shr_ndep_readnl(nl_file_name, ndep_nflds)
    call shr_drydep_readnl(nl_file_name, drydep_nflds)
    call shr_megan_readnl(nl_file_name, megan_nflds)
    call shr_fire_emis_readnl(nl_file_name, emis_nflds)
    call shr_carma_readnl(nl_file_name, carma_fields)
    call shr_lightning_coupling_readnl(nl_file_name, atm_provides_lightning)

  end subroutine read_surface_fields_namelists

  !-----------------------------------------------------------
  ! advertise fields
  !-----------------------------------------------------------
  subroutine advertise_fields(gcomp, flds_scalar_name, rc)

    ! use statements
    use ESMF              , only : ESMF_MAXSTR
    use nuopc_shr_methods , only : chkerr
    use srf_field_check   , only : set_active_Sl_ram1
    use srf_field_check   , only : set_active_Sl_fv
    use srf_field_check   , only : set_active_Sl_soilw
    use srf_field_check   , only : set_active_Fall_flxdst1
    use srf_field_check   , only : set_active_Fall_flxvoc
    use srf_field_check   , only : set_active_Fall_flxfire
    use srf_field_check   , only : set_active_Fall_fco2_lnd
    use srf_field_check   , only : set_active_Faoo_fco2_ocn
    use srf_field_check   , only : set_active_Faxa_nhx
    use srf_field_check   , only : set_active_Faxa_noy
    use spmd_utils        , only : masterproc
    use cam_logfile       , only : iulog

    ! input/output variables
    type(ESMF_GridComp)            :: gcomp
    character(len=*) , intent(in)  :: flds_scalar_name
    integer          , intent(out) :: rc

    ! local variables
    type(ESMF_State)       :: importState
    type(ESMF_State)       :: exportState
    character(ESMF_MAXSTR) :: stdname
    character(ESMF_MAXSTR) :: cvalue
    integer                :: n, num
    logical                :: flds_co2a      ! use case
    logical                :: flds_co2b      ! use case
    logical                :: flds_co2c      ! use case
    character(len=128)     :: fldname
    character(len=*), parameter :: subname='(atm_import_export:advertise_fields)'
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call NUOPC_ModelGet(gcomp, importState=importState, exportState=exportState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !--------------------------------
    ! determine necessary toggles for below
    !--------------------------------

    call NUOPC_CompAttributeGet(gcomp, name='flds_co2a', value=cvalue, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) flds_co2a
    if (masterproc) write(iulog,'(a)') trim(subname)//'flds_co2a = '// trim(cvalue)

    call NUOPC_CompAttributeGet(gcomp, name='flds_co2b', value=cvalue, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) flds_co2b
    if (masterproc) write(iulog,'(a)') trim(subname)//'flds_co2b = '// trim(cvalue)

    call NUOPC_CompAttributeGet(gcomp, name='flds_co2c', value=cvalue, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    read(cvalue,*) flds_co2c
    if (masterproc) write(iulog,'(a)') trim(subname)//'flds_co2c = '// trim(cvalue)

    !--------------------------------
    ! Export fields
    !--------------------------------

    if (masterproc) write(iulog,'(a)') trim(subname)//'export_fields '

    call fldlist_add(fldsFrAtm_num, fldsFrAtm, trim(flds_scalar_name))
    call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Sa_topo'       )
    call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Sa_z'          )
    call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Sa_u'          )
    call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Sa_v'          )
    call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Sa_tbot'       )
    call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Sa_ptem'       )
    call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Sa_shum'       )
    call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Sa_pbot'       )
    call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Sa_dens'       )
    call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Sa_pslv'       )
    call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Sa_o3'         )
    call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Faxa_rainc'    )
    call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Faxa_rainl'    )
    call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Faxa_snowc'    )
    call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Faxa_snowl'    )
    call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Faxa_lwdn'     )
    call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Faxa_swndr'    )
    call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Faxa_swvdr'    )
    call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Faxa_swndf'    )
    call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Faxa_swvdf'    )
    call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Faxa_swnet'    )  ! only diagnostic

    ! from atm - black carbon deposition fluxes (3)
    ! (1) => bcphidry, (2) => bcphodry, (3) => bcphiwet
    call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Faxa_bcph', ungridded_lbound=1, ungridded_ubound=3)

    ! from atm - organic carbon deposition fluxes (3)
    ! (1) => ocphidry, (2) => ocphodry, (3) => ocphiwet
    call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Faxa_ocph', ungridded_lbound=1, ungridded_ubound=3)

    ! from atm - wet dust deposition frluxes (4 sizes)
    ! (1) => dstwet1, (2) => dstwet2, (3) => dstwet3, (4) => dstwet4
    call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Faxa_dstwet', ungridded_lbound=1, ungridded_ubound=4)

    ! from atm - dry dust deposition frluxes (4 sizes)
    ! (1) => dstdry1, (2) => dstdry2, (3) => dstdry3, (4) => dstdry4
    call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Faxa_dstdry', ungridded_lbound=1, ungridded_ubound=4)

    call ESMF_LogWrite(subname//' export fields co2', ESMF_LOGMSG_INFO)

    ! from atm co2 fields
    if (flds_co2a .or. flds_co2b .or. flds_co2c) then
       call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Sa_co2prog' )
       call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Sa_co2diag' )
    end if

    if (ndep_nflds > 0) then
       ! The following is when CAM/WACCM computes ndep
       call set_active_Faxa_nhx(.true.)
       call set_active_Faxa_noy(.true.)
    else
       ! The following is used for reading in stream data
       call set_active_Faxa_nhx(.false.)
       call set_active_Faxa_noy(.false.)
    end if
    ! Assume that 2 fields are always sent as part of Faxa_ndep
    call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Faxa_ndep', ungridded_lbound=1, ungridded_ubound=2)

    ! lightning flash freq
    if (atm_provides_lightning) then
       call fldlist_add(fldsFrAtm_num, fldsFrAtm, 'Sa_lightning')
    end if

    ! Now advertise above export fields
    if (masterproc) write(iulog,*) trim(subname)//' advertise export fields'
    do n = 1,fldsFrAtm_num
       call NUOPC_Advertise(exportState, standardName=fldsFrAtm(n)%stdname, &
            TransferOfferGeomObject='will provide', rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    enddo

    !-----------------
    ! Import fields
    !-----------------

    if (masterproc) write(iulog,'(a)') trim(subname)//' import fields '

    call fldlist_add(fldsToAtm_num, fldsToAtm, trim(flds_scalar_name))
    call fldlist_add(fldsToAtm_num, fldsToAtm, 'Sx_anidr'  )
    call fldlist_add(fldsToAtm_num, fldsToAtm, 'Sx_avsdf'  )
    call fldlist_add(fldsToAtm_num, fldsToAtm, 'Sx_anidf'  )
    call fldlist_add(fldsToAtm_num, fldsToAtm, 'Sx_avsdr'  )
    call fldlist_add(fldsToAtm_num, fldsToAtm, 'Sl_lfrac'  )
    call fldlist_add(fldsToAtm_num, fldsToAtm, 'Si_ifrac'  )
    call fldlist_add(fldsToAtm_num, fldsToAtm, 'So_ofrac'  )
    call fldlist_add(fldsToAtm_num, fldsToAtm, 'Sx_tref'   )
    call fldlist_add(fldsToAtm_num, fldsToAtm, 'Sx_qref'   )
    call fldlist_add(fldsToAtm_num, fldsToAtm, 'Sx_t'      )
    call fldlist_add(fldsToAtm_num, fldsToAtm, 'So_t'      )
    call fldlist_add(fldsToAtm_num, fldsToAtm, 'Sl_fv'     );  call set_active_Sl_fv(.true.)
    call fldlist_add(fldsToAtm_num, fldsToAtm, 'Sl_ram1'   );  call set_active_Sl_ram1(.true.)
    call fldlist_add(fldsToAtm_num, fldsToAtm, 'Sl_snowh'  )
    call fldlist_add(fldsToAtm_num, fldsToAtm, 'Si_snowh'  )
    call fldlist_add(fldsToAtm_num, fldsToAtm, 'So_ssq'    )
    call fldlist_add(fldsToAtm_num, fldsToAtm, 'So_re'     )
    call fldlist_add(fldsToAtm_num, fldsToAtm, 'So_ustar'  )
    call fldlist_add(fldsToAtm_num, fldsToAtm, 'Sx_u10'    )
    call fldlist_add(fldsToAtm_num, fldsToAtm, 'So_ugustOut')
    call fldlist_add(fldsToAtm_num, fldsToAtm, 'So_u10withGust')
    call fldlist_add(fldsToAtm_num, fldsToAtm, 'Faxx_taux' )
    call fldlist_add(fldsToAtm_num, fldsToAtm, 'Faxx_tauy' )
    call fldlist_add(fldsToAtm_num, fldsToAtm, 'Faxx_lat'  )
    call fldlist_add(fldsToAtm_num, fldsToAtm, 'Faxx_sen'  )
    call fldlist_add(fldsToAtm_num, fldsToAtm, 'Faxx_lwup' )
    call fldlist_add(fldsToAtm_num, fldsToAtm, 'Faxx_evap' )

    ! dust fluxes from land (4 sizes)
    call fldlist_add(fldsToAtm_num, fldsToAtm, 'Fall_flxdst', ungridded_lbound=1, ungridded_ubound=4)
    call set_active_Fall_flxdst1(.true.)

    ! co2 fields from land and ocean
    if (flds_co2b .or. flds_co2c) then
       call fldlist_add(fldsToAtm_num, fldsToAtm, 'Fall_fco2_lnd')
       call set_active_Fall_fco2_lnd(.true.)
    end if
    if (flds_co2c) then
       call fldlist_add(fldsToAtm_num, fldsToAtm, 'Faoo_fco2_ocn')
       call set_active_Faoo_fco2_ocn(.true.)
    end if

    ! dry deposition velocities from land - ALSO initialize drydep here
    if (drydep_nflds > 0) then
       call fldlist_add(fldsToAtm_num, fldsToAtm, 'Sl_ddvel', ungridded_lbound=1, ungridded_ubound=drydep_nflds)
    end if

    ! MEGAN VOC emissions fluxes from land
    if (megan_nflds > 0) then
       call fldlist_add(fldsToAtm_num, fldsToAtm, 'Fall_voc', ungridded_lbound=1, ungridded_ubound=megan_nflds)
       call set_active_Fall_flxvoc(.true.)
    end if

    ! fire emissions fluxes from land
    if (emis_nflds > 0) then
       call fldlist_add(fldsToAtm_num, fldsToAtm, 'Fall_fire', ungridded_lbound=1, ungridded_ubound=emis_nflds)
       call fldlist_add(fldsToAtm_num, fldsToAtm, 'Sl_fztop')
       call set_active_Fall_flxfire(.true.)
    end if

    ! CARMA volumetric soil water from land
    if (carma_fields /= ' ') then
       call fldlist_add(fldsToAtm_num, fldsToAtm, 'Sl_soilw') ! optional for carma
       call set_active_Sl_soilw(.true.) ! check for carma
    end if

    ! ------------------------------------------
    ! Now advertise above import fields
    ! ------------------------------------------
    call ESMF_LogWrite(trim(subname)//' advertise import fields ', ESMF_LOGMSG_INFO)
    do n = 1,fldsToAtm_num
       call NUOPC_Advertise(importState, standardName=fldsToAtm(n)%stdname, &
            TransferOfferGeomObject='will provide', rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    enddo

  end subroutine advertise_fields

  !===============================================================================

  subroutine realize_fields(gcomp, Emesh, flds_scalar_name, flds_scalar_num, single_column, rc)

    use ESMF              , only : ESMF_MeshGet, ESMF_StateGet
    use ESMF              , only : ESMF_FieldRegridGetArea,ESMF_FieldGet
    use nuopc_shr_methods , only : chkerr
    use shr_mpi_mod       , only : shr_mpi_min, shr_mpi_max
    use spmd_utils        , only : masterproc, mpicom
    use physics_grid      , only : get_area_p
    use cam_abortutils    , only : check_allocate
    use cam_logfile       , only : iulog

    ! input/output variables
    type(ESMF_GridComp) , intent(inout) :: gcomp
    type(ESMF_Mesh)     , intent(in)    :: Emesh
    character(len=*)    , intent(in)    :: flds_scalar_name
    integer             , intent(in)    :: flds_scalar_num
    logical             , intent(in)    :: single_column
    integer             , intent(out)   :: rc

    ! local variables
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Field)      :: lfield
    integer               :: numOwnedElements
    integer               :: n, ierr
    real(r8), allocatable :: mesh_areas(:)
    real(r8), pointer     :: dataptr(:)
    real(r8)              :: max_mod2med_areacor
    real(r8)              :: max_med2mod_areacor
    real(r8)              :: min_mod2med_areacor
    real(r8)              :: min_med2mod_areacor
    real(r8)              :: max_mod2med_areacor_glob
    real(r8)              :: max_med2mod_areacor_glob
    real(r8)              :: min_mod2med_areacor_glob
    real(r8)              :: min_med2mod_areacor_glob
    character(len=cl)     :: cvalue
    character(len=cl)     :: mesh_atm
    character(len=cl)     :: mesh_lnd
    character(len=cl)     :: mesh_ocn
    logical               :: samegrid_atm_lnd_ocn
    character(len=*), parameter :: subname='(atm_import_export:realize_fields)'
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    call ESMF_LogWrite(subname//' called', ESMF_LOGMSG_INFO)

    call NUOPC_ModelGet(gcomp, importState=importState, exportState=exportState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call fldlist_realize( &
         state=ExportState, &
         fldList=fldsFrAtm, &
         numflds=fldsFrAtm_num, &
         flds_scalar_name=flds_scalar_name, &
         flds_scalar_num=flds_scalar_num, &
         tag=subname//':camExport',&
         mesh=Emesh, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call fldlist_realize( &
         state=importState, &
         fldList=fldsToAtm, &
         numflds=fldsToAtm_num, &
         flds_scalar_name=flds_scalar_name, &
         flds_scalar_num=flds_scalar_num, &
         tag=subname//':camImport',&
         mesh=Emesh, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Determine if atm/lnd/ocn are on the same grid - if so set area correction factors to 1
    call NUOPC_CompAttributeGet(gcomp, name='mesh_atm', value=mesh_atm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompAttributeGet(gcomp, name='mesh_lnd', value=mesh_lnd, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompAttributeGet(gcomp, name='mesh_ocn', value=mesh_ocn, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    samegrid_atm_lnd_ocn = .false.
    if ( trim(mesh_lnd) /= 'UNSET' .and. trim(mesh_atm) == trim(mesh_lnd) .and. &
         trim(mesh_ocn) /= 'UNSET' .and. trim(mesh_atm) == trim(mesh_ocn)) then
       samegrid_atm_lnd_ocn = .true.
    elseif ( trim(mesh_lnd) == 'UNSET' .and. trim(mesh_atm) == trim(mesh_ocn)) then
       samegrid_atm_lnd_ocn = .true.
    elseif ( trim(mesh_ocn) == 'UNSET' .and. trim(mesh_atm) == trim(mesh_lnd)) then
       samegrid_atm_lnd_ocn = .true.
    end if

    ! allocate area correction factors
    call ESMF_MeshGet(Emesh, numOwnedElements=numOwnedElements, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    allocate (mod2med_areacor(numOwnedElements), stat=ierr)
    call check_allocate(ierr, subname, 'mod2med_areacor(numOwnedElements)', &
                        file=__FILE__, line=__LINE__)
    allocate (med2mod_areacor(numOwnedElements), stat=ierr)
    call check_allocate(ierr, subname, 'med2mod_areacor(numOwnedElements)', &
                        file=__FILE__, line=__LINE__)

    if (single_column .or. samegrid_atm_lnd_ocn) then

       mod2med_areacor(:) = 1._r8
       med2mod_areacor(:) = 1._r8

    else

       ! Determine areas for regridding
       call ESMF_StateGet(exportState, itemName=trim(fldsFrAtm(2)%stdname), field=lfield, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_FieldRegridGetArea(lfield, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       call ESMF_FieldGet(lfield, farrayPtr=dataptr, rc=rc)
       if (chkerr(rc,__LINE__,u_FILE_u)) return
       allocate(mesh_areas(numOwnedElements), stat=ierr)
       call check_allocate(ierr, subname, 'mesh_areas(numOwnedElements)', &
                        file=__FILE__, line=__LINE__)
       mesh_areas(:) = dataptr(:)

       ! Determine flux correction factors (module variables)
       do n = 1,numOwnedElements
          mod2med_areacor(n) = get_area_p(n) / mesh_areas(n)
          med2mod_areacor(n) = 1._r8 / mod2med_areacor(n)
       end do
       deallocate(mesh_areas)

    end if

    min_mod2med_areacor = minval(mod2med_areacor)
    max_mod2med_areacor = maxval(mod2med_areacor)
    min_med2mod_areacor = minval(med2mod_areacor)
    max_med2mod_areacor = maxval(med2mod_areacor)
    call shr_mpi_max(max_mod2med_areacor, max_mod2med_areacor_glob, mpicom)
    call shr_mpi_min(min_mod2med_areacor, min_mod2med_areacor_glob, mpicom)
    call shr_mpi_max(max_med2mod_areacor, max_med2mod_areacor_glob, mpicom)
    call shr_mpi_min(min_med2mod_areacor, min_med2mod_areacor_glob, mpicom)

    if (masterproc) then
       write(iulog,'(2A,2g23.15,A )') trim(subname),' :  min_mod2med_areacor, max_mod2med_areacor ',&
            min_mod2med_areacor_glob, max_mod2med_areacor_glob, 'CAM'
       write(iulog,'(2A,2g23.15,A )') trim(subname),' :  min_med2mod_areacor, max_med2mod_areacor ',&
            min_med2mod_areacor_glob, max_med2mod_areacor_glob, 'CAM'
    end if

    call ESMF_LogWrite(trim(subname)//' done', ESMF_LOGMSG_INFO)

  end subroutine realize_fields

  !===============================================================================

  subroutine import_fields( gcomp, cam_in, restart_init, rc)

    ! -----------------------------------------------------
    ! Set field pointers in import state and
    ! copy from field pointer to CAM-SIMA array data structure
    ! -----------------------------------------------------

    use physics_types     , only : cam_in_t
    use shr_const_mod     , only : shr_const_stebol
    use shr_sys_mod       , only : shr_sys_abort
    use nuopc_shr_methods , only : chkerr
!CAM-SIMA NOTE: Need to uncomment these once carbon cycling is enabled in SIMA.
!    use co2_cycle         , only : c_i, co2_readFlux_ocn, co2_readFlux_fuel
!    use co2_cycle         , only : co2_transport, co2_time_interp_ocn, co2_time_interp_fuel
!    use co2_cycle         , only : data_flux_ocn, data_flux_fuel
    use physconst         , only : mwco2
    use time_manager      , only : is_first_step, get_nstep
    use physics_grid      , only : columns_on_task
    use runtime_obj       , only : wv_stdname
    use ccpp_scheme_utils , only : ccpp_constituent_index

    ! input/output variabes
    type(ESMF_GridComp)               :: gcomp
    type(cam_in_t)    , intent(inout) :: cam_in
    logical, optional , intent(in)    :: restart_init
    integer           , intent(out)   :: rc

    ! local variables
    type(ESMF_State)   :: importState
    integer            :: i,n  ! loop indices
    integer            :: ierr
    integer            :: nstep
    integer            :: wv_const_index
    logical            :: overwrite_flds
    logical            :: exists
    logical            :: exists_fco2_ocn
    logical            :: exists_fco2_lnd
    character(len=128) :: fldname
    character(len=512) :: errmsg
    real(r8), pointer  :: fldptr2d(:,:)
    real(r8), pointer  :: fldptr1d(:)
    real(r8), pointer  :: fldptr_lat(:)
    real(r8), pointer  :: fldptr_lwup(:)
    real(r8), pointer  :: fldptr_avsdr(:)
    real(r8), pointer  :: fldptr_anidr(:)
    real(r8), pointer  :: fldptr_avsdf(:)
    real(r8), pointer  :: fldptr_anidf(:)
    real(r8), pointer  :: fldptr_tsurf(:)
    real(r8), pointer  :: fldptr_tocn(:)
    real(r8), pointer  :: fldptr_tref(:)
    real(r8), pointer  :: fldptr_qref(:)
    real(r8), pointer  :: fldptr_u10(:)
    real(r8), pointer  :: fldptr_snowhland(:)
    real(r8), pointer  :: fldptr_snowhice(:)
    real(r8), pointer  :: fldptr_ifrac(:)
    real(r8), pointer  :: fldptr_ofrac(:)
    real(r8), pointer  :: fldptr_lfrac(:)
    real(r8), pointer  :: fldptr_taux(:)
    real(r8), pointer  :: fldptr_tauy(:)
    real(r8), pointer  :: fldptr_sen(:)
    real(r8), pointer  :: fldptr_evap(:)
    logical, save      :: first_time = .true.
    character(len=*), parameter :: subname='(atm_import_export:import_fields)'
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Get import state
    call NUOPC_ModelGet(gcomp, importState=importState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! don't overwrite fields if invoked during the initialization phase
    ! of a 'continue' or 'branch' run type with data from .rs file
    overwrite_flds = .true.
    if (present(restart_init)) overwrite_flds = .not. restart_init

    ! Find CCPP constituents index for water vapor,
    ! as it is needed to properly pass evaporation into
    ! the constituent fluxes array:
    call ccpp_constituent_index(wv_stdname, wv_const_index, ierr, errmsg)
    if (ierr /= 0) then
       call shr_sys_abort(subname//':: Failed to get water vapor CCPP constituent index with the following error: '//errmsg)
    end if

    !--------------------------
    ! Required atmosphere input fields
    !--------------------------

    if (overwrite_flds) then
       call state_getfldptr(importState, 'Faxx_taux', fldptr=fldptr_taux, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(importState, 'Faxx_tauy', fldptr=fldptr_tauy, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(importState, 'Faxx_sen' , fldptr=fldptr_sen, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call state_getfldptr(importState, 'Faxx_evap', fldptr=fldptr_evap, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       do i = 1, columns_on_task
          cam_in%wsx(i)                     = -fldptr_taux(i) * med2mod_areacor(i)
          cam_in%wsy(i)                     = -fldptr_tauy(i) * med2mod_areacor(i)
          cam_in%shf(i)                     = -fldptr_sen(i)  * med2mod_areacor(i)
          !Add water vapor to constituent fluxes array if present:
          if (wv_const_index > 0) then
             cam_in%cflx(i, wv_const_index) = -fldptr_evap(i) * med2mod_areacor(i)
          end if
       end do
    end if  ! end of overwrite_flds

    call state_getfldptr(importState, 'Faxx_lat', fldptr=fldptr_lat, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(importState, 'Faxx_lwup', fldptr=fldptr_lwup, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(importState, 'Sx_avsdr', fldptr=fldptr_avsdr, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(importState, 'Sx_anidr', fldptr=fldptr_anidr, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(importState, 'Sx_avsdf', fldptr=fldptr_avsdf, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(importState, 'Sx_anidf', fldptr=fldptr_anidf, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(importState, 'Sx_t', fldptr=fldptr_tsurf, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(importState, 'So_t', fldptr=fldptr_tocn, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(importState, 'Sl_snowh', fldptr=fldptr_snowhland, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(importState, 'Si_snowh', fldptr=fldptr_snowhice,  rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(importState, 'Sx_tref', fldptr=fldptr_tref, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(importState, 'Sx_qref', fldptr=fldptr_qref, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(importState, 'Sx_u10', fldptr=fldptr_u10, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(importState, 'Si_ifrac', fldptr=fldptr_ifrac, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(importState, 'So_ofrac', fldptr=fldptr_ofrac, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(importState, 'Sl_lfrac', fldptr=fldptr_lfrac, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Only do area correction on fluxes
    do i = 1, columns_on_task
       cam_in%lhf(i)       = -fldptr_lat(i)  * med2mod_areacor(i)
       cam_in%lwup(i)      = -fldptr_lwup(i) * med2mod_areacor(i)
       cam_in%asdir(i)     =  fldptr_avsdr(i)
       cam_in%aldir(i)     =  fldptr_anidr(i)
       cam_in%asdif(i)     =  fldptr_avsdf(i)
       cam_in%aldif(i)     =  fldptr_anidf(i)
       cam_in%ts(i)        =  fldptr_tsurf(i)
       cam_in%sst(i)       =  fldptr_tocn(i)
       cam_in%tref(i)      =  fldptr_tref(i)
       cam_in%qref(i)      =  fldptr_qref(i)
       cam_in%u10(i)       =  fldptr_u10(i)
       cam_in%snowhland(i) =  fldptr_snowhland(i)
       cam_in%snowhice(i)  =  fldptr_snowhice(i)
       cam_in%icefrac(i)   =  fldptr_ifrac(i)
       cam_in%ocnfrac(i)   =  fldptr_ofrac(i)
       cam_in%landfrac(i)  =  fldptr_lfrac(i)
    end do

! Commented out until dimensions and usage are figured out
#if 0
    ! Optional fields

    call state_getfldptr(importState, 'Sl_ram1', fldptr=fldptr1d, exists=exists, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (exists) then
       if ( associated(cam_in%ram1) ) then
          do i = 1, columns_on_task
             cam_in%ram1(i) = fldptr1d(i)
          end do
       end if
    end if

    call state_getfldptr(importState, 'Sl_fv', fldptr=fldptr1d, exists=exists, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (exists) then
       if ( associated(cam_in%fv) ) then
          do i = 1, columns_on_task
             cam_in%fv(i) = fldptr1d(i)
          end do
       end if
    end if

    ! For CARMA - soil water from land
    call state_getfldptr(importState, 'Sl_soilw', fldptr=fldptr1d, exists=exists, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (exists) then
       if ( associated(cam_in%soilw)) then
          do i = 1, columns_on_task
             cam_in%soilw(i) = fldptr1d(i)
          end do
       end if
    end if

    ! dry deposition fluxes from land
    call state_getfldptr(importState, 'Fall_flxdst', fldptr2d=fldptr2d, exists=exists, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (exists) then
       if ( associated(cam_in%dstflx) ) then
          do i = 1, columns_on_task
             do n = 1, size(fldptr2d, dim=1)
                cam_in%dstflx(i,n) = fldptr2d(n,i) * med2mod_areacor(i)
             end do
          end do
       end if
    end if

    ! MEGAN VOC emis fluxes from land
    call state_getfldptr(importState, 'Fall_voc', fldptr2d=fldptr2d, exists=exists, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (exists) then
       if ( associated(cam_in%meganflx) ) then
          do i = 1, columns_on_task
             do n = 1, size(fldptr2d, dim=1)
                cam_in%meganflx(i,n) = fldptr2d(n,i) * med2mod_areacor(i)
             end do
          end do
       end if
    end if

    ! fire emission fluxes from land
    call state_getfldptr(importState, 'Fall_fire', fldptr2d=fldptr2d, exists=exists, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (exists) then
       if ( associated(cam_in%fireflx) .and. associated(cam_in%fireztop) ) then
          do i = 1, columns_on_task
             do n = 1, size(fldptr2d, dim=1)
                cam_in%fireflx(i,n) = fldptr2d(n,i) * med2mod_areacor(i)
             end do
          end do
       end if
    end if
    call state_getfldptr(importState, 'Sl_fztop', fldptr=fldptr1d, exists=exists, rc=rc)
    if (exists) then
       do i = 1, columns_on_task
          cam_in%fireztop(i) = fldptr1d(i)
       end do
    end if
#endif

#if 0
! Ignoring depvel for now as it has a problematic second dimension (number of dry deposited species)
! and it was determined that it probably will not be used in CAM-SIMA for some time
    ! dry dep velocities
    call state_getfldptr(importState, 'Sl_ddvel', fldptr2d=fldptr2d, exists=exists, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (exists) then
       do i = 1, columns_on_task
          do n = 1, size(fldptr2d, dim=1)
             cam_in%depvel(i,n) = fldptr2d(n,i)
          end do
       end do
    end if
#endif

#if 0
! Commented out until water isotopes or carbon ccle fluxe are implemented in CAM-SIMA
    ! fields needed to calculate water isotopes to ocean evaporation processes
    call state_getfldptr(importState,  'So_ustar', fldptr=fldptr1d, exists=exists, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (exists) then
       do i = 1, columns_on_task
          cam_in%ustar(i) = fldptr1d(i)
       end do
    end if
    call state_getfldptr(importState,  'So_re', fldptr=fldptr1d, exists=exists, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (exists) then
       do i = 1, columns_on_task
          cam_in%re(i)= fldptr1d(i)
       end do
    end if
    call state_getfldptr(importState,  'So_ssq', fldptr=fldptr1d, exists=exists, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (exists) then
       do i = 1, columns_on_task
          cam_in%ssq(i) = fldptr1d(i)
       end do
    end if

    call state_getfldptr(importState,  'So_ugustOut', fldptr=fldptr1d, exists=exists, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (exists) then
       do i = 1, columns_on_task
          cam_in%ugustOut(i) = fldptr1d(i)
       end do
    end if

    call state_getfldptr(importState,  'So_u10withGust', fldptr=fldptr1d, exists=exists, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (exists) then
       do i = 1, columns_on_task
          cam_in%u10withGusts(i) = fldptr1d(i)
       end do
    end if

    ! bgc scenarios
    call state_getfldptr(importState,  'Fall_fco2_lnd', fldptr=fldptr1d, exists=exists_fco2_lnd, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (exists_fco2_lnd) then
       do i = 1, columns_on_task
          cam_in%fco2_lnd(i) = -fldptr1d(i) * med2mod_areacor(i)
       end do
    end if
    call state_getfldptr(importState,  'Faoo_fco2_ocn', fldptr=fldptr1d, exists=exists_fco2_ocn, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (exists_fco2_ocn) then
       do i = 1, columns_on_task
          cam_in%fco2_ocn(i) = -fldptr1d(i) * med2mod_areacor(i)
       end do
    else
       ! Consistency check
       if (co2_readFlux_ocn) then
          call shr_sys_abort(subname // ':: co2_readFlux_ocn and x2a_Faoo_fco2_ocn cannot both be active')
       end if
    end if
    call state_getfldptr(importState,  'Faoo_dms_ocn', fldptr=fldptr1d, exists=exists, rc=rc)
    if (exists) then
       do i = 1, columns_on_task
          cam_in%fdms(i) = -fldptr1d(i) * med2mod_areacor(i)
       end do
    end if

    ! -----------------------------------
    ! Get total co2 flux from components,
    ! -----------------------------------

    ! Note - co2_transport determines if cam_in(c)%cflx(i,c_i(1:4)) is allocated

    if (co2_transport() .and. overwrite_flds) then

       ! Interpolate in time for flux data read in
       if (co2_readFlux_ocn) then
          call co2_time_interp_ocn
       end if
       if (co2_readFlux_fuel) then
          call co2_time_interp_fuel
       end if

       ! from ocn : data read in or from coupler or zero
       ! from fuel: data read in or zero
       ! from lnd : through coupler or zero
       ! all co2 fluxes in unit kgCO2/m2/s

       do i=1, columns_on_task

          ! co2 flux from ocn
          if (exists_fco2_ocn) then
             cam_in%cflx(i,c_i(1)) = cam_in%fco2_ocn(i)
          else if (co2_readFlux_ocn) then
             ! convert from molesCO2/m2/s to kgCO2/m2/s
             cam_in%cflx(i,c_i(1)) = &
                -data_flux_ocn%co2flx(i,c)*(1._r8- cam_in%landfrac(i))*mwco2*1.0e-3_r8
             else
                cam_in%cflx(i,c_i(1)) = 0._r8
             end if

             ! co2 flux from fossil fuel
             if (co2_readFlux_fuel) then
                cam_in%cflx(i,c_i(2)) = data_flux_fuel%co2flx(i)
             else
                cam_in%cflx(i,c_i(2)) = 0._r8
             end if

             ! co2 flux from land (cpl already multiplies flux by land fraction)
             if (exists_fco2_lnd) then
                cam_in%cflx(i,c_i(3)) = cam_in%fco2_lnd(i)
             else
                cam_in%cflx(i,c_i(3)) = 0._r8
             end if

             ! merged co2 flux
             cam_in%cflx(i,c_i(4)) = cam_in%cflx(i,c_i(1)) + cam_in%cflx(i,c_i(2)) + cam_in%cflx(i,c_i(3))
          end do
       end do

    end if
#endif

    ! if first step, determine longwave up flux from the surface temperature
    if (first_time) then
       if (is_first_step()) then
          do i=1, columns_on_task
             cam_in%lwup(i) = shr_const_stebol*(cam_in%ts(i)**4)
          end do
       end if
       first_time = .false.
    end if

  end subroutine import_fields

  !===============================================================================

  subroutine export_fields( gcomp, model_mesh, model_clock, cam_out, rc)

    ! -----------------------------------------------------
    ! Set field pointers in export set
    ! Copy from CAM-SIMA array data structure into state fldptr
    ! -----------------------------------------------------

    use ESMF              , only : ESMF_Clock
    use nuopc_shr_methods , only : chkerr
    use srf_field_check   , only : active_Faxa_nhx, active_Faxa_noy
    use physics_types     , only : cam_out_t
    use time_manager      , only : is_first_step, get_nstep
    use physics_grid      , only : columns_on_task
    use atm_stream_ndep   , only : stream_ndep_init, stream_ndep_interp
    use atm_stream_ndep   , only : stream_ndep_is_initialized

    !-------------------------------
    ! Pack the export state
    !-------------------------------

    ! input/output variables
    type(ESMF_GridComp)              :: gcomp
    type(ESMF_Mesh) , intent(in)     :: model_mesh
    type(ESMF_Clock), intent(in)     :: model_clock
    type(cam_out_t) , intent(inout)  :: cam_out
    integer         , intent(out)    :: rc

    ! local variables
    type(ESMF_State)  :: exportState
    integer           :: i ! index variable
    logical           :: exists
    real(r8)          :: scale_ndep
    ! 2d pointers
    real(r8), pointer :: fldptr_ndep(:,:)
    real(r8), pointer :: fldptr_bcph(:,:)  , fldptr_ocph(:,:)
    real(r8), pointer :: fldptr_dstwet(:,:), fldptr_dstdry(:,:)
    ! 1d pointers
    real(r8), pointer :: fldptr_soll(:)    , fldptr_sols(:)
    real(r8), pointer :: fldptr_solld(:)   , fldptr_solsd(:)
    real(r8), pointer :: fldptr_snowc(:)   , fldptr_snowl(:)
    real(r8), pointer :: fldptr_rainc(:)   , fldptr_rainl(:)
    real(r8), pointer :: fldptr_lwdn(:)    , fldptr_swnet(:)
    real(r8), pointer :: fldptr_topo(:)    , fldptr_zbot(:)
    real(r8), pointer :: fldptr_ubot(:)    , fldptr_vbot(:)
    real(r8), pointer :: fldptr_pbot(:)    , fldptr_tbot(:)
    real(r8), pointer :: fldptr_shum(:)    , fldptr_dens(:)
    real(r8), pointer :: fldptr_ptem(:)    , fldptr_pslv(:)
    real(r8), pointer :: fldptr_co2prog(:) , fldptr_co2diag(:)
    real(r8), pointer :: fldptr_ozone(:)
    real(r8), pointer :: fldptr_lght(:)
    character(len=*), parameter :: subname='(atm_import_export:export_fields)'
    !---------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Get export state
    call NUOPC_ModelGet(gcomp, exportState=exportState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! required export state variables
    call state_getfldptr(exportState, 'Sa_topo', fldptr=fldptr_topo, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'Sa_z'   , fldptr=fldptr_zbot, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'Sa_u'   , fldptr=fldptr_ubot, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'Sa_v'   , fldptr=fldptr_vbot, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'Sa_tbot', fldptr=fldptr_tbot, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'Sa_pbot', fldptr=fldptr_pbot, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'Sa_shum', fldptr=fldptr_shum, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'Sa_dens', fldptr=fldptr_dens, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'Sa_ptem', fldptr=fldptr_ptem, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'Sa_pslv', fldptr=fldptr_pslv, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    do i = 1, columns_on_task
       fldptr_topo(i) = cam_out%topo(i)
       fldptr_zbot(i) = cam_out%zbot(i)
       fldptr_ubot(i) = cam_out%ubot(i)
       fldptr_vbot(i) = cam_out%vbot(i)
       fldptr_pbot(i) = cam_out%pbot(i)
       fldptr_tbot(i) = cam_out%tbot(i)
       fldptr_shum(i) = cam_out%qbot(i,1)
       fldptr_dens(i) = cam_out%rho(i)
       fldptr_ptem(i) = cam_out%thbot(i)
       fldptr_pslv(i) = cam_out%psl(i)
    end do

    ! required export flux variables
    call state_getfldptr(exportState, 'Faxa_swnet', fldptr=fldptr_swnet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'Faxa_lwdn' , fldptr=fldptr_lwdn , rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'Faxa_rainc', fldptr=fldptr_rainc, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'Faxa_rainl', fldptr=fldptr_rainl, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'Faxa_snowc', fldptr=fldptr_snowc, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'Faxa_snowl', fldptr=fldptr_snowl, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'Faxa_swndr', fldptr=fldptr_soll, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'Faxa_swvdr', fldptr=fldptr_sols, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'Faxa_swndf', fldptr=fldptr_solld, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'Faxa_swvdf', fldptr=fldptr_solsd, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    do i = 1, columns_on_task
       fldptr_lwdn(i)  = cam_out%flwds(i) * mod2med_areacor(i)
       fldptr_swnet(i) = cam_out%netsw(i) * mod2med_areacor(i)
       fldptr_snowc(i) = cam_out%precsc(i)*1000._r8 * mod2med_areacor(i)
       fldptr_snowl(i) = cam_out%precsl(i)*1000._r8 * mod2med_areacor(i)
       fldptr_rainc(i) = (cam_out%precc(i) - cam_out%precsc(i))*1000._r8 * mod2med_areacor(i)
       fldptr_rainl(i) = (cam_out%precl(i) - cam_out%precsl(i))*1000._r8 * mod2med_areacor(i)
       fldptr_soll(i)  = cam_out%soll(i)  * mod2med_areacor(i)
       fldptr_sols(i)  = cam_out%sols(i)  * mod2med_areacor(i)
       fldptr_solld(i) = cam_out%solld(i) * mod2med_areacor(i)
       fldptr_solsd(i) = cam_out%solsd(i) * mod2med_areacor(i)
    end do

    ! aerosol deposition fluxes
    call state_getfldptr(exportState, 'Faxa_bcph', fldptr2d=fldptr_bcph, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'Faxa_ocph', fldptr2d=fldptr_ocph, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'Faxa_dstdry', fldptr2d=fldptr_dstdry, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call state_getfldptr(exportState, 'Faxa_dstwet', fldptr2d=fldptr_dstwet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    ! (1) => bcphidry, (2) => bcphodry, (3) => bcphiwet
    ! (1) => ocphidry, (2) => ocphodry, (3) => ocphiwet
    do i = 1, columns_on_task
       fldptr_bcph(1,i)   = cam_out%bcphidry(i) * mod2med_areacor(i)
       fldptr_bcph(2,i)   = cam_out%bcphodry(i) * mod2med_areacor(i)
       fldptr_bcph(3,i)   = cam_out%bcphiwet(i) * mod2med_areacor(i)
       fldptr_ocph(1,i)   = cam_out%ocphidry(i) * mod2med_areacor(i)
       fldptr_ocph(2,i)   = cam_out%ocphodry(i) * mod2med_areacor(i)
       fldptr_ocph(3,i)   = cam_out%ocphiwet(i) * mod2med_areacor(i)
       fldptr_dstdry(1,i) = cam_out%dstdry1(i)  * mod2med_areacor(i)
       fldptr_dstdry(2,i) = cam_out%dstdry2(i)  * mod2med_areacor(i)
       fldptr_dstdry(3,i) = cam_out%dstdry3(i)  * mod2med_areacor(i)
       fldptr_dstdry(4,i) = cam_out%dstdry4(i)  * mod2med_areacor(i)
       fldptr_dstwet(1,i) = cam_out%dstwet1(i)  * mod2med_areacor(i)
       fldptr_dstwet(2,i) = cam_out%dstwet2(i)  * mod2med_areacor(i)
       fldptr_dstwet(3,i) = cam_out%dstwet3(i)  * mod2med_areacor(i)
       fldptr_dstwet(4,i) = cam_out%dstwet4(i)  * mod2med_areacor(i)
    end do

    call state_getfldptr(exportState, 'Sa_o3', fldptr=fldptr_ozone, exists=exists, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (exists) then
       do i = 1, columns_on_task
          fldptr_ozone(i) = cam_out%ozone(i) ! atm ozone
       end do
    end if

    call state_getfldptr(exportState, 'Sa_lightning', fldptr=fldptr_lght, exists=exists, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (exists) then
       do i = 1, columns_on_task
          fldptr_lght(i) = cam_out%lightning_flash_freq(i) ! cloud-to-ground lightning flash frequency (/min)
       end do
    end if

    call state_getfldptr(exportState, 'Sa_co2prog', fldptr=fldptr_co2prog, exists=exists, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (exists) then
       do i = 1, columns_on_task
          fldptr_co2prog(i) = cam_out%co2prog(i) ! atm prognostic co2
       end do
    end if

    call state_getfldptr(exportState, 'Sa_co2diag', fldptr=fldptr_co2diag, exists=exists, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (exists) then
       do i = 1, columns_on_task
          fldptr_co2diag(i) = cam_out%co2diag(i) ! atm diagnostic co2
       end do
    end if

    ! If ndep fields are not computed in cam and must be obtained from the ndep input stream
    call state_getfldptr(exportState, 'Faxa_ndep', fldptr2d=fldptr_ndep, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (.not. active_Faxa_nhx .and. .not. active_Faxa_noy) then
       if (.not. stream_ndep_is_initialized) then
          call stream_ndep_init(model_mesh, model_clock, rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
          stream_ndep_is_initialized = .true.
       end if
       call stream_ndep_interp(cam_out, rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       ! NDEP read from forcing is expected to be in units of gN/m2/sec - but the mediator
       ! expects units of kgN/m2/sec
       scale_ndep = .001_r8
    else
       ! If waccm computes ndep, then its in units of kgN/m2/s - and the mediator expects
       ! units of kgN/m2/sec, so the following conversion needs to happen
       scale_ndep = 1._r8
    end if
    do i = 1, columns_on_task
       fldptr_ndep(1,i) = cam_out%nhx_nitrogen_flx(i) * scale_ndep * mod2med_areacor(i)
       fldptr_ndep(2,i) = cam_out%noy_nitrogen_flx(i) * scale_ndep * mod2med_areacor(i)
    end do

  end subroutine export_fields

  !===============================================================================

  subroutine fldlist_add(num, fldlist, stdname, ungridded_lbound, ungridded_ubound)

    ! use statements
    use ESMF , only : ESMF_LOGMSG_ERROR

    ! input/otuput variables
    integer            , intent(inout) :: num
    type(fldlist_type) , intent(inout) :: fldlist(:)
    character(len=*)   , intent(in)    :: stdname
    integer, optional  , intent(in)    :: ungridded_lbound
    integer, optional  , intent(in)    :: ungridded_ubound

    ! local variables
    character(len=*), parameter :: subname='(atm_import_export:fldlist_add)'
    !-------------------------------------------------------------------------------

    ! Set up a list of field information

    num = num + 1
    if (num > fldsMax) then
       call ESMF_LogWrite(trim(subname)//": ERROR num > fldsMax "//trim(stdname), &
            ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__)
       return
    endif
    fldlist(num)%stdname = trim(stdname)

    if (present(ungridded_lbound) .and. present(ungridded_ubound)) then
       fldlist(num)%ungridded_lbound = ungridded_lbound
       fldlist(num)%ungridded_ubound = ungridded_ubound
    end if

  end subroutine fldlist_add

  !===============================================================================

  subroutine fldlist_realize(state, fldList, numflds, flds_scalar_name, flds_scalar_num, mesh, tag, rc)

    use NUOPC             , only : NUOPC_IsConnected, NUOPC_Realize
    use ESMF              , only : ESMF_MeshLoc_Element, ESMF_FieldCreate, ESMF_TYPEKIND_R8
    use ESMF              , only : ESMF_Field, ESMF_State, ESMF_Mesh, ESMF_StateRemove
    use ESMF              , only : ESMF_LogFoundError, ESMF_LOGMSG_INFO, ESMF_SUCCESS
    use ESMF              , only : ESMF_LogWrite, ESMF_LOGERR_PASSTHRU
    use nuopc_shr_methods , only : chkerr
    use spmd_utils        , only : masterproc
    use cam_logfile       , only : iulog

    ! input/output variables
    type(ESMF_State)    , intent(inout) :: state
    type(fldlist_type) , intent(in)     :: fldList(:)
    integer             , intent(in)    :: numflds
    character(len=*)    , intent(in)    :: flds_scalar_name
    integer             , intent(in)    :: flds_scalar_num
    character(len=*)    , intent(in)    :: tag
    type(ESMF_Mesh)     , intent(in)    :: mesh
    integer             , intent(inout) :: rc

    ! local variables
    integer                :: n
    type(ESMF_Field)       :: field
    character(len=80)      :: stdname
    character(CL)          :: msg
    character(len=*),parameter  :: subname='(atm_import_export:fldlist_realize)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    do n = 1, numflds
       stdname = fldList(n)%stdname
       if (NUOPC_IsConnected(state, fieldName=stdname)) then
          if (stdname == trim(flds_scalar_name)) then
             if (masterproc) then
                write(iulog,'(a)') trim(subname)//trim(tag)//" field = "//trim(stdname)//" is connected on root pe"
             end if
             ! Create the scalar field
             call SetScalarField(field, flds_scalar_name, flds_scalar_num, rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
          else
             ! Create the field
             if (fldlist(n)%ungridded_lbound > 0 .and. fldlist(n)%ungridded_ubound > 0) then
                field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R8, name=stdname, meshloc=ESMF_MESHLOC_ELEMENT, &
                     ungriddedLbound=(/fldlist(n)%ungridded_lbound/), &
                     ungriddedUbound=(/fldlist(n)%ungridded_ubound/), &
                     gridToFieldMap=(/2/), rc=rc)
                if (ChkErr(rc,__LINE__,u_FILE_u)) return
                if (masterproc) then
                   write(iulog,'(a,i8,a,i8)') trim(subname)// trim(tag)//" Field = "//trim(stdname)// &
                        " is connected using mesh with lbound ", fldlist(n)%ungridded_lbound,&
                        " and with ubound ",fldlist(n)%ungridded_ubound
                end if
             else
                field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R8, name=stdname, meshloc=ESMF_MESHLOC_ELEMENT, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
                if (masterproc) then
                   write(iulog,'(a)') trim(subname)// trim(tag)//" Field = "//trim(stdname)// " is connected using mesh "
                end if
             end if
          endif

          ! NOW call NUOPC_Realize
          call NUOPC_Realize(state, field=field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
       else
          if (stdname /= trim(flds_scalar_name)) then
             if (masterproc) then
                write(iulog,'(a)')trim(subname)//trim(tag)//" Field = "//trim(stdname)//" is not connected"
             end if
             call ESMF_StateRemove(state, (/stdname/), rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return
          end if
       end if
    end do

  contains  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine SetScalarField(field, flds_scalar_name, flds_scalar_num, rc)
      ! ----------------------------------------------
      ! create a field with scalar data on the root pe
      ! ----------------------------------------------

      use ESMF, only : ESMF_Field, ESMF_DistGrid, ESMF_Grid
      use ESMF, only : ESMF_DistGridCreate, ESMF_GridCreate, ESMF_LogFoundError, ESMF_LOGERR_PASSTHRU
      use ESMF, only : ESMF_FieldCreate, ESMF_GridCreate, ESMF_TYPEKIND_R8

      ! input/output variables
      type(ESMF_Field) , intent(inout) :: field
      character(len=*) , intent(in)    :: flds_scalar_name
      integer          , intent(in)    :: flds_scalar_num
      integer          , intent(inout) :: rc

      ! local variables
      type(ESMF_Distgrid) :: distgrid
      type(ESMF_Grid)     :: grid
      character(len=*), parameter :: subname='(atm_import_export:SetScalarField)'
      ! ----------------------------------------------

      rc = ESMF_SUCCESS

      ! create a DistGrid with a single index space element, which gets mapped onto DE 0.
      distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/1/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

      grid = ESMF_GridCreate(distgrid, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

      field = ESMF_FieldCreate(name=trim(flds_scalar_name), grid=grid, typekind=ESMF_TYPEKIND_R8, &
           ungriddedLBound=(/1/), ungriddedUBound=(/flds_scalar_num/), gridToFieldMap=(/2/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=u_FILE_u)) return

    end subroutine SetScalarField

  end subroutine fldlist_realize

  !===============================================================================
  subroutine state_getfldptr(State, fldname, fldptr, fldptr2d, exists, rc)

    ! ----------------------------------------------
    ! Get pointer to a state field
    ! ----------------------------------------------

    use ESMF              , only : ESMF_State, ESMF_Field, ESMF_Mesh, ESMF_FieldStatus_Flag
    use ESMF              , only : ESMF_StateGet, ESMF_FieldGet, ESMF_MeshGet
    use ESMF              , only : ESMF_FIELDSTATUS_COMPLETE, ESMF_FAILURE
    use ESMF              , only : ESMF_STATEITEM_NOTFOUND, ESMF_StateItem_Flag
    use nuopc_shr_methods , only : chkerr

    ! input/output variables
    type(ESMF_State)  , intent(in)    :: State
    character(len=*)  , intent(in)    :: fldname
    real(R8), optional, pointer       :: fldptr(:)
    real(R8), optional, pointer       :: fldptr2d(:,:)
    logical , optional, intent(out)   :: exists
    integer           , intent(out)   :: rc

    ! local variables
    type(ESMF_FieldStatus_Flag) :: status
    type(ESMF_StateItem_Flag)   :: itemFlag
    type(ESMF_Field)            :: lfield
    type(ESMF_Mesh)             :: lmesh
    integer                     :: nnodes, nelements
    logical                     :: lexists
    character(len=*), parameter :: subname='(atm_import_export:state_getfldptr)'
    ! ----------------------------------------------

    rc = ESMF_SUCCESS

    lexists = .true.

    ! Determine if field with name fldname exists in state
    if (present(exists)) then
       call ESMF_StateGet(state, trim(fldname), itemFlag, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       if (itemflag == ESMF_STATEITEM_NOTFOUND) then
          lexists = .false.
       end if
       exists = lexists
    end if

    if (lexists) then
       call ESMF_StateGet(State, itemName=trim(fldname), field=lfield, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       if (present(fldptr)) then
          call ESMF_FieldGet(lfield, farrayPtr=fldptr, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       else if (present(fldptr2d)) then
          call ESMF_FieldGet(lfield, farrayPtr=fldptr2d, rc=rc)
          if (ChkErr(rc,__LINE__,u_FILE_u)) return
       end if
    end if

  end subroutine state_getfldptr

end module atm_import_export
