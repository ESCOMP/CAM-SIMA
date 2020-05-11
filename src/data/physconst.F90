module physconst

! Physical constants.  Use csm_share values whenever available.

   use ccpp_kinds,   only: kind_phys

   use shr_const_mod,  only: shr_const_g,       shr_const_stebol
   use shr_const_mod,  only: shr_const_tkfrz,   shr_const_mwdair
   use shr_const_mod,  only: shr_const_rdair,   shr_const_mwwv
   use shr_const_mod,  only: shr_const_latice,  shr_const_latvap
   use shr_const_mod,  only: shr_const_cpdair,  shr_const_rhofw
   use shr_const_mod,  only: shr_const_cpwv,    shr_const_rgas
   use shr_const_mod,  only: shr_const_karman,  shr_const_pstd
   use shr_const_mod,  only: shr_const_rhodair, shr_const_avogad
   use shr_const_mod,  only: shr_const_boltz,   shr_const_cpfw
   use shr_const_mod,  only: shr_const_rwv,     shr_const_zvir
   use shr_const_mod,  only: shr_const_pi,      shr_const_rearth
   use shr_const_mod,  only: shr_const_sday,    shr_const_cday
   use shr_const_mod,  only: shr_const_spval,   shr_const_omega
   use shr_const_mod,  only: shr_const_cpvir,   shr_const_tktrip
   use shr_const_mod,  only: shr_const_cpice
   use physics_grid,   only: pcols => columns_on_task
   use phys_vert_coord,only: pver, pverp
   use cam_abortutils, only: endrun
   use constituents,   only: pcnst

   implicit none
   private
   save

   public  :: physconst_readnl
   public  :: physconst_init
   public  :: physconst_update
   public  :: physconst_calc_kappav

   !> \section arg_table_physconst  Argument Table
   !! \htmlinclude physconst.html
   ! Constants based off share code or defined in physconst
   ! Avogadro's number (molecules/kmole)
   real(kind_phys), public, parameter :: avogad      = real(shr_const_avogad, kind_phys)
   ! Boltzman's constant (J/K/molecule)
   real(kind_phys), public, parameter :: boltz       = real(shr_const_boltz, kind_phys)
   ! sec in calendar day ~ sec
   real(kind_phys), public, parameter :: cday        = real(shr_const_cday, kind_phys)
   ! specific heat of fresh h2o (J/K/kg)
   real(kind_phys), public, parameter :: cpliq       = real(shr_const_cpfw, kind_phys)
   ! specific heat of ice (J/K/kg)
   real(kind_phys), public, parameter :: cpice       = real(shr_const_cpice, kind_phys)
   ! Von Karman constant
   real(kind_phys), public, parameter :: karman      = real(shr_const_karman, kind_phys)
   ! Latent heat of fusion (J/kg)
   real(kind_phys), public, parameter :: latice      = real(shr_const_latice, kind_phys)
   ! Latent heat of vaporization (J/kg)
   real(kind_phys), public, parameter :: latvap      = real(shr_const_latvap, kind_phys)
   ! 3.14...
   real(kind_phys), public, parameter :: pi          = real(shr_const_pi, kind_phys)
   ! Standard pressure (Pascals)
   real(kind_phys), public, protected :: pstd        = real(shr_const_pstd, kind_phys)
   ! Reference temperature
   real(kind_phys), public, parameter :: tref        = 288._kind_phys
   ! reference lapse rate [K/m]
   real(kind_phys), public, parameter :: lapse_rate  = 0.0065_kind_phys
   ! Universal gas constant (J/K/kmol)
   real(kind_phys), public, parameter :: r_universal = real(shr_const_rgas, kind_phys)
   ! Density of liquid water (STP)
   real(kind_phys), public, parameter :: rhoh2o      = real(shr_const_rhofw, kind_phys)
   !special value
   real(kind_phys), public, parameter :: spval       = real(shr_const_spval, kind_phys)
   ! Stefan-Boltzmann's constant (W/m^2/K^4)
   real(kind_phys), public, parameter :: stebol      = real(shr_const_stebol, kind_phys)
   ! Triple point temperature of water (K)
   real(kind_phys), public, parameter :: h2otrip     = real(shr_const_tktrip, kind_phys)

   ! Speed of light in a vacuum (m/s)
   real(kind_phys), public, parameter :: c0          = 2.99792458e8_kind_phys
   ! Planck's constant (J.s)
   real(kind_phys), public, parameter :: planck      = 6.6260755e-34_kind_phys

! Molecular weights
   ! molecular weight co2
   real(kind_phys), public, parameter :: mwco2       = 44._kind_phys
   ! molecular weight n2o
   real(kind_phys), public, parameter :: mwn2o       = 44._kind_phys
   ! molecular weight ch4
   real(kind_phys), public, parameter :: mwch4       = 16._kind_phys
   ! molecular weight cfc11
   real(kind_phys), public, parameter :: mwf11       = 136._kind_phys
   ! molecular weight cfc12
   real(kind_phys), public, parameter :: mwf12       = 120._kind_phys
   ! molecular weight O3
   real(kind_phys), public, parameter :: mwo3        = 48._kind_phys
   real(kind_phys), public, parameter :: mwso2       = 64._kind_phys
   real(kind_phys), public, parameter :: mwso4       = 96._kind_phys
   real(kind_phys), public, parameter :: mwh2o2      = 34._kind_phys
   real(kind_phys), public, parameter :: mwdms       = 62._kind_phys
   real(kind_phys), public, parameter :: mwnh4       = 18._kind_phys


   ! modifiable physical constants for aquaplanet

   ! gravitational acceleration (m/s**2)
   real(kind_phys), public, protected :: gravit  = real(shr_const_g, kind_phys)
   ! sec in siderial day ~ sec
   real(kind_phys), public, protected :: sday    = real(shr_const_sday, kind_phys)
   ! molecular weight h2o
   real(kind_phys), public, protected :: mwh2o   = real(shr_const_mwwv, kind_phys)
   ! specific heat of water vapor (J/K/kg)
   real(kind_phys), public, protected :: cpwv    = real(shr_const_cpwv, kind_phys)
   ! molecular weight dry air
   real(kind_phys), public, protected :: mwdry   = real(shr_const_mwdair, kind_phys)
   ! specific heat of dry air (J/K/kg)
   real(kind_phys), public, protected :: cpair   = real(shr_const_cpdair, kind_phys)
   ! radius of earth (m)
   real(kind_phys), public, protected :: rearth  = real(shr_const_rearth, kind_phys)
   ! Freezing point of water (K)
   real(kind_phys), public, protected :: tmelt   = real(shr_const_tkfrz, kind_phys)

!-------------  Variables below here are derived from those above -------------

   ! reciprocal of gravit
   real(kind_phys), public, protected :: rga        = 1._kind_phys/real(shr_const_g, kind_phys)
   ! reciprocal of earth radius
   real(kind_phys), public, protected :: ra         = 1._kind_phys/real(shr_const_rearth, kind_phys)
   ! earth rot ~ rad/sec
   real(kind_phys), public, protected :: omega      = real(shr_const_omega, kind_phys)
   ! Water vapor gas constant ~ J/K/kg
   real(kind_phys), public, protected :: rh2o       = real(shr_const_rwv, kind_phys)
   ! Dry air gas constant     ~ J/K/kg
   real(kind_phys), public, protected :: rair       = real(shr_const_rdair, kind_phys)
   ! ratio of h2o to dry air molecular weights
   real(kind_phys), public, protected :: epsilo     = real(shr_const_mwwv/shr_const_mwdair, kind_phys)
   ! (rh2o/rair) - 1
   real(kind_phys), public, protected :: zvir       = real(shr_const_zvir, kind_phys)
   ! CPWV/CPDAIR - 1.0
   real(kind_phys), public, protected :: cpvir      = real(shr_const_cpvir, kind_phys)
   ! density of dry air at STP  ~ kg/m^3
   real(kind_phys), public, protected :: rhodair    = real(shr_const_rhodair, kind_phys)
   ! R/Cp
   real(kind_phys), public, protected :: cappa      = real((shr_const_rgas/shr_const_mwdair)/shr_const_cpdair, kind_phys)
   ! Coriolis expansion coeff -> omega/sqrt(0.375)
   real(kind_phys), public, protected :: ez
   real(kind_phys), public, protected :: Cpd_on_Cpv = real(shr_const_cpdair/shr_const_cpwv, kind_phys)

   !-------------  Variables below here are for WACCM-X -----------------------
   ! composition dependent specific heat at constant pressure
   real(kind_phys), public, pointer :: cpairv(:,:)
   ! composition dependent gas "constant"
   real(kind_phys), public, pointer :: rairv(:,:)
   ! rairv/cpairv
   real(kind_phys), public, pointer :: cappav(:,:)
   ! composition dependent atmosphere mean mass
   real(kind_phys), public, pointer :: mbarv(:,:)
   ! (rh2o/rair) - 1
   real(kind_phys), public, pointer :: zvirv(:,:)
   ! molecular viscosity      kg/m/s
   real(kind_phys), public, pointer :: kmvis(:,:)
   ! molecular conductivity   J/m/s/K
   real(kind_phys), public, pointer :: kmcnd(:,:)

   ! inverse molecular weights
   real(kind_phys) :: o2_mwi, o_mwi, h_mwi, n2_mwi
   ! constituent indexes
   integer :: o2_ndx=-1, o_ndx=-1, h_ndx=-1

!==============================================================================
CONTAINS
!==============================================================================

   ! Read namelist variables.
   subroutine physconst_readnl(nlfile)
      use shr_kind_mod,   only: r8 => shr_kind_r8
      use shr_nl_mod,     only: find_group_name => shr_nl_find_group_name
      use shr_flux_mod,   only: shr_flux_adjust_constants
!      use mpi,            only: mpi_bcast !!XXgoldyXX: Why not?
      use mpi,            only: mpi_real8
      use spmd_utils,     only: masterproc, masterprocid, mpicom
      use cam_logfile,    only: iulog

      ! filepath for file containing namelist input
      character(len=*), intent(in) :: nlfile

      ! Local variables
      integer :: unitn, ierr
      character(len=*), parameter :: subname = 'physconst_readnl'
      logical :: newg, newsday, newmwh2o, newcpwv
      logical :: newmwdry, newcpair, newrearth, newtmelt, newomega


      ! Physical constants needing to be reset (ie. for aqua planet experiments)
      namelist /physconst_nl/  gravit, sday, mwh2o, cpwv, mwdry, cpair,       &
           rearth, tmelt, omega

      !------------------------------------------------------------------------

      if (masterproc) then
         open(newunit=unitn, file=trim(nlfile), status='old')
         call find_group_name(unitn, 'physconst_nl', status=ierr)
         if (ierr == 0) then
            read(unitn, physconst_nl, iostat=ierr)
            if (ierr /= 0) then
               call endrun(subname // ':: ERROR reading namelist')
            end if
         end if
         close(unitn)
      end if

      ! Broadcast namelist variables
      call mpi_bcast(gravit, masterprocid,  mpi_real8, 0, mpicom, ierr)
      call mpi_bcast(sday,   masterprocid,  mpi_real8, 0, mpicom, ierr)
      call mpi_bcast(mwh2o,  masterprocid,  mpi_real8, 0, mpicom, ierr)
      call mpi_bcast(cpwv,   masterprocid,  mpi_real8, 0, mpicom, ierr)
      call mpi_bcast(mwdry,  masterprocid,  mpi_real8, 0, mpicom, ierr)
      call mpi_bcast(cpair,  masterprocid,  mpi_real8, 0, mpicom, ierr)
      call mpi_bcast(rearth, masterprocid,  mpi_real8, 0, mpicom, ierr)
      call mpi_bcast(tmelt,  masterprocid,  mpi_real8, 0, mpicom, ierr)
      call mpi_bcast(omega,  masterprocid,  mpi_real8, 0, mpicom, ierr)

      newg     =  gravit /= real(shr_const_g, kind_phys)
      newsday  =  sday   /= real(shr_const_sday, kind_phys)
      newmwh2o =  mwh2o  /= real(shr_const_mwwv, kind_phys)
      newcpwv  =  cpwv   /= real(shr_const_cpwv, kind_phys)
      newmwdry =  mwdry  /= real(shr_const_mwdair, kind_phys)
      newcpair =  cpair  /= real(shr_const_cpdair, kind_phys)
      newrearth=  rearth /= real(shr_const_rearth, kind_phys)
      newtmelt =  tmelt  /= real(shr_const_tkfrz, kind_phys)
      newomega =  omega  /= real(shr_const_omega, kind_phys)

      if (newg .or. newsday .or. newmwh2o .or. newcpwv .or. newmwdry .or.     &
           newrearth .or. newtmelt .or. newomega) then
         if (masterproc) then
            write(iulog,*) '**************************************************'
            write(iulog,*) '*  New Physical Constant Values set via namelist *'
            write(iulog,*) '*                                                *'
            write(iulog,*) '* Physical Constant Old Value                  New Value         *'
            if (newg) then
               write(iulog,*) '* GRAVIT          ',shr_const_g,gravit,'*'
            end if
            if (newsday) then
               write(iulog,*) '* SDAY            ',shr_const_sday,sday,'*'
            end if
            if (newmwh2o) then
               write(iulog,*) '* MWH20           ',shr_const_mwwv,mwh2o,'*'
            end if
            if (newcpwv) then
               write(iulog,*) '* CPWV            ',shr_const_cpwv,cpwv,'*'
            end if
            if (newmwdry) then
               write(iulog,*) '* MWDRY           ',shr_const_mwdair,mwdry,'*'
            end if
            if (newcpair) then
               write(iulog,*) '* CPAIR           ',shr_const_cpdair,cpair,'*'
            end if
            if (newrearth) then
               write(iulog,*) '* REARTH          ',shr_const_rearth,rearth,'*'
            end if
            if (newtmelt) then
               write(iulog,*) '* TMELT           ',shr_const_tkfrz,tmelt,'*'
            end if
            if (newomega) then
               write(iulog,*) '* OMEGA           ',shr_const_omega,omega,'*'
            end if
            write(iulog,*)'******************************************************'
         end if
         rga         = 1._kind_phys / gravit
         ra          = 1._kind_phys / rearth
         if (.not. newomega) then
            omega       = 2.0_kind_phys * pi / sday
         end if
         cpvir       = (cpwv / cpair) - 1._kind_phys
         epsilo      = mwh2o / mwdry

         ! rair and rh2o must be defined before any of the variables that use them
         rair        = r_universal / mwdry
         rh2o        = r_universal / mwh2o

         cappa       = rair / cpair
         rhodair     = pstd / (rair*tmelt)
         zvir        =  (rh2o / rair) - 1.0_kind_phys
         Cpd_on_Cpv  = cpair / cpwv

         ! Adjust constants in shr_flux_mod.
         call shr_flux_adjust_constants(zvir=real(zvir, r8),                  &
              cpvir=real(cpvir, r8), gravit=real(gravit, r8))

      end if
      ez          = omega / sqrt(0.375_kind_phys)

   end subroutine physconst_readnl

   !===========================================================================

   subroutine physconst_init(pcols, pver, pverp)
      !!XXgoldyXX: v until we get constituents figured out in CCPP
#if 0
      !!XXgoldyXX: ^  until we get constituents figured out in CCPP
      use constituents, only: cnst_get_ind, cnst_mw
      !!XXgoldyXX: v until we get constituents figured out in CCPP
#endif
      !!XXgoldyXX: ^  until we get constituents figured out in CCPP

      integer, intent(in) :: pcols
      integer, intent(in) :: pver
      integer, intent(in) :: pverp

      integer         :: n_ndx
      integer         :: ierr
      real(kind_phys) :: o2_mw, o_mw, h_mw, n_mw
      logical         :: planet_mars
      character(len=*), parameter :: subname = 'physconst_init'


      planet_mars = .false.
      if (planet_mars) then
         pstd = 6.0E1_kind_phys
      end if

      !------------------------------------------------------------------------
      !  Allocate constituent dependent properties
      !------------------------------------------------------------------------
      allocate(cpairv(pcols,pver), stat=ierr)
      if (ierr /= 0) then
         call endrun(subname//': allocate cpairv failed')
      end if
      allocate(rairv(pcols,pver), stat=ierr)
      if (ierr /= 0) then
         call endrun(subname//': allocate rairv failed')
      end if
      allocate(cappav(pcols,pver), stat=ierr)
      if (ierr /= 0) then
         call endrun(subname//': allocate cappav failed')
      end if
      allocate(mbarv(pcols,pver), stat=ierr)
      if (ierr /= 0) then
         call endrun(subname//': allocate mbarv failed')
      end if
      allocate(zvirv(pcols,pver), stat=ierr)
      if (ierr /= 0) then
         call endrun(subname//': allocate zvirv failed')
      end if
      allocate(kmvis(pcols,pverp), stat=ierr)
      if (ierr /= 0) then
         call endrun(subname//': allocate kmvis failed')
      end if
      allocate(kmcnd(pcols,pverp), stat=ierr)
      if (ierr /= 0) then
         call endrun(subname//': allocate kmcnd failed')
      end if

      !------------------------------------------------------------------------
      !  Initialize constituent dependent properties
      !------------------------------------------------------------------------
      cpairv(:pcols,:pver) = cpair
      rairv(:pcols,:pver) = rair
      cappav(:pcols,:pver) = rair/cpair
      mbarv(:pcols,:pver) = mwdry
      zvirv(:pcols,:pver) = cpair

      !!XXgoldyXX: v until we get constituents figured out in CCPP
#if 0
      !!XXgoldyXX: ^  until we get constituents figured out in CCPP
      call cnst_get_ind('O2', o2_ndx, abort=.false.)
      call cnst_get_ind('O',  o_ndx,  abort=.false.)
      call cnst_get_ind('H',  h_ndx,  abort=.false.)
      call cnst_get_ind('N',  n_ndx,  abort=.false.)

      if (o2_ndx>0) then
         o2_mw = cnst_mw(o2_ndx)
         o2_mwi = 1.0_kind_phys/o2_mw
      endif
      if (o_ndx>0) then
         o_mw = cnst_mw(o_ndx)
         o_mwi = 1.0_kind_phys/o_mw
      endif
      if (h_ndx>0) then
         h_mw = cnst_mw(h_ndx)
         h_mwi = 1.0_kind_phys/h_mw
      endif
      if (n_ndx>0) then
         n_mw = cnst_mw(n_ndx)
         n2_mwi = 0.5_kind_phys/n_mw
      endif
!!XXgoldyXX: v until we get constituents figured out in CCPP
#endif
!!XXgoldyXX: ^  until we get constituents figured out in CCPP

   end subroutine physconst_init

   !===========================================================================
   subroutine physconst_update(mmr, t, ncol, to_moist_factor)

      !-----------------------------------------------------------------------
      ! Update the physics "constants" that vary
      !-----------------------------------------------------------------------

      !-------------------------Arguments-------------------------------------

      real(kind_phys), intent(in) :: mmr(pcols,pver,pcnst) ! constituents q array from state structure
      real(kind_phys), intent(in) :: t(pcols,pver)   ! temperature t array from state structure
      integer, intent(in)  :: ncol            ! number of columns
      real(kind_phys),  optional, intent(in) :: to_moist_factor(:,:)
!!XXgoldyXX: v until we get constituents figured out in CCPP
#if 0
!!XXgoldyXX: ^  until we get constituents figured out in CCPP
      !
      !--------------------------------Local storage---------------------------
      integer :: i,k                                 ! column,level,constituent indices

      real(kind_phys):: mmro, mmro2, mmrh, mmrn2 ! Mass mixing ratios of O, O2, H, and N
      real(kind_phys):: mbarvi, tint             ! Mean mass, temperature, and specific heat on interface levels
      real(kind_phys):: dof1, dof2               ! Degress of freedom for cpairv calculation
      real(kind_phys):: kv1, kv2, kv3, kv4       ! Coefficients for kmvis calculation
      real(kind_phys):: kc1, kc2, kc3, kc4       ! Coefficients for kmcnd calculation
      real(kind_phys) :: to_moist_fact(ncol,pver)

      !--------------------------------------------
      ! Set constants needed for updates
      !--------------------------------------------
      dof1 = 5._kind_phys
      dof2 = 7._kind_phys
      kv1  = 4.03_kind_phys
      kv2  = 3.42_kind_phys
      kv3  = 3.9_kind_phys
      kv4  = 0.69_kind_phys
      kc1  = 56._kind_phys
      kc2  = 56._kind_phys
      kc3  = 75.9_kind_phys
      kc4  = 0.69_kind_phys

      to_moist_fact(:,:) = 1._kind_phys

      if (present(to_moist_factor)) then
         to_moist_fact(:ncol,:) = to_moist_factor(:ncol,:)
      end if

      if (o2_ndx<0 .or. o_ndx<0 .or. h_ndx<0) then
         call endrun('physconst_update: ERROR -- needed constituents are not available')
      endif

      !--------------------------------------------
      ! update cpairv, rairv, mbarv, and cappav
      !--------------------------------------------
      do k=1,pver
         do i=1,ncol
            mmro  = mmr(i,k,o_ndx)*to_moist_fact(i,k) ! convert to moist mass mixing ratios
            mmro2 = mmr(i,k,o2_ndx)*to_moist_fact(i,k)
            mmrh  = mmr(i,k,h_ndx)*to_moist_fact(i,k)
            mmrn2 = 1._kind_phys-mmro-mmro2-mmrh
            mbarv(i,k,lchnk) = 1._kind_phys/( mmro *o_mwi  + &
                 mmro2*o2_mwi + &
                 mmrn2*n2_mwi + &
                 mmrh *h_mwi )
            rairv(i,k,lchnk) = shr_const_rgas / mbarv(i,k,lchnk)
            cpairv(i,k,lchnk) = 0.5_kind_phys*shr_const_rgas &
                 * ( dof1*mmro *o_mwi  + &
                 dof2*mmro2*o2_mwi + &
                 dof2*mmrn2*n2_mwi + &
                 dof1*mmrh *h_mwi )

            cappav(i,k,lchnk) = rairv(i,k,lchnk)/cpairv(i,k,lchnk)
         enddo
      enddo

      do k=2,pver
         do i=1,ncol
            mmro  = .5_kind_phys*(mmr(i,k-1,o_ndx) *to_moist_fact(i,k-1)+mmr(i,k,o_ndx) *to_moist_fact(i,k))
            mmro2 = .5_kind_phys*(mmr(i,k-1,o2_ndx)*to_moist_fact(i,k-1)+mmr(i,k,o2_ndx)*to_moist_fact(i,k))
            mmrh  = .5_kind_phys*(mmr(i,k-1,h_ndx) *to_moist_fact(i,k-1)+mmr(i,k,h_ndx) *to_moist_fact(i,k))
            mmrn2 = 1._kind_phys-mmro-mmro2-mmrh
            mbarvi = .5_kind_phys*(mbarv(i,k-1,lchnk)+mbarv(i,k,lchnk))
            tint = .5_kind_phys*(t(i,k-1)+t(i,k))

            kmvis(i,k,lchnk) = (kv1*mmro2*o2_mwi+        &
                 kv2*mmrn2*n2_mwi+        &
                 kv3*mmro*o_mwi)*mbarvi*  &
                 tint**kv4 * 1.e-7_kind_phys
            kmcnd(i,k,lchnk) = (kc1*mmro2*o2_mwi+             &
                 kc2*mmrn2*n2_mwi+        &
                 kc3*mmro*o_mwi)*mbarvi*   &
                 tint**kc4 * 1.e-5_kind_phys
         enddo
      enddo
      do i=1,ncol
         kmvis(i,1,lchnk) = 1.5_kind_phys*kmvis(i,2,lchnk)-.5_kind_phys*kmvis(i,3,lchnk)
         kmcnd(i,1,lchnk) = 1.5_kind_phys*kmcnd(i,2,lchnk)-.5_kind_phys*kmcnd(i,3,lchnk)
         kmvis(i,pverp,lchnk) = kmvis(i,pver,lchnk)
         kmcnd(i,pverp,lchnk) = kmcnd(i,pver,lchnk)
      enddo

!!XXgoldyXX: v until we get constituents figured out in CCPP
#endif
!!XXgoldyXX: ^  until we get constituents figured out in CCPP
   end subroutine physconst_update

   !===========================================================================

   subroutine physconst_calc_kappav(i0,i1,j0,j1,k0,k1,ntotq,tracer,kappav,cpv)
     ! assumes moist MMRs

     ! args
     integer,  intent(in) :: i0,i1,j0,j1,k0,k1, ntotq
     real(kind_phys), intent(in) :: tracer(i0:i1,j0:j1,k0:k1,ntotq) ! Tracer array
     real(kind_phys), intent(out) :: kappav(i0:i1,j0:j1,k0:k1)
     real(kind_phys), optional, intent(out) :: cpv(i0:i1,j0:j1,k0:k1)
!!XXgoldyXX: v until we get constituents figured out in CCPP
kappav = 0._kind_phys
cpv = 0._kind_phys
#if 0
!!XXgoldyXX: ^  until we get constituents figured out in CCPP
     ! local vars
     integer :: i,j,k
     real(kind_phys),  dimension(i0:i1,j0:j1,k0:k1) :: rgas_var, cp_var, mmro, mmro2, mmrh, mmrn2

     real(kind_phys), parameter :: dof1 = 5.0_kind_phys ! Degrees of freedom for cpair3v calculation
     real(kind_phys), parameter :: dof2 = 7.0_kind_phys ! Degrees of freedom for cpair3v calculation

     if (o2_ndx<0 .or. o_ndx<0 .or. h_ndx<0) then
        call endrun('physconst_calc_kappav: ERROR -- things are not initialized')
     endif

     !-----------------------------------------------------------------------
     !  Calculate constituent dependent specific heat, gas constant and cappa
     !-----------------------------------------------------------------------
!$omp parallel do private(i,j,k)
     do k = k0,k1
        do j = j0,j1
           do i = i0,i1
              mmro(i,j,k)  = tracer(i,j,k,o_ndx)
              mmro2(i,j,k) = tracer(i,j,k,o2_ndx)
              mmrh(i,j,k)  = tracer(i,j,k,h_ndx)
              mmrn2(i,j,k) = 1._kind_phys-mmro(i,j,k)-mmro2(i,j,k)-mmrh(i,j,k)

              rgas_var(i,j,k) = shr_const_rgas &
                              * ( mmro (i,j,k)*o_mwi + &
                                  mmro2(i,j,k)*o2_mwi + &
                                  mmrn2(i,j,k)*n2_mwi + &
                                  mmrh (i,j,k)*h_mwi )

              cp_var(i,j,k) = 0.5_kind_phys*shr_const_rgas &
                            * ( dof1*mmro (i,j,k)*o_mwi + &
                                dof2*mmro2(i,j,k)*o2_mwi + &
                                dof2*mmrn2(i,j,k)*n2_mwi + &
                                dof1*mmrh (i,j,k)*h_mwi )

              kappav(i,j,k) = rgas_var(i,j,k)/cp_var(i,j,k)

           enddo
        enddo
     enddo

     if (present(cpv)) then
        cpv(:,:,:) = cp_var(:,:,:)
     endif
!!XXgoldyXX: v until we get constituents figured out in CCPP
#endif
!!XXgoldyXX: ^  until we get constituents figured out in CCPP

   end subroutine physconst_calc_kappav
end module physconst
