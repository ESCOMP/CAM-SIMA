module physconst

   ! Physical constants.  Use csm_share values whenever available.
   use ccpp_kinds,     only: kind_phys
   use shr_const_mod,  only: shr_const_g
   use shr_const_mod,  only: shr_const_stebol
   use shr_const_mod,  only: shr_const_tkfrz
   use shr_const_mod,  only: shr_const_mwdair
   use shr_const_mod,  only: shr_const_rdair
   use shr_const_mod,  only: shr_const_mwwv
   use shr_const_mod,  only: shr_const_latice
   use shr_const_mod,  only: shr_const_latvap
   use shr_const_mod,  only: shr_const_cpdair
   use shr_const_mod,  only: shr_const_rhofw
   use shr_const_mod,  only: shr_const_cpwv
   use shr_const_mod,  only: shr_const_rgas
   use shr_const_mod,  only: shr_const_karman
   use shr_const_mod,  only: shr_const_pstd
   use shr_const_mod,  only: shr_const_rhodair
   use shr_const_mod,  only: shr_const_avogad
   use shr_const_mod,  only: shr_const_boltz
   use shr_const_mod,  only: shr_const_cpfw
   use shr_const_mod,  only: shr_const_rwv
   use shr_const_mod,  only: shr_const_zvir
   use shr_const_mod,  only: shr_const_pi
   use shr_const_mod,  only: shr_const_rearth
   use shr_const_mod,  only: shr_const_sday
   use shr_const_mod,  only: shr_const_cday
   use shr_const_mod,  only: shr_const_omega
   use shr_const_mod,  only: shr_const_cpvir
   use shr_const_mod,  only: shr_const_tktrip
   use shr_const_mod,  only: shr_const_cpice
   use shr_flux_mod,   only: shr_flux_adjust_constants
   use cam_abortutils, only: endrun

   implicit none
   private
   save

   public :: physconst_readnl

   real(kind_phys), parameter :: UNSET_NAMELIST = 0.0_kind_phys

   !> \section arg_table_physconst  Argument Table
   !! \htmlinclude physconst.html
   ! Constants based off share code or defined in physconst
   real(kind_phys), public, parameter :: avogad      = real(shr_const_avogad, kind_phys)     ! Avogadro's number (molecules kmole-1)
   real(kind_phys), public, parameter :: boltz       = real(shr_const_boltz, kind_phys)      ! Boltzman's constant (J K-1 molecule-1)
   real(kind_phys), public, parameter :: cday        = real(shr_const_cday, kind_phys)       ! sec in calendar day (seconds)
   real(kind_phys), public, parameter :: cpliq       = real(shr_const_cpfw, kind_phys)       ! specific heat of fresh h2o (J K-1 kg-1)
   real(kind_phys), public, parameter :: cpice       = real(shr_const_cpice, kind_phys)      ! specific heat of ice (J K-1 kg-1)
   real(kind_phys), public, parameter :: karman      = real(shr_const_karman, kind_phys)     ! Von Karman constant
   real(kind_phys), public, parameter :: latice      = real(shr_const_latice, kind_phys)     ! Latent heat of fusion (J kg-1)
   real(kind_phys), public, parameter :: latvap      = real(shr_const_latvap, kind_phys)     ! Latent heat of vaporization (J kg-1)
   real(kind_phys), public, parameter :: pi          = real(shr_const_pi, kind_phys)         ! 3.14...
   real(kind_phys), public, protected :: pstd        = real(shr_const_pstd, kind_phys)       ! Standard pressure (Pascals)
   real(kind_phys), public, protected :: pref        = 1.0e5_kind_phys                       ! Reference surface pressure (Pascals)
   real(kind_phys), public, parameter :: tref        = 288._kind_phys                        ! Reference temperature (K)
   real(kind_phys), public, parameter :: lapse_rate  = 0.0065_kind_phys                      ! reference lapse rate (K m-1)
   real(kind_phys), public, parameter :: r_universal = real(shr_const_rgas, kind_phys)       ! Universal gas constant (J K-1 kmol-1)
   real(kind_phys), public, parameter :: rhoh2o      = real(shr_const_rhofw, kind_phys)      ! Density of liquid water at STP (kg m-3)
   real(kind_phys), public, parameter :: stebol      = real(shr_const_stebol, kind_phys)     ! Stefan-Boltzmann's constant (W m-2 K-4)
   real(kind_phys), public, parameter :: h2otrip     = real(shr_const_tktrip, kind_phys)     ! Triple point temperature of water (K)

   real(kind_phys), public, parameter :: c0          = 2.99792458e8_kind_phys      ! Speed of light in a vacuum (m s-1)
   real(kind_phys), public, parameter :: planck      = 6.6260755e-34_kind_phys     ! Planck's constant (J.s)

   ! Molecular weights (g mol-1)
   real(kind_phys), public, parameter :: mwco2       =  44._kind_phys                      ! molecular weight co2
   real(kind_phys), public, parameter :: mwn2o       =  44._kind_phys                      ! molecular weight n2o
   real(kind_phys), public, parameter :: mwch4       =  16._kind_phys                      ! molecular weight ch4
   real(kind_phys), public, parameter :: mwf11       = 136._kind_phys                      ! molecular weight cfc11
   real(kind_phys), public, parameter :: mwf12       = 120._kind_phys                      ! molecular weight cfc12
   real(kind_phys), public, parameter :: mwo3        =  48._kind_phys                      ! molecular weight O3
   real(kind_phys), public, parameter :: mwso2       =  64._kind_phys                      ! molecular weight so2
   real(kind_phys), public, parameter :: mwso4       =  96._kind_phys                      ! molecular weight so4
   real(kind_phys), public, parameter :: mwh2o2      =  34._kind_phys                      ! molecular weight h2o2
   real(kind_phys), public, parameter :: mwdms       =  62._kind_phys                      ! molecular weight dms
   real(kind_phys), public, parameter :: mwnh4       =  18._kind_phys                      ! molecular wieght nh4
   real(kind_phys), public, protected :: mwh2o       =  real(shr_const_mwwv, kind_phys)    ! molecular weight h2o
   real(kind_phys), public, protected :: mwdry       =  real(shr_const_mwdair, kind_phys)  ! molecular weight dry air

   ! modifiable physical constants for  other planets (including aquaplanet)
   real(kind_phys), public, protected :: gravit  = real(shr_const_g, kind_phys)            ! gravitational acceleration (m s-2)
   real(kind_phys), public, protected :: sday    = real(shr_const_sday, kind_phys)         ! sec in sidereal day (seconds)
   real(kind_phys), public, protected :: cpwv    = real(shr_const_cpwv, kind_phys)         ! specific heat of water vapor (J K-1 kg-1)
   real(kind_phys), public, protected :: cpair   = real(shr_const_cpdair, kind_phys)       ! specific heat of dry air (J K-1 kg-1)
   real(kind_phys), public, protected :: rearth  = real(shr_const_rearth, kind_phys)       ! radius of earth (m)
   real(kind_phys), public, protected :: tmelt   = real(shr_const_tkfrz, kind_phys)        ! Freezing point of water (K)

   !-----  Variables below here are derived from those above -----------------

   real(kind_phys), public, protected :: rga          = 1._kind_phys/real(shr_const_g, kind_phys)         ! reciprocal of gravit (s2 m-1)
   real(kind_phys), public, protected :: rearth_recip = 1._kind_phys/real(shr_const_rearth, kind_phys)    ! reciprocal of earth radius (m-1)
   real(kind_phys), public, protected :: omega        = real(shr_const_omega, kind_phys)                  ! earth rot (rad sec-1)
   real(kind_phys), public, protected :: rh2o         = real(shr_const_rwv, kind_phys)                    ! Water vapor gas constant (J K-1 kg-1)
   real(kind_phys), public, protected :: rair         = real(shr_const_rdair, kind_phys)                  ! Dry air gas constant     (J K-1 kg-1)
   real(kind_phys), public, protected :: epsilo       = real(shr_const_mwwv/shr_const_mwdair, kind_phys)  ! ratio of h2o to dry air molecular weights
   real(kind_phys), public, protected :: zvir         = real(shr_const_zvir, kind_phys)                   ! (rh2o/rair) - 1
   real(kind_phys), public, protected :: cpvir        = real(shr_const_cpvir, kind_phys)                  ! CPWV/CPDAIR - 1.0
   real(kind_phys), public, protected :: rhodair      = real(shr_const_rhodair, kind_phys)                ! density of dry air at STP (kg m-3)
   real(kind_phys), public, protected :: cappa        = real((shr_const_rgas/shr_const_mwdair)/shr_const_cpdair, kind_phys)  ! R/Cp
   real(kind_phys), public, protected :: ez                                                             ! Coriolis expansion coeff -> omega/sqrt(0.375)
   real(kind_phys), public, protected :: Cpd_on_Cpv   = real(shr_const_cpdair/shr_const_cpwv, kind_phys)

!==============================================================================
CONTAINS
!==============================================================================

   ! Read namelist variables.
   subroutine physconst_readnl(nlfile)
      use shr_nl_mod,      only: find_group_name => shr_nl_find_group_name
      use spmd_utils,      only: masterproc, mpicom, masterprocid
      use mpi,             only: mpi_real8
      use cam_logfile,     only: iulog
      use runtime_obj,     only: unset_real

      ! Dummy argument: filepath for file containing namelist input
      character(len=*), intent(in) :: nlfile

      ! Local variables
      integer                     :: unitn, ierr
      logical                     :: newg
      logical                     :: newsday
      logical                     :: newmwh2o
      logical                     :: newcpwv
      logical                     :: newmwdry
      logical                     :: newcpair
      logical                     :: newrearth
      logical                     :: newtmelt
      logical                     :: newomega
      integer,          parameter :: lsize = 76
      integer,          parameter :: fsize = 23
      character(len=*), parameter :: subname = 'physconst_readnl :: '
      character(len=lsize)        :: banner
      character(len=lsize)        :: bline
      character(len=fsize)        :: field
      real(kind_phys)             :: user_defined_gravit
      real(kind_phys)             :: user_defined_sday
      real(kind_phys)             :: user_defined_mwh2o
      real(kind_phys)             :: user_defined_cpwv
      real(kind_phys)             :: user_defined_mwdry
      real(kind_phys)             :: user_defined_cpair
      real(kind_phys)             :: user_defined_rearth
      real(kind_phys)             :: user_defined_tmelt
      real(kind_phys)             :: user_defined_omega

      ! Physical constants needing to be reset
      !    (e.g., for aqua planet experiments)
      namelist /physconst_nl/  user_defined_gravit, user_defined_sday, user_defined_mwh2o, user_defined_cpwv, user_defined_mwdry,              &
           user_defined_cpair, user_defined_rearth, user_defined_tmelt, user_defined_omega
      !----------------------------------------------------------------------
      user_defined_gravit = UNSET_REAL
      user_defined_sday = UNSET_REAL
      user_defined_mwh2o = UNSET_REAL
      user_defined_cpwv = UNSET_REAL
      user_defined_mwdry = UNSET_REAL
      user_defined_cpair = UNSET_REAL
      user_defined_rearth = UNSET_REAL
      user_defined_tmelt = UNSET_REAL
      user_defined_omega = UNSET_REAL

      banner = repeat('*', lsize)
      bline = "***"//repeat(' ', lsize - 6)//"***"
2000  format("*** ",a,2("   ",E18.10),"  ***")
      if (masterproc) then
         open(newunit=unitn, file=trim(nlfile), status='old')
         call find_group_name(unitn, 'physconst_nl', status=ierr)
         if (ierr == 0) then
            read(unitn, physconst_nl, iostat=ierr)
            if (ierr /= 0) then
               call endrun(subname//'ERROR reading namelist, physconst_nl')
            end if
         end if
         close(unitn)
      end if

      ! Broadcast namelist variables
      call MPI_bcast(user_defined_gravit, 1, mpi_real8, masterprocid, mpicom, ierr)
      if (ierr /= 0) call endrun(subname//": FATAL: mpi_bcast: user_defined_gravit")
      call MPI_bcast(user_defined_sday,   1, mpi_real8, masterprocid, mpicom, ierr)
      if (ierr /= 0) call endrun(subname//": FATAL: mpi_bcast: user_defined_sday")
      call MPI_bcast(user_defined_mwh2o,  1, mpi_real8, masterprocid, mpicom, ierr)
      if (ierr /= 0) call endrun(subname//": FATAL: mpi_bcast: user_defined_mwh20")
      call MPI_bcast(user_defined_cpwv,   1, mpi_real8, masterprocid, mpicom, ierr)
      if (ierr /= 0) call endrun(subname//": FATAL: mpi_bcast: user_defined_cpwv")
      call MPI_bcast(user_defined_mwdry,  1, mpi_real8, masterprocid, mpicom, ierr)
      if (ierr /= 0) call endrun(subname//": FATAL: mpi_bcast: user_defined_mwdry")
      call MPI_bcast(user_defined_cpair,  1, mpi_real8, masterprocid, mpicom, ierr)
      if (ierr /= 0) call endrun(subname//": FATAL: mpi_bcast: user_defined_cpair")
      call MPI_bcast(user_defined_rearth, 1, mpi_real8, masterprocid, mpicom, ierr)
      if (ierr /= 0) call endrun(subname//": FATAL: mpi_bcast: user_defined_rearth")
      call MPI_bcast(user_defined_tmelt,  1, mpi_real8, masterprocid, mpicom, ierr)
      if (ierr /= 0) call endrun(subname//": FATAL: mpi_bcast: user_defined_tmelt")
      call MPI_bcast(user_defined_omega,  1, mpi_real8, masterprocid, mpicom, ierr)
      if (ierr /= 0) call endrun(subname//": FATAL: mpi_bcast: user_defined_omega")

      newg     =  user_defined_gravit /= shr_const_g .and. user_defined_gravit /= UNSET_NAMELIST
      newsday  =  user_defined_sday   /= shr_const_sday .and. user_defined_sday /= UNSET_NAMELIST
      newmwh2o =  user_defined_mwh2o  /= shr_const_mwwv .and. user_defined_mwh2o /= UNSET_NAMELIST
      newcpwv  =  user_defined_cpwv   /= shr_const_cpwv .and. user_defined_cpwv /= UNSET_NAMELIST
      newmwdry =  user_defined_mwdry  /= shr_const_mwdair .and. user_defined_mwdry /= UNSET_NAMELIST
      newcpair =  user_defined_cpair  /= shr_const_cpdair .and. user_defined_cpair /= UNSET_NAMELIST
      newrearth=  user_defined_rearth /= shr_const_rearth .and. user_defined_rearth /= UNSET_NAMELIST
      newtmelt =  user_defined_tmelt  /= shr_const_tkfrz .and. user_defined_tmelt /= UNSET_NAMELIST
      newomega =  user_defined_omega  /= shr_const_omega .and. user_defined_omega /= UNSET_NAMELIST

      if (newg .or. newsday .or. newmwh2o .or. newcpwv .or. newmwdry .or.     &
           newrearth .or. newtmelt .or. newomega) then

         ! Populate the new constants into module after mpi_bcast
         if(newg)      gravit = user_defined_gravit
         if(newsday)   sday   = user_defined_sday
         if(newmwh2o)  mwh2o  = user_defined_mwh2o
         if(newcpwv)   cpwv   = user_defined_cpwv
         if(newmwdry)  mwdry  = user_defined_mwdry
         if(newcpair)  cpair  = user_defined_cpair
         if(newrearth) rearth = user_defined_rearth
         if(newtmelt)  tmelt  = user_defined_tmelt
         if(newomega)  omega  = user_defined_omega

         if (masterproc) then
            write(iulog, *) banner
            write(iulog, *) '***    New Physical Constant Values set ',       &
                 'via namelist                     ***'
            write(iulog, *) bline
            write(iulog, *) '*** Physical Constant    Old Value                  New Value         ***'
            if (newg) then
               field = 'GRAVIT'
               write(iulog, 2000) field, shr_const_g, gravit
            end if
            if (newsday) then
               field = 'SDAY'
               write(iulog, 2000) field, shr_const_sday, sday
            end if
            if (newmwh2o) then
               field = 'MWH20'
               write(iulog, 2000) field, shr_const_mwwv, mwh2o
            end if
            if (newcpwv) then
               field = 'CPWV'
               write(iulog, 2000) field, shr_const_cpwv, cpwv
            end if
            if (newmwdry) then
               field = 'MWDRY'
               write(iulog, 2000) field, shr_const_mwdair, mwdry
            end if
            if (newcpair) then
               field = 'CPAIR'
               write(iulog, 2000) field, shr_const_cpdair, cpair
            end if
            if (newrearth) then
               field = 'REARTH'
               write(iulog, 2000) field, shr_const_rearth, rearth
            end if
            if (newtmelt) then
               field = 'TMELT'
               write(iulog, 2000) field, shr_const_tkfrz, tmelt
            end if
            if (newomega) then
               field = 'OMEGA'
               write(iulog, 2000) field, shr_const_omega, omega
            end if
            write(iulog,*) banner
         end if

         rga = 1._kind_phys / gravit
         rearth_recip  = 1._kind_phys / rearth
         if (.not. newomega) then
            omega = 2.0_kind_phys * pi / sday
         end if
         cpvir  = (cpwv / cpair) - 1._kind_phys
         epsilo = mwh2o / mwdry

         !  defined rair and rh2o before any of the variables that use them
         rair = r_universal / mwdry
         rh2o = r_universal / mwh2o

         cappa       = rair / cpair
         rhodair     = pstd / (rair * tmelt)
         zvir        = (rh2o / rair) - 1.0_kind_phys
         Cpd_on_Cpv  = cpair / cpwv

         ! Adjust constants in shr_flux_mod.
         call shr_flux_adjust_constants(zvir=zvir, cpvir=cpvir, gravit=gravit)
      end if

      ez = omega / sqrt(0.375_kind_phys)

   end subroutine physconst_readnl

end module physconst
