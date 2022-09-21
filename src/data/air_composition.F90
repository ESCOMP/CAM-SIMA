! air_composition module defines major species of the atmosphere and manages the physical properties that are dependent on the composition of air
module air_composition

   use ccpp_kinds,     only: kind_phys
   use cam_abortutils, only: endrun, check_allocate

   implicit none
   private
   save

   public  :: air_composition_readnl
   public  :: air_composition_init
   public  :: air_composition_update
   ! get_cp_dry: (generalized) heat capacity for dry air
   public :: get_cp_dry
   ! get_cp: (generalized) heat capacity
   public :: get_cp
   ! get_R_dry: (generalized) dry air gas constant
   public :: get_R_dry
   ! get_R: Compute generalized R
   public :: get_R
   ! get_mbarv: molecular weight of dry air
   public :: get_mbarv

   private :: air_species_info

   integer,         parameter :: unseti = -HUGE(1)
   real(kind_phys), parameter :: unsetr =  HUGE(1.0_kind_phys)

   ! composition of air
   !
   integer, parameter :: num_names_max = 30
   character(len=80)  :: dry_air_species(num_names_max)
   character(len=80)  :: water_species_in_air(num_names_max)

   integer, protected, public :: dry_air_species_num
   integer, protected, public :: water_species_in_air_num

   ! Thermodynamic variables
   integer,                      protected, public :: thermodynamic_active_species_num = unseti
   integer,         allocatable, protected, public :: thermodynamic_active_species_idx(:)
   integer,         allocatable,            public :: thermodynamic_active_species_idx_dycore(:)
   real(kind_phys), allocatable, protected, public :: thermodynamic_active_species_cp(:)
   real(kind_phys), allocatable, protected, public :: thermodynamic_active_species_cv(:)
   real(kind_phys), allocatable, protected, public :: thermodynamic_active_species_R(:)
   ! thermodynamic_active_species_mwi: inverse molecular weights dry air
   real(kind_phys), allocatable, protected, public :: thermodynamic_active_species_mwi(:)
   ! thermodynamic_active_species_kv: molecular diffusion
   real(kind_phys), allocatable, protected, public :: thermodynamic_active_species_kv(:)
   ! thermodynamic_active_species_kc: thermal conductivity
   real(kind_phys), allocatable, protected, public :: thermodynamic_active_species_kc(:)
   !
   ! for energy computations liquid and ice species need to be identified
   !
   ! thermodynamic_active_species_liq_num: number of liquid water species
   integer,               protected, public :: thermodynamic_active_species_liq_num = unseti
   ! thermodynamic_active_species_ice_num: number of frozen water species
   integer,               protected, public :: thermodynamic_active_species_ice_num = unseti
   ! thermodynamic_active_species_liq_idx: index of liquid water species
   integer,  allocatable, protected, public :: thermodynamic_active_species_liq_idx(:)
   ! thermodynamic_active_species_liq_idx_dycore: index of liquid water species
   integer,  allocatable,            public :: thermodynamic_active_species_liq_idx_dycore(:)
   ! thermodynamic_active_species_ice_idx: index of ice water species
   integer,  allocatable, protected, public :: thermodynamic_active_species_ice_idx(:)
   ! thermodynamic_active_species_ice_idx_dycore: index of ice water species
   integer,  allocatable,            public :: thermodynamic_active_species_ice_idx_dycore(:)
   ! enthalpy_reference_state: choices: 'ice', 'liq', 'wv'
   character(len=3), public, protected :: enthalpy_reference_state = 'xxx'

   integer, protected, public :: wv_idx = -1 ! Water vapor index

   !------------- Variables for consistent themodynamics --------------------
   !

   ! coefficients in expressions for molecular diffusion coefficients
   ! kv1,..,kv3 are coefficients for kmvis calculation
   ! kc1,..,kc3 are coefficients for kmcnd calculation
   ! Liu, H.-L., et al. (2010), Thermosphere extension of the Whole Atmosphere Community Climate Model,
   !    J. Geophys. Res., 115, A12302, doi:10.1029/2010JA015586.
   real(kind_phys), public, parameter :: kv1 = 4.03_kind_phys * 1.e-7_kind_phys
   real(kind_phys), public, parameter :: kv2 = 3.42_kind_phys * 1.e-7_kind_phys
   real(kind_phys), public, parameter :: kv3 = 3.9_kind_phys * 1.e-7_kind_phys
   real(kind_phys), public, parameter :: kc1 = 56._kind_phys * 1.e-5_kind_phys
   real(kind_phys), public, parameter :: kc2 = 56._kind_phys * 1.e-5_kind_phys
   real(kind_phys), public, parameter :: kc3 = 75.9_kind_phys * 1.e-5_kind_phys

   real(kind_phys), public, parameter :: kv_temp_exp = 0.69_kind_phys
   real(kind_phys), public, parameter :: kc_temp_exp = 0.69_kind_phys

   !> \section arg_table_air_composition  Argument Table
   !! \htmlinclude air_composition.html
   ! standard dry air (constant composition)
   real(kind_phys), public, protected :: mmro2 = unsetr  ! Mass mixing ratio of O2
   real(kind_phys), public, protected :: mmrn2 = unsetr  ! Mass mixing ratio of N2
   real(kind_phys), public, protected :: o2_mwi = unsetr ! Inverse mol. weight of O2
   real(kind_phys), public, protected :: n2_mwi = unsetr ! Inverse mol. weight of N2
   real(kind_phys), public, protected :: mbar = unsetr   ! Mean mass at mid level

   ! cpairv:  composition dependent specific heat at constant pressure
   real(kind_phys), public, protected, allocatable :: cpairv(:,:)
   ! rairv: composition dependent gas "constant"
   real(kind_phys), public, protected, allocatable :: rairv(:,:)
   ! cappav: rairv / cpairv
   real(kind_phys), public, protected, allocatable :: cappav(:,:)
   ! mbarv: composition dependent atmosphere mean mass
   real(kind_phys), public, protected, allocatable :: mbarv(:,:)
   ! zvirv: rh2o / rair - 1
   real(kind_phys), public, protected, allocatable :: zvirv(:,:)

   !
   ! Interfaces for public routines
   interface get_cp_dry
      module procedure get_cp_dry_1hd
      module procedure get_cp_dry_2hd
   end interface get_cp_dry

   interface get_cp
      module procedure get_cp_1hd
      module procedure get_cp_2hd
   end interface get_cp

   interface get_R_dry
      module procedure get_R_dry_1hd
      module procedure get_R_dry_2hd
   end interface get_R_dry

   interface get_R
      module procedure get_R_1hd
      module procedure get_R_2hd
   end interface get_R

   interface get_mbarv
      module procedure get_mbarv_1hd
   end interface get_mbarv

CONTAINS

   ! Read namelist variables.
   subroutine air_composition_readnl(nlfile)
      use shr_nl_mod,     only: find_group_name => shr_nl_find_group_name
      use spmd_utils,     only: masterproc, mpicom, masterprocid
      use mpi,            only: mpi_character
      use cam_logfile,    only: iulog

      ! Dummy argument: filepath for file containing namelist input
      character(len=*), intent(in) :: nlfile

      ! Local variables
      integer                     :: unitn, ierr, indx
      integer,          parameter :: lsize = 76
      character(len=*), parameter :: subname = 'air_composition_readnl :: '
      character(len=lsize)        :: banner
      character(len=lsize)        :: bline

      ! Variable components of dry air and water species in air
      namelist /air_composition_nl/ dry_air_species, water_species_in_air
      !-----------------------------------------------------------------------

      banner = repeat('*', lsize)
      bline = "***"//repeat(' ', lsize - 6)//"***"

      ! Read variable components of dry air and water species in air
      dry_air_species = (/ (' ', indx = 1, num_names_max) /)
      water_species_in_air = (/ (' ', indx = 1, num_names_max) /)

      if (masterproc) then
         open(newunit=unitn, file=trim(nlfile), status='old')
         call find_group_name(unitn, 'air_composition_nl', status=ierr)
         if (ierr == 0) then
            read(unitn, air_composition_nl, iostat=ierr)
            if (ierr /= 0) then
               call endrun(subname//'ERROR reading namelist, air_composition_nl')
            end if
         end if
         close(unitn)
      end if

      call mpi_bcast(dry_air_species, len(dry_air_species)*num_names_max,     &
           mpi_character, masterprocid, mpicom, ierr)
      if (ierr /= 0) call endrun(subname//": FATAL: mpi_bcast: dry_air_species")
      call mpi_bcast(water_species_in_air,                                    &
           len(water_species_in_air)*num_names_max, mpi_character,            &
           masterprocid, mpicom, ierr)
      if (ierr /= 0) call endrun(subname//": FATAL: mpi_bcast: water_species_in_air")

      dry_air_species_num = 0
      water_species_in_air_num = 0
      do indx = 1, num_names_max
         if ( (LEN_TRIM(dry_air_species(indx)) > 0) .and.                     &
              (TRIM(dry_air_species(indx)) /= 'N2')) then
            dry_air_species_num = dry_air_species_num + 1
         end if
         if (LEN_TRIM(water_species_in_air(indx)) > 0) then
            water_species_in_air_num = water_species_in_air_num + 1
         end if
      end do

      ! Initialize number of thermodynamically active species
      thermodynamic_active_species_num =                                      &
           dry_air_species_num + water_species_in_air_num

      if (masterproc) then
         write(iulog, *) banner
         write(iulog, *) bline

         if (dry_air_species_num == 0) then
            write(iulog, *) " Thermodynamic properties of dry air are ",      &
                 "fixed at troposphere values"
         else
            write(iulog, *) " Thermodynamic properties of dry air are ",      &
                 "based on variable composition of the following species:"
            do indx = 1, dry_air_species_num
               write(iulog, *) '   ',  trim(dry_air_species(indx))
            end do
            write(iulog,*) ' '
         end if
         write(iulog,*) " Thermodynamic properties of moist air are ",        &
              "based on variable composition of the following water species:"
         do indx = 1, water_species_in_air_num
            write(iulog, *) '   ', trim(water_species_in_air(indx))
         end do
         write(iulog, *) bline
         write(iulog, *) banner
      end if

   end subroutine air_composition_readnl

   !===========================================================================

   subroutine air_composition_init()
      use string_utils, only: to_str
      use spmd_utils,   only: masterproc
      use cam_logfile,  only: iulog
      use physconst,    only: r_universal, cpair, rair, cpwv, rh2o, cpliq, cpice, mwdry, zvir, mwh2o
      use physics_types,only: ix_qv, ix_cld_liq, ix_rain !Remove once constituents are enabled - PEVERWHEE
      use physics_grid, only: pcols => columns_on_task
      use vert_coord,   only: pver

      integer  :: icnst, ix, isize, ierr, idx
      integer  :: liq_num, ice_num
      integer  :: liq_idx(water_species_in_air_num)
      integer  :: ice_idx(water_species_in_air_num)
      logical  :: has_liq, has_ice
      real(kind_phys) :: mw

      character(len=*), parameter :: subname = 'air_composition_init'

      !
      ! define cp and R for species in species_name
      !
      ! Last major species in namelist dry_air_species is derived from the
      !    other major species (since the sum of dry mixing ratios for
      !    major species of dry air add must add to one)
      !
      ! cv = R * dofx / 2;   cp = R * (1 + (dofx / 2))
      ! DOF == Degrees of Freedom
      ! dof1 = monatomic ideal gas, 3 translational DOF
      real(kind_phys), parameter :: dof1 = 3._kind_phys
      real(kind_phys), parameter :: cv1 = 0.5_kind_phys * r_universal * dof1
      real(kind_phys), parameter :: cp1 = 0.5_kind_phys * r_universal * (2._kind_phys + dof1)
      ! dof2 = diatomic ideal gas, 3 translational + 2 rotational = 5 DOF
      real(kind_phys), parameter :: dof2 = 5._kind_phys
      real(kind_phys), parameter :: cv2 = 0.5_kind_phys * r_universal * dof2
      real(kind_phys), parameter :: cp2 = 0.5_kind_phys * r_universal * (2._kind_phys + dof2)
      ! dof3 = polyatomic ideal gas, 3 translational + 3 rotational = 6 DOF
      real(kind_phys), parameter :: dof3 = 6._kind_phys
      real(kind_phys), parameter :: cv3 = 0.5_kind_phys * r_universal * dof3
      real(kind_phys), parameter :: cp3 = 0.5_kind_phys * r_universal * (2._kind_phys + dof3)

      liq_num = 0
      ice_num = 0
      has_liq = .false.
      has_ice = .false.
      ix = -1
      ! standard dry air (constant composition)
      o2_mwi = 1._kind_phys / 32._kind_phys
      n2_mwi = 1._kind_phys / 28._kind_phys
      mmro2 = 0.235_kind_phys
      mmrn2 = 0.765_kind_phys
      mbar = 1._kind_phys / ((mmro2 * o2_mwi) + (mmrn2 * n2_mwi))

      ! init for variable composition dry air

      isize = dry_air_species_num + water_species_in_air_num
      allocate(thermodynamic_active_species_idx(isize), stat=ierr)
      call check_allocate(ierr, subname,'thermodynamic_active_species_idx(isize)', &
                          file=__FILE__, line=__LINE__)
      allocate(thermodynamic_active_species_idx_dycore(isize), stat=ierr)
      call check_allocate(ierr, subname,'thermodynamic_active_species_idx_dycore(isize)', &
                          file=__FILE__, line=__LINE__)
      allocate(thermodynamic_active_species_cp(0:isize), stat=ierr)
      call check_allocate(ierr, subname,'thermodynamic_active_species_cp(0:isize)', &
                          file=__FILE__, line=__LINE__)
      allocate(thermodynamic_active_species_cv(0:isize), stat=ierr)
      call check_allocate(ierr, subname,'thermodynamic_active_species_cv(0:isize)', &
                          file=__FILE__, line=__LINE__)
      allocate(thermodynamic_active_species_R(0:isize), stat=ierr)
      call check_allocate(ierr, subname,'thermodynamic_active_species_R(0:isize)', &
                          file=__FILE__, line=__LINE__)

      isize = dry_air_species_num
      allocate(thermodynamic_active_species_mwi(0:isize), stat=ierr)
      call check_allocate(ierr, subname,'thermodynamic_active_species_mwi(0:isize)', &
                          file=__FILE__, line=__LINE__)
      allocate(thermodynamic_active_species_kv(0:isize), stat=ierr)
      call check_allocate(ierr, subname,'thermodynamic_active_species_kv(0:isize)', &
                          file=__FILE__, line=__LINE__)
      allocate(thermodynamic_active_species_kc(0:isize), stat=ierr)
      call check_allocate(ierr, subname,'thermodynamic_active_species_kc(0:isize)', &
                          file=__FILE__, line=__LINE__)
      !------------------------------------------------------------------------
      !  Allocate constituent dependent properties
      !------------------------------------------------------------------------
      allocate(cpairv(pcols,pver), stat=ierr)
      call check_allocate(ierr, subname,'cpairv(pcols,pver)', &
                          file=__FILE__, line=__LINE__)
      allocate(rairv(pcols,pver),  stat=ierr)
      call check_allocate(ierr, subname,'rairv(pcols,pver)', &
                          file=__FILE__, line=__LINE__)
      allocate(cappav(pcols,pver), stat=ierr)
      call check_allocate(ierr, subname,'cappav(pcols,pver)', &
                          file=__FILE__, line=__LINE__)
      allocate(mbarv(pcols,pver),  stat=ierr)
      call check_allocate(ierr, subname,'mbarv(pcols,pver)', &
                          file=__FILE__, line=__LINE__)
      allocate(zvirv(pcols,pver),  stat=ierr)
      call check_allocate(ierr, subname, 'zvirv(pcols,pver)', &
                          file=__FILE__, line=__LINE__)

      thermodynamic_active_species_idx        = -HUGE(1)
      thermodynamic_active_species_idx_dycore = -HUGE(1)
      thermodynamic_active_species_cp         = 0.0_kind_phys
      thermodynamic_active_species_cv         = 0.0_kind_phys
      thermodynamic_active_species_R          = 0.0_kind_phys
      thermodynamic_active_species_mwi        = 0.0_kind_phys
      thermodynamic_active_species_kv         = 0.0_kind_phys
      thermodynamic_active_species_kc         = 0.0_kind_phys
      !------------------------------------------------------------------------
      !  Initialize constituent dependent properties
      !------------------------------------------------------------------------
      cpairv(:pcols, :pver) = cpair
      rairv(:pcols,  :pver) = rair
      cappav(:pcols, :pver) = rair / cpair
      mbarv(:pcols,  :pver) = mwdry
      zvirv(:pcols,  :pver) = zvir
      !
      if (dry_air_species_num > 0) then
         !
         ! The last major species in dry_air_species is derived from the
         !    others and constants associated with it are initialized here
         !
         if (TRIM(dry_air_species(dry_air_species_num + 1)) == 'N2') then
#if 0
            call air_species_info('N', ix, mw)
            mw = 2.0_kind_phys * mw
            icnst = 0 ! index for the derived tracer N2
            thermodynamic_active_species_cp(icnst) = cp2 / mw
            thermodynamic_active_species_cv(icnst) = cv2 / mw !N2
            thermodynamic_active_species_R  (icnst) = r_universal / mw
            thermodynamic_active_species_mwi(icnst) = 1.0_kind_phys / mw
            thermodynamic_active_species_kv(icnst)  = kv2
            thermodynamic_active_species_kc(icnst)  = kc2
#endif
            !
            ! if last major species is not N2 then add code here
            !
         else
            write(iulog, *) subname, ' derived major species not found: ',    &
                 dry_air_species(dry_air_species_num)
            call endrun(subname//': derived major species not found')
         end if
      else
         !
         ! dry air is not species dependent
         !
         icnst = 0
         thermodynamic_active_species_cp (icnst) = cpair
         thermodynamic_active_species_cv (icnst) = cpair - rair
         thermodynamic_active_species_R  (icnst) = rair
      end if
      !
      !************************************************************************
      !
      ! add prognostic components of dry air
      !
      !************************************************************************
      !
      icnst = 1
      do idx = 1, dry_air_species_num
         select case (TRIM(dry_air_species(idx)))
            !
            ! O
            !
         case('O_mixing_ratio_wrt_dry_air')
!Un-comment once constituents are enabled - PEVERWHEE
#if 0
            call air_species_info('O', ix, mw)
            thermodynamic_active_species_idx(icnst) = ix
            thermodynamic_active_species_cp (icnst) = cp1 / mw
            thermodynamic_active_species_cv (icnst) = cv1 / mw
            thermodynamic_active_species_R  (icnst) = r_universal / mw
            thermodynamic_active_species_mwi(icnst) = 1.0_kind_phys / mw
            thermodynamic_active_species_kv(icnst)  = kv3
            thermodynamic_active_species_kc(icnst)  = kc3
            icnst = icnst + 1
#endif
            !
            ! O2
            !
         case('O2_mixing_ratio_wrt_dry_air')
!Un-comment once constituents are enabled - PEVERWHEE
#if 0
            call air_species_info('O2', ix, mw)
            thermodynamic_active_species_idx(icnst) = ix
            thermodynamic_active_species_cp (icnst) = cp2 / mw
            thermodynamic_active_species_cv (icnst) = cv2 / mw
            thermodynamic_active_species_R  (icnst) = r_universal / mw
            thermodynamic_active_species_mwi(icnst) = 1.0_kind_phys / mw
            thermodynamic_active_species_kv(icnst)  = kv1
            thermodynamic_active_species_kc(icnst)  = kc1
            icnst = icnst + 1
#endif
            !
            ! H
            !
         case('H_mixing_ratio_wrt_dry_air')
!Un-comment once constituents are enabled - PEVERWHEE
#if 0
            call air_species_info('H', ix, mw)
            thermodynamic_active_species_idx(icnst) = ix
            thermodynamic_active_species_cp (icnst) = cp1 / mw
            thermodynamic_active_species_cv (icnst) = cv1 / mw
            thermodynamic_active_species_R  (icnst) = r_universal / mw
            thermodynamic_active_species_mwi(icnst) = 1.0_kind_phys / mw
            ! Hydrogen not included in calculation of diffusivity and conductivity
            thermodynamic_active_species_kv(icnst)  = 0.0_kind_phys
            thermodynamic_active_species_kc(icnst)  = 0.0_kind_phys
            icnst = icnst + 1
#endif
            !
            ! If support for more major species is to be included add code here
            !
         case default
            write(iulog, *) subname, ' dry air component not found: ',        &
                 dry_air_species(idx)
            call endrun(subname//': dry air component not found')
         end select

         if (masterproc) then
            write(iulog, *) "Dry air composition ",                           &
                 TRIM(dry_air_species(idx)),                                  &
                 icnst-1,thermodynamic_active_species_idx(icnst-1),           &
                 thermodynamic_active_species_mwi(icnst-1),                   &
                 thermodynamic_active_species_cp(icnst-1),                    &
                 thermodynamic_active_species_cv(icnst-1)
         end if
      end do
      isize = dry_air_species_num+1
      icnst = 0 ! N2
      if(isize > 0) then
         if(masterproc) then
            write(iulog, *) "Dry air composition ",                           &
                 TRIM(dry_air_species(idx)),                                  &
                 icnst, -1, thermodynamic_active_species_mwi(icnst),          &
                 thermodynamic_active_species_cp(icnst),                      &
                 thermodynamic_active_species_cv(icnst)
         end if
      end if
      !
      !************************************************************************
      !
      ! Add non-dry components of moist air (water vapor and condensates)
      !
      !************************************************************************
      !
      icnst = dry_air_species_num + 1
      do idx = 1, water_species_in_air_num
         select case (TRIM(water_species_in_air(idx)))
            !
            ! Q
            !
         case('water_vapor_specific_humidity')
!            call air_species_info('Q', ix, mw) ! this should be uncommented once constituents are enabled - PEVERWHEE
            ix = ix_qv ! this should be removed once constituents are enabled
            mw = mwh2o !this should be removed once constituents are enabled
            thermodynamic_active_species_idx(icnst) = ix
            thermodynamic_active_species_cp (icnst) = cpwv
            thermodynamic_active_species_cv (icnst) = cv3 / mw
            thermodynamic_active_species_R  (icnst) = rh2o
            icnst = icnst + 1
            !
            ! CLDLIQ
            !
         case('cloud_liquid_water_mixing_ratio_of_moist_air')
!            call air_species_info('CLDLIQ', ix, mw) ! this should be uncommented once constituents are enabled - PEVERWHEE
            ix = ix_cld_liq ! this should be removed once constituents are enabled
            thermodynamic_active_species_idx(icnst) = ix
            thermodynamic_active_species_cp (icnst) = cpliq
            thermodynamic_active_species_cv (icnst) = cpliq
            liq_num           = liq_num+1
            liq_idx (liq_num) = ix
            icnst = icnst + 1
            has_liq = .true.
            !
            ! CLDICE
            !
         case('cloud_ice_mixing_ratio_of_moist_air')
!            call air_species_info('CLDICE', ix, mw) ! this should be uncommented once constituents are enabled - PEVERWHEE
            ix = -1 !Model should die if it gets here, until constituents are enabled - PEVERWHEE
            thermodynamic_active_species_idx(icnst) = ix
            thermodynamic_active_species_cp (icnst) = cpice
            thermodynamic_active_species_cv (icnst) = cpice
            ice_num           = ice_num+1
            ice_idx(ice_num)  = ix
            icnst = icnst + 1
            has_ice = .true.
            !
            ! RAINQM
            !
         case('rain_water_mixing_ratio')
!            call air_species_info('RAINQM', ix, mw) ! this should be uncommented once constituents are enabled - PEVERWHEE
            ix = ix_rain ! this should be removed once constituents are enabled - PEVERWHEE
            thermodynamic_active_species_idx(icnst) = ix
            thermodynamic_active_species_cp (icnst) = cpliq
            thermodynamic_active_species_cv (icnst) = cpliq
            liq_num           = liq_num+1
            liq_idx(liq_num)  = ix
            icnst = icnst + 1
            has_liq = .true.
            !
            ! SNOWQM
            !
         case('snow_water_mixing_ratio')
!            call air_species_info('SNOWQM', ix, mw) ! this should be uncommented once constituents are enabled - PEVERWHEE
            ix = -1 !Model should die if it gets here, until constituents are enabled - PEVERWHEE
            thermodynamic_active_species_idx(icnst) = ix
            thermodynamic_active_species_cp (icnst) = cpice
            thermodynamic_active_species_cv (icnst) = cpice
            ice_num           = ice_num+1
            ice_idx(ice_num)  = ix
            icnst = icnst + 1
            has_ice = .true.
            !
            ! GRAUQM
            !
         case('graupel_mixing_ratio')
!            call air_species_info('GRAUQM', ix, mw) ! this should be uncommented once constituents are enabled - PEVERWHEE
            ix = -1 !Model should die if it gets here, until constituents are enabled - PEVERWHEE
            thermodynamic_active_species_idx(icnst) = ix
            thermodynamic_active_species_cp (icnst) = cpice
            thermodynamic_active_species_cv (icnst) = cpice
            ice_num           = ice_num+1
            ice_idx(ice_num)  = ix
            icnst = icnst + 1
            has_ice = .true.
            !
            ! If support for more major species is to be included add code here
            !
         case default
            write(iulog, *) subname, ' moist air component not found: ',      &
                 water_species_in_air(idx)
            call endrun(subname//': moist air component not found')
         end select
         !
         !
         !
         if (masterproc) then
            write(iulog, *) "Thermodynamic active species ",                  &
                 TRIM(water_species_in_air(idx))
            write(iulog, *) "   global index                           : ",   &
                 icnst-1
            write(iulog, *) "   thermodynamic_active_species_idx       : ",   &
                 thermodynamic_active_species_idx(icnst-1)
            write(iulog, *) "   cp                                     : ",   &
                 thermodynamic_active_species_cp(icnst-1)
            write(iulog, *) "   cv                                     : ",   &
                 thermodynamic_active_species_cv(icnst-1)
            if (has_liq) then
               write(iulog, *) "   register phase (liquid or ice)         :", &
                    " liquid"
            end if
            if (has_ice) then
               write(iulog, *) "   register phase (liquid or ice)         :", &
                    " ice"
            end if
            write(iulog, *) "  "
         end if
         has_liq = .false.
         has_ice = .false.
      end do

      allocate(thermodynamic_active_species_liq_idx(liq_num), stat=ierr)
      call check_allocate(ierr, subname,'thermodynamic_active_species_liq_idx(liq_num)', &
                          file=__FILE__, line=__LINE__)
      allocate(thermodynamic_active_species_liq_idx_dycore(liq_num), stat=ierr)
      call check_allocate(ierr, subname,'thermodynamic_active_species_liq_idx_dycore(liq_num)', &
                          file=__FILE__, line=__LINE__)
      allocate(thermodynamic_active_species_ice_idx(ice_num), stat=ierr)
      call check_allocate(ierr, subname,'thermodynamic_active_species_ice_idx(ice_num)', &
                          file=__FILE__, line=__LINE__)
      allocate(thermodynamic_active_species_ice_idx_dycore(ice_num), stat=ierr)
      call check_allocate(ierr, subname,'thermodynamic_active_species_ice_idx_dycore(ice_num)', &
                          file=__FILE__, line=__LINE__)

      thermodynamic_active_species_liq_idx = liq_idx(1:liq_num)
      thermodynamic_active_species_liq_num = liq_num

      ! array initialized by the dycore
      thermodynamic_active_species_liq_idx_dycore = -99

      thermodynamic_active_species_ice_idx = ice_idx(1:ice_num)
      thermodynamic_active_species_ice_num = ice_num

      ! array initialized by the dycore
      thermodynamic_active_species_ice_idx_dycore = -99

      if (water_species_in_air_num /= 1 + liq_num+ice_num) then
         write(iulog, '(2a,2(i0,a))') subname,                                &
              "  water_species_in_air_num = ",                                &
              water_species_in_air_num, ", should be ",              &
              (1 + liq_num + ice_num), " (1 + liq_num + ice_num)"
         call endrun(subname//': water_species_in_air_num /= 1+liq_num+ice_num')
      end if
      enthalpy_reference_state = 'ice'
      if (masterproc) then
         write(iulog, *)   'Enthalpy reference state           : ',           &
              TRIM(enthalpy_reference_state)
      end if
   end subroutine air_composition_init

   !===========================================================================
   !-----------------------------------------------------------------------
   ! air_composition_update: Update the physics "constants" that vary
   !-------------------------------------------------------------------------
   !===========================================================================

   subroutine air_composition_update(mmr, ncol, to_moist_factor)

      real(kind_phys),           intent(in) :: mmr(:,:,:) ! constituents array
      integer,                   intent(in) :: ncol       ! number of columns
      real(kind_phys), optional, intent(in) :: to_moist_factor(:,:)

      call get_R_dry(mmr(:ncol, :, :), thermodynamic_active_species_idx, &
           rairv(:ncol, :), fact=to_moist_factor)
      call get_cp_dry(mmr(:ncol,:,:), thermodynamic_active_species_idx, &
           cpairv(:ncol,:), fact=to_moist_factor)
      call get_mbarv(mmr(:ncol,:,:), thermodynamic_active_species_idx, &
           mbarv(:ncol,:), fact=to_moist_factor)

      cappav(:ncol,:) = rairv(:ncol,:) / cpairv(:ncol,:)

   end subroutine air_composition_update

   !===========================================================================
   !***************************************************************************
   !
   ! get_cp_dry: Compute dry air heat capacity under constant pressure
   !
   !***************************************************************************
   !
   subroutine get_cp_dry_1hd(tracer, active_species_idx, cp_dry, fact)
      use cam_abortutils,  only: endrun
      use string_utils,    only: to_str
      use physconst,       only: cpair

      ! Dummy arguments
      ! tracer: tracer array
      real(kind_phys),           intent(in)  :: tracer(:,:,:)
      integer,                   intent(in)  :: active_species_idx(:)
      ! fact: optional dry pressure level thickness
      real(kind_phys), optional, intent(in)  :: fact(:,:)
      ! cp_dry: dry air heat capacity under constant pressure
      real(kind_phys),           intent(out) :: cp_dry(:,:)

      ! Local variables
      integer  :: idx, kdx , m_cnst, qdx
      ! factor: dry pressure level thickness
      real(kind_phys) :: factor(SIZE(cp_dry, 1), SIZE(cp_dry, 2))
      real(kind_phys) :: residual(SIZE(cp_dry, 1), SIZE(cp_dry, 2))
      real(kind_phys) :: mmr
      character(len=*), parameter :: subname = 'get_cp_dry_1hd: '

      if (dry_air_species_num == 0) then
         ! dry air heat capacity not species dependent
         cp_dry = cpair
      else
         ! dry air heat capacity is species dependent
         if (present(fact)) then
            if (SIZE(fact, 1) /= SIZE(factor, 1)) then
               call endrun(subname//"SIZE mismatch in dimension 1 "//         &
                    to_str(SIZE(fact, 1))//' /= '//to_str(SIZE(factor, 1)))
            end if
            if (SIZE(fact, 2) /= SIZE(factor, 2)) then
               call endrun(subname//"SIZE mismatch in dimension 2 "//         &
                    to_str(SIZE(fact, 2))//' /= '//to_str(SIZE(factor, 2)))
            end if
            factor = fact(:,:)
         else
            factor = 1.0_kind_phys
         end if

         cp_dry = 0.0_kind_phys
         residual = 1.0_kind_phys
         do qdx = 1, dry_air_species_num
            m_cnst = active_species_idx(qdx)
            do kdx = 1, SIZE(cp_dry, 2)
               do idx = 1, SIZE(cp_dry, 1)
                  mmr = tracer(idx, kdx, m_cnst) * factor(idx, kdx)
                  cp_dry(idx, kdx) = cp_dry(idx, kdx) +             &
                       (thermodynamic_active_species_cp(qdx) * mmr)
                  residual(idx, kdx) = residual(idx, kdx) - mmr
               end do
            end do
         end do
         qdx = 0 ! N2
         do kdx = 1, SIZE(cp_dry, 2)
            do idx = 1, SIZE(cp_dry, 1)
               cp_dry(idx, kdx) = cp_dry(idx, kdx) +                          &
                    (thermodynamic_active_species_cp(qdx) * residual(idx, kdx))
            end do
         end do
      end if
   end subroutine get_cp_dry_1hd

   !===========================================================================

   subroutine get_cp_dry_2hd(tracer, active_species_idx, cp_dry, fact)
      ! Version of get_cp_dry for arrays that have a second horizontal index

      ! Dummy arguments
      ! tracer: tracer array
      real(kind_phys),           intent(in)  :: tracer(:,:,:,:)
      integer,                   intent(in)  :: active_species_idx(:)
      ! fact:        optional dry pressure level thickness
      real(kind_phys), optional, intent(in)  :: fact(:,:,:)
      ! cp_dry: dry air heat capacity under constant pressure
      real(kind_phys),           intent(out) :: cp_dry(:,:,:)

      ! Local variable
      integer  :: jdx

      do jdx = 1, SIZE(cp_dry, 2)
         if (present(fact)) then
            call get_cp_dry(tracer(:,jdx,:,:), active_species_idx,            &
                 cp_dry(:,jdx,:), fact=fact(:,jdx,:))
         else
            call get_cp_dry(tracer(:,jdx,:,:), active_species_idx,            &
                 cp_dry(:,jdx,:))
         end if
      end do

   end subroutine get_cp_dry_2hd

   !===========================================================================
   !
   !***************************************************************************
   !
   ! get_cp: Compute generalized heat capacity at constant pressure
   !
   !***************************************************************************
   !
   subroutine get_cp_1hd(tracer, inv_cp, cp, dp_dry, active_species_idx_dycore)
      use cam_abortutils,  only: endrun
      use string_utils,    only: to_str

      ! Dummy arguments
      ! tracedr: Tracer array
      real(kind_phys),           intent(in)  :: tracer(:,:,:)
      real(kind_phys), optional, intent(in)  :: dp_dry(:,:)
      ! inv_cp: output inverse cp instead of cp
      logical,                   intent(in)  :: inv_cp
      real(kind_phys),           intent(out) :: cp(:,:)
      ! active_species_idx_dycore: array of indices for index of
      !    thermodynamic active species in dycore tracer array
      !    (if different from physics index)
      integer,         optional, intent(in)  :: active_species_idx_dycore(:)

      ! Local variables
      integer  :: qdx, itrac
      real(kind_phys) :: sum_species(SIZE(cp, 1), SIZE(cp, 2))
      real(kind_phys) :: sum_cp(SIZE(cp, 1), SIZE(cp, 2))
      real(kind_phys) :: factor(SIZE(cp, 1), SIZE(cp, 2))
      integer  :: idx_local(thermodynamic_active_species_num)
      character(len=*), parameter :: subname = 'get_cp_1hd: '

      if (present(active_species_idx_dycore)) then
         if (SIZE(active_species_idx_dycore) /=                               &
              thermodynamic_active_species_num) then
            call endrun(subname//"SIZE mismatch "//                           &
                 to_str(SIZE(active_species_idx_dycore))//' /= '//           &
                 to_str(thermodynamic_active_species_num))
        end if
         idx_local = active_species_idx_dycore
      else
         idx_local = thermodynamic_active_species_idx
      end if

      if (present(dp_dry)) then
         factor = 1.0_kind_phys / dp_dry
      else
         factor = 1.0_kind_phys
      end if
      sum_species = 1.0_kind_phys ! all dry air species sum to 1
      do qdx = dry_air_species_num + 1, thermodynamic_active_species_num
         itrac = idx_local(qdx)
         sum_species(:,:) = sum_species(:,:) +                            &
              (tracer(:,:,itrac) * factor(:,:))
      end do

      if (dry_air_species_num == 0) then
         sum_cp = thermodynamic_active_species_cp(0)
      else
         call get_cp_dry(tracer, idx_local, sum_cp, fact=factor)
      end if
      do qdx = dry_air_species_num + 1, thermodynamic_active_species_num
         itrac = idx_local(qdx)
         sum_cp(:,:) = sum_cp(:,:) +                                      &
              (thermodynamic_active_species_cp(qdx) * tracer(:,:,itrac) *   &
              factor(:,:))
      end do
      if (inv_cp) then
         cp = sum_species / sum_cp
      else
         cp = sum_cp / sum_species
      end if

   end subroutine get_cp_1hd

   !===========================================================================

   subroutine get_cp_2hd(tracer, inv_cp, cp, dp_dry, active_species_idx_dycore)
      ! Version of get_cp for arrays that have a second horizontal index
      use cam_abortutils, only: endrun
      use string_utils,   only: to_str

      ! Dummy arguments
      ! tracer: Tracer array
      real(kind_phys),           intent(in)  :: tracer(:,:,:,:)
      real(kind_phys), optional, intent(in)  :: dp_dry(:,:,:)
      ! inv_cp: output inverse cp instead of cp
      logical,                   intent(in)  :: inv_cp
      real(kind_phys),           intent(out) :: cp(:,:,:)
      ! active_species_idx_dycore: array of indicies for index of
      !    thermodynamic active species in dycore tracer array
      !    (if different from physics index)
      integer, optional,  intent(in)  :: active_species_idx_dycore(:)

      ! Local variables
      integer  :: jdx
      integer  :: idx_local(thermodynamic_active_species_num)
      character(len=*), parameter :: subname = 'get_cp_2hd: '

      do jdx = 1, SIZE(cp, 2)
         if (present(dp_dry)) then
            call get_cp(tracer(:, jdx, :, :), inv_cp, cp(:, jdx, :),          &
                 dp_dry=dp_dry(:, jdx, :), active_species_idx_dycore=active_species_idx_dycore)
         else
            call get_cp(tracer(:, jdx, :, :), inv_cp, cp(:, jdx, :),          &
                 active_species_idx_dycore=active_species_idx_dycore)
         end if
      end do

   end subroutine get_cp_2hd

   !===========================================================================

   !***************************************************************************
   !
   ! get_R_dry: Compute generalized dry air gas constant R
   !
   !***************************************************************************
   !
   subroutine get_R_dry_1hd(tracer, active_species_idx_dycore, R_dry, fact)
      use physconst,       only: rair

      ! tracer: tracer array
      real(kind_phys),           intent(in)  :: tracer(:, :, :)
      ! active_species_idx_dycore: index of active species in tracer
      integer,                   intent(in)  :: active_species_idx_dycore(:)
      ! R_dry: dry air R
      real(kind_phys),           intent(out) :: R_dry(:, :)
      ! fact:   optional factor for converting tracer to dry mixing ratio
      real(kind_phys), optional, intent(in)  :: fact(:, :)

      ! Local variables
      integer  :: idx, kdx, m_cnst, qdx
      real(kind_phys) :: factor(SIZE(tracer, 1), SIZE(tracer, 2))
      real(kind_phys) :: residual(SIZE(R_dry, 1), SIZE(R_dry, 2))
      real(kind_phys) :: mmr

      if (dry_air_species_num == 0) then
         !
         ! dry air not species dependent
         !
         R_dry = rair
      else
         if (present(fact)) then
            factor = fact(:,:)
         else
            factor = 1.0_kind_phys
         end if

         R_dry = 0.0_kind_phys
         residual = 1.0_kind_phys
         do qdx = 1, dry_air_species_num
            m_cnst = active_species_idx_dycore(qdx)
            do kdx = 1, SIZE(R_dry, 2)
               do idx = 1, SIZE(R_dry, 1)
                  mmr = tracer(idx, kdx, m_cnst) * factor(idx, kdx)
                  R_dry(idx, kdx) = R_dry(idx, kdx) +                         &
                       (thermodynamic_active_species_R(qdx) * mmr)
                  residual(idx, kdx) = residual(idx, kdx) - mmr
               end do
            end do
         end do
         !
         ! N2 derived from the others
         !
         qdx = 0
         do kdx = 1, SIZE(R_dry, 2)
            do idx = 1, SIZE(R_dry, 1)
               R_dry(idx, kdx) = R_dry(idx, kdx) +                            &
                    (thermodynamic_active_species_R(qdx) * residual(idx, kdx))
            end do
         end do
      end if
   end subroutine get_R_dry_1hd

   !===========================================================================

   subroutine get_R_dry_2hd(tracer, active_species_idx_dycore, R_dry, fact)
      ! Version of get_R_dry for arrays that have a second horizontal index

      ! tracer: tracer array
      real(kind_phys),           intent(in)  :: tracer(:, :, :, :)
      ! active_species_idx_dycore: index of active species in tracer
      integer,            intent(in)  :: active_species_idx_dycore(:)
      ! R_dry: dry air R
      real(kind_phys),           intent(out) :: R_dry(:, :, :)
      ! fact:   optional factor for converting tracer to dry mixing ratio
      real(kind_phys), optional, intent(in)  :: fact(:, :, :)

      ! Local variable
      integer  :: jdx

      do jdx = 1, SIZE(tracer, 2)
         if (present(fact)) then
            call get_R_dry(tracer(:, jdx, :, :), active_species_idx_dycore,      &
                 R_dry(:, jdx, :), fact=fact(:, jdx, :))
         else
            call get_R_dry(tracer(:, jdx, :, :), active_species_idx_dycore,      &
                 R_dry(:, jdx, :))
         end if
      end do

   end subroutine get_R_dry_2hd

   !===========================================================================
   !
   !***************************************************************************
   !
   ! get_R: Compute generalized R
   !        This code (both 1hd and 2hd) is currently unused and untested
   !
   !***************************************************************************
   !
   subroutine get_R_1hd(tracer, active_species_idx, R, fact)
      use cam_abortutils,  only: endrun
      use string_utils,    only: to_str

      ! Dummy arguments
      ! tracer: !tracer array
      real(kind_phys), intent(in)  :: tracer(:, :, :)
      ! active_species_idx: index of active species in tracer
      integer,         intent(in)  :: active_species_idx(:)
      ! R: generalized gas constant
      real(kind_phys), intent(out) :: R(:, :)
      ! fact: optional factor for converting tracer to dry mixing ratio
      real(kind_phys), optional, intent(in) :: fact(:, :)

      ! Local variables
      integer  :: qdx, itrac
      real(kind_phys) :: factor(SIZE(tracer, 1), SIZE(tracer, 2))
      real(kind_phys) :: sum_species(SIZE(R, 1), SIZE(R, 2))
      integer  :: idx_local(thermodynamic_active_species_num)

      character(len=*), parameter :: subname = 'get_R_1hd: '

      if (present(fact)) then
         if (SIZE(fact, 1) /= SIZE(factor, 1)) then
            call endrun(subname//"SIZE mismatch in dimension 1 "//            &
                 to_str(SIZE(fact, 1))//' /= '//to_str(SIZE(factor, 1)))
         end if
         if (SIZE(fact, 2) /= SIZE(factor, 2)) then
            call endrun(subname//"SIZE mismatch in dimension 2 "//            &
                 to_str(SIZE(fact, 2))//' /= '//to_str(SIZE(factor, 2)))
         end if
         call get_R_dry(tracer, active_species_idx, R, fact=fact)
         factor = fact(:,:)
      else
         call get_R_dry(tracer, active_species_idx, R)
         factor = 1.0_kind_phys
      end if
      idx_local = active_species_idx
      sum_species = 1.0_kind_phys ! all dry air species sum to 1
      do qdx = dry_air_species_num + 1, thermodynamic_active_species_num
         itrac = idx_local(qdx)
         sum_species(:,:) = sum_species(:,:) +                            &
              (tracer(:,:,itrac) * factor(:,:))
      end do
      do qdx = dry_air_species_num + 1, thermodynamic_active_species_num
         itrac = idx_local(qdx)
         R(:,:) = R(:,:) +                                                &
              (thermodynamic_active_species_R(qdx) * tracer(:,:,itrac) *    &
              factor(:,:))
      end do
      R = R / sum_species
   end subroutine get_R_1hd

   !===========================================================================

   subroutine get_R_2hd(tracer, active_species_idx, R, fact)

      ! Dummy arguments
      ! tracer: !tracer array
      real(kind_phys), intent(in)  :: tracer(:, :, :, :)
      ! active_species_idx: index of active species in tracer
      integer,         intent(in)  :: active_species_idx(:)
      ! R: generalized gas constant
      real(kind_phys), intent(out) :: R(:, :, :)
      ! fact: optional factor for converting tracer to dry mixing ratio
      real(kind_phys), optional, intent(in) :: fact(:, :, :)

      ! Local variable
      integer  :: jdx

      do jdx = 1, SIZE(tracer, 2)
         if (present(fact)) then
            call get_R(tracer(:, jdx, :, :), active_species_idx,          &
                 R(:, jdx, :), fact=fact(:, jdx, :))
         else
            call get_R(tracer(:, jdx, :, :), active_species_idx,          &
                 R(:, jdx, :))
         end if
      end do

   end subroutine get_R_2hd

   !===========================================================================

   !*************************************************************************************************************************
   !
   ! compute molecular weight dry air
   !
   !*************************************************************************************************************************
   !
   subroutine get_mbarv_1hd(tracer, active_species_idx, mbarv_in, fact)
     use physconst,        only: mwdry, rair, cpair
     real(kind_phys), intent(in)  :: tracer(:,:,:)                      !tracer array
     integer,         intent(in)  :: active_species_idx(:)              !index of active species in tracer
     real(kind_phys), intent(out) :: mbarv_in(:,:)                      !molecular weight of dry air
     real(kind_phys), optional, intent(in) :: fact(:,:)                 !factor for converting tracer to dry mixing ratio

     integer :: idx, kdx, m_cnst, qdx
     real(kind_phys):: factor(SIZE(mbarv_in, 1), SIZE(mbarv_in, 2))
     real(kind_phys):: residual(SIZE(tracer, 1), SIZE(mbarv_in, 2))
     real(kind_phys):: mm
     !
     ! dry air not species dependent
     !
     if (dry_air_species_num==0) then
       mbarv_in = mwdry
     else
       if (present(fact)) then
         factor(:,:) = fact(:,:)
       else
         factor(:,:) = 1.0_kind_phys
       endif

       mbarv_in = 0.0_kind_phys
       residual = 1.0_kind_phys
       do qdx = 1, dry_air_species_num
         m_cnst = active_species_idx(qdx)
         do kdx = 1, SIZE(mbarv_in, 2)
           do idx = 1, SIZE(mbarv_in, 1)
             mm = tracer(idx, kdx, m_cnst) * factor(idx, kdx)
             mbarv_in(idx, kdx) = mbarv_in(idx, kdx) + thermodynamic_active_species_mwi(qdx) * mm
             residual(idx, kdx) = residual(idx, kdx) - mm
           end do
         end do
       end do
       qdx = 0 ! N2
       do kdx = 1, SIZE(mbarv_in, 2)
         do idx = 1, SIZE(mbarv_in, 1)
           mbarv_in(idx, kdx) = mbarv_in(idx, kdx) + thermodynamic_active_species_mwi(qdx) * residual(idx, kdx)
         end do
       end do
       mbarv_in(:,:) = 1.0_kind_phys / mbarv_in(:,:)
     end if
   end subroutine get_mbarv_1hd

   !===========================================================================

   subroutine air_species_info(name, index, molec_weight, caller)
      use cam_abortutils, only: endrun
      use cam_logfile,    only: iulog
      !!XXgoldyXX: v until we get constituents figured out in CCPP
#if 0
      !!XXgoldyXX: ^  until we get constituents figured out in CCPP
      use constituents, only: cnst_get_ind, cnst_mw
      !!XXgoldyXX: v until we get constituents figured
      !out in CCPP
#endif
      !!XXgoldyXX: ^  until we get constituents
      !figured out in CCPP
      ! Find the constituent index of <name> and return it in
      !    <index>. Return the constituent molecular weight in
      !    <molec_weight>

      ! Dummy arguments
      character(len=*),           intent(in)    :: name
      integer,                    intent(inout) :: index
      real(kind_phys),            intent(out)   :: molec_weight
      character(len=*), optional, intent(in)    :: caller
      ! Local parameter
      character(len=*), parameter :: subname = 'air_species_info: '

      ! vv commented out until we get constituents figured out - PEVERWHEE
      !   until then, the model will die if this subroutine is called
      !call cnst_get_ind(trim(name), index, abort=.false.)
      ! ^^ commented out until we get constituents figured out - PEVERWHEE
      !   until then, the model will die if this subroutine is called
      if (index < 1) then
         if (present(caller)) then
            write(iulog, *) trim(caller), ": air component not found, '", &
                 trim(name), "'"
            call endrun(trim(caller)//": air component not found, '"//    &
                 trim(name)//"'")
         else
            write(iulog, *) subname, "air component not found, '",        &
                 trim(name), "'"
            call endrun(subname//"air component not found, '"//           &
                 trim(name)//"'")
         end if
      else
      ! vv commented out until we get constituents figured out - PEVERWHEE
      !   molec_weight = cnst_mw(index)
      ! ^^ commented out until we get constituents figured out - PEVERWHEE
      end if

   end subroutine air_species_info


end module air_composition
