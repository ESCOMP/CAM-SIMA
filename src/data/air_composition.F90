! air_composition module defines major species of the atmosphere and manages
! the physical properties that are dependent on the composition of air
module air_composition

   use ccpp_kinds,           only: kind_phys
   use cam_abortutils,       only: endrun, check_allocate
   use runtime_obj,          only: unset_real, unset_int
   use phys_vars_init_check, only: std_name_len
   use physics_types,        only: cpairv, rairv, cappav, mbarv, zvirv

   implicit none
   private
   save

   public  :: air_composition_init
   public  :: dry_air_composition_update
   public  :: water_composition_update

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
   ! update_zvirv: update composition-dependent zvir
   public :: update_zvirv

   private :: air_species_info

   ! composition of air
   !
   logical, protected, public, allocatable  :: const_is_water_species(:)
   integer, protected, public  :: dry_air_species_num
   integer, protected, public  :: water_species_in_air_num

   ! Thermodynamic variables
   integer,                      protected, public :: thermodynamic_active_species_num = unset_int
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
   integer,               protected, public :: thermodynamic_active_species_liq_num = unset_int
   ! thermodynamic_active_species_ice_num: number of frozen water species
   integer,               protected, public :: thermodynamic_active_species_ice_num = unset_int
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
   real(kind_phys), public, protected :: mmro2 = unset_real  ! Mass mixing ratio of O2
   real(kind_phys), public, protected :: mmrn2 = unset_real  ! Mass mixing ratio of N2
   real(kind_phys), public, protected :: o2_mwi = unset_real ! Inverse mol. weight of O2
   real(kind_phys), public, protected :: n2_mwi = unset_real ! Inverse mol. weight of N2
   real(kind_phys), public, protected :: mbar = unset_real   ! Mean mass at mid level

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

   !===========================================================================

   subroutine air_composition_init()
      use shr_kind_mod,         only: shr_kind_cl
      use string_utils,         only: to_str
      use spmd_utils,           only: masterproc
      use cam_logfile,          only: iulog
      use physconst,            only: r_universal, cpwv
      use physconst,            only: rh2o, cpliq, cpice
      use physconst,            only: cpair, rair
      use physics_grid,         only: pcols => columns_on_task
      use vert_coord,           only: pver
      use runtime_obj,          only: wv_stdname
      use cam_constituents,     only: const_name, num_advected
      use cam_constituents,     only: const_set_thermo_active
      use cam_constituents,     only: const_set_water_species

      integer  :: icnst, ix, ierr, idx
      integer  :: liq_num, ice_num, water_species_num, dry_species_num
      integer  :: liq_idx(num_advected)
      integer  :: ice_idx(num_advected)
      logical  :: has_liq, has_ice
      real(kind_phys) :: mw
      character(len=std_name_len) :: cnst_stdname

      character(len=*), parameter :: subname = 'air_composition_init'
      character(len=shr_kind_cl)  :: errmsg

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

      errmsg = ''

      liq_num = 0
      ice_num = 0
      ix = -1
      ! standard dry air (constant composition)
      o2_mwi = 1._kind_phys / 32._kind_phys
      n2_mwi = 1._kind_phys / 28._kind_phys
      mmro2 = 0.235_kind_phys
      mmrn2 = 0.765_kind_phys
      mbar = 1._kind_phys / ((mmro2 * o2_mwi) + (mmrn2 * n2_mwi))

      ! init for variable composition dry air

      allocate(thermodynamic_active_species_idx(0:num_advected), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname,'thermodynamic_active_species_idx(num_advected)', &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)
      allocate(thermodynamic_active_species_idx_dycore(num_advected), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname,'thermodynamic_active_species_idx_dycore(num_advected)', &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)
      allocate(thermodynamic_active_species_cp(0:num_advected), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname,'thermodynamic_active_species_cp(0:num_advected)', &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)
      allocate(thermodynamic_active_species_cv(0:num_advected), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname,'thermodynamic_active_species_cv(0:num_advected)', &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)
      allocate(thermodynamic_active_species_R(0:num_advected), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname,'thermodynamic_active_species_R(0:num_advected)', &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)

      allocate(thermodynamic_active_species_mwi(0:num_advected), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname,'thermodynamic_active_species_mwi(0:num_advected)', &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)
      allocate(thermodynamic_active_species_kv(0:num_advected), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname,'thermodynamic_active_species_kv(0:num_advected)', &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)
      allocate(thermodynamic_active_species_kc(0:num_advected), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname,'thermodynamic_active_species_kc(0:num_advected)', &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)
      allocate(const_is_water_species(num_advected), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'const_is_water_species', file=__FILE__, line=__LINE__, errmsg=errmsg)

      thermodynamic_active_species_idx        = -HUGE(1)
      thermodynamic_active_species_idx_dycore = -HUGE(1)
      thermodynamic_active_species_cp         = 0.0_kind_phys
      thermodynamic_active_species_cv         = 0.0_kind_phys
      thermodynamic_active_species_R          = 0.0_kind_phys
      thermodynamic_active_species_mwi        = 0.0_kind_phys
      thermodynamic_active_species_kv         = 0.0_kind_phys
      thermodynamic_active_species_kc         = 0.0_kind_phys
      const_is_water_species                  = .false.

      !
      !************************************************************************
      !
      ! add prognostic components of air
      !
      !************************************************************************
      !
      icnst = 1
      water_species_num = 0
      dry_species_num = 0
      do idx = 1, num_advected
         cnst_stdname = const_name(idx)
         has_liq = .false.
         has_ice = .false.
         select case (TRIM((cnst_stdname)))
            !
            ! O
            !
         case('O_mixing_ratio_wrt_dry_air')
            call air_species_info('O_mixing_ratio_wrt_dry_air', ix, mw)
            thermodynamic_active_species_idx(icnst) = ix
            thermodynamic_active_species_cp (icnst) = cp1 / mw
            thermodynamic_active_species_cv (icnst) = cv1 / mw
            thermodynamic_active_species_R  (icnst) = r_universal / mw
            thermodynamic_active_species_mwi(icnst) = 1.0_kind_phys / mw
            thermodynamic_active_species_kv(icnst)  = kv3
            thermodynamic_active_species_kc(icnst)  = kc3
            icnst = icnst + 1
            dry_species_num = dry_species_num + 1
            !Notify constituent object that this species is
            !thermodynamically active
            call const_set_thermo_active(idx, .true.)
            !
            ! O2
            !
         case('O2_mixing_ratio_wrt_dry_air')
            call air_species_info('O2_mixing_ratio_wrt_dry_air', ix, mw)
            thermodynamic_active_species_idx(icnst) = ix
            thermodynamic_active_species_cp (icnst) = cp2 / mw
            thermodynamic_active_species_cv (icnst) = cv2 / mw
            thermodynamic_active_species_R  (icnst) = r_universal / mw
            thermodynamic_active_species_mwi(icnst) = 1.0_kind_phys / mw
            thermodynamic_active_species_kv(icnst)  = kv1
            thermodynamic_active_species_kc(icnst)  = kc1
            icnst = icnst + 1
            dry_species_num = dry_species_num + 1
            !Notify constituent object that this species is
            !thermodynamically active
            call const_set_thermo_active(idx, .true.)
            !
            ! H
            !
         case('H_mixing_ratio_wrt_dry_air')
            call air_species_info('H_mixing_ratio_wrt_dry_air', ix, mw)
            thermodynamic_active_species_idx(icnst) = ix
            thermodynamic_active_species_cp (icnst) = cp1 / mw
            thermodynamic_active_species_cv (icnst) = cv1 / mw
            thermodynamic_active_species_R  (icnst) = r_universal / mw
            thermodynamic_active_species_mwi(icnst) = 1.0_kind_phys / mw
            ! Hydrogen not included in calculation of diffusivity and conductivity
            thermodynamic_active_species_kv(icnst)  = 0.0_kind_phys
            thermodynamic_active_species_kc(icnst)  = 0.0_kind_phys
            icnst = icnst + 1
            dry_species_num = dry_species_num + 1
            !Notify constituent object that this species is
            !thermodynamically active
            call const_set_thermo_active(idx, .true.)
            !
            ! N2
            !
         case('N2_mixing_ratio_wrt_dry_air')
            call air_species_info('N2_mixing_ratio_wrt_dry_air', ix, mw)
            mw = 2.0_kind_phys * mw
            icnst = 0 ! index for the derived tracer N2
            thermodynamic_active_species_cp(icnst) = cp2 / mw
            thermodynamic_active_species_cv(icnst) = cv2 / mw !N2
            thermodynamic_active_species_R  (icnst) = r_universal / mw
            thermodynamic_active_species_mwi(icnst) = 1.0_kind_phys / mw
            thermodynamic_active_species_kv(icnst)  = kv2
            thermodynamic_active_species_kc(icnst)  = kc2
            !Notify constituent object that this species is
            !thermodynamically active
            call const_set_thermo_active(idx, .true.)
            !
            ! Q
            !
         case(wv_stdname) !water_vapor_mixing_ratio_wrt_moist_air_and_condensed_water
            call air_species_info(wv_stdname, ix, mw)
            wv_idx = ix ! set water species index for use in get_hydrostatic_energy
            thermodynamic_active_species_idx(icnst) = ix
            thermodynamic_active_species_cp (icnst) = cpwv
            thermodynamic_active_species_cv (icnst) = cv3 / mw
            thermodynamic_active_species_R  (icnst) = rh2o
            icnst = icnst + 1
            water_species_num = water_species_num + 1
            const_is_water_species(ix) = .true.
            !Notify constituent object that this species is
            !thermodynamically active
            call const_set_thermo_active(idx, .true.)
            !Notify constituent object that this species is
            !a water species
            call const_set_water_species(idx, .true.)
            !
            ! CLDLIQ
            !
         case('cloud_liquid_water_mixing_ratio_wrt_moist_air_and_condensed_water')
            call air_species_info('cloud_liquid_water_mixing_ratio_wrt_moist_air_and_condensed_water', &
                                  ix, mw)
            thermodynamic_active_species_idx(icnst) = ix
            thermodynamic_active_species_cp (icnst) = cpliq
            thermodynamic_active_species_cv (icnst) = cpliq
            liq_num           = liq_num+1
            liq_idx (liq_num) = ix
            icnst = icnst + 1
            water_species_num = water_species_num + 1
            has_liq = .true.
            const_is_water_species(ix) = .true.
            !Notify constituent object that this species is
            !thermodynamically active
            call const_set_thermo_active(idx, .true.)
            !Notify constituent object that this species is
            !a water species
            call const_set_water_species(idx, .true.)
            !
            ! CLDICE
            !
         case('cloud_ice_mixing_ratio_wrt_moist_air_and_condensed_water')
            call air_species_info('cloud_ice_mixing_ratio_wrt_moist_air_and_condensed_water', &
                                  ix, mw)
            thermodynamic_active_species_idx(icnst) = ix
            thermodynamic_active_species_cp (icnst) = cpice
            thermodynamic_active_species_cv (icnst) = cpice
            ice_num           = ice_num+1
            ice_idx(ice_num)  = ix
            icnst = icnst + 1
            water_species_num = water_species_num + 1
            has_ice = .true.
            const_is_water_species(ix) = .true.
            !Notify constituent object that this species is
            !thermodynamically active
            call const_set_thermo_active(idx, .true.)
            !Notify constituent object that this species is
            !a water species
            call const_set_water_species(idx, .true.)

            !
            ! RAINQM
            !
         case('rain_mixing_ratio_wrt_moist_air_and_condensed_water')
            call air_species_info('rain_mixing_ratio_wrt_moist_air_and_condensed_water', ix, mw)
            thermodynamic_active_species_idx(icnst) = ix
            thermodynamic_active_species_cp (icnst) = cpliq
            thermodynamic_active_species_cv (icnst) = cpliq
            liq_num           = liq_num+1
            liq_idx(liq_num)  = ix
            icnst = icnst + 1
            water_species_num = water_species_num + 1
            has_liq = .true.
            const_is_water_species(ix) = .true.
            !Notify constituent object that this species is
            !thermodynamically active
            call const_set_thermo_active(idx, .true.)
            !Notify constituent object that this species is
            !a water species
            call const_set_water_species(idx, .true.)
            !
            ! SNOWQM
            !
         case('snow_mixing_ratio_wrt_moist_air_and_condensed_water')
            call air_species_info('snow_mixing_ratio_wrt_moist_air_and_condensed_water', ix, mw)
            thermodynamic_active_species_idx(icnst) = ix
            thermodynamic_active_species_cp (icnst) = cpice
            thermodynamic_active_species_cv (icnst) = cpice
            ice_num           = ice_num+1
            ice_idx(ice_num)  = ix
            icnst = icnst + 1
            water_species_num = water_species_num + 1
            has_ice = .true.
            const_is_water_species(ix) = .true.
            !Notify constituent object that this species is
            !thermodynamically active
            call const_set_thermo_active(idx, .true.)
            !Notify constituent object that this species is
            !a water species
            call const_set_water_species(idx, .true.)
            !
            ! GRAUQM
            !
         case('graupel_water_mixing_ratio_wrt_moist_air_and_condensed_water')
            call air_species_info('graupel_water_mixing_ratio_wrt_moist_air_and_condensed_water', &
                                  ix, mw)
            thermodynamic_active_species_idx(icnst) = ix
            thermodynamic_active_species_cp (icnst) = cpice
            thermodynamic_active_species_cv (icnst) = cpice
            ice_num           = ice_num+1
            ice_idx(ice_num)  = ix
            icnst = icnst + 1
            water_species_num = water_species_num + 1
            has_ice = .true.
            const_is_water_species(ix) = .true.
            !Notify constituent object that this species is
            !thermodynamically active
            call const_set_thermo_active(idx, .true.)
            !Notify constituent object that this species is
            !a water species
            call const_set_water_species(idx, .true.)
            !
            ! If support for more major species is to be included add code here
            !
         case default
            cycle ! skip thermodynamically inactive species
         end select

         if (masterproc) then
            write(iulog, *) "Thermodynamic active species ",                  &
                 TRIM(cnst_stdname)
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
      end do

      !Set dry air thermodynamic properities if no dry air species provided:
      if (dry_species_num == 0) then
        !Note:  The zeroeth index is used to represent all of dry
        !       air instead of just N2 in this configuration
        thermodynamic_active_species_cp(0) = cpair
        thermodynamic_active_species_cv(0) = cpair - rair
        thermodynamic_active_species_R(0)  = rair
      end if

      water_species_in_air_num = water_species_num
      dry_air_species_num = dry_species_num
      thermodynamic_active_species_num = water_species_num + dry_species_num

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
   ! dry_air_composition_update: Update the physics "constants" that vary
   !-------------------------------------------------------------------------
   !===========================================================================

   subroutine dry_air_composition_update(mmr, ncol, to_dry_factor)

      !(mmr = dry mixing ratio, if not, use to_dry_factor to convert!)
      real(kind_phys),           intent(in) :: mmr(:,:,:) ! mixing ratios for species dependent dry air
      integer,                   intent(in) :: ncol       ! number of columns
      real(kind_phys), optional, intent(in) :: to_dry_factor(:,:)

      call get_R_dry(mmr(:ncol, :, :), thermodynamic_active_species_idx, &
           rairv(:ncol, :), fact=to_dry_factor)
      call get_cp_dry(mmr(:ncol,:,:), thermodynamic_active_species_idx, &
           cpairv(:ncol,:), fact=to_dry_factor)
      call get_mbarv(mmr(:ncol,:,:), thermodynamic_active_species_idx, &
           mbarv(:ncol,:), fact=to_dry_factor)

      cappav(:ncol,:) = rairv(:ncol,:) / cpairv(:ncol,:)

   end subroutine dry_air_composition_update

   !===========================================================================
   !---------------------------------------------------------------------------
   ! water_composition_update: Update generalized cp or cv depending on dycore
   ! (enthalpy for pressure-based dynamical cores and internal energy for z-based dynamical cores)
   !---------------------------------------------------------------------------
   !===========================================================================
   subroutine water_composition_update(mmr, ncol, energy_formula, cp_or_cv_dycore, to_dry_factor)
      use cam_thermo_formula, only: ENERGY_FORMULA_DYCORE_FV, ENERGY_FORMULA_DYCORE_SE, ENERGY_FORMULA_DYCORE_MPAS
      use string_utils,       only: stringify

      real(kind_phys),           intent(in)  :: mmr(:,:,:)           ! constituents array
      integer,                   intent(in)  :: ncol                 ! number of columns
      integer,                   intent(in)  :: energy_formula       ! energy formula for dynamical core
      real(kind_phys),           intent(out) :: cp_or_cv_dycore(:,:) ! enthalpy or heat capacity, dycore dependent [J K-1 kg-1]
      real(kind_phys), optional, intent(in)  :: to_dry_factor(:,:)

      character(len=*), parameter :: subname = 'water_composition_update'

      ! update enthalpy or internal energy scaling factor for energy consistency with CAM physics
      if (energy_formula == ENERGY_FORMULA_DYCORE_FV) then
         ! FV: moist pressure vertical coordinate does not need update.
      else if (energy_formula == ENERGY_FORMULA_DYCORE_SE) then
         ! SE
         ! Note: species index subset to 1: because SIMA currently uses index 0. See GitHub issue #334 in ESCOMP/CAM-SIMA.
         call get_cp(mmr(:ncol,:,:), .false., cp_or_cv_dycore(:ncol,:), &
                     factor=to_dry_factor, &
                     active_species_idx_dycore=thermodynamic_active_species_idx(1:thermodynamic_active_species_num), &
                     cpdry=cpairv(:ncol,:))
      else if (energy_formula == ENERGY_FORMULA_DYCORE_MPAS) then
         ! MPAS
         ! Note: species index subset to 1: because SIMA currently uses index 0. See GitHub issue #334 in ESCOMP/CAM-SIMA.
         call get_R(mmr(:ncol,:,:), &
                    thermodynamic_active_species_idx(1:thermodynamic_active_species_num), &
                    cp_or_cv_dycore(:ncol,:), fact=to_dry_factor, Rdry=rairv(:ncol,:))

         ! internal energy coefficient for MPAS
         ! (equation 92 in Eldred et al. 2023; doi:10.1002/qj.4353)
         cp_or_cv_dycore(:ncol,:) = cp_or_cv_dycore(:ncol,:) * (cpairv(:ncol,:) - rairv(:ncol,:)) / rairv(:ncol,:)
      else
         call endrun(subname//': dycore energy formula (value = '//stringify((/energy_formula/))//') not supported')
      end if
   end subroutine water_composition_update

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
   subroutine get_cp_1hd(tracer, inv_cp, cp, factor, active_species_idx_dycore, cpdry)
      use cam_abortutils,  only: endrun
      use string_utils,    only: to_str

      ! Dummy arguments
      ! tracer: Tracer array
      !
      ! if factor not present then tracer must be a dry mixing ratio
      ! if factor present tracer*factor must be a dry mixing ratio
      !
      real(kind_phys),           intent(in)  :: tracer(:,:,:)
      ! inv_cp: output inverse cp instead of cp
      logical,                   intent(in)  :: inv_cp
      real(kind_phys),           intent(out) :: cp(:,:)
      ! factor: to convert tracer to dry mixing ratio
      ! if provided, then tracer is not a dry mass mixing ratio
      real(kind_phys), optional, intent(in)  :: factor(:,:)
      ! active_species_idx_dycore: array of indices for index of
      !    thermodynamic active species in dycore tracer array
      !    (if different from physics index)
      integer,         optional, intent(in)  :: active_species_idx_dycore(:)
      real(kind_phys), optional, intent(in)  :: cpdry(:,:)

      ! Local variables
      integer  :: qdx, itrac
      real(kind_phys) :: sum_species(SIZE(cp, 1), SIZE(cp, 2))
      real(kind_phys) :: sum_cp(SIZE(cp, 1), SIZE(cp, 2))
      real(kind_phys) :: factor_local(SIZE(cp, 1), SIZE(cp, 2))
      integer  :: idx_local(thermodynamic_active_species_num)
      character(len=*), parameter :: subname = 'get_cp_1hd: '

      if (present(active_species_idx_dycore)) then
         if (SIZE(active_species_idx_dycore) /=                               &
              thermodynamic_active_species_num) then
            call endrun(subname//"SIZE mismatch "//                           &
                 to_str(SIZE(active_species_idx_dycore))//' /= '//            &
                 to_str(thermodynamic_active_species_num))
         end if
         idx_local = active_species_idx_dycore
      else
         idx_local = thermodynamic_active_species_idx
      end if

      if (present(factor)) then
         factor_local = factor
      else
         factor_local = 1.0_kind_phys
      end if

      sum_species = 1.0_kind_phys ! all dry air species sum to 1
      do qdx = dry_air_species_num + 1, thermodynamic_active_species_num
         itrac = idx_local(qdx)
         sum_species(:,:) = sum_species(:,:) + (tracer(:,:,itrac) * factor_local(:,:))
      end do

      if (dry_air_species_num == 0) then
         sum_cp = thermodynamic_active_species_cp(0)
      else if (present(cpdry)) then
         !
         ! if cpdry is known don't recompute
         !
         sum_cp = cpdry
      else
         ! Get heat capacity at constant pressure (Cp) for dry air:
         call get_cp_dry(tracer, idx_local, sum_cp, fact=factor_local)
      end if

      ! Add water species to Cp:
      do qdx = dry_air_species_num + 1, thermodynamic_active_species_num
         itrac = idx_local(qdx)
         sum_cp(:,:) = sum_cp(:,:) +                                      &
              (thermodynamic_active_species_cp(qdx) * tracer(:,:,itrac) * factor_local(:,:))
      end do

      if (inv_cp) then
         cp = sum_species / sum_cp
      else
         cp = sum_cp / sum_species
      end if

   end subroutine get_cp_1hd

   !===========================================================================

   subroutine get_cp_2hd(tracer, inv_cp, cp, factor, active_species_idx_dycore, cpdry)
      ! Version of get_cp for arrays that have a second horizontal index
      use cam_abortutils, only: endrun
      use string_utils,   only: to_str

      ! Dummy arguments
      ! tracer: Tracer array
      real(kind_phys),           intent(in)  :: tracer(:,:,:,:)
      ! inv_cp: output inverse cp instead of cp
      logical,                   intent(in)  :: inv_cp
      real(kind_phys),           intent(out) :: cp(:,:,:)
      real(kind_phys), optional, intent(in)  :: factor(:,:,:)
      ! active_species_idx_dycore: array of indicies for index of
      !    thermodynamic active species in dycore tracer array
      !    (if different from physics index)
      integer, optional,  intent(in)  :: active_species_idx_dycore(:)
      real(kind_phys), optional, intent(in)  :: cpdry(:,:,:)

      ! Local variables
      integer  :: jdx
      integer  :: idx_local(thermodynamic_active_species_num)
      character(len=*), parameter :: subname = 'get_cp_2hd: '

      do jdx = 1, SIZE(cp, 2)
         if (present(factor).and.present(cpdry)) then
            call get_cp(tracer(:, jdx, :, :), inv_cp, cp(:, jdx, :),&
                 factor=factor(:, jdx, :), active_species_idx_dycore=active_species_idx_dycore, cpdry=cpdry(:,jdx,:))
         else if (present(factor)) then
            call get_cp(tracer(:, jdx, :, :), inv_cp, cp(:, jdx, :),&
                 factor=factor(:, jdx, :), active_species_idx_dycore=active_species_idx_dycore)
         else if (present(cpdry)) then
            call get_cp(tracer(:, jdx, :, :), inv_cp, cp(:, jdx, :),&
                 active_species_idx_dycore=active_species_idx_dycore, cpdry=cpdry(:,jdx,:))
         else
            call get_cp(tracer(:, jdx, :, :), inv_cp, cp(:, jdx, :),&
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
   subroutine get_R_1hd(tracer, active_species_idx, R, fact, Rdry)
      use cam_abortutils,  only: endrun
      use string_utils,    only: to_str
      use physconst,       only: rair

      ! Dummy arguments
      ! tracer: !tracer array
      real(kind_phys), intent(in)  :: tracer(:, :, :)
      ! active_species_idx: index of active species in tracer
      integer,         intent(in)  :: active_species_idx(:)
      ! R: generalized gas constant
      real(kind_phys), intent(out) :: R(:, :)
      ! fact: optional factor for converting tracer to dry mixing ratio
      real(kind_phys), optional, intent(in) :: fact(:, :)
      real(kind_phys), optional, intent(in) :: Rdry(:, :)

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
         factor = fact(:,:)
      else
         factor = 1.0_kind_phys
      end if

      if (dry_air_species_num == 0) then
         R = rair
      else if (present(Rdry)) then
         R = Rdry
      else
         call get_R_dry(tracer, active_species_idx, R, fact=factor)
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
     use physconst,        only: mwdry
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

   !==========================================================================
   subroutine update_zvirv()
      use physconst, only: rh2o

      zvirv = (rh2o / rairv) - 1._kind_phys

   end subroutine update_zvirv

   !===========================================================================

   subroutine air_species_info(name, index, molec_weight, caller)
      use cam_abortutils,   only: endrun
      use cam_logfile,      only: iulog
      use cam_constituents, only: const_get_index, const_molec_weight

      ! Dummy arguments
      character(len=*),           intent(in)    :: name
      integer,                    intent(out) :: index
      real(kind_phys),            intent(out)   :: molec_weight
      character(len=*), optional, intent(in)    :: caller
      ! Local parameter
      character(len=*), parameter :: subname = 'air_species_info: '

      call const_get_index(name, index, abort=.false.)
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
         molec_weight = const_molec_weight(index)
      end if

   end subroutine air_species_info


end module air_composition
