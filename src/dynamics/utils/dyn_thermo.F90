module dyn_thermo

   !Create interfaces for physcis-based
   !thermodynamic routines that ensure
   !the real kind is the same that is used
   !in the dycore.

   use shr_kind_mod,   only: kind_dyn=>shr_kind_r8
   use ccpp_kinds,     only: kind_phys
   use cam_abortutils, only: check_allocate

   implicit none
   private

   !Public subroutines contained in this module are:

   public :: get_cp
   public :: get_cp_dry
   public :: get_kappa_dry
   public :: get_ps
   public :: get_dp
   public :: get_dp_ref
   public :: get_sum_species
   public :: get_molecular_diff_coef
   public :: get_molecular_diff_coef_reference
   public :: get_rho_dry
   public :: get_gz_given_dp_Tv_Rdry
   public :: get_virtual_temp
   public :: get_R_dry
   public :: get_exner
   public :: get_enthalpy

!==============================================================================
CONTAINS
!==============================================================================

   !
   !*************************************************************************************************************************
   !
   ! Compute generalized heat capacity at constant pressure
   !
   !*************************************************************************************************************************
   !
   subroutine get_cp(tracer,inv_cp,cp,dp_dry,active_species_idx_dycore)

      use air_composition, only: get_cp_phys=>get_cp

      !Subroutine (dummy) arguments:

      real(kind_dyn), intent(in)           :: tracer(:,:,:,:) !Tracer array
      real(kind_dyn), optional, intent(in) :: dp_dry(:,:,:)
      logical , intent(in)                 :: inv_cp          !output inverse cp instead of cp
      real(kind_dyn), intent(out)          :: cp(:,:,:)
      !
      ! array of indicies for index of thermodynamic active species in dycore
      ! tracer array
      ! (if different from physics index)
      !
      integer, optional, intent(in)  :: active_species_idx_dycore(:)

      !Declare local variables:
      real(kind_phys), allocatable :: tracer_phys(:,:,:,:)
      real(kind_phys), allocatable :: cp_phys(:,:,:)
      real(kind_phys), allocatable :: factor_phys(:,:,:)

      !check_allocate variables:
      integer :: iret !allocate status integer
      character(len=*), parameter :: subname = 'get_cp (dyn)'

      !Check if kinds are different:
      if (kind_phys == kind_dyn) then

         ! The dynamics and physics kind is the same, so just call the physics
         ! routine directly:
         if(present(dp_dry)) then
            call get_cp_phys(tracer,inv_cp,cp, &
                             factor=1.0_kind_phys/dp_dry, &
                             active_species_idx_dycore=active_species_idx_dycore)
         else
            call get_cp_phys(tracer,inv_cp,cp, &
                             active_species_idx_dycore=active_species_idx_dycore)
         endif

      else

         !Allocate local variables:
         allocate(tracer_phys(size(tracer,1), size(tracer,2), size(tracer,3), size(tracer,4)), stat=iret)
         call check_allocate(iret, subname, &
                             'tracer_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(cp_phys(size(cp,1), size(cp,2), size(cp,3)), stat=iret)
         call check_allocate(iret, subname, &
                             'cp_phys', &
                             file=__FILE__, line=__LINE__)


         !Set local input variables:
         tracer_phys = real(tracer, kind_phys)

         !Allocate and set optional variables:
         if (present(dp_dry)) then
            allocate(factor_phys(size(dp_dry, 1), size(dp_dry, 2), size(dp_dry,3)), stat=iret)
            call check_allocate(iret, subname, &
                                'factor_phys', &
                                file=__FILE__, line=__LINE__)

            !Set optional local variable:
            factor_phys = 1.0_kind_phys/real(dp_dry, kind_phys)
         end if

         !Call physics routine using local variables with matching kinds:
         call get_cp_phys(tracer_phys,inv_cp,cp_phys, &
                          factor=factor_phys, &
                          active_species_idx_dycore=active_species_idx_dycore)

         !Set output variables back to dynamics kind:
         cp = real(cp_phys, kind_dyn)

         !Deallocate variables:
         deallocate(tracer_phys)
         deallocate(cp_phys)

         if (allocated(factor_phys)) then
            deallocate(factor_phys)
         end if


      end if !kind check

   end subroutine get_cp
   !
   !****************************************************************************************************************
   !
   ! Compute dry air heat capacity under constant pressure
   !
   !****************************************************************************************************************
   !
   subroutine get_cp_dry(tracer,active_species_idx,cp_dry,fact)

      use air_composition, only: get_cp_dry_phys=>get_cp_dry

      !Subroutine (dummy) arguments:

      real(kind_dyn), intent(in)           :: tracer(:,:,:,:)
      integer,  intent(in)                 :: active_species_idx(:)     ! Tracer arrays
      real(kind_dyn), optional, intent(in) :: fact(:,:,:)
      real(kind_dyn), intent(out)          :: cp_dry(:,:,:)             ! dry pressure level thickness

      !Declare local variables:
      real(kind_phys), allocatable :: tracer_phys(:,:,:,:)
      real(kind_phys), allocatable :: cp_dry_phys(:,:,:)
      real(kind_phys), allocatable :: fact_phys(:,:,:)

      !check_allocate variables:
      integer :: iret !allocate status integer
      character(len=*), parameter :: subname = 'get_cp_dry (dyn)'

      !Check if kinds are different:
      if (kind_phys == kind_dyn) then

         !The dynamics and physics kind is the same, so just call the physics
         !routine directly:
         call get_cp_dry_phys(tracer,active_species_idx,&
                              cp_dry, fact=fact)

      else

         !Allocate local variables:
         allocate(tracer_phys(size(tracer,1), size(tracer,2), size(tracer,3), size(tracer,4)), stat=iret)
         call check_allocate(iret, subname, &
                             'tracer_phys', &
                             file=__FILE__, line=__LINE__)


         allocate(cp_dry_phys(size(cp_dry,1), size(cp_dry,2), size(cp_dry,3)), stat=iret)
         call check_allocate(iret, subname, &
                             'cp_dry_phys', &
                             file=__FILE__, line=__LINE__)


         !Set local variables:
         tracer_phys = real(tracer, kind_phys)

         if (present(fact)) then
            allocate(fact_phys(size(fact,1), size(fact,2), size(fact,3)), stat=iret)
            call check_allocate(iret, subname, &
                                'fact_phys', &
                               file=__FILE__, line=__LINE__)

            !Set optional local variable:
            fact_phys = real(fact, kind_phys)
         end if

         !Call physics routine using local vriables with matching kinds:
         call get_cp_dry_phys(tracer_phys,active_species_idx,&
                              cp_dry_phys, fact=fact_phys)

         !Set output variables back to dynamics kind:
         cp_dry = real(cp_dry_phys, kind_dyn)

         !Deallocate variables:
         deallocate(tracer_phys)
         deallocate(cp_dry_phys)

         if (allocated(fact_phys)) then
           deallocate(fact_phys)
        end if

      end if !kind check

   end subroutine get_cp_dry
   !
   !*************************************************************************************************************************
   !
   ! compute generalized kappa =Rdry/cpdry
   !
   !*************************************************************************************************************************
   !
   subroutine get_kappa_dry(tracer,active_species_idx,kappa_dry,fact)

      use cam_thermo, only: get_kappa_dry_phys=>get_kappa_dry

      !Subroutine (dummy) arguments:

      real(kind_dyn), intent(in)           :: tracer(:,:,:,:)                    !tracer array
      integer,  intent(in)                 :: active_species_idx(:)              !index of thermodynamic active tracers
      real(kind_dyn), intent(out)          :: kappa_dry(:,:,:)                   !kappa dry
      real(kind_dyn), optional, intent(in) :: fact(:,:,:)                        !factor for converting tracer to dry mixing ratio

      !Declare local variables:
      real(kind_phys), allocatable :: tracer_phys(:,:,:,:)
      real(kind_phys), allocatable :: kappa_dry_phys(:,:,:)
      real(kind_phys), allocatable :: fact_phys(:,:,:)

      !check_allocate variables:
      integer :: iret !allocate status integer
      character(len=*), parameter :: subname = 'get_kappa_dry (dyn)'

      !Check if kinds are different:
      if (kind_phys == kind_dyn) then


         !The dynamics and physics kind is the same, so just call the physics
         !routine directly:
         call get_kappa_dry_phys(tracer,active_species_idx,kappa_dry,&
                                 fact=fact)

      else

         !Allocate local variables:
         allocate(tracer_phys(size(tracer,1), size(tracer,2), size(tracer,3), size(tracer,4)), stat=iret)
         call check_allocate(iret, subname, &
                             'tracer_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(kappa_dry_phys(size(kappa_dry,1), size(kappa_dry,2), size(kappa_dry,3)), stat=iret)
         call check_allocate(iret, subname, &
                             'kappa_dry_phys', &
                             file=__FILE__, line=__LINE__)

         !Set local variables:
         tracer_phys = real(tracer, kind_phys)

         if (present(fact)) then
            allocate(fact_phys(size(fact,1), size(fact,2), size(fact,3)), stat=iret)
            call check_allocate(iret, subname, &
                                'fact_phys', &
                                file=__FILE__, line=__LINE__)

            !Set optional local variable:
            fact_phys = real(fact, kind_phys)
         end if

         !Call physics routine using local vriables with matching kinds:
         call get_kappa_dry_phys(tracer_phys,active_species_idx,&
                                 kappa_dry_phys, fact=fact_phys)

         !Set output variables back to dynamics kind:
         kappa_dry = real(kappa_dry_phys, kind_dyn)

         !Deallocate variables:
         deallocate(tracer_phys)
         deallocate(kappa_dry_phys)

         if (allocated(fact_phys)) then
            deallocate(fact_phys)
         end if

      end if !kind check

   end subroutine get_kappa_dry
   !
   !****************************************************************************************************************
   !
   ! get pressure from dry pressure and thermodynamic active species (e.g., forms of water: water vapor, cldliq, etc.)
   !
   !****************************************************************************************************************
   !
   subroutine get_ps(tracer_mass,active_species_idx,dp_dry,ps,ptop)

      use cam_thermo, only: get_ps_phys=>get_ps

      !Subroutine (dummy) arguments:

      real(kind_dyn), intent(in)   :: tracer_mass(:,:,:,:)            ! Tracer array
      real(kind_dyn), intent(in)   :: dp_dry(:,:,:)                   ! dry pressure level thickness
      real(kind_dyn), intent(out)  :: ps(:,:)                         ! surface pressure
      real(kind_dyn), intent(in)   :: ptop
      integer,  intent(in)         :: active_species_idx(:)

      !Declare local variables:
      real(kind_phys), allocatable :: tracer_mass_phys(:,:,:,:) ! Tracer array
      real(kind_phys), allocatable :: dp_dry_phys(:,:,:)        ! dry pressure level thickness
      real(kind_phys), allocatable :: ps_phys(:,:)              ! surface pressure

      real(kind_phys) :: ptop_phys

      !check_allocate variables:
      integer :: iret !allocate status integer
      character(len=*), parameter :: subname = 'get_ps (dyn)'

      !Check if kinds are different:
      if (kind_phys == kind_dyn) then

         !The dynamics and physics kind is the same, so just call the physics
         !routine directly:
         call get_ps_phys(tracer_mass,active_species_idx,dp_dry,ps,ptop)

      else

         !Allocate local variables:
         allocate(tracer_mass_phys(size(tracer_mass,1), size(tracer_mass,2), size(tracer_mass,3), size(tracer_mass,4)), stat=iret)
         call check_allocate(iret, subname, &
                             'tracer_mass_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(dp_dry_phys(size(dp_dry,1), size(dp_dry,2), size(dp_dry,3)), stat=iret)
         call check_allocate(iret, subname, &
                             'dp_dry_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(ps_phys(size(ps,1), size(ps,2)), stat=iret)
         call check_allocate(iret, subname, &
                             'ps_phys', &
                             file=__FILE__, line=__LINE__)

         !Set local variables:
         tracer_mass_phys = real(tracer_mass, kind_phys)
         dp_dry_phys      = real(dp_dry, kind_phys)
         ptop_phys        = real(ptop, kind_phys)

         !Call physics routine using local vriables with matching kinds:
         call get_ps_phys(tracer_mass_phys,active_species_idx,dp_dry_phys,ps_phys,ptop_phys)

         !Set output variables back to dynamics kind:
         ps = real(ps_phys, kind_dyn)

         !Deallocate variables:
         deallocate(tracer_mass_phys)
         deallocate(dp_dry_phys)
         deallocate(ps_phys)

      end if !kind check

   end subroutine get_ps
   !
   !****************************************************************************************************************
   !
   ! Compute pressure level thickness from dry pressure and thermodynamic active
   ! species mixing ratios
   !
   ! Tracer can either be in units of dry mixing ratio (mixing_ratio=1) or
   ! "mass" (=m*dp_dry) (mixing_ratio=2)
   !
   !****************************************************************************************************************
   !
   subroutine get_dp(tracer,mixing_ratio,active_species_idx,dp_dry,dp,ps,ptop)

      use cam_thermo, only: get_dp_phys=>get_dp

      !Subroutine (dummy) arguments:

      real(kind_dyn), intent(in)  :: tracer(:,:,:,:)         !tracers; quantity specified by mixing_ratio arg
      integer,  intent(in)        :: mixing_ratio            ! 1 => tracer is dry mixing ratio
                                                             ! 2 => tracer is mass (q*dp)
      integer,  intent(in)  :: active_species_idx(:)         ! index for thermodynamic species in tracer array
      real(kind_dyn), intent(in)  :: dp_dry(:,:,:)           ! dry pressure level thickness
      real(kind_dyn), intent(out) :: dp(:,:,:)               ! pressure level thickness
      real(kind_dyn), optional,intent(out) :: ps(:,:)        ! surface pressure (if ps present then ptop
                                                             !                   must be present)
      real(kind_dyn), optional,intent(in)  :: ptop           ! pressure at model top

      !Declare local variables:
      real(kind_phys), allocatable :: tracer_phys(:,:,:,:)
      real(kind_phys), allocatable :: dp_dry_phys(:,:,:)
      real(kind_phys), allocatable :: dp_phys(:,:,:)
      real(kind_phys), allocatable :: ps_phys(:,:)
      real(kind_phys), allocatable :: ptop_phys  !Allocatable in order to indicate "presence" to get_dp_phys subroutine.

      !check_allocate variables:
      integer :: iret !allocate status integer
      character(len=*), parameter :: subname = 'get_dp (dyn)'

      !Check if kinds are different:
      if (kind_phys == kind_dyn) then

         !The dynamics and physics kind is the same, so just call the physics
         !routine directly:
         call get_dp_phys(tracer,mixing_ratio,active_species_idx,dp_dry,dp,ps,ptop)

      else

         !Allocate local variables:
         allocate(tracer_phys(size(tracer,1), size(tracer,2), size(tracer,3), size(tracer,4)), stat=iret)
         call check_allocate(iret, subname, &
                             'tracer_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(dp_dry_phys(size(dp_dry,1), size(dp_dry,2), size(dp_dry,3)), stat=iret)
         call check_allocate(iret, subname, &
                             'dp_dry_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(dp_phys(size(dp,1), size(dp,2), size(dp,3)), stat=iret)
         call check_allocate(iret, subname, &
                             'dp_phys', &
                             file=__FILE__, line=__LINE__)

         !Set local variables:
         tracer_phys = real(tracer, kind_phys)
         dp_dry_phys = real(dp_dry, kind_phys)

         if (present(ptop)) then
            allocate(ptop_phys, stat=iret)
            call check_allocate(iret, subname, 'ptop_phys', &
                                file=__FILE__, line=__LINE__)

            !Set optional local variable:
            ptop_phys = real(ptop, kind_phys)
         end if

         if (present(ps)) then
            allocate(ps_phys(size(ps,1),size(ps,2)), stat=iret)
            call check_allocate(iret, subname, &
                                'ps_phys', &
                                file=__FILE__, line=__LINE__)
         end if

         !Call physics routine using local vriables with matching kinds:
         call get_dp_phys(tracer_phys,mixing_ratio,&
                          active_species_idx,dp_dry_phys,dp_phys,ps_phys,ptop_phys)


         !Set output variables back to dynamics kind:
         dp = real(dp_phys, kind_dyn)

         if (present(ps)) then
            ps = real(ps_phys, kind_dyn)
            deallocate(ps_phys)
         end if

         !Deallocate variables:
         deallocate(tracer_phys)
         deallocate(dp_dry_phys)
         deallocate(dp_phys)

         if (allocated(ptop_phys)) then
            deallocate(ptop_phys)
         end if

      end if !kind check

   end subroutine get_dp
   !
   !*************************************************************************************************************************
   !
   ! compute reference pressure levels
   !
   !*************************************************************************************************************************
   !
   subroutine get_dp_ref(hyai, hybi, ps0, phis, dp_ref, ps_ref)

      use cam_thermo, only: get_dp_ref_phys=>get_dp_ref

      !Subroutine (dummy) arguments:

      real(kind_dyn), intent(in)   :: hyai(:), hybi(:), ps0
      real(kind_dyn), intent(in)   :: phis(:,:)
      real(kind_dyn), intent(out)  :: dp_ref(:,:,:)
      real(kind_dyn), intent(out)  :: ps_ref(:,:)

      !Declare local variables:
      real(kind_phys), allocatable :: hyai_phys(:)
      real(kind_phys), allocatable :: hybi_phys(:)
      real(kind_phys), allocatable :: phis_phys(:,:)
      real(kind_phys), allocatable :: dp_ref_phys(:,:,:)
      real(kind_phys), allocatable :: ps_ref_phys(:,:)

      real(kind_phys) :: ps0_phys

      !check_allocate variables:
      integer :: iret !allocate status integer
      character(len=*), parameter :: subname = 'get_dp_ref (dyn)'

      !Check if kinds are different:
      if (kind_phys == kind_dyn) then

         !The dynamics and physics kind is the same, so just call the physics
         !routine directly:
         call get_dp_ref_phys(hyai, hybi, ps0, phis, dp_ref, ps_ref)

      else

         !Allocate local variables:
         allocate(hyai_phys(size(hyai)), stat=iret)
         call check_allocate(iret, subname, &
                             'hyai_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(hybi_phys(size(hybi)), stat=iret)
         call check_allocate(iret, subname, &
                             'hybi_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(phis_phys(size(phis,1), size(phis,2)), stat=iret)
         call check_allocate(iret, subname, &
                             'phis_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(dp_ref_phys(size(dp_ref,1), size(dp_ref,2), size(dp_ref,3)), stat=iret)
         call check_allocate(iret, subname, &
                             'dp_ref_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(ps_ref_phys(size(ps_ref,1), size(ps_ref,2)), stat=iret)
         call check_allocate(iret, subname, &
                             'ps_ref_phys', &
                             file=__FILE__, line=__LINE__)

         !Set local variables:
         hyai_phys = real(hyai, kind_phys)
         hybi_phys = real(hybi, kind_phys)
         ps0_phys  = real(ps0, kind_phys)
         phis_phys = real(phis, kind_phys)

         !Call physics routine using local vriables with matching kinds:
         call get_dp_ref_phys(hyai_phys, hybi_phys, ps0_phys, &
                              phis_phys, dp_ref_phys, ps_ref_phys)

         !Set output variables back to dynamics kind:
         dp_ref = real(dp_ref_phys, kind_dyn)
         ps_ref = real(ps_ref_phys, kind_dyn)

         !Deallocate variables:
         deallocate(hyai_phys)
         deallocate(hybi_phys)
         deallocate(phis_phys)
         deallocate(dp_ref_phys)
         deallocate(ps_ref_phys)

      end if !kind check

   end subroutine get_dp_ref
   !
   !****************************************************************************************************************
   !
   ! Compute sum of thermodynamically active species
   !
   ! tracer is in units of dry mixing ratio unless optional argument dp_dry is present in which case tracer is
   ! in units of "mass" (=m*dp)
   !
   !****************************************************************************************************************
   !
   subroutine get_sum_species(tracer,active_species_idx,sum_species,dp_dry)

      use cam_thermo, only: get_sum_species_phys=>get_sum_species

      !Subroutine (dummy) arguments:

      real(kind_dyn), intent(in)           :: tracer(:,:,:,:)         ! tracer array
      integer,  intent(in)                 :: active_species_idx(:)   ! index for thermodynamic active tracers
      real(kind_dyn), optional, intent(in) :: dp_dry(:,:,:)           ! dry pressure level thickness is present
                                                                      ! then tracer is in units of mass
      real(kind_dyn), intent(out)          :: sum_species(:,:,:)      ! sum species

      !Declare local variables:
      real(kind_phys), allocatable :: tracer_phys(:,:,:,:)      ! tracer array
      real(kind_phys), allocatable :: sum_species_phys(:,:,:)   ! sum species
      real(kind_phys), allocatable :: dp_dry_phys(:,:,:)        ! dry pressure level thickness is present

      !check_allocate variables:
      integer :: iret !allocate status integer
      character(len=*), parameter :: subname = 'get_sum_species (dyn)'

      !Check if kinds are different:
      if (kind_phys == kind_dyn) then

         !The dynamics and physics kind is the same, so just call the physics
         !routine directly:
         call get_sum_species_phys(tracer,active_species_idx,sum_species, &
                                   dp_dry=dp_dry)

      else

         !Allocate local variables:
         allocate(tracer_phys(size(tracer,1), size(tracer,2), size(tracer,3), size(tracer,4)), stat=iret)
         call check_allocate(iret, subname, &
                             'tracer_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(sum_species_phys(size(sum_species,1), size(sum_species,2), size(sum_species,3)), stat=iret)
         call check_allocate(iret, subname, &
                             'sum_species_phys', &
                             file=__FILE__, line=__LINE__)

         !Set local variables:
         tracer_phys = real(tracer, kind_phys)

         if (present(dp_dry)) then
            allocate(dp_dry_phys(size(dp_dry,1), size(dp_dry,2), size(dp_dry,3)), stat=iret)
            call check_allocate(iret, subname, &
                                'dp_dry_phys', &
                                file=__FILE__, line=__LINE__)

            !Set optional local variable:
            dp_dry_phys = real(dp_dry, kind_phys)
         end if

         !Call physics routine using local vriables with matching kinds:
         call get_sum_species_phys(tracer_phys,active_species_idx,sum_species_phys, &
                                   dp_dry=dp_dry_phys)

         !Set output variables back to dynamics kind:
         sum_species = real(sum_species_phys, kind_dyn)

         !Deallocate variables:
         deallocate(tracer_phys)
         deallocate(sum_species_phys)

         if (allocated(dp_dry_phys)) then
            deallocate(dp_dry_phys)
         end if

      end if !kind check

   end subroutine get_sum_species
   !
   !*************************************************************************************************************************
   !
   ! compute 3D molecular diffusion and thermal conductivity
   !
   !*************************************************************************************************************************
   !
   subroutine get_molecular_diff_coef(temp,get_at_interfaces,sponge_factor,kmvis,kmcnd, ntrac,&
        tracer, fact, active_species_idx_dycore, mbarv_in)

      use cam_thermo, only: get_molecular_diff_coef_phys=>get_molecular_diff_coef

      !Subroutine (dummy) arguments:

      real(kind_dyn), intent(in)           :: temp(:,:,:)            !temperature
      logical,  intent(in)                 :: get_at_interfaces      ! 1:compute kmvis and kmcnd at interfaces
                                                                     ! 0: compute kmvis and kmcnd at mid-levels
      real(kind_dyn), intent(in)           :: sponge_factor(:)       ! multiply kmvis and kmcnd with sponge_factor (for sponge layer)
      real(kind_dyn), intent(out)          :: kmvis(:,:,:)
      real(kind_dyn), intent(out)          :: kmcnd(:,:,:)
      integer, intent(in)                  :: ntrac
      real(kind_dyn), intent(in)           :: tracer(:,:,:,:)                  ! tracer array
      integer, intent(in), optional        :: active_species_idx_dycore(:)     ! index of active species in tracer
      real(kind_dyn), intent(in), optional :: fact(:,:,:)                      ! if tracer is in units of mass or moist
                                                                               ! fact converts to dry mixing ratio: tracer/fact
      real(kind_dyn), intent(in), optional :: mbarv_in(:,:,:)                  ! composition dependent atmosphere mean mass

      !Declare local variables:
      real(kind_phys), allocatable :: temp_phys(:,:,:)
      real(kind_phys), allocatable :: sponge_factor_phys(:)
      real(kind_phys), allocatable :: tracer_phys(:,:,:,:)
      real(kind_phys), allocatable :: kmvis_phys(:,:,:)
      real(kind_phys), allocatable :: kmcnd_phys(:,:,:)
      real(kind_phys), allocatable :: fact_phys(:,:,:)
      real(kind_phys), allocatable :: mbarv_in_phys(:,:,:)

      !check_allocate variables:
      integer :: iret !allocate status integer
      character(len=*), parameter :: subname = 'get_molecular_diff_coef (dyn)'

      !Check if kinds are different:
      if (kind_phys == kind_dyn) then

         !The dynamics and physics kind is the same, so just call the physics
         !routine directly:
         call get_molecular_diff_coef_phys(temp,get_at_interfaces, &
                                           sponge_factor,kmvis,kmcnd, &
                                           tracer, &
                                           fact=fact, &
                                           active_species_idx_dycore=active_species_idx_dycore, &
                                           mbarv_in=mbarv_in)

      else

         !Allocate local variables:
         allocate(temp_phys(size(temp,1), size(temp,2), size(temp,3)), stat=iret)
         call check_allocate(iret, subname, &
                             'temp_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(sponge_factor_phys(size(sponge_factor,1)), stat=iret)
         call check_allocate(iret, subname, &
                             'sponge_factor_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(tracer_phys(size(tracer,1), size(tracer,2), size(tracer,3), size(tracer,4)), stat=iret)
         call check_allocate(iret, subname, &
                             'tracer_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(kmvis_phys(size(kmvis,1), size(kmvis,2), size(kmvis,3)), stat=iret)
         call check_allocate(iret, subname, &
                             'kmvis_phys', &
                             file=__FILE__, line=__LINE__)
         allocate(kmcnd_phys(size(kmcnd,1), size(kmcnd,3), size(kmcnd,3)), stat=iret)
         call check_allocate(iret, subname, &
                             'kmcnd_phys', &
                             file=__FILE__, line=__LINE__)

         !Set local variables:
         temp_phys          = real(temp, kind_phys)
         tracer_phys        = real(tracer, kind_phys)
         sponge_factor_phys = real(sponge_factor, kind_phys)

         if (present(fact)) then
            allocate(fact_phys(size(fact,1), size(fact,2), size(fact,3)), stat=iret)
            call check_allocate(iret, subname, &
                                'fact_phys', &
                                file=__FILE__, line=__LINE__)

            !Set optional local variable:
            fact_phys = real(fact, kind_phys)
         end if
         if (present(mbarv_in)) then
            allocate(mbarv_in_phys(size(mbarv_in,1), size(mbarv_in,2), size(mbarv_in,3)), stat=iret)
            call check_allocate(iret, subname, &
                                'mbarv_in_phys', &
                                file=__FILE__, line=__LINE__)

            !Set optional local variable:
            mbarv_in_phys = real(mbarv_in, kind_phys)
         end if

         !Call physics routine using local vriables with matching kinds:
         call get_molecular_diff_coef_phys(temp_phys,get_at_interfaces, &
                                           sponge_factor_phys,kmvis_phys,kmcnd_phys, &
                                           tracer_phys, &
                                           fact=fact_phys, &
                                           active_species_idx_dycore=active_species_idx_dycore,&
                                           mbarv_in=mbarv_in_phys)

         !Set output variables back to dynamics kind:
         kmvis = real(kmvis_phys, kind_dyn)
         kmcnd = real(kmcnd_phys, kind_dyn)

         !Deallocate variables:
         deallocate(temp_phys)
         deallocate(sponge_factor_phys)
         deallocate(tracer_phys)
         deallocate(kmvis_phys)
         deallocate(kmcnd_phys)

         if (allocated(fact_phys)) then
            deallocate(fact_phys)
         end if
         if (allocated(mbarv_in_phys)) then
            deallocate(mbarv_in_phys)
         end if

      end if !kind check

   end subroutine get_molecular_diff_coef
   !
   !*************************************************************************************************************************
   !
   ! compute reference vertical profile of density, molecular diffusion and
   ! thermal conductivity
   !
   !*************************************************************************************************************************
   !
   subroutine get_molecular_diff_coef_reference(tref,press,sponge_factor,kmvis_ref,kmcnd_ref,rho_ref)

      use cam_thermo, only: get_molecular_diff_coef_reference_phys=>get_molecular_diff_coef_reference

      !Subroutine (dummy) arguments:
      real(kind_dyn), intent(in)  :: tref             !reference temperature
      real(kind_dyn), intent(in)  :: press(:)         !pressure
      real(kind_dyn), intent(in)  :: sponge_factor(:) !multiply kmvis and kmcnd with sponge_factor (for sponge layer)
      real(kind_dyn), intent(out) :: kmvis_ref(:)     !reference molecular diffusion coefficient
      real(kind_dyn), intent(out) :: kmcnd_ref(:)     !reference thermal conductivity coefficient
      real(kind_dyn), intent(out) :: rho_ref(:)       !reference density

      !Declare local variables:
      real(kind_phys), allocatable :: press_phys(:)
      real(kind_phys), allocatable :: sponge_factor_phys(:)
      real(kind_phys), allocatable :: kmvis_ref_phys(:)
      real(kind_phys), allocatable :: kmcnd_ref_phys(:)
      real(kind_phys), allocatable :: rho_ref_phys(:)

      real(kind_phys) :: tref_phys

      !check_allocate variables:
      integer :: iret !allocate status integer
      character(len=*), parameter :: subname = 'get_molecular_diff_coef_reference (dyn)'

      !Check if kinds are different:
      if (kind_phys == kind_dyn) then

         !The dynamics and physics kind is the same, so just call the physics
         !routine directly:
         call get_molecular_diff_coef_reference_phys(tref,press,&
                                                     sponge_factor,&
                                                     kmvis_ref,kmcnd_ref,rho_ref)

      else

         !Allocate local variables:
         allocate(press_phys(size(press,1)), stat=iret)
         call check_allocate(iret, subname, &
                             'press_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(sponge_factor_phys(size(sponge_factor,1)), stat=iret)
         call check_allocate(iret, subname, &
                             'sponge_factor_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(kmvis_ref_phys(size(kmvis_ref,1)), stat=iret)
         call check_allocate(iret, subname, &
                             'kmvis_ref_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(kmcnd_ref_phys(size(kmcnd_ref,1)), stat=iret)
         call check_allocate(iret, subname, &
                             'kmcnd_ref_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(rho_ref_phys(size(rho_ref,1)), stat=iret)
         call check_allocate(iret, subname, &
                             'rho_ref_phys', &
                             file=__FILE__, line=__LINE__)

         !Set local variables:
         tref_phys          = real(tref, kind_phys)
         press_phys         = real(press, kind_phys)
         sponge_factor_phys = real(sponge_factor, kind_phys)

         !Call physics routine using local vriables with matching kinds:
         call get_molecular_diff_coef_reference_phys(tref_phys,press_phys,&
                                                     sponge_factor_phys,&
                                                     kmvis_ref_phys,kmcnd_ref_phys,&
                                                     rho_ref_phys)

         !Set output variables back to dynamics kind:
         kmvis_ref = real(kmvis_ref_phys, kind_dyn)
         kmcnd_ref = real(kmcnd_ref_phys, kind_dyn)
         rho_ref   = real(rho_ref_phys, kind_dyn)

         !Deallocate variables:
         deallocate(press_phys)
         deallocate(sponge_factor_phys)
         deallocate(kmvis_ref_phys)
         deallocate(kmcnd_ref_phys)
         deallocate(rho_ref_phys)

      end if !kind check

   end subroutine get_molecular_diff_coef_reference
   !
   !*************************************************************************************************************************
   !
   ! compute dry density from temperature (temp) and pressure (dp_dry and
   ! tracer)
   !
   !*************************************************************************************************************************
   !
   subroutine get_rho_dry(tracer,temp,ptop,dp_dry,tracer_mass,&
        rho_dry, rhoi_dry,active_species_idx_dycore)

      use cam_thermo, only: get_rho_dry_phys=>get_rho_dry

      !Subroutine (dummy) arguments:
      real(kind_dyn), intent(in)           :: tracer(:,:,:,:)      !Tracer array
      real(kind_dyn), intent(in)           :: temp(:,:,:)          !Temperature
      real(kind_dyn), intent(in)           :: ptop
      real(kind_dyn), intent(in)           :: dp_dry(:,:,:)
      logical,  intent(in)                 :: tracer_mass
      real(kind_dyn), optional,intent(out) :: rho_dry(:,:,:)
      real(kind_dyn), optional,intent(out) :: rhoi_dry(:,:,:)
      !
      ! array of indicies for index of thermodynamic active species in dycore
      ! tracer array
      ! (if different from physics index)
      !
      integer, optional, intent(in)         :: active_species_idx_dycore(:)

      !Declare local variables:
      real(kind_phys), allocatable :: tracer_phys(:,:,:,:)
      real(kind_phys), allocatable :: temp_phys(:,:,:)
      real(kind_phys), allocatable :: dp_dry_phys(:,:,:)
      real(kind_phys), allocatable :: rho_dry_phys(:,:,:)
      real(kind_phys), allocatable :: rhoi_dry_phys(:,:,:)

      real(kind_phys) :: ptop_phys

      !check_allocate variables:
      integer :: iret !allocate status integer
      character(len=*), parameter :: subname = 'get_rho_dry (dyn)'

      !Check if kinds are different:
      if (kind_phys == kind_dyn) then

         !The dynamics and physics kind is the same, so just call the physics
         !routine directly:
         call get_rho_dry_phys(tracer,temp,ptop, &
                               dp_dry,tracer_mass, &
                               rho_dry=rho_dry, &
                               rhoi_dry=rhoi_dry, &
                               active_species_idx_dycore=active_species_idx_dycore)
      else

         !Allocate local variables:
         allocate(tracer_phys(size(tracer,1), size(tracer,2), size(tracer,3), size(tracer,4)), stat=iret)
         call check_allocate(iret, subname, &
                             'tracer_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(temp_phys(size(temp,1), size(temp,2), size(temp,3)), stat=iret)
         call check_allocate(iret, subname, &
                             'temp_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(dp_dry_phys(size(dp_dry,1), size(dp_dry,2), size(dp_dry,3)), stat=iret)
         call check_allocate(iret, subname, &
                             'dp_dry_phys', &
                             file=__FILE__, line=__LINE__)

         !Set local variables:
         tracer_phys = real(tracer, kind_phys)
         temp_phys   = real(temp, kind_phys)
         ptop_phys   = real(ptop, kind_phys)
         dp_dry_phys = real(dp_dry, kind_phys)

         if (present(rho_dry)) then
            allocate(rho_dry_phys(size(rho_dry,1), size(rho_dry,2), size(rho_dry,3)), stat=iret)
            call check_allocate(iret, subname, &
                                'rho_dry_phys', &
                                file=__FILE__, line=__LINE__)
         end if
         if (present(rhoi_dry)) then
            allocate(rhoi_dry_phys(size(rhoi_dry,1), size(rhoi_dry,2), size(rhoi_dry,3)), stat=iret)
            call check_allocate(iret, subname, &
                                'rhoi_dry_phys', &
                                file=__FILE__, line=__LINE__)

         end if

         !Call physics routine using local variables with matching kinds:
         call get_rho_dry_phys(tracer_phys,temp_phys, &
                               ptop_phys, dp_dry_phys,tracer_mass, &
                               rho_dry=rho_dry_phys, &
                               rhoi_dry=rhoi_dry_phys, &
                               active_species_idx_dycore=active_species_idx_dycore)

         !Set output variables back to dynamics kind:
         if (present(rho_dry)) then
            rho_dry = real(rho_dry_phys, kind_dyn)
            deallocate(rho_dry_phys)
         end if
         if (present(rhoi_dry)) then
            rhoi_dry = real(rhoi_dry_phys, kind_dyn)
            deallocate(rhoi_dry_phys)
         end if

         !Deallocate variables:
         deallocate(tracer_phys)
         deallocate(temp_phys)
         deallocate(dp_dry_phys)

      end if !kind check

   end subroutine get_rho_dry
   !
   !****************************************************************************************************************
   !
   ! Compute geopotential from pressure level thickness and virtual temperature
   !
   !****************************************************************************************************************
   !
   subroutine get_gz_given_dp_Tv_Rdry(dp,T_v,R_dry,phis,ptop,gz,pmid)

      use cam_thermo, only: get_gz_given_dp_Tv_Rdry_phys=>get_gz

      !Subroutine (dummy) arguments:
      real(kind_dyn), intent(in)            :: dp   (:,:,:) ! pressure level thickness
      real(kind_dyn), intent(in)            :: T_v  (:,:,:) ! virtual temperature
      real(kind_dyn), intent(in)            :: R_dry(:,:,:) ! R dry
      real(kind_dyn), intent(in)            :: phis (:,:)   ! surface geopotential
      real(kind_dyn), intent(in)            :: ptop         ! model top presure
      real(kind_dyn), intent(out)           :: gz(:,:,:)    ! geopotential
      real(kind_dyn), optional, intent(out) :: pmid(:,:,:)  ! mid-level pressure

      !Declare local variables:
      real(kind_phys), allocatable :: dp_phys(:,:,:)
      real(kind_phys), allocatable :: T_v_phys(:,:,:)
      real(kind_phys), allocatable :: R_dry_phys(:,:,:)
      real(kind_phys), allocatable :: phis_phys(:,:)
      real(kind_phys), allocatable :: gz_phys(:,:,:)
      real(kind_phys), allocatable :: pmid_phys(:,:,:)

      real(kind_phys) :: ptop_phys

      !check_allocate variables:
      integer :: iret !allocate status integer
      character(len=*), parameter :: subname = 'get_gz_given_dp_Tv_Rdry (dyn)'

      !Check if kinds are different:
      if (kind_phys == kind_dyn) then

         !The dynamics and physics kind is the same, so just call the physics
         !routine directly:
         call get_gz_given_dp_Tv_Rdry_phys(dp,T_v,R_dry,phis,ptop,gz,&
                                           pmid=pmid)

      else

         !Allocate local variables:
         allocate(dp_phys(size(dp,1), size(dp,2), size(dp,3)), stat=iret)
         call check_allocate(iret, subname, &
                             'dp_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(T_v_phys(size(T_v,1), size(T_v,2), size(T_v,3)), stat=iret)
         call check_allocate(iret, subname, &
                             'T_v_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(R_dry_phys(size(R_dry,1), size(R_dry,2), size(R_dry,3)), stat=iret)
         call check_allocate(iret, subname, &
                             'R_dry_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(phis_phys(size(phis,1), size(phis,2)), stat=iret)
         call check_allocate(iret, subname, &
                             'phis_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(gz_phys(size(gz,1), size(gz,2), size(gz,3)), stat=iret)
         call check_allocate(iret, subname, &
                             'gz_phys', &
                             file=__FILE__, line=__LINE__)

         !Set local variables:
         dp_phys    = real(dp, kind_phys)
         T_v_phys   = real(T_v, kind_phys)
         R_dry_phys = real(R_dry, kind_phys)
         phis_phys  = real(phis, kind_phys)
         ptop_phys  = real(ptop, kind_phys)

         if (present(pmid)) then
            !Allocate variable if optional argument is present:
            allocate(pmid_phys(size(pmid,1), size(pmid,2), size(pmid,3)), stat=iret)
            call check_allocate(iret, subname, 'pmid_phys', &
                                file=__FILE__, line=__LINE__)
         end if

         !Call physics routine using local vriables with matching kinds:
         call get_gz_given_dp_Tv_Rdry_phys(dp_phys,T_v_phys, &
                                           R_dry_phys,phis_phys,ptop_phys,gz_phys, &
                                           pmid=pmid_phys)

         !Set output variables back to dynamics kind:
         gz = real(gz_phys, kind_dyn)

         if (present(pmid)) then
            pmid = real(pmid_phys, kind_dyn)
            deallocate(pmid_phys)
         end if

         !Deallocate variables:
         deallocate(dp_phys)
         deallocate(T_v_phys)
         deallocate(R_dry_phys)
         deallocate(phis_phys)
         deallocate(gz_phys)

      end if !kind check

   end subroutine get_gz_given_dp_Tv_Rdry
   !
   !****************************************************************************************************************
   !
   ! Compute virtual temperature T_v
   !
   ! tracer is in units of dry mixing ratio unless optional argument dp_dry is
   ! present in which case tracer is
   ! in units of "mass" (=m*dp)
   !
   ! If temperature is not supplied then just return factor that T needs to be
   ! multiplied by to get T_v
   !
   !****************************************************************************************************************
   !
   subroutine get_virtual_temp(tracer,T_v,temp,dp_dry,sum_q, &
                               active_species_idx_dycore)

      use cam_thermo, only: get_virtual_temp_phys=>get_virtual_temp

      !Subroutine (dummy) arguments:
      real(kind_dyn), intent(in)           :: tracer(:,:,:,:)     ! tracer array
      real(kind_dyn), intent(out)          :: T_v(:,:,:)          ! virtual temperature
      real(kind_dyn), optional, intent(in) :: temp(:,:,:)         ! temperature
      real(kind_dyn), optional, intent(in) :: dp_dry(:,:,:)       ! dry pressure level thickness
      real(kind_dyn), optional,intent(out) :: sum_q(:,:,:)        ! sum tracer
      !
      ! array of indicies for index of thermodynamic active species in dycore
      ! tracer array
      ! (if different from physics index)
      !
      integer, optional,  intent(in) :: active_species_idx_dycore(:)

      !Declare local variables:
      real(kind_phys), allocatable :: tracer_phys(:,:,:,:)
      real(kind_phys), allocatable :: T_v_phys(:,:,:)
      real(kind_phys), allocatable :: temp_phys(:,:,:)
      real(kind_phys), allocatable :: dp_dry_phys(:,:,:)
      real(kind_phys), allocatable :: sum_q_phys(:,:,:)

      !check_allocate variables:
      integer :: iret !allocate status integer
      character(len=*), parameter :: subname = 'get_virtual_temp (dyn)'

      !Check if kinds are different:
      if (kind_phys == kind_dyn) then

         !The dynamics and physics kind is the same, so just call the physics
         !routine directly:
         call get_virtual_temp_phys(tracer,T_v, &
                                    temp=temp,dp_dry=dp_dry,sum_q=sum_q, &
                                    active_species_idx_dycore=active_species_idx_dycore)

      else

         !Allocate local variables:
         allocate(tracer_phys(size(tracer,1), size(tracer,2), size(tracer,3), size(tracer,4)), stat=iret)
         call check_allocate(iret, subname, &
                             'tracer_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(T_v_phys(size(T_v,1), size(T_v,2), size(T_v,3)), stat=iret)
         call check_allocate(iret, subname, &
                             'T_v_phys', &
                             file=__FILE__, line=__LINE__)

         !Set local variables:
         tracer_phys = real(tracer, kind_phys)

         if (present(temp)) then
            !Allocate variable if optional argument is present:
            allocate(temp_phys(size(temp,1), size(temp,2), size(temp,3)), stat=iret)
            call check_allocate(iret, subname, 'temp_phys', &
                                file=__FILE__, line=__LINE__)

            !Set optional local variable:
            temp_phys = real(temp, kind_phys)
         end if

         if (present(dp_dry)) then
            !Allocate variable if optional argument is present:
            allocate(dp_dry_phys(size(dp_dry,1), size(dp_dry,2), size(dp_dry,3)), stat=iret)
            call check_allocate(iret, subname, 'dp_dry_phys', &
                                file=__FILE__, line=__LINE__)

            !Set optional local variable:
            dp_dry_phys = real(dp_dry, kind_phys)
         end if

         if (present(sum_q)) then
            !Allocate variable if optional argument is present:
            allocate(sum_q_phys(size(sum_q,1), size(sum_q,2), size(sum_q,3)), stat=iret)
            call check_allocate(iret, subname, 'sum_q_phys', &
                                file=__FILE__, line=__LINE__)
         end if

         !Call physics routine using local vriables with matching kinds:
         call get_virtual_temp_phys(tracer_phys,T_v_phys, &
                                    temp=temp_phys,dp_dry=dp_dry_phys,sum_q=sum_q_phys, &
                                    active_species_idx_dycore=active_species_idx_dycore)

         !Set output variables back to dynamics kind:
         T_v = real(T_v_phys, kind_dyn)

         if (present(sum_q)) then
            sum_q = real(sum_q_phys, kind_dyn)
            deallocate(sum_q_phys)
         end if

         !Deallocate variables:
         deallocate(tracer_phys)
         deallocate(T_v_phys)

         if (allocated(temp_phys)) then
            deallocate(temp_phys)
         end if
         if (allocated(dp_dry_phys)) then
            deallocate(dp_dry_phys)
         end if

      end if !kind check

   end subroutine get_virtual_temp
   !
   !****************************************************************************************************************
   !
   ! Compute generalized dry air gas constant R
   !
   !****************************************************************************************************************
   !
   subroutine get_R_dry(tracer,active_species_idx_dycore,R_dry,fact)

      use air_composition, only: get_R_dry_phys=>get_R_dry

      !Subroutine (dummy) arguments:
      real(kind_dyn), intent(in)           :: tracer(:,:,:,:)                !tracer array
      integer,  intent(in)                 :: active_species_idx_dycore(:)   !index of active species in tracer
      real(kind_dyn), intent(out)          :: R_dry(:,:,:)                   !dry air R
      real(kind_dyn), optional, intent(in) :: fact(:,:,:)                    !factor for converting tracer to dry mixing ratio

      !Declare local variables:
      real(kind_phys), allocatable :: tracer_phys(:,:,:,:)
      real(kind_phys), allocatable :: R_dry_phys(:,:,:)
      real(kind_phys), allocatable :: fact_phys(:,:,:)

      !check_allocate variables:
      integer :: iret !allocate status integer
      character(len=*), parameter :: subname = 'get_R_dry (dyn)'

      !Check if kinds are different:
      if (kind_phys == kind_dyn) then

         !The dynamics and physics kind is the same, so just call the physics
         !routine directly:
         call get_R_dry_phys(tracer,active_species_idx_dycore,R_dry, &
                             fact=fact)

      else

         !Allocate local variables:
         allocate(tracer_phys(size(tracer,1), size(tracer,2), size(tracer,3), size(tracer,4)), stat=iret)
         call check_allocate(iret, subname, &
                             'tracer_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(R_dry_phys(size(R_dry,1), size(R_dry,2), size(R_dry,3)), stat=iret)
         call check_allocate(iret, subname, &
                             'R_dry_phys', &
                             file=__FILE__, line=__LINE__)

         !Set local variables:
         tracer_phys = real(tracer, kind_phys)

         if (present(fact)) then
            !Allocate variable if optional argument is present:
            allocate(fact_phys(size(fact,1), size(fact,2), size(fact,3)), stat=iret)
            call check_allocate(iret, subname, 'fact_phys', &
                                file=__FILE__, line=__LINE__)

            !Set optional local variable:
            fact_phys = real(fact, kind_phys)
         end if

         !Call physics routine using local vriables with matching kinds:
         call get_R_dry_phys(tracer_phys, &
                             active_species_idx_dycore,R_dry_phys,fact=fact_phys)

         !Set output variables back to dynamics kind:
         R_dry = real(R_dry_phys, kind_dyn)

         !Deallocate variables:
         deallocate(tracer_phys)
         deallocate(R_dry_phys)

         if (allocated(fact_phys)) then
            deallocate(fact_phys)
         end if

      end if !kind check

   end subroutine get_R_dry
   !
   !****************************************************************************************************************
   !
   ! Compute Exner pressure
   !
   !****************************************************************************************************************
   !
   subroutine get_exner(tracer,mixing_ratio,active_species_idx,&
        dp_dry,ptop,p00,inv_exner,exner,poverp0)

      use cam_thermo, only: get_exner_phys=>get_exner

      !Subroutine (dummy) arguments:
      real(kind_dyn), intent(in)   :: tracer(:,:,:,:)          ! tracers; quantity specified by mixing_ratio arg
      integer,  intent(in)         :: mixing_ratio             ! 1 => tracer is mixing ratio
                                                               ! 2 => tracer is mass (q*dp)
      integer,  intent(in)         :: active_species_idx(:)    ! index for thermodynamic species in tracer array
      real(kind_dyn), intent(in)   :: dp_dry(:,:,:)            ! dry pressure level thickness
      real(kind_dyn), intent(in)   :: ptop                     ! pressure at model top
      real(kind_dyn), intent(in)   :: p00                      ! reference pressure for Exner pressure (usually 1000hPa)
      logical , intent(in)         :: inv_exner                ! logical for outputting inverse Exner or Exner pressure
      real(kind_dyn), intent(out)  :: exner(:,:,:)
      real(kind_dyn), optional, intent(out) :: poverp0(:,:,:)  ! for efficiency when a routine needs this variable

      !Declare local variables:
      real(kind_phys), allocatable :: tracer_phys(:,:,:,:)
      real(kind_phys), allocatable :: dp_dry_phys(:,:,:)
      real(kind_phys), allocatable :: exner_phys(:,:,:)
      real(kind_phys), allocatable :: poverp0_phys(:,:,:)

      real(kind_phys) :: ptop_phys
      real(kind_phys) :: p00_phys

      !check_allocate variables:
      integer :: iret !allocate status integer
      character(len=*), parameter :: subname = 'get_exner (dyn)'

      !Check if kinds are different:
      if (kind_phys == kind_dyn) then

         !The dynamics and physics kind is the same, so just call the physics
         !routine directly:
         call get_exner_phys(tracer,mixing_ratio,active_species_idx,&
                             dp_dry,ptop,p00,inv_exner,exner,poverp0=poverp0)

      else

         !Allocate local variables:
         allocate(tracer_phys(size(tracer,1), size(tracer,2), size(tracer,3), size(tracer,4)), stat=iret)
         call check_allocate(iret, subname, &
                             'tracer_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(dp_dry_phys(size(dp_dry,1), size(dp_dry,2), size(dp_dry,3)), stat=iret)
         call check_allocate(iret, subname, &
                             'dp_dry_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(exner_phys(size(exner,1), size(exner,2), size(exner,3)), stat=iret)
         call check_allocate(iret, subname, &
                             'exner_phys', &
                             file=__FILE__, line=__LINE__)

         !Set local variables:
         tracer_phys = real(tracer, kind_phys)
         dp_dry_phys = real(dp_dry, kind_phys)
         ptop_phys   = real(ptop, kind_phys)
         p00_phys    = real(p00, kind_phys)

         if (present(poverp0)) then
            !Allocate variable if optional argument is present:
            allocate(poverp0_phys(size(poverp0,1), size(poverp0,2), size(poverp0,3)), stat=iret)
            call check_allocate(iret, subname, 'poverp0_phys', &
                                file=__FILE__, line=__LINE__)
         end if

         !Call physics routine using local vriables with matching kinds:
         call get_exner_phys(tracer_phys,mixing_ratio,active_species_idx,&
                             dp_dry_phys,ptop_phys,p00_phys,inv_exner,exner_phys, &
                             poverp0=poverp0_phys)

         !Set optional output variables back to dynamics kind:
         if (present(poverp0)) then
            poverp0 = real(poverp0_phys, kind_dyn)
            deallocate(poverp0_phys)
         end if

         !Deallocate variables:
         deallocate(tracer_phys)
         deallocate(dp_dry_phys)
         deallocate(exner_phys)

         !Set output variables back to dynamics kind:
         exner = real(exner_phys, kind_dyn)

      end if !kind check

   end subroutine get_exner
   !
   !****************************************************************************************************************
   !
   ! g*compute thermal energy = cp*T*dp, where dp is pressure level thickness,
   ! cp is generalized cp and T temperature
   !
   ! Note:tracer is in units of m*dp_dry ("mass")
   !
   !****************************************************************************************************************
   !
   subroutine get_enthalpy(tracer_mass,temp,dp_dry,enthalpy, &
                                 active_species_idx_dycore)

      use cam_thermo, only: get_enthalpy_phys=>get_enthalpy

      !Subroutine (dummy) arguments:
      real(kind_dyn), intent(in)  :: tracer_mass(:,:,:,:)     !tracer array (mass weighted)
      real(kind_dyn), intent(in)  :: temp(:,:,:)              !temperature
      real(kind_dyn), intent(in)  :: dp_dry(:,:,:)            !dry presure level thickness
      real(kind_dyn), intent(out) :: enthalpy(:,:,:)    !thermal energy in each column: sum cp*T*dp
      !
      ! array of indicies for index of thermodynamic active species in dycore
      ! tracer array
      ! (if different from physics index)
      !
      integer, optional, dimension(:), intent(in) :: active_species_idx_dycore

      !Declare local variables:
      real(kind_phys), allocatable :: tracer_mass_phys(:,:,:,:)
      real(kind_phys), allocatable :: temp_phys(:,:,:)
      real(kind_phys), allocatable :: dp_dry_phys(:,:,:)
      real(kind_phys), allocatable :: enthalpy_phys(:,:,:)

      !check_allocate variables:
      integer :: iret !allocate status integer
      character(len=*), parameter :: subname = 'get_enthalpy (dyn)'

      !Check if kinds are different:
      if (kind_phys == kind_dyn) then

         !The dynamics and physics kind is the same, so just call the physics
         !routine directly:
         call get_enthalpy_phys(tracer_mass,temp,dp_dry,enthalpy,&
                                      active_species_idx_dycore=active_species_idx_dycore)

      else

         !Allocate local variables:
         allocate(tracer_mass_phys(size(tracer_mass,1), size(tracer_mass,2), size(tracer_mass,3), size(tracer_mass,4)), stat=iret)
         call check_allocate(iret, subname, &
                             'tracer_mass_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(temp_phys(size(temp,1), size(temp,2), size(temp,3)), stat=iret)
         call check_allocate(iret, subname, &
                             'temp_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(dp_dry_phys(size(dp_dry,1), size(dp_dry,2), size(dp_dry,3)), stat=iret)
         call check_allocate(iret, subname, &
                             'dp_dry_phys', &
                             file=__FILE__, line=__LINE__)

         allocate(enthalpy_phys(size(enthalpy,1), size(enthalpy,2), size(enthalpy,3)), stat=iret)
         call check_allocate(iret, subname, &
                             'enthalpy_phys', &
                             file=__FILE__, line=__LINE__)

         !Set local variables:
         tracer_mass_phys    = real(tracer_mass, kind_phys)
         temp_phys           = real(temp, kind_phys)
         dp_dry_phys         = real(dp_dry_phys, kind_phys)

         !Call physics routine using local vriables with matching kinds:
         call get_enthalpy_phys(tracer_mass_phys,temp_phys,&
                                      dp_dry_phys,enthalpy_phys,&
                                      active_species_idx_dycore=active_species_idx_dycore)

         !Set output variables back to dynamics kind:
         enthalpy = real(enthalpy_phys, kind_dyn)

         !Deallocate variables:
         deallocate(tracer_mass_phys)
         deallocate(temp_phys)
         deallocate(dp_dry_phys)
         deallocate(enthalpy_phys)

      end if !kind check

   end subroutine get_enthalpy

end module dyn_thermo
