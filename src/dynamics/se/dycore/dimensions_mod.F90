module dimensions_mod
  use shr_kind_mod, only: r8=>shr_kind_r8

  implicit none
  private

  !
  ! The variables below hold indices of water vapor and condensate loading tracers as well as
  ! associated heat capacities (initialized in dyn_init):
  !
  !   qsize_condensate_loading_idx     = index of water tracers included in condensate loading according to CAM physics
  !   qsize_condensate_loading_idx_gll = index of water tracers included in condensate loading terms for SE tracers
  !
  ! Note that when running without CSLAM then
  !
  !   qsize_condensate_loading_idx_gll = qsize_condensate_loading_idx
  !
  ! but when running with CSLAM then SE tracers are only the water tracers included in the condensate loading
  !
  character(len=16),  allocatable, public :: cnst_name_gll(:)     ! constituent names for SE tracers
  character(len=128), allocatable, public :: cnst_longname_gll(:) ! long name of SE tracers
  !
  !moist cp in energy conversion term
  !
  ! .false.: force dycore to use cpd (cp dry) instead of moist cp
  ! .true. : use moist cp in dycore
  !
  logical           , public :: lcp_moist = .true.

  integer, parameter, public :: np = NP
  integer, parameter, public :: nc = 3    !cslam resolution
  integer           , public :: fv_nphys  !physics-grid resolution - the "MAX" is so that the code compiles with NC=0

  integer, public, protected :: qsize_d   !SE tracer dimension size
  integer, public, protected :: ntrac_d   !FVM tracer dimension size

  integer, public            :: ntrac = 0 !ntrac is set in dyn_comp
  integer, public            :: qsize = 0 !qsize is set in dyn_comp
  !
  ! hyperviscosity is applied on approximate pressure levels
  ! Similar to CAM-EUL; see CAM5 scietific documentation (Note TN-486), equation (3.09), page 58.
  !
  logical,            public :: hypervis_dynamic_ref_state = .false.
  ! fvm dimensions:
  logical, public :: lprint!for debugging
  integer, parameter, public :: ngpc=3          !number of Gausspoints for the fvm integral approximation   !phl change from 4
  integer, parameter, public :: irecons_tracer=6!=1 is PCoM, =3 is PLM, =6 is PPM for tracer reconstruction
  integer, allocatable, public :: irecons_tracer_lev(:)
  integer, parameter, public :: nhe=1           !Max. Courant number
  integer, parameter, public :: nhr=2           !halo width needed for reconstruction - phl
  integer, parameter, public :: nht=nhe+nhr     !total halo width where reconstruction is needed (nht<=nc) - phl
  integer, parameter, public :: ns=3!quadratic halo interpolation - recommended setting for nc=3
  !nhc determines width of halo exchanged with neighboring elements
  integer, parameter, public :: nhc = nhr+(nhe-1)+(ns-MOD(ns,2))/2
                                                !(different from halo needed for elements on edges and corners
  integer, parameter, public :: lbc = 1-nhc
  integer, parameter, public :: ubc = nc+nhc
  logical, public            :: large_Courant_incr

  integer, public :: kmin_jet,kmax_jet !min and max level index for the jet
  integer, public :: fvm_supercycling
  integer, public :: fvm_supercycling_jet

  integer, allocatable, public :: kord_tr(:), kord_tr_cslam(:)

  real(r8), allocatable, public :: nu_scale_top(:) ! scaling of del2 viscosity in sponge layer (initialized in dyn_comp)
  real(r8), allocatable, public :: nu_lev(:)
  real(r8), allocatable, public :: otau(:)

  integer,  public :: ksponge_end       ! sponge is active k=1,ksponge_end
  real (r8), allocatable, public :: nu_div_lev(:) ! scaling of viscosity in sponge layer

  real(r8), allocatable, public :: kmvis_ref(:)        !reference profiles for molecular diffusion
  real(r8), allocatable, public :: kmcnd_ref(:)        !reference profiles for molecular diffusion
  real(r8), allocatable, public :: rho_ref(:)          !reference profiles for rho
  real(r8), allocatable, public :: km_sponge_factor(:) !scaling for molecular diffusion (when used as sponge)
  real(r8), allocatable, public :: kmvisi_ref(:)       !reference profiles for molecular diffusion
  real(r8), allocatable, public :: kmcndi_ref(:)       !reference profiles for molecular diffusion
  real(r8), allocatable, public :: rhoi_ref(:)         !reference profiles for rho

  integer,  public :: nhc_phys
  integer,  public :: nhe_phys
  integer,  public :: nhr_phys
  integer,  public :: ns_phys

  integer, public :: npdg = 0  ! dg degree for hybrid cg/dg element  0=disabled

  integer, public, protected :: npsq
  integer, public, protected :: nlev
  integer, public, protected :: nlevp

!  params for a mesh
!  integer, public, parameter :: max_elements_attached_to_node = 7
!  integer, public, parameter :: s_nv = 2*max_elements_attached_to_node

  !default for non-refined mesh (note that these are *not* parameters now)
  integer, public  :: max_elements_attached_to_node = 4
  integer, public  :: s_nv = 6
  integer, public  :: max_corner_elem               = 1 !max_elements_attached_to_node-3
  integer, public  :: max_neigh_edges               = 8 !4 + 4*max_corner_elem

  integer, public :: ne
  integer, public :: nelem       ! total number of elements
  integer, public :: nelemd      ! number of elements per MPI task
  integer, public :: nelemdmax   ! max number of elements on any MPI task
  integer, public :: nPhysProc                          ! This is the number of physics processors/ per dynamics processor
  integer, public :: nnodes,npart,nmpi_per_node
  integer, public :: GlobalUniqueCols

  !Public subroutines
  public :: dimensions_mod_init
  public :: set_mesh_dimensions

!==============================================================================
contains
!==============================================================================

  subroutine dimensions_mod_init()

     ! Allocate and initalize the relevant SE dycore dimension variables.

     use vert_coord,     only: pver, pverp
     use constituents,   only: pcnst
     use cam_abortutils, only: endrun
     use string_utils,   only: to_str

     ! Local variables:

     integer :: iret

     character(len=*), parameter :: subname = 'dimensions_mod_init'

     ! Set tracer dimension variables:

#ifdef FVM_TRACERS
     qsize_d = 10 ! SE tracers (currently SE supports 10 condensate loading tracers)
     ntrac_d = pcnst
#else
     qsize_d = pcnst
     ntrac_d = 0 ! No fvm tracers if CSLAM is off
#endif

     ! Set grid dimension variables:

     npsq  = np*np
     nlev  = pver
     nlevp = pverp

     ! Allocate vertically-dimensioned variables:

     allocate(irecons_tracer_lev(pver), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate irecons_tracer_lev(pver) failed with stat: '//&
                    to_str(iret))
     end if

     allocate(nu_scale_top(pver), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate nu_scale_top(pver) failed with stat: '//&
                    to_str(iret))
     end if

     allocate(nu_lev(pver), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate nu_lev(pver) failed with stat: '//&
                    to_str(iret))
     end if

     allocate(otau(pver), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate otau(pver) failed with stat: '//&
                    to_str(iret))
     end if

     allocate(nu_div_lev(pver), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate nu_div_lev(pver) failed with stat: '//&
                    to_str(iret))
     end if

     allocate(kmvis_ref(pver), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate kmvis_ref(pver) failed with stat: '//&
                    to_str(iret))
     end if

     allocate(kmcnd_ref(pver), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate kmcnd_ref(pver) failed with stat: '//&
                    to_str(iret))
     end if

     allocate(rho_ref(pver), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate rho_ref(pver) failed with stat: '//&
                    to_str(iret))
     end if

     allocate(km_sponge_factor(pver), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate km_sponge_factor(pver) failed with stat: '//&
                    to_str(iret))
     end if

     allocate(kmvisi_ref(pverp), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate kmvisi_ref(pverp) failed with stat: '//&
                    to_str(iret))
     end if

     allocate(kmcndi_ref(pverp), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate kmcndi_ref(pverp) failed with stat: '//&
                    to_str(iret))
     end if

     allocate(rhoi_ref(pverp), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate rhoi_ref(pverp) failed with stat: '//&
                    to_str(iret))
     end if


  end subroutine dimensions_mod_init

!==============================================================================

  subroutine set_mesh_dimensions()

    ! new "params"
    max_elements_attached_to_node = 7  ! variable resolution
    s_nv = 2*max_elements_attached_to_node

    !recalculate these
    max_corner_elem               = max_elements_attached_to_node-3
    max_neigh_edges               = 4 + 4*max_corner_elem


  end subroutine set_mesh_dimensions

!==============================================================================

end module dimensions_mod

