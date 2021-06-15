module element_mod

  use shr_kind_mod,           only: r8=>shr_kind_r8, i8=>shr_kind_i8
  use coordinate_systems_mod, only: spherical_polar_t, cartesian2D_t, cartesian3D_t, distance
  use edgetype_mod,           only: edgedescriptor_t
  use gridgraph_mod,          only: gridvertex_t
  use cam_abortutils,         only: endrun, check_allocate

  implicit none
  private
  integer, public, parameter :: timelevels = 3


! =========== PRIMITIVE-EQUATION DATA-STRUCTURES =====================

  type, public :: elem_state_t

    ! prognostic variables for preqx solver

    ! prognostics must match those in prim_restart_mod.F90
    ! vertically-lagrangian code advects dp3d instead of ps
    ! tracers Q, Qdp always use 2 level time scheme

    real(kind=r8), allocatable :: v(:,:,:,:,:)     ! velocity
    real(kind=r8), allocatable :: T(:,:,:,:)       ! temperature
    real(kind=r8), allocatable :: dp3d(:,:,:,:)    ! dry delta p on levels
    real(kind=r8), allocatable :: psdry(:,:)       ! dry surface pressure
    real(kind=r8), allocatable :: phis(:,:)        ! surface geopotential (prescribed)
    real(kind=r8), allocatable :: Qdp(:,:,:,:,:)   ! Tracer mass

  end type elem_state_t

  !___________________________________________________________________
  type, public :: derived_state_t
     !
     ! storage for subcycling tracers/dynamics
     !
    real(kind=r8), allocatable :: vn0(:,:,:,:)                ! velocity for SE tracer advection
    real(kind=r8), allocatable :: dpdiss_biharmonic(:,:,:)    ! mean dp dissipation tendency, if nu_p>0
    real(kind=r8), allocatable :: dpdiss_ave(:,:,:)           ! mean dp used to compute psdiss_tens

    ! diagnostics for explicit timestep
    real(kind=r8), allocatable :: phi(:,:,:)                  ! geopotential
    real(kind=r8), allocatable :: omega(:,:,:)                ! vertical velocity

    ! semi-implicit diagnostics: computed in explict-component, reused in Helmholtz-component.
    real(kind=r8), allocatable :: zeta(:,:,:)                 ! relative vorticity
    real(kind=r8), allocatable :: div(:,:,:,:)                ! divergence

    ! tracer advection fields used for consistency and limiters
    real(kind=r8), allocatable :: dp(:,:,:)                   ! for dp_tracers at physics timestep
    real(kind=r8), allocatable :: divdp(:,:,:)                ! divergence of dp
    real(kind=r8), allocatable :: divdp_proj(:,:,:)           ! DSSed divdp
    real(kind=r8), allocatable :: mass(:)                     ! total tracer mass for diagnostics

    ! forcing terms for CAM
    real(kind=r8), allocatable :: FQ(:,:,:,:)                 ! tracer forcing
    real(kind=r8), allocatable :: FM(:,:,:,:)                 ! momentum forcing
    real(kind=r8), allocatable :: FDP(:,:,:)                  ! save full updated dp right after physics
    real(kind=r8), allocatable :: FT(:,:,:)                   ! temperature forcing
    real(kind=r8), allocatable :: etadot_prescribed(:,:,:)    ! prescribed vertical tendency
    real(kind=r8), allocatable :: u_met(:,:,:)                ! zonal component of prescribed meteorology winds
    real(kind=r8), allocatable :: dudt_met(:,:,:)             ! rate of change of zonal component of prescribed meteorology winds
    real(kind=r8), allocatable :: v_met(:,:,:)                ! meridional component of prescribed meteorology winds
    real(kind=r8), allocatable :: dvdt_met(:,:,:)             ! rate of change of meridional component of prescribed meteorology winds
    real(kind=r8), allocatable :: T_met(:,:,:)                ! prescribed meteorology temperature
    real(kind=r8), allocatable :: dTdt_met(:,:,:)             ! rate of change of prescribed meteorology temperature
    real(kind=r8), allocatable :: ps_met(:,:)                 ! surface pressure of prescribed meteorology
    real(kind=r8), allocatable :: dpsdt_met(:,:)              ! rate of change of surface pressure of prescribed meteorology
    real(kind=r8), allocatable :: nudge_factor(:,:,:)         ! nudging factor (prescribed)
    real(kind=r8), allocatable :: Utnd(:,:)                   ! accumulated U tendency due to nudging towards prescribed met
    real(kind=r8), allocatable :: Vtnd(:,:)                   ! accumulated V tendency due to nudging towards prescribed met
    real(kind=r8), allocatable :: Ttnd(:,:)                   ! accumulated T tendency due to nudging towards prescribed met

    real(kind=r8), allocatable :: pecnd(:,:,:)                ! pressure perturbation from condensate

  end type derived_state_t

  !___________________________________________________________________
  type, public :: elem_accum_t


    ! the "4" timelevels represents data computed at:
    !  1  t-.5
    !  2  t+.5   after dynamics
    !  3  t+.5   after forcing
    !  4  t+.5   after Robert
    ! after calling TimeLevelUpdate, all times above decrease by 1.0


  end type elem_accum_t


! ============= DATA-STRUCTURES COMMON TO ALL SOLVERS ================

  type, public :: index_t
     integer, allocatable :: ia(:),ja(:)
     integer :: is,ie
     integer :: NumUniquePts
     integer :: UniquePtOffset
  end type index_t

  !___________________________________________________________________
  type, public :: element_t
     integer :: LocalId
     integer :: GlobalId

     ! Coordinate values of element points
     type(spherical_polar_t), allocatable :: spherep(:,:)             ! Spherical coords of GLL points

     ! Equ-angular gnomonic projection coordinates
     type(cartesian2D_t), allocatable    :: cartp(:,:)                ! gnomonic coords of GLL points
     type(cartesian2D_t)                 :: corners(4)                ! gnomonic coords of element corners
     real(kind=r8)                       :: u2qmap(4,2)               ! bilinear map from ref element to quad in cubedsphere coordinates
                                                                      ! SHOULD BE REMOVED
     ! 3D cartesian coordinates
     type(cartesian3D_t)                 :: corners3D(4)

     ! Element diagnostics
     real(kind=r8)              :: area                               ! Area of element
     real(kind=r8)              :: normDinv                           ! some type of norm of Dinv used for CFL
     real(kind=r8)              :: dx_short                           ! short length scale in km
     real(kind=r8)              :: dx_long                            ! long length scale in km

     real(kind=r8), allocatable :: variable_hyperviscosity(:,:)       ! hyperviscosity based on above
     real(kind=r8)              :: hv_courant                         ! hyperviscosity courant number
     real(kind=r8), allocatable :: tensorVisc(:,:,:,:)                !og, matrix V for tensor viscosity

     ! Edge connectivity information
!     integer :: node_numbers(4)
!     integer :: node_multiplicity(4)                 ! number of elements sharing corner node

     type(GridVertex_t)      :: vertex                               ! element grid vertex information
     type(EdgeDescriptor_t)  :: desc

     type(elem_state_t)      :: state

     type(derived_state_t)   :: derived
     ! Metric terms
     real(kind=r8), allocatable :: met(:,:,:,:)               ! metric tensor on velocity and pressure grid
     real(kind=r8), allocatable :: metinv(:,:,:,:)            ! metric tensor on velocity and pressure grid
     real(kind=r8), allocatable :: metdet(:,:)                ! g = SQRT(det(g_ij)) on velocity and pressure grid
     real(kind=r8), allocatable :: rmetdet(:,:)               ! 1/metdet on velocity pressure grid
     real(kind=r8), allocatable :: D(:,:,:,:)                 ! Map covariant field on cube to vector field on the sphere
     real(kind=r8), allocatable :: Dinv(:,:,:,:)              ! Map vector field on the sphere to covariant v on cube


     ! Mass flux across the sides of each sub-element.
     ! The storage is redundent since the mass across shared sides
     ! must be equal in magnitude and opposite in sign.
     ! The layout is like:
     !   --------------------------------------------------------------
     ! ^|    (1,4,3)     |                |              |    (4,4,3) |
     ! ||                |                |              |            |
     ! ||(1,4,4)         |                |              |(4,4,4)     |
     ! ||         (1,4,2)|                |              |     (4,4,2)|
     ! ||                |                |              |            |
     ! ||   (1,4,1)      |                |              |  (4,4,1)   |
     ! |---------------------------------------------------------------
     ! S|                |                |              |            |
     ! e|                |                |              |            |
     ! c|                |                |              |            |
     ! o|                |                |              |            |
     ! n|                |                |              |            |
     ! d|                |                |              |            |
     !  ---------------------------------------------------------------
     ! C|                |                |              |            |
     ! o|                |                |              |            |
     ! o|                |                |              |            |
     ! r|                |                |              |            |
     ! d|                |                |              |            |
     ! i|                |                |              |            |
     ! n---------------------------------------------------------------
     ! a|    (1,1,3)     |                |              |    (4,1,3) |
     ! t|                |                |              |(4,1,4)     |
     ! e|(1,1,4)         |                |              |            |
     !  |         (1,1,2)|                |              |     (4,1,2)|
     !  |                |                |              |            |
     !  |    (1,1,1)     |                |              |  (4,1,1)   |
     !  ---------------------------------------------------------------
     !          First Coordinate ------->
     real(kind=r8), allocatable :: sub_elem_mass_flux(:,:,:,:)

     ! Convert vector fields from spherical to rectangular components
     ! The transpose of this operation is its pseudoinverse.
     real(kind=r8), allocatable :: vec_sphere2cart(:,:,:,:)

     ! Mass matrix terms for an element on a cube face
     real(kind=r8), allocatable :: mp(:,:)     ! mass matrix on v and p grid
     real(kind=r8), allocatable :: rmp(:,:)    ! inverse mass matrix on v and p grid

     ! Mass matrix terms for an element on the sphere
     ! This mass matrix is used when solving the equations in weak form
     ! with the natural (surface area of the sphere) inner product
     real(kind=r8), allocatable :: spheremp(:,:)     ! mass matrix on v and p grid
     real(kind=r8), allocatable :: rspheremp(:,:)    ! inverse mass matrix on v and p grid

     integer(i8), allocatable   :: gdofP(:,:)        ! global degree of freedom (P-grid)

     real(kind=r8), allocatable :: fcor(:,:)         ! Coriolis term

     type(index_t)          :: idxP
     type(index_t), pointer :: idxV
     integer :: FaceNum

     ! force element_t to be a multiple of 8 bytes.
     ! on BGP, code will crash (signal 7, or signal 15) if 8 byte alignment is off
     ! check core file for:
     ! core.63:Generated by interrupt..(Alignment Exception DEAR=0xa1ef671c ESR=0x01800000 CCR0=0x4800a002)
     integer :: dummy
  end type element_t

  !___________________________________________________________________
  public :: element_coordinates
  public :: element_var_coordinates
  public :: element_var_coordinates3D
  public :: GetColumnIdP,GetColumnIdV
  public :: allocate_element_desc
  public :: allocate_element_dims
  public :: PrintElem

!==============================================================================
contains
!==============================================================================

  subroutine PrintElem(arr)

    use dimensions_mod, only: np

    real(kind=r8) :: arr(:,:)
    integer :: i,j

      do j=np,1,-1
         write(6,*) (arr(i,j), i=1,np)
      enddo

  end subroutine PrintElem
! ===================== ELEMENT_MOD METHODS ==========================

  function GetColumnIdP(elem,i,j) result(col_id)

    ! Get unique identifier for a Physics column on the P-grid

    type(element_t), intent(in) :: elem
    integer, intent(in) :: i,j
    integer :: col_id
    col_id = elem%gdofP(i,j)
  end function GetColumnIdP

  !___________________________________________________________________
  function GetColumnIdV(elem,i,j) result(col_id)

    !  Get unique identifier for a Physics column on the V-grid

    type(element_t), intent(in) :: elem
    integer, intent(in) :: i,j
    integer :: col_id
    col_id = elem%gdofP(i,j)
  end function GetColumnIdV

  !___________________________________________________________________
  function element_coordinates(start,end,points) result(cart)

    ! Initialize 2D rectilinear element colocation points

    type (cartesian2D_t), intent(in) :: start
    type (cartesian2D_t), intent(in) :: end
    real(r8),             intent(in) :: points(:)
    type (cartesian2D_t)             :: cart(SIZE(points),SIZE(points))

    type (cartesian2D_t) :: length, centroid
    real(r8)             :: y
    integer              :: i,j

    length%x   = 0.50D0*(end%x-start%x)
    length%y   = 0.50D0*(end%y-start%y)
    centroid%x = 0.50D0*(end%x+start%x)
    centroid%y = 0.50D0*(end%y+start%y)
    do j=1,SIZE(points)
       y = centroid%y + length%y*points(j)
       do i=1,SIZE(points)
          cart(i,j)%x = centroid%x + length%x*points(i)
          cart(i,j)%y = y
       end do
    end do
  end function element_coordinates

  !___________________________________________________________________
  function element_var_coordinates(c,points) result(cart)

    type (cartesian2D_t), intent(in) :: c(4)
    real(r8),             intent(in) :: points(:)
    type (cartesian2D_t)             :: cart(SIZE(points),SIZE(points))

    real(r8) :: p(size(points))
    real(r8) :: q(size(points))
    integer  :: i,j

    p(:) = (1.0D0-points(:))/2.0D0
    q(:) = (1.0D0+points(:))/2.0D0

    do j=1,SIZE(points)
       do i=1,SIZE(points)
          cart(i,j)%x = p(i)*p(j)*c(1)%x &
                      + q(i)*p(j)*c(2)%x &
                      + q(i)*q(j)*c(3)%x &
                      + p(i)*q(j)*c(4)%x
          cart(i,j)%y = p(i)*p(j)*c(1)%y &
                      + q(i)*p(j)*c(2)%y &
                      + q(i)*q(j)*c(3)%y &
                      + p(i)*q(j)*c(4)%y
       end do
    end do
  end function element_var_coordinates

  !___________________________________________________________________
  function element_var_coordinates3d(c,points) result(cart)

    type(cartesian3D_t), intent(in) :: c(4)
    real(r8), intent(in) :: points(:)

    type(cartesian3D_t)  :: cart(SIZE(points),SIZE(points))

    real(r8) :: p(size(points))
    real(r8) :: q(size(points)), r
    integer  :: i,j

    p(:) = (1.0D0-points(:))/2.0D0
    q(:) = (1.0D0+points(:))/2.0D0

    do j=1,SIZE(points)
       do i=1,SIZE(points)
          cart(i,j)%x = p(i)*p(j)*c(1)%x &
                      + q(i)*p(j)*c(2)%x &
                      + q(i)*q(j)*c(3)%x &
                      + p(i)*q(j)*c(4)%x
          cart(i,j)%y = p(i)*p(j)*c(1)%y &
                      + q(i)*p(j)*c(2)%y &
                      + q(i)*q(j)*c(3)%y &
                      + p(i)*q(j)*c(4)%y
          cart(i,j)%z = p(i)*p(j)*c(1)%z &
                      + q(i)*p(j)*c(2)%z &
                      + q(i)*q(j)*c(3)%z &
                      + p(i)*q(j)*c(4)%z

          ! project back to sphere:
          r = distance(cart(i,j))
          cart(i,j)%x = cart(i,j)%x/r
          cart(i,j)%y = cart(i,j)%y/r
          cart(i,j)%z = cart(i,j)%z/r
       end do
    end do
  end function element_var_coordinates3d

  !___________________________________________________________________
  subroutine allocate_element_desc(elem)

    use dimensions_mod, only: max_neigh_edges

    type (element_t), intent(inout)   :: elem(:)
    integer                           :: num, j, i, iret

    character(len=*), parameter :: subname = 'allocate_element_desc (SE)'

    num = SIZE(elem)

    do j=1,num
       allocate(elem(j)%desc%putmapP(max_neigh_edges), stat=iret)
       call check_allocate(iret, subname, 'elem%desc%putmapP(max_neigh_edges)', &
                           file=__FILE__, line=__LINE__)

       allocate(elem(j)%desc%getmapP(max_neigh_edges), stat=iret)
       call check_allocate(iret, subname, 'elem%desc%getmapP(max_neigh_edges)', &
                           file=__FILE__, line=__LINE__)

       allocate(elem(j)%desc%putmapP_ghost(max_neigh_edges), stat=iret)
       call check_allocate(iret, subname, 'elem%desc%putmapP_ghost(max_neigh_edges)', &
                           file=__FILE__, line=__LINE__)

       allocate(elem(j)%desc%getmapP_ghost(max_neigh_edges), stat=iret)
       call check_allocate(iret, subname, 'elem%desc%getmapP_ghost(max_neigh_edges)', &
                           file=__FILE__, line=__LINE__)

       allocate(elem(j)%desc%putmapS(max_neigh_edges), stat=iret)
       call check_allocate(iret, subname, 'elem%desc%putmapS(max_neigh_edges)', &
                           file=__FILE__, line=__LINE__)

       allocate(elem(j)%desc%getmapS(max_neigh_edges), stat=iret)
       call check_allocate(iret, subname, 'elem%desc%getmapS(max_neigh_edges)', &
                           file=__FILE__, line=__LINE__)

       allocate(elem(j)%desc%reverse(max_neigh_edges), stat=iret)
       call check_allocate(iret, subname, 'elem%desc%reverse(max_neigh_edges)', &
                           file=__FILE__, line=__LINE__)

       allocate(elem(j)%desc%globalID(max_neigh_edges), stat=iret)
       call check_allocate(iret, subname, 'elem%desc%globalID(max_neigh_edges)', &
                           file=__FILE__, line=__LINE__)

       allocate(elem(j)%desc%loc2buf(max_neigh_edges), stat=iret)
       call check_allocate(iret, subname, 'elem%desc%loc2buf(max_neigh_edges)', &
                           file=__FILE__, line=__LINE__)

       do i=1,max_neigh_edges
          elem(j)%desc%loc2buf(i)=i
          elem(j)%desc%globalID(i)=-1
       enddo

    end do
  end subroutine allocate_element_desc

  !___________________________________________________________________
  subroutine allocate_element_dims(elem)

    ! Allocate the SE element arrays using the pre-calculated SE dimensions

    use dimensions_mod, only: np, nc, npsq, nlev, nlevp, qsize_d, ntrac_d

    !Dummy arguments:
    type(element_t), intent(inout) :: elem(:)

    !Local arguments:
    integer :: num, i, iret

    character(len=*), parameter :: subname = 'allocate_element_dims (SE)'

    !---------------

    num = size(elem)

    do i=1,num

      !Coordinate values of element points:
      allocate(elem(i)%spherep(np,np), stat=iret)
      call check_allocate(iret, subname, 'elem%spherep(np,np)', &
                          file=__FILE__, line=__LINE__)

      !Gnomonic coords of GLL points:
      allocate(elem(i)%cartp(np,np), stat=iret)
      call check_allocate(iret, subname, 'elem%cartp(np,np)', &
                          file=__FILE__, line=__LINE__)

      !Variable Hyperviscosity:
      allocate(elem(i)%variable_hyperviscosity(np,np), stat=iret)
      call check_allocate(iret, subname, 'elem%variable_hyperviscosity(np,np)', &
                          file=__FILE__, line=__LINE__)

      !og, matrix V for tensor viscosity:
      allocate(elem(i)%tensorVisc(np,np,2,2), stat=iret)
      call check_allocate(iret, subname, 'elem%tensorVisc(np,np,2,2)', &
                          file=__FILE__, line=__LINE__)

      !Allocate "state" variables:
      !--------------------------

      ! velocity
      allocate(elem(i)%state%v(np,np,2,nlev,timelevels), stat=iret)
      call check_allocate(iret, subname, 'elem%state%v(np,np,2,nlev,timelevels)', &
                          file=__FILE__, line=__LINE__)

      ! temperature
      allocate(elem(i)%state%T(np,np,nlev,timelevels), stat=iret)
      call check_allocate(iret, subname, 'elem%state%T(np,np,nlev,timelevels)', &
                          file=__FILE__, line=__LINE__)

      ! dry delta p on levels
      allocate(elem(i)%state%dp3d(np,np,nlev,timelevels), stat=iret)
      call check_allocate(iret, subname, 'elem%state%dp3d(np,np,nlev,timelevels)', &
                          file=__FILE__, line=__LINE__)

      ! dry surface pressure
      allocate(elem(i)%state%psdry(np,np), stat=iret)
      call check_allocate(iret, subname, 'elem%state%psdry(np,np)', &
                          file=__FILE__, line=__LINE__)

      ! surface geopotential (prescribed)
      allocate(elem(i)%state%phis(np,np), stat=iret)
      call check_allocate(iret, subname, 'elem%state%phis(np,np)', &
                          file=__FILE__, line=__LINE__)

      ! Tracer mass
      allocate(elem(i)%state%Qdp(np,np,nlev,qsize_d,2), stat=iret)
      call check_allocate(iret, subname, 'elem%state%Qdp(np,np,nlev,qsize_d,2)', &
                          file=__FILE__, line=__LINE__)

      !--------------------------

      !Allocate "derived" variables:
      !----------------------------

      ! velocity for SE tracer advection
      allocate(elem(i)%derived%vn0(np,np,2,nlev), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%vn0(np,np,2,nlev)', &
                          file=__FILE__, line=__LINE__)

      ! mean dp dissipation tendency, if nu_p>0
      allocate(elem(i)%derived%dpdiss_biharmonic(np,np,nlev), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%dpdiss_biharmonic(np,np,nlev)', &
                          file=__FILE__, line=__LINE__)

      ! mean dp used to compute psdiss_tens
      allocate(elem(i)%derived%dpdiss_ave(np,np,nlev), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%dpdiss_ave(np,np,nlev)', &
                          file=__FILE__, line=__LINE__)

      ! geopotential
      allocate(elem(i)%derived%phi(np,np,nlev), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%phi(np,np,nlev)', &
                          file=__FILE__, line=__LINE__)

      ! vertical velocity
      allocate(elem(i)%derived%omega(np,np,nlev), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%omega(np,np,nlev)', &
                          file=__FILE__, line=__LINE__)

      ! relative vorticity
      allocate(elem(i)%derived%zeta(np,np,nlev), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%zeta(np,np,nlev)', &
                          file=__FILE__, line=__LINE__)

      ! divergence
      allocate(elem(i)%derived%div(np,np,nlev,timelevels), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%div(np,np,nlev,timelevels)', &
                          file=__FILE__, line=__LINE__)

      ! for dp_tracers at physics timestep
      allocate(elem(i)%derived%dp(np,np,nlev), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%dp(np,np,nlev)', &
                          file=__FILE__, line=__LINE__)

      ! divergence of dp
      allocate(elem(i)%derived%divdp(np,np,nlev), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%divdp(np,np,nlev)', &
                          file=__FILE__, line=__LINE__)

      ! DSSed divdp
      allocate(elem(i)%derived%divdp_proj(np,np,nlev), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%divdp_proj(np,np,nlev)', &
                          file=__FILE__, line=__LINE__)

      ! total tracer mass for diagnostics
      allocate(elem(i)%derived%mass(max(qsize_d,ntrac_d)+9), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%mass(max(qsize_d,ntrac_d)+9)', &
                          file=__FILE__, line=__LINE__)

      ! tracer forcing
      allocate(elem(i)%derived%FQ(np,np,nlev,qsize_d), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%FQ(np,np,nlev,qsize_d)', &
                          file=__FILE__, line=__LINE__)

      ! momentum forcing
      allocate(elem(i)%derived%FM(np,np,2,nlev), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%FM(np,np,2,nlev)', &
                          file=__FILE__, line=__LINE__)

      ! save full updated dp right after physics
      allocate(elem(i)%derived%FDP(np,np,nlev), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%FDP(np,np,nlev)', &
                          file=__FILE__, line=__LINE__)

      ! temperature forcing
      allocate(elem(i)%derived%FT(np,np,nlev), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%FT(np,np,nlev)', &
                          file=__FILE__, line=__LINE__)

      ! prescribed vertical tendency
      allocate(elem(i)%derived%etadot_prescribed(np,np,nlevp), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%etadot_prescribed(np,np,nlevp)', &
                          file=__FILE__, line=__LINE__)

      ! zonal component of prescribed meteorology winds
      allocate(elem(i)%derived%u_met(np,np,nlev), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%u_met(np,np,nlev)', &
                          file=__FILE__, line=__LINE__)

      ! rate of change of zonal component of prescribed meteorology winds
      allocate(elem(i)%derived%dudt_met(np,np,nlev), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%dudt_met(np,np,nlev)', &
                          file=__FILE__, line=__LINE__)

      ! meridional component of prescribed meteorology winds
      allocate(elem(i)%derived%v_met(np,np,nlev), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%v_met(np,np,nlev)', &
                          file=__FILE__, line=__LINE__)

      ! rate of change of meridional component of prescribed meteorology winds
      allocate(elem(i)%derived%dvdt_met(np,np,nlev), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%dvdt_met(np,np,nlev)', &
                          file=__FILE__, line=__LINE__)

      ! prescribed meteorology temperature
      allocate(elem(i)%derived%T_met(np,np,nlev), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%T_met(np,np,nlev)', &
                          file=__FILE__, line=__LINE__)

      ! rate of change of prescribed meteorology temperature
      allocate(elem(i)%derived%dTdt_met(np,np,nlev), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%dTdt_met(np,np,nlev)', &
                          file=__FILE__, line=__LINE__)

      ! surface pressure of prescribed meteorology
      allocate(elem(i)%derived%ps_met(np,np), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%ps_met(np,np)', &
                          file=__FILE__, line=__LINE__)

      ! rate of change of surface pressure of prescribed meteorology
      allocate(elem(i)%derived%dpsdt_met(np,np), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%dpsdt_met(np,np)', &
                          file=__FILE__, line=__LINE__)

      ! nudging factor (prescribed)
      allocate(elem(i)%derived%nudge_factor(np,np,nlev), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%nudge_factor(np,np,nlev)', &
                          file=__FILE__, line=__LINE__)

      ! accumulated U tendency due to nudging towards prescribed met
      allocate(elem(i)%derived%Utnd(npsq,nlev), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%Utnd(npsq,nlev)', &
                          file=__FILE__, line=__LINE__)

      ! accumulated V tendency due to nudging towards prescribed met
      allocate(elem(i)%derived%Vtnd(npsq,nlev), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%Vtnd(npsq,nlev)', &
                          file=__FILE__, line=__LINE__)

      ! accumulated T tendency due to nudging towards prescribed met
      allocate(elem(i)%derived%Ttnd(npsq,nlev), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%Ttnd(npsq,nlev)', &
                          file=__FILE__, line=__LINE__)

      ! pressure perturbation from condensate
      allocate(elem(i)%derived%pecnd(np,np,nlev), stat=iret)
      call check_allocate(iret, subname, 'elem%derived%pecnd(np,np,nlev)', &
                          file=__FILE__, line=__LINE__)

      !----------------------------

      !Allocate "Metric terms":
      !-----------------------

      ! metric tensor on velocity and pressure grid
      allocate(elem(i)%met(np,np,2,2), stat=iret)
      call check_allocate(iret, subname, 'elem%met(np,np,2,2)', &
                          file=__FILE__, line=__LINE__)

      ! metric tensor on velocity and pressure grid
      allocate(elem(i)%metinv(np,np,2,2), stat=iret)
      call check_allocate(iret, subname, 'elem%metinv(np,np,2,2)', &
                          file=__FILE__, line=__LINE__)

      ! g = SQRT(det(g_ij)) on velocity and pressure grid
      allocate(elem(i)%metdet(np,np), stat=iret)
      call check_allocate(iret, subname, 'elem%metdet(np,np)', &
                          file=__FILE__, line=__LINE__)

      ! 1/metdet on velocity pressure grid
      allocate(elem(i)%rmetdet(np,np), stat=iret)
      call check_allocate(iret, subname, 'elem%rmetdet(np,np)', &
                          file=__FILE__, line=__LINE__)

      ! Map covariant field on cube to vector field on the sphere
      allocate(elem(i)%D(np,np,2,2), stat=iret)
      call check_allocate(iret, subname, 'elem%D(np,np,2,2)', &
                          file=__FILE__, line=__LINE__)

      ! Map vector field on the sphere to covariant v on cube
      allocate(elem(i)%Dinv(np,np,2,2), stat=iret)
      call check_allocate(iret, subname, 'elem%Dinv(np,np,2,2)', &
                          file=__FILE__, line=__LINE__)

      !-----------------------

      !First Coordinate:
      allocate(elem(i)%sub_elem_mass_flux(nc,nc,4,nlev), stat=iret)
      call check_allocate(iret, subname, 'elem%sub_elem_mass_flux(nc,nc,4,nlev)', &
                          file=__FILE__, line=__LINE__)

      !Spherical -> rectangular converter:
      allocate(elem(i)%vec_sphere2cart(np,np,3,2), stat=iret)
      call check_allocate(iret, subname, 'elem%vec_sphere2cart(np,np,3,2)', &
                          file=__FILE__, line=__LINE__)

      !Mass matrix on v and p grid:
      allocate(elem(i)%mp(np,np), stat=iret)
      call check_allocate(iret, subname, 'elem%mp(np,np)', &
                          file=__FILE__, line=__LINE__)

      !Inverse mass matrix on v and p grid:
      allocate(elem(i)%rmp(np,np), stat=iret)
      call check_allocate(iret, subname, 'elem%rmp(np,np)', &
                          file=__FILE__, line=__LINE__)

      !Mass matrix on v and p grid:
      allocate(elem(i)%spheremp(np,np), stat=iret)
      call check_allocate(iret, subname, 'elem%spheremp(np,np)', &
                          file=__FILE__, line=__LINE__)

      !Inverse mass matrix on v and p grid:
      allocate(elem(i)%rspheremp(np,np), stat=iret)
      call check_allocate(iret, subname, 'elem%rspheremp(np,np)', &
                          file=__FILE__, line=__LINE__)

      !Global degree of freedom (P-grid):
      allocate(elem(i)%gdofP(np,np), stat=iret)
      call check_allocate(iret, subname, 'elem%gdofP(np,np)', &
                          file=__FILE__, line=__LINE__)

      !Coriolis term:
      allocate(elem(i)%fcor(np,np), stat=iret)
      call check_allocate(iret, subname, 'elem%fcor(np,np)', &
                          file=__FILE__, line=__LINE__)

      !Index terms:
      !-----------

      allocate(elem(i)%idxP%ia(npsq), stat=iret)
      call check_allocate(iret, subname, 'elem%idxP%ia(npsq)', &
                          file=__FILE__, line=__LINE__)

      allocate(elem(i)%idxP%ja(npsq), stat=iret)
      call check_allocate(iret, subname, 'elem%idxP%ja(npsq)', &
                          file=__FILE__, line=__LINE__)

      !-----------

    end do

  end subroutine allocate_element_dims


end module element_mod
