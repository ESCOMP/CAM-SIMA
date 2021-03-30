#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

!MODULE FVM_CONTROL_VOLUME_MOD---------------------------------------------CE-for FVM
! AUTHOR: Christoph Erath, 11.June 2011                                             !
! This module contains everything to initialize the arrival. It also provides the   !
! interpolation points for the reconstruction (projection from one face to another  !
! when the element is on the cube edge)                                             !
! It also intialize the start values, see also fvm_analytic                         !
!-----------------------------------------------------------------------------------!
module fvm_control_volume_mod
  use shr_kind_mod,           only: r8=>shr_kind_r8
  use coordinate_systems_mod, only: spherical_polar_t
  use element_mod,            only: element_t
  use dimensions_mod,         only: nc, nhe, nlev, ntrac_d, qsize_d,ne, np, nhr, ns, nhc
  use dimensions_mod,         only: fv_nphys, nhe_phys, nhr_phys, ns_phys, nhc_phys,fv_nphys
  use dimensions_mod,         only: irecons_tracer
  use string_utils,           only: to_str
  use cam_abortutils,         only: endrun

  implicit none
  private
  integer :: nh ! = 2 (nhr=2; nhe=1)
                ! = 3 (nhr=2; nhe=2)

  type, public :: fvm_struct
    ! fvm tracer mixing ratio: (kg/kg)
    real(kind=r8), allocatable :: c(:,:,:,:)
    real(kind=r8), allocatable :: se_flux(:,:,:,:)

    real(kind=r8), allocatable :: dp_fvm(:,:,:)
    real(kind=r8), allocatable :: dp_ref(:)
    real(kind=r8), allocatable :: dp_ref_inverse(:)
    real(kind=r8), allocatable :: psc(:,:)

    real(kind=r8), allocatable :: inv_area_sphere(:,:)    ! inverse area_sphere
    real(kind=r8), allocatable :: inv_se_area_sphere(:,:) ! inverse area_sphere

    integer                  :: faceno         !face number
    ! number of south,....,swest and 0 for interior element
    integer                  :: cubeboundary

#ifdef waccm_debug
    real(kind=r8), allocatable :: CSLAM_gamma(:,:,:,:)
#endif
    real(kind=r8), allocatable :: displ_max(:,:,:)
    integer, allocatable       :: flux_vec(:,:,:,:)
    !
    !
    ! cartesian location of vertices for flux sides
    !
    !  x-coordinate of vertex 1: vtx_cart(1,1i,j,1,1)  = fvm%acartx(i)
    !  y-coordinate of vertex 1: vtx_cart(1,2,i,j,2,1) = fvm%acarty(j)
    !
    !  x-coordinate of vertex 2: vtx_cart(2,1,i,j) = fvm%acartx(i+1)
    !  y-coordinate of vertex 2: vtx_cart(2,2,i,j) = fvm%acarty(j  )
    !
    !  x-coordinate of vertex 3: vtx_cart(3,1,i,j) = fvm%acartx(i+1)
    !  y-coordinate of vertex 3: vtx_cart(3,2,i,j) = fvm%acarty(j+1)
    !
    !  x-coordinate of vertex 4: vtx_cart(4,1,i,j) = fvm%acartx(i  )
    !  y-coordinate of vertex 4: vtx_cart(4,2,i,j) = fvm%acarty(j+1)
    !
    real(kind=r8), allocatable :: vtx_cart(:,:,:,:)
    !
    ! flux_orient(1,i,j) = panel on which control volume (i,j) is located
    ! flux_orient(2,i,j) = cshift value for vertex permutation
    !
    real(kind=r8), allocatable :: flux_orient(:,:,:)
    !
    ! i,j: indicator function for non-existent cells (0 for corner halo and 1 elsewhere)
    !
    integer, allocatable     :: ifct(:,:)
    integer, allocatable     :: rot_matrix(:,:,:,:)
    !
    real(kind=r8)           :: dalpha, dbeta                      ! central-angle for gnomonic coordinates
    type(spherical_polar_t), allocatable :: center_cart(:,:)      ! center of fvm cell in gnomonic coordinates
    real(kind=r8),           allocatable :: area_sphere(:,:)      ! spherical area of fvm cell
    real(kind=r8),           allocatable :: spherecentroid(:,:,:) ! centroids
    !
    ! pre-computed metric terms (for efficiency)
    !
    ! recons_metrics(1,:,:) = spherecentroid(1,:,:)**2 -spherecentroid(3,:,:)
    ! recons_metrics(2,:,:) = spherecentroid(2,:,:)**2 -spherecentroid(4,:,:)
    ! recons_metrics(3,:,:) = spherecentroid(1,:,:)*spherecentroid(2,:,:)-spherecentroid(5,:,:)

    real(kind=r8), allocatable :: recons_metrics(:,:,:)
    !
    ! recons_metrics_integral(1,:,:) = 2.0_r8*spherecentroid(1,:,:)**2 -spherecentroid(3,:,:)
    ! recons_metrics_integral(2,:,:) = 2.0_r8*spherecentroid(2,:,:)**2 -spherecentroid(4,:,:)
    ! recons_metrics_integral(3,:,:) = 2.0_r8*spherecentroid(1,:,:)*spherecentroid(2,:,:)-spherecentroid(5,:,:)
    !
    real(kind=r8), allocatable :: recons_metrics_integral(:,:,:)
    !
    integer                    :: jx_min(3), jx_max(3), jy_min(3), jy_max(3) !bounds for computation

    ! provide fixed interpolation points with respect to the arrival grid for
    ! reconstruction
    integer, allocatable        :: ibase(:,:,:)
    real(kind=r8), allocatable  :: halo_interp_weight(:,:,:,:)
    real(kind=r8), allocatable  :: centroid_stretch(:,:,:) !for finite-difference reconstruction
    !
    ! pre-compute weights for reconstruction at cell vertices
    !
    !  ! Evaluate constant order terms
    !  value = fcube(a,b) + &
    !  ! Evaluate linear order terms
    !          recons(1,a,b) * (cartx - centroid(1,a,b)) + &
    !          recons(2,a,b) * (carty - centroid(2,a,b)) + &
    !  ! Evaluate second order terms
    !          recons(3,a,b) * (centroid(1,a,b)**2 - centroid(3,a,b)) + &
    !          recons(4,a,b) * (centroid(2,a,b)**2 - centroid(4,a,b)) + &
    !          recons(5,a,b) * (centroid(1,a,b) * centroid(2,a,b) - centroid(5,a,b)) + &
    !
    !          recons(3,a,b) * (cartx - centroid(1,a,b))**2 + &
    !          recons(4,a,b) * (carty - centroid(2,a,b))**2 + &
    !          recons(5,a,b) * (cartx - centroid(1,a,b)) * (carty - centroid(2,a,b))
    !
    real(kind=r8), allocatable :: vertex_recons_weights(:,:,:,:)
    !
    ! for mapping fvm2dyn
    !
    real(kind=r8), allocatable :: norm_elem_coord(:,:,:)
    !
    !******************************************
    !
    ! separate physics grid variables
    !
    !******************************************
    !
    real(kind=r8)           , allocatable :: phis_physgrid(:,:)
    real(kind=r8)           , allocatable :: vtx_cart_physgrid(:,:,:,:)
    real(kind=r8)           , allocatable :: flux_orient_physgrid(:,:,:)
    integer                 , allocatable :: ifct_physgrid(:,:)
    integer                 , allocatable :: rot_matrix_physgrid(:,:,:,:)
    real(kind=r8)           , allocatable :: spherecentroid_physgrid(:,:,:)
    real(kind=r8)           , allocatable :: recons_metrics_physgrid(:,:,:)
    real(kind=r8)           , allocatable :: recons_metrics_integral_physgrid(:,:,:)
    ! centroid_stretch_physgrid for finite-difference reconstruction
    real(kind=r8)           , allocatable :: centroid_stretch_physgrid       (:,:,:)
    real(kind=r8)                         :: dalpha_physgrid, dbeta_physgrid             ! central-angle for gnomonic coordinates
    type(spherical_polar_t) , allocatable :: center_cart_physgrid(:,:)        ! center of fvm cell in gnomonic coordinates
    real(kind=r8)           , allocatable :: area_sphere_physgrid(:,:)        ! spherical area of fvm cell
    integer                               :: jx_min_physgrid(3), jx_max_physgrid(3) !bounds for computation
    integer                               :: jy_min_physgrid(3), jy_max_physgrid(3) !bounds for computation
    integer                 , allocatable :: ibase_physgrid(:,:,:)
    real(kind=r8)           , allocatable :: halo_interp_weight_physgrid(:,:,:,:)
    real(kind=r8)           , allocatable :: vertex_recons_weights_physgrid(:,:,:,:)

    real(kind=r8)           , allocatable :: norm_elem_coord_physgrid(:,:,:)
    real(kind=r8)           , allocatable :: Dinv_physgrid(:,:,:,:)

    real(kind=r8)           , allocatable :: fc(:,:,:,:)
    real(kind=r8)           , allocatable :: fc_phys(:,:,:,:)
    real(kind=r8)           , allocatable :: ft(:,:,:)
    real(kind=r8)           , allocatable :: fm(:,:,:,:)
    real(kind=r8)           , allocatable :: dp_phys(:,:,:)
  end type fvm_struct

  public :: fvm_mesh, fvm_set_cubeboundary, allocate_physgrid_vars
  public :: allocate_fvm_dims

  real(kind=r8),parameter, public   :: bignum = 1.0E20_r8

!==============================================================================
contains
!==============================================================================

  subroutine fvm_set_cubeboundary(elem, fvm)
    implicit none
    type (element_t) , intent(in)      :: elem
    type (fvm_struct), intent(inout)   :: fvm

    logical                            :: corner
    integer                            :: j, mynbr_cnt, mystart
    integer                            :: nbrsface(8)! store the neighbours in north, south

    fvm%faceno=elem%FaceNum
    ! write the neighbors in the structure
    fvm%cubeboundary=0
    corner=.FALSE.
    do j=1,8
       mynbr_cnt = elem%vertex%nbrs_ptr(j+1) - elem%vertex%nbrs_ptr(j) !length of neighbor location
       mystart = elem%vertex%nbrs_ptr(j)
       !NOTE: assuming that we do not have multiple corner neighbors (so not a refined mesh)
       if (mynbr_cnt > 0 ) then
          nbrsface(j)=elem%vertex%nbrs_face(mystart)
          ! note that if the element lies on a corner, it will be at j=5,6,7,8
          if ((nbrsface(j) /= fvm%faceno) .AND. (j<5)) then
             fvm%cubeboundary=j
          endif
       else   ! corner on the cube
          if (.NOT. corner) then
             nbrsface(j)=-1
             fvm%cubeboundary=j
             corner=.TRUE.
          else
             if ( ne == 0 ) then
                ! dont check this condition.  note that we call this code
                ! generate phys grid template files, so we need to be able
                ! to call create_ari() to create the subcells even though
                ! cslam cant run with the unstructed ne=0 case
             else
                print *,'Error in fvm_CONTROL_VOLUME_MOD - Subroutine fvm_MESH_ARI: '
                call endrun('Do not allow one element per face for fvm, please increase ne!')
             endif
          endif
       end if
    end do
  end subroutine fvm_set_cubeboundary

  subroutine fvm_mesh(elem, fvm)
    use fvm_analytic_mod, only : compute_halo_vars
    use fvm_analytic_mod, only : create_interpolation_points
    use derivative_mod  , only : subcell_integration

    implicit none
    type (element_t), intent(in)     :: elem
    type (fvm_struct), intent(inout) :: fvm
    integer :: i,j
    real (kind=r8)            :: tmp(np,np)
    !
    ! initialize metric and related terms on panel
    !
    call compute_halo_vars(&    !input
         fvm%faceno,fvm%cubeboundary,nc,nhc,nhe,   &  !input
         fvm%jx_min,fvm%jx_max,fvm%jy_min,fvm%jy_max,&!output
         fvm%flux_orient,fvm%ifct,fvm%rot_matrix)     !output
    do j=1,nc
      do i=1,nc
        fvm%norm_elem_coord(1,i,j) = elem%corners(1)%x+(i-0.5_r8)*fvm%dalpha
        fvm%norm_elem_coord(2,i,j) = elem%corners(1)%y+(j-0.5_r8)*fvm%dalpha
      end do
    end do

    !
    ! overwrite areas for consistency with SE areas (that are O(10E-5) incorrect)
    !
!    tmp = 1.0_r8
!    call subcell_integration(tmp, np, nc, elem%metdet,fvm%area_sphere)
    !
    ! do the same for physics grid
    !
    call compute_halo_vars(&
         fvm%faceno,fvm%cubeboundary,fv_nphys,nhc_phys,nhe_phys,&
         fvm%jx_min_physgrid,fvm%jx_max_physgrid,fvm%jy_min_physgrid,fvm%jy_max_physgrid,&
         fvm%flux_orient_physgrid,fvm%ifct_physgrid,fvm%rot_matrix_physgrid)
    do j=1,fv_nphys
      do i=1,fv_nphys
        fvm%norm_elem_coord_physgrid(1,i,j) = elem%corners(1)%x+(i-0.5_r8)*fvm%dalpha_physgrid
        fvm%norm_elem_coord_physgrid(2,i,j) = elem%corners(1)%y+(j-0.5_r8)*fvm%dalpha_physgrid
      end do
    end do
    !
    ! initialize halo interpolation variables
    !
    call create_interpolation_points(elem,&
         nc,nhc,nhr,ns,nh,fvm%cubeboundary,&
         fvm%dalpha,fvm%dbeta,fvm%ibase,fvm%halo_interp_weight)
    call create_interpolation_points(elem,&
         fv_nphys,nhc_phys,nhr_phys,ns_phys,nhr_phys,fvm%cubeboundary,&
         fvm%dalpha_physgrid,fvm%dbeta_physgrid,fvm%ibase_physgrid,fvm%halo_interp_weight_physgrid)
  end subroutine fvm_mesh


  subroutine allocate_physgrid_vars(fvm,par)
    use cam_logfile   , only : iulog
    use parallel_mod  , only : parallel_t
    use dimensions_mod, only : nelemd
    type (fvm_struct), intent(inout) :: fvm(:)
    type (parallel_t), intent(in)    :: par
    integer :: ie, iret

    character(len=*), parameter :: subname = 'allocate_physgrid_vars (SE)'

    nhc_phys = fv_nphys
    nhe_phys = 0
    nhr_phys = 2
    ns_phys  = MAX(fv_nphys,2)

    if(par%masterproc) then
      write(iulog,*)"allocating physgrid grid vars"
      write(iulog,*)"fv_nphys,nhc_phys,nhe_phys,nhr_phys,ns_phys = ",&
           fv_nphys,nhc_phys,nhe_phys,nhr_phys,ns_phys
    end if

    do ie=1,nelemd
      allocate(fvm(ie)%phis_physgrid          (fv_nphys,fv_nphys), stat=iret)
      if (iret /= 0) then
         call endrun(subname//': allocate fvm(ie)%phis_physgrid(fv_nphys,fv_nphys) failed with stat: '//&
                     to_str(iret))
      end if

      allocate(fvm(ie)%vtx_cart_physgrid      (4,2,1-nhc_phys:fv_nphys+nhc_phys,1-nhc_phys:fv_nphys+nhc_phys), stat=iret)
      if (iret /= 0) then
         call endrun(subname//': allocate '//&
                     'fvm(ie)%vtx_cart_physgrid(4,2,1-nhc_phys:fv_nphys+nhc_phys,1-nhc_phys:fv_nphys+nhc_phys)'//&
                     ' failed with stat: '//to_str(iret))
      end if

      allocate(fvm(ie)%flux_orient_physgrid   (2,1-nhc_phys:fv_nphys+nhc_phys,1-nhc_phys:fv_nphys+nhc_phys), stat=iret)
      if (iret /= 0) then
         call endrun(subname//': allocate '//&
                     'fvm(ie)%flux_orient_physgrid(2,1-nhc_phys:fv_nphys+nhc_phys,1-nhc_phys:fv_nphys+nhc_phys)'//&
                     ' failed with stat: '//to_str(iret))
      end if

      allocate(fvm(ie)%ifct_physgrid         (1-nhc_phys:fv_nphys+nhc_phys,1-nhc_phys:fv_nphys+nhc_phys), stat=iret)
      if (iret /= 0) then
         call endrun(subname//': allocate '//&
                     'fvm(ie)%ifct_physgrid(1-nhc_phys:fv_nphys+nhc_phys,1-nhc_phys:fv_nphys+nhc_phys)'//&
                     ' failed with stat: '//to_str(iret))
      end if

      allocate(fvm(ie)%rot_matrix_physgrid   (2,2,1-nhc_phys:fv_nphys+nhc_phys,1-nhc_phys:fv_nphys+nhc_phys), stat=iret)
      if (iret /= 0) then
         call endrun(subname//': allocate '//&
                     'fvm(ie)%rot_matrix_physgrid(2,2,1-nhc_phys:fv_nphys+nhc_phys,1-nhc_phys:fv_nphys+nhc_phys)'//&
                     ' failed with stat: '//to_str(iret))
      end if

      allocate(fvm(ie)%spherecentroid_physgrid(irecons_tracer-1,&
           1-nhc_phys:fv_nphys+nhc_phys,1-nhc_phys:fv_nphys+nhc_phys), stat=iret)
      if (iret /= 0) then
         call endrun(subname//': allocate '//&
                     'fvm(ie)%spherecentroid_physgrid(irecons_tracer-1,1-nhc_phys:fv_nphys+nhc_phys,1-nhc_phys:fv_nphys+nhc_phys)'//&
                     ' failed with stat: '//to_str(iret))
      end if

      allocate(fvm(ie)%recons_metrics_physgrid         (3,1-nhe_phys:fv_nphys+nhe_phys,1-nhe_phys:fv_nphys+nhe_phys), stat=iret)
      if (iret /= 0) then
         call endrun(subname//': allocate '//&
                     'fvm(ie)%recons_metrics_physgrid(3,1-nhe_phys:fv_nphys+nhe_phys,1-nhe_phys:fv_nphys+nhe_phys)'//&
                     ' failed with stat: '//to_str(iret))
      end if

      allocate(fvm(ie)%recons_metrics_integral_physgrid(3,1-nhe_phys:fv_nphys+nhe_phys,1-nhe_phys:fv_nphys+nhe_phys), stat=iret)
      if (iret /= 0) then
         call endrun(subname//': allocate '//&
                     'fvm(ie)%recons_metrics_integral_physgrid(3,1-nhe_phys:fv_nphys+nhe_phys,1-nhe_phys:fv_nphys+nhe_phys)'//&
                     ' failed with stat: '//to_str(iret))
      end if

      allocate(fvm(ie)%centroid_stretch_physgrid       (7,1-nhe_phys:fv_nphys+nhe_phys,1-nhe_phys:fv_nphys+nhe_phys), stat=iret)
      if (iret /= 0) then
         call endrun(subname//': allocate '//&
                     'fvm(ie)%centroid_stretch_physgrid(7,1-nhe_phys:fv_nphys+nhe_phys,1-nhe_phys:fv_nphys+nhe_phys)'//&
                     ' failed with stat: '//to_str(iret))
      end if

      allocate(fvm(ie)%center_cart_physgrid(fv_nphys,fv_nphys), stat=iret)
      if (iret /= 0) then
         call endrun(subname//': allocate fvm(ie)%center_cart_physgrid(fv_nphys,fv_nphys) failed with stat: '//to_str(iret))
      end if

      allocate(fvm(ie)%area_sphere_physgrid(fv_nphys,fv_nphys), stat=iret)
      if (iret /= 0) then
         call endrun(subname//': allocate fvm(ie)%area_sphere_physgrid(fv_nphys,fv_nphys) failed with stat: '//to_str(iret))
      end if

      allocate(fvm(ie)%ibase_physgrid(1-nhr_phys:fv_nphys+nhr_phys,1:nhr_phys,2), stat=iret)
      if (iret /= 0) then
         call endrun(subname//': allocate fvm(ie)%ibase_physgrid(1-nhr_phys:fv_nphys+nhr_phys,1:nhr_phys,2)'//&
                     ' failed with stat: '//to_str(iret))
      end if

      allocate(fvm(ie)%halo_interp_weight_physgrid(1:ns_phys,1-nhr_phys:fv_nphys+nhr_phys,1:nhr_phys,2), stat=iret)
      if (iret /= 0) then
         call endrun(subname//': allocate '//&
                     'fvm(ie)%halo_interp_weight_physgrid(1:ns_phys,1-nhr_phys:fv_nphys+nhr_phys,1:nhr_phys,2)'//&
                     ' failed with stat: '//to_str(iret))
      end if

      allocate(fvm(ie)%vertex_recons_weights_physgrid(4,1:irecons_tracer-1,1-nhe_phys:fv_nphys+nhe_phys,&
            1-nhe_phys:fv_nphys+nhe_phys), stat=iret)
      if (iret /= 0) then
         call endrun(subname//': allocate '//&
                     'fvm(ie)%vertex_recons_weights_physgrid(4,1:irecons_tracer-1,1-nhe_phys:fv_nphys+nhe_phys,'//&
                     '1-nhe_phys:fv_nphys+nhe_phys) failed with stat: '//to_str(iret))
      end if

      allocate(fvm(ie)%norm_elem_coord_physgrid(2,1-nhc_phys:fv_nphys+nhc_phys,1-nhc_phys:fv_nphys+nhc_phys    ))
      if (iret /= 0) then
         call endrun(subname//': allocate '//&
                     'fvm(ie)%vertex_recons_weights_physgrid(4,1:irecons_tracer-1,1-nhe_phys:fv_nphys+nhe_phys,'//&
                     '1-nhe_phys:fv_nphys+nhe_phys) failed with stat: '//to_str(iret))
      end if

      allocate(fvm(ie)%Dinv_physgrid           (  1-nhc_phys:fv_nphys+nhc_phys,1-nhc_phys:fv_nphys+nhc_phys,2,2), stat=iret)
      if (iret /= 0) then
         call endrun(subname//': allocate '//&
                     'fvm(ie)%Dinv_physgrid(1-nhc_phys:fv_nphys+nhc_phys,1-nhc_phys:fv_nphys+nhc_phys,2,2)'//&
                     ' failed with stat: '//to_str(iret))
      end if

      allocate(fvm(ie)%fc(nc,nc,nlev,max(ntrac_d,qsize_d)), stat=iret)
      if (iret /= 0) then
         call endrun(subname//': allocate fvm(ie)%fc(nc,nc,nlev,max(ntrac_d,qsize_d)) failed with stat: '//to_str(iret))
      end if

      allocate(fvm(ie)%fc_phys(1-nhc_phys:fv_nphys+nhc_phys,1-nhc_phys:fv_nphys+nhc_phys,nlev,max(ntrac_d,qsize_d)), stat=iret)
      if (iret /= 0) then
         call endrun(subname//': allocate '//&
                     'fvm(ie)%fc_phys(1-nhc_phys:fv_nphys+nhc_phys,1-nhc_phys:fv_nphys+nhc_phys,nlev,max(ntrac_d,qsize_d))'//&
                     ' failed with stat: '//to_str(iret))
      end if

      allocate(fvm(ie)%ft(1-nhc_phys:fv_nphys+nhc_phys,1-nhc_phys:fv_nphys+nhc_phys,nlev), stat=iret)
      if (iret /= 0) then
         call endrun(subname//': allocate fvm(ie)%ft(1-nhc_phys:fv_nphys+nhc_phys,1-nhc_phys:fv_nphys+nhc_phys,nlev)'//&
                     ' failed with stat: '//to_str(iret))
      end if

      allocate(fvm(ie)%fm(1-nhc_phys:fv_nphys+nhc_phys,1-nhc_phys:fv_nphys+nhc_phys,2,nlev), stat=iret)
      if (iret /= 0) then
         call endrun(subname//': allocate fvm(ie)%fm(1-nhc_phys:fv_nphys+nhc_phys,1-nhc_phys:fv_nphys+nhc_phys,2,nlev)'//&
                     ' failed with stat: '//to_str(iret))
      end if

      allocate(fvm(ie)%dp_phys(1-nhc_phys:fv_nphys+nhc_phys,1-nhc_phys:fv_nphys+nhc_phys,nlev), stat=iret)
      if (iret /= 0) then
         call endrun(subname//': allocate fvm(ie)%dp_phys(1-nhc_phys:fv_nphys+nhc_phys,1-nhc_phys:fv_nphys+nhc_phys,nlev)'//&
                     ' failed with stat: '//to_str(iret))
      end if

    end do
  end subroutine allocate_physgrid_vars

  !======================

  subroutine allocate_fvm_dims(fvm)

  ! Allocate the SE FVM arrays using the pre-calculated SE dimensions

  use dimensions_mod, only: nelemd

  !Dummy arguments:
  type(fvm_struct), intent(inout) :: fvm(:)

  !Local arguments
  integer :: ie, iret

  character(len=*), parameter :: subname = 'allocate_fvm_dims (SE)'

  !---------------

  !Set "nh" integer:
  nh = nhr+(nhe-1)

  do ie=1,nelemd

     !fvm tracer mixing ratio:
     allocate(fvm(ie)%c(1-nhc:nc+nhc,1-nhc:nc+nhc,nlev,ntrac_d), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate fvm(ie)%c(1-nhc:nc+nhc,1-nhc:nc+nhc,nlev,ntrac_d) failed with stat: '//&
                    to_str(iret))
     end if

     allocate(fvm(ie)%se_flux(1-nhe:nc+nhe,1-nhe:nc+nhe,4,nlev), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate fvm(ie)%se_flux(1-nhe:nc+nhe,1-nhe:nc+nhe,4,nlev) failed with stat: '//&
                    to_str(iret))
     end if

     allocate(fvm(ie)%dp_fvm(1-nhc:nc+nhc,1-nhc:nc+nhc,nlev), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate fvm(ie)%dp_fvm(1-nhc:nc+nhc,1-nhc:nc+nhc,nlev) failed with stat: '//&
                    to_str(iret))
     end if

     allocate(fvm(ie)%dp_ref(nlev), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate fvm(ie)%dp_ref(nlev) failed with stat: '//&
                    to_str(iret))
     end if

     allocate(fvm(ie)%dp_ref_inverse(nlev), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate fvm(ie)%dp_ref_inverse(nlev) failed with stat: '//&
                    to_str(iret))
     end if

     allocate(fvm(ie)%psc(nc,nc), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate fvm(ie)%psc(nc,nc) failed with stat: '//&
                    to_str(iret))
     end if

     ! inverse area_sphere
     allocate(fvm(ie)%inv_area_sphere(nc,nc), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate fvm(ie)%inv_area_sphere(nc,nc) failed with stat: '//&
                    to_str(iret))
     end if

     ! inverse area_sphere
     allocate(fvm(ie)%inv_se_area_sphere(nc,nc), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate fvm(ie)%inv_se_area_sphere(nc,nc) failed with stat: '//&
                    to_str(iret))
     end if

#ifdef waccm_debug
     allocate(fvm(ie)%CSLAM_gamma(nc,nc,nlev,4), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate fvm(ie)%CSLAM_gamma(nc,nc,nlev,4) failed with stat: '//&
                    to_str(iret))
     end if
#endif

     allocate(fvm(ie)%displ_max(1-nhc:nc+nhc,1-nhc:nc+nhc,4), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate fvm(ie)%displ_max(1-nhc:nc+nhc,1-nhc:nc+nhc,4) failed with stat: '//&
                    to_str(iret))
     end if

     allocate(fvm(ie)%flux_vec(2,1-nhc:nc+nhc,1-nhc:nc+nhc,4), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate fvm(ie)%flux_vec(2,1-nhc:nc+nhc,1-nhc:nc+nhc,4) failed with stat: '//&
                    to_str(iret))
     end if

     ! cartesian location of vertices for flux sides
     allocate(fvm(ie)%vtx_cart(4,2,1-nhc:nc+nhc,1-nhc:nc+nhc), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate fvm(ie)%vtx_cart(4,2,1-nhc:nc+nhc,1-nhc:nc+nhc) failed with stat: '//&
                    to_str(iret))
     end if

     allocate(fvm(ie)%flux_orient(2,1-nhc:nc+nhc,1-nhc:nc+nhc), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate fvm(ie)%flux_orient(2,1-nhc:nc+nhc,1-nhc:nc+nhc) failed with stat: '//&
                    to_str(iret))
     end if

     ! indicator function for non-existent cells
     allocate(fvm(ie)%ifct(1-nhc:nc+nhc,1-nhc:nc+nhc), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate fvm(ie)%ifct(1-nhc:nc+nhc,1-nhc:nc+nhc) failed with stat: '//&
                    to_str(iret))
     end if

     allocate(fvm(ie)%rot_matrix(2,2,1-nhc:nc+nhc,1-nhc:nc+nhc), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate fvm(ie)%rot_matrix(2,2,1-nhc:nc+nhc,1-nhc:nc+nhc) failed with stat: '//&
                    to_str(iret))
     end if

     ! center of fvm cell in gnomonic coordinates
     allocate(fvm(ie)%center_cart(nc,nc), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate fvm(ie)%center_cart(nc,nc) failed with stat: '//&
                    to_str(iret))
     end if

     ! spherical area of fvm cell
     allocate(fvm(ie)%area_sphere(nc,nc), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate fvm(ie)%area_sphere(nc,nc) failed with stat: '//&
                    to_str(iret))
     end if

     ! centroids
     allocate(fvm(ie)%spherecentroid(irecons_tracer-1,1-nhc:nc+nhc,1-nhc:nc+nhc), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate fvm(ie)%spherecentroid(irecons_tracer-1,1-nhc:nc+nhc,1-nhc:nc+nhc)'//&
                    ' failed with stat: '//to_str(iret))
     end if

     ! pre-computed metric terms (for efficiency)
     allocate(fvm(ie)%recons_metrics(3,1-nhe:nc+nhe,1-nhe:nc+nhe), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate fvm(ie)%recons_metrics(3,1-nhe:nc+nhe,1-nhe:nc+nhe)'//&
                    ' failed with stat: '//to_str(iret))
     end if

     allocate(fvm(ie)%recons_metrics_integral(3,1-nhe:nc+nhe,1-nhe:nc+nhe), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate fvm(ie)%recons_metrics_integral(3,1-nhe:nc+nhe,1-nhe:nc+nhe)'//&
                    ' failed with stat: '//to_str(iret))
     end if

     ! provide fixed interpolation points with respect to the arrival grid for reconstruction
     allocate(fvm(ie)%ibase(1-nh:nc+nh,1:nhr,2), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate fvm(ie)%ibase(1-nh:nc+nh,1:nhr,2) failed with stat: '//&
                    to_str(iret))
     end if

     allocate(fvm(ie)%halo_interp_weight(1:ns,1-nh:nc+nh,1:nhr,2), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate fvm(ie)%halo_interp_weight(1:ns,1-nh:nc+nh,1:nhr,2) failed with stat: '//&
                    to_str(iret))
     end if

     ! for finite-difference reconstruction
     allocate(fvm(ie)%centroid_stretch(7,1-nhe:nc+nhe,1-nhe:nc+nhe), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate fvm(ie)%centroid_stretch(7,1-nhe:nc+nhe,1-nhe:nc+nhe)'//&
                    ' failed with stat: '//to_str(iret))
     end if

     ! pre-compute weights for reconstruction at cell vertices
     allocate(fvm(ie)%vertex_recons_weights(4,1:irecons_tracer-1,1-nhe:nc+nhe,1-nhe:nc+nhe), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate '//&
                    'fvm(ie)%vertex_recons_weights(4,1:irecons_tracer-1,1-nhe:nc+nhe,1-nhe:nc+nhe)'//&
                    ' failed with stat: '//to_str(iret))
     end if

     ! for mapping fvm2dyn
     allocate(fvm(ie)%norm_elem_coord(2,1-nhc:nc+nhc,1-nhc:nc+nhc), stat=iret)
     if (iret /= 0) then
        call endrun(subname//': allocate fvm(ie)%norm_elem_coord(2,1-nhc:nc+nhc,1-nhc:nc+nhc)'//&
                    ' failed with stat: '//to_str(iret))
     end if

  end do

  end subroutine allocate_fvm_dims

  !======================

end module fvm_control_volume_mod
