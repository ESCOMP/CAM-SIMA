module gravity_waves_sources
  use shr_kind_mod,   only: r8 => shr_kind_r8

  !SE dycore:
  use derivative_mod, only: derivative_t
  use dimensions_mod, only: np,nlev
  use edgetype_mod,   only: EdgeBuffer_t
  use element_mod,    only: element_t
  use hybrid_mod,     only: hybrid_t

  implicit none
  private
  save

  !! gravity_waves_sources created by S Santos, 10 Aug 2011
  !!
  !! gws_src_fnct starts parallel environment and computes frontogenesis
  !!   for use by WACCM (via dp_coupling)

  public  :: gws_src_fnct
  public  :: gws_src_vort
  public  :: gws_init
  private :: compute_frontogenesis
  private :: compute_vorticity_4gw

  type (EdgeBuffer_t) :: edge3,edge1
  type (derivative_t)   :: deriv
  real(r8) :: psurf_ref

!----------------------------------------------------------------------
CONTAINS
!----------------------------------------------------------------------

  subroutine gws_init(elem)

    !SE dycore:
    use parallel_mod,   only: par
    use edge_mod,       only: initEdgeBuffer
    use hycoef,         only: hypi
    use dimensions_mod, only: nlevp
    use thread_mod,     only: horz_num_threads

    ! Elem will be needed for future updates to edge code
    type(element_t), pointer :: elem(:)

    ! Set up variables similar to dyn_comp and prim_driver_mod initializations
    call initEdgeBuffer(par, edge3, elem, 3*nlev,nthreads=1)
    call initEdgeBuffer(par, edge1, elem, nlev,nthreads=1)

    psurf_ref = hypi(nlevp)

  end subroutine gws_init

  subroutine gws_src_fnct(elem, tl, tlq, frontgf, frontga,nphys)

    use vert_coord,     only: pver
    use cam_abortutils, only: check_allocate
    use shr_kind_mod,   only: shr_kind_cl

    !SE dycore:
    use derivative_mod, only: derivinit
    use dimensions_mod, only: npsq, nelemd
    use dof_mod,        only: UniquePoints
    use hybrid_mod,     only: config_thread_region, get_loop_ranges
    use parallel_mod,   only: par
    use thread_mod,     only: horz_num_threads
    use dimensions_mod, only: fv_nphys

    type (element_t), intent(inout), dimension(:) :: elem
    integer, intent(in)          :: tl, nphys, tlq
    real (kind=r8), intent(out) :: frontgf(nphys*nphys,pver,nelemd)
    real (kind=r8), intent(out) :: frontga(nphys*nphys,pver,nelemd)

    ! Local variables
    type (hybrid_t) :: hybrid
    integer :: nets, nete, ithr, ncols, ie, iret
    real(kind=r8), allocatable  :: frontgf_thr(:,:,:,:)
    real(kind=r8), allocatable  :: frontga_thr(:,:,:,:)
    character(len=shr_kind_cl)  :: errmsg

    character(len=*), parameter :: subname = 'gws_src_fnct'

    ! This does not need to be a thread private data-structure
    call derivinit(deriv)
    !!$OMP PARALLEL NUM_THREADS(horz_num_threads),  DEFAULT(SHARED), PRIVATE(nets,nete,hybrid,ie,ncols,frontgf_thr,frontga_thr)
!    hybrid = config_thread_region(par,'horizontal')
    hybrid = config_thread_region(par,'serial')
    call get_loop_ranges(hybrid,ibeg=nets,iend=nete)

    allocate(frontgf_thr(nphys,nphys,nlev,nets:nete), stat=iret, errmsg=errmsg)
    call check_allocate(iret, subname, &
                        'frontgf_thr(nphys,nphys,nlev,nets:nete)', &
                        file=__FILE__, line=__LINE__, errmsg=errmsg)

    allocate(frontga_thr(nphys,nphys,nlev,nets:nete), stat=iret, errmsg=errmsg)
    call check_allocate(iret, subname, &
                        'frontga_thr(nphys,nphys,nlev,nets:nete)', &
                        file=__FILE__, line=__LINE__, errmsg=errmsg)

    call compute_frontogenesis(frontgf_thr,frontga_thr,tl,tlq,elem,deriv,hybrid,nets,nete,nphys)
    if (fv_nphys>0) then
      do ie=nets,nete
        frontgf(:,:,ie) = RESHAPE(frontgf_thr(:,:,:,ie),(/nphys*nphys,nlev/))
        frontga(:,:,ie) = RESHAPE(frontga_thr(:,:,:,ie),(/nphys*nphys,nlev/))
      end do
    else
      do ie=nets,nete
        ncols = elem(ie)%idxP%NumUniquePts
        call UniquePoints(elem(ie)%idxP, nlev, frontgf_thr(:,:,:,ie), frontgf(1:ncols,:,ie))
        call UniquePoints(elem(ie)%idxP, nlev, frontga_thr(:,:,:,ie), frontga(1:ncols,:,ie))
      end do
    end if
    deallocate(frontga_thr)
    deallocate(frontgf_thr)
    !!$OMP END PARALLEL

  end subroutine gws_src_fnct

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine gws_src_vort(elem, tl, tlq, vort4gw, nphys)
    use derivative_mod, only  : derivinit
    use dimensions_mod, only  : nelemd
    use dof_mod, only         : UniquePoints
    use hybrid_mod, only      : config_thread_region, get_loop_ranges
    use parallel_mod, only    : par
    use vert_coord, only      : pver
    use thread_mod, only      : horz_num_threads
    use dimensions_mod, only  : fv_nphys
    use cam_abortutils, only  : check_allocate
    use shr_kind_mod,   only  : shr_kind_cl

    implicit none
    type (element_t), intent(in), dimension(:) :: elem
    integer, intent(in)          :: tl, nphys, tlq

    !
    real (kind=r8), intent(out) :: vort4gw(nphys*nphys,pver,nelemd)

    ! Local variables
    type (hybrid_t) :: hybrid
    integer :: nets, nete, ithr, ncols, ie, ierr

    !
    real(kind=r8), allocatable  ::  vort4gw_thr(:,:,:,:)
    character(len=shr_kind_cl)  :: errmsg

    character(len=*), parameter :: subname = 'gws_src_vort'

    ! This does not need to be a thread private data-structure
    call derivinit(deriv)
    !!$OMP PARALLEL NUM_THREADS(horz_num_threads),  DEFAULT(SHARED), PRIVATE(nets,nete,hybrid,ie,ncols,vort4gw_thr)
    hybrid = config_thread_region(par,'serial')
    call get_loop_ranges(hybrid,ibeg=nets,iend=nete)

    allocate(vort4gw_thr(nphys,nphys,nlev,nets:nete), stat=ierr, errmsg=errmsg)
    call check_allocate(ierr, subname, &
                        'vort4gw_thr(nphys,nphys,nlev,nets:nete)', &
                        file=__FILE__, line=__LINE__, errmsg=errmsg)

    call compute_vorticity_4gw(vort4gw_thr,tl,tlq,elem,deriv,hybrid,nets,nete,nphys)

    if (fv_nphys>0) then
      do ie=nets,nete
        vort4gw(:,:,ie) = RESHAPE(vort4gw_thr(:,:,:,ie),(/nphys*nphys,nlev/))
      end do
    else
      do ie=nets,nete
        ncols = elem(ie)%idxP%NumUniquePts
        call UniquePoints(elem(ie)%idxP, nlev, vort4gw_thr(:,:,:,ie), vort4gw(1:ncols,:,ie))
      end do
    end if
    deallocate(vort4gw_thr)

    !!$OMP END PARALLEL

  end subroutine gws_src_vort

  subroutine compute_vorticity_4gw(vort4gw,tl,tlq,elem,ederiv,hybrid,nets,nete,nphys)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! compute vorticity for use in gw params
  !   F = ( curl ) [U,V]
  !
  ! Original by Peter Lauritzen, Julio Bacmeister*, Dec 2024
  ! Patterned on 'compute_frontogenesis'
  !
  ! * corresponding/blame-able
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use derivative_mod, only: vorticity_sphere
    use edge_mod,       only: edgevpack, edgevunpack
    use bndry_mod,      only: bndry_exchange
    use dimensions_mod, only: fv_nphys
    use fvm_mapping,    only: dyn2phys

    type(hybrid_t),     intent(in)            :: hybrid
    type(element_t),    intent(in)            :: elem(:)
    type(derivative_t), intent(in)            :: ederiv
    integer,            intent(in)            :: nets,nete,nphys
    integer,            intent(in)            :: tl,tlq
    real(r8),           intent(out)           :: vort4gw(nphys,nphys,nlev,nets:nete)

    ! local
    real(r8) :: area_inv(fv_nphys,fv_nphys), tmp(np,np)
    real(r8) :: vort_gll(np,np,nlev,nets:nete)
    integer  :: k,kptr,i,j,ie,component,h,nq,m_cnst,n0

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! First calculate vorticity on GLL grid
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! set timelevel=1 for velocities
    n0=tl
    do ie=nets,nete
       do k=1,nlev
          call vorticity_sphere(elem(ie)%state%v(:,:,:,k,n0),ederiv,elem(ie),vort_gll(:,:,k,ie))
       end do
       do k=1,nlev
          vort_gll(:,:,k,ie) = vort_gll(:,:,k,ie)*elem(ie)%spheremp(:,:)
       end do
       ! pack
       call edgeVpack(edge1, vort_gll(:,:,:,ie),nlev,0,ie)
    end do
    call bndry_exchange(hybrid,edge1,location='compute_vorticity_4gw')
    do ie=nets,nete
       call edgeVunpack(edge1, vort_gll(:,:,:,ie),nlev,0,ie)
       ! apply inverse mass matrix,
       do k=1,nlev
          vort_gll(:,:,k,ie) = vort_gll(:,:,k,ie)*elem(ie)%rspheremp(:,:)
       end do

       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       ! Now regrid from GLL to PhysGrid if necessary
       ! otherwise just return vorticity on GLL grid
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       if (fv_nphys>0) then
          tmp = 1.0_r8
          area_inv = dyn2phys(tmp,elem(ie)%metdet)
          area_inv = 1.0_r8/area_inv
          do k=1,nlev
             vort4gw(:,:,k,ie) = dyn2phys( vort_gll(:,:,k,ie) , elem(ie)%metdet , area_inv )
          end do
       else
          do k=1,nlev
             vort4gw(:,:,k,ie) = vort_gll(:,:,k,ie)
          end do
       end if
    end do


  end subroutine compute_vorticity_4gw

  subroutine compute_frontogenesis(frontgf,frontga,tl,tlq,elem,ederiv,hybrid,nets,nete,nphys)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! compute frontogenesis function F
  !   F =  -gradth dot C
  ! with:
  !   theta  = potential temperature
  !   gradth = grad(theta)
  !   C = ( gradth dot grad ) U
  !
  ! Original by Mark Taylor, July 2011
  ! Change by Santos, 10 Aug 2011:
  ! Integrated into gravity_waves_sources module, several arguments made global
  !  to prevent repeated allocation/initialization
  !
  ! Frontogenesis function correction by Walter Hannah, Mark Taylor, and Jack Chen. October 2025
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use physconst,       only: cappa
    use air_composition, only: dry_air_species_num,thermodynamic_active_species_num
    use air_composition, only: thermodynamic_active_species_idx_dycore
    use dyn_grid,        only: hvcoord

    !SE dycore:
    use derivative_mod,  only: gradient_sphere, ugradv_sphere
    use edge_mod,        only: edgevpack, edgevunpack
    use bndry_mod,       only: bndry_exchange
    use dimensions_mod,  only: fv_nphys,ntrac
    use fvm_mapping,     only: dyn2phys_vector,dyn2phys

    type(hybrid_t),     intent(in)            :: hybrid
    type(element_t),    intent(inout), target :: elem(:)
    type(derivative_t), intent(in)            :: ederiv
    integer,            intent(in)            :: nets,nete,nphys
    integer,            intent(in)            :: tl,tlq
    real(r8),           intent(out)           :: frontgf(nphys,nphys,nlev,nets:nete)
    real(r8),           intent(out)           :: frontga(nphys,nphys,nlev,nets:nete)

    ! local
    real(r8) :: area_inv(fv_nphys,fv_nphys), tmp(np,np)
    real(r8) :: uv_tmp(fv_nphys*fv_nphys,2,nlev)
    real(r8) :: frontgf_gll(np,np,nlev,nets:nete)
    real(r8) :: frontga_gll(np,np,nlev,nets:nete)
    integer  :: k,kptr,i,j,ie,component,h,nq,m_cnst
    real(r8) :: gradth(np,np,2,nlev,nets:nete) ! grad(theta)
    real(r8) :: p(np,np,nlev)               ! pressure at mid points
    real(r8) :: pint(np,np,nlev+1)          ! pressure at interface points
    real(r8) :: gradp(np,np,2)              ! grad(pressure)
    real(r8) :: theta(np,np,nlev)           ! potential temperature at mid points
    real(r8) :: dtheta_dp(np,np,nlev)       ! d(theta)/dp    for eta to pressure surface correction
    real(r8) :: grad_wind_cart(np,np,2)     ! horizontal gradient of zonal and meridional wind on cartesian coordinate on isobaric surface
    real(r8) :: wind_cart(np,np,3,nlev)     ! zonal & meridional wind on cartesian coordinate
    real(r8) :: ddp_wind_cart(np,np,3,nlev) ! vertical gradient of zonal & meridional wind on cartesian coordinate
    real(r8) :: C(np,np,2), sum_water(np,np)

    !  By Mark Taylor
    !  For a vector velocity "v", a tensor "grad(v)", and a vector "grad(theta)",
    !  this loop computes the vector "grad(theta)*grad(v)"
    !
    !  Representing the tensor "grad(v)" in spherical coordinates is difficult.  This routine
    !  avoids this by computing a mathematically equivalent form using a mixture of
    !  Cartesian and spherical coordinates
    !
    !  This routine is a modified version of derivative_mod.F90:ugradv_sphere() in that the
    !  grad(v) term is modified to compute grad_p(v) - the gradient on p-surfaces expressed
    !  in terms of the gradient on model surfaces and a vertical pressure gradient.
    !
    !  First, v is represented in cartesian coordinates  v(c) for c=1,2,3
    !  For each v(c), we compute its gradient on p-surfaces via:
    !     grad(v(c)) - d(v(c))/dz grad(p)
    !  Each of these gradients is represented in *spherical* coordinates (i=1,2)
    !
    !  We then dot each of these vectors with grad(theta).  This dot product is computed
    !  in spherical coordinates.  The end result is wind_cart(c), for c=1,2,3
    !  These three scalars are the three Cartesian coefficients of
    !  the vector "grad(theta)*grad(v)"
    !
    !  This Cartesian vector is then transformed back to spherical coordinates
    !

    do ie=nets,nete
      ! pressure at model top
      pint(:,:,1) = hvcoord%hyai(1)*hvcoord%ps0

      do k=1,nlev
        ! moist pressure at mid points
        sum_water(:,:) = 1.0_r8
        do nq=dry_air_species_num+1,thermodynamic_active_species_num
          m_cnst = thermodynamic_active_species_idx_dycore(nq)
          !
          ! make sure Q is updated
          !
          sum_water(:,:) = sum_water(:,:) + elem(ie)%state%Qdp(:,:,k,m_cnst,tlq)/elem(ie)%state%dp3d(:,:,k,tl)
        end do
        p(:,:,k) = pint(:,:,k) + 0.5_r8*sum_water(:,:)*elem(ie)%state%dp3d(:,:,k,tl)
        ! moist pressure at interface for next iteration
        pint(:,:,k+1) = pint(:,:,k)+elem(ie)%state%dp3d(:,:,k,tl)
        !
        theta(:,:,k) = elem(ie)%state%T(:,:,k,tl)*(psurf_ref / p(:,:,k))**cappa
      end do

      call compute_vertical_derivative(pint,p,theta,dtheta_dp)

      do k=1,nlev
        call gradient_sphere(theta(:,:,k),ederiv,elem(ie)%Dinv,gradth(:,:,:,k,ie))

        call gradient_sphere(p(:,:,k),ederiv,elem(ie)%Dinv,gradp)

        do component=1,2
          gradth(:,:,component,k,ie) = gradth(:,:,component,k,ie) - dtheta_dp(:,:,k) * gradp(:,:,component)
        end do
      end do

      do k=1,nlev
        do component=1,3
          wind_cart(:,:,component,k) = sum( elem(ie)%vec_sphere2cart(:,:,component,:) * elem(ie)%state%v(:,:,:,k,tl),3 )
        end do
      end do

      do component=1,3
        call compute_vertical_derivative(pint,p,wind_cart(:,:,component,:),ddp_wind_cart(:,:,component,:))
      end do
      do k=1,nlev
        call gradient_sphere(p(:,:,k),ederiv,elem(ie)%Dinv,gradp)

        do component=1,3
          call gradient_sphere(wind_cart(:,:,component,k),ederiv,elem(ie)%Dinv,grad_wind_cart)
          do i=1,2
            grad_wind_cart(:,:,i) = grad_wind_cart(:,:,i) - ddp_wind_cart(:,:,component,k) * gradp(:,:,i)
          end do
          wind_cart(:,:,component,k) = sum( gradth(:,:,:,k,ie) * grad_wind_cart , 3 )
        end do

        do i=1,2
          C(:,:,i) = sum(wind_cart(:,:,:,k)*elem(ie)%vec_sphere2cart(:,:,:,i), 3)
        end do

        ! gradth dot C
        frontgf_gll(:,:,k,ie) = -( C(:,:,1)*gradth(:,:,1,k,ie) +  C(:,:,2)*gradth(:,:,2,k,ie)  )
        ! apply mass matrix
        gradth(:,:,1,k,ie)=gradth(:,:,1,k,ie)*elem(ie)%spheremp(:,:)
        gradth(:,:,2,k,ie)=gradth(:,:,2,k,ie)*elem(ie)%spheremp(:,:)
        frontgf_gll(:,:,k,ie)=frontgf_gll(:,:,k,ie)*elem(ie)%spheremp(:,:)
      enddo
      ! pack
      call edgeVpack(edge3, frontgf_gll(:,:,:,ie),nlev,0,ie)
      call edgeVpack(edge3, gradth(:,:,:,:,ie),2*nlev,nlev,ie)
    enddo
    call bndry_exchange(hybrid,edge3,location='compute_frontogenesis')
    do ie=nets,nete
      call edgeVunpack(edge3, frontgf_gll(:,:,:,ie),nlev,0,ie)
      call edgeVunpack(edge3, gradth(:,:,:,:,ie),2*nlev,nlev,ie)
      ! apply inverse mass matrix,
      do k=1,nlev
        gradth(:,:,1,k,ie)=gradth(:,:,1,k,ie)*elem(ie)%rspheremp(:,:)
        gradth(:,:,2,k,ie)=gradth(:,:,2,k,ie)*elem(ie)%rspheremp(:,:)
        frontgf_gll(:,:,k,ie)=frontgf_gll(:,:,k,ie)*elem(ie)%rspheremp(:,:)
      end do
      if (fv_nphys>0) then
        uv_tmp(:,:,:) = dyn2phys_vector(gradth(:,:,:,:,ie),elem(ie))
        do k=1,nlev
          h=0
          do j=1,fv_nphys
            do i=1,fv_nphys
              h=h+1
              frontga(i,j,k,ie) = atan2 ( uv_tmp(h,2,k) , uv_tmp(h,1,k) + 1.e-10_r8 )
            end do
          end do
        end do
        !
        ! compute inverse physgrid area for mapping of scaler
        !
        tmp = 1.0_r8
        area_inv = dyn2phys(tmp,elem(ie)%metdet)
        area_inv = 1.0_r8/area_inv
        do k=1,nlev
          frontgf(:,:,k,ie) = dyn2phys(frontgf_gll(:,:,k,ie),elem(ie)%metdet,area_inv)
        end do
      else
        do k=1,nlev
          frontgf(:,:,k,ie)=frontgf_gll(:,:,k,ie)
          ! Frontogenesis angle
          frontga(:,:,k,ie) = atan2 ( gradth(:,:,2,k,ie) , gradth(:,:,1,k,ie) + 1.e-10_r8 )
        end do
      end if
    enddo
  end subroutine compute_frontogenesis

  subroutine compute_vertical_derivative(pint,pmid,data,ddata_dp)
    !---------------------------------------------------------------------------
    real(r8),   intent(in ) :: pint(np,np,nlev+1)
    real(r8),   intent(in ) :: pmid(np,np,nlev)
    real(r8),   intent(in ) :: data(np,np,nlev)
    real(r8),   intent(out) :: ddata_dp(np,np,nlev)
    !---------------------------------------------------------------------------
    integer :: k
    real(r8) :: pint_above(np,np) ! pressure interpolated to interface above the current k mid-point
    real(r8) :: pint_below(np,np) ! pressure interpolated to interface below the current k mid-point
    real(r8) :: dint_above(np,np) ! data interpolated to interface above the current k mid-point
    real(r8) :: dint_below(np,np) ! data interpolated to interface below the current k mid-point
    !---------------------------------------------------------------------------
    do k = 1,nlev
      if (k==1) then
        pint_above = pmid(:,:,k)
        pint_below = pint(:,:,k+1)
        dint_above = data(:,:,k)
        dint_below = ( data(:,:,k+1) + data(:,:,k) ) / 2.0_r8
      elseif (k==nlev) then
        pint_above = pint(:,:,k)
        pint_below = pmid(:,:,k)
        dint_above = ( data(:,:,k-1) + data(:,:,k) ) / 2.0_r8
        dint_below = data(:,:,k)
      else
        pint_above = pint(:,:,k)
        pint_below = pint(:,:,k+1)
        dint_above = ( data(:,:,k-1) + data(:,:,k) ) / 2.0_r8
        dint_below = ( data(:,:,k+1) + data(:,:,k) ) / 2.0_r8
      end if
      ddata_dp(:,:,k) = ( dint_above - dint_below ) / ( pint_above - pint_below )
    end do
  end subroutine compute_vertical_derivative

end module gravity_waves_sources
