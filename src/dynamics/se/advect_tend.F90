!----------------------------------------------------------------------
! this module computes the total advection tendencies of advected
! constituents for the finite volume dycore
!----------------------------------------------------------------------
module advect_tend

  use shr_kind_mod, only : r8 => shr_kind_r8

  private

  public :: compute_adv_tends_xyz

  real(r8), allocatable :: adv_tendxyz(:,:,:,:,:)

contains

  !----------------------------------------------------------------------
  ! computes the total advective tendencies
  ! called twice each time step:
  !   - first call sets the initial mixing ratios
  !   - second call computes and outputs the tendencies
  !----------------------------------------------------------------------
  subroutine compute_adv_tends_xyz(elem,fvm,nets,nete,qn0,n0)

    use shr_kind_mod,          only: cl=>shr_kind_cl
    use cam_history,           only: history_out_field
    use time_manager,          only: get_step_size
!    use cam_constituents,      only: tottnam,num_advected
    use cam_constituents,      only: num_advected
    use cam_abortutils,        only: check_allocate

    ! SE dycore:
    use dimensions_mod,         only: nc,np,nlev,ntrac,use_cslam
    use element_mod,            only: element_t
    use fvm_control_volume_mod, only: fvm_struct

    implicit none

    type (element_t), intent(in) :: elem(:)
    type(fvm_struct), intent(in) :: fvm(:)
    integer,          intent(in) :: nets,nete,qn0,n0
    real(r8) :: dt,idt
    integer  :: i,j,ic,nx,ie,iret
    logical  :: init
    real(r8), allocatable, dimension(:,:) :: ftmp

    character(len=cl) :: errmsg

    character(len=*), parameter :: subname = 'compute_adv_tends_xyz'

    if (use_cslam) then
      nx=nc
    else
      nx=np
    end if

    allocate( ftmp(nx*nx,nlev), stat=iret, errmsg=errmsg )
    call check_allocate(iret, subname, 'ftmp(nx*nx,nlev)', &
                        file=__FILE__, line=__LINE__, errmsg=errmsg)

    init = .false.
    if ( .not. allocated( adv_tendxyz ) ) then
      init = .true.
      allocate( adv_tendxyz(nx,nx,nlev,num_advected,nets:nete), stat=iret, errmsg=errmsg )
      call check_allocate(iret, subname, 'adv_tendxyz(nx,nx,nlev,num_advected,nets:nete)', &
                          file=__FILE__, line=__LINE__, errmsg=errmsg)

      adv_tendxyz(:,:,:,:,:) = 0._r8
    end if

    if (use_cslam) then
      do ie=nets,nete
        do ic = 1, num_advected
          adv_tendxyz(:,:,:,ic,ie) = fvm(ie)%c(1:nc,1:nc,:,ic) - adv_tendxyz(:,:,:,ic,ie)
        end do
      end do
    else
      do ie=nets,nete
        do ic = 1, num_advected
          adv_tendxyz(:,:,:,ic,ie) = elem(ie)%state%Qdp(:,:,:,ic,qn0)/elem(ie)%state%dp3d(:,:,:,n0)  - adv_tendxyz(:,:,:,ic,ie)
        end do
      end do
    end if

#ifdef cam_thermo_history
    if ( .not. init ) then
      dt = get_step_size()
      idt = 1._r8/dt

      do ie=nets,nete
        do ic = 1, num_advected
          do j=1,nx
            do i=1,nx
              ftmp(i+(j-1)*nx,:) = adv_tendxyz(i,j,:,ic,ie)
            end do
          end do
          call history_out_field(tottnam(ic), ftmp)
        end do
      end do
      deallocate(adv_tendxyz)
    end if
    deallocate(ftmp)
#else
    if (.not. init) then
      deallocate(adv_tendxyz)
    end if
    deallocate(ftmp)
#endif
  end subroutine compute_adv_tends_xyz
#ifdef SCAM
  !----------------------------------------------------------------------
  ! computes camiop specific tendencies
  ! and writes these to the camiop file
  ! called twice each time step:
  !   - first call sets the initial mixing ratios/state
  !   - second call computes and outputs the tendencies
  !----------------------------------------------------------------------
  subroutine compute_write_iop_fields(elem,fvm,nets,nete,qn0,n0)
    use cam_abortutils,         only: endrun
    use cam_history,            only: outfld, hist_fld_active
    use time_manager,           only: get_step_size
    use constituents,           only: pcnst,cnst_name
    use dimensions_mod,         only: nc,np,nlev,use_cslam,npsq
    use element_mod,            only: element_t
    use fvm_control_volume_mod, only: fvm_struct
    implicit none

    type (element_t), intent(inout) :: elem(:)
    type(fvm_struct), intent(inout) :: fvm(:)
    integer,          intent(in) :: nets,nete,qn0,n0
    real(r8) :: dt
    real(r8), allocatable        :: q_new(:,:,:)
    real(r8), allocatable        :: q_adv(:,:,:)
    real(r8), allocatable        :: t_adv(:,:)
    real(r8), allocatable        :: out_q(:,:)
    real(r8), allocatable        :: out_t(:,:)
    real(r8), allocatable        :: out_u(:,:)
    real(r8), allocatable        :: out_v(:,:)
    real(r8), allocatable        :: out_ps(:)

    integer  :: i,j,ic,nx,ie,nxsq,p
    integer  :: ierr
    logical  :: init
    character(len=*), parameter :: sub = 'compute_write_iop_fields:'
    !----------------------------------------------------------------------------

    if (use_cslam) then
      nx=nc
    else
      nx=np
    end if
    nxsq=nx*nx

    init = .false.
    dt = get_step_size()

    if ( .not. allocated( iop_qtendxyz ) ) then
      init = .true.

      allocate( iop_qtendxyz(nx,nx,nlev,pcnst,nets:nete),stat=ierr )
      if (ierr/=0) call endrun( sub//': not able to allocate iop_qtendxyz' )
      iop_qtendxyz = 0._r8
      allocate( derivedfq(nx,nx,nlev,pcnst,nets:nete),stat=ierr )
      if (ierr/=0) call endrun( sub//': not able to allocate derivedfq' )
      derivedfq = 0._r8
      allocate( iop_qtendxyz_init(nx,nx,nlev,pcnst,nets:nete),stat=ierr )
      if (ierr/=0) call endrun( sub//': not able to allocate iop_qtendxyz' )
      iop_qtendxyz_init = 0._r8
      allocate( iop_ttendxyz(nx,nx,nlev,nets:nete),stat=ierr )
      if (ierr/=0) call endrun( sub//': not able to allocate iop_ttendxyz' )
      iop_ttendxyz = 0._r8
      allocate( iop_ttendxyz_init(nx,nx,nlev,nets:nete),stat=ierr )
      if (ierr/=0) call endrun( sub//': not able to allocate iop_ttendxyz_init' )
      iop_ttendxyz_init = 0._r8
    end if

    ! save initial/calc tendencies on second call to this routine.
    if (use_cslam) then
      do ie=nets,nete
        do ic=1,pcnst
          iop_qtendxyz(:,:,:,ic,ie) = fvm(ie)%c(1:nc,1:nc,:,ic) - iop_qtendxyz(:,:,:,ic,ie)
        end do
      end do
    else
      do ie=nets,nete
        do ic=1,pcnst
          iop_qtendxyz(:,:,:,ic,ie) = elem(ie)%state%Qdp(:,:,:,ic,qn0)/elem(ie)%state%dp3d(:,:,:,n0)  - iop_qtendxyz(:,:,:,ic,ie)
       end do
      end do
    end if
    do ie=nets,nete
       iop_ttendxyz(:,:,:,ie) = elem(ie)%state%T(:,:,:,n0)  - iop_ttendxyz(:,:,:,ie)
    end do

    if (init) then
       do ie=nets,nete
          iop_ttendxyz_init(:,:,:,ie)   = iop_ttendxyz(:,:,:,ie)
          iop_qtendxyz_init(:,:,:,:,ie) = iop_qtendxyz(:,:,:,:,ie)
          derivedfq(:,:,:,:,ie)=elem(ie)%derived%FQ(:,:,:,:)/dt
       end do
    end if

    if ( .not. init ) then
      allocate( q_adv(nxsq,nlev,pcnst),stat=ierr )
      if (ierr/=0) call endrun( sub//': not able to allocate q_adv' )
      q_adv = 0._r8
      allocate( t_adv(npsq,nlev),stat=ierr )
      if (ierr/=0) call endrun( sub//': not able to allocate t_adv' )
      t_adv = 0._r8
      allocate( q_new(nx,nx,nlev),stat=ierr )
      if (ierr/=0) call endrun( sub//': not able to allocate q_new' )
      q_new = 0._r8
      allocate( out_q(npsq,nlev),stat=ierr )
      if (ierr/=0) call endrun( sub//': not able to allocate out_q' )
      out_q = 0._r8
      allocate( out_t(npsq,nlev),stat=ierr )
      if (ierr/=0) call endrun( sub//': not able to allocate out_t' )
      out_t = 0._r8
      allocate( out_u(npsq,nlev),stat=ierr )
      if (ierr/=0) call endrun( sub//': not able to allocate out_u' )
      out_u = 0._r8
      allocate( out_v(npsq,nlev),stat=ierr )
      if (ierr/=0) call endrun( sub//': not able to allocate out_v' )
      out_v = 0._r8
      allocate( out_ps(npsq),stat=ierr )
      if (ierr/=0) call endrun( sub//': not able to allocate out_ps' )
      out_ps = 0._r8
      do ie=nets,nete
         do j=1,nx
            do i=1,nx
               t_adv(i+(j-1)*np,:) = iop_ttendxyz(i,j,:,ie)/dt - elem(ie)%derived%FT(i,j,:)
               out_u(i+(j-1)*np,:) = elem(ie)%state%v(i,j,1,:,n0)
               out_v(i+(j-1)*np,:) = elem(ie)%state%v(i,j,2,:,n0)
               out_ps(i+(j-1)*np) = elem(ie)%state%psdry(i,j)

               ! to retain bfb, replace state q and t with roundoff version calculated using the ordering and tendencies of the
               ! scam prognostic equation
               elem(ie)%state%T(i,j,:,n0) =  iop_ttendxyz_init(i,j,:,ie) + dt*(elem(ie)%derived%FT(i,j,:) + t_adv(i+(j-1)*np,:))
               out_t(i+(j-1)*np,:) = elem(ie)%state%T(i,j,:,n0)
               do p=1,pcnst
                  q_adv(i+(j-1)*nx,:,p) = iop_qtendxyz(i,j,:,p,ie)/dt - derivedfq(i,j,:,p,ie)
                  q_new(i,j,:) = iop_qtendxyz_init(i,j,:,p,ie) + dt*(derivedfq(i,j,:,p,ie) + q_adv(i+(j-1)*nx,:,p))
                  if (use_cslam) then
                     fvm(ie)%c(i,j,:,p)=q_new(i,j,:)
                  else
                     elem(ie)%state%Qdp(i,j,:,p,qn0)=q_new(i,j,:)*elem(ie)%state%dp3d(i,j,:,n0)
                  end if
               end do
               out_q(i+(j-1)*nx,:) = elem(ie)%state%Qdp(i,j,:,1,qn0)/elem(ie)%state%dp3d(i,j,:,n0)
            end do
         end do
         call outfld('Ps',out_ps,npsq,ie)
         call outfld('t',out_t,npsq,ie)
         call outfld('q',out_q,nxsq,ie)
         call outfld('u',out_u,npsq,ie)
         call outfld('v',out_v,npsq,ie)
         call outfld('divT3d',t_adv,npsq,ie)
         do p=1,pcnst
            call outfld(trim(cnst_name(p))//'_dten',q_adv(:,:,p),nxsq,ie)
         end do
      end do

      deallocate(iop_ttendxyz)
      deallocate(iop_ttendxyz_init)
      deallocate(iop_qtendxyz)
      deallocate(iop_qtendxyz_init)
      deallocate(derivedfq)
      deallocate(out_t)
      deallocate(out_q)
      deallocate(out_u)
      deallocate(out_v)
      deallocate(out_ps)
      deallocate(t_adv)
      deallocate(q_adv)
      deallocate(q_new)

    end if
  end subroutine compute_write_iop_fields
#endif
end module advect_tend
