module ic_held_suarez

  !-----------------------------------------------------------------------
  !
  ! Purpose: Set Held-Suarez initial conditions based on input coordinates
  !
  !
  !-----------------------------------------------------------------------
  use cam_logfile,         only: iulog
  use shr_kind_mod,        only: r8 => shr_kind_r8
  use cam_abortutils,      only: endrun, check_allocate
  use spmd_utils,          only: masterproc
  use shr_sys_mod,         only: shr_sys_flush

  implicit none
  private

  ! Public interface
  public :: hs94_set_ic

!==============================================================================
CONTAINS
!==============================================================================

  subroutine hs94_set_ic(latvals, lonvals, U, V, T, PS, PHIS,           &
       Q, m_cnst, mask, verbose)
    use shr_kind_mod,              only: cx => shr_kind_cx
    use ccpp_kinds,                only: kind_phys
    use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t
    use cam_ccpp_cap,              only: cam_model_const_properties
    use cam_constituents,          only: const_get_index, const_qmin
    use runtime_obj,               only: wv_stdname

    !-----------------------------------------------------------------------
    !
    ! Purpose: Set Held-Suarez initial values for dynamics state variables
    !
    !-----------------------------------------------------------------------

    ! Dummy arguments
    real(r8),           intent(in)    :: latvals(:) ! lat in degrees (ncol)
    real(r8),           intent(in)    :: lonvals(:) ! lon in degrees (ncol)
    real(r8), optional, intent(inout) :: U(:,:)     ! zonal velocity
    real(r8), optional, intent(inout) :: V(:,:)     ! meridional velocity
    real(r8), optional, intent(inout) :: T(:,:)     ! temperature
    real(r8), optional, intent(inout) :: PS(:)      ! surface pressure
    real(r8), optional, intent(out)   :: PHIS(:)    ! surface geopotential
    real(r8), optional, intent(inout) :: Q(:,:,:)   ! tracer (ncol, lev, m)
    integer,  optional, intent(in)    :: m_cnst(:)  ! tracer indices (reqd. if Q)
    logical,  optional, intent(in)    :: mask(:)    ! Only init where .true.
    logical,  optional, intent(in)    :: verbose    ! For internal use

    ! Local variables
    logical, allocatable              :: mask_use(:)
    logical                           :: verbose_use
    logical                           :: const_has_default
    integer                           :: i, k, m
    integer                           :: ix_q
    integer                           :: ncol
    integer                           :: nlev
    integer                           :: ncnst
    integer                           :: iret
    real(kind_phys)                   :: const_default_value !Constituent default value
    real(kind_phys)                   :: const_qmin_value    !Constituent minimum value
    character(len=cx)                 :: errmsg              !CCPP error message
    character(len=*), parameter       :: subname = 'HS94_SET_IC'

    !Private array of constituent properties (for property interface functions)
    type(ccpp_constituent_prop_ptr_t), pointer :: const_props(:)

    allocate(mask_use(size(latvals)), stat=iret)
    call check_allocate(iret, subname, 'mask_use(size(latvals))', &
                        file=__FILE__, line=__LINE__)

    if (present(mask)) then
      if (size(mask_use) /= size(mask)) then
        call endrun('cnst_init_default: input, mask, is wrong size')
      end if
      mask_use = mask
    else
      mask_use = .true.
    end if

    if (present(verbose)) then
      verbose_use = verbose
    else
      verbose_use = .true.
    end if

    ncol = size(latvals, 1)
    nlev = -1
    if (present(U)) then
      nlev = size(U, 2)
      do k = 1, nlev
        where(mask_use)
          U(:,k) = 0.0_r8
        end where
      end do
      if(masterproc .and. verbose_use) then
        write(iulog,*) '          U initialized by "',subname,'"'
      end if
    end if

    if (present(V)) then
      nlev = size(V, 2)
      do k = 1, nlev
        where(mask_use)
          V(:,k) = 0.0_r8
        end where
      end do
      if(masterproc .and. verbose_use) then
        write(iulog,*) '          V initialized by "',subname,'"'
      end if
    end if

    if (present(T)) then
      nlev = size(T, 2)
      do k = 1, nlev
        where(mask_use)
          T(:,k) = 250.0_r8
        end where
      end do
      if(masterproc .and. verbose_use) then
        write(iulog,*) '          T initialized by "',subname,'"'
      end if
    end if

    if (present(PS)) then
      where(mask_use)
        PS = 100000.0_r8
      end where
      if(masterproc .and. verbose_use) then
        write(iulog,*) '          PS initialized by "',subname,'"'
      end if
    end if

    if (present(PHIS)) then
      PHIS = 0.0_r8
      if(masterproc .and. verbose_use) then
        write(iulog,*) '          PHIS initialized by "',subname,'"'
      end if
    end if

    if (present(Q)) then
      !Get water vapor constituent index:
      call const_get_index(wv_stdname, ix_q)

      !Get constituent properties object:
      const_props => cam_model_const_properties()

      !Determine array sizes:
      nlev = size(Q, 2)
      ncnst = size(m_cnst, 1)

      !Loop over all constituents:
      do m = 1, ncnst
        if (m_cnst(m) == ix_q) then
          ! No water vapor in Held-Suarez
          do k = 1, nlev
            where(mask_use)
              Q(:,k,m) = 0.0_r8
            end where
          end do

          if(masterproc .and. verbose_use) then
            write(iulog,*) '          ', wv_stdname, ' initialized by "',subname,'"'
          end if

        else
          !Extract constituent minimum value:
          const_qmin_value = const_qmin(m_cnst(m))

          !Initialize constituent to its minimum value:
          Q(:,:,m) = real(const_qmin_value, r8)

          !Check for default value in constituent properties object:
          call const_props(m_cnst(m))%has_default(const_has_default, &
                                                iret,      &
                                                errmsg)
          if (iret /= 0) then
            call endrun(errmsg, file=__FILE__, line=__LINE__)
          end if

          if (const_has_default) then

            !If default value exists, then extract default value
            !from constituent properties object:
            call const_props(m_cnst(m))%default_value(const_default_value, &
                                                      iret,        &
                                                      errmsg)
            if (iret /= 0) then
              call endrun(errmsg, file=__FILE__, line=__LINE__)
            end if

            !Set constituent to default value in masked region:
            do k=1,nlev
              where(mask_use)
                Q(:,k,m) = real(const_default_value, r8)
              end where
            end do

          end if !has_default
        end if !water vapor
      end do
    end if

    deallocate(mask_use)

  end subroutine hs94_set_ic

end module ic_held_suarez
