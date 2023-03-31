MODULE shr_mem_mod

  use shr_kind_mod, only : shr_kind_r8
  use shr_log_mod, only: s_logunit => shr_log_Unit
  use shr_sys_mod, only: shr_sys_abort

  implicit none
  private

  ! PUBLIC: Public interfaces

  public ::  shr_mem_getusage, &
       shr_mem_init

  ! PUBLIC: Public interfaces

  real(shr_kind_r8) :: mb_blk = 0.0_shr_kind_r8

  !===============================================================================
CONTAINS
  !===============================================================================

  subroutine shr_mem_init(prt, strbuf)

    implicit none

    !----- arguments -----

    logical, optional :: prt
    character(len=*), optional :: strbuf
    !----- local -----

    ! --- Memory stats ---
    integer :: msize                   ! memory size (high water)
    integer :: mrss0,mrss1,mrss2       ! temporary rss
    integer :: mshare,mtext,mdatastack
    logical :: lprt
    integer :: ierr

    integer :: GPTLget_memusage

    real(shr_kind_r8),allocatable :: mem_tmp(:)

    character(*),parameter :: subname = "(shr_mem_init)"
    !---------------------------------------------------

  end subroutine shr_mem_init

  !===============================================================================

  subroutine shr_mem_getusage(r_msize,r_mrss,prt)

    implicit none

    !----- arguments ---
    real(shr_kind_r8) :: r_msize,r_mrss
    logical, optional :: prt

  end subroutine shr_mem_getusage

  !===============================================================================

END MODULE shr_mem_mod
