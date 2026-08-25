!simple demonstration parameterization which uses
!the tendency set by temp_adjust
!

MODULE tend_apply

  USE ccpp_kinds, ONLY: kind_phys

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: tend_apply_init
  PUBLIC :: tend_apply_run

CONTAINS

!> \section arg_table_tend_apply_init  Argument Table
!! \htmlinclude arg_table_tend_apply_init.html
!!
  subroutine tend_apply_init(eddy_len, errmsg, errflg)

    real(kind_phys),    intent(out)   :: eddy_len(:)
    character(len=512), intent(out)   :: errmsg
    integer,            intent(out)   :: errflg

    errmsg = ''
    errflg = 0

    eddy_len = 0._kind_phys

  end subroutine tend_apply_init

!> \section arg_table_tend_apply_run  Argument Table
!! \htmlinclude arg_table_tend_apply_run.html
!!
  subroutine tend_apply_run(nbox, ptend, eddy_len, errmsg, errflg)

    integer,            intent(in)    :: nbox
    real(kind_phys),    intent(in)    :: ptend(:)
    real(kind_phys),    intent(in)    :: eddy_len(:)
    character(len=512), intent(out)   :: errmsg
    integer,            intent(out)   :: errflg

    ! This routine currently does nothing with its inputs

    errmsg = ''
    errflg = 0

  end subroutine tend_apply_run

END MODULE tend_apply
