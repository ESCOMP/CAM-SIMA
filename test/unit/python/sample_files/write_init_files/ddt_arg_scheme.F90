! Test scheme taking a host DDT variable as an argument; used to verify
! that DDT-typed suite variables are excluded from generated init code.
module ddt_arg_scheme

  implicit none
  private

  public :: ddt_arg_scheme_run

contains

  !> \section arg_table_ddt_arg_scheme_run  Argument Table
  !! \htmlinclude arg_table_ddt_arg_scheme_run.html
  !!
  subroutine ddt_arg_scheme_run(test_list, errmsg, errflg)

    use ddt_arg_host_mod, only: test_list_t

    type(test_list_t),  intent(in)  :: test_list(:)
    character(len=512), intent(out) :: errmsg
    integer,            intent(out) :: errflg

    errmsg = ''
    errflg = 0

  end subroutine ddt_arg_scheme_run

end module ddt_arg_scheme
