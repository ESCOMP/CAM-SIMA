! Host module providing an opaque DDT and a module-level variable of that
! type, used to test that DDT-typed suite variables are excluded from
! generated initialization code.
module ddt_arg_host_mod

  implicit none
  public

  !> \section arg_table_test_list_t  Argument Table
  !! \htmlinclude arg_table_test_list_t.html
  !!
  type test_list_t
    integer :: num_items = 0
  end type test_list_t

  !> \section arg_table_ddt_arg_host_mod  Argument Table
  !! \htmlinclude arg_table_ddt_arg_host_mod.html
  !!
  type(test_list_t), allocatable :: test_list(:)

end module ddt_arg_host_mod
