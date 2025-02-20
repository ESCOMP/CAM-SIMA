module ref_two

  use theta_ddt, only: theta_ddt_type

  implicit none
  private

!> \section arg_table_ref_two  Argument Table
!! \htmlinclude ref_two.html
  type(theta_ddt_type), public, pointer :: thost_thermo_vars_obj(:, :) => NULL()

end module ref_two
