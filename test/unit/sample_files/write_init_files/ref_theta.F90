module ref_theta

  use ccpp_kinds, only: kind_phys
!  use physics_types_mf, only: ncol
!  use physics_types_mf, only: pver

  implicit none
  private

!> \section arg_table_ref_theta  Argument Table
!! \htmlinclude ref_theta.html
  real(kind_phys), public, allocatable :: theta(:, :)

end module ref_theta
