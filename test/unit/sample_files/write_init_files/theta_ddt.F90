module theta_ddt

   use ccpp_kinds, only: kind_phys

   implicit none
   private
   save

type, public :: theta_ddt_type
  real(kind_phys) :: theta(:, :)   = -HUGE(1.0_r8)
  real(kind_phys) :: theta_e(:, :) = -HUGE(1.0_r8)
end type theta_ddt_type

!==============================================================================
CONTAINS
!==============================================================================

end module theta_ddt
