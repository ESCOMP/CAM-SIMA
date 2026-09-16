!-----------------------------------------------------------------------
! Mock physics_grid for unit testing.
! Provides a settable columns_on_task (the real module computes it from
! the decomposition; tests set it directly).
!-----------------------------------------------------------------------
module physics_grid

  implicit none
  private

  integer, public :: columns_on_task = 4

end module physics_grid
