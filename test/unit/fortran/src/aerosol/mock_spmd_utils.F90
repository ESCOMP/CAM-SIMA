!-----------------------------------------------------------------------
! Mock spmd_utils for unit testing.
! Provides masterproc = .true. (single-process test environment).
!-----------------------------------------------------------------------
module spmd_utils

  implicit none

  logical, public :: masterproc = .true.
  integer, public :: mpicom = 0

end module spmd_utils
