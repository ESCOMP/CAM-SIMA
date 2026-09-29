!-----------------------------------------------------------------------
! Mock vert_coord for unit testing.
! Provides pver = 30 (CAM5 default vertical levels).
!-----------------------------------------------------------------------
module vert_coord

  implicit none

  integer, parameter, public :: pver  = 30
  integer, parameter, public :: pverp = 31

end module vert_coord
