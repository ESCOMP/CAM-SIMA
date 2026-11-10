!-----------------------------------------------------------------------
! Mock cam_logfile for unit testing.
! Provides iulog pointing to stdout (unit 6).
!-----------------------------------------------------------------------
module cam_logfile

  implicit none

  integer, public :: iulog = 6

end module cam_logfile
