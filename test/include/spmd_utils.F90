module spmd_utils

   implicit none
   private

   integer, parameter, public :: masterprocid = 0
   integer, parameter, public :: iam = 0
   integer, parameter, public :: npes = 1
   logical, parameter, public :: masterproc = .true.

end module spmd_utils
