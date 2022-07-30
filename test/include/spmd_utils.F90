module spmd_utils

   use mpi, only: MPI_COMM_WORLD

   implicit none
   private

   integer, parameter, public :: mpicom = MPI_COMM_WORLD
   integer, parameter, public :: iam = 0
   integer, parameter, public :: npes = 1
   logical, parameter, public :: masterproc = .true.
   integer, parameter, public :: masterprocid = 0

end module spmd_utils
