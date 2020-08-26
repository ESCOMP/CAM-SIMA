module physics_column_type

   use shr_kind_mod, only: r8 => shr_kind_r8
!   use ISO_FORTRAN_ENV, only: kind_phys
   use ccpp_kinds,   only: kind_phys
 

   implicit none
   private
   save

!> \section arg_table_physics_column_t  Argument Table
!! \htmlinclude physics_column_t.html
type, public :: physics_column_t
   ! A type to hold all grid and task information for a single physics column
   ! Column information
   real(kind_phys)             :: lat_rad = -HUGE(1.0_r8) ! Latitude in radians
   real(kind_phys)             :: lon_rad = -HUGE(1.0_r8) ! Longitude in radians
   real(kind_phys)             :: lat_deg = -HUGE(1.0_r8) ! Latitude in degrees
   real(kind_phys)             :: lon_deg = -HUGE(1.0_r8) ! Longitude in degrees
   real(kind_phys)             :: area = -1.0_r8          ! Column area
   real(kind_phys)             :: weight = -1.0_r8        ! Column integration weight
   ! File decomposition
   integer              :: global_col_num = -1     ! Location on data file
   integer              :: coord_indices(2) = -1   ! Global lon/lat (if used)
   ! Dynamics decomposition
   integer              :: dyn_task = -1           ! Dynamics MPI task
   integer              :: local_dyn_block = -1    ! Block number for this task
   integer              :: global_dyn_block = -1   ! Global dyn block number
   !    If there is more than one block index, they are in the same order
   !    as in the dynamics block structure
   integer, allocatable :: dyn_block_index(:)      ! Index(cies) into block
   ! Physics decomposition
   integer              :: phys_task = -1          ! Physics MPI task
   integer              :: local_phys_chunk = -1   ! Local phys 'block' number
   integer              :: phys_chunk_index = -1   ! Index into physics chunk
end type physics_column_t


!==============================================================================
CONTAINS
!==============================================================================

end module physics_column_type
