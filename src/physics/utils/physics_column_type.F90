module physics_column_type

   use shr_kind_mod, only: r8 => shr_kind_r8
   use ccpp_kinds,   only: kind_pcol => kind_phys

   implicit none
   private
   save

   public :: assignment ( = )

   interface assignment ( = )
      module procedure copy_phys_col
   end interface

   !Physics column kind:
   public kind_pcol

   !physics column fill value:
   real(kind_pcol), parameter :: pcol_fill_val = -1e36_kind_pcol

!> \section arg_table_physics_column_t  Argument Table
!! \htmlinclude physics_column_t.html
type, public :: physics_column_t
   ! A type to hold all grid and task information for a single physics column
   ! Column information
   real(kind_pcol)             :: lat_rad = pcol_fill_val ! Latitude in radians
   real(kind_pcol)             :: lon_rad = pcol_fill_val ! Longitude in radians
   real(kind_pcol)             :: lat_deg = pcol_fill_val ! Latitude in degrees
   real(kind_pcol)             :: lon_deg = pcol_fill_val ! Longitude in degrees
   real(kind_pcol)             :: area    = pcol_fill_val ! Column area
   real(kind_pcol)             :: weight  = pcol_fill_val ! Column integration weight
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


   subroutine copy_phys_col(phys_col_out, phys_col_in)

      use cam_abortutils, only: endrun
      use string_utils,   only: to_str

      ! Copy all of the values from one physics_column type
      ! structure to another.

      ! Dummy (input) variables:
      type(physics_column_t), intent(out) :: phys_col_out
      type(physics_column_t), intent(in)  :: phys_col_in

      ! Local variables:
      integer :: istat

      character(len=*),  parameter :: subname = 'copy_phys_col'

      ! Copy values from input array to output array:

      ! Column information
      phys_col_out%lat_rad = phys_col_in%lat_rad
      phys_col_out%lon_rad = phys_col_in%lon_rad
      phys_col_out%lat_deg = phys_col_in%lat_deg
      phys_col_out%lon_deg = phys_col_in%lon_deg
      phys_col_out%area    = phys_col_in%area
      phys_col_out%weight  = phys_col_in%weight

      ! File decomposition
      phys_col_out%global_col_num   = phys_col_in%global_col_num
      phys_col_out%coord_indices(:) = phys_col_in%coord_indices(:)

      ! Dynamics decomposition
      phys_col_out%dyn_task         = phys_col_in%dyn_task
      phys_col_out%local_dyn_block  = phys_col_in%local_dyn_block
      phys_col_out%global_dyn_block = phys_col_in%global_dyn_block

      ! Dynamics blocks
      if (allocated(phys_col_in%dyn_block_index)) then
         ! De-allocate output block indices if allocated to incorrect size:
         if (allocated(phys_col_out%dyn_block_index)) then
            if (size(phys_col_out%dyn_block_index) /= &
               size(phys_col_in%dyn_block_index)) then
               deallocate(phys_col_out%dyn_block_index)
            end if
         end if

         ! If necessary, allocate output to match size of input:
         if (.not. allocated(phys_col_out%dyn_block_index)) then
            allocate(phys_col_out%dyn_block_index(size(phys_col_in%dyn_block_index)), &
                     stat=istat)
            if (istat /= 0) then
               call endrun(subname//': allocate phys_col_out%dyn_block_index failed '//&
                           'with stat: '//to_str(istat))
            end if
         end if

         phys_col_out%dyn_block_index(:) = phys_col_in%dyn_block_index(:)
      else if (allocated(phys_col_out%dyn_block_index)) then
         ! De-allocate output array if input array has not been allocated:
         deallocate(phys_col_out%dyn_block_index)
      end if

      ! Physics decomposition
      phys_col_out%phys_task        = phys_col_in%phys_task
      phys_col_out%local_phys_chunk = phys_col_in%local_phys_chunk
      phys_col_out%phys_chunk_index = phys_col_in%phys_chunk_index

   end subroutine copy_phys_col

end module physics_column_type
