module find_max_nonzero_index

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                     FIND_MAX_NONZERO_INDEX                           !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  - The purpose of this module is to compare a variable at a specific
!     index between CAM and CAM-SIMA.
!
!  SUBROUTINES:
!    - find_index_and_rank: finds the largest non-zero value in a 
!       given array and saves the index(es) and rank of the value
!       as module-level variables
!    - set_index_and_rank: manually sets the module-level index(es)
!       variables
!    - print_value: prints info and the value of a variable at the
!       indexes corresponding to what was found in find_index_and_rank
!
!  USAGE:
!    1. Copy this module into the CAM source tree
!    2. Configure a CAM case to have the same number of tasks as your
!        CAM-SIMA case and set -pcols=<# larger than total columns>
!    3. Add a call in the CAM code to find_index_and_rank for the
!        variable in question
!      - this will be a variable that is known to be "wrong" in CAM-SIMA
!    4. Also in the CAM code, add calls to print_value for any other
!        variables that MAY be wrong in CAM-SIMA
!    5. In CAM-SIMA, add a call to set_index_and_rank with the
!        rank and index(es) [index = -1 if not present] instead of
!        find_index_and_rank. Then add the same print_value statements
!        as you did in CAM
!    6. Run both cases and compare! Good luck.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use cam_logfile, only: iulog
   use spmd_utils,  only: iam, masterproc, mpicom, masterprocid
   use ccpp_kinds,  only: kind_phys

   implicit none
   private
   save

   public :: find_index_and_rank
   public :: set_index_and_rank
   public :: print_value

   integer, public :: horiz_index = -1
   integer, public :: vert_index = -1
   integer, public :: rank = -1
   real(kind=kind_phys), parameter :: buffer = 1.0e-20_kind_phys

   interface find_index_and_rank
      module procedure find_index_and_rank_2d
      module procedure find_index_and_rank_3d
   end interface find_index_and_rank

   interface print_value
      module procedure print_value_2d
      module procedure print_value_3d
   end interface print_value

   contains

      subroutine find_index_and_rank_3d(var)
         use mpi,            only: mpi_maxloc, mpi_status_size
         use mpi,            only: mpi_2double_precision, mpi_integer
         real(kind=kind_phys), intent(in) :: var(:,:)

         ! local variables
         integer :: col_idx, lev_idx, max_rank
         integer :: max_col, max_lev, max_col_gl, max_lev_gl
         real(kind=kind_phys) :: max_value(2), max_value_gl(2)
         integer :: mpi_stat(mpi_status_size), ierr
         character(len=64) :: fmt_str

         if (.not. any(var > buffer)) then
            if (masterproc) then
               write(iulog,*) 'CHECK_NONZERO_INDEX: No values are non-zero!'
            end if
         end if
         ! Find the largest non-zero value
         max_value = 0._kind_phys
         do lev_idx = 1, size(var,2)
            do col_idx = 1, size(var,1)
               if (var(col_idx, lev_idx) > max_value(1)) then
                  max_value(1) = var(col_idx, lev_idx)
                  max_col = col_idx
                  max_lev = lev_idx
               end if
            end do
         end do

         !MPI call to get global value:
         call mpi_allreduce(max_value, max_value_gl, 1,                    &
                            MPI_2DOUBLE_PRECISION,                         &
                            mpi_maxloc, mpicom, ierr)
         if (iam == int(max_value_gl(2)) .and. .not. masterproc) then
            !The largest value happened on this task, so the local value
            !is the global value. So send the local value's dimension
            !indices (column and level index) to the root task:
            call mpi_send(max_col, 1, mpi_integer, masterprocid, 0,   &
                          mpicom, ierr)
            call mpi_send(max_lev, 1, mpi_integer, masterprocid, 0,   &
                          mpicom, ierr)
         else if (iam /= int(max_value_gl(2)) .and. masterproc) then
            !The root task needs to receive the relevant max value indices
            !from a different task:
            call mpi_recv(max_col_gl, 1, mpi_integer,              &
                          int(max_value_gl(2)), 0, mpicom,         &
                          mpi_stat, ierr)
            call mpi_recv(max_lev_gl, 1, mpi_integer,              &
                          int(max_value_gl(2)), 0, mpicom,         &
                          mpi_stat, ierr)
         else if (masterprocid == int(max_value_gl(2))) then
            !The biggest value is on the root MPI task already, so just
            !set directly:
            max_col_gl = max_col
            max_lev_gl = max_lev
         end if
         horiz_index = max_col_gl
         vert_index = max_lev_gl
         rank = int(max_value_gl(2))
         fmt_str = "(1x,a,t50,1x,a,1x,a,1x,a,1x,a)"
         if (iam == rank) then
            write(iulog,*) '********NON-ZERO INDEX CHECK********'
            write(iulog,fmt_str) 'VARIABLE', ' VALUE  ', 'COLUMN ', ' LEVEL ', '  RANK ' 
            write(iulog,fmt_str) '--------', '--------', '-------', '-------', '-------'
         end if

      end subroutine find_index_and_rank_3d

      subroutine find_index_and_rank_2d(var, vertical_index)
         use mpi,            only: mpi_maxloc, mpi_status_size
         use mpi,            only: mpi_2double_precision, mpi_integer
         real(kind=kind_phys), intent(in) :: var(:)
         logical, optional,    intent(in) :: vertical_index
         ! local variables
         integer :: col_idx, max_rank
         integer :: max_col, max_col_gl
         real(kind=kind_phys) :: max_value(2), max_value_gl(2)
         integer :: mpi_stat(mpi_status_size), ierr
         logical :: vert_index_loc
         character(len=64) :: fmt_str

         if (present(vertical_index)) then
            vert_index_loc = vertical_index
         else
            vert_index_loc = .false.
         end if

         if (.not. any(var > buffer)) then
            if (masterproc) then
               write(iulog,*) 'CHECK_NONZERO_INDEX: No values are non-zero!'
            end if
         end if
         ! Find the largest non-zero value
         max_value = 0._kind_phys
         do col_idx = 1, size(var,1)
            if (var(col_idx) > max_value(1)) then
               max_value(1) = var(col_idx)
               max_col = col_idx
            end if
         end do

         !MPI call to get global value:
         call mpi_allreduce(max_value, max_value_gl, 1,                    &
                            MPI_2DOUBLE_PRECISION,                         &
                            mpi_maxloc, mpicom, ierr)
         if (iam == int(max_value_gl(2)) .and. .not. masterproc) then
            !The largest value happened on this task, so the local value
            !is the global value. So send the local value's dimension
            !index to the root task:
            call mpi_send(max_col, 1, mpi_integer, masterprocid, 0,   &
                          mpicom, ierr)
         else if (iam /= int(max_value_gl(2)) .and. masterproc) then
            !The root task needs to receive the relevant max value indices
            !from a different task:
            call mpi_recv(max_col_gl, 1, mpi_integer,              &
                          int(max_value_gl(2)), 0, mpicom,         &
                          mpi_stat, ierr)
         else if (masterprocid == int(max_value_gl(2))) then
            !The biggest value is on the root MPI task already, so just
            !set directly:
            max_col_gl = max_col
         end if
         if (vert_index_loc) then
            vert_index = max_col_gl
         else
            horiz_index = max_col_gl
         end if
         fmt_str = "(1x,a,t50,1x,a,1x,a,1x,a,1x,a)"
         if (iam == rank) then
            write(iulog,*) '********NON-ZERO INDEX CHECK********'
            write(iulog,fmt_str) 'VARIABLE', ' VALUE  ', 'COLUMN ', ' LEVEL ', '  RANK ' 
            write(iulog,fmt_str) '--------', '--------', '-------', '-------', '-------'
         end if

      end subroutine find_index_and_rank_2d

      subroutine set_index_and_rank(rank_in, column_index, layer_index)
         integer, intent(in) :: rank_in
         integer, intent(in) :: column_index
         integer, intent(in) :: layer_index

         character(len=64) :: fmt_str

         rank = rank_in
         horiz_index = column_index
         vert_index = layer_index
         fmt_str = "(1x,a,t50,1x,a,1x,a,1x,a,1x,a)"
         if (iam == rank) then
            write(iulog,*) '********NON-ZERO INDEX CHECK********'
            write(iulog,fmt_str) 'VARIABLE', ' VALUE  ', 'COLUMN ', ' LEVEL ', '  RANK ' 
            write(iulog,fmt_str) '--------', '--------', '-------', '-------', '-------'
         end if

      end subroutine set_index_and_rank

      subroutine print_value_3d(var, context)
         real(kind=kind_phys), intent(in) :: var(:,:)
         character(len=*), intent(in) :: context

         character(len=64) :: fmt_str, fmt_str_data

         if (rank < 0) then
            return
         end if

         if (horiz_index < 0 .or. vert_index < 0) then
            write(iulog,*) 'PRINT_VALUE_3D - cannot print value for ', context
            return
         end if
         fmt_str = "(1x,a,t50,1x,a,1x,a,1x,a,1x,a)"
         fmt_str_data = "(1x,a,t50,1x,e8.2,1x,i7,1x,i7,1x,i7)" 

         if (iam == rank) then
            !write(iulog,*) '********NON-ZERO INDEX CHECK********'
            !write(iulog,fmt_str) 'VARIABLE', ' VALUE ', 'COLUMN ', ' LEVEL ', '  RANK ' 
            !write(iulog,fmt_str) '--------', '-------', '-------', '-------', '-------'
            write(iulog,fmt_str_data) context, var(horiz_index, vert_index), &
                   horiz_index, vert_index, rank
         end if

      end subroutine print_value_3d

      subroutine print_value_2d(var, context, vertical_index)
         real(kind=kind_phys), intent(in) :: var(:)
         character(len=*), intent(in) :: context
         logical, optional, intent(in) :: vertical_index

         logical :: vert_index_loc
         character(len=64) :: fmt_str, fmt_str_data

         if (rank < 0) then
            return
         end if
         if (present(vertical_index)) then
            vert_index_loc = vertical_index
         else
            vert_index_loc = .false.
         end if
         fmt_str = "(1x,a,t50,1x,a,1x,a,1x,a,1x,a)"

         if (iam == rank) then
            !write(iulog,*) '********NON-ZERO INDEX CHECK********'
            !write(iulog,fmt_str) 'VARIABLE', ' VALUE ', 'COLUMN ', ' LEVEL ', '  RANK ' 
            !write(iulog,fmt_str) '--------', '-------', '-------', '-------', '-------'
            if (vert_index_loc) then
               if (vert_index < 0) then
                  write(iulog,*) 'ERROR - variable ', context, ' does not have same dimensions'
               else
                  fmt_str_data= "(1x,a,t50,1x,e8.2,1x,a,1x,i7,1x,i7)" 
                  write(iulog,fmt_str_data) context, var(vert_index), '    N/A', &
                      vert_index, rank
               end if
            else
               if (horiz_index < 0) then
                  write(iulog,*) 'ERROR - variable ', context, ' does not have same dimensions'
               else
                  fmt_str_data= "(1x,a,t50,1x,e8.2,1x,i7,1x,a,1x,i7)" 
                  write(iulog,fmt_str_data) context, var(horiz_index), horiz_index, &
                      '    N/A', rank
               end if
            end if
         end if

      end subroutine print_value_2d

end module find_max_nonzero_index
