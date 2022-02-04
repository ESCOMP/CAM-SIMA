!
! This work (Common Community Physics Package Framework), identified by
! NOAA, NCAR, CU/CIRES, is free of known copyright restrictions and is
! placed in the public domain.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
! THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
! IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
! CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


!>
!! @brief Auto-generated Module to read namelist variables for banana
!!
!
module banana_namelist

   use ccpp_kinds,  only: kind_phys
   use runtime_obj, only: unset_str, unset_int

   implicit none
   private

   public :: autogen_banana_readnl

   !> \section arg_table_banana_namelist  Argument Table
   !! \htmlinclude banana_namelist.html
   integer, public, protected :: rayk0 = unset_int
   real(kind_phys), public, protected :: raykrange = -HUGE(1.0_kind_phys)
   real(kind_phys), public, protected :: raytau0 = -HUGE(1.0_kind_phys)

CONTAINS

   subroutine autogen_banana_readnl(nl_unit, mpicomm, mpiroot, mpi_isroot, logunit)
      use mpi,            only: MPI_Integer, MPI_Real8
      use shr_nl_mod,     only: shr_nl_find_group_name
      use cam_abortutils, only: endrun

      ! Dummy arguments
      integer, intent(in) :: nl_unit
      integer, intent(in) :: mpicomm
      integer, intent(in) :: mpiroot
      logical, intent(in) :: mpi_isroot
      integer, intent(in) :: logunit

      ! Local variables
      integer                     :: ierr
      character(len=*), parameter :: subname = 'autogen_banana_readnl'

      namelist /banana_nl/ rayk0, raykrange, raytau0

      ! Read the namelist on the root task
      if (mpi_isroot) then
         rewind(nl_unit)
         call shr_nl_find_group_name(nl_unit, 'banana_nl', status=ierr)
         if (ierr == 0) then
            read(nl_unit, banana_nl, iostat=ierr)
            if (ierr /= 0) then
               call endrun(subname//':: ERROR reading namelist, banana_nl')
            end if
         else
            call endrun(subname//':: ERROR: Did not find namelist group, banana_nl.')
         end if
         ! Print out namelist values
         write(logunit, *) 'Namelist values from banana_nl for banana'
         write(logunit, *) 'rayk0 = ', rayk0
         write(logunit, *) 'raykrange = ', raykrange
         write(logunit, *) 'raytau0 = ', raytau0
      end if
      ! Broadcast the namelist variables
      call mpi_bcast(rayk0, 1, MPI_Integer, mpiroot, mpicomm, ierr)
      call mpi_bcast(raykrange, 1, MPI_Real8, mpiroot, mpicomm, ierr)
      call mpi_bcast(raytau0, 1, MPI_Real8, mpiroot, mpicomm, ierr)

   end subroutine autogen_banana_readnl

end module banana_namelist
