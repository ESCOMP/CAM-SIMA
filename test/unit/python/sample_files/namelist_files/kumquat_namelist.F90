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
!! @brief Auto-generated Module to read namelist variables for kumquat
!!
!
module kumquat_namelist

   use ccpp_kinds,  only: kind_phys
   use runtime_obj, only: unset_str, unset_int

   implicit none
   private

   public :: autogen_kumquat_readnl

   !> \section arg_table_kumquat_namelist  Argument Table
   !! \htmlinclude kumquat_namelist.html
   integer, public, protected :: pgwv = unset_int
   real(kind_phys), public, protected :: kq_dc = -HUGE(1.0_kind_phys)
   logical, public, protected :: tau_0_ubc = .false.
   character(len=256), public, protected :: bnd_rdggm = unset_str
   integer, public, parameter :: kq_farr1_dimension = 2
   real(kind_phys), public, protected :: kq_farr1(2) = -HUGE(1.0_kind_phys)
   integer, public, parameter :: kq_fake2_dimension = 3
   integer, public, parameter :: kq_fake2_dimension_2 = 2
   integer, public, protected :: kq_fake2(3, 2) = unset_int
   integer, public, parameter :: kq_fchar3_dimension = 7
   character(len=256), public, protected :: kq_fchar3(7) = unset_str

CONTAINS

   subroutine autogen_kumquat_readnl(nl_unit, mpicomm, mpiroot, mpi_isroot, logunit)
      use mpi,            only: MPI_Character, MPI_Integer, MPI_Logical, MPI_Real8
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
      character(len=*), parameter :: subname = 'autogen_kumquat_readnl'

      namelist /kumquat_nl/ pgwv, kq_dc, tau_0_ubc, bnd_rdggm, kq_farr1, kq_fake2, kq_fchar3

      ! Read the namelist on the root task
      if (mpi_isroot) then
         rewind(nl_unit)
         call shr_nl_find_group_name(nl_unit, 'kumquat_nl', status=ierr)
         if (ierr == 0) then
            read(nl_unit, kumquat_nl, iostat=ierr)
            if (ierr /= 0) then
               call endrun(subname//':: ERROR reading namelist, kumquat_nl')
            end if
         else
            call endrun(subname//':: ERROR: Did not find namelist group, kumquat_nl.')
         end if
         ! Print out namelist values
         write(logunit, *) 'Namelist values from kumquat_nl for kumquat'
         write(logunit, *) 'pgwv = ', pgwv
         write(logunit, *) 'kq_dc = ', kq_dc
         write(logunit, *) 'tau_0_ubc = ', tau_0_ubc
         write(logunit, *) 'bnd_rdggm = ', bnd_rdggm
         write(logunit, *) 'kq_farr1 = ', kq_farr1
         write(logunit, *) 'kq_fake2 = ', kq_fake2
         write(logunit, *) 'kq_fchar3 = ', kq_fchar3
      end if
      ! Broadcast the namelist variables
      call mpi_bcast(pgwv, 1, MPI_Integer, mpiroot, mpicomm, ierr)
      call mpi_bcast(kq_dc, 1, MPI_Real8, mpiroot, mpicomm, ierr)
      call mpi_bcast(tau_0_ubc, 1, MPI_Logical, mpiroot, mpicomm, ierr)
      call mpi_bcast(bnd_rdggm, len(bnd_rdggm), MPI_Character, mpiroot, mpicomm, ierr)
      call mpi_bcast(kq_farr1, 2, MPI_Real8, mpiroot, mpicomm, ierr)
      call mpi_bcast(kq_fake2, 6, MPI_Integer, mpiroot, mpicomm, ierr)
      call mpi_bcast(kq_fchar3, len(kq_fchar3(1))*7, MPI_Character, mpiroot, mpicomm, ierr)

   end subroutine autogen_kumquat_readnl

end module kumquat_namelist
