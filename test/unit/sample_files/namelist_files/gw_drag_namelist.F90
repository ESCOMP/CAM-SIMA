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
!! @brief Auto-generated Module to read namelist variables for gw_drag
!!
!
module gw_drag_namelist

   use ccpp_kinds,  only: kind_phys
   use runtime_obj, only: unset_str, unset_int

   implicit none
   private

   public :: autogen_gw_drag_readnl

   !> \section arg_table_gw_drag_namelist  Argument Table
   !! \htmlinclude gw_drag_namelist.html
   integer, public, protected :: pgwv = unset_int
   real(kind_phys), public, protected :: gw_dc = -HUGE(1.0_kind_phys)
   logical, public, protected :: tau_0_ubc = .false.
   character(len=256), public, protected :: bnd_rdggm = unset_str
   integer, parameter   :: cam_nl_autogen1_dimension = 2
   real(kind_phys), public, protected :: gw_farr1(2) = -HUGE(1.0_kind_phys)
   integer, parameter   :: cam_nl_autogen2_dimension = 3
   integer, parameter   :: cam_nl_autogen3_dimension = 2
   integer, public, protected :: gw_fake2(3, 2) = unset_int
   integer, parameter   :: cam_nl_autogen4_dimension = 7
   character(len=256), public, protected :: gw_fchar3(7) = unset_str

CONTAINS

   subroutine autogen_gw_drag_readnl(nl_unit, mpicomm, mpiroot, mpi_isroot, logunit)
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
      character(len=*), parameter :: subname = 'autogen_gw_drag_readnl'

      namelist /gw_drag_nl/ pgwv, gw_dc, tau_0_ubc, bnd_rdggm, gw_farr1, gw_fake2, gw_fchar3

      ! Read the namelist on the root task
      if (mpi_isroot) then
         rewind(nl_unit)
         call shr_nl_find_group_name(nl_unit, 'gw_drag_nl', status=ierr)
         if (ierr == 0) then
            read(nl_unit, gw_drag_nl, iostat=ierr)
            if (ierr /= 0) then
               call endrun(subname//':: ERROR reading namelist, gw_drag_nl')
            end if
         else
            call endrun(subname//':: ERROR: Did not find namelist group, gw_drag_nl.')
         end if
         ! Print out namelist values
         write(logunit, *) 'Namelist values from gw_drag_nl for gw_drag'
         write(logunit, *) 'pgwv = ', pgwv
         write(logunit, *) 'gw_dc = ', gw_dc
         write(logunit, *) 'tau_0_ubc = ', tau_0_ubc
         write(logunit, *) 'bnd_rdggm = ', bnd_rdggm
         write(logunit, *) 'gw_farr1 = ', gw_farr1
         write(logunit, *) 'gw_fake2 = ', gw_fake2
         write(logunit, *) 'gw_fchar3 = ', gw_fchar3
      end if
      ! Broadcast the namelist variables
      call mpi_bcast(pgwv, 1, MPI_Integer, mpiroot, mpicomm, ierr)
      call mpi_bcast(gw_dc, 1, MPI_Real8, mpiroot, mpicomm, ierr)
      call mpi_bcast(tau_0_ubc, 1, MPI_Logical, mpiroot, mpicomm, ierr)
      call mpi_bcast(bnd_rdggm, len(bnd_rdggm), MPI_Character, mpiroot, mpicomm, ierr)
      call mpi_bcast(gw_farr1, 2, MPI_Real8, mpiroot, mpicomm, ierr)
      call mpi_bcast(gw_fake2, 6, MPI_Integer, mpiroot, mpicomm, ierr)
      call mpi_bcast(gw_fchar3, len(gw_fchar3(1))*7, MPI_Character, mpiroot, mpicomm, ierr)

   end subroutine autogen_gw_drag_readnl

end module gw_drag_namelist
