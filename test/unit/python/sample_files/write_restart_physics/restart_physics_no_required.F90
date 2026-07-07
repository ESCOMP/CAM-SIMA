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
!! @brief Auto-generated physics restart source file
!!
!
module restart_physics_no_required

   use pio, only: var_desc_t


   implicit none
   private

!! public interfaces
   public :: restart_physics_init
   public :: restart_physics_write
   public :: restart_physics_read

! Private module data

contains

   subroutine restart_physics_init(file, errmsg, errflg)
      use pio,                       only: file_desc_t, pio_double
      use cam_pio_utils,             only: cam_pio_def_dim, cam_pio_def_var
      use cam_ccpp_cap,              only: cam_model_const_properties
      use physics_grid,              only: num_global_phys_cols
      use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t
      type(file_desc_t), intent(inout) :: file
      character(len=512),intent(out)   :: errmsg
      integer,           intent(out)   :: errflg

      ! Local variables
      integer, allocatable :: dimids(:)
      integer :: constituent_idx
      type(ccpp_constituent_prop_ptr_t), pointer :: const_props(:)
      character(len=256) :: const_diag_name


      ! Allocate dimids to the number of unique dimensions
      allocate(dimids(0), stat=errflg, errmsg=errmsg)
      if (errflg /= 0) then
         return
      end if
      ! Define required restart variables on the restart file
   end subroutine restart_physics_init

   subroutine restart_physics_write(file, grid_id, errmsg, errflg)
      use pio,                       only: file_desc_t, io_desc_t, pio_write_darray, pio_double
      use cam_ccpp_cap,              only: cam_model_const_properties, cam_constituents_array
      use ccpp_kinds,                only: kind_phys
      use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t
      use physics_grid,              only: num_global_phys_cols
      use cam_grid_support,          only: cam_grid_id, cam_grid_write_dist_array

      type(file_desc_t), intent(inout) :: file
      integer,            intent(in)   :: grid_id
      character(len=512),intent(out)   :: errmsg
      integer,           intent(out)   :: errflg

      ! Local variables
      integer                         :: dims(0)
      integer                          :: grid_decomp
      integer                          :: grid_dims(2)
      integer                          :: field_shape(2)
      integer                          :: constituent_idx
      type(ccpp_constituent_prop_ptr_t), pointer :: const_props(:)
   end subroutine restart_physics_write

   subroutine restart_physics_read()
   end subroutine restart_physics_read

end module restart_physics_no_required
