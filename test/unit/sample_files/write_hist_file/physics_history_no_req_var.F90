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
!! @brief Auto-generated Physics history source file
!!
!
module physics_history_no_req_var


   implicit none
   private


!! public interfaces
   public :: physics_history_init
   public :: physics_history_out

CONTAINS

   subroutine physics_history_init()
      use cam_ccpp_cap,              only: cam_model_const_properties
      use cam_history,               only: history_add_field
      use cam_history_support,       only: horiz_only
      use cam_constituents,          only: const_get_index
      use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t

      ! Local variables:

      integer :: const_index
      integer :: errcode
      logical :: const_is_dry
      character(len=256) :: errmsg
      type(ccpp_constituent_prop_ptr_t), pointer :: const_props_ptr(:)
      character(len=*), parameter :: subname = "physics_history_init"


   end subroutine physics_history_init

   subroutine physics_history_out()
      use cam_ccpp_cap,              only: cam_constituents_array
      use cam_history,               only: history_out_field
      use cam_constituents,          only: const_get_index
      use ccpp_kinds,                only: kind_phys
      use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t

      ! Local variables:

      !! Local variables
      real(kind_phys), pointer :: const_data_ptr(:,:,:)
      character(len=512) :: standard_name
      integer :: const_index
      character(len=*), parameter :: subname = "physics_history_out"


   end subroutine physics_history_out

end module physics_history_no_req_var
