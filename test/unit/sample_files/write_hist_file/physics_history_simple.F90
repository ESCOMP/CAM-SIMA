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
module physics_history_simple


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
      use physics_types_simple,      only: slp

      ! Local variables:

      integer :: const_index
      integer :: errcode
      logical :: const_is_dry
      character(len=256) :: errmsg
      type(ccpp_constituent_prop_ptr_t), pointer :: const_props_ptr(:)
      character(len=*), parameter :: subname = "physics_history_init"

      call history_add_field('SLP', 'air_pressure_at_sea_level', horiz_only, 'avg', 'Pa')
      call const_get_index('super_cool_cat_const', const_index, abort=.false., warning=.false.)
      if (const_index >= 0) then
         const_props_ptr => cam_model_const_properties()
         call const_props_ptr(const_index)%is_dry(const_is_dry, errcode, errmsg)
         if (const_is_dry) then
            call history_add_field('COOL_CAT', 'super_cool_cat_const', 'lev', 'avg', 'kg kg-1', mixing_ratio='dry')
         else
            call history_add_field('COOL_CAT', 'super_cool_cat_const', 'lev', 'avg', 'kg kg-1', mixing_ratio='wet')
         end if
      end if

   end subroutine physics_history_init

   subroutine physics_history_out()
      use cam_ccpp_cap,              only: cam_constituents_array
      use cam_history,               only: history_out_field
      use cam_constituents,          only: const_get_index
      use ccpp_kinds,                only: kind_phys
      use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t
      use physics_types_simple,      only: slp

      ! Local variables:

      !! Local variables
      real(kind_phys), pointer :: const_data_ptr(:,:,:)
      character(len=512) :: standard_name
      integer :: const_index
      character(len=*), parameter :: subname = "physics_history_out"

      call history_out_field('SLP', slp)
      call const_get_index('super_cool_cat_const', const_index, abort=.false., warning=.false.)
      if (const_index >= 0) then
         const_data_ptr => cam_constituents_array()
         call history_out_field('COOL_CAT', const_data_ptr(:,:,const_index))
      end if

   end subroutine physics_history_out

end module physics_history_simple
