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
module restart_physics_const_dim

   use pio, only: var_desc_t


   implicit none
   private

!! public interfaces
   public :: restart_physics_init
   public :: restart_physics_write
   public :: restart_physics_read

! Private module data
   type(var_desc_t), allocatable :: cool_cat_for_each_const_desc(:)
   type(var_desc_t), allocatable :: cool_default_cat_for_each_const_desc(:)

contains

   subroutine restart_physics_init(file, errmsg, errflg)
      use pio,                       only: file_desc_t, pio_double
      use cam_pio_utils,             only: cam_pio_def_dim, cam_pio_def_var
      use cam_ccpp_cap,              only: cam_model_const_properties
      use physics_grid,              only: num_global_phys_cols
      use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t
      use simple_sub, only: pcols
      type(file_desc_t), intent(inout) :: file
      character(len=512),intent(out)   :: errmsg
      integer,           intent(out)   :: errflg

      ! Local variables
      integer, allocatable :: dimids(:)
      integer :: constituent_idx
      type(ccpp_constituent_prop_ptr_t), pointer :: const_props(:)
      character(len=256) :: const_diag_name


      ! Allocate dimids to the number of unique dimensions
      allocate(dimids(1), stat=errflg, errmsg=errmsg)
      if (errflg /= 0) then
         return
      end if
      ! Define required restart variables on the restart file
      const_props => cam_model_const_properties()

      ! Define potentially new dimension 'horizontal_dimension'
      call cam_pio_def_dim(file, 'ncol', num_global_phys_cols, dimids(1), existOK=.true.)
      ! Handling for constituent-dimensioned variable 'cool_cat_for_each_const'
      allocate(cool_cat_for_each_const_desc(size(const_props)), stat=errflg, errmsg=errmsg)
      if (errflg /= 0) then
         return
      end if
      do constituent_idx = 1, size(const_props)
         ! Grab constituent diagnostic name:
         call const_props(constituent_idx)%diagnostic_name(const_diag_name)
         call cam_pio_def_var(file, 'cool_cat_for_each_const_'//trim(const_diag_name), pio_double, (/dimids(1)/), &
             cool_cat_for_each_const_desc(constituent_idx), existOK=.false.)
      end do

      ! Handling for constituent-dimensioned variable 'cool_default_cat_for_each_const'
      allocate(cool_default_cat_for_each_const_desc(size(const_props)), stat=errflg, errmsg=errmsg)
      if (errflg /= 0) then
         return
      end if
      do constituent_idx = 1, size(const_props)
         ! Grab constituent diagnostic name:
         call const_props(constituent_idx)%diagnostic_name(const_diag_name)
         call cam_pio_def_var(file, 'cool_default_cat_for_each_const_'//trim(const_diag_name), pio_double, (/dimids(1)/), &
             cool_default_cat_for_each_const_desc(constituent_idx), existOK=.false.)
      end do

   end subroutine restart_physics_init

   subroutine restart_physics_write(file, grid_id, errmsg, errflg)
      use pio,                       only: file_desc_t, io_desc_t, pio_write_darray, pio_double
      use cam_ccpp_cap,              only: cam_model_const_properties, cam_constituents_array
      use ccpp_kinds,                only: kind_phys
      use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t
      use physics_grid,              only: num_global_phys_cols
      use cam_grid_support,          only: cam_grid_id, cam_grid_write_dist_array
      use simple_sub, only: pcols
      use physics_types, only: cool_cat_for_each_const
      use physics_types, only: cool_default_cat_for_each_const

      type(file_desc_t), intent(inout) :: file
      integer,            intent(in)   :: grid_id
      character(len=512),intent(out)   :: errmsg
      integer,           intent(out)   :: errflg

      ! Local variables
      integer                         :: dims(1)
      integer                          :: grid_decomp
      integer                          :: grid_dims(2)
      integer                          :: field_shape(2)
      integer                          :: constituent_idx
      type(ccpp_constituent_prop_ptr_t), pointer :: const_props(:)
      ! Grab physics grid
      grid_decomp = cam_grid_id('physgrid')
      dims(1) = pcols
      ! Write required restart variables to the restart file
      const_props => cam_model_const_properties()

      ! Handling for constituent-dimensioned variable 'cool_cat_for_each_const'
      do constituent_idx = 1, size(const_props)
         field_shape(1) = num_global_phys_cols
         call cam_grid_write_dist_array(file, grid_decomp, (/dims(1)/), (/field_shape(1)/), cool_cat_for_each_const(:,constituent_idx), &
             cool_cat_for_each_const_desc(constituent_idx))
      end do

      ! Handling for constituent-dimensioned variable 'cool_default_cat_for_each_const'
      do constituent_idx = 1, size(const_props)
         field_shape(1) = num_global_phys_cols
         call cam_grid_write_dist_array(file, grid_decomp, (/dims(1)/), (/field_shape(1)/), cool_default_cat_for_each_const(:,constituent_idx), &
             cool_default_cat_for_each_const_desc(constituent_idx))
      end do

   end subroutine restart_physics_write

   subroutine restart_physics_read()
   end subroutine restart_physics_read

end module restart_physics_const_dim
