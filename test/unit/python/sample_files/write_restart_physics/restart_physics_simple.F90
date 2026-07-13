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
module restart_physics_simple

   use pio, only: var_desc_t


   implicit none
   private

!! public interfaces
   public :: restart_physics_init
   public :: restart_physics_write
   public :: restart_physics_read

! Private module data
   type(var_desc_t) :: theta_desc
   type(var_desc_t) :: slp_desc
   type(var_desc_t) :: ptend_desc
   type(var_desc_t), allocatable :: cnst_desc(:)

contains

   subroutine restart_physics_init(file, errmsg, errflg)
      use pio,                       only: file_desc_t, pio_double
      use cam_pio_utils,             only: cam_pio_def_dim, cam_pio_def_var
      use cam_ccpp_cap,              only: cam_model_const_properties, cam_constituents_array
      use physics_grid,              only: num_global_phys_cols
      use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t
      use vert_coord,   only: pver
      use physics_grid, only: columns_on_task
      type(file_desc_t), intent(inout) :: file
      character(len=512),intent(out)   :: errmsg
      integer,           intent(out)   :: errflg

      ! Local variables
      integer, allocatable :: dimids(:)
      integer :: constituent_idx
      integer :: nonadvected_idx
      logical :: advected
      type(ccpp_constituent_prop_ptr_t), pointer :: const_props(:)
      character(len=256) :: const_diag_name


      ! Allocate dimids to the number of unique dimensions
      allocate(dimids(2), stat=errflg, errmsg=errmsg)
      if (errflg /= 0) then
         return
      end if
      call cam_pio_def_dim(file, 'ncol', num_global_phys_cols, dimids(1), existOK=.true.)
      call cam_pio_def_dim(file, 'lev', pver, dimids(2), existOK=.true.)

      ! Define required restart variables on the restart file
      call cam_pio_def_var(file, 'theta', pio_double, (/dimids(1), dimids(2)/), theta_desc, existOK=.false.)
      if (errflg /= 0) then
         write(errmsg,*) 'restart_physics_init: error defining variable theta'
         return
      end if


      ! Define required restart variables on the restart file
      call cam_pio_def_var(file, 'slp', pio_double, (/dimids(1)/), slp_desc, existOK=.false.)
      if (errflg /= 0) then
         write(errmsg,*) 'restart_physics_init: error defining variable slp'
         return
      end if


      ! Define required restart variables on the restart file
      call cam_pio_def_var(file, 'ptend', pio_double, (/dimids(1)/), ptend_desc, existOK=.false.)
      if (errflg /= 0) then
         write(errmsg,*) 'restart_physics_init: error defining variable ptend'
         return
      end if


      ! Handling for non-advected constituent vars (advected constituents handled by dynamics restart)
      ! Allocate cnst_desc to total size of constituents array; some will be unused
      allocate(cnst_desc(size(const_props)))
      nonadvected_idx = 1
      do constituent_idx = 1, size(const_props)
         call const_props(constituent_idx)%is_advected(advected)
         if (.not. advected) then
            call const_props(constituent_idx)%diagnostic_name(const_diag_name)
            call cam_pio_def_var(file, trim(const_diag_name), pio_double, (/dimids(1), dimids(2)/), cnst_desc(nonadvected_idx), existOK=.false.)
            nonadvected_idx = nonadvected_idx + 1
         end if
      end do
   end subroutine restart_physics_init

   subroutine restart_physics_write(file, grid_id, errmsg, errflg)
      use pio,                       only: file_desc_t, io_desc_t, pio_write_darray, pio_double
      use cam_ccpp_cap,              only: cam_model_const_properties, cam_constituents_array
      use ccpp_kinds,                only: kind_phys
      use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t
      use physics_grid,              only: num_global_phys_cols
      use cam_grid_support,          only: cam_grid_id, cam_grid_write_dist_array
      use vert_coord,   only: pver
      use physics_grid, only: columns_on_task
      use physics_types, only: ptend
      use physics_types, only: slp
      use physics_types, only: theta

      type(file_desc_t), intent(inout) :: file
      integer,            intent(in)   :: grid_id
      character(len=512),intent(out)   :: errmsg
      integer,           intent(out)   :: errflg

      ! Local variables
      integer                          :: dims(2)
      integer                          :: grid_decomp
      integer                          :: grid_dims(2)
      integer                          :: field_shape(2)
      integer                          :: constituent_idx
      integer                          :: nonadvected_idx
      logical                          :: advected
      real(kind=kind_phys), pointer    :: field_data_ptr(:,:,:)
      type(ccpp_constituent_prop_ptr_t), pointer :: const_props(:)
      ! Grab physics grid
      grid_decomp = cam_grid_id('physgrid')
      dims(1) = columns_on_task
      dims(2) = pver
      ! Write required restart variables to the restart file
      field_shape(1) = num_global_phys_cols
      field_shape(2) = size(theta, 2)
      call cam_grid_write_dist_array(file, grid_decomp, (/dims(1),dims(2)/), field_shape, theta, theta_desc)
      ! Handle horizontal-only field
      field_shape(1) = num_global_phys_cols
      call cam_grid_write_dist_array(file, grid_decomp, (/dims(1)/), (/field_shape(1)/), slp, slp_desc)
      ! Handle horizontal-only field
      field_shape(1) = num_global_phys_cols
      call cam_grid_write_dist_array(file, grid_decomp, (/dims(1)/), (/field_shape(1)/), ptend, ptend_desc)

      ! Handling for non-advected constituent vars (advected constituents handled by dynamics restart)
      field_shape(1) = num_global_phys_cols
      field_shape(2) = pver
      nonadvected_idx = 1
      field_data_ptr => cam_constituents_array()
      do constituent_idx = 1, size(const_props)
         call const_props(constituent_idx)%is_advected(advected)
         if (.not. advected) then
            call cam_grid_write_dist_array(file, grid_decomp, (/dims(1), dims(2)/), field_shape, field_data_ptr(:,:,constituent_idx), &
                cnst_desc(nonadvected_idx))
            nonadvected_idx = nonadvected_idx + 1
         end if
      end do
   end subroutine restart_physics_write

   subroutine restart_physics_read()
   end subroutine restart_physics_read

end module restart_physics_simple
