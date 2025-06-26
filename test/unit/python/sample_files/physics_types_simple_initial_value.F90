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
!! @brief Auto-generated Variables for registry source file, physics_types_simple_initial_value
!!
!
module physics_types_simple_initial_value

   use ccpp_kinds, only: kind_phys


   implicit none
   private

!> \section arg_table_physics_types_simple_initial_value  Argument Table
!! \htmlinclude physics_types_simple_initial_value.html
   ! theta: Potential temperature
   real(kind_phys), public, pointer              :: theta(:, :) => NULL()
   ! slp: Air pressure at sea level
   real(kind_phys), public, allocatable          :: slp(:)
   ! eddy_len: Eddy length scale
   real(kind_phys), public, pointer              :: eddy_len(:) => NULL()

!! public interfaces
   public :: allocate_physics_types_simple_initial_value_fields
   public :: physics_types_simple_initial_value_tstep_init

CONTAINS

   subroutine allocate_physics_types_simple_initial_value_fields(horizontal_dimension,            &
        vertical_layer_dimension, set_init_val_in, reallocate_in)
      use shr_infnan_mod,   only: nan => shr_infnan_nan, assignment(=)
      use cam_abortutils,   only: endrun
      !! Dummy arguments
      integer,           intent(in) :: horizontal_dimension
      integer,           intent(in) :: vertical_layer_dimension
      logical, optional, intent(in) :: set_init_val_in
      logical, optional, intent(in) :: reallocate_in

      !! Local variables
      logical                     :: set_init_val
      logical                     :: reallocate
      character(len=*), parameter :: subname =                                                    &
           "allocate_physics_types_simple_initial_value_fields"

      ! Set optional argument values
      if (present(set_init_val_in)) then
         set_init_val = set_init_val_in
      else
         set_init_val = .true.
      end if
      if (present(reallocate_in)) then
         reallocate = reallocate_in
      else
         reallocate = .false.
      end if

      if (associated(theta)) then
         if (reallocate) then
            deallocate(theta)
            nullify(theta)
         else
            call endrun(subname//": theta is already associated, cannot allocate")
         end if
      end if
      allocate(theta(horizontal_dimension, vertical_layer_dimension))
      if (set_init_val) then
         theta = nan
      end if
      if (allocated(slp)) then
         if (reallocate) then
            deallocate(slp)
         else
            call endrun(subname//": slp is already allocated, cannot allocate")
         end if
      end if
      allocate(slp(horizontal_dimension))
      if (set_init_val) then
         slp = 101325.0_kind_phys
      end if
      if (associated(eddy_len)) then
         if (reallocate) then
            deallocate(eddy_len)
            nullify(eddy_len)
         else
            call endrun(subname//": eddy_len is already associated, cannot allocate")
         end if
      end if
      allocate(eddy_len(horizontal_dimension))
      if (set_init_val) then
         eddy_len = nan
      end if
   end subroutine allocate_physics_types_simple_initial_value_fields

   subroutine physics_types_simple_initial_value_tstep_init()

      !! Local variables
      character(len=*), parameter :: subname = "physics_types_simple_initial_value_tstep_init"

   end subroutine physics_types_simple_initial_value_tstep_init

end module physics_types_simple_initial_value
