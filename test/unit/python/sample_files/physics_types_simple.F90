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
!! @brief Auto-generated Variables for registry source file, physics_types_simple
!!
!
module physics_types_simple

  use ccpp_kinds, only: kind_phys


  implicit none
  private

!> \section arg_table_physics_types_simple  Argument Table
!! \htmlinclude physics_types_simple.html
  ! ncol: Number of horizontal columns
  integer,         public,          protected :: ncol = 0
  ! latitude: Latitude
  real(kind_phys), public, pointer, protected :: latitude(:) => NULL()
  ! longitude: Longitude
  real(kind_phys), public, pointer, protected :: longitude(:) => NULL()

!! public interfaces
  public :: allocate_physics_types_simple_fields
  public :: physics_types_simple_tstep_init

CONTAINS

  subroutine allocate_physics_types_simple_fields(horizontal_dimension, set_init_val_in,          &
       reallocate_in)
    use shr_infnan_mod,   only: nan => shr_infnan_nan, assignment(=)
    use cam_abortutils,   only: endrun
    !! Dummy arguments
    integer,           intent(in) :: horizontal_dimension
    logical, optional, intent(in) :: set_init_val_in
    logical, optional, intent(in) :: reallocate_in

    !! Local variables
    logical                     :: set_init_val
    logical                     :: reallocate
    character(len=*), parameter :: subname = "allocate_physics_types_simple_fields"

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

    if (set_init_val) then
      ncol = 0
    end if
    if (associated(latitude)) then
      if (reallocate) then
        deallocate(latitude)
        nullify(latitude)
      else
        call endrun(subname//": latitude is already associated, cannot allocate")
      end if
    end if
    allocate(latitude(horizontal_dimension))
    if (set_init_val) then
      latitude = nan
    end if
    if (associated(longitude)) then
      if (reallocate) then
        deallocate(longitude)
        nullify(longitude)
      else
        call endrun(subname//": longitude is already associated, cannot allocate")
      end if
    end if
    allocate(longitude(horizontal_dimension))
    if (set_init_val) then
      longitude = nan
    end if
  end subroutine allocate_physics_types_simple_fields

  subroutine physics_types_simple_tstep_init()

    !! Local variables
    character(len=*), parameter :: subname = "physics_types_simple_tstep_init"

  end subroutine physics_types_simple_tstep_init

end module physics_types_simple
