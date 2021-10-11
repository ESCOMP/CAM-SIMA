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
!! @brief Auto-generated Variables for registry source file, physics_types_ddt2
!!
!
module physics_types_ddt2

  use ccpp_kinds, only: kind_phys


  implicit none
  private

!> \section arg_table_physics_base  Argument Table
!! \htmlinclude physics_base.html
  type, bind(C) :: physics_base
    ! ncol: Number of horizontal columns
    integer                    :: ncol = 0
    ! pver: Number of vertical layers
    integer                    :: pver = 0
  end type physics_base

!> \section arg_table_model_wind  Argument Table
!! \htmlinclude model_wind.html
  type, public :: model_wind
    ! u: X wind
    real(kind_phys),         pointer          :: u(:, :) => NULL()
    ! v: Y wind
    real(kind_phys),         pointer          :: v(:, :) => NULL()
  end type model_wind

!> \section arg_table_physics_state  Argument Table
!! \htmlinclude physics_state.html
  type, extends(physics_base) :: physics_state
    ! latitude: Latitude
    real(kind_phys),          pointer          :: latitude(:) => NULL()
    ! longitude: Longitude
    real(kind_phys),          pointer          :: longitude(:) => NULL()
    ! wind: Model wind
    type(model_wind)                           :: wind
  end type physics_state

!> \section arg_table_physics_types_ddt2  Argument Table
!! \htmlinclude physics_types_ddt2.html
  ! phys_state: Physics state variables updated by dynamical core
  type(physics_state), public            :: phys_state

!! public interfaces
  public :: allocate_physics_types_ddt2_fields
  public :: physics_types_ddt2_tstep_init

CONTAINS

  subroutine allocate_physics_types_ddt2_fields(horizontal_dimension, vertical_layer_dimension,   &
       set_init_val_in, reallocate_in)
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
    character(len=*), parameter :: subname = "allocate_physics_types_ddt2_fields"

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

    if (associated(phys_state%latitude)) then
      if (reallocate) then
        deallocate(phys_state%latitude)
        nullify(phys_state%latitude)
      else
        call endrun(subname//": phys_state%latitude is already associated, cannot allocate")
      end if
    end if
    allocate(phys_state%latitude(horizontal_dimension))
    if (set_init_val) then
      phys_state%latitude = nan
    end if
    if (associated(phys_state%longitude)) then
      if (reallocate) then
        deallocate(phys_state%longitude)
        nullify(phys_state%longitude)
      else
        call endrun(subname//": phys_state%longitude is already associated, cannot allocate")
      end if
    end if
    allocate(phys_state%longitude(horizontal_dimension))
    if (set_init_val) then
      phys_state%longitude = nan
    end if
    if (associated(phys_state%wind%u)) then
      if (reallocate) then
        deallocate(phys_state%wind%u)
        nullify(phys_state%wind%u)
      else
        call endrun(subname//": phys_state%wind%u is already associated, cannot allocate")
      end if
    end if
    allocate(phys_state%wind%u(horizontal_dimension, vertical_layer_dimension))
    if (set_init_val) then
      phys_state%wind%u = nan
    end if
    if (associated(phys_state%wind%v)) then
      if (reallocate) then
        deallocate(phys_state%wind%v)
        nullify(phys_state%wind%v)
      else
        call endrun(subname//": phys_state%wind%v is already associated, cannot allocate")
      end if
    end if
    allocate(phys_state%wind%v(horizontal_dimension, vertical_layer_dimension))
    if (set_init_val) then
      phys_state%wind%v = nan
    end if
    if (set_init_val) then
      phys_state%ncol = 0
    end if
    if (set_init_val) then
      phys_state%pver = 0
    end if
  end subroutine allocate_physics_types_ddt2_fields

  subroutine physics_types_ddt2_tstep_init()

    !! Local variables
    character(len=*), parameter :: subname = "physics_types_ddt2_tstep_init"

  end subroutine physics_types_ddt2_tstep_init

end module physics_types_ddt2
