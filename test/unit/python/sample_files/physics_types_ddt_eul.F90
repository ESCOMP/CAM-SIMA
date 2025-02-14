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
!! @brief Auto-generated Variables for registry source file, physics_types_ddt
!!
!
module physics_types_ddt

  use ccpp_kinds, only: kind_phys
  use physconst,  only: cpair
  use physconst,  only: rair


  implicit none
  private

!> \section arg_table_physics_state  Argument Table
!! \htmlinclude physics_state.html
  type, public :: physics_state
    ! ncol: Number of horizontal columns
    integer                    :: ncol = 0
  end type physics_state

!> \section arg_table_physics_types_ddt  Argument Table
!! \htmlinclude physics_types_ddt.html
  ! latitude: Latitude
  real(kind_phys),     public, pointer,     protected :: latitude(:) => NULL()
  ! longitude: Longitude
  real(kind_phys),     public, pointer,     protected :: longitude(:) => NULL()
  ! cappav: Composition-dependent ratio of dry air gas constant to specific heat at constant
  ! pressure
  real(kind_phys),     public, allocatable          :: cappav(:, :)
  ! phys_state: Physics state variables updated by dynamical core
  type(physics_state), public                       :: phys_state

!! public interfaces
  public :: allocate_physics_types_ddt_fields
  public :: physics_types_ddt_tstep_init

CONTAINS

  subroutine allocate_physics_types_ddt_fields(horizontal_dimension, vertical_layer_dimension,    &
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
    character(len=*), parameter :: subname = "allocate_physics_types_ddt_fields"

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
    if (allocated(cappav)) then
      if (reallocate) then
        deallocate(cappav)
      else
        call endrun(subname//": cappav is already allocated, cannot allocate")
      end if
    end if
    allocate(cappav(horizontal_dimension, vertical_layer_dimension))
    if (set_init_val) then
      cappav = rair/cpair
      call                                                                                        &
           mark_as_initialized('composition_dependent_ratio_of_dry_air_gas_constant_to_specific_heat_at_constant_pressure')

    end if
    if (set_init_val) then
      phys_state%ncol = 0
    end if
  end subroutine allocate_physics_types_ddt_fields

  subroutine physics_types_ddt_tstep_init()

    !! Local variables
    character(len=*), parameter :: subname = "physics_types_ddt_tstep_init"

  end subroutine physics_types_ddt_tstep_init

end module physics_types_ddt
