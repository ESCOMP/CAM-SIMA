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
!! @brief Auto-generated Variables for registry source file, physics_types_ddt_array
!!
!
module physics_types_ddt_array

  use ccpp_kinds, only: kind_phys


  implicit none
  private

!> \section arg_table_physics_state  Argument Table
!! \htmlinclude physics_state.html
  type, public :: physics_state
    ! ncol: Number of horizontal columns
    integer                                   :: ncol = 0
    ! latitude: Latitude
    real(kind_phys),         pointer          :: latitude(:) => NULL()
    ! longitude: Longitude
    real(kind_phys),         pointer          :: longitude(:) => NULL()
    ! q: Constituent mixing ratio
    real(kind_phys),         pointer          :: q(:, :, :) => NULL()
  end type physics_state

!> \section arg_table_physics_types_ddt_array  Argument Table
!! \htmlinclude physics_types_ddt_array.html
  ! ix_qv: Index of water vapor specific humidity
  integer,             public            :: ix_qv = 1
  ! ix_cld_liq: Index of cloud liquid water mixing ratio of moist air
  integer,             public            :: ix_cld_liq = 2
  ! phys_state: Physics state variables updated by dynamical core
  type(physics_state), public            :: phys_state

!! public interfaces
  public :: allocate_physics_types_ddt_array_fields
  public :: physics_types_ddt_array_tstep_init

CONTAINS

  subroutine allocate_physics_types_ddt_array_fields(horizontal_dimension,                        &
       vertical_layer_dimension, number_of_constituents, set_init_val_in, reallocate_in)
    use shr_infnan_mod,   only: nan => shr_infnan_nan, assignment(=)
    use cam_abortutils,   only: endrun
    !! Dummy arguments
    integer,           intent(in) :: horizontal_dimension
    integer,           intent(in) :: vertical_layer_dimension
    integer,           intent(in) :: number_of_constituents
    logical, optional, intent(in) :: set_init_val_in
    logical, optional, intent(in) :: reallocate_in

    !! Local variables
    logical                     :: set_init_val
    logical                     :: reallocate
    character(len=*), parameter :: subname = "allocate_physics_types_ddt_array_fields"

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
      ix_qv = 1
    end if
    if (set_init_val) then
      ix_cld_liq = 2
    end if
    if (set_init_val) then
      phys_state%ncol = 0
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
    if (associated(phys_state%q)) then
      if (reallocate) then
        deallocate(phys_state%q)
        nullify(phys_state%q)
      else
        call endrun(subname//": phys_state%q is already associated, cannot allocate")
      end if
    end if
    allocate(phys_state%q(horizontal_dimension, vertical_layer_dimension,                         &
         number_of_constituents))
    if (set_init_val) then
      phys_state%q = nan
    end if
    if (set_init_val) then
      phys_state%q(:,:,ix_qv) = nan
    end if
    if (set_init_val) then
      phys_state%q(:,:,ix_cld_liq) = nan
    end if
  end subroutine allocate_physics_types_ddt_array_fields

  subroutine physics_types_ddt_array_tstep_init()

    !! Local variables
    character(len=*), parameter :: subname = "physics_types_ddt_array_tstep_init"

  end subroutine physics_types_ddt_array_tstep_init

end module physics_types_ddt_array
