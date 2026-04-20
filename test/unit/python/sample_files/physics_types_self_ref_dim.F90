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
!! @brief Auto-generated Variables for registry source file, physics_types_self_ref_dim
!!
!
module physics_types_self_ref_dim

  use ccpp_kinds, only: kind_phys


  implicit none
  private

!> \section arg_table_physics_types_self_ref_dim  Argument Table
!! \htmlinclude physics_types_self_ref_dim.html
  ! ncol: Number of horizontal columns
  integer,         public,              protected :: ncol = 0
  ! pver: Number of vertical layers
  integer,         public,              protected :: pver = 0
  ! ndust: Number of dust size bins
  integer,         public                       :: ndust = 4
  ! rndst: Dust radii by size bin
  real(kind_phys), public, allocatable          :: rndst(:, :, :)

!! public interfaces
  public :: allocate_physics_types_self_ref_dim_fields
  public :: physics_types_self_ref_dim_tstep_init

contains

  subroutine allocate_physics_types_self_ref_dim_fields(set_init_val_in, reallocate_in)
    use shr_infnan_mod,   only: nan => shr_infnan_nan, assignment(=)
    use cam_abortutils,   only: endrun


    !! Dummy arguments
    logical, optional, intent(in) :: set_init_val_in
    logical, optional, intent(in) :: reallocate_in

    !! Local variables
    logical                     :: set_init_val
    logical                     :: reallocate
    character(len=*), parameter :: subname = "allocate_physics_types_self_ref_dim_fields"
    integer                     :: dust_size_bin_dimension
    integer                     :: horizontal_dimension
    integer                     :: vertical_layer_dimension

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

    ! Set self-referential dimension variables
    dust_size_bin_dimension = ndust
    horizontal_dimension = ncol
    vertical_layer_dimension = pver

    if (set_init_val) then
      ncol = 0
    end if
    if (set_init_val) then
      pver = 0
    end if
    if (set_init_val) then
      ndust = 4
    end if
    if (allocated(rndst)) then
      if (reallocate) then
        deallocate(rndst)
      else
        call endrun(subname//": rndst is already allocated, cannot allocate")
      end if
    end if
    allocate(rndst(horizontal_dimension, vertical_layer_dimension, dust_size_bin_dimension))
    if (set_init_val) then
      rndst = 0.0_kind_phys
    end if
  end subroutine allocate_physics_types_self_ref_dim_fields

  subroutine physics_types_self_ref_dim_tstep_init()

    !! Local variables
    character(len=*), parameter :: subname = "physics_types_self_ref_dim_tstep_init"

  end subroutine physics_types_self_ref_dim_tstep_init

end module physics_types_self_ref_dim
