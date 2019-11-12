module physics_types_ddt

  use ccpp_kinds, only: kind_phys

implicit none
private

  type, public :: physics_state
    ! ncol: Number of horizontal columns
    integer                          :: ncol = 0
    ! longitude: Longitude
    real(kind_phys),         pointer :: longitude(:) => NULL()
  end type physics_state
  
  ! latitude: Latitude
  real(kind_phys),     public, pointer, protected :: latitude(:) => NULL()
  ! phys_state: Physics state variables updated by dynamical core
  type(physics_state), public                   :: phys_state

!! public interfaces
  public :: allocate_physics_types_ddt_fields

CONTAINS

  subroutine allocate_physics_types_ddt_fields(horizontal_dimension, set_to_nan_in,               &
       reallocate_in)
    use shr_infnan_mod,   only: nan => shr_infnan_nan, assignment(=)
    use cam_abortutils,   only: endrun
    !! Dummy arguments
    integer,           intent(in) :: horizontal_dimension
    logical, optional, intent(in) :: set_to_nan_in
    logical, optional, intent(in) :: reallocate_in

    !! Local variables
    logical                     :: set_to_nan
    logical                     :: reallocate
    character(len=*), parameter :: subname = "allocate_physics_types_ddt_fields"

    ! Set optional argument values
    if (present(set_to_nan_in)) then
      set_to_nan = set_to_nan_in
    else
      set_to_nan = .true.
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
    if (set_to_nan) then
      latitude = nan
    end if
    if (set_to_nan) then
      phys_state%ncol = HUGE(1)
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
    if (set_to_nan) then
      phys_state%longitude = nan
    end if
  end subroutine allocate_physics_types_ddt_fields

end module physics_types_ddt
