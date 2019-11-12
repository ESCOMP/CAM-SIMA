module physics_types_parameter

  use ccpp_kinds, only: kind_phys

implicit none
private

  ! ncol: Number of horizontal columns
  integer,         public,            protected :: ncol = 0
  ! latitude: Latitude
  real(kind_phys), public, pointer,   protected :: latitude(:) => NULL()
  ! longitude: Longitude
  real(kind_phys), public, pointer,   protected :: longitude(:) => NULL()
  ! pver: Vertical layer dimension
  integer,         public, parameter          :: pver = 42

!! public interfaces
  public :: allocate_physics_types_parameter_fields

CONTAINS

  subroutine allocate_physics_types_parameter_fields(horizontal_dimension, set_to_nan_in,         &
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
    character(len=*), parameter :: subname = "allocate_physics_types_parameter_fields"

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

    if (set_to_nan) then
      ncol = HUGE(1)
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
    if (associated(longitude)) then
      if (reallocate) then
        deallocate(longitude)
        nullify(longitude)
      else
        call endrun(subname//": longitude is already associated, cannot allocate")
      end if
    end if
    allocate(longitude(horizontal_dimension))
    if (set_to_nan) then
      longitude = nan
    end if
  end subroutine allocate_physics_types_parameter_fields

end module physics_types_parameter
