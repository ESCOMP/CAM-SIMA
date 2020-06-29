module physics_types_parameter

  use ccpp_kinds, only: kind_phys

implicit none
private

!Number of physics variables which can be read from Initial Conditions (IC) file:
integer, public, parameter :: ic_var_num = 2

!Max length of registered variable standard names:
integer, public, parameter :: std_name_len = 24

!Max length of input (IC) file variable names:
integer, public, parameter :: ic_name_len = 3

character(len=24), public :: input_var_stdnames(ic_var_num) = (/ &
  'latitude                ', &
  'longitude               ' /)

character(len=3), public :: input_var_names(1, ic_var_num) = reshape((/ &
  'lat', &
  'lon' /), (/1, ic_var_num/))

!> \section arg_table_physics_types_parameter  Argument Table
!! \htmlinclude physics_types_parameter.html
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

  subroutine allocate_physics_types_parameter_fields(horizontal_dimension, set_init_val_in,       &
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
    character(len=*), parameter :: subname = "allocate_physics_types_parameter_fields"

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
  end subroutine allocate_physics_types_parameter_fields

end module physics_types_parameter
