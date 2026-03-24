module aerosol_physical_properties
  ! Mock stub for unit tests.
  ! Provides physprop_get_id which simply returns a deterministic index
  ! based on the filename hash (position in an internal counter).
  implicit none
  private
  public :: physprop_get_id

  integer, save :: next_id = 0

contains

  integer function physprop_get_id(filename)
    character(len=*), intent(in) :: filename
    next_id = next_id + 1
    physprop_get_id = next_id
  end function physprop_get_id

end module aerosol_physical_properties
