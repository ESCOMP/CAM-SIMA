module phys_vars_init_check

implicit none
private

  !Total number of physics-related variables:
  integer, public, parameter :: phys_var_num = 18

  !Max length of physics-related variable standard names:
  integer, public, parameter :: std_name_len = 49

  !Max length of input (IC) file variable names:
  integer, public, parameter :: ic_name_len = 3

  !Array storing all physics-related variable standard names:
  character(len=49), public, protected :: phys_var_stdnames(phys_var_num) = (/ &
    'longitude                                        ', &
    'physics_state_from_dynamics                      ', &
    'horizontal_dimension                             ', &
    'latitude                                         ', &
    'reference_pressure_at_interface                  ', &
    'reference_pressure                               ', &
    'reference_pressure_normalized_by_surface_pressure', &
    'top_of_model_air_pressure                        ', &
    'surface_reference_air_pressure                   ', &
    'number_of_pure_pressure_levels_at_top            ', &
    'pressure_at_troposhere_cloud_top                 ', &
    'index_of_pressure_at_troposhere_cloud_top        ', &
    'pressure_at_top_of_aerosol_model                 ', &
    'index_of_pressure_at_top_of_aerosol_model        ', &
    'pressure_at_top_of_molecular_diffusion           ', &
    'pressure_at_bottom_of_molcular_diffusion         ', &
    'molecular_diffusion_flag                         ', &
    'index_of_pressure_at_bottom_of_molcular_diffusion' /)

  !Array storing all registered IC file input names for each variable:
  character(len=3), public, protected :: input_var_names(1, phys_var_num) = reshape((/ &
    'lon', &
    '   ', &
    '   ', &
    'lat', &
    '   ', &
    '   ', &
    '   ', &
    '   ', &
    '   ', &
    '   ', &
    '   ', &
    '   ', &
    '   ', &
    '   ', &
    '   ', &
    '   ', &
    '   ', &
    '   ' /), (/1, phys_var_num/))

  !Logical array to indicate whether or not variable is initialized:
  logical, public, protected :: initialized_vars(phys_var_num) = (/ &
    .false., &
    .false., &
    .false., &
    .false., &
    .false., &
    .false., &
    .false., &
    .false., &
    .false., &
    .false., &
    .false., &
    .false., &
    .false., &
    .false., &
    .false., &
    .false., &
    .false., &
    .false. /)

!! public interfaces
  public :: mark_as_initialized
  public :: is_initialized

CONTAINS

  subroutine mark_as_initialized(varname)

    !This subroutine  marks the variable as
    !initialized in the `initialized_vars` array,
    !which means any initialization check should now
    !now return True.

    use cam_abortutils, only: endrun

    implicit none

    character(len=*), intent(in) :: varname !Variable name being marked

    integer :: stdnam_idx !standard name array index

    !Loop over standard name array:
    do stdnam_idx = 1, phys_var_num
      !Check if standard name matches provided variable name:
      if (trim(phys_var_stdnames(stdnam_idx)) == trim(varname)) then
        !If so, then set associated initialized_vars
        !array index to true:
        initialized_vars(stdnam_idx) = .true.

        !Exit function:
        exit
      end if
    end do

    !If loop has completed with no matches, then endrun with warning
    !that variable didn't exist in standard names array:
    call endrun(&
    "Variable '"//trim(varname)//"' is missing from phys_var_stdnames array.")

  end subroutine mark_as_initialized


  logical function is_initialized(varname)

    !This function checks if the variable is
    !already initialized according to the
    !`initialized_vars` array.

    use cam_abortutils, only: endrun

    implicit none

    character(len=*), intent(in) :: varname !Variable name being checked

    integer :: stdnam_idx !standard name array index

    is_initialized = .false.

    !Loop over standard name array:
    do stdnam_idx = 1, phys_var_num
      !Check if standard name matches provided variable name:
      if (trim(phys_var_stdnames(stdnam_idx)) == trim(varname)) then
        !If so, then return initialized_vars
        !value associated with that index:
        is_initialized = initialized_vars(stdnam_idx)
      end if
    end do

    !If loop has completed with no matches, then endrun with warning
    !that variable didn't exist in standard names array:
    call endrun(&
    "Variable '"//trim(varname)//"' is missing from phys_var_stdnames array.")

  end function is_initialized

end module phys_vars_init_check
