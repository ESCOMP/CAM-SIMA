module physics_inputs

implicit none
private

!! public interfaces
  public :: physics_read_data

CONTAINS

  subroutine physics_read_data(file, suite_names, timestep)
    use pio,                  only: file_desc_t
    use cam_abortutils,       only: endrun
    use shr_kind_mod,         only: SHR_KIND_CS, SHR_KIND_CL
    use physics_data,         only: read_field, find_input_name_idx
    use phys_vars_init_check, only: phys_var_stdnames, input_var_names
    use phys_vars_init_check, only: std_name_len
    use cam_ccpp_cap,         only: ccpp_physics_suite_variables
    use physics_types,        only: ncol, latitude, longitude

    ! Dummy arguments
    type(file_desc_t), intent(inout) :: file
    character(len=SHR_KIND_CS)       :: suite_names(:) !Names of CCPP suites
    integer,           intent(in)    :: timestep

    !Local variables:

    !Character array containing all CCPP-required vairable standard names:
    character(len=std_name_len), allocatable :: ccpp_required_data(:)

    !String which stores names of any missing vars:
    character(len=SHR_KIND_CL) :: missing_required_vars
    character(len=SHR_KIND_CL) :: missing_input_names

    character(len=512) :: errmsg    !CCPP framework error message
    integer            :: errflg    !CCPP framework error flag
    integer            :: name_idx  !Input variable array index
    integer            :: req_idx   !Required variable array index
    integer            :: suite_idx !Suite array index

    !Initalize missing variables string:
    missing_required_vars = ' '
    missing_input_names   = ' '

    !Loop over CCPP physics/chemistry suites:
    do suite_idx = 1, size(suite_names, 1)

      !Search for all needed CCPP input variables,
      !so that they can bx e read from input file if need be:
      call ccpp_physics_suite_variables(suite_names(suite_idx), ccpp_required_data, &
        errmsg, errflg, input_vars_in=.true., &
        output_vars_in=.false.)

      !Loop over all required variables as specified by CCPP suite:
      do req_idx = 1, size(ccpp_required_data, 1)

        !Find IC file input name array index for required variable:
        name_idx = find_input_name_idx(ccpp_required_data(req_idx))

        !If variable is already initialized, then skip it:
        if (name_idx == -2) cycle

        !If an index was never found, then save variable name and check the rest
        !of the variables, after which the model simulation will end:
        if (name_idx == -1) then
          if (len_trim(missing_required_vars) == 0) then
            missing_required_vars(len_trim(missing_required_vars)+1:) = &
              trim(ccpp_required_data(req_idx))
          else
            missing_required_vars(len_trim(missing_required_vars)+1:) = &
              ', '//trim(ccpp_required_data(req_idx))
          end if
          !Continue on with variable loop:
          cycle
        end if

        !Next, check that the input variable names aren't blank.
        !If so, then save variable name and check the rest of the
        !variables, after which the model simulation will end:
        if (len_trim(input_var_names(1,name_idx)) == 0) then
          if (len_trim(missing_input_names) == 0) then
            missing_input_names(len_trim(missing_input_names)+1:) = &
              trim(ccpp_required_data(req_idx))
          else
            missing_input_names(len_trim(missing_input_names)+1:) = &
              ', '//trim(ccpp_required_data(req_idx))
          end if
          !Continue on with variable loop:
          cycle
        end if

        if (trim(phys_var_stdnames(name_idx)) == 'horizontal_dimension') then
          call read_field(file, input_var_names(:,name_idx), timestep, ncol)
        end if

        if (trim(phys_var_stdnames(name_idx)) == 'latitude') then
          call read_field(file, input_var_names(:,name_idx), timestep, latitude)
        end if

        if (trim(phys_var_stdnames(name_idx)) == 'longitude') then
          call read_field(file, input_var_names(:,name_idx), timestep, longitude)
        end if

      end do !Suite-required variables

      !End simulation if there are missing input
      !variables that are required:
      if (len_trim(missing_required_vars) > 0) then
        call endrun("Required variables missing from registered list of input variables: "//&
          trim(missing_required_vars))
      end if

      !End simulation if there are variables that
      !have no input names:
      if (len_trim(missing_input_names) > 0) then
        call endrun(&
          "Required variables missing a list of input names (<ic_file_input_names>): "//&
          trim(missing_input_names))
      end if

      !Deallocate required variables array for use in next suite:
      deallocate(ccpp_required_data)

    end do !CCPP suites

  end subroutine physics_read_data


end module physics_inputs
