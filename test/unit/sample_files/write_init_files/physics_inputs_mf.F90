module physics_inputs_mf

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
      use physics_data,         only: no_exist_idx, init_mark_idx, prot_no_init_idx
      use cam_ccpp_cap,         only: ccpp_physics_suite_variables
      use phys_vars_init_check_mf, only: phys_var_stdnames, input_var_names
      use phys_vars_init_check_mf, only: std_name_len
      use physics_types_mf,        only: slp
      use ref_theta,        only: theta

      ! Dummy arguments
      type(file_desc_t), intent(inout) :: file
      character(len=SHR_KIND_CS)       :: suite_names(:) !Names of CCPP suites
      integer,           intent(in)    :: timestep

      !Local variables:

      !Character array containing all CCPP-required vairable standard names:
      character(len=std_name_len), allocatable :: ccpp_required_data(:)

      !Strings which store names of any missing or non-initialized vars:
      character(len=SHR_KIND_CL) :: missing_required_vars
      character(len=SHR_KIND_CL) :: protected_non_init_vars
      character(len=SHR_KIND_CL) :: missing_input_names

      character(len=512) :: errmsg    !CCPP framework error message
      integer            :: errflg    !CCPP framework error flag
      integer            :: name_idx  !Input variable array index
      integer            :: req_idx   !Required variable array index
      integer            :: suite_idx !Suite array index
      character(len=2)   :: sep  = '' !String separator used to print error messages
      character(len=2)   :: sep2 = '' !String separator used to print error messages
      character(len=2)   :: sep3 = '' !String separator used to print error messages

      !Initalize missing and non-initialized variables strings:
      missing_required_vars = ' '
      protected_non_init_vars = ' '
      missing_input_names   = ' '

      !Loop over CCPP physics/chemistry suites:
      do suite_idx = 1, size(suite_names, 1)

         !Search for all needed CCPP input variables,
         !so that they can bx e read from input file if need be:
         call ccpp_physics_suite_variables(suite_names(suite_idx), ccpp_required_data, &
            errmsg, errflg, input_vars_in=.true., output_vars_in=.false.)

         !Loop over all required variables as specified by CCPP suite:
         do req_idx = 1, size(ccpp_required_data, 1)

            !Find IC file input name array index for required variable:
            name_idx = find_input_name_idx(ccpp_required_data(req_idx))

            !Check for special index values:
            select case (name_idx)

               case (init_mark_idx)

                  !If variable is already initialized, then do nothing.

               case (no_exist_idx)

                  !If an index was never found, then save variable name and check the rest
                  !of the variables, after which the model simulation will end:
                  missing_required_vars(len_trim(missing_required_vars)+1:) = &
                     trim(sep)//trim(ccpp_required_data(req_idx))

                  !Update character separator to now include comma:
                  sep = ', '

               case (prot_no_init_idx)

                  !If an index was found for a protected variable, but that variable
                  !was never marked as initialized, then save the variable name and check
                  !the rest of the variables, after which the model simulation will end:
                  protected_non_init_vars(len_trim(protected_non_init_vars)+1:) = &
                     trim(sep2)//trim(ccpp_required_data(req_idx))

                  !Update character separator to now include comma:
                  sep2 = ', '

               case default

                  !Check that the input variable names aren't blank.
                  !If so, then save variable name and check the rest of the
                  !variables, after which the model simulation will end:
                  if (len_trim(input_var_names(1,name_idx)) == 0) then
                     missing_input_names(len_trim(missing_input_names)+1:) = &
                        trim(sep3)//trim(ccpp_required_data(req_idx))

                     !Update character separator to now include comma:
                     sep3 = ', '

                     !Continue on with variable loop:
                     cycle
                  end if

                  !Read variable from IC file:

                  if (trim(phys_var_stdnames(name_idx)) == 'sea_level_pressure') then
                     call read_field(file, input_var_names(:,name_idx), timestep, slp)
                  end if

                  if (trim(phys_var_stdnames(name_idx)) == 'potential_temperature') then
                     call read_field(file, input_var_names(:,name_idx), 'lev', timestep, theta)
                  end if

            end select !special indices

         end do !Suite-required variables

         !End simulation if there are missing input
         !variables that are required:
         if (len_trim(missing_required_vars) > 0) then
            call endrun("Required variables missing from registered list of input variables: "//&
               trim(missing_required_vars))
         end if

         !End simulation if there are protected input
         !variables that are not initialized:
         if (len_trim(protected_non_init_vars) > 0) then
            call endrun("Required, protected input variables are not initialized: "//&
               trim(protected_non_init_vars))
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

end module physics_inputs_mf
