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
!! @brief Auto-generated Initial conditions source file, physics_inputs_param.F90
!!
!
module physics_inputs_param


   implicit none
   private


!! public interfaces
   public :: physics_read_data
   public :: physics_check_data

CONTAINS

   subroutine physics_read_data(file, suite_names, timestep, read_initialized_variables)
      use pio,                        only: file_desc_t
      use cam_abortutils,             only: endrun
      use spmd_utils,                 only: masterproc
      use shr_kind_mod,               only: SHR_KIND_CS, SHR_KIND_CL, SHR_KIND_CX
      use physics_data,               only: read_field, find_input_name_idx, no_exist_idx, init_mark_idx, prot_no_init_idx, const_idx
      use physics_data,               only: read_constituent_dimensioned_field
      use cam_ccpp_cap,               only: ccpp_physics_suite_variables, cam_constituents_array, cam_model_const_properties
      use ccpp_kinds,                 only: kind_phys
      use phys_vars_init_check_param, only: phys_var_num, phys_var_stdnames, input_var_names, std_name_len, is_initialized
      use ccpp_constituent_prop_mod,  only: ccpp_constituent_prop_ptr_t
      use cam_logfile,                only: iulog
      use physics_types_param,        only: g, slp, theta

      ! Dummy arguments
      type(file_desc_t),          intent(inout) :: file
      character(len=SHR_KIND_CS), intent(in)    :: suite_names(:) !Names of CCPP suites
      integer,                    intent(in)    :: timestep
      logical,  optional,         intent(in)    :: read_initialized_variables

      ! Local variables:

      ! Character array containing all CCPP-required variable standard names:
      character(len=std_name_len), allocatable :: ccpp_required_data(:)

      ! Strings which store names of any missing or non-initialized vars:
      character(len=SHR_KIND_CL) :: missing_required_vars
      character(len=SHR_KIND_CL) :: protected_non_init_vars

      character(len=SHR_KIND_CX) :: errmsg          !CCPP framework error message
      integer                    :: errflg          !CCPP framework error flag
      integer                    :: n               !Loop control variable
      integer                    :: name_idx        !Input variable array index
      integer                    :: constituent_idx !Constituent table index
      integer                    :: const_input_idx !input_var_names index for a consituent
      integer                    :: req_idx         !Required variable array index
      integer                    :: suite_idx       !Suite array index
      character(len=2)           :: sep             !String separator used to print err messages
      character(len=2)           :: sep2            !String separator used to print err messages
      character(len=2)           :: sep3            !String separator used to print err messages
      real(kind=kind_phys), pointer :: field_data_ptr(:,:,:)
      logical                    :: var_found       !Bool to determine if consituent found in data files
      character(len=std_name_len) :: std_name       !Variable to hold constiutent standard name

      ! Fields needed for getting default data value for constituents
      type(ccpp_constituent_prop_ptr_t), pointer :: const_props(:)
      real(kind=kind_phys)                       :: constituent_default_value
      real(kind=kind_phys)                       :: constituent_min_value
      integer                                    :: constituent_errflg
      character(len=512)                         :: constituent_errmsg
      logical                                    :: constituent_has_default

      ! Logical to default optional argument to False:
      logical                    :: use_init_variables

      ! Get constituent properties pointer:
      const_props => cam_model_const_properties()

      ! Initialize missing and non-initialized variables strings:
      missing_required_vars = ' '
      protected_non_init_vars = ' '
      sep = ''
      sep2 = ''
      sep3 = ''

      ! Initialize use_init_variables based on whether it was input to function:
      if (present(read_initialized_variables)) then
         use_init_variables = read_initialized_variables
      else
         use_init_variables = .false.
      end if

      ! Loop over CCPP physics/chemistry suites:
      do suite_idx = 1, size(suite_names, 1)

         ! Search for all needed CCPP input variables, so that they can be read from input file if need be:
            call ccpp_physics_suite_variables(suite_names(suite_idx), ccpp_required_data, errmsg, errflg, input_vars=.true., output_vars=.false.)

         ! Loop over all required variables and read from file if uninitialized:
         do req_idx = 1, size(ccpp_required_data, 1)

            ! Find IC file input name array index for required variable:
            name_idx = find_input_name_idx(ccpp_required_data(req_idx), use_init_variables, constituent_idx)

            ! Check for special index values:
            select case (name_idx)

               case (init_mark_idx)

                  ! If variable is already initialized, then do nothing.

               case (no_exist_idx)

                  ! If an index was never found, then save variable name and check the rest of the variables, after which the model simulation will
                  ! end:
                     missing_required_vars(len_trim(missing_required_vars)+1:) = trim(sep)//trim(ccpp_required_data(req_idx))

                  ! Update character separator to now include comma:
                  sep = ', '

               case (prot_no_init_idx)

                  ! If an index was found for a protected variable, but that variable was never marked as initialized, then save the variable name
                  ! and check the rest of the variables, after which the model simulation will end:
                     protected_non_init_vars(len_trim(protected_non_init_vars)+1:) = trim(sep2)//trim(ccpp_required_data(req_idx))

                  ! Update character separator to now include comma:
                  sep2 = ', '

               case (const_idx)

                  ! If an index was found in the constituent hash table, then do nothing, this will be handled later.

               case default

                  ! Read variable from IC file:

                  select case (trim(phys_var_stdnames(name_idx)))
                     case ('potential_temperature')
                        call read_field(file, 'potential_temperature', input_var_names(:,name_idx), 'lev', timestep, theta)

                     case ('air_pressure_at_sea_level')
                        call read_field(file, 'air_pressure_at_sea_level', input_var_names(:,name_idx), timestep, slp)

                     case ('gravitational_acceleration')
                        call endrun('Cannot read g from file'//', g has no horizontal dimension; g is a protected variable')

                  end select !read variables
               end select !special indices

         end do !Suite-required variables

         ! End simulation if there are missing input variables that are required:
         if (len_trim(missing_required_vars) > 0) then
            call endrun("Required variables missing from registered list of input variables: "//&
               trim(missing_required_vars))
         end if

         ! End simulation if there are protected input variables that are not initialized:
         if (len_trim(protected_non_init_vars) > 0) then
            call endrun("Required, protected input variables are not initialized: "//&
               trim(protected_non_init_vars))
         end if

         ! Deallocate required variables array for use in next suite:
         deallocate(ccpp_required_data)

      end do !CCPP suites

      ! Read in constituent variables if not using init variables
      field_data_ptr => cam_constituents_array()

      ! Iterate over all registered constituents
      do constituent_idx = 1, size(const_props)
         var_found = .false.
         ! Check if constituent standard name in registered SIMA standard names list:
         call const_props(constituent_idx)%standard_name(std_name)
         if(any(phys_var_stdnames == trim(std_name))) then
            ! Don't read the variable in if it's already initialized
            if (is_initialized(std_name)) then
               cycle
            end if
            ! Find array index to extract correct input names:
            do n=1, phys_var_num
               if(trim(phys_var_stdnames(n)) == trim(std_name)) then
                  const_input_idx = n
                  exit
               end if
            end do
            call read_field(file, std_name, input_var_names(:,const_input_idx), 'lev', timestep, field_data_ptr(:,:,constituent_idx),                  &
                 mark_as_read=.false., error_on_not_found=.false., var_found=var_found)
         else
            ! If not in standard names list, then just use constituent name as input file name:
            call read_field(file, std_name, [std_name], 'lev', timestep, field_data_ptr(:,:,constituent_idx), mark_as_read=.false.,                    &
                 error_on_not_found=.false., var_found=var_found)
         end if
         if(.not. var_found) then
            constituent_has_default = .false.
            call const_props(constituent_idx)%has_default(constituent_has_default, constituent_errflg, constituent_errmsg)
            if (constituent_errflg /= 0) then
               call endrun(constituent_errmsg, file=__FILE__, line=__LINE__)
            end if
            if (.not. constituent_has_default) then
               ! Intialize to constituent's configured minimum value
               call const_props(constituent_idx)%minimum(constituent_min_value, constituent_errflg, constituent_errmsg)
               field_data_ptr(:,:,constituent_idx) = constituent_min_value
               if (masterproc) then
                  write(iulog,*) 'Constituent ', trim(std_name), ' default value not configured. Setting to min value of ', constituent_min_value
               end if
            end if
         end if
      end do

   end subroutine physics_read_data

   subroutine physics_check_data(file_name, suite_names, timestep, min_difference, min_relative_value, err_on_fail)
      use pio,                        only: file_desc_t, pio_nowrite
      use cam_abortutils,             only: endrun
      use shr_kind_mod,               only: SHR_KIND_CS, SHR_KIND_CL, SHR_KIND_CX
      use physics_data,               only: check_field, find_input_name_idx, no_exist_idx, init_mark_idx, prot_no_init_idx, const_idx
      use cam_ccpp_cap,               only: ccpp_physics_suite_variables, cam_advected_constituents_array, cam_model_const_properties
      use cam_constituents,           only: const_get_index
      use ccpp_kinds,                 only: kind_phys
      use cam_logfile,                only: iulog
      use spmd_utils,                 only: masterproc
      use phys_vars_init_check,       only: is_read_from_file
      use ioFileMod,                  only: cam_get_file
      use cam_pio_utils,              only: cam_pio_openfile, cam_pio_closefile
      use ccpp_constituent_prop_mod,  only: ccpp_constituent_prop_ptr_t
      use phys_vars_init_check_param, only: phys_var_num, phys_var_stdnames, input_var_names, std_name_len
      use physics_types_param,        only: theta

      ! Dummy arguments
      character(len=SHR_KIND_CL), intent(in) :: file_name
      character(len=SHR_KIND_CS), intent(in) :: suite_names(:) !Names of CCPP suites
      integer,                    intent(in) :: timestep
      real(kind_phys),            intent(in) :: min_difference
      real(kind_phys),            intent(in) :: min_relative_value
      logical,                    intent(in) :: err_on_fail

      ! Local variables:

      ! Character array containing all CCPP-required variable standard names:
      character(len=std_name_len), allocatable :: ccpp_required_data(:)

      ! Strings which store names of any missing or non-initialized vars:
      character(len=SHR_KIND_CL) :: missing_required_vars
      character(len=SHR_KIND_CL) :: protected_non_init_vars
      character(len=SHR_KIND_CL) :: missing_input_names

      character(len=SHR_KIND_CX) :: errmsg    !CCPP framework error message
      integer                    :: errflg    !CCPP framework error flag
      integer                    :: n         !Loop control variable
      integer                    :: name_idx  !Input variable array index
      integer                    :: constituent_idx !Index of variable in constituent array
      integer                    :: const_input_idx !input_var_names index for a consituent
      integer                    :: req_idx   !Required variable array index
      integer                    :: suite_idx !Suite array index
      character(len=SHR_KIND_CL) :: ncdata_check_loc
      type(file_desc_t), pointer :: file
      logical                    :: file_found
      logical                    :: is_first
      logical                    :: is_read
      logical                    :: diff_found
      logical                    :: overall_diff_found
      character(len=std_name_len) :: std_name       !Variable to hold constiutent standard name
      real(kind=kind_phys), pointer :: field_data_ptr(:,:,:)
      type(ccpp_constituent_prop_ptr_t), pointer :: const_props(:)

      ! Initalize missing and non-initialized variables strings:
      missing_required_vars = ' '
      protected_non_init_vars = ' '
      missing_input_names   = ' '
      nullify(file)
      is_first = .true.
      overall_diff_found = .false.

      if (masterproc) then
         write(iulog,*) ''
         write(iulog,*) '********** Physics Check Data Results **********'
         write(iulog,*) ''
         write(iulog,*) 'TIMESTEP: ', timestep
      end if
      if (file_name == 'UNSET') then
         write(iulog,*) 'WARNING: Namelist variable ncdata_check is UNSET.', ' Model will run, but physics check data will not be printed'
         return
      end if
      ! Open check file:
      call cam_get_file(file_name, ncdata_check_loc, allow_fail=.true., lexist=file_found, log_info=.false.)
      if (.not. file_found) then
         write(iulog,*) 'WARNING: Check file ', trim(file_name), ' not found. Model will run, but physics check data will not be printed'
         return
      end if
      allocate(file)
      call cam_pio_openfile(file, ncdata_check_loc, pio_nowrite, log_info=.false.)
      ! Loop over CCPP physics/chemistry suites:
      do suite_idx = 1, size(suite_names, 1)

         ! Search for all needed CCPP input variables, so that they can be read from input file if need be:
            call ccpp_physics_suite_variables(suite_names(suite_idx), ccpp_required_data, errmsg, errflg, input_vars=.false., output_vars=.true.)

         ! Loop over all required variables as specified by CCPP suite:
         do req_idx = 1, size(ccpp_required_data, 1)

            ! Find IC file input name array index for required variable:
            name_idx = find_input_name_idx(ccpp_required_data(req_idx), .true., constituent_idx)

            ! Check for special index values:
            select case (name_idx)

               case (const_idx)

                  ! If variable is a constituent, then do nothing. We'll handle these later.

               case (init_mark_idx)

                  ! If variable only has an initial_value but not read from file, then do nothing, even if it is modified by the physics scheme.
                  ! There is nothing we can check against.

               case (no_exist_idx)

                  ! If the index for an output variable was not found, then do nothing. We won't try to check these.

               case default

                  ! Check variable vs input check file:

                  select case (trim(phys_var_stdnames(name_idx)))
                  case ('potential_temperature')
                     call check_field(file, input_var_names(:,name_idx), 'lev', timestep, theta, 'potential_temperature', min_difference,              &
                          min_relative_value, is_first, diff_found)

                  end select !check variables
                  if (diff_found) then
                     overall_diff_found = .true.
                  end if
            end select !special indices

         end do !Suite-required variables

         ! Deallocate required variables array for use in next suite:
         deallocate(ccpp_required_data)

      end do !CCPP suites

      ! Check constituent variables
      field_data_ptr => cam_advected_constituents_array()
      const_props => cam_model_const_properties()

      do constituent_idx = 1, size(const_props)
         ! Check if constituent standard name in registered SIMA standard names list:
         call const_props(constituent_idx)%standard_name(std_name)
         if(any(phys_var_stdnames == std_name)) then
            ! Find array index to extract correct input names:
            do n=1, phys_var_num
               if(trim(phys_var_stdnames(n)) == trim(std_name)) then
                  const_input_idx = n
                  exit
               end if
            end do
            call check_field(file, input_var_names(:,const_input_idx), 'lev', timestep, field_data_ptr(:,:,constituent_idx), std_name,                 &
                 min_difference, min_relative_value, is_first, diff_found)
            if (diff_found) then
               overall_diff_found = .true.
            end if
         else
            ! If not in standard names list, then just use constituent name as input file name:
            call check_field(file, [std_name], 'lev', timestep, field_data_ptr(:,:,constituent_idx), std_name, min_difference, min_relative_value,     &
                 is_first, diff_found)
            if (diff_found) then
               overall_diff_found = .true.
            end if
         end if
      end do
      ! Close check file:
      call cam_pio_closefile(file)
      deallocate(file)
      nullify(file)
      if (is_first) then
         if (masterproc) then
            write(iulog,*) ''
            write(iulog,*) 'No differences found!'
         end if
      end if
      if (masterproc) then
         write(iulog,*) ''
         write(iulog,*) '********** End Physics Check Data Results **********'
         write(iulog,*) ''
      end if
      ! Endrun if differences were found on this timestep and err_on_fail=TRUE
      if (overall_diff_found .and. err_on_fail .and. masterproc) then
         call endrun('ERROR: Difference(s) found during ncdata check', file=__FILE__, line=__LINE__)
      end if
   end subroutine physics_check_data

end module physics_inputs_param
