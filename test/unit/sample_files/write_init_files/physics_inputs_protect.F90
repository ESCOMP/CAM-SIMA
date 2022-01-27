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
!! @brief Auto-generated Initial conditions source file, physics_inputs_protect.F90
!!
!
module physics_inputs_protect


   implicit none
   private


!! public interfaces
   public :: physics_read_data
   public :: physics_check_data

CONTAINS

   subroutine physics_read_data(file, suite_names, timestep, read_initialized_variables)
      use pio,                          only: file_desc_t
      use cam_abortutils,               only: endrun
      use shr_kind_mod,                 only: SHR_KIND_CS, SHR_KIND_CL, SHR_KIND_CX
      use physics_data,                 only: read_field, find_input_name_idx, no_exist_idx
      use physics_data,                 only: init_mark_idx, prot_no_init_idx
      use cam_ccpp_cap,                 only: ccpp_physics_suite_variables
      use phys_vars_init_check_protect, only: phys_var_stdnames, input_var_names, std_name_len
      use physics_types_protected,      only: slp, theta

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

      character(len=SHR_KIND_CX) :: errmsg    !CCPP framework error message
      integer                    :: errflg    !CCPP framework error flag
      integer                    :: name_idx  !Input variable array index
      integer                    :: req_idx   !Required variable array index
      integer                    :: suite_idx !Suite array index
      character(len=2)           :: sep  = '' !String separator used to print error messages
      character(len=2)           :: sep2 = '' !String separator used to print error messages
      character(len=2)           :: sep3 = '' !String separator used to print error messages

      ! Logical to default optional argument to False:
      logical                    :: use_init_variables

      ! Initalize missing and non-initialized variables strings:
      missing_required_vars = ' '
      protected_non_init_vars = ' '

      ! Initialize use_init_variables based on whether it was input to function:
      if (present(read_initialized_variables)) then
         use_init_variables = read_initialized_variables
      else
         use_init_variables = .false.
      end if

      ! Loop over CCPP physics/chemistry suites:
      do suite_idx = 1, size(suite_names, 1)

         ! Search for all needed CCPP input variables, so that they can be read from input file
         ! if need be:
            call ccpp_physics_suite_variables(suite_names(suite_idx), ccpp_required_data,         &
                 errmsg, errflg, input_vars=.true., output_vars=.false.)

         ! Loop over all required variables and read from file if uninitialized:
         do req_idx = 1, size(ccpp_required_data, 1)

            ! Find IC file input name array index for required variable:
            name_idx = find_input_name_idx(ccpp_required_data(req_idx), use_init_variables)

            ! Check for special index values:
            select case (name_idx)

               case (init_mark_idx)

                  ! If variable is already initialized, then do nothing.

               case (no_exist_idx)

                  ! If an index was never found, then save variable name and check the rest of
                  ! the variables, after which the model simulation will end:
                     missing_required_vars(len_trim(missing_required_vars)+1:) =                  &
                          trim(sep)//trim(ccpp_required_data(req_idx))

                  ! Update character separator to now include comma:
                  sep = ', '

               case (prot_no_init_idx)

                  ! If an index was found for a protected variable, but that variable was never
                  ! marked as initialized, then save the variable name and check the rest of the
                  ! variables, after which the model simulation will end:
                     protected_non_init_vars(len_trim(protected_non_init_vars)+1:) =              &
                          trim(sep2)//trim(ccpp_required_data(req_idx))

                  ! Update character separator to now include comma:
                  sep2 = ', '

               case default

                  ! Read variable from IC file:

                  select case (trim(phys_var_stdnames(name_idx)))
                     case ('potential_temperature')
                        call                                                                      &
                             endrun('Cannot read theta from file'//                               &
                             ', theta is a protected variable')

                     case ('air_pressure_at_sea_level')
                        call read_field(file, 'air_pressure_at_sea_level',                        &
                             input_var_names(:,name_idx), timestep, slp)

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

   end subroutine physics_read_data

   subroutine physics_check_data(file_name, suite_names, timestep, min_difference,                &
        min_relative_value)
      use pio,                          only: file_desc_t, pio_nowrite
      use cam_abortutils,               only: endrun
      use shr_kind_mod,                 only: SHR_KIND_CS, SHR_KIND_CL, SHR_KIND_CX
      use physics_data,                 only: check_field, find_input_name_idx, no_exist_idx
      use physics_data,                 only: init_mark_idx, prot_no_init_idx
      use cam_ccpp_cap,                 only: ccpp_physics_suite_variables
      use ccpp_kinds,                   only: kind_phys
      use cam_logfile,                  only: iulog
      use spmd_utils,                   only: masterproc
      use phys_vars_init_check,         only: is_read_from_file
      use ioFileMod,                    only: cam_get_file
      use cam_pio_utils,                only: cam_pio_openfile, cam_pio_closefile
      use phys_vars_init_check_protect, only: phys_var_stdnames, input_var_names, std_name_len
      use physics_types_protected,      only: slp, theta

      ! Dummy arguments
      character(len=SHR_KIND_CL), intent(in) :: file_name
      character(len=SHR_KIND_CS), intent(in) :: suite_names(:) !Names of CCPP suites
      integer,                    intent(in) :: timestep
      real(kind_phys),            intent(in) :: min_difference
      real(kind_phys),            intent(in) :: min_relative_value

      ! Local variables:

      ! Character array containing all CCPP-required variable standard names:
      character(len=std_name_len), allocatable :: ccpp_required_data(:)

      ! Strings which store names of any missing or non-initialized vars:
      character(len=SHR_KIND_CL) :: missing_required_vars
      character(len=SHR_KIND_CL) :: protected_non_init_vars
      character(len=SHR_KIND_CL) :: missing_input_names

      character(len=SHR_KIND_CX) :: errmsg    !CCPP framework error message
      integer                    :: errflg    !CCPP framework error flag
      integer                    :: name_idx  !Input variable array index
      integer                    :: req_idx   !Required variable array index
      integer                    :: suite_idx !Suite array index
      character(len=SHR_KIND_CL) :: ncdata_check_loc
      type(file_desc_t), pointer :: file
      logical                    :: file_found
      logical                    :: is_first

      ! Initalize missing and non-initialized variables strings:
      missing_required_vars = ' '
      protected_non_init_vars = ' '
      missing_input_names   = ' '
      nullify(file)
      is_first = .true.

      if (masterproc) then
         write(iulog,*) ''
         write(iulog,*) '********** Physics Check Data Results **********'
         write(iulog,*) ''
         write(iulog,*) 'TIMESTEP: ', timestep
      end if
      if (file_name == 'UNSET') then
         write(iulog,*) 'WARNING: Namelist variable ncdata_check is UNSET.',                      &
              ' Model will run, but physics check data will not be printed'
         return
      end if
      ! Open check file:
      call cam_get_file(file_name, ncdata_check_loc, allow_fail=.true., lexist=file_found,        &
           log_info=.false.)
      if (.not. file_found) then
         write(iulog,*) 'WARNING: Check file ', trim(file_name),                                  &
              ' not found. Model will run, but physics check data will not be printed'
         return
      end if
      allocate(file)
      call cam_pio_openfile(file, ncdata_check_loc, pio_nowrite, log_info=.false.)
      ! Loop over CCPP physics/chemistry suites:
      do suite_idx = 1, size(suite_names, 1)

         ! Search for all needed CCPP input variables, so that they can be read from input file
         ! if need be:
            call ccpp_physics_suite_variables(suite_names(suite_idx), ccpp_required_data,         &
                 errmsg, errflg, input_vars=.true., output_vars=.false.)

         ! Loop over all required variables as specified by CCPP suite:
         do req_idx = 1, size(ccpp_required_data, 1)

            ! Find IC file input name array index for required variable:
            if (.not. is_read_from_file(ccpp_required_data(req_idx), name_idx)) then
               continue
            end if
            ! Check variable vs input check file:

            select case (trim(phys_var_stdnames(name_idx)))
               case ('potential_temperature')
                  call check_field(file, input_var_names(:,name_idx), 'lev', timestep, theta,     &
                       'potential_temperature', min_difference, min_relative_value, is_first)

               case ('air_pressure_at_sea_level')
                  call check_field(file, input_var_names(:,name_idx), timestep, slp,              &
                       'air_pressure_at_sea_level', min_difference, min_relative_value, is_first)

            end select !check variables
         end do !Suite-required variables

         ! Deallocate required variables array for use in next suite:
         deallocate(ccpp_required_data)

      end do !CCPP suites

      ! Close check file:
      call cam_pio_closefile(file)
      deallocate(file)
      nullify(file)
      if (is_first) then
         write(iulog,*) ''
         write(iulog,*) 'No differences found!'
      end if
      write(iulog,*) ''
      write(iulog,*) '********** End Physics Check Data Results **********'
      write(iulog,*) ''
   end subroutine physics_check_data

end module physics_inputs_protect
