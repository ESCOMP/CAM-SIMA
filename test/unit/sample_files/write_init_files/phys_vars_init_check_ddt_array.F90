module phys_vars_init_check_ddt_array

   implicit none
   private

   !Total number of physics-related variables:
   integer, public, parameter :: phys_var_num = 4

   !Max length of physics-related variable standard names:
   integer, public, parameter :: std_name_len = 30

   !Max length of input (IC) file variable names:
   integer, public, parameter :: ic_name_len = 12

   !Parameterized initialized_vars options - order matters
   integer, public, parameter ::  UNINITIALIZED = 0
   integer, public, parameter ::    INITIALIZED = 1
   integer, public, parameter ::      PARAMETER = 2
   integer, public, parameter :: READ_FROM_FILE = 3

   !Array storing all physics-related variable standard names:
   character(len=30), public, protected :: phys_var_stdnames(phys_var_num) = (/ &
      'index_of_potential_temperature', &
      'eddy_length_scale             ', &
      'sea_level_pressure            ', &
      'potential_temperature         ' /)

   !Array storing all registered IC file input names for each variable:
   character(len=12), public, protected :: input_var_names(2, phys_var_num) = reshape((/ &
      'ix_theta    ', '            ', &
      'eddy_len    ', '            ', &
      'slp         ', 'sea_lev_pres', &
      'theta       ', 'pot_temp    ' /), (/2, phys_var_num/))

   !Logical array to indicate whether or not variable is protected:
   logical, public, protected :: protected_vars(phys_var_num) = (/ &
      .false., &
      .false., &
      .false., &
      .false. /)

   !array to indicate: variable is UNINITIALIZED, INTIIALIZED, PARAMETER or READ_FROM_FILE:
   integer, public, protected :: initialized_vars(phys_var_num) = (/ &
      UNINITIALIZED, &
      UNINITIALIZED, &
      UNINITIALIZED, &
      UNINITIALIZED /)

!! public interfaces
   public :: mark_as_initialized
   public :: mark_as_read_from_file
   public :: is_initialized

CONTAINS

   subroutine mark_as_initialized(varname)

      !This subroutine  marks the variable as
      !INITIALIZED in the `initialized_vars` array,
      !which means any initialization check should now
      !now return True.

      use cam_abortutils, only: endrun

      character(len=*), intent(in) :: varname !Variable name being marked

      integer :: stdnam_idx !Standard name array index

      logical :: found_var = .false. !Logical which inidcates variable exists in array

      !Loop over standard name array:
      do stdnam_idx = 1, phys_var_num
         !Check if standard name matches provided variable name:
         if (trim(phys_var_stdnames(stdnam_idx)) == trim(varname)) then
            !Only set to INITIALIZED if not already PARAMETER or READ_FROM_FILE
            if (initialized_vars(stdnam_idx) < PARAMETER) then
               !If so, then set associated initialized_vars
               !array index to INITIALIZED:
               initialized_vars(stdnam_idx) = INITIALIZED
            end if

            !Indicate variable has been found:
            found_var = .true.

            !Exit loop:
            exit
         end if
      end do

      if (.not.found_var) then
         !If loop has completed with no matches, then endrun with warning
         !that variable didn't exist in standard names array:
         call endrun(&
         "Variable '"//trim(varname)//"' is missing from phys_var_stdnames array.")
      end if

   end subroutine mark_as_initialized


   subroutine mark_as_read_from_file(varname)

      !This subroutine marks the varible as
      !READ_FROM_FILE in the initialized_vars array

      use cam_abortutils, only: endrun

      character(len=*), intent(in) :: varname !Variable name being marked

      integer :: stdnam_idx !Standard name array index

      logical :: found_var = .false. !Logical which indicates variable exists in array

      !Loop over input name array:
      do stdnam_idx = 1, phys_var_num
         !Check if standard name matches provided variable name:
         if (trim(input_var_names(1,stdnam_idx)) ==                                               &
              trim(varname).or.trim(input_var_names(2,stdnam_idx)) == trim(varname)) then
            !If so, then set associated initialized_vars
            !array index to READ_FROM_FILE:
            initialized_vars(stdnam_idx) = READ_FROM_FILE

            !Indicate variable has been found:
            found_var = .true.

            !Exit loop:
            exit
         end if
      end do

      if (.not.found_var) then
         !If loop has completed with no matches, then endrun with warning
         !that variable didn't exist in input names array:
         call endrun(&
         "Variable '"//trim(varname)//"' is missing from input_var_names array.")
      end if

   end subroutine mark_as_read_from_file


   logical function is_initialized(varname)

      !This function checks if the variable is
      !already initialized according to the
      !`initialized_vars` array.

      use cam_abortutils, only: endrun


      character(len=*), intent(in) :: varname !Variable name being checked

      integer :: stdnam_idx !standard name array index

      is_initialized = .false.

      !Loop over standard name array:
      do stdnam_idx = 1, phys_var_num
         !Check if standard name matches provided variable name:
         if (trim(phys_var_stdnames(stdnam_idx)) == trim(varname)) then
            !If so, then return True if PARAMETER, INITIALIZED, OR READ_FROM_FILE
            is_initialized = (initialized_vars(stdnam_idx) > UNINITIALIZED)

            !Exit loop:
            exit
         end if
      end do

      if (.not.is_initialized) then
         !If loop has completed with no matches, then endrun with warning
         !that variable didn't exist in standard names array:
         call endrun(&
         "Variable '"//trim(varname)//"' is missing from phys_var_stdnames array.")
      end if

   end function is_initialized



end module phys_vars_init_check_ddt_array
