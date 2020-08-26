module phys_vars_init_check_protect

   implicit none
   private

   !Total number of physics-related variables:
   integer, public, parameter :: phys_var_num = 3

   !Max length of physics-related variable standard names:
   integer, public, parameter :: std_name_len = 21

   !Max length of input (IC) file variable names:
   integer, public, parameter :: ic_name_len = 12

   !Array storing all physics-related variable standard names:
   character(len=21), public, protected :: phys_var_stdnames(phys_var_num) = (/ &
      'potential_temperature', &
      'sea_level_pressure   ', &
      'eddy_length_scale    ' /)

   !Array storing all registered IC file input names for each variable:
   character(len=12), public, protected :: input_var_names(2, phys_var_num) = reshape((/ &
      'theta       ', 'pot_temp    ', &
      'psl         ', 'sea_lev_pres', &
      'eddy_len    ', '            ' /), (/2, phys_var_num/))

   !Logical array to indicate whether or not variable is protected:
   logical, public, protected :: protected_vars(phys_var_num) = (/ &
      .true., &
      .false., &
      .false. /)

   !Logical array to indicate whether or not variable is initialized:
   logical, public, protected :: initialized_vars(phys_var_num) = (/ &
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

      character(len=*), intent(in) :: varname !Variable name being marked

      integer :: stdnam_idx !Standard name array index

      logical :: found_var = .false. !Logical which inidcates variable exists in array

      !Loop over standard name array:
      do stdnam_idx = 1, phys_var_num
         !Check if standard name matches provided variable name:
         if (trim(phys_var_stdnames(stdnam_idx)) == trim(varname)) then
            !If so, then set associated initialized_vars
            !array index to true:
            initialized_vars(stdnam_idx) = .true.

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
            !If so, then return initialized_vars
            !value associated with that index:
            is_initialized = initialized_vars(stdnam_idx)

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

end module phys_vars_init_check_protect
