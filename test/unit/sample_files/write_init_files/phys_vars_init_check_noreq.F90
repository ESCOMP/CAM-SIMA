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
!! @brief Auto-generated Initialization-checking source file
!!
!
module phys_vars_init_check_noreq


   implicit none
   private

   !Total number of physics-related variables:
   integer, public, parameter :: phys_var_num = 3

   !Max length of physics-related variable standard names:
   integer, public, parameter :: std_name_len = 25

   !Max length of input (IC) file variable names:
   integer, public, parameter :: ic_name_len = 12

   !Parameterized initialized_vars options - order matters
   integer, public, parameter ::  UNINITIALIZED = 0
   integer, public, parameter ::    INITIALIZED = 1
   integer, public, parameter ::          PARAM = 2
   integer, public, parameter :: READ_FROM_FILE = 3

   !Array storing all physics-related variable standard names:
   character(len=25), public, protected :: phys_var_stdnames(phys_var_num) = (/ &
      'potential_temperature    ', &
      'air_pressure_at_sea_level', &
      'eddy_length_scale        ' /)

   !Array storing all registered IC file input names for each variable:
   character(len=12), public, protected :: input_var_names(2, phys_var_num) = reshape((/ &
      'theta       ', 'pot_temp    ', &
      'slp         ', 'sea_lev_pres', &
      'eddy_len    ', '            ' /), (/2, phys_var_num/))

   !Logical array to indicate whether or not variable is protected:
   logical, public, protected :: protected_vars(phys_var_num) = (/ &
      .false., &
      .false., &
      .false. /)

   !array to indicate: variable is UNINITIALIZED, INTIIALIZED, PARAM or READ_FROM_FILE:
   integer, public, protected :: initialized_vars(phys_var_num) = (/ &
      UNINITIALIZED, &
      UNINITIALIZED, &
      UNINITIALIZED /)

!! public interfaces
   public :: mark_as_initialized
   public :: mark_as_read_from_file
   public :: is_initialized
   public :: is_read_from_file

CONTAINS

   subroutine mark_as_initialized(varname)

      !This subroutine  marks the variable as
      !INITIALIZED in the `initialized_vars` array,
      !which means any initialization check should
      !now return True.

      use cam_abortutils, only: endrun

      character(len=*), intent(in) :: varname !Variable name being marked

      integer :: stdnam_idx !Standard name array index

      logical :: found_var !Logical which indicates variable exists in array

      found_var = .false.
      !Loop over standard name array:
      do stdnam_idx = 1, phys_var_num
         !Check if standard name matches provided variable name:
         if (trim(phys_var_stdnames(stdnam_idx)) == trim(varname)) then
            !Only set to INITIALIZED if not already PARAM or READ_FROM_FILE
            if (initialized_vars(stdnam_idx) < PARAM) then
               !If so, then set associated initialized_vars
               !array index to INITIALIZED:
               initialized_vars(stdnam_idx) = INITIALIZED
            end if

            !Indicate variable has been found:
            found_var = .true.
            exit ! Exit loop
         end if
      end do

      if (.not. found_var) then
         !If loop has completed with no matches, then endrun with warning
         !that variable didn't exist in standard names array:
         call endrun("Variable '"//trim(varname)//"' is missing from phys_var_stdnames array.")
      end if

   end subroutine mark_as_initialized


   subroutine mark_as_read_from_file(varname)

      !This subroutine marks the varible as
      !READ_FROM_FILE in the initialized_vars array

      use cam_abortutils, only: endrun

      character(len=*), intent(in) :: varname !Variable name being marked

      integer :: stdnam_idx !Standard name array index

      logical :: found_var !Logical which indicates variable exists in array

      found_var = .false.
      !Loop over input name array:
      do stdnam_idx = 1, phys_var_num
         !Check if input variable name matches provided variable name:
         if (trim(phys_var_stdnames(stdnam_idx)) == trim(varname)) then
            !Check if initialized_vars at that index has already been set to PARAM
            if (initialized_vars(stdnam_idx) == PARAM) then
               !If so, call endrun because that should not happen
               call                                                                               &
                    endrun("Variable '"//trim(varname)//                                          &
                    "' was read from file, but was a parameter")
            end if
            !Otherwise, set associated initialized_vars
            !array index to READ_FROM_FILE:
            initialized_vars(stdnam_idx) = READ_FROM_FILE

            !Indicate variable has been found:
            found_var = .true.
            exit ! Exit loop
         end if
      end do

      if (.not. found_var) then
         !If loop has completed with no matches, then endrun with warning
         !that variable didn't exist in standard names array:
         call endrun("Variable '"//trim(varname)//"' is missing from phys_var_stdnames array.")
      end if

   end subroutine mark_as_read_from_file


   logical function is_initialized(varname)

      !This function checks if the variable is
      !already initialized according to the
      !`initialized_vars` array.

      use cam_abortutils, only: endrun


      character(len=*), intent(in) :: varname !Variable name being checked
      character(len=*), parameter  :: subname = 'is_initialized: '

      integer :: stdnam_idx !standard name array index
      logical :: found      !check that <varname> was found

      is_initialized = .false.
      found = .false.

      !Loop over standard name array:
      do stdnam_idx = 1, phys_var_num
         !Check if standard name matches provided variable name:
         if (trim(phys_var_stdnames(stdnam_idx)) == trim(varname)) then
            !If so, then return True if PARAM, INITIALIZED, OR READ_FROM_FILE
            is_initialized = (initialized_vars(stdnam_idx) > UNINITIALIZED)
            found = .true.
            exit ! Exit loop
         end if
      end do

      if (.not. found) then
         !If loop has completed with no matches, then endrun with warning
         !that variable didn't exist in standard names array:
         call                                                                                     &
              endrun(subname//"Variable '"//trim(varname)//                                       &
              "' is missing from phys_var_stdnames array.")
      end if

   end function is_initialized


   logical function is_read_from_file(varname, stdnam_idx_out)

      !This function checks if the variable is
      !read from file according to the
      !`initialized_vars` array.

      use cam_abortutils, only: endrun


      character(len=*), intent(in)   :: varname !Variable name being checked
      integer, optional, intent(out) :: stdnam_idx_out

      character(len=*), parameter    :: subname = 'is_read_from_file: '
      integer                        :: stdnam_idx !standard name array index
      logical                        :: found      !check that <varname> was found

      is_read_from_file = .false.
      found = .false.

      !Loop over standard name array:
      do stdnam_idx = 1, phys_var_num
         !Check if standard name matches provided variable name:
         if (trim(phys_var_stdnames(stdnam_idx)) == trim(varname)) then
            !If so, then return True if READ_FROM_FILE:
            is_read_from_file = (initialized_vars(stdnam_idx) == READ_FROM_FILE)
            !Mark as found:
            found = .true.
            exit ! Exit loop
         end if
      end do

      if (.not. found) then
         !If loop has completed with no matches, then endrun with warning
         !that variable didn't exist in standard names array:
         call                                                                                     &
              endrun(subname//"Variable '"//trim(varname)//                                       &
              "' is missing from phys_var_stdnames array.")
      end if
      if (present(stdnam_idx_out)) then
         stdnam_idx_out = stdnam_idx
      end if

   end function is_read_from_file

end module phys_vars_init_check_noreq
