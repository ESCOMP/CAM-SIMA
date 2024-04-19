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
module phys_vars_init_check_mf


   implicit none
   private

!! public interfaces
   public :: mark_as_initialized
   public :: mark_as_read_from_file
   public :: is_initialized
   public :: is_read_from_file

!! Parameterized initialized_vars options - order matters
   integer, public, parameter ::  UNINITIALIZED = 0
   integer, public, parameter ::    INITIALIZED = 1
   integer, public, parameter ::          PARAM = 2
   integer, public, parameter :: READ_FROM_FILE = 3
   ! Total number of physics-related variables:
   integer, public, parameter :: phys_var_num = 2
   integer, public, parameter :: phys_const_num = 15

   !Max length of physics-related variable standard names:
   integer, public, parameter :: std_name_len = 25

   ! Max length of input (IC) file variable names:
   integer, public, parameter :: ic_name_len = 5

   ! Physics-related input variable standard names:
   character(len=25), public, protected :: phys_var_stdnames(phys_var_num) = (/ &
      'potential_temperature    ', &
      'air_pressure_at_sea_level' /)

   character(len=36), public, protected :: phys_const_stdnames(phys_const_num) = (/ &
      "ccpp_constituent_minimum_values     ", &
      "ccpp_constituent_properties         ", &
      "ccpp_constituents                   ", &
      "ccpp_error_code                     ", &
      "ccpp_error_message                  ", &
      "do_log_output                       ", &
      "log_output_unit                     ", &
      "mpi_communicator                    ", &
      "mpi_rank                            ", &
      "mpi_root                            ", &
      "number_of_ccpp_advected_constituents", &
      "number_of_ccpp_constituents         ", &
      "number_of_mpi_tasks                 ", &
      "suite_name                          ", &
      "suite_part                          " /)
   !Array storing all registered IC file input names for each variable:
   character(len=5), public, protected :: input_var_names(1, phys_var_num) = reshape((/ &
      'theta', &
      'slp  ' /), (/1, phys_var_num/))

   ! Array indicating whether or not variable is protected:
   logical, public, protected :: protected_vars(phys_var_num)= (/ &
      .false., &
      .false. /)

   ! Variable state (UNINITIALIZED, INTIIALIZED, PARAM or READ_FROM_FILE):
   integer, public, protected :: initialized_vars(phys_var_num)= (/ &
      UNINITIALIZED, &
      UNINITIALIZED /)


CONTAINS

   subroutine mark_as_initialized(varname)

      ! This subroutine marks the variable, <varname>, as
      ! INITIALIZED in the `initialized_vars` array,
      ! which means any initialization check should
      ! now return True.

      ! Dummy argument
      character(len=*), intent(in) :: varname    !Variable name being marked

      ! Local variable
      integer                      :: stdnam_idx !Standard name array index

      ! Search for <varname> in the standard name array:
      do stdnam_idx = 1, phys_var_num
         if (trim(phys_var_stdnames(stdnam_idx)) == trim(varname)) then
            ! Only set to INITIALIZED if state is UNINITIALIZED
            if (initialized_vars(stdnam_idx) < PARAM) then
               initialized_vars(stdnam_idx) = INITIALIZED
            end if
            exit ! Exit loop once variable has been found and initialized
         end if
      end do

      ! No match is not an error because <phys_var_stdnames> only
      !    contains variables required by a physics suite.

   end subroutine mark_as_initialized

   subroutine mark_as_read_from_file(varname)

      ! This subroutine marks the varible, <varname>, as READ_FROM_FILE in the
      !    initialized_vars array

      use cam_abortutils, only: endrun

      ! Dummy argument
      character(len=*), intent(in) :: varname    ! Variable name being marked

      ! Local variables
      integer                     :: stdnam_idx ! Standard name array index
      logical                     :: found_var  ! .true. if <varname> is in arr.
      character(len=*), parameter :: subname = 'mark_as_read_from_file'

      found_var = .false.
      ! Set variable to READ_FROM_FILE:
      do stdnam_idx = 1, phys_var_num
         if (trim(phys_var_stdnames(stdnam_idx)) == trim(varname)) then
            ! It is an error if the variable has already been set to PARAM
            if (initialized_vars(stdnam_idx) == PARAM) then
               call endrun("Variable '"//trim(varname)//                      &
                    "' was read from file, but is a parameter")
            end if
            initialized_vars(stdnam_idx) = READ_FROM_FILE

            ! Indicate variable has been found:
            found_var = .true.
            exit ! Exit loop once variable has been found and marked
         end if
      end do

      if (.not. found_var) then
         ! This condition is an internal error, it should not happen
         call endrun(subname//": Variable '"//trim(varname)//                 &
              "' is missing from phys_var_stdnames array.")
      end if

   end subroutine mark_as_read_from_file

   logical function is_initialized(varname)

      ! This function checks if the variable, <varname>, is already
      !    initialized according to the 'initialized_vars' array.

      use cam_abortutils, only: endrun

      ! Dummy argument
      character(len=*), intent(in) :: varname ! Variable name being checked

      ! Local variables
      integer                     :: stdnam_idx ! Standard name array index
      logical                     :: found      ! Check that <varname> was found
      character(len=*), parameter :: subname = 'is_initialized: '

      is_initialized = .false.
      found = .false.

      ! Check if variable is initialized (PARAM, INITIALIZED, or READ_FROM_FILE)
      do stdnam_idx = 1, phys_var_num
         if (trim(phys_var_stdnames(stdnam_idx)) == trim(varname)) then
            is_initialized = (initialized_vars(stdnam_idx) > UNINITIALIZED)
            found = .true.
            exit ! Exit loop once variable has been found and checked
         end if
      end do

      if (.not. found) then
         ! This condition is an internal error, it should not happen
         call endrun(subname//": Variable '"//trim(varname)//                 &
              "' is missing from phys_var_stdnames array.")
      end if

   end function is_initialized

   subroutine is_read_from_file(varname, is_read, stdnam_idx_out)

      ! This subroutine checks if the variable, <varname>, is read from
      !    file according to the 'initialized_vars' array.

      use cam_abortutils, only: endrun

      ! Dummy arguments
      character(len=*),  intent(in)  :: varname ! Variable name being checked
      logical,           intent(out) :: is_read ! Set to .true. if from file
      integer, optional, intent(out) :: stdnam_idx_out

      ! Local variables

      integer                     :: stdnam_idx ! Standard name array index
      logical                     :: found      ! Check that <varname> was found
      character(len=*), parameter :: subname = 'is_read_from_file: '

      is_read = .false.
      found = .false.

      ! Return .true. if the variable's status is READ_FROM_FILE:
      do stdnam_idx = 1, phys_var_num
         if (trim(phys_var_stdnames(stdnam_idx)) == trim(varname)) then
            is_read = (initialized_vars(stdnam_idx) == READ_FROM_FILE)
            ! Mark as found:
            found = .true.
            exit ! Exit loop once variable has been found and checked
         end if
      end do

      if (.not. found) then
         ! Check to see if this is an internally-protected variable
         do stdnam_idx = 1, phys_const_num
            if (trim(phys_const_stdnames(stdnam_idx)) == trim(varname)) then
               found = .true.
               exit ! Exit loop once variable has been found
            end if
         end do
      end if

      if (.not. found) then
         ! This condition is an internal error, it should not happen
         call endrun(subname//": Variable '"//trim(varname)//                 &
              "' is missing from phys_var_stdnames array.")
      end if
      if (present(stdnam_idx_out)) then
         stdnam_idx_out = stdnam_idx
      end if

   end subroutine is_read_from_file

end module phys_vars_init_check_mf
