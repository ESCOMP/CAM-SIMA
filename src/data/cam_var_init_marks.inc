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
