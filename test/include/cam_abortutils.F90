module cam_abortutils

   use shr_kind_mod, only: max_chars=>SHR_KIND_CX

   implicit none
   private

   public :: endrun
   public :: check_endrun
   public :: check_allocate

   character(len=max_chars) :: abort_msg = ''

CONTAINS

   logical function check_endrun(test_desc, output)
      character(len=*), optional, intent(in) :: test_desc
      integer,          optional, intent(in) :: output

      ! Return .true. if an endrun message has been created
      check_endrun = len_trim(abort_msg) > 0
      if (check_endrun .and. present(output)) then
         ! Output the endrun message to <output>
         if (output > 0) then
            if (present(test_desc)) then
               write(output, *) "FAIL: ", trim(test_desc)
            end if
            write(output, *) trim(abort_msg)
         end if
      end if
      ! Always clear the endrun message
      abort_msg = ''
   end function check_endrun

   subroutine endrun(message, file, line)
      ! Dummy arguments
      character(len=*),           intent(in) :: message
      character(len=*), optional, intent(in) :: file
      integer,          optional, intent(in) :: line

      if (present(file) .and. present(line)) then
         write(abort_msg, '(4a,i0)') trim(message), ' at ', trim(file), ':', line
      else if (present(file)) then
         write(abort_msg, '(3a)') trim(message), ' at ', trim(file)
      else if (present(line)) then
         write(abort_msg, '(2a,i0)') trim(message), ' on line ', line
      else
         write(abort_msg, '(a)') trim(message)
      end if

  end subroutine endrun

   subroutine check_allocate(errcode, subname, fieldname, errmsg, file, line)
      ! If <errcode> is not zero, call endrun with an error message

      ! Dummy arguments
      integer,                    intent(in) :: errcode
      character(len=*),           intent(in) :: subname
      character(len=*),           intent(in) :: fieldname
      character(len=*), optional, intent(in) :: errmsg
      character(len=*), optional, intent(in) :: file
      integer,          optional, intent(in) :: line
      ! Local variable
      character(len=max_chars) :: abort_msg

      if (errcode /= 0) then
         if (present(errmsg)) then
            write(abort_msg, '(6a)') trim(subname), ": Allocate of '",        &
                 trim(fieldname), "' failed; '", trim(errmsg), "'"
         else
            write(abort_msg, '(4a,i0)') trim(subname), ": Allocate of '",     &
                 trim(fieldname), "' failed with code ", errcode
         end if
         call endrun(abort_msg, file=file, line=line)
      end if

   end subroutine check_allocate

end module cam_abortutils
