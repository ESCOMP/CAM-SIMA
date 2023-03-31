module cam_abortutils

   use shr_sys_mod,  only: shr_sys_abort, shr_sys_flush
   use shr_kind_mod, only: max_chars=>shr_kind_cx, msg_len=>SHR_KIND_CS
   use shr_kind_mod, only: r8 => shr_kind_r8
   use shr_mem_mod,  only: shr_mem_getusage
   use pio,          only: file_desc_t
   use cam_logfile,  only: iulog

   implicit none
   private
   save

   public :: endrun
   public :: check_allocate
   public :: check_endrun ! Stub needed for testing

CONTAINS

   subroutine check_allocate(errcode, subname, fieldname, file, line)
      ! If <errcode> is not zero, call endrun with an error message

      ! Dummy arguments
      integer,                    intent(in) :: errcode
      character(len=*),           intent(in) :: subname
      character(len=*),           intent(in) :: fieldname
      character(len=*), optional, intent(in) :: file
      integer,          optional, intent(in) :: line
      ! Local variables
      character(len=max_chars) :: abort_msg
      real(r8)                 :: mem_val, mem_hw_val

      if (errcode /= 0) then
         ! Get memory values
         call shr_mem_getusage(mem_hw_val, mem_val)

         ! Write error message with memory stats
         write(abort_msg, '(4a,i0,a,f10.2,a,f10.2,a)')                        &
              trim(subname), ": Allocate of '",                               &
              trim(fieldname), "' failed with code ", errcode,                &
              ". Memory highwater is ", mem_hw_val,                           &
              " mb, current memory usage is ", mem_val, " mb"

         ! End the simulation
         call endrun(abort_msg, file=file, line=line)
      end if

   end subroutine check_allocate

   subroutine endrun(message, file, line)
      ! Parallel emergency stop
      ! Dummy arguments
      character(len=*),           intent(in) :: message
      character(len=*), optional, intent(in) :: file
      integer,          optional, intent(in) :: line
      ! Local variables
      character(len=max_chars)               :: abort_msg
      if (present(file) .and. present(line)) then
         write(abort_msg, '(4a,i0)') trim(message),' at ',trim(file),':',line
      else if (present(file)) then
         write(abort_msg, '(3a)') trim(message),' at ',trim(file)
      else if (present(line)) then
         write(abort_msg, '(2a,i0)') trim(message),' on line ',line
      else
         write(abort_msg, '(a)') trim(message)
      end if
      call shr_sys_abort(abort_msg)

   end subroutine endrun

   logical function check_endrun(test_desc, output)
      character(len=*), optional, intent(in) :: test_desc
      integer,          optional, intent(in) :: output

      ! Return .true. if an endrun message has been created
      ! Stub, always return .false.
      check_endrun = .false.

   end function check_endrun

end module cam_abortutils
