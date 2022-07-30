module string_utils_tests

   use shr_kind_mod,         only: max_chars=>SHR_KIND_CX
   use shr_kind_mod,         only: max_flen=>SHR_KIND_CL
   use shr_kind_mod,         only: cs=>SHR_KIND_CS

   implicit none
   private

   public test_string_utils

CONTAINS

   subroutine test_string_utils(errcnt, testcnt)
      use string_utils, only: parse_multiplier
      ! Dummy arguments
      integer, intent(out) :: errcnt
      integer, intent(out) :: testcnt
      ! Local variables
      integer                 :: multiplier
      character(len=cs)       :: token
      character(len=max_flen) :: errmsg
      character(len=*), parameter :: subname = 'test_string_utils: '

      errcnt = 0
      testcnt = 0
      ! Test normal case
      call parse_multiplier("9*nstep", multiplier, token, errmsg=errmsg)
      testcnt = testcnt + 1
      if ((multiplier /= 9) .or. (trim(token) /= "nstep")) then
         write(6, *) subname, trim(errmsg)
         errcnt = errcnt + 1
      end if
      ! Test default count
      call parse_multiplier("nstep", multiplier, token, errmsg=errmsg)
      testcnt = testcnt + 1
      if ((multiplier /= 1) .or. (trim(token) /= "nstep")) then
         write(6, *) subname, trim(errmsg)
         errcnt = errcnt + 1
      end if
      ! Test bad multiplier
      call parse_multiplier("9a*nstep", multiplier, token, errmsg=errmsg)
      testcnt = testcnt + 1
      if ((multiplier /= -1) .or. (len_trim(token) > 0)) then
         if (multiplier /= -1) then
            write(6, '(2a,i0,a)') subname, "multiplier = ", multiplier,       &
                 ", should be -1"
         end if
         if (len_trim(token) > 0) then
            write(6, *) subname, "token = '", trim(token), "', should be empty"
         end if
         errcnt = errcnt + 1
      else if (adjustl(trim(errmsg)) /=                                       &
           "Invalid multiplier, '9a' in '9a*nstep'") then
         write(6, *) subname, "!", trim(errmsg), "!"
         errcnt = errcnt + 1
      end if
      ! Test empty string
      call parse_multiplier("", multiplier, token, errmsg=errmsg)
      testcnt = testcnt + 1
      if ((multiplier /= 0) .or. (trim(token) /= "")) then
         write(6, *) subname, trim(errmsg)
         errcnt = errcnt + 1
      end if
      ! Test member of allowed set
      call parse_multiplier("9*nstep", multiplier, token, errmsg=errmsg,      &
           allowed_set = (/ 'nhour ', 'nhours', 'nstep ', 'nsteps' /))
      testcnt = testcnt + 1
      if ((multiplier /= 9) .or. (trim(token) /= "nstep")) then
         write(6, *) subname, trim(errmsg)
         errcnt = errcnt + 1
      end if
      ! Test not member of allowed set
      call parse_multiplier("9*step", multiplier, token, errmsg=errmsg,       &
           allowed_set = (/ 'nhour ', 'nstep ', 'nsteps' /))
      testcnt = testcnt + 1
      if ((multiplier /= -1) .or. (trim(token) /= "")) then
         write(6, *) subname, trim(errmsg)
         errcnt = errcnt + 1
      else if (adjustl(trim(errmsg)) /=                                       &
           "Error, token, 'step' not in (/ 'nhour', 'nstep', 'nsteps' /)") then
         write(6, *) subname, "!", trim(errmsg), "!"
         errcnt = errcnt + 1
      end if

   end subroutine test_string_utils

end module string_utils_tests
