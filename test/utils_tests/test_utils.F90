program test_utils


   use shr_kind_mod,       only: max_chars=>SHR_KIND_CX
   use shr_kind_mod,       only: max_flen=>SHR_KIND_CL
   use string_utils_tests, only: test_string_utils

   implicit none

   integer                           :: out_unit = 6
   integer                           :: ierr
   integer                           :: errcnt
   integer                           :: testcnt
   integer                           :: total_errcnt = 0
   integer                           :: total_tests = 0

   ! Test string utilities
   call test_string_utils(errcnt, testcnt)
   total_errcnt = total_errcnt + errcnt
   total_tests = total_tests + testcnt

   if (total_errcnt > 0) then
      write(6, '(a,i0,a)') 'FAIL, ', total_errcnt, ' errors found'
      STOP 1
   else
      write(6, '(a,i0,a)') "All ", total_tests, " utility tests passed!"
      STOP 0
   end if

end program test_utils
