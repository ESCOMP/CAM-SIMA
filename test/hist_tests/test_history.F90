module test_hist_mod

   implicit none
   private

   public :: run_test

CONTAINS

   subroutine run_test(test_msg, test_file, sample_dir, out_unit,              &
        num_configs, volumes, max_frames, precisions, &
        test_cnt, err_cnt)
      use shr_kind_mod,         only: max_flen=>SHR_KIND_CL
      use cam_abortutils,       only: endrun, check_endrun
      use cam_hist_file, only: hist_file_t
      use cam_hist_file, only: hist_read_namelist_config

      ! Dummy arguments
      character(len=*), intent(in)  :: test_msg
      character(len=*), intent(in)  :: test_file
      character(len=*), intent(in)  :: sample_dir
      integer,          intent(in)  :: num_configs
      character(len=*), intent(in)  :: volumes(:)
      integer,          intent(in)  :: max_frames(:)
      character(len=*), intent(in)  :: precisions(:)
      integer,          intent(in)  :: out_unit
      integer,          intent(out) :: test_cnt
      integer,          intent(out) :: err_cnt
      ! Local variables
      type(hist_file_t), pointer :: tconfig_arr(:)
      character(len=max_flen)    :: test_path
      integer                    :: indx

      test_cnt = 0
      err_cnt = 0
      test_cnt = test_cnt + 1 ! Did read work?
      test_path = trim(sample_dir)//trim(test_file)
      tconfig_arr => hist_read_namelist_config(test_path)
      if (check_endrun(test_desc=test_msg, output=out_unit)) then
         err_cnt = err_cnt + 1
      end if
      if (err_cnt == 0) then
         test_cnt = test_cnt + 1 ! Did the config array get allocated?
         if (.not. associated(tconfig_arr)) then
            err_cnt = err_cnt + 1
            write(out_unit, *) "FAIL: ", trim(test_msg),                      &
                 ": tconfig_arr not allocated"
         end if
      end if
      if (err_cnt == 0) then
         test_cnt = test_cnt + 1 ! Is the config array the right size?
         if (size(tconfig_arr) /= num_configs) then
            err_cnt = err_cnt + 1
            write(out_unit, '(3a,i0,a,i0)') "FAIL: ", trim(test_msg),         &
                 ": tconfig_arr has ", size(tconfig_arr),                     &
                 " entries, should be ", num_configs
         end if
         do indx = 1, num_configs
            test_cnt = test_cnt + 1 ! Is volume correct?
            if (trim(tconfig_arr(indx)%filename())  /= trim(volumes(indx))) then
               err_cnt = err_cnt + 1
               write(out_unit, '(3a,i0,5a)') "FAIL: ", trim(test_msg),        &
                    ": volume(", indx, ") is '",                              &
                    trim(tconfig_arr(indx)%filename()), "', should be '",     &
                    trim(volumes(indx)), "'"
            end if
            test_cnt = test_cnt + 1 ! Is max_frames correct?
            if (tconfig_arr(indx)%max_frame() /= max_frames(indx)) then
               err_cnt = err_cnt + 1
               write(out_unit, '(3a,i0,a,i0)') "FAIL: ", trim(test_msg),      &
                    ": tconfig_arr has max_frames = ",                        &
                    tconfig_arr(indx)%max_frame(), ", should be ",            &
                    max_frames(indx)
            end if
            test_cnt = test_cnt + 1 ! Is precision correct?
            if (tconfig_arr(indx)%precision() /= precisions(indx)) then
               err_cnt = err_cnt + 1
               write(out_unit, '(3a,i0,4a)') "FAIL: ", trim(test_msg),        &
                    ": precision(", indx, ") is ",                            &
                    trim(tconfig_arr(indx)%precision()), ", should be ",      &
                    trim(precisions(indx))
            end if
         end do
      end if

   end subroutine run_test

end module test_hist_mod

!=========================================================================

program test_history

   use shr_kind_mod,   only: max_chars=>SHR_KIND_CX
   use shr_kind_mod,   only: max_flen=>SHR_KIND_CL
   use cam_abortutils, only: endrun, check_endrun
   use cam_hist_file,  only: hist_file_t
   use cam_hist_file,  only: hist_read_namelist_config
   use test_hist_mod,  only: run_test

   implicit none

   integer                    :: out_unit = 6
   integer                    :: ierr
   integer                    :: errcnt
   integer                    :: testcnt
   integer                    :: total_errcnt = 0
   integer                    :: total_tests = 0
   character(len=max_flen)    :: sample_dir
   character(len=max_flen)    :: test_file
   character(len=max_chars)   :: test_msg
   type(hist_file_t), pointer :: test_config_arr(:) => NULL()

   ! Get sample directory from command line
   errcnt = command_argument_count()
   if (errcnt /= 1) then
      call get_command_argument(0, value=test_file, status=ierr)
      if (ierr > 0) then
         test_file = "test_history.F90"
      end if
      write(6, *) "USAGE: ", trim(test_file), " <sample_file_directory>"
      STOP 1
   end if
   call get_command_argument(1, value=sample_dir, status=ierr)
   if (ierr > 0) then
      write(6, *) "ERROR retrieving <sample_file_directory> from command line"
      STOP 1
   else if ((ierr < 0) .or. (len_trim(sample_dir) == max_flen)) then
      write(6, *) "ERROR <sample_file_directory> too long"
      STOP 1
   end if
   if (sample_dir(len_trim(sample_dir):len_trim(sample_dir)) /= "/") then
      sample_dir = trim(sample_dir)//"/"
   end if

   call MPI_init(errcnt)

   ! Read non-existent file test
   test_file = trim(sample_dir)//"ThisFileBetterNotExist.fool"
   test_config_arr => hist_read_namelist_config(test_file)
   total_tests = total_tests + 1
   if (.not. check_endrun()) then
      total_errcnt = total_errcnt + 1
      write(out_unit, *) "FAIL: Non-existent file read test"
   end if

   ! Read single-good config test
   test_file = "single_good_config.nl"
   test_msg = "single_good_config.nl file read test"
   call run_test(test_msg, test_file, sample_dir, out_unit, 1,                &
        (/ "%c.cam.h1.%y-%m-%d-%s.nc" /), (/ 13 /),                           &
        (/ 'REAL32' /), testcnt, errcnt)
   total_tests = total_tests + testcnt
   total_errcnt = total_errcnt + errcnt

   ! Read single-good config test
   test_file = "two_good_configs.nl"
   test_msg = "two_good_configs.nl file read test"
   call run_test(test_msg, test_file, sample_dir, out_unit, 2,                &
        (/ "%c.cam.h1.%y-%m-%d-%s.nc", "%c.cam.h0.%y-%m-%d-%s.nc" /),         &
        (/ 13, 30 /), (/ 'REAL32', 'REAL64' /), testcnt, errcnt)
   total_tests = total_tests + testcnt
   total_errcnt = total_errcnt + errcnt

   call MPI_finalize(errcnt)

   if (total_errcnt > 0) then
      write(6, '(2(a,i0))') 'FAIL, error count = ', total_errcnt,             &
           ' / ', total_tests
      STOP 1
   else
      write(6, '(a,i0,a)') "All ", total_tests, " history tests passed!"
      STOP 0
   end if

end program test_history
