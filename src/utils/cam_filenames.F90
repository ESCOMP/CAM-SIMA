module cam_filenames

   ! Module and methods to handle filenames needed for the model. This
   ! includes input filenames, and most output filenames that the model
   ! uses. All filenames that the model uses will use methods or data
   ! constructed by this module. In some cases (such as the cam_history module)
   ! other modules or routines will store the actual filenames used, but
   ! this module is used to determine the names.

   use shr_kind_mod,     only: cl=>shr_kind_cl

   implicit none
   private
   save

   public :: get_dir                 ! Get the directory name from a full path
   public :: interpret_filename_spec ! Interpret a filename specifier

!==============================================================================
CONTAINS
!==============================================================================

   pure character(len=cl) function get_dir(filepath)

      ! Return the directory from a filename with a full path

      ! Dummy argument
      character(len=*), intent(in) :: filepath ! Full path for a filename

      ! local variable
      integer :: filenameposition ! Character pos for last character of directory
      !------------------------------------------------------------------------

      ! Get the directory name of the input dataset
      filenameposition = index(filepath, '/', back=.true.)
      if (filenameposition == 0)then
         get_dir  = './'
      else
         get_dir  = filepath(1:filenameposition)
      end if

   end function get_dir

   !===========================================================================

   character(len=cl) function interpret_filename_spec(filename_spec, unit, accum_type, &
        prev, case, instance, yr_spec, mon_spec, day_spec, sec_spec, incomplete_ok)
      use time_manager,     only: get_curr_date, get_prev_date
      use spmd_utils,       only: masterproc
      use cam_logfile,      only: iulog
      use cam_abortutils,   only: endrun
      use cam_control_mod,  only: caseid

      ! Create a filename from a filename specifier. The
      ! filename specifier includes codes for setting things such as the
      ! year, month, day, seconds in day, caseid, and file unit (e.g., h0, i).
      !
      ! Interpret filename specifier string (<filename_spec>) with:
      !
      !      %c for case (<case>)
      !      %i for instance specification (<instance>)
      !      %u for unit specification (<unit>)
      !      %f for accumulation file (<accum_type>)
      !      %y for year (<yr_spec>)
      !      %m for month (<mon_spec>)
      !      %d for day (<day_spec>)
      !      %s for second (<sec_spec>)
      !      %% for the "%" character
      !
      ! If <prev> is present and .true. label the file with previous time-step
      ! If <incomplete_ok> is present and .true., then wildcards without
      !   values passed as optional dummy arguments will not generate an error.
      !   This allows a partial resolution of the filename_spec.

      ! Dummy Arguments
      character(len=*),           intent(in) :: filename_spec
      character(len=*), optional, intent(in) :: unit
      character(len=*), optional, intent(in) :: accum_type
      logical,          optional, intent(in) :: prev
      character(len=*), optional, intent(in) :: case
      character(len=*), optional, intent(in) :: instance
      integer,          optional, intent(in) :: yr_spec
      integer,          optional, intent(in) :: mon_spec
      integer,          optional, intent(in) :: day_spec
      integer,          optional, intent(in) :: sec_spec
      logical,          optional, intent(in) :: incomplete_ok

      ! Local variables
      integer           :: year     ! Simulation year
      integer           :: month    ! Simulation month
      integer           :: day      ! Simulation day
      integer           :: ncsec    ! Seconds into current simulation day
      character(len=cl) :: string   ! Temporary character string
      character(len=cl) :: fmt_str  ! Format character string
      integer           :: indx     ! Loop variable
      integer           :: next     ! Index location in <filename_spec>
      logical           :: previous ! If should label with previous time-step
      logical           :: done
      logical           :: incomplete_ok_use
      character(len=*), parameter :: subname = "INTERPRET_FILENAME_SPEC: "
      !------------------------------------------------------------------------

      if (len_trim(filename_spec) == 0)then
         call endrun (subname//'filename specifier is empty')
      end if
      if (index(trim(filename_spec), " ") /= 0)then
         call endrun(subname//"filename specifier may not contain a space:"// &
              trim(filename_spec), file=__FILE__, line=__LINE__)
      end if
      if (present(incomplete_ok)) then
         incomplete_ok_use = incomplete_ok
      else
         incomplete_ok_use = .false.
      end if
      !
      ! Determine year, month, day and sec to put in filename
      !
      if (present(yr_spec) .and. present(mon_spec) .and.                      &
           present(day_spec) .and. present(sec_spec)) then
         year  = yr_spec
         month = mon_spec
         day   = day_spec
         ncsec = sec_spec
      else if (.not. incomplete_ok_use) then
         if (present(prev)) then
            previous = prev
         else
            previous = .false.
         end if
         if (previous) then
            call get_prev_date(year, month, day, ncsec)
         else
            call get_curr_date(year, month, day, ncsec)
         end if
      else
         if (present(yr_spec)) then
            year = yr_spec
         end if
         if (present(mon_spec)) then
            month = mon_spec
         end if
         if (present(day_spec)) then
            day = day_spec
         end if
         if (present(sec_spec)) then
            ncsec = sec_spec
         end if
      end if ! No else, do not use these quantities below.
      !
      ! Go through each character in the filename specifier and interpret
      !   if it is a format specifier
      !
      indx = 1
      interpret_filename_spec = ''
      do while (indx <= len_trim(filename_spec))
         !
         ! If following is an expansion string
         !
         if (filename_spec(indx:indx) == "%") then
            indx = indx + 1
            select case(filename_spec(indx:indx))
            case('c')   ! caseid
               if (present(case)) then
                  string = trim(case)
               else if (len_trim(caseid) > 0) then
                  string = trim(caseid)
               else if (incomplete_ok_use) then
                  string = "%c"
               else
                  write(string, *) "case needed in filename_spec, ",          &
                       "but not provided to function, filename_spec = '",     &
                       trim(filename_spec), "'"
                  if (masterproc) then
                     write(iulog, *) subname, trim(string)
                  end if
                  call endrun(subname//trim(string))
               end if
            case('u')   ! unit description (e.g., h2)
               if (present(unit)) then
                  string = trim(unit)
               else if (incomplete_ok_use) then
                  string = "%u"
               else
                  write(string, *) "unit needed in filename_spec, ",          &
                       "but not provided to function, filename_spec = '",     &
                       trim(filename_spec), "'"
                  if (masterproc) then
                     write(iulog, *) subname, trim(string)
                  end if
                  call endrun(subname//trim(string))
               end if
            case('f')   ! accumulate flag (i or a)
               if (present(accum_type)) then
                  string = trim(accum_type)
               else if (incomplete_ok_use) then
                  string = "%f"
               else
                  write(string, *) "flag needed in filename_spec, ",          &
                       "but not provided to function, filename_spec = '",     &
                       trim(filename_spec), "'"
                  if (masterproc) then
                     write(iulog, *) subname, trim(string)
                  end if
                  call endrun(subname//trim(string))
               end if
            case('i')   ! instance description (e.g., _0001)
               if (present(instance)) then
                  string = trim(instance)
               else if (incomplete_ok_use) then
                  string = "%i"
               else
                  write(string, *) "instance needed in filename_spec, ",      &
                       "but not provided to function, filename_spec = '",     &
                       trim(filename_spec), "'"
                  if (masterproc) then
                     write(iulog, *) subname, trim(string)
                  end if
                  call endrun(subname//trim(string))
               end if
            case('y')   ! year
               if (.not. present(yr_spec) .and. incomplete_ok_use) then
                  string = '%y'
               else
                  if (year > 99999) then
                     fmt_str = '(i6.6)'
                  else if (year > 9999) then
                     fmt_str = '(i5.5)'
                  else
                     fmt_str = '(i4.4)'
                  end if
                  write(string,fmt_str) year
               end if
            case('m')   ! month
               if (.not. present(mon_spec) .and. incomplete_ok_use) then
                  string = '%m'
               else
                  write(string,'(i2.2)') month
               end if
            case('d')   ! day
               if (.not. present(day_spec) .and. incomplete_ok_use) then
                  string = '%d'
               else
                  write(string,'(i2.2)') day
               end if
            case('s')   ! second
               if (.not. present(sec_spec) .and. incomplete_ok_use) then
                  string = '%s'
               else
                  write(string,'(i5.5)') ncsec
               end if
            case('%')   ! percent character
               string = "%"
            case default
               call endrun(subname//"Invalid expansion character: "//         &
                    filename_spec(indx:indx))
            end select
            !
            ! Otherwise take normal text up to the next "%" character
            !
         else
            next = index(filename_spec(indx:), "%")
            if (next == 0) then
               next = len_trim(filename_spec(indx:)) + 1
            end if
            string = filename_spec(indx:next+indx-2)
            indx = next + indx - 2
         end if
         if (len_trim(interpret_filename_spec) == 0) then
            interpret_filename_spec = trim(string)
         else
            if ((len_trim(interpret_filename_spec)+len_trim(string)) > cl) then
               call endrun(subname//                                          &
                    "Resultant filename too long, trying to add: '"//         &
                    trim(string)//"' to '"//trim(interpret_filename_spec)//"'")
            end if
            interpret_filename_spec = trim(interpret_filename_spec)//trim(string)
         end if
         indx = indx + 1
      end do
      if (len_trim(interpret_filename_spec) == 0) then
         call endrun(subname//"Resulting filename is empty. Filename spec: "//trim(filename_spec))
      end if

   end function interpret_filename_spec

end module cam_filenames
