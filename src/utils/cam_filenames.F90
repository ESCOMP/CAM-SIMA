module cam_filenames

   ! Module and methods to handle filenames needed for the model. This
   ! includes input filenames, and most output filenames that the model
   ! uses. All filenames that the model uses will use methods or data
   ! constructed by this module. In some cases (such as the cam_history module)
   ! other modules or routines will store the actual filenames used, but
   ! this module is used to determine the names.

   use shr_kind_mod,     only: cl=>shr_kind_cl
   use time_manager,     only: get_curr_date, get_prev_date
   use string_utils,     only: to_str
   use spmd_utils,       only: masterproc
   use cam_control_mod,  only: caseid
   use cam_abortutils,   only: endrun
   use cam_logfile,      only: iulog

   implicit none
   private
   save

   public :: get_dir                 ! Get the directory name from a full path
   public :: interpret_filename_spec ! Interpret a filename specifier

!==============================================================================
CONTAINS
!==============================================================================

   character(len=cl) function get_dir(filepath)

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

   character(len=cl) function interpret_filename_spec(filename_spec, unit,    &
        prev, case, instance, yr_spec, mon_spec, day_spec, sec_spec)

      ! Create a filename from a filename specifier. The
      ! filename specifyer includes codes for setting things such as the
      ! year, month, day, seconds in day, caseid, and file unit (e.g., h0, i).
      !
      ! Interpret filename specifier string (<filename_spec>) with:
      !
      !      %c for case (<case>)
      !      %i for instance specification (<instance>)
      !      %u for unit specification (<unit>)
      !      %y for year (<yr_spec>)
      !      %m for month (<mon_spec>)
      !      %d for day (<day_spec>)
      !      %s for second (<sec_spec>)
      !      %% for the "%" character
      !
      ! If <prev> is present and .true. label the file with previous time-step

      ! Dummy Arguments
      character(len=*),           intent(in) :: filename_spec
      character(len=*), optional, intent(in) :: unit
      logical,          optional, intent(in) :: prev
      character(len=*), optional, intent(in) :: case
      character(len=*), optional, intent(in) :: instance
      integer,          optional, intent(in) :: yr_spec
      integer,          optional, intent(in) :: mon_spec
      integer,          optional, intent(in) :: day_spec
      integer,          optional, intent(in) :: sec_spec

      ! Local variables
      integer           :: year     ! Simulation year
      integer           :: month    ! Simulation month
      integer           :: day      ! Simulation day
      integer           :: ncsec    ! Seconds into current simulation day
      character(len=cl) :: string   ! Temporary character string
      character(len=cl) :: fmt_str  ! Format character string
      integer           :: indx    ! Loop variable
      integer           :: next     ! Index location in <filename_spec>
      logical           :: previous ! If should label with previous time-step
      logical           :: done
      character(len=*), parameter :: subname = "INTERPRET_FILENAME_SPEC: "
      !------------------------------------------------------------------------

      if (len_trim(filename_spec) == 0)then
         call endrun (subname//'filename specifier is empty')
      end if
      if (index(trim(filename_spec), " ") /= 0)then
         call endrun(subname//"filename specifier can not contain a space:"// &
              trim(filename_spec), file=__FILE__, line=__LINE__)
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
      else
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
      end if
      !
      ! Go through each character in the filename specifyer and interpret
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
               else
                  string = trim(caseid)
               end if
            case('u')   ! unit description (e.g., h2)
               if (.not. present(unit)) then
                  write(string, *) "unit needed in filename_spec, ",          &
                       "but not provided to subroutine, filename_spec = '",   &
                       trim(filename_spec), "'"
                  if (masterproc) then
                     write(iulog, *) subname, trim(string)
                  end if
                  call endrun(subname//trim(string))
               end if
               string = trim(unit)
            case('i')   ! instance description (e.g., _0001)
               if (.not. present(instance)) then
                  write(string, *) "instance needed in filename_spec, ",      &
                       "but not provided to subroutine, filename_spec = '",   &
                       trim(filename_spec), "'"
                  if (masterproc) then
                     write(iulog, *) subname, trim(string)
                  end if
                  call endrun(subname//trim(string))
               end if
               string = trim(instance)
            case('y')   ! year
               if (year > 99999) then
                  fmt_str = '(i6.6)'
               else if (year > 9999) then
                  fmt_str = '(i5.5)'
               else
                  fmt_str = '(i4.4)'
               end if
               write(string,fmt_str) year
            case('m')   ! month
               write(string,'(i2.2)') month
            case('d')   ! day
               write(string,'(i2.2)') day
            case('s')   ! second
               write(string,'(i5.5)') ncsec
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
            if (next == 0) then
               exit
            end if
            string = filename_spec(indx:next+indx-2)
            indx = next + indx - 2
         end if
         if (len_trim(interpret_filename_spec) == 0) then
            interpret_filename_spec = trim(string)
         else
            if ((len_trim(interpret_filename_spec)+len_trim(string)) >= cl) then
               call endrun(subname//"Resultant filename too long")
            end if
            interpret_filename_spec = trim(interpret_filename_spec)//trim(string)
         end if
         indx = indx + 1
      end do
      if (len_trim(interpret_filename_spec) == 0) then
         call endrun(subname//"Resulting filename is empty")
      end if

   end function interpret_filename_spec

end module cam_filenames
