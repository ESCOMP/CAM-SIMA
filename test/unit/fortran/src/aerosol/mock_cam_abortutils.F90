!-----------------------------------------------------------------------
! Mock cam_abortutils for unit testing.
! Provides endrun that prints a message and calls error stop.
!-----------------------------------------------------------------------
module cam_abortutils

  implicit none
  private

  public :: endrun
  public :: check_allocate

contains

  subroutine endrun(msg, file, line)
    character(len=*), intent(in) :: msg
    character(len=*), intent(in), optional :: file
    integer,          intent(in), optional :: line
    if (present(file) .and. present(line)) then
       write(*,*) 'MOCK endrun: ', trim(msg), ' at ', trim(file), ':', line
    else
       write(*,*) 'MOCK endrun: ', trim(msg)
    end if
    error stop 1
  end subroutine endrun

  subroutine check_allocate(errcode, subname, fieldname, file, line, errmsg)
    integer,                    intent(in) :: errcode
    character(len=*),           intent(in) :: subname
    character(len=*),           intent(in) :: fieldname
    character(len=*), optional, intent(in) :: file
    integer,          optional, intent(in) :: line
    character(len=*), optional, intent(in) :: errmsg

    if (errcode /= 0) then
       call endrun(trim(subname)//': failed to allocate '//trim(fieldname), &
            file=file, line=line)
    end if
  end subroutine check_allocate

end module cam_abortutils
