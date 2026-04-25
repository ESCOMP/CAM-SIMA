!-----------------------------------------------------------------------
! Mock cam_abortutils for unit testing.
! Provides endrun that prints a message and calls error stop.
!-----------------------------------------------------------------------
module cam_abortutils

  implicit none
  private

  public :: endrun

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

end module cam_abortutils
