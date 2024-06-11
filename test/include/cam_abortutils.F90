module cam_abortutils

  implicit none
  private

  public endrun

CONTAINS

  subroutine endrun(msg)
    character(len=*), intent(in) :: msg

    write(6, *) msg
    STOP
  end subroutine endrun

end module cam_abortutils
