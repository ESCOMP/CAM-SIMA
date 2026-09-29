!-----------------------------------------------------------------------
! Mock shr_string_mod for unit testing.
! Provides shr_string_toUpper and shr_string_toLower.
!-----------------------------------------------------------------------
module shr_string_mod

  implicit none
  private

  public :: shr_string_toUpper
  public :: shr_string_toLower

contains

  function shr_string_toUpper(str) result(upper)
    character(len=*), intent(in) :: str
    character(len=len(str)) :: upper
    integer :: i, ic
    upper = str
    do i = 1, len_trim(str)
       ic = iachar(str(i:i))
       if (ic >= iachar('a') .and. ic <= iachar('z')) then
          upper(i:i) = achar(ic - 32)
       end if
    end do
  end function shr_string_toUpper

  function shr_string_toLower(str) result(lower)
    character(len=*), intent(in) :: str
    character(len=len(str)) :: lower
    integer :: i, ic
    lower = str
    do i = 1, len_trim(str)
       ic = iachar(str(i:i))
       if (ic >= iachar('A') .and. ic <= iachar('Z')) then
          lower(i:i) = achar(ic + 32)
       end if
    end do
  end function shr_string_toLower

end module shr_string_mod
