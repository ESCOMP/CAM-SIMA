
module ppgrid

!-----------------------------------------------------------------------
!
! Purpose:  USED ONLY FOR BACKWARDS COMPATIBILITY WITH CAM!!!!
!           PLEASE DELETE ONCE NO LONGER NEEDED BY "to_be_ccppized"
!           PHYSICS SCHEMES!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!           Thanks!
!-----------------------------------------------------------------------

  use physics_grid,     only: pcols => columns_on_task
  use vert_coord,       only: pver => pver, pverp => pverp !WILL NEED TO CHANGE THESE NAMES TO
                                                           !SOMETHING ELSE IN CAM-SIMA!

  implicit none
  private
  save

  public pcols
  public pver
  public pverp

end module ppgrid
