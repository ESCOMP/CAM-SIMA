module dyn_grid
    implicit none

    private
    ! Provide APIs required by CAM Control.
    public :: model_grid_init
contains

! Called by `cam_init` in `src/control/cam_comp.F90`.
subroutine model_grid_init()
end subroutine model_grid_init

end module dyn_grid
