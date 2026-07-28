module restart_dynamics

use dyn_comp,           only: dyn_export_t
use pio,                only: file_desc_t

implicit none
private
save

public :: &
   init_restart_dynamics,  &
   write_restart_dynamics

!=========================================================================================
contains
!=========================================================================================

subroutine init_restart_dynamics(file, dyn_out)

   ! arguments
   type(file_desc_t),  target     :: File
   type(dyn_export_t), intent(in) :: dyn_out
   !----------------------------------------------------------------------------
   
   ! STUB-ROUTINE

end subroutine init_restart_dynamics

!=========================================================================================

subroutine write_restart_dynamics(File, dyn_out)

   ! arguments
   type(File_desc_t), target :: File
   type(dyn_export_t), intent(in)  :: dyn_out
   !----------------------------------------------------------------------------

   ! STUB-ROUTINE
end subroutine write_restart_dynamics

end module restart_dynamics
