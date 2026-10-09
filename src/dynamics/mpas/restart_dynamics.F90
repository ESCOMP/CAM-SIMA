module restart_dynamics

! Writing and reading grid and dynamics state information to/from restart files is
! delegated to MPAS utility code.  This module provides the CAM interfaces for the
! restart functionality.  CAM just provides MPAS with the PIO filehandle to the
! restart file.

use dyn_comp,           only: dyn_export_t, mpas_dynamical_core
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
   type(file_desc_t), pointer :: file_ptr
   !----------------------------------------------------------------------------

   file_ptr => File

   call mpas_dynamical_core % read_write_stream(File, 'w', 'invariant+restart+input')

end subroutine write_restart_dynamics

end module restart_dynamics
