module restart_dynamics

! Writing and reading grid and dynamics state information to/from restart files is
! delegated to MPAS utility code.  This module provides the CAM interfaces for the
! restart functionality.  CAM just provides MPAS with the PIO filehandle to the
! restart file.

use dyn_comp,           only: dyn_export_t, dyn_import_t, mpas_dynamical_core
use pio,                only: file_desc_t

implicit none
private
save

public :: &
   init_restart_dynamics,  &
   write_restart_dynamics, &
   read_restart_dynamics

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

!=========================================================================================

subroutine read_restart_dynamics(File, dyn_in, dyn_out)
   use dyn_comp, only: dyn_init
   use runtime_obj, only: cam_runtime_opts

   ! arguments
   type(File_desc_t), target :: File
   type(dyn_import_t), intent(in) :: dyn_in
   type(dyn_export_t), intent(in) :: dyn_out

   call dyn_init(cam_runtime_opts, dyn_in, dyn_out)

end subroutine read_restart_dynamics

end module restart_dynamics
