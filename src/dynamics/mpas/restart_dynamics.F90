module restart_dynamics

! Writing and reading grid and dynamics state information to/from restart files is
! delegated to MPAS utility code.  This module provides the CAM interfaces for the
! restart functionality.  CAM just provides MPAS with the PIO filehandle to the
! restart file.

implicit none
private

public :: &
   init_restart_dynamics,  &
   write_restart_dynamics, &
   read_restart_dynamics

!=========================================================================================
contains
!=========================================================================================

subroutine init_restart_dynamics(file, dyn_out)
   use pio,                only: file_desc_t
   use dyn_comp,           only: dyn_export_t

   ! arguments
   type(file_desc_t),  target     :: File
   type(dyn_export_t), intent(in) :: dyn_out
   !----------------------------------------------------------------------------
   
   ! STUB-ROUTINE

end subroutine init_restart_dynamics

!=========================================================================================

subroutine write_restart_dynamics(File, dyn_out)
   use pio,                only: file_desc_t
   use dyn_comp,           only: dyn_export_t, mpas_dynamical_core

   ! arguments
   type(file_desc_t), target       :: File
   type(dyn_export_t), intent(in)  :: dyn_out
   !----------------------------------------------------------------------------

   call mpas_dynamical_core % read_write_stream(File, 'w', 'invariant+restart+input')

end subroutine write_restart_dynamics

!=========================================================================================

subroutine read_restart_dynamics(File, dyn_in, dyn_out)
   use dyn_comp,    only: dyn_init
   use runtime_obj, only: cam_runtime_opts
   use pio,         only: file_desc_t
   use dyn_comp,    only: dyn_export_t, dyn_import_t

   ! arguments
   type(file_desc_t), target :: File
   type(dyn_import_t), intent(in) :: dyn_in
   type(dyn_export_t), intent(in) :: dyn_out

   call dyn_init(cam_runtime_opts, dyn_in, dyn_out)

end subroutine read_restart_dynamics

end module restart_dynamics
