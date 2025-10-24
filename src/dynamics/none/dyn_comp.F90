module dyn_comp

   !----------------------------------------------------------------------
   !   This module contains stub interfaces for a case running with no
   !   dynamical core
   !----------------------------------------------------------------------

   implicit none
   private
   save

   public :: dyn_readnl
   public :: dyn_register
   public :: dyn_init
   public :: dyn_run
   public :: dyn_final
   public :: dyn_import_t
   public :: dyn_export_t
   public :: dyn_state

   type :: dyn_state
      character(len=16) :: name = "Null dycore"
   end type dyn_state

   type dyn_import_t
      character(len=16) :: name = "Null import"
   end type dyn_import_t

   type dyn_export_t
      character(len=16) :: name = "Null export"
   end type dyn_export_t

!==============================================================================
CONTAINS
!==============================================================================

   subroutine dyn_readnl(nlfilename)
      ! Null dycore, no action

      ! Dummy argument
      character(len=*), intent(in) :: nlfilename

   end subroutine dyn_readnl

!==============================================================================

   subroutine dyn_register()
      ! Null dycore, no action

   end subroutine dyn_register

!==============================================================================

   subroutine dyn_init(cam_runtime_opts, dyn_in, dyn_out)
      use runtime_obj, only: set_cam_dycore, runtime_options
      ! Null dycore, no action

      ! Dummy arguments:
      type(runtime_options), intent(in)  :: cam_runtime_opts
      type(dyn_import_t),    intent(out) :: dyn_in
      type(dyn_export_t),    intent(out) :: dyn_out

      ! Note: dynamical core energy formula is set in dyn_grid based on dynamical core
      ! that provided the initial conditions file
      call set_cam_dycore('null')

   end subroutine dyn_init

!==============================================================================

   subroutine dyn_run(dyn_out)

      ! Dummy arguments
      type(dyn_export_t), intent(inout) :: dyn_out

   end subroutine dyn_run

!==============================================================================

   subroutine dyn_final(dyn_in, dyn_out)

      ! Dummy arguments
      type(dyn_import_t), intent(inout) :: dyn_in
      type(dyn_export_t), intent(inout) :: dyn_out

   end subroutine dyn_final

end module dyn_comp
