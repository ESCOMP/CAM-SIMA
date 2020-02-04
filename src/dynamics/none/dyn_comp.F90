module dyn_comp

   !----------------------------------------------------------------------
   !   This module contains stub interfaces for a case running with no
   !   dynamical core
   !----------------------------------------------------------------------

   use shr_kind_mod, only: r8 => SHR_KIND_R8

   implicit none
   private
   save

   public :: dyn_readnl
   public :: dyn_register
   public :: dyn_init
   public :: dyn_run1
   public :: dyn_run2
   public :: dyn_run3
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

   subroutine dyn_init(dyn_in, dyn_out)
      ! Null dycore, no action

      ! Dummy arguments:
      type (dyn_import_t),     intent(out) :: dyn_in
      type (dyn_export_t),     intent(out) :: dyn_out

   end subroutine dyn_init

!==============================================================================

   subroutine dyn_run1(phys_state, phys_tend, dyn_in, dyn_out, dtime_out)
      use physics_types,  only: physics_state, physics_tend

      ! Dummy arguments
      type(physics_state), intent(inout) :: phys_state
      type(physics_tend),  intent(inout) :: phys_tend
      type(dyn_import_t),  intent(inout) :: dyn_in
      type(dyn_export_t),  intent(inout) :: dyn_out
      real(r8),            intent(out)   :: dtime_out

      dtime_out = 1800.0_r8

   end subroutine dyn_run1

!==============================================================================

   subroutine dyn_run2(phys_state, phys_tend, dyn_in, dyn_out)
      use physics_types, only: physics_state, physics_tend

      ! Dummy arguments
      type(physics_state), intent(inout) :: phys_state
      type(physics_tend),  intent(inout) :: phys_tend
      type(dyn_import_t),  intent(inout) :: dyn_in
      type(dyn_export_t),  intent(inout) :: dyn_out

   end subroutine dyn_run2

!==============================================================================

   subroutine dyn_run3(dtime, phys_state, phys_tend, cam_out, dyn_in, dyn_out)
      use physics_types, only: physics_state, physics_tend
      use camsrfexch,    only: cam_out_t

      ! Dummy arguments
      real(r8),            intent(in)    :: dtime
      type(cam_out_t),     intent(in)    :: cam_out
      type(physics_state), intent(inout) :: phys_state
      type(physics_tend),  intent(inout) :: phys_tend
      type(dyn_import_t),  intent(inout) :: dyn_in
      type(dyn_export_t),  intent(inout) :: dyn_out

   end subroutine dyn_run3

!==============================================================================

   subroutine dyn_final(dyn_in, dyn_out)

      ! Dummy arguments
      type (dyn_import_t), intent(inout)           :: dyn_in
      type (dyn_export_t), intent(inout)           :: dyn_out


   end subroutine dyn_final

end module dyn_comp
