module drydep_coupling

   implicit none
   private

   public :: drydep_coupling_set_nflds
   public :: drydep_coupling_set_list

!> \section arg_table_drydep_coupling  Argument Table
!! \htmlinclude drydep_coupling.html
   ! Number of gas species with land-computed dry deposition velocities
   ! received through the coupler field Sl_ddvel.
   ! CMEPS shr_drydep_mod owns this value and sets it in shr_drydep_readnl
   ! during the NUOPC advertise phase.
   !
   ! We mirror it here before physics initialization allocates the registry
   ! fields using it as a dimension.
   ! It can only be mirrored via a setter (not USE) since the framework
   ! will not recognize it in the argument table otherwise.
   integer, public, protected :: n_drydep = 0

   ! Species names of the drv_flds_in drydep_list, in list order.
   ! The list order is shared with the land model passing in Sl_ddvel, so
   ! it cannot be reordered or trimmed atm-side.
   character(len=32), allocatable, public, protected :: drydep_list(:)

contains

   subroutine drydep_coupling_set_nflds(n_drydep_in)
      integer, intent(in) :: n_drydep_in

      n_drydep = n_drydep_in
   end subroutine drydep_coupling_set_nflds

   subroutine drydep_coupling_set_list(drydep_list_in)
      character(len=*), intent(in) :: drydep_list_in(:)

      drydep_list = drydep_list_in
   end subroutine drydep_coupling_set_list

end module drydep_coupling
