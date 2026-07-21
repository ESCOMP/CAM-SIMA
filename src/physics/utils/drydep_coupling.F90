module drydep_coupling

   implicit none
   private

   public :: drydep_coupling_set_nflds
   public :: drydep_coupling_set_list

   !> \section arg_table_drydep_coupling  Argument Table
   !! \htmlinclude drydep_coupling.html
   ! Number of gas species with land-computed dry deposition velocities
   ! received through the coupler field Sl_ddvel. The authoritative value
   ! is owned by shr_drydep_mod (CMEPS) and set by shr_drydep_readnl during
   ! the NUOPC advertise phase; the cap mirrors it here, before physics
   ! initialization allocates registry fields dimensioned by it.
   integer, public, protected :: n_drydep = 0

   ! Species names of the drv_flds_in drydep_list, in list order. The list
   ! order is the Sl_ddvel coupler index contract with the land model, so
   ! it cannot be reordered or trimmed atm-side. Mirrored by the cap along
   ! with the count; consumed by gas_drydep_ccpp at init (no metadata
   ! entry: character arrays are use-associated, not capgen-passed).
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
