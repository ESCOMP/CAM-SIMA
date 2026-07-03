module drydep_coupling

   implicit none
   private

   public :: drydep_coupling_set_nflds

   !> \section arg_table_drydep_coupling  Argument Table
   !! \htmlinclude drydep_coupling.html
   ! Number of gas species with land-computed dry deposition velocities
   ! received through the coupler field Sl_ddvel. The authoritative value
   ! is owned by shr_drydep_mod (CMEPS) and set by shr_drydep_readnl during
   ! the NUOPC advertise phase; the cap mirrors it here, before physics
   ! initialization allocates registry fields dimensioned by it.
   integer, public, protected :: n_drydep = 0

contains

   subroutine drydep_coupling_set_nflds(n_drydep_in)
      integer, intent(in) :: n_drydep_in

      n_drydep = n_drydep_in
   end subroutine drydep_coupling_set_nflds

end module drydep_coupling
