module megan_coupling

   implicit none
   private

   public :: megan_coupling_set_nflds

   !> \section arg_table_megan_coupling  Argument Table
   !! \htmlinclude megan_coupling.html
   ! Number of MEGAN VOC emission fields received through the coupler
   ! field Fall_voc. The authoritative value is owned by shr_megan_mod
   ! (CMEPS) and set by shr_megan_readnl during the NUOPC advertise
   ! phase; the cap mirrors it here, before physics initialization
   ! allocates registry fields dimensioned by it. When MEGAN is inactive
   ! the count stays 0 and those fields have zero extent.
   integer, public, protected :: megan_nflds = 0

contains

   subroutine megan_coupling_set_nflds(megan_nflds_in)
      integer, intent(in) :: megan_nflds_in

      megan_nflds = megan_nflds_in
   end subroutine megan_coupling_set_nflds

end module megan_coupling
