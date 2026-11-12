module megan_coupling
   implicit none
   private

   public :: megan_coupling_set_nflds

   !> \section arg_table_megan_coupling  Argument Table
   !! \htmlinclude megan_coupling.html
   ! Number of MEGAN VOC emission fields received
   ! through the coupler field Fall_voc.
   integer, public, protected :: megan_nflds = 0

contains

   subroutine megan_coupling_set_nflds(megan_nflds_in)
      integer, intent(in) :: megan_nflds_in

      megan_nflds = megan_nflds_in
   end subroutine megan_coupling_set_nflds

end module megan_coupling
