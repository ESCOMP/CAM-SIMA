module cam_ccpp_scheme_namelists
! This routine will be auto-generated

   implicit none
   private

   public :: cam_read_ccpp_scheme_namelists

CONTAINS

   subroutine cam_read_ccpp_scheme_namelists(nlfile, active_schemes,          &
        mpi_communicator, rootprocid, isrootproc,                             &
        waccmx_opt, gw_front, gw_front_igw)

      character(len=*),  intent(in)    :: nlfile
      character(len=*),  intent(in)    :: active_schemes(:)
      integer,           intent(in)    :: mpi_communicator
      integer,           intent(in)    :: rootprocid
      logical,           intent(in)    :: isrootproc
      character(len=16), intent(inout) :: waccmx_opt
      logical,           intent(inout) :: gw_front
      logical,           intent(inout) :: gw_front_igw

   end subroutine cam_read_ccpp_scheme_namelists

end module cam_ccpp_scheme_namelists
