module spmd_utils

!-----------------------------------------------------------------------
!
! Purpose: This module is responsible for miscellaneous SPMD utilities
!          and information that are shared between dynamics and
!          physics packages.
!
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
!- use statements ------------------------------------------------------
!-----------------------------------------------------------------------

   implicit none
   private                   ! Make the default access private
   save

!-----------------------------------------------------------------------
! Public interfaces ----------------------------------------------------
!-----------------------------------------------------------------------
   public spmd_init

   integer, public                :: mpicom
   logical, public                :: masterproc
   integer, public                :: masterprocid
   integer, public                :: iam
   integer, public                :: npes
   ! processor name for this task
   character, allocatable         :: proc_name(:)
   ! the value of iam which is assigned the masterproc duties
   integer,   parameter           :: DEFAULT_MASTERPROC = 0

!========================================================================
CONTAINS
!========================================================================

   subroutine spmd_init(mpicom_atm)
      use mpi, only: MPI_MAX_PROCESSOR_NAME
      !-----------------------------------------------------------------------
      !
      ! Purpose: MPI initialization routine:
      !
      ! Method: get number of cpus, processes, tids, etc
      !         dynamics and physics decompositions are set up later
      !
      !-----------------------------------------------------------------------

      ! Dummy argument
      integer, intent(in) :: mpicom_atm

      !
      ! Local variables
      !
      integer                               :: ierr        ! return error status
      integer                               :: length      ! length of name
      character(len=mpi_max_processor_name) :: tmp_name    ! temporary storage

      !------------------------------------------------------------------------
      !
      ! Determine CAM MPI communicator group
      !
      mpicom  = mpicom_atm
      !
      ! Get my id
      !
      call mpi_comm_rank (mpicom, iam, ierr)
      masterprocid = DEFAULT_MASTERPROC
      masterproc = iam == masterprocid
      !
      ! Get number of processors
      !
      call mpi_comm_size (mpicom, npes, ierr)
      !
      ! Get processor name
      !
      call mpi_get_processor_name (tmp_name, length, ierr)
      ! Attempt to allocate proc_name padded to 8 bytes
      ierr = MOD(length, 8)
      if ( (ierr > 0) .and.                                                   &
           ((length + (8 - ierr)) <= mpi_max_processor_name)) then
         allocate(proc_name(length + (8 - ierr)))
      else
         allocate(proc_name(length))
      end if
      proc_name(1:length) = tmp_name(1:length)
      proc_name(length+1:) = ' '

   end subroutine spmd_init

 end module spmd_utils
