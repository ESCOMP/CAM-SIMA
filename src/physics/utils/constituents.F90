module constituents

   implicit none
   private

   public :: cnst_readnl

   integer, protected, public :: pcnst = 0
   ! Namelist variable
   ! readtrace: Obtain initial tracer data from IC file if .true.
   logical, public, protected :: readtrace = .true.

CONTAINS

   subroutine cnst_readnl(nlfile)

      use mpi,            only: mpi_logical
      use shr_nl_mod,     only: find_group_name => shr_nl_find_group_name
      use spmd_utils,     only: masterproc, mpicom, mstrid=>masterprocid
      use cam_logfile,    only: iulog
      use cam_abortutils, only: endrun
      !!XXgoldyXX: v see comment about pcnst
      use physics_types,  only: ix_qv, ix_cld_liq, ix_rain
      !!XXgoldyXX: ^ see comment about pcnst

      ! nlfile: filepath for file containing namelist input
      character(len=*), intent(in) :: nlfile

      ! Local variables
      integer                      :: unitn, ierr
      character(len=*), parameter  :: sub = 'cnst_readnl'

      namelist /constituents_nl/ readtrace
      !------------------------------------------------------------------------

      !!XXgoldyXX: v Need to figure out how to figure out pcnst
      pcnst = 3
      ix_qv = 1
      ix_cld_liq = 2
      ix_rain = 3
      !!XXgoldyXX: ^ Need to figure out how to figure out pcnst

      if (masterproc) then
         open(newunit=unitn, file=trim(nlfile), status='old')
         call find_group_name(unitn, 'constituents_nl', status=ierr)
         if (ierr == 0) then
            read(unitn, constituents_nl, iostat=ierr)
            if (ierr /= 0) then
               call endrun(sub//': FATAL: reading namelist')
            end if
         end if
         close(unitn)
      end if

      call mpi_bcast(readtrace, 1, mpi_logical, mstrid, mpicom, ierr)
      if (ierr /= 0) then
         call endrun(sub//": FATAL: mpi_bcast: readtrace")
      end if

      if (masterproc) then
         write(iulog,*)'Summary of constituent module options:'
         if (readtrace) then
            write(iulog,*)'  Attempt to read constituent initial values ',    &
                 'from the initial file by default'
         else
            write(iulog,*)'  Do not read constituent initial values ',        &
                 'from the initial file'
         end if
      end if

   end subroutine cnst_readnl

end module constituents
