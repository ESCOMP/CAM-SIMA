module gmean_mod
   !-----------------------------------------------------------------------
   !
   ! Purpose:
   ! Perform global mean calculations for energy conservation and other checks.
   !
   ! Method:
   ! Reproducible (scalable):
   !    Convert to fixed point (integer representation) to enable
   !    reproducibility when using MPI collectives.
   ! If error checking is on (via setting reprosum_diffmax > 0 and
   !    reprosum_recompute = .true. in user_nl_cpl), shr_reprosum_calc will
   !    check the accuracy of its computation with a fast but
   !    non-reproducible algorithm. If any error is reported, report
   !    the difference and the expected sum and abort run (call endrun)
   !
   ! gmean_mod in to_be_ccppized is different from the CAM version and
   ! has chunk support removed.
   !
   !
   !-----------------------------------------------------------------------
   use shr_kind_mod,     only: r8 => shr_kind_r8
   use physics_grid,     only: pcols => columns_on_task
   use perf_mod,         only: t_startf, t_stopf
   use cam_logfile,      only: iulog

   implicit none
   private

   public :: gmean       ! compute global mean of 2D fields on physics decomposition

   interface gmean
      module procedure gmean_arr
      module procedure gmean_scl
   end interface gmean

   private :: gmean_fixed_repro
   private :: gmean_float_norepro

   ! Set do_gmean_tests to .true. to run a gmean challenge test
   logical, private    :: do_gmean_tests = .false.

CONTAINS

   !
   !========================================================================
   !

   subroutine gmean_arr (arr, arr_gmean, nflds)
      use shr_strconvert_mod, only: toString
      use spmd_utils,         only: masterproc
      use cam_abortutils,     only: endrun
      use shr_reprosum_mod,   only: shr_reprosum_reldiffmax, shr_reprosum_recompute, shr_reprosum_tolExceeded
      !-----------------------------------------------------------------------
      !
      ! Purpose:
      ! Compute the global mean of each field in "arr" in the physics grid
      !
      ! Method is to call shr_reprosum_calc (called from gmean_fixed_repro)
      !-----------------------------------------------------------------------
      !
      ! Arguments
      !
      integer,  intent(in)  :: nflds            ! number of fields
      real(r8), intent(in)  :: arr(pcols, nflds)
      real(r8), intent(out) :: arr_gmean(nflds) ! global means
      !
      ! Local workspace
      !
      real(r8)                   :: rel_diff(2, nflds)
      integer                    :: ifld ! field index
      integer                    :: num_err
      logical                    :: write_warning
      !
      !-----------------------------------------------------------------------
      !
      call t_startf('gmean_arr')
      call t_startf ('gmean_fixed_repro')
      call gmean_fixed_repro(arr, arr_gmean, rel_diff, nflds)
      call t_stopf ('gmean_fixed_repro')

      ! check that "fast" reproducible sum is accurate enough. If not, calculate
      ! using old method
      write_warning = masterproc
      num_err = 0
      if (shr_reprosum_tolExceeded('gmean', nflds, write_warning,             &
           iulog, rel_diff)) then
         if (shr_reprosum_recompute) then
            do ifld = 1, nflds
               if (rel_diff(1, ifld) > shr_reprosum_reldiffmax) then
                  call gmean_float_norepro(arr(:,ifld), arr_gmean(ifld), ifld)
                  num_err = num_err + 1
               end if
            end do
         end if
      end if
      call t_stopf('gmean_arr')
      if (num_err > 0) then
         call endrun('gmean: '//toString(num_err)//' reprosum errors found')
      end if

   end subroutine gmean_arr

   !
   !========================================================================
   !

   subroutine gmean_scl (arr, gmean)
      !-----------------------------------------------------------------------
      !
      ! Purpose:
      ! Compute the global mean of a single field in "arr" in the physics grid
      !
      !-----------------------------------------------------------------------
      !
      ! Arguments
      !
      real(r8), intent(in)  :: arr(pcols)
      ! Input array
      real(r8), intent(out) :: gmean      ! global means
      !
      ! Local workspace
      !
      integer, parameter    :: nflds = 1
      real(r8)              :: gmean_array(nflds)
      real(r8)              :: array(pcols, nflds)
      integer               :: ncols, lchnk

      array(:ncols, 1) = arr(:ncols)
      call gmean_arr(array, gmean_array, nflds)
      gmean = gmean_array(1)

   end subroutine gmean_scl

   !
   !========================================================================
   !

   subroutine gmean_float_norepro(arr, repro_sum, index)
      !-----------------------------------------------------------------------
      !
      ! Purpose:
      ! Compute the global mean of <arr> in the physics chunked
      !    decomposition using a fast but non-reproducible algorithm.
      !    Log that value along with the value computed by
      !    shr_reprosum_calc (<repro_sum>)
      !
      !-----------------------------------------------------------------------

      use physconst,    only: pi
      use spmd_utils,   only: masterproc, masterprocid, mpicom
      use mpi,          only: mpi_real8, mpi_sum
      use physics_grid, only: get_wght_p
      !
      ! Arguments
      !
      real(r8), intent(in) :: arr(pcols)
      real(r8), intent(in) :: repro_sum ! Value computed by reprosum
      integer,  intent(in) :: index     ! Index of field in original call
      !
      ! Local workspace
      !
      integer             :: icol
      integer             :: ierr
      real(r8)            :: wght
      real(r8)            :: check
      real(r8)            :: check_sum
      real(r8), parameter :: pi4 = 4.0_r8 * pi

      !
      !-----------------------------------------------------------------------
      !
      ! Calculate and print out non-reproducible value
      check = 0.0_r8
      do icol = 1, pcols
         wght = get_wght_p(icol)
         check = check + arr(icol) * wght
      end do
      call MPI_reduce(check, check_sum, 1, mpi_real8, mpi_sum,     &
                       masterprocid, mpicom, ierr)

      ! normalization
      check_sum = check_sum / pi4

      if (masterproc) then
         write(iulog, '(a,i0,2(a,e20.13e2))') 'gmean(', index, ') = ',        &
              check_sum, ', reprosum reported ', repro_sum
      end if

   end subroutine gmean_float_norepro

   !
   !========================================================================
   !
   subroutine gmean_fixed_repro(arr, arr_gmean, rel_diff, nflds)
      !-----------------------------------------------------------------------
      !
      ! Purpose:
      ! Compute the global mean of each field in "arr" in the physics grid
      ! with a reproducible yet scalable implementation
      ! based on a fixed-point algorithm.
      !
      !-----------------------------------------------------------------------
      use spmd_utils,       only: mpicom
      use physics_grid,     only: get_wght_all_p
      use physics_grid,     only: ngcols_p => num_global_phys_cols
      use physconst,        only: pi
      use shr_reprosum_mod, only: shr_reprosum_calc
      use cam_abortutils,   only: check_allocate
      !
      ! Arguments
      !
      integer,  intent(in)  :: nflds ! number of fields
      real(r8), intent(in)  :: arr(pcols,nflds)
      ! arr_gmean: output global sums
      real(r8), intent(out) :: arr_gmean(nflds)
      ! rel_diff: relative and absolute differences from shr_reprosum_calc
      real(r8), intent(out) :: rel_diff(2, nflds)
      !
      ! Local workspace
      !
      real(r8),         parameter :: pi4 = 4.0_r8 * pi
      character(len=*), parameter :: subname = 'gmean_fixed_repro: '

      integer               :: icol, ifld        ! column, field indices
      integer               :: errflg

      real(r8)              :: wght(pcols)       ! integration weights
      real(r8), allocatable :: xfld(:,:)         ! weighted summands

      errflg = 0

      allocate(xfld(pcols, nflds), stat=errflg)
      call check_allocate(errflg, subname, 'xfld(pcols, nflds)', &
                          file=__FILE__, line=__LINE__)

      ! pre-weight summands
      call get_wght_all_p(pcols, wght)

      do ifld = 1, nflds
         do icol = 1, pcols
            xfld(icol, ifld) = arr(icol, ifld) * wght(icol)
         end do
      end do

      ! call fixed-point algorithm
      call shr_reprosum_calc ( &
           arr       = xfld, &
           arr_gsum  = arr_gmean, &
           nsummands = pcols, &    ! # of local summands
           dsummands = pcols, &    ! declared first dimension of arr.
           nflds     = nflds, &
           commid    = mpicom, &
           rel_diff  = rel_diff &
      )

      deallocate(xfld)
      ! final normalization
      arr_gmean(:) = arr_gmean(:) / pi4

   end subroutine gmean_fixed_repro

end module gmean_mod
