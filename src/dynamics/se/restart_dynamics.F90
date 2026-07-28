module restart_dynamics

! Write and read dynamics fields from the restart file.  For exact restart
! it is necessary to write all element data, including duplicate columns,
! to the file.  The namelist option, se_write_restart_unstruct, is
! available to write just the unique columns to the restart file using the
! same unstructured grid used by the history and initial files.  This
! results in the introduction of a roundoff size difference on restart, but
! writes the fields in the unstructured grid format which is easier to
! modify if the user desires to introduce perturbations or other
! adjustments into the run.  The restart file containing the unstructured
! grid format may also be used for an initial run.

use pio,              only: var_desc_t

implicit none
private
save

public :: init_restart_dynamics
public :: write_restart_dynamics
!public :: read_restart_dynamics

! these variables are module data so they can be shared between the
! file definition and write phases
type(var_desc_t)              :: psdry_desc, udesc, vdesc, tdesc
type(var_desc_t), allocatable :: qdesc_dp(:)
type(var_desc_t)              :: dp_fvm_desc
type(var_desc_t), pointer     :: c_fvm_desc(:)

integer, private :: nelem_tot = -1 ! Correct total number of elements

!=========================================================================================
CONTAINS
!=========================================================================================

subroutine init_nelem_tot()
  use spmd_utils,     only: mpicom
  use mpi,            only: mpi_integer, mpi_sum
  use dimensions_mod, only: nelemd

  integer :: ierr

  if (nelem_tot < 0) then
    call MPI_Allreduce(nelemd, nelem_tot, 1, MPI_INTEGER, MPI_SUM, mpicom, ierr)
  end if
end subroutine init_nelem_tot

subroutine init_restart_dynamics(file, dyn_out)
   use hycoef,           only: init_restart_hycoef
   use cam_ccpp_cap,              only: cam_model_const_properties
   use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t
   use pio, only: file_desc_t, pio_global, pio_unlimited, pio_double, pio_seterrorhandling
   use pio, only: pio_bcast_error, pio_def_dim, pio_def_var, pio_put_att
   use dyn_comp, only: dyn_export_t, write_restart_unstruct
   use cam_grid_support, only: cam_grid_header_info_t, cam_grid_id
   use cam_grid_support, only: cam_grid_write_attr
   use dimensions_mod,   only: np, npsq, ne, nelemd, fv_nphys
   ! Define dimensions, variables, attributes for restart file.

   ! This is not really an "init" routine.  It is called before
   ! write_restart_dynamics every time an restart is written.

   ! arguments
   type(file_desc_t),  intent(inout) :: file
   type(dyn_export_t), intent(in)    :: dyn_out

   ! local variables
   integer :: i
   integer :: vdimids(2)
   integer :: nlev_dimid
   integer :: ncol_dimid
   integer :: ncol_fvm_dimid
   integer :: time_dimid

   integer :: ierr, err_handling

   integer :: grid_id
   type(cam_grid_header_info_t) :: info
   integer :: constituent_idx
   type(ccpp_constituent_prop_ptr_t), pointer :: const_props(:)
   character(len=256) :: const_diag_name

   !----------------------------------------------------------------------------

   call init_nelem_tot()
   call init_restart_hycoef(file, vdimids)
   nlev_dimid = vdimids(1)

   call pio_seterrorhandling(File, pio_bcast_error, err_handling)

   ierr = PIO_Def_Dim(File, 'time', PIO_UNLIMITED, time_dimid)

   ! GLL restart fields

   ! number of columns written to restart depends on whether all columns in the
   ! element structures are written, or just the unique columns (unstructured grid)
   if (write_restart_unstruct) then
      grid_id = cam_grid_id('GLL')
      call cam_grid_write_attr(File, grid_id, info)
      ncol_dimid = info%get_hdimid(1)
   else
      ierr = PIO_Def_Dim(File,'nenpnp', nelem_tot*np*np, ncol_dimid)
      ierr = PIO_Put_Att(File, PIO_GLOBAL, 'ne', ne)
      ierr = PIO_Put_Att(File, PIO_GLOBAL, 'np', np)
   end if

   ierr = PIO_Def_Var(File, 'PSDRY', pio_double, (/ncol_dimid, time_dimid/), psdry_desc)
   ierr = PIO_Def_Var(File, 'U', pio_double, (/ncol_dimid, nlev_dimid, time_dimid/), Udesc)
   ierr = PIO_Def_Var(File, 'V', pio_double, (/ncol_dimid, nlev_dimid, time_dimid/), Vdesc)
   ierr = PIO_Def_Var(File, 'T', pio_double, (/ncol_dimid, nlev_dimid, time_dimid/), Tdesc)

   const_props => cam_model_const_properties()
   allocate(qdesc_dp(size(const_props)))
   do constituent_idx = 1, size(const_props)
      ! Grab constituent diagnostic name:
      call const_props(constituent_idx)%diagnostic_name(const_diag_name)
      ierr = PIO_Def_Var(File,"dp"//trim(const_diag_name), pio_double, &
                         (/ncol_dimid, nlev_dimid, time_dimid/), Qdesc_dp(constituent_idx))
   end do

   ! CSLAM restart fields

   if (fv_nphys > 0) then

      grid_id = cam_grid_id('FVM')
      call cam_grid_write_attr(File, grid_id, info)
      ncol_fvm_dimid = info%get_hdimid(1)

      ierr = PIO_Def_Var(File, 'dp_fvm', pio_double, &
         (/ncol_fvm_dimid, nlev_dimid, time_dimid/), dp_fvm_desc)

      allocate(c_fvm_desc(size(const_props)))
      do constituent_idx = 1, size(const_props)
         call const_props(constituent_idx)%diagnostic_name(const_diag_name)
         ierr = PIO_Def_Var(File, trim(const_diag_name)//"_fvm", pio_double, &
            (/ncol_fvm_dimid, nlev_dimid, time_dimid/), c_fvm_desc(constituent_idx))
      end do

   end if

   call pio_seterrorhandling(File, err_handling)

end subroutine init_restart_dynamics

!=========================================================================================

subroutine write_restart_dynamics(File, dyn_out)
  use control_mod,               only: qsplit
  use pio,                       only: file_desc_t, pio_offset_kind, io_desc_t, pio_double
  use pio,                       only: pio_initdecomp, pio_freedecomp, pio_setframe, pio_write_darray
  use dyn_comp,                  only: dyn_export_t, write_restart_unstruct
  use dyn_grid,                  only: timelevel
  use cam_grid_support,          only: cam_grid_id, cam_grid_write_var, cam_grid_get_decomp, cam_grid_dimensions
  use element_mod,               only: element_t
  use fvm_control_volume_mod,    only: fvm_struct
  use hycoef,                    only: write_restart_hycoef
  use time_mod,                  only: TimeLevel_Qdp
  use parallel_mod,              only: par
  use dimensions_mod,            only: nc, np, npsq, ne, nelemd, fv_nphys, nlev
  use cam_ccpp_cap,              only: cam_model_const_properties
  use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t
  use cam_pio_utils,             only: pio_subsystem
  use thread_mod,                only: horz_num_threads
  use spmd_utils,                only: iam
  use shr_kind_mod,              only: r8 => shr_kind_r8

   type(file_desc_t), intent(inout) :: File
   type(dyn_export_t), intent(in)   :: dyn_out

   ! local variables
   integer(pio_offset_kind), parameter :: t_idx = 1

   type(element_t),  pointer :: elem(:)
   type(fvm_struct), pointer :: fvm(:)

   integer :: tl, tlqdp
   integer :: i, ie, ii, j, k, m
   integer :: ierr

   integer :: grid_id
   integer :: grid_dimlens(2)



   integer :: array_lens(3)
   integer :: file_lens(2)
   type(io_desc_t), pointer :: iodesc3d_fvm
   real(r8),    allocatable :: buf3d(:,:,:)
   type(ccpp_constituent_prop_ptr_t), pointer :: const_props(:)



   character(len=*), parameter :: sub = 'write_restart_dynamics'
   !----------------------------------------------------------------------------

   call write_restart_hycoef(File)

   tl = timelevel%n0
   call TimeLevel_Qdp(timelevel, qsplit, tlQdp)
   const_props => cam_model_const_properties()

   if (iam .lt. par%nprocs) then
      elem => dyn_out%elem
      fvm => dyn_out%fvm
   else
      allocate (elem(0), fvm(0))
   endif

   ! write fields on GLL grid

   if (write_restart_unstruct) then
      call write_unstruct()
   else
      call write_elem()
   end if

   ! write CSLAM fields

   if (fv_nphys > 0) then

      grid_id = cam_grid_id('FVM')

      ! write coords for FVM grid
      call cam_grid_write_var(File, grid_id)

      call cam_grid_dimensions(grid_id, grid_dimlens)
      allocate(buf3d(nc*nc,nlev,nelemd))
      array_lens = (/nc*nc, nlev, nelemd/)
      file_lens  = (/grid_dimlens(1), nlev/)
      call cam_grid_get_decomp(grid_id, array_lens, file_lens, pio_double, iodesc3d_fvm)

      do ie = 1, nelemd
         do k = 1, nlev
            ii = 1
            do j = 1, nc
               do i = 1, nc
                  buf3d(ii,k,ie) = fvm(ie)%dp_fvm(i,j,k)
                  ii = ii + 1
               end do
            end do
         end do
      end do
      call PIO_Setframe(file, dp_fvm_desc, t_idx)
      call PIO_Write_Darray(file, dp_fvm_desc, iodesc3d_fvm, buf3d, ierr)

      do m = 1, size(const_props)
         do ie = 1, nelemd
            do k = 1, nlev
               ii = 1
               do j = 1, nc
                  do i = 1, nc
                     buf3d(ii,k,ie) = fvm(ie)%c(i,j,k,m)
                     ii = ii + 1
                  end do
               end do
            end do
         end do
         call PIO_Setframe(file, c_fvm_desc(m), t_idx)
         call PIO_Write_Darray(file, c_fvm_desc(m), iodesc3d_fvm, buf3d, ierr)
      end do

      deallocate(c_fvm_desc)
      deallocate(buf3d)
      ! should this call be made on a pointer?
      !call pio_freedecomp(File, iodesc3d_fvm)

   end if

   if (iam >= par%nprocs) then
      deallocate(elem, fvm)
   endif

!-------------------------------------------------------------------------------
contains
!-------------------------------------------------------------------------------

subroutine write_elem()

   ! local variables
   integer          :: i, ie, j, k
   integer          :: ierr
   integer, pointer :: ldof(:)

   type(io_desc_t)  :: iodesc2d, iodesc3d

   real(kind=r8), pointer :: var3d(:,:,:,:), var2d(:,:,:)
   !----------------------------------------------------------------------------

   ldof => get_restart_decomp(elem, 1)
   call PIO_InitDecomp(pio_subsystem, pio_double, (/nelem_tot*np*np/), ldof, iodesc2d)
   deallocate(ldof)

   ldof => get_restart_decomp(elem, nlev)
   call PIO_InitDecomp(pio_subsystem, pio_double, (/nelem_tot*np*np,nlev/), ldof, iodesc3d)
   deallocate(ldof)

   allocate(var2d(np,np,nelemd))
   allocate(var3d(np,np,nelemd,nlev))

   !$omp parallel do num_threads(horz_num_threads) private(ie, j, i)
   do ie = 1, nelemd
      do j = 1, np
         do i = 1, np
            var2d(i,j,ie) = elem(ie)%state%psdry(i,j)
         end do
      end do
   end do
   call PIO_Setframe(File, psdry_desc, t_idx)
   call PIO_Write_Darray(File, psdry_desc, iodesc2d, var2d, ierr)

   !$omp parallel do num_threads(horz_num_threads) private(ie, k, j, i)
   do ie = 1, nelemd
      do k = 1, nlev
         do j = 1, np
            do i = 1, np
               var3d(i,j,ie,k) = elem(ie)%state%V(i,j,1,k,tl)
            end do
         end do
      end do
   end do
   call PIO_Setframe(File, Udesc, t_idx)
   call PIO_Write_Darray(File, Udesc, iodesc3d, var3d, ierr)

   !$omp parallel do num_threads(horz_num_threads) private(ie, k, j, i)
   do ie = 1, nelemd
      do k = 1, nlev
         do j = 1, np
            do i = 1, np
               var3d(i,j,ie,k) = elem(ie)%state%V(i,j,2,k,tl)
            end do
         end do
      end do
   end do
   call PIO_Setframe(File, Vdesc, t_idx)
   call PIO_Write_Darray(File, Vdesc, iodesc3d, var3d, ierr)

   !$omp parallel do num_threads(horz_num_threads) private(ie, k, j, i)
   do ie = 1, nelemd
      do k = 1, nlev
         do j = 1, np
            do i = 1, np
               var3d(i,j,ie,k) = elem(ie)%state%T(i,j,k,tl)
            end do
         end do
      end do
   end do
   call PIO_Setframe(File, Tdesc, t_idx)
   call PIO_Write_Darray(File, Tdesc, iodesc3d, var3d, ierr)

   do m = 1, size(const_props)

      !$omp parallel do num_threads(horz_num_threads) private(ie, k, j, i)
      do ie = 1, nelemd
         do k = 1, nlev
            do j = 1, np
               do i = 1, np
                  var3d(i,j,ie,k) = elem(ie)%state%Qdp(i,j,k,m,tlQdp)
               end do
            end do
         end do
      end do
      call PIO_Setframe(File, Qdesc_dp(m), t_idx)
      call PIO_Write_Darray(File, Qdesc_dp(m), iodesc3d, var3d, ierr)

   end do

   deallocate(var2d)
   deallocate(var3d)
   deallocate(qdesc_dp)

   call pio_freedecomp(File, iodesc2d)
   call pio_freedecomp(File, iodesc3d)

end subroutine write_elem

!-------------------------------------------------------------------------------

subroutine write_unstruct()

   ! local variables
   integer          :: i, ie, ii, j, k
   integer          :: ierr

   integer :: array_lens_3d(3), array_lens_2d(2)
   integer :: file_lens_2d(2), file_lens_1d(1)

   type(io_desc_t), pointer :: iodesc
   real(r8),    allocatable :: var2d(:,:), var3d(:,:,:)
   !----------------------------------------------------------------------------

   grid_id = cam_grid_id('GLL')

   ! write coordinate variables for unstructured GLL grid
   call cam_grid_write_var(File, grid_id)

   ! create map for distributed write
   call cam_grid_dimensions(grid_id, grid_dimlens)

   ! create map for distributed write of 2D fields
   array_lens_2d = (/npsq, nelemd/)
   file_lens_1d  = (/grid_dimlens(1)/)
   call cam_grid_get_decomp(grid_id, array_lens_2d, file_lens_1d, pio_double, iodesc)

   allocate(var2d(npsq,nelemd))

   do ie = 1, nelemd
      ii = 1
      do j = 1, np
         do i = 1, np
            var2d(ii,ie) = elem(ie)%state%psdry(i,j)
            ii = ii + 1
         end do
      end do
   end do
   call PIO_Setframe(File, psdry_desc, t_idx)
   call PIO_Write_Darray(File, psdry_desc, iodesc, var2d, ierr)

   nullify(iodesc)
   deallocate(var2d)

   ! create map for distributed write of 3D fields
   array_lens_3d = (/npsq, nlev, nelemd/)
   file_lens_2d  = (/grid_dimlens(1), nlev/)
   call cam_grid_get_decomp(grid_id, array_lens_3d, file_lens_2d, pio_double, iodesc)

   allocate(var3d(npsq,nlev,nelemd))

   do ie = 1, nelemd
      do k = 1, nlev
         ii = 1
         do j = 1, np
            do i = 1, np
               var3d(ii,k,ie) = elem(ie)%state%V(i,j,1,k,tl)
               ii = ii + 1
            end do
         end do
      end do
   end do
   call PIO_Setframe(File, Udesc, t_idx)
   call PIO_Write_Darray(File, Udesc, iodesc, var3d, ierr)

   do ie = 1, nelemd
      do k = 1, nlev
         ii = 1
         do j = 1, np
            do i = 1, np
               var3d(ii,k,ie) = elem(ie)%state%V(i,j,2,k,tl)
               ii = ii + 1
            end do
         end do
      end do
   end do
   call PIO_Setframe(File, Vdesc, t_idx)
   call PIO_Write_Darray(File, Vdesc, iodesc, var3d, ierr)

   do ie = 1, nelemd
      do k = 1, nlev
         ii = 1
         do j = 1, np
            do i = 1, np
               var3d(ii,k,ie) = elem(ie)%state%T(i,j,k,tl)
               ii = ii + 1
            end do
         end do
      end do
   end do
   call PIO_Setframe(File, Tdesc, t_idx)
   call PIO_Write_Darray(File, Tdesc, iodesc, var3d, ierr)

   do m = 1, size(const_props)

      !$omp parallel do num_threads(horz_num_threads) private(ie, k, j, i)
      do ie = 1, nelemd
         do k = 1, nlev
            ii = 1
            do j = 1, np
               do i = 1, np
                  var3d(ii,k,ie) = elem(ie)%state%Qdp(i,j,k,m,tlQdp)
                  ii = ii + 1
               end do
            end do
         end do
      end do
      call PIO_Setframe(File, Qdesc_dp(m), t_idx)
      call PIO_Write_Darray(File, Qdesc_dp(m), iodesc, var3d, ierr)

   end do

   deallocate(var3d)
   deallocate(qdesc_dp)

end subroutine write_unstruct

!-------------------------------------------------------------------------------

end subroutine write_restart_dynamics

!=========================================================================================

!=========================================================================================
! Private
!=========================================================================================

function get_restart_decomp(elem, lev) result(ldof)
   use dimensions_mod, only: nelemd, np
   use element_mod,    only: element_t

   ! Get the integer mapping of a variable in the dynamics decomp in memory.
   ! The canonical ordering is as on the file. A 0 value indicates that the
   ! variable is not on the file (eg halo or boundary values)

   type(element_t), intent(in) :: elem(:)
   integer,         intent(in) :: lev
   integer,         pointer    :: ldof(:)

   integer :: i, j, k, ie
   !----------------------------------------------------------------------------

   allocate(ldof(nelemd*np*np*lev))

   j = 1
   do k = 1, lev
      do ie = 1, nelemd
         do i = 1, np*np
            ldof(j) = (elem(ie)%GlobalID-1)*np*np + (k-1)*nelem_tot*np*np + i
            j = j + 1
         end do
      end do
   end do

end function get_restart_decomp

!=========================================================================================

function get_restart_decomp_fvm(elem, lev) result(ldof)
   use dimensions_mod, only: nc, nelemd
   use element_mod,    only: element_t

   type(element_t), intent(in) :: elem(:)
   integer,         intent(in) :: lev
   integer,         pointer    :: ldof(:)

   integer :: i, j, k, ie
   !----------------------------------------------------------------------------

   allocate(ldof(nelemd*nc*nc*lev))

   j = 1
   do k = 1, lev
      do ie = 1, nelemd
         do i = 1, nc*nc
            ldof(j) = (elem(ie)%GlobalID-1)*nc*nc + (k-1)*nelem_tot*nc*nc + i
            j = j + 1
         end do
      end do
   end do

end function get_restart_decomp_fvm

!=========================================================================================

end module restart_dynamics
