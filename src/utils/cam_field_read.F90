module cam_field_read

   !-----------------------------------------------------------------------
   !
   ! MODULE: cam_field_read
   !
   ! DESCRIPTION: Interfaces to read fields from PIO files
   !
   !-----------------------------------------------------------------------

   use shr_kind_mod,   only: r8 => shr_kind_r8
   use shr_sys_mod,    only: shr_sys_flush
   use pio,            only: pio_offset_kind, file_desc_t, var_desc_t
   use pio,            only: pio_inq_dimid, pio_max_var_dims, io_desc_t
   use pio,            only: pio_double, pio_setframe
   use spmd_utils,     only: masterproc
   use cam_abortutils, only: endrun
   use cam_logfile,    only: iulog
   !!XXgoldyXX: v support SCAM?
   !  use shr_scam_mod,   only: shr_scam_getCloseLatLon  ! Standardized system subroutines
   !  use scamMod,        only: scmlat,scmlon,single_column
   !!XXgoldyXX: ^ support SCAM?
   !
   implicit none
   private

   logical :: debug = .true.
   !!XXgoldyXX: v support SCAM?
   integer :: single_column = .false.
   !!XXgoldyXX: ^ support SCAM?

   !
   !EOP
   !

   public :: cam_read_field

   interface cam_read_field
      module procedure infld_real8_1d
      module procedure infld_real8_2d
      module procedure infld_real8_3d
   end interface cam_read_field

!-----------------------------------------------------------------------
CONTAINS
!-----------------------------------------------------------------------

   subroutine get_grid_diminfo(grid_name, grid_id, dim1name, dim2name,        &
        dim_bounds)
      use cam_grid_support, only: cam_grid_id, cam_grid_get_dim_names
      use cam_grid_support, only: cam_grid_get_array_bounds
      use cam_grid_support, only: cam_grid_check

      ! Dummy arguments
      character(len=*), intent(in)    :: grid_name
      integer,          intent(out)   :: grid_id
      character(len=*), intent(out)   :: dim1name
      character(len=*), intent(out)   :: dim2name
      integer,          intent(inout) :: dim_bounds(:,:)

      ! Local variables
      character(len=*), parameter   :: subname = 'get_grid_dimname'

      grid_id = cam_grid_id(trim(grid_name))
      if (.not. cam_grid_check(grid_id)) then
         call endrun(subname//': Internal error, no "'//grid_name//'" grid')
      end if
      call cam_grid_get_dim_names(grid_id, dim1name, dim2name)
      call cam_grid_get_array_bounds(grid_id, dim_bounds)
   end subroutine get_grid_diminfo

   !
   ! ROUTINE: infld_real8_1d
   !
   subroutine infld_real8_1d(varname, ncid, field, readvar, gridname, timelevel)
      !
      ! infld_real8_1d:
      ! Netcdf I/O of 8-byte real field from netCDF file
      ! Field on file is either 1D or 2D
      ! Local array, <field> is 1D
      !

      use pio,              only: pio_get_var, pio_read_darray
      use pio,              only: pio_setdebuglevel
      use pio,              only: PIO_MAX_NAME, pio_inquire, pio_inq_dimname
      use cam_grid_support, only: cam_grid_get_decomp, cam_grid_is_unstructured
      use cam_grid_support, only: cam_grid_id, cam_grid_dimensions
      use cam_grid_support, only: max_hcoordname_len, cam_grid_is_block_indexed
      use cam_pio_utils,    only: cam_pio_check_var

      ! Dummy arguments
      character(len=*),           intent(in)    :: varname ! variable name
      type(file_desc_t),          intent(inout) :: ncid    ! input unit
      ! field: array to be returned (decomposed or global)
      real(r8),                   intent(inout) :: field(:)
      ! readvar: true => variable is on initial dataset
      logical,                    intent(out)   :: readvar
      ! gridname: Name of variable's grid (default 'physgrid')
      character(len=*), optional, intent(in)    :: gridname
      integer,          optional, intent(in)    :: timelevel
      !
      ! LOCAL VARIABLES:
      type(io_desc_t), pointer                  :: iodesc
      integer                                   :: grid_id ! grid ID
      integer                                   :: index, jndex
      ! ierr: return status
      integer                                   :: ierr
      ! varid: variable id
      type(var_desc_t)                          :: varid

      ! arraydimsize field dimension length
      integer                                   :: arraydimsize(1)

      ! ndims: The number of dimensions of the field in the file
      integer                                   :: ndims
      ! target_ndims: The number of expected dimensions for field on file
      integer                                   :: target_ndims
      ! dimids: file variable dims
      integer                                   :: dimids(PIO_MAX_VAR_DIMS)
      ! dimlens: file variable shape
      integer                                   :: dimlens(PIO_MAX_VAR_DIMS)
      integer                                   :: grid_dimlens(2)
      logical                                   :: block_indexed
      logical                                   :: unstruct
      ! Offsets for reading global variables
      ! strt: start col index for netcdf 1-d
      integer                                   :: strt(1) = 1
      ! cnt: ncol count for netcdf 1-d
      integer                                   :: cnt (1) = 1
      integer                                   :: dim_bounds(2, 2)
      character(len=PIO_MAX_NAME)               :: dim1name, dim2name
      character(len=PIO_MAX_NAME)               :: tmpname
      character(len=128)                        :: errormsg

      logical                                   :: readvar_tmp
      character(len=*),               parameter :: subname = 'INFLD_REAL8_1D'

      ! For SCAM
      real(r8)                  :: closelat, closelon
      integer                   :: lonidx, latidx

      nullify(iodesc)

      !
      !-----------------------------------------------------------------------
      !
      !    call pio_setdebuglevel(3)

      dim1name = ''
      dim2name = ''
      dim_bounds(:,1) = 0
      dim_bounds(:,2) = -1
      if (present(gridname)) then
         call get_grid_diminfo(trim(gridname), grid_id, dim1name, dim2name,  &
              dim_bounds)
      else
         call get_grid_diminfo('physgrid', grid_id, dim1name, dim2name,       &
              dim_bounds)
      end if

      ! Get the number of columns in the global grid.
      call cam_grid_dimensions(grid_id, grid_dimlens)
      ! Is the grid block indexed?
      block_indexed = cam_grid_is_block_indexed(grid_id)
      ! Is this an unstructured grid (i.e., one column dimension on file)?
      unstruct = cam_grid_is_unstructured(grid_id)
      if (block_indexed) then
         call endrun(subname//': Block indexed 1D field is invalid')
      else
         target_ndims = 1 ! 1D file ==> 1D field (logical 2D field)
      end if
      if (debug .and. masterproc) then
         if (present(gridname)) then
            write(iulog, '(5a)') subname, ': field = ', trim(varname),        &
                 ', grid = ',trim(gridname)
         else
            write(iulog, '(4a)') subname, ': field = ', trim(varname),        &
                 ', grid = physgrid'
         end if
         call shr_sys_flush(iulog)
      end if
      !
      ! Read netCDF file
      !
      !
      ! Check if field is on file; get netCDF variable id
      !
      call cam_pio_check_var(ncid, varname, varid, ndims, dimids, dimlens,    &
           readvar_tmp)
      !
      ! If field is on file:
      !
      if (readvar_tmp) then
         if (debug .and. masterproc) then
            if (ndims < 1) then
               call endrun(subname//': too few dimensions for '//trim(varname))
            else if (ndims == 1) then
               write(iulog, '(2a,3(i0,a))') subname, ': field(',              &
                    dim_bounds(1,1), ':', dim_bounds(1,2), '), file(',        &
                    dimlens(1), ')'
            else if (ndims == 2) then
               write(iulog, '(2a,4(i0,a))') subname, ': field(',              &
                    dim_bounds(1,1), ':', dim_bounds(1,2), '), file(',        &
                    dimlens(1), ', ', dimlens(2), ')'
            else
               write(errormsg, '(3a,i0)') ': too many dimensions for, ',      &
                    trim(varname), ', ', ndims
               call endrun(subname//trim(errormsg))
            end if
            call shr_sys_flush(iulog)
         end if
         ! Check to make sure that any 'extra' dimension is time
         if (ndims > target_ndims + 1) then
            call endrun(subname//': too many dimensions for '//trim(varname))
         else if (ndims == target_ndims + 1) then
            ierr = pio_inq_dimname(ncid, dimids(ndims), tmpname)
            if (trim(tmpname) /= 'time') then
               call endrun(subname//': dimension mismatch for '//trim(varname))
            end if
         else if (ndims < target_ndims) then
            call endrun(subname//': too few dimensions for '//trim(varname))
         end if ! No else, things are okay
         !
         ! Get array dimension id's and sizes
         !
         arraydimsize(1) = (dim_bounds(1,2) - dim_bounds(1,1) + 1)
         if (arraydimsize(1) /= size(field, 1)) then
            write(errormsg, '(4a,i0)') ': Mismatch between array bounds ',    &
                 'and field size for ', trim(varname), ', dimension ', 1
            call endrun(subname//errormsg)
         end if

         ! Check that the number of columns in the file matches the number of
         ! columns in the grid object.
         if (block_indexed) then
            if (dimlens(1) /= (grid_dimlens(1) * grid_dimlens(2))) then
               write(errormsg, '(2a,4(a,i0))') ': First dim mismatch for ',   &
                    trim(varname), ', file = ', dimlens(1), ', grid = ',      &
                    grid_dimlens(1), ' * ', grid_dimlens(2), ' = ',           &
                    (grid_dimlens(1) * grid_dimlens(2))
               call endrun(subname//trim(errormsg))
            end if
         else
            do jndex = 1, target_ndims
               if (dimlens(jndex) /= grid_dimlens(jndex)) then
                  write(errormsg, '(a,i0,2a,2(a,i0))') ': Dim ', jndex,       &
                       ' mismatch for ', trim(varname), ', file = ',          &
                       dimlens(jndex), 'grid = ', grid_dimlens(jndex)
                  call endrun(subname//trim(errormsg))
               end if
            end do
         end if

         if(ndims == target_ndims + 1) then
            if(present(timelevel)) then
               call pio_setframe(ncid, varid,                                 &
                    int(timelevel, kind=pio_offset_kind))
            else
               call pio_setframe(ncid, varid, int(1, kind=pio_offset_kind))
            end if
            ndims = ndims - 1
         end if

         ! NB: strt and cnt were initialized to 1
         if (single_column) then
            if (unstruct) then
               ! Clearly, this will not work for an unstructured dycore
               call endrun(subname//': SCAM not supported in this configuration')
            else
               call endrun(subname//': SCAM support not implemented')
            end if
         else
            ! All distributed array processing
            call cam_grid_get_decomp(grid_id, arraydimsize, dimlens(1:ndims), &
                 pio_double, iodesc)
            call pio_read_darray(ncid, varid, iodesc, field, ierr)
         end if

         if (masterproc) then
            write(iulog,*) subname//': read field '//trim(varname)
         end if

      end if  ! end of readvar_tmp

      readvar = readvar_tmp

   end subroutine infld_real8_1d

   !
   ! ROUTINE: infld_real8_2d
   !
   subroutine infld_real8_2d(varname, ncid, field, readvar, gridname,         &
        timelevel, dim3name, dim3_bnds)
      !
      ! infld_real8_2d:
      ! Netcdf I/O of 8-byte real field from netCDF file
      ! Field on file is either 2D or 3D
      ! Local array, <field> is 2D
      !

      use pio,              only: pio_get_var, pio_read_darray
      use pio,              only: pio_setdebuglevel
      use pio,              only: PIO_MAX_NAME, pio_inquire, pio_inq_dimname
      use cam_grid_support, only: cam_grid_get_decomp, cam_grid_is_unstructured
      use cam_grid_support, only: cam_grid_id, cam_grid_dimensions
      use cam_grid_support, only: max_hcoordname_len, cam_grid_is_block_indexed
      use cam_pio_utils,    only: cam_pio_check_var

      ! Dummy arguments
      character(len=*),  intent(in)    :: varname ! variable name
      type(file_desc_t), intent(inout) :: ncid    ! input unit
      ! field: array to be returned (decomposed or global)
      real(r8),          intent(inout)        :: field(:,:)
      ! readvar: true => variable is on initial dataset
      logical,                    intent(out) :: readvar
      ! gridname: Name of variable's grid (default 'physgrid')
      character(len=*), optional, intent(in)  :: gridname
      integer,          optional, intent(in)  :: timelevel
      ! dim3name: Name of vertical dimension, if field reprsents a 3D quantity
      character(len=*), optional, intent(in)  :: dim3name
      ! dim3_bnds: Bounds of vertical dimension, if field is 3D
      integer,          optional, intent(in)  :: dim3_bnds(2)
      !
      ! LOCAL VARIABLES:
      type(io_desc_t),  pointer               :: iodesc
      integer                                 :: grid_id ! grid ID
      integer                                 :: index, jndex
      ! ierr: return status
      integer                                 :: ierr
      ! varid: variable id
      type(var_desc_t)                        :: varid

      ! arraydimsize field dimension lengths
      integer                                 :: arraydimsize(2)

      ! ndims: The number of dimensions of the field in the file
      integer                                 :: ndims
      ! target_ndims: The number of expected dimensions for field on file
      integer                                 :: target_ndims
      ! dimids: file variable dims
      integer                                 :: dimids(PIO_MAX_VAR_DIMS)
      ! dimlens: file variable shape
      integer                                 :: dimlens(PIO_MAX_VAR_DIMS)
      integer                                 :: grid_dimlens(2)
      logical                                 :: block_indexed
      logical                                 :: unstruct
      ! Offsets for reading global variables
      ! strt: start col index for netcdf 1-d
      integer                                 :: strt(1) = 1
      ! cnt: ncol count for netcdf 1-d
      integer                                 :: cnt (1) = 1
      integer                                 :: dim_bounds(2, 2)
      character(len=PIO_MAX_NAME)             :: dim1name, dim2name
      character(len=PIO_MAX_NAME)             :: file_dnames(PIO_MAX_VAR_DIMS)
      character(len=PIO_MAX_NAME)             :: tmpname
      character(len=128)                      :: errormsg

      logical                                 :: readvar_tmp
      character(len=*), parameter             :: subname = 'INFLD_REAL8_2D'

      ! For SCAM
      real(r8)                  :: closelat, closelon
      integer                   :: lonidx, latidx

      nullify(iodesc)

      !
      !-----------------------------------------------------------------------
      !
      !    call pio_setdebuglevel(3)

      dim1name = ''
      dim2name = ''
      dim_bounds(:,1) = 0
      dim_bounds(:,2) = -1
      if (present(gridname)) then
         call get_grid_diminfo(trim(gridname), grid_id, dim1name, dim2name,  &
              dim_bounds)
      else
         call get_grid_diminfo('physgrid', grid_id, dim1name, dim2name,       &
              dim_bounds)
      end if

      ! Get the number of columns in the global grid.
      call cam_grid_dimensions(grid_id, grid_dimlens)
      ! Is the grid block indexed?
      block_indexed = cam_grid_is_block_indexed(grid_id)
      ! Is this an unstructured grid (i.e., one column dimension on file)?
      unstruct = cam_grid_is_unstructured(grid_id)
      if (block_indexed) then
         target_ndims = 1 ! 1D file ==> 2D field (logical 2D field)
      else
         target_ndims = 2 ! 2D file ==> 2D field (logical 2D or 3D field)
      end if
      if (debug .and. masterproc) then
         if (present(gridname)) then
            write(iulog, '(5a)') subname, ': field = ', trim(varname),        &
                 ', grid = ',trim(gridname)
         else
            write(iulog, '(4a)') subname, ': field = ', trim(varname),        &
                 ', grid = physgrid'
         end if
         call shr_sys_flush(iulog)
      end if
      !
      ! If <field> is a 3D quantity, fix up its dimensions
      if ((dim_bounds(2,2) <= 0) .or. (dim_bounds(2,2) < dim_bounds(2,1))) then
         if (.not. present(dim3name)) then
            call endrun(subname//': dim3name must be present for 3D field')
         else if (.not. present(dim3_bnds)) then
            call endrun(subname//': dim3_bnds must be present for 3D field')
         else if (.not. unstruct) then
            call endrun(subname//': 3D field requires unstructured grid')
         end if
         dim_bounds(2,:) = dim3_bnds(:)
         dim2name = trim(dim3name)
      end if
      !
      ! Read netCDF file
      !
      !
      ! Check if field is on file; get netCDF variable id
      !
      call cam_pio_check_var(ncid, varname, varid, ndims, dimids, dimlens,    &
           readvar_tmp, dimnames=file_dnames)
      !
      ! If field is on file:
      !
      if (readvar_tmp) then
         if (debug .and. masterproc) then
            if (ndims < 1) then
               call endrun(subname//': too few dimensions for '//trim(varname))
            else if (ndims == 2) then
               write(iulog, '(2a,6(i0,a))') subname, ': field(',              &
                    dim_bounds(1,1), ':', dim_bounds(1,2), ',',               &
                    dim_bounds(2,1), ':', dim_bounds(2,2), '), file(',        &
                    dimlens(1), ', ', dimlens(2), ')'
            else if (ndims == 3) then
               write(iulog, '(2a,7(i0,a))') subname, ': field(',              &
                    dim_bounds(1,1), ':', dim_bounds(1,2), ',',               &
                    dim_bounds(2,1), ':', dim_bounds(2,2), '), file(',        &
                    dimlens(1), ', ', dimlens(2), ', ', dimlens(3), ')'
            else
               write(errormsg, '(3a,i0)') ': too many dimensions for, ',      &
                    trim(varname), ', ', ndims
               call endrun(subname//trim(errormsg))
            end if
            call shr_sys_flush(iulog)
         end if
         ! Check to make sure that any 'extra' dimension is time
         if (ndims > target_ndims + 1) then
            call endrun(subname//': too many dimensions for '//trim(varname))
         else if (ndims == target_ndims + 1) then
            ierr = pio_inq_dimname(ncid, dimids(ndims), tmpname)
            if (trim(tmpname) /= 'time') then
               call endrun(subname//': dimension mismatch for '//trim(varname))
            end if
         else if (ndims < target_ndims) then
            call endrun(subname//': too few dimensions for '//trim(varname))
         end if ! No else, things are okay
         !
         ! Get array dimension id's and sizes
         !
         arraydimsize(1) = (dim_bounds(1,2) - dim_bounds(1,1) + 1)
         arraydimsize(2) = (dim_bounds(2,2) - dim_bounds(2,1) + 1)
         do jndex = 1, 2
            if (arraydimsize(jndex) /= size(field, jndex)) then
               write(errormsg, '(2(a,i0),3a,i0)')                             &
                    ': Mismatch between array size (', arraydimsize(jndex),   &
                    ') and field size (', size(field, jndex), ') for ',       &
                    trim(varname), ', dimension = ', jndex
               call endrun(subname//errormsg)
            end if
         end do

         ! Check that the number of columns in the file matches the number of
         ! columns in the grid object.
         if (block_indexed) then
            if (dimlens(1) /= (grid_dimlens(1) * grid_dimlens(2))) then
               write(errormsg, '(2a,4(a,i0))') ': First dim mismatch for ',   &
                    trim(varname), ', file = ', dimlens(1), ', grid = ',      &
                    grid_dimlens(1), ' * ', grid_dimlens(2), ' = ',           &
                    (grid_dimlens(1) * grid_dimlens(2))
               call endrun(subname//trim(errormsg))
            end if
         else
            index = 0
            do jndex = 1, target_ndims
               if (present(dim3name)) then
                  if (trim(file_dnames(jndex)) == trim(dim3name)) then
                     ! The vertical dimension may be in between array dims
                     index = -1
                     cycle ! Do not check the vertical dimension
                  end if
               end if
               if ( (grid_dimlens(jndex+index) > 1) .and.                     &
                    (dimlens(jndex) /= grid_dimlens(jndex+index))) then
                  write(errormsg, '(a,i0,2a,2(a,i0))') ': Dim ', jndex,       &
                       ' mismatch for ', trim(varname), ', file = ',          &
                       dimlens(jndex), 'grid = ', grid_dimlens(jndex+index)
                  call endrun(subname//trim(errormsg))
               end if
            end do
         end if

         if(ndims == target_ndims + 1) then
            if(present(timelevel)) then
               call pio_setframe(ncid, varid,                                 &
                    int(timelevel, kind=pio_offset_kind))
            else
               call pio_setframe(ncid, varid, int(1, kind=pio_offset_kind))
            end if
            ndims = ndims - 1
         end if

         ! NB: strt and cnt were initialized to 1
         if (single_column) then
            if (unstruct) then
               ! Clearly, this will not work for an unstructured dycore
               call endrun(subname//': SCAM not supported in this configuration')
            else
               call endrun(subname//': SCAM support not implemented')
            end if
         else
            ! All distributed array processing
            call cam_grid_get_decomp(grid_id, arraydimsize, dimlens(1:ndims), &
                 pio_double, iodesc, file_dnames=file_dnames(1:target_ndims))
            call pio_read_darray(ncid, varid, iodesc, field, ierr)
         end if

         if (masterproc) then
            write(iulog,*) subname//': read field '//trim(varname)
         end if

      end if  ! end of readvar_tmp

      readvar = readvar_tmp

      return

   end subroutine infld_real8_2d

   !
   ! ROUTINE: infld_real8_3d
   !
   subroutine infld_real8_3d(varname, ncid, field, readvar, dim3name,         &
        dim3_bnds, dim3_pos, gridname, timelevel)
      !
      ! infld_real8_3d:
      ! Netcdf I/O of 8-byte real field from netCDF file
      ! Field on file is 3D
      ! Local array, <field> is 3D
      !

      use pio,              only: pio_get_var, pio_read_darray
      use pio,              only: pio_setdebuglevel
      use pio,              only: PIO_MAX_NAME, pio_inquire, pio_inq_dimname
      use cam_grid_support, only: cam_grid_get_decomp, cam_grid_is_unstructured
      use cam_grid_support, only: cam_grid_id, cam_grid_dimensions
      use cam_grid_support, only: max_hcoordname_len, cam_grid_is_block_indexed
      use cam_pio_utils,    only: cam_pio_check_var

      ! Dummy arguments
      character(len=*),  intent(in)           :: varname ! variable name
      type(file_desc_t), intent(inout)        :: ncid    ! input unit
      ! field: array to be returned (decomposed or global)
      real(r8),          intent(inout)        :: field(:,:,:)
      ! readvar: true => variable is on initial dataset
      logical,                    intent(out) :: readvar
      ! dim3name: Name of vertical dimension, if field reprsents a 3D quantity
      character(len=*),           intent(in)  :: dim3name
      ! dim3_bnds: Bounds of vertical dimension, if field is 3D
      integer,                    intent(in)  :: dim3_bnds(2)
      integer,          optional, intent(in)  :: dim3_pos
      ! gridname: Name of variable's grid (default 'physgrid')
      character(len=*), optional, intent(in)  :: gridname
      integer,          optional, intent(in)  :: timelevel
      !
      ! LOCAL VARIABLES:
      type(io_desc_t),  pointer               :: iodesc
      integer                                 :: grid_id ! grid ID
      integer                                 :: index, jndex
      ! ierr: return status
      integer                                 :: ierr
      ! varid: variable id
      type(var_desc_t)                        :: varid

      ! arraydimsize field dimension lengths
      integer                                 :: arraydimsize(3)

      ! ndims: The number of dimensions of the field in the file
      integer                                 :: ndims
      ! target_ndims: The number of expected dimensions for field on file
      integer                                 :: target_ndims
      ! dimids: file variable dims
      integer                                 :: dimids(PIO_MAX_VAR_DIMS)
      ! dimlens: file variable shape
      integer                                 :: dimlens(PIO_MAX_VAR_DIMS)
      integer                                 :: grid_dimlens(2)
      logical                                 :: block_indexed
      logical                                 :: unstruct
      ! Offsets for reading global variables
      ! strt: start col index for netcdf 1-d
      integer                                 :: strt(1) = 1
      ! cnt: ncol count for netcdf 1-d
      integer                                 :: cnt (1) = 1
      integer                                 :: dim_bounds(3, 2)
      character(len=PIO_MAX_NAME)             :: dim1name, dim2name
      character(len=PIO_MAX_NAME)             :: file_dnames(PIO_MAX_VAR_DIMS)
      character(len=PIO_MAX_NAME)             :: tmpname
      character(len=128)                      :: errormsg

      logical                                 :: readvar_tmp
      character(len=*), parameter             :: subname = 'INFLD_REAL8_3D'

      ! For SCAM
      real(r8)                  :: closelat, closelon
      integer                   :: lonidx, latidx

      nullify(iodesc)

      !
      !-----------------------------------------------------------------------
      !
      !    call pio_setdebuglevel(3)

      dim1name = ''
      dim2name = ''
      dim_bounds(:,1) = 0
      dim_bounds(:,2) = -1
      if (present(gridname)) then
         call get_grid_diminfo(trim(gridname), grid_id, dim1name, dim2name,  &
              dim_bounds)
      else
         call get_grid_diminfo('physgrid', grid_id, dim1name, dim2name,       &
              dim_bounds)
      end if

      ! Get the number of columns in the global grid.
      call cam_grid_dimensions(grid_id, grid_dimlens)
      ! Is the grid block indexed?
      block_indexed = cam_grid_is_block_indexed(grid_id)
      ! Is this an unstructured grid (i.e., one column dimension on file)?
      unstruct = cam_grid_is_unstructured(grid_id)
      if (block_indexed) then
         target_ndims = 2 ! 2D file ==> 3D field (logical 3D field)
      else
         target_ndims = 3 ! 3D file ==> 3D field (logical 3D field)
      end if
      if (debug .and. masterproc) then
         if (present(gridname)) then
            write(iulog, '(5a)') subname, ': field = ', trim(varname),        &
                 ', grid = ',trim(gridname)
         else
            write(iulog, '(4a)') subname, ': field = ', trim(varname),        &
                 ', grid = physgrid'
         end if
         call shr_sys_flush(iulog)
      end if
      !
      ! Ensure that <field> is configured correctly
      if (dim_bounds(2,2) < dim_bounds(2,1)) then
         if (present(gridname)) then
            write(errormsg, *) ': grid, ', trim(gridname),                    &
                 ' invalid for3D field'
         else
            write(errormsg, *) ': grid, physgrid, invalid for3D field'
         end if
      else
         if (present(dim3_pos)) then
            if ((dim3_pos < 1) .or. (dim3_pos > 3)) then
               call endrun(subname//': Bad value for dim3_pos')
            end if
            index = dim3_pos
         else
            index = 2
         end if
         do jndex = 3, index + 1, -1
            dim_bounds(jndex,1) = dim_bounds(jndex - 1,1)
            dim_bounds(jndex,2) = dim_bounds(jndex - 1,2)
         end do
         dim_bounds(index,1) = dim3_bnds(1)
         dim_bounds(index,2) = dim3_bnds(2)
      end if
      !
      ! Read netCDF file
      !
      !
      ! Check if field is on file; get netCDF variable id
      !
      call cam_pio_check_var(ncid, varname, varid, ndims, dimids, dimlens,    &
           readvar_tmp, dimnames=file_dnames)
      !
      ! If field is on file:
      !
      if (readvar_tmp) then
         if (debug .and. masterproc) then
            if (ndims < 2) then
               call endrun(subname//': too few dimensions for '//trim(varname))
            else if (ndims == 2) then
               write(iulog, '(2a,8(i0,a))') subname, ': field(',              &
                    dim_bounds(1,1), ':', dim_bounds(1,2), ',',               &
                    dim_bounds(2,1), ':', dim_bounds(2,2), ',',               &
                    dim_bounds(3,1), ':', dim_bounds(3,2), '), file(',        &
                    dimlens(1), ', ', dimlens(2), ')'
            else if (ndims == 3) then
               write(iulog, '(2a,9(i0,a))') subname, ': field(',              &
                    dim_bounds(1,1), ':', dim_bounds(1,2), ',',               &
                    dim_bounds(2,1), ':', dim_bounds(2,2), ',',               &
                    dim_bounds(3,1), ':', dim_bounds(3,2), '), file(',        &
                    dimlens(1), ', ', dimlens(2), ', ', dimlens(3), ')'
            else
               write(errormsg, '(3a,i0)') ': too many dimensions for, ',      &
                    trim(varname), ', ', ndims
               call endrun(subname//trim(errormsg))
            end if
            call shr_sys_flush(iulog)
         end if
         ! Check to make sure that any 'extra' dimension is time
         if (ndims > target_ndims + 1) then
            call endrun(subname//': too many dimensions for '//trim(varname))
         else if (ndims == target_ndims + 1) then
            ierr = pio_inq_dimname(ncid, dimids(ndims), tmpname)
            if (trim(tmpname) /= 'time') then
               call endrun(subname//': dimension mismatch for '//trim(varname))
            end if
         else if (ndims < target_ndims) then
            call endrun(subname//': too few dimensions for '//trim(varname))
         end if ! No else, things are okay
         !
         ! Get array dimension id's and sizes
         !
         arraydimsize(1) = (dim_bounds(1,2) - dim_bounds(1,1) + 1)
         arraydimsize(2) = (dim_bounds(2,2) - dim_bounds(2,1) + 1)
         arraydimsize(3) = (dim_bounds(3,2) - dim_bounds(3,1) + 1)
         do jndex = 1, 3
            if (arraydimsize(jndex) /= size(field, jndex)) then
               write(errormsg, '(4a,i0)') ': Mismatch between array bounds ', &
                    'and field size for ', trim(varname), ', dimension ', jndex
               call endrun(subname//errormsg)
            end if
         end do

         ! Check that the number of columns in the file matches the number of
         ! columns in the grid object.
         if (block_indexed) then
            if (dimlens(1) /= (grid_dimlens(1) * grid_dimlens(2))) then
               write(errormsg, '(2a,4(a,i0))') ': First dim mismatch for ',   &
                    trim(varname), ', file = ', dimlens(1), ', grid = ',      &
                    grid_dimlens(1), ' * ', grid_dimlens(2), ' = ',           &
                    (grid_dimlens(1) * grid_dimlens(2))
               call endrun(subname//trim(errormsg))
            end if
         else
            do jndex = 1, target_ndims
               if (trim(file_dnames(jndex)) == trim(dim3name)) then
                  ! The vertical dimension may be in between array dims
                  index = -1
                  cycle ! Do not check the vertical dimension
               end if
               if ( (grid_dimlens(jndex+index) > 1) .and.                     &
                    (dimlens(jndex) /= grid_dimlens(jndex+index))) then
                  write(errormsg, '(a,i0,2a,2(a,i0))') ': Dim ', jndex,       &
                       ' mismatch for ', trim(varname), ', file = ',          &
                       dimlens(jndex), 'grid = ', grid_dimlens(jndex+index)
                  call endrun(subname//trim(errormsg))
               end if
            end do
         end if

         if(ndims == target_ndims + 1) then
            if(present(timelevel)) then
               call pio_setframe(ncid, varid,                                 &
                    int(timelevel, kind=pio_offset_kind))
            else
               call pio_setframe(ncid, varid, int(1, kind=pio_offset_kind))
            end if
            ndims = ndims - 1
         end if

         ! NB: strt and cnt were initialized to 1
         if (single_column) then
            if (unstruct) then
               ! Clearly, this will not work for an unstructured dycore
               call endrun(subname//': SCAM not supported in this configuration')
            else
               call endrun(subname//': SCAM support not implemented')
            end if
         else
            ! All distributed array processing
            call cam_grid_get_decomp(grid_id, arraydimsize, dimlens(1:ndims), &
                 pio_double, iodesc, file_dnames=file_dnames(1:target_ndims))
            call pio_read_darray(ncid, varid, iodesc, field, ierr)
         end if

         if (masterproc) then
            write(iulog,*) subname//': read field '//trim(varname)
         end if

      end if  ! end of readvar_tmp

      readvar = readvar_tmp

      return

   end subroutine infld_real8_3d

end module cam_field_read
