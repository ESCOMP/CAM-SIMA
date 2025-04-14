! Copyright (C) 2025 National Science Foundation-National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
module sima_ccpp_FileIO

   !This module contains the specific CAM-SIMA
   !interfaces for the CCPP FileIO object, which
   !allows CCPP schemes to perform file I/O
   !with the same libraries CAM-SIMA uses (e.g. PIO).

   use pio,          only: file_desc_t
   use shr_kind_mod, only: cl=>shr_kind_cl

   implicit none

   private
   save

   !Internal SIMA data type used for PIO file handling:
   type :: sima_pio_fh_type
      logical            :: is_file_open = .false.  !Is NetCDF file currently open?
      type(file_desc_t)  :: pio_fh                  !PIO File handle type
      character(len=cl)  :: file_path               !Local path to NetCDF file
   end type


   !Module variables (need to be saved across timesteps):
   !--------------

   !Total number of open NetCDF file allowed at one time:
   integer, parameter :: max_open_netcdf_files = 100

   !Structure used to store/access open file handles:
   type(sima_pio_fh_type) :: sima_pio_fh_data(max_open_netcdf_files)

   !--------------

   !Define type-polymorphic module
   !procedure interface
   interface sima_get_netcdf_var
      module procedure sima_get_netcdf_var_int
      module procedure sima_get_netcdf_var_real
      module procedure sima_get_netcdf_var_char
   end interface

   !Declare public procedures
   public :: sima_open_netcdf_file
   public :: sima_close_netcdf_file
   public :: sima_get_netcdf_dim
   public :: sima_get_netcdf_var

!==============================================================================
contains
!==============================================================================

   subroutine sima_open_netcdf_file(file_path, file_id, errcode, errmsg)

      !Attempt to open a NetCDF file using
      !SIMA's I/O system (PIO).

      use ioFileMod,        only: cam_get_file
      use cam_pio_utils,    only: cam_pio_openfile
      use pio,              only: PIO_NOWRITE

      !----------------------
      !Input variables:
      character(len=*), intent(in) :: file_path !Path to file (could be relative path).

      !Output variables:
      integer, intent(out)          :: file_id   !PIO file handle array ID
      integer, intent(out)          :: errcode   !Error code
      character(len=*), intent(out) :: errmsg    !Error message

      !Local variables:
      integer            :: file_num         !Loop control variable for PIO file handle array
      type(file_desc_t)  :: file_handle      !PIO file handle
      character(len=cl)  :: local_file_path  !NetCDF file path on local file system
      !----------------------

      !Find File array ID that is not currently in use:
      file_id = -1
      do file_num=1, max_open_netcdf_files
         if(.not.sima_pio_fh_data(file_num)%is_file_open) then
            file_id = file_num
            exit
         end if
      end do

      !If no remaining array indices are available then
      !error out:
      if(file_id == -1) then
         errcode = 1
         errmsg  = "Too many files currently open in SIMA, please close a file or increase 'max_open_netcdf_files'."
      end if

      call cam_get_file(file_path, local_file_path)
      call cam_pio_openfile(file_handle, local_file_path, PIO_NOWRITE)

      !Add new file handle to dictionary:
      sima_pio_fh_data(file_id)%pio_fh       = file_handle
      sima_pio_fh_data(file_id)%file_path    = local_file_path
      sima_pio_fh_data(file_id)%is_file_open = .true.

      !File was successfully opened,
      !so properly set the error code
      !and message:
      errcode = 0
      errmsg = ''

   end subroutine sima_open_netcdf_file

   !+++++++++++++++++++++++++++++++++++++

   subroutine sima_close_netcdf_file(file_id, errcode, errmsg)

      !Closes a NetCDF file using SIMA's I/O system (PIO).
      !It also resets the File Handle array ID to a "bad"
      !value.

      use pio, only: pio_closefile

      !----------------------
      !Input/Output variables:
      integer, intent(inout)        :: file_id !PIO file handle array ID

      !Output variables:
      integer, intent(out)          :: errcode !Error code
      character(len=*), intent(out) :: errmsg  !Error message

      !Local variables:
      type(file_desc_t)  :: file_handle        !PIO file handle
      character(len=cl)  :: local_file_path    !NetCDF file path on local file system
      !----------------------

      !Close NetCDF File:
      call pio_closefile(sima_pio_fh_data(file_id)%pio_fh)

      !Inidcate that file handle array id is no longer in use:
      sima_pio_fh_data(file_id)%is_file_open = .false.

      !Set the current File ID to a "bad" value:
      file_id = -1

      !File was successfully closed,
      !so properly set the error code
      !and message:
      errcode = 0
      errmsg = ''

   end subroutine sima_close_netcdf_file

   !+++++++++++++++++++++++++++++++++++++

   subroutine sima_get_netcdf_dim(file_id, dimname, errcode, errmsg, dimlen)

      !Check for and then read a NetCDF dimension length
      !into the provided output variable using SIMA's
      !I/O system (PIO).

      use pio, only: pio_inq_dimid
      use pio, only: pio_seterrorhandling
      use pio, only: pio_inq_dimlen
      use pio, only: PIO_NOERR
      use pio, only: PIO_BCAST_ERROR

      !----------------------
      !Input variables:
      integer, intent(in)          :: file_id         !PIO file handle array ID
      !type(file_desc_t), intent(inout) :: pio_file_handle !File ID type used by PIO
      !character(len=*),  intent(in)    :: file_path       !Path to NetCDF file
      character(len=*),  intent(in):: dimname         !variable name

      !Output variables:
      integer, intent(out), optional   :: dimlen          !Dimension length
      integer, intent(out)             :: errcode         !Error code
      character(len=*), intent(out)    :: errmsg          !Error message

      !Local variables:
      type(file_desc_t) :: pio_file_handle !File handle type used by PIO
      character(len=cl)  :: file_path      !Path to NetCDF file
      integer           :: err_handling    ! PIO error handling code
      integer           :: dim_id          ! NetCDF dimension ID
      !----------------------

      !Check if output vaiable was requested,
      !if not then just return with passing
      !error code.
      if(.not.present(dimlen)) then
         errcode = 0
         errmsg  = ''
         return
      end if

      !Extract open file information:
      pio_file_handle = sima_pio_fh_data(file_id)%pio_fh
      file_path       = sima_pio_fh_data(file_id)%file_path

      !Force PIO to send an error code instead of dying:
      call pio_seterrorhandling(pio_file_handle, PIO_BCAST_ERROR, oldmethod=err_handling)

      !Look for dimension on file:
      errcode = pio_inq_dimid(pio_file_handle, dimname, dim_id)
      if(errcode /= PIO_NOERR) then
         errcode = 2 !Make sure error code is non-zero
         errmsg = "Failed to find '"//dimname//"' in "//file_path
         if(present(dimlen)) then
            dimlen = -1
         end if
         !Reset PIO back to original error handling method:
         call pio_seterrorhandling(pio_file_handle, err_handling)
         return
      end if

      !Find dimension size/length if requested:
      if(present(dimlen)) then
         errcode = pio_inq_dimlen(pio_file_handle, dim_id, dimlen)
         if (errcode /= PIO_NOERR) then
            errcode = 3 !Make sure error code is non-zero
            errmsg  = "Failed to failed '"//dimname//"' length in "//file_path
            dimlen  = -1
            !Reset PIO back to original error handling method:
            call pio_seterrorhandling(pio_file_handle, err_handling)
            return
         end if
      end if

      !Reset PIO back to original error handling method:
      call pio_seterrorhandling(pio_file_handle, err_handling)

      !Variable was successfully read, so properly set the error
      !code and message:
      errcode = 0
      errmsg  = ''

   end subroutine sima_get_netcdf_dim

   !+++++++++++++++++++++++++++++++++++++

   subroutine sima_get_netcdf_var_int(file_id, varname, var, errcode, errmsg)

      !Check for and then read an INTEGER-type NetCDF variable
      !into the provided output variable using SIMA's I/O system (PIO).

      use ccpp_kinds, only: kind_phys
      use pio,        only: pio_inq_varid
      use pio,        only: pio_inq_dimlen
      use pio,        only: pio_inquire_variable
      use pio,        only: pio_seterrorhandling
      use pio,        only: pio_get_var
      use pio,        only: PIO_NOERR
      use pio,        only: PIO_BCAST_ERROR

      !----------------------
      !Input variables:
      integer, intent(in)           :: file_id  !PIO file handle array ID
      character(len=*),  intent(in) :: varname  !variable name

      !Output variables:
      integer, pointer, intent(out) :: var(..)             !Integer variable that file data will be read to.
                                                           !Note that it is assumed-rank variable
      integer, intent(out)              :: errcode         !Error code
      character(len=*), intent(out)     :: errmsg          !Error message

      !Local variables:
      type(file_desc_t)    :: pio_file_handle !File handle type used by PIO
      character(len=cl)    :: file_path       !Path to NetCDF file
      integer              :: err_handling    !PIO error handling code
      integer              :: var_id          !NetCDF variable ID
      integer              :: ndims           !Number of variable dimensions on NetCDF file
      integer, allocatable :: dim_ids(:)      !Variable dimension IDs
      integer, allocatable :: dim_sizes(:)    !Variable dimension sizes

      integer :: i !loop control variable
      !----------------------

      !Extract open file information:
      pio_file_handle = sima_pio_fh_data(file_id)%pio_fh
      file_path       = sima_pio_fh_data(file_id)%file_path

      !Force PIO to send an error code instead of dying:
      call pio_seterrorhandling(pio_file_handle, PIO_BCAST_ERROR, oldmethod=err_handling)

      !Look for variable on file:
      errcode = pio_inq_varid(pio_file_handle, varname, var_id)
      if(errcode /= PIO_NOERR) then
         errcode = 4 !Make sure error code is non-zero
         errmsg  = "Failed to find '"//varname//"' in "//file_path
         !Reset PIO back to original error handling method:
         call pio_seterrorhandling(pio_file_handle, err_handling)
         return
      end if

      !Get number of variable dimensions on file:
      errcode = pio_inquire_variable(pio_file_handle, var_id, ndims=ndims)
      if(errcode /= PIO_NOERR) then
         errcode = 5 !Make sure error code is non-zero
         errmsg  = "Failed to find number of dimensions for '"//varname//"' in "//file_path
         !Reset PIO back to original error handling method:
         call pio_seterrorhandling(pio_file_handle, err_handling)
         return
      end if

      !Check that the variable rank as specified by the caller
      !matches what is found on the NetCDF file:
      errcode = 0
      select rank(var)
         rank(0)
            if(ndims /= 0) errcode = 6
         rank(1)
            if(ndims /= 1) errcode = 6
         rank(2)
            if(ndims /= 2) errcode = 6
         rank(3)
            if(ndims /= 3) errcode = 6
         rank(4)
            if(ndims /= 4) errcode = 6
         rank(5)
            if(ndims /= 5) errcode = 6
         rank default
            !PIO can only handle up to 5 dimensions,
            !so error out if array rank is greater than that.
            errcode = 7
            errmsg  = "Unsupported rank for variable '"//varname//"'"
            return
      end select
      if(errcode == 6) then
         errmsg  = "Variable '"//varname//"' isn't declared with the correct number of dimensions"
         !Reset PIO back to original error handling method:
         call pio_seterrorhandling(pio_file_handle, err_handling)
         return
      end if

      !Get variable dimension sizes, if applicable:
      if(ndims > 0) then
         !Allocate NetCDF variable dimension ID array:
         allocate(dim_ids(ndims), stat=errcode, errmsg=errmsg)
         if(errcode /= 0) then
            !Reset PIO back to original error handling method:
            call pio_seterrorhandling(pio_file_handle, err_handling)
            return
         end if

         !Get variable dimension IDs:
         errcode = pio_inquire_variable(pio_file_handle, var_id, dimids=dim_ids)

         !Allocate NetCDF variable dimension sizes array:
         allocate(dim_sizes(ndims), stat=errcode, errmsg=errmsg)
         if(errcode /= 0) then
            !Reset PIO back to original error handling method:
            call pio_seterrorhandling(pio_file_handle, err_handling)
            return
         end if

         !Get dimension sizes:
         do i = 1, ndims
            errcode = pio_inq_dimlen(pio_file_handle, dim_ids(i), dim_sizes(i))
            if(errcode /= PIO_NOERR) then
               errmsg  = "Failed to find dimension sizes for '"//varname//"' in "//file_path
               !Reset PIO back to original error handling method:
               call pio_seterrorhandling(pio_file_handle, err_handling)
               return
            end if
         end do
      end if

      !Now attempt to allocate and initialize variable, and
      !read-in the NetCDF data:
      select rank(var)
         rank(0)

            var = huge(1)
            errcode = pio_get_var(pio_file_handle, var_id, var)

         rank(1)

            allocate(var(dim_sizes(1)), stat=errcode, errmsg=errmsg)
            if(errcode /= 0) then
               !Reset PIO back to original error handling method:
               call pio_seterrorhandling(pio_file_handle, err_handling)
               return
            end if
            var(:) = huge(1)
            errcode = pio_get_var(pio_file_handle, var_id, var(:))

         rank(2)

            allocate(var(dim_sizes(1), dim_sizes(2)), stat=errcode, errmsg=errmsg)
            if(errcode /= 0) then
               !Reset PIO back to original error handling method:
               call pio_seterrorhandling(pio_file_handle, err_handling)
               return
            end if
            var(:,:) = huge(1)
            errcode = pio_get_var(pio_file_handle, var_id, var(:,:))

         rank(3)

            allocate(var(dim_sizes(1), dim_sizes(2), dim_sizes(3)), stat=errcode, errmsg=errmsg)
            if(errcode /= 0) then
               !Reset PIO back to original error handling method:
               call pio_seterrorhandling(pio_file_handle, err_handling)
               return
            end if
            var(:,:,:) = huge(1)
            errcode = pio_get_var(pio_file_handle, var_id, var(:,:,:))

         rank(4)

            allocate(var(dim_sizes(1), dim_sizes(2), dim_sizes(3), dim_sizes(4)), stat=errcode, errmsg=errmsg)
            if(errcode /= 0) then
               !Reset PIO back to original error handling method:
               call pio_seterrorhandling(pio_file_handle, err_handling)
               return
            end if
            var(:,:,:,:) = huge(1)
            errcode = pio_get_var(pio_file_handle, var_id, var(:,:,:,:))

         rank(5)

            allocate(var(dim_sizes(1), dim_sizes(2), dim_sizes(3), dim_sizes(4), dim_sizes(5)), &
                     stat=errcode, errmsg=errmsg)
            if(errcode /= 0) then
               !Reset PIO back to original error handling method:
               call pio_seterrorhandling(pio_file_handle, err_handling)
               return
            end if
            var(:,:,:,:,:) = huge(1)
            errcode = pio_get_var(pio_file_handle, var_id, var(:,:,:,:,:))

         !No default needed as it was already checked above.
      end select

      if (errcode /= PIO_NOERR) then
         errcode = 8 !Make sure error code is non-zero
         errmsg  = "Failed to read '"//varname//"' from "//file_path
         !Reset PIO back to original error handling method:
         call pio_seterrorhandling(pio_file_handle, err_handling)
         return
      end if

      !Reset PIO back to original error handling method:
      call pio_seterrorhandling(pio_file_handle, err_handling)

      !Variable was successfully read, so properly set the error
      !code and message:
      errcode = 0
      errmsg = ''

   end subroutine sima_get_netcdf_var_int

   !+++++++++++++++++++++++++++++++++++++

   subroutine sima_get_netcdf_var_real(file_id, varname, var, errcode, errmsg)

      !Check for and then read a REAL-type NetCDF variable into
      !the provided output variable using SIMA's I/O system (PIO).

      use ccpp_kinds, only: kind_phys
      use pio,        only: pio_inq_varid
      use pio,        only: pio_inq_dimlen
      use pio,        only: pio_inquire_variable
      use pio,        only: pio_seterrorhandling
      use pio,        only: pio_get_var
      use pio,        only: PIO_NOERR
      use pio,        only: PIO_BCAST_ERROR

      !----------------------
      !Input variables:
      integer, intent(in)           :: file_id !PIO file handle array ID
      character(len=*),  intent(in) :: varname !variable name

      !Output variables:
      real(kind_phys), pointer, intent(out) :: var(..)      !Real variable that file data will be read to.
                                                            !Note that it is an assumed-rank variable.
      integer, intent(out)                      :: errcode  !Error code
      character(len=*), intent(out)             :: errmsg   !Error message

      !Local variables:
      type(file_desc_t)    :: pio_file_handle !File handle type used by PIO
      character(len=cl)    :: file_path       !Path to NetCDF file
      integer              :: err_handling    !PIO error handling code
      integer              :: var_id          !NetCDF variable ID
      integer              :: ndims           !Number of variable dimensions on NetCDF file
      integer, allocatable :: dim_ids(:)      !Variable dimension IDs
      integer, allocatable :: dim_sizes(:)    !Variable dimension sizes

      integer :: i !loop control variable
      !----------------------

      !Extract open file information:
      pio_file_handle = sima_pio_fh_data(file_id)%pio_fh
      file_path       = sima_pio_fh_data(file_id)%file_path

      !Force PIO to send an error code instead of dying:
      call pio_seterrorhandling(pio_file_handle, PIO_BCAST_ERROR, oldmethod=err_handling)

      !Look for variable on file:
      errcode = pio_inq_varid(pio_file_handle, varname, var_id)
      if (errcode /= PIO_NOERR) then
         errcode = 5 !Make sure error code is non-zero
         errmsg  = "Failed to find '"//varname//"' in "//file_path
         !Reset PIO back to original error handling method:
         call pio_seterrorhandling(pio_file_handle, err_handling)
         return
      end if

      !Get number of variable dimensions on file:
      errcode = pio_inquire_variable(pio_file_handle, var_id, ndims=ndims)
      if(errcode /= PIO_NOERR) then
         errcode = 5 !Make sure error code is non-zero
         errmsg  = "Failed to find number of dimensions for '"//varname//"' in "//file_path
         !Reset PIO back to original error handling method:
         call pio_seterrorhandling(pio_file_handle, err_handling)
         return
      end if

      !Check that the variable rank as specified by the caller
      !matches what is found on the NetCDF file:
      errcode = 0
      select rank(var)
         rank(0)
            if(ndims /= 0) errcode = 6
         rank(1)
            if(ndims /= 1) errcode = 6
         rank(2)
            if(ndims /= 2) errcode = 6
         rank(3)
            if(ndims /= 3) errcode = 6
         rank(4)
            if(ndims /= 4) errcode = 6
         rank(5)
            if(ndims /= 5) errcode = 6
         rank default
            !PIO can only handle up to 5 dimensions,
            !so error out if array rank is greater than that.
            errcode = 7
            errmsg  = "Unsupported rank for variable '"//varname//"'"
            return
      end select
      if(errcode == 6) then
         errmsg  = "Variable '"//varname//"' isn't declared with the correct number of dimensions"
         !Reset PIO back to original error handling method:
         call pio_seterrorhandling(pio_file_handle, err_handling)
         return
      end if

      !Get variable dimension sizes, if applicable:
      if(ndims > 0) then
         !Allocate NetCDF variable dimension ID array:
         allocate(dim_ids(ndims), stat=errcode, errmsg=errmsg)
         if(errcode /= 0) then
            !Reset PIO back to original error handling method:
            call pio_seterrorhandling(pio_file_handle, err_handling)
            return
         end if

         !Get variable dimension IDs:
         errcode = pio_inquire_variable(pio_file_handle, var_id, dimids=dim_ids)

         !Allocate NetCDF variable dimension sizes array:
         allocate(dim_sizes(ndims), stat=errcode, errmsg=errmsg)
         if(errcode /= 0) then
            !Reset PIO back to original error handling method:
            call pio_seterrorhandling(pio_file_handle, err_handling)
            return
         end if

         !Get dimension sizes:
         do i = 1, ndims
            errcode = pio_inq_dimlen(pio_file_handle, dim_ids(i), dim_sizes(i))
            if(errcode /= PIO_NOERR) then
               errmsg  = "Failed to find dimension sizes for '"//varname//"' in "//file_path
               !Reset PIO back to original error handling method:
               call pio_seterrorhandling(pio_file_handle, err_handling)
               return
            end if
         end do

      end if

      !Now attempt to allocate and initialize variable, and
      !read-in the NetCDF data:
      select rank(var)
         rank(0)

            var = huge(1._kind_phys)
            errcode = pio_get_var(pio_file_handle, var_id, var)

         rank(1)

            allocate(var(dim_sizes(1)), stat=errcode, errmsg=errmsg)
            if(errcode /= 0) then
               !Reset PIO back to original error handling method:
               call pio_seterrorhandling(pio_file_handle, err_handling)
               return
            end if
            var(:) = huge(1._kind_phys)
            errcode = pio_get_var(pio_file_handle, var_id, var(:))

         rank(2)

            allocate(var(dim_sizes(1), dim_sizes(2)), stat=errcode, errmsg=errmsg)
            if(errcode /= 0) then
               !Reset PIO back to original error handling method:
               call pio_seterrorhandling(pio_file_handle, err_handling)
               return
            end if
            var(:,:) = huge(1._kind_phys)
            errcode = pio_get_var(pio_file_handle, var_id, var(:,:))

         rank(3)

            allocate(var(dim_sizes(1), dim_sizes(2), dim_sizes(3)), stat=errcode, errmsg=errmsg)
            if(errcode /= 0) then
               !Reset PIO back to original error handling method:
               call pio_seterrorhandling(pio_file_handle, err_handling)
               return
            end if
            var(:,:,:) = huge(1._kind_phys)
            errcode = pio_get_var(pio_file_handle, var_id, var(:,:,:))

         rank(4)

            allocate(var(dim_sizes(1), dim_sizes(2), dim_sizes(3), dim_sizes(4)), stat=errcode, errmsg=errmsg)
            if(errcode /= 0) then
               !Reset PIO back to original error handling method:
               call pio_seterrorhandling(pio_file_handle, err_handling)
               return
            end if
            var(:,:,:,:) = huge(1._kind_phys)
            errcode = pio_get_var(pio_file_handle, var_id, var(:,:,:,:))

         rank(5)

            allocate(var(dim_sizes(1), dim_sizes(2), dim_sizes(3), dim_sizes(4), dim_sizes(5)), &
                     stat=errcode, errmsg=errmsg)
            if(errcode /= 0) then
               !Reset PIO back to original error handling method:
               call pio_seterrorhandling(pio_file_handle, err_handling)
               return
            end if
            var(:,:,:,:,:) = huge(1._kind_phys)
            errcode = pio_get_var(pio_file_handle, var_id, var(:,:,:,:,:))

         !No default needed as it was already checked above.
      end select

      if (errcode /= PIO_NOERR) then
         errcode = 8 !Make sure error code is non-zero
         errmsg  = "Failed to read '"//varname//"' from "//file_path
         !Reset PIO back to original error handling method:
         call pio_seterrorhandling(pio_file_handle, err_handling)
         return
      end if

      !Reset PIO back to original error handling method:
      call pio_seterrorhandling(pio_file_handle, err_handling)

      !Variable was successfully read, so properly set the error
      !code and message:
      errcode = 0
      errmsg = ''

   end subroutine sima_get_netcdf_var_real

   !+++++++++++++++++++++++++++++++++++++

   subroutine sima_get_netcdf_var_char(file_id, varname, var, errcode, errmsg)

      !Check for and then read a CHARACTER-type NetCDF variable into
      !the provided output variable using SIMA's I/O system (PIO).

      use ccpp_kinds, only: kind_phys
      use pio,        only: pio_inq_varid
      use pio,        only: pio_inq_dimlen
      use pio,        only: pio_inquire_variable
      use pio,        only: pio_seterrorhandling
      use pio,        only: pio_get_var
      use pio,        only: PIO_NOERR
      use pio,        only: PIO_BCAST_ERROR

      use netcdf,     only: NF90_CHAR
      !----------------------
      !Input variables:
      integer, intent(in)           :: file_id   !PIO file handle array ID
      character(len=*),  intent(in) :: varname   !variable name

      !Output variables:
      character(len=:), pointer, intent(out) :: var(..) !Character variable that file data will be read to.
                                                        !Note that it is an assumed-rank variable
      integer, intent(out)                   :: errcode !Error code
      character(len=*), intent(out)          :: errmsg  !Error message

      !Local variables:
      type(file_desc_t)    :: pio_file_handle !File handle type used by PIO
      character(len=cl)    :: file_path       !Path to NetCDF file
      integer              :: err_handling    !PIO error handling code
      integer              :: var_id          !NetCDF variable ID
      integer              :: nc_type         !NetCDF variable type
      integer              :: ndims           !Number of variable dimensions on NetCDF file
      integer, allocatable :: dim_ids(:)      !Variable dimension IDs
      integer, allocatable :: dim_sizes(:)    !Variable dimension sizes

      integer :: i !loop control variable
      !----------------------

      !Extract open file information:
      pio_file_handle = sima_pio_fh_data(file_id)%pio_fh
      file_path       = sima_pio_fh_data(file_id)%file_path

      !Force PIO to send an error code instead of dying:
      call pio_seterrorhandling(pio_file_handle, PIO_BCAST_ERROR, oldmethod=err_handling)

      !Look for variable on file:
      errcode = pio_inq_varid(pio_file_handle, varname, var_id)
      if (errcode /= PIO_NOERR) then
         errcode = 5 !Make sure error code is non-zero
         errmsg  = "Failed to find '"//varname//"' in "//file_path
         !Reset PIO back to original error handling method:
         call pio_seterrorhandling(pio_file_handle, err_handling)
         return
      end if

      !Get variable type and number of variable dimensions on file:
      errcode = pio_inquire_variable(pio_file_handle, var_id, xtype=nc_type, ndims=ndims)
      if(errcode /= PIO_NOERR) then
         errcode = 6 !Make sure error code is non-zero
         errmsg  = "Failed to find number of dimensions for '"//varname//"' in "//file_path
         !Reset PIO back to original error handling method:
         call pio_seterrorhandling(pio_file_handle, err_handling)
         return
      end if

      !Check that variable is a character array
      !(as we cannot currently handle string-type variables):
      if(nc_type /= NF90_CHAR) then
         errcode = 7
         errmsg = "NetCDF Variable '"//varname//"' is not a character array.  File can be found here: "//file_path
         !Reset PIO back to original error handling method:
         call pio_seterrorhandling(pio_file_handle, err_handling)
         return
      end if

      !Check that the variable rank as specified by the caller
      !matches what is found on the NetCDF file:

      !NOTE:  NetCDF supports both character arrays and string-type
      !data depending on the NetCDF version, so the dimensions
      !might be one larger than the actual array size if it
      !includes the character length as a dimension as well.
      !Ideally the actual type would be checked and handled
      !differently, but for now just assume a character array
      !and check for ndims = rank+1
      errcode = 0
      select rank(var)
         rank(0)
            if(ndims /= 1) errcode = 8
         rank(1)
            if(ndims /= 2) errcode = 8
         rank(2)
            if(ndims /= 3) errcode = 8
         rank(3)
            if(ndims /= 4) errcode = 8
         rank(4)
            if(ndims /= 5) errcode = 8
         rank(5)
            if(ndims /= 6) errcode = 8
         rank default
            !PIO can only handle up to 5 dimensions,
            !so error out if array rank is greater than that.
            errcode = 9
            errmsg  = "Unsupported rank for variable '"//varname//"'"
            return
      end select
      if(errcode == 8) then
         errmsg  = "Variable '"//varname//"' isn't declared with the correct number of dimensions"
         !Reset PIO back to original error handling method:
         call pio_seterrorhandling(pio_file_handle, err_handling)
         return
      end if

      !Get variable dimension sizes, if applicable:
      if(ndims > 0) then
         !Allocate NetCDF variable dimension ID array:
         allocate(dim_ids(ndims), stat=errcode, errmsg=errmsg)
         if(errcode /= 0) then
            !Reset PIO back to original error handling method:
            call pio_seterrorhandling(pio_file_handle, err_handling)
            return
         end if

         !Get variable dimension IDs:
         errcode = pio_inquire_variable(pio_file_handle, var_id, dimids=dim_ids)

         !Allocate NetCDF variable dimension sizes array:
         allocate(dim_sizes(ndims), stat=errcode, errmsg=errmsg)
         if(errcode /= 0) then
            !Reset PIO back to original error handling method:
            call pio_seterrorhandling(pio_file_handle, err_handling)
            return
         end if

         !Get dimension sizes:
         do i = 1, ndims
            errcode = pio_inq_dimlen(pio_file_handle, dim_ids(i), dim_sizes(i))
            if(errcode /= PIO_NOERR) then
               errmsg  = "Failed to find dimension sizes for '"//varname//"' in "//file_path
               !Reset PIO back to original error handling method:
               call pio_seterrorhandling(pio_file_handle, err_handling)
               return
            end if
         end do

      end if

      !Now attempt to allocate and initialize variable, and
      !read-in the NetCDF data.  Note that the first dimenstion
      !is the length of the character array, so need to start
      !the dim_sizes allocation count at index two:
      select rank(var)
         rank(0)

            allocate(character(dim_sizes(1)) :: var, stat=errcode, errmsg=errmsg)
            if(errcode /= 0) then
               !Reset PIO back to original error handling method:
               call pio_seterrorhandling(pio_file_handle, err_handling)
               return
            end if
            var = 'UNSET'
            errcode = pio_get_var(pio_file_handle, var_id, var)

         rank(1)

            allocate(character(dim_sizes(1)) :: var(dim_sizes(2)), stat=errcode, errmsg=errmsg)
            if(errcode /= 0) then
               !Reset PIO back to original error handling method:
               call pio_seterrorhandling(pio_file_handle, err_handling)
               return
            end if
            var(:) = 'UNSET'
            errcode = pio_get_var(pio_file_handle, var_id, var(:))

         rank(2)

            allocate(character(dim_sizes(1)) :: var(dim_sizes(2), dim_sizes(3)), stat=errcode, errmsg=errmsg)
            if(errcode /= 0) then
               !Reset PIO back to original error handling method:
               call pio_seterrorhandling(pio_file_handle, err_handling)
               return
            end if
            var(:,:) = 'UNSET'
            errcode = pio_get_var(pio_file_handle, var_id, var(:,:))

         rank(3)

            allocate(character(dim_sizes(1)) :: var(dim_sizes(2), dim_sizes(3), dim_sizes(4)), stat=errcode, errmsg=errmsg)
            if(errcode /= 0) then
               !Reset PIO back to original error handling method:
               call pio_seterrorhandling(pio_file_handle, err_handling)
               return
            end if
            var(:,:,:) = 'UNSET'
            errcode = pio_get_var(pio_file_handle, var_id, var(:,:,:))

         rank(4)

            allocate(character(dim_sizes(1)) :: var(dim_sizes(2), dim_sizes(3), dim_sizes(4), dim_sizes(5)), &
                     stat=errcode, errmsg=errmsg)
            if(errcode /= 0) then
               !Reset PIO back to original error handling method:
               call pio_seterrorhandling(pio_file_handle, err_handling)
               return
            end if
            var(:,:,:,:) = 'UNSET'
            errcode = pio_get_var(pio_file_handle, var_id, var(:,:,:,:))

         rank(5)

            allocate(character(dim_sizes(1)) :: var(dim_sizes(2), dim_sizes(3), dim_sizes(4), dim_sizes(5), dim_sizes(6)), &
                     stat=errcode, errmsg=errmsg)
            if(errcode /= 0) then
               !Reset PIO back to original error handling method:
               call pio_seterrorhandling(pio_file_handle, err_handling)
               return
            end if
            var(:,:,:,:,:) = 'UNSET'
            errcode = pio_get_var(pio_file_handle, var_id, var(:,:,:,:,:))

         !No default needed as it was already checked above.
      end select

      if (errcode /= PIO_NOERR) then
         errcode = 10 !Make sure error code is non-zero
         errmsg  = "Failed to read '"//varname//"' from "//file_path
         !Reset PIO back to original error handling method:
         call pio_seterrorhandling(pio_file_handle, err_handling)
         return
      end if

      !Reset PIO back to original error handling method:
      call pio_seterrorhandling(pio_file_handle, err_handling)

      !Variable was successfully read, so properly set the error
      !code and message:
      errcode = 0
      errmsg = ''

   end subroutine sima_get_netcdf_var_char

end module sima_ccpp_FileIO
