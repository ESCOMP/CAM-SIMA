! Copyright (C) 2025 National Science Foundation-National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
module sima_ccpp_FileIO

  !This module contains the specific CAM-SIMA
  !interfaces for the CCPP FileIO object, which
  !allows CCPP schemes to perform file I/O
  !with the same libraries CAM-SIMA uses (e.g. PIO).

  implicit none

  private
  save

  !Define type-polymorphic module
  !procedure interface
  interface sima_get_netcdf_var
    module procedure sima_get_netcdf_var_int
    module procedure sima_get_netcdf_var_real
  end interface

  !Declare public procedures
  public :: sima_get_netcdf_dim
  public :: sima_get_netcdf_var

!==============================================================================
contains
!==============================================================================

   subroutine sima_get_netcdf_dim(pio_file_handle, file_path, dimname, errcode, errmsg, dimlen)

     !Check for and then read a NetCDF dimension length
     !into the provided output variable using SIMA's
     !I/O system (PIO).

     use pio, only: file_desc_t
     use pio, only: pio_inq_dimid
     use pio, only: pio_seterrorhandling
     use pio, only: pio_inq_dimlen
     use pio, only: PIO_NOERR
     use pio, only: PIO_BCAST_ERROR

     !----------------------
     !Input variables:
     type(file_desc_t), intent(inout) :: pio_file_handle !File ID type used by PIO
     character(len=*),  intent(in)    :: file_path       !Path to NetCDF file
     character(len=*),  intent(in)    :: dimname         !variable name

     !Output variables:
     integer, intent(out), optional   :: dimlen          !Dimension length
     integer, intent(out)             :: errcode         !Error code
     character(len=*), intent(out)    :: errmsg          !Error message

     !Local variables:
     integer :: err_handling   ! PIO error handling code
     integer :: dim_id         ! NetCDF dimension ID
     !----------------------

     !Check if output vaiable was requested,
     !if not then just return with passing
     !error code.
     if(.not.present(dimlen)) then
       errcode = 0
       errmsg  = ''
       return
     end if

     !Force PIO to send an error code instead of dying:
     call pio_seterrorhandling(pio_file_handle, PIO_BCAST_ERROR, oldmethod=err_handling)

     !Look for dimension on file:
     errcode = pio_inq_dimid(pio_file_handle, dimname, dim_id)
     if(errcode /= PIO_NOERR) then
        errcode = 1 !Make sure error code is non-zero
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
           errcode = 2 !Make sure error code is non-zero
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

   subroutine sima_get_netcdf_var_int(pio_file_handle, file_path, varname, var, errcode, errmsg)

     !Check for and then read an INTEGER-type NetCDF variable
     !into the provided output variable using SIMA's I/O system (PIO).

     use ccpp_kinds, only: kind_phys
     use pio,        only: file_desc_t
     use pio,        only: pio_inq_varid
     use pio,        only: pio_seterrorhandling
     use pio,        only: pio_get_var
     use pio,        only: PIO_NOERR
     use pio,        only: PIO_BCAST_ERROR

     !----------------------
     !Input variables:
     type(file_desc_t), intent(inout) :: pio_file_handle !File ID type used by PIO
     character(len=*),  intent(in)    :: file_path       !Path to NetCDF file
     character(len=*),  intent(in)    :: varname         !variable name

     !Output variables:
     integer, intent(out)             :: var(..)         !Variable that file data will be read to.
                                                         !Note that it is assumed-rank variable
     integer,  intent(out)            :: errcode         !Error code
     character(len=*), intent(out)    :: errmsg          !Error message

     !Local variables:
     integer :: err_handling   ! PIO error handling code
     integer :: var_id         ! NetCDF variable ID
     !----------------------

          !Initialize output variable to "bad" value:
     select rank(var)
        rank(0)
           var            = huge(1)
        rank(1)
           var(:)         = huge(1)
        rank(2)
           var(:,:)       = huge(1)
        rank(3)
           var(:,:,:)     = huge(1)
        rank(4)
           var(:,:,:,:)   = huge(1)
        rank(5)
           var(:,:,:,:,:) = huge(1)
        rank default
           !PIO can only handle up to 5 dimensions,
           !so error out if array rank is greater than that.
           errcode  = 3
           errmsg   = "Unsupported rank for variable '"//varname//"'"
           return
     end select

     !Force PIO to send an error code instead of dying:
     call pio_seterrorhandling(pio_file_handle, PIO_BCAST_ERROR, oldmethod=err_handling)

     !Look for variable on file:
     errcode = pio_inq_varid(pio_file_handle, varname, var_id)
     if (errcode /= PIO_NOERR) then
        errcode = 4 !Make sure error code is non-zero
        errmsg  = "Failed to find '"//varname//"' in "//file_path
        !Reset PIO back to original error handling method:
        call pio_seterrorhandling(pio_file_handle, err_handling)
        return
     end if

     !Now attempt to read-in the NetCDF data:
     select rank(var)
        rank(0)
           errcode = pio_get_var(pio_file_handle, var_id, var)
        rank(1)
           errcode = pio_get_var(pio_file_handle, var_id, var(:))
        rank(2)
           errcode = pio_get_var(pio_file_handle, var_id, var(:,:))
        rank(3)
           errcode = pio_get_var(pio_file_handle, var_id, var(:,:,:))
        rank(4)
           errcode = pio_get_var(pio_file_handle, var_id, var(:,:,:,:))
        rank(5)
           errcode = pio_get_var(pio_file_handle, var_id, var(:,:,:,:,:))
        !No default needed as it was already checked above.
     end select

     if (errcode /= PIO_NOERR) then
        errcode = 5 !Make sure error code is non-zero
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

   subroutine sima_get_netcdf_var_real(pio_file_handle, file_path, varname, var, errcode, errmsg)

     !Check for and then read a REAL-type NetCDF variable into
     !the provided output variable using SIMA's I/O system (PIO).

     use ccpp_kinds, only: kind_phys
     use pio,        only: file_desc_t
     use pio,        only: pio_inq_varid
     use pio,        only: pio_seterrorhandling
     use pio,        only: pio_get_var
     use pio,        only: PIO_NOERR
     use pio,        only: PIO_BCAST_ERROR

     !----------------------
     !Input variables:
     type(file_desc_t), intent(inout) :: pio_file_handle !File ID type used by PIO
     character(len=*),  intent(in)    :: file_path       !Path to NetCDF file
     character(len=*),  intent(in)    :: varname         !variable name

     !Output variables:
     real(kind_phys), intent(out)     :: var(..)         !Real variable that file data will be read to.
                                                         !Note that it is an assumed-rank variable.
     integer,  intent(out)            :: errcode         !Error code
     character(len=*), intent(out)    :: errmsg          !Error message

     !Local variables:
     integer :: err_handling   ! PIO error handling code
     integer :: var_id         ! NetCDF variable ID
     !----------------------

     !Initialize output variable to "bad" value:
     select rank(var)
        rank(0)
           var            = huge(1._kind_phys)
        rank(1)
           var(:)         = huge(1._kind_phys)
        rank(2)
           var(:,:)       = huge(1._kind_phys)
        rank(3)
           var(:,:,:)     = huge(1._kind_phys)
        rank(4)
           var(:,:,:,:)   = huge(1._kind_phys)
        rank(5)
           var(:,:,:,:,:) = huge(1._kind_phys)
        rank default
           !PIO can only handle up to 5 dimensions,
           !so error out if array rank is greater than that.
           errcode      = 3
           errmsg       = "Unsupported rank for variable '"//varname//"'"
           return
     end select

     !Force PIO to send an error code instead of dying:
     call pio_seterrorhandling(pio_file_handle, PIO_BCAST_ERROR, oldmethod=err_handling)

     !Look for variable on file:
     errcode = pio_inq_varid(pio_file_handle, varname, var_id)
     if (errcode /= PIO_NOERR) then
        errcode = 4 !Make sure error code is non-zero
        errmsg  = "Failed to find '"//varname//"' in "//file_path
        !Reset PIO back to original error handling method:
        call pio_seterrorhandling(pio_file_handle, err_handling)
        return
     end if

     !Now attempt to read-in the NetCDF data:
     select rank(var)
        rank(0)
           errcode = pio_get_var(pio_file_handle, var_id, var)
        rank(1)
           errcode = pio_get_var(pio_file_handle, var_id, var(:))
        rank(2)
           errcode = pio_get_var(pio_file_handle, var_id, var(:,:))
        rank(3)
           errcode = pio_get_var(pio_file_handle, var_id, var(:,:,:))
        rank(4)
           errcode = pio_get_var(pio_file_handle, var_id, var(:,:,:,:))
        rank(5)
           errcode = pio_get_var(pio_file_handle, var_id, var(:,:,:,:,:))
        !No default needed as it was already checked above.
     end select

     if (errcode /= PIO_NOERR) then
        errcode = 5 !Make sure error code is non-zero
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

end module sima_ccpp_FileIO
