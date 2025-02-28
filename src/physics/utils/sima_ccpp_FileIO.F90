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

  public :: sima_get_netcdf_var

!==============================================================================
contains
!==============================================================================

   subroutine sima_get_netcdf_var(pio_file_handle, varname, var, errcode, errmsg)

     !Check for and then read a NetCDF variable into the provided
     !output variable using SIMA's I/O system (PIO).

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
     character(len=*),  intent(in)    :: varname         !variable name

     !Output variables:
     real(kind_phys), intent(out)     :: var(:)          !Variable that file data will be read to.
     integer,  intent(out)            :: errcode         !Error code
     character(len=*), intent(out)    :: errmsg          !Error message

     !Local variables:
     integer :: err_handling   ! PIO error handling code
     integer :: var_id         ! NetCDF variable ID
     !----------------------

     !Force PIO to send an error code instead of dying:
     call pio_seterrorhandling(pio_file_handle, PIO_BCAST_ERROR, oldmethod=err_handling)

     !Look for variable on file:
     errcode = pio_inq_varid(pio_file_handle, varname, var_id)
     if (errcode /= PIO_NOERR) then
        errcode = 1 !Make sure error code is non-zero
        errmsg  = "Failed to find '"//varname//"' in NetCDF file."
        var(:)  = huge(1._kind_phys)
        !Reset PIO back to original error handling method:
        call pio_seterrorhandling(pio_file_handle, err_handling)
        return
     end if

     !If found, then attempt to read-in the NetCDF data:
     errcode = pio_get_var(pio_file_handle, var_id, var)
     if (errcode /= PIO_NOERR) then
        errcode = 2 !Make sure error code is non-zero
        errmsg  = "Failed to read '"//varname//"' from NetCDF file."
        var(:)  = huge(1._kind_phys)
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

   end subroutine sima_get_netcdf_var

end module sima_ccpp_FileIO
