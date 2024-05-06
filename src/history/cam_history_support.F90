module cam_history_support

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!
   !!  cam_history_support is used by cam_history as well as by the dycores
   !!    (for vertical coordinate support).
   !!
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use shr_kind_mod,     only: r8=>shr_kind_r8
   use pio,              only: var_desc_t, file_desc_t, PIO_MAX_NAME
   use cam_abortutils,   only: endrun
   use cam_logfile,      only: iulog
   use spmd_utils,       only: masterproc
   use cam_grid_support, only: cam_grid_patch_t, cam_grid_header_info_t
   use cam_grid_support, only: max_hcoordname_len
   use cam_pio_utils,    only: cam_pio_handle_error

   implicit none
   private
   save

   ! max_fieldname_len = max chars for field name
   integer, parameter, public :: max_fieldname_len = PIO_MAX_NAME
   ! default fill value for history NetCDF fields
   real(r8), parameter, public :: hist_default_fillvalue = 1.e36_r8
   integer,  parameter, public :: pfiles = 12        ! max number of tapes

  ! Some parameters for use with interpolated output namelist items
  integer,          parameter, public :: interp_type_native            = 0
  integer,          parameter, public :: interp_type_bilinear          = 1
  integer,          parameter, public :: interp_gridtype_equal_poles   = 1
  integer,          parameter, public :: interp_gridtype_gauss         = 2
  integer,          parameter, public :: interp_gridtype_equal_nopoles = 3

  !---------------------------------------------------------------------------
  !
  !  interp_info_t: Information for lat/lon interpolated history output
  !
  !---------------------------------------------------------------------------
  type, public :: interp_info_t
    ! store the  lat-lon grid information
    character(len=28)     :: gridname = ''
    integer               :: grid_id  = -1
    ! gridtype = 1      equally spaced, including poles (FV scalars output grid)
    ! gridtype = 2      Gauss grid (CAM Eulerian)
    ! gridtype = 3      equally spaced, no poles (FV staggered velocity)
    integer               :: interp_gridtype = interp_gridtype_equal_poles
    ! interpolate_type = 0: native high order interpolation
    ! interpolate_type = 1: bilinear interpolation
    integer               :: interp_type = interp_type_bilinear
    integer               :: interp_nlat = 0
    integer               :: interp_nlon = 0
    real(r8), pointer     :: interp_lat(:) => NULL()
    real(r8), pointer     :: interp_lon(:) => NULL()
    real(r8), pointer     :: interp_gweight(:) => NULL()
  end type interp_info_t

end module cam_history_support
