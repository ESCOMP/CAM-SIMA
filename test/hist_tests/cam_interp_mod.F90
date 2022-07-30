module cam_interp_mod

   use shr_kind_mod, only: r8=>shr_kind_r8

   implicit none
   private

   ! Some parameters for use with interpolated output namelist items
   integer,          parameter, public :: interp_type_native            = 0
   integer,          parameter, public :: interp_type_bilinear          = 1
   integer,          parameter, public :: interp_gridtype_equal_poles   = 1
   integer,          parameter, public :: interp_gridtype_gauss         = 2
   integer,          parameter, public :: interp_gridtype_equal_nopoles = 3

   type, public :: hist_interp_info_t
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
   contains
      procedure :: reset => interp_reset
   end type hist_interp_info_t

CONTAINS

   subroutine interp_reset(this)
      class(hist_interp_info_t), intent(inout) :: this

      this%gridname = ''
      this%grid_id  = -1
      this%interp_gridtype = interp_gridtype_equal_poles
      this%interp_type = interp_type_bilinear
      this%interp_nlat = 0
      this%interp_nlon = 0
      if (associated(this%interp_lat)) then
         deallocate(this%interp_lat)
         nullify(this%interp_lat)
      end if
      if (associated(this%interp_lon)) then
         deallocate(this%interp_lon)
         nullify(this%interp_lon)
      end if
      if (associated(this%interp_gweight)) then
         deallocate(this%interp_gweight)
         nullify(this%interp_gweight)
      end if
   end subroutine interp_reset

end module cam_interp_mod
