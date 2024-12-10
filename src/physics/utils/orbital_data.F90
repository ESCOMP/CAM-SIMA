! Copyright (C) 2024 National Science Foundation-National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
module orbital_data
!--------------------------------------------------------------------------
!
! Provides access to conditions calculated based on the Earth's orbit.
!
!--------------------------------------------------------------------------

  use shr_kind_mod,  only: R8 => SHR_KIND_R8
  use shr_const_mod, only: PI => SHR_CONST_PI
  use shr_orb_mod,   only: FILL_R8 => SHR_ORB_UNDEF_REAL

  implicit none
  private

  public :: orbital_data_init, orbital_data_advance

  ! Calculated orbidal data for the current simulation day
   real(R8),              protected, public :: solar_declination  = FILL_R8 ! Solar declination angle [radians]
   real(R8),              protected, public :: earth_sun_distance = FILL_R8 ! Earth-sun distance [AU]
   real(R8), allocatable, protected, public :: solar_zenith_angle(:)        ! Solar zenith angle (column) [radians]
    
!=======================================================================
contains
!=======================================================================

  subroutine orbital_data_init(number_of_columns)

    !-----------------------------------------------------------------------
    !
    ! Initialize the orbital data module.
    !
    !-----------------------------------------------------------------------

    integer, intent(in) :: number_of_columns
   
    allocate(solar_zenith_angle(number_of_columns), source=FILL_R8)
   
  end subroutine orbital_data_init

  !=======================================================================

  subroutine orbital_data_advance(calendar_day, latitudes, longitudes)
   
    !-----------------------------------------------------------------------
    !
    ! Advance the orbital data to the current simulation time.
    !
    !-----------------------------------------------------------------------
   
    use shr_orb_mod,     only: shr_orb_decl, shr_orb_cosz
    use cam_control_mod, only: eccen, mvelpp, lambm0, obliqr

    real(R8), intent(in) :: calendar_day  ! Fractional Julian calendar day (1.xx to 365.xx)
    real(R8), intent(in) :: latitudes(:)  ! Centered latitude (column) [radians]
    real(R8), intent(in) :: longitudes(:) ! Centered longitude (column) [radians]

    integer :: i

    ! Compute the solar declination angle [radians] and Earth-sun distance [AU]
    call shr_orb_decl(calendar_day, eccen, mvelpp, lambm0, obliqr, &
                      solar_declination, earth_sun_distance)
   
    ! Compute the solar zenith angle [radians]
    do i = 1, size(latitudes)
      solar_zenith_angle(i) = acos(shr_orb_cosz(calendar_day, latitudes(i), &
                                                longitudes(i), solar_declination))
    end do
   
   end subroutine orbital_data_advance

  !=======================================================================

end module orbital_data
   
