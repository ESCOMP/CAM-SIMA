!-----------------------------------------------------------------------
! Mock physics_types for unit testing.
! Provides the registry-generated modal aerosol fields consumed by
! modal_aerosol_state_mod (in the full model these are written in place
! by the CCPPized modal aerosol calcsize/wateruptake schemes).
! The arrays are left unallocated; tests exercising the size-weight,
! wet-diameter, or water-uptake paths must allocate and fill them
! (ncol, pver, nmodes).
!-----------------------------------------------------------------------
module physics_types

  use ccpp_kinds, only: kind_phys

  implicit none

  ! target so the aerosol_mmr_host accessors can return pointers to them
  ! (mirrors the real registry-generated fields).
  ! dry number mode diameter of modal aerosol [m]
  real(kind_phys), public, allocatable, target :: dgncur_a(:,:,:)
  ! wet number mode diameter of modal aerosol [m]
  real(kind_phys), public, allocatable, target :: dgncur_awet(:,:,:)
  ! aerosol water mass mixing ratio per mode [kg kg-1]
  real(kind_phys), public, allocatable, target :: qaerwat_aer(:,:,:)

end module physics_types
