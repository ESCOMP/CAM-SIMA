module hybvcoord_mod
  use shr_kind_mod,   only: r8=>shr_kind_r8

  implicit none
  private

  !-----------------------------------------------------------------------
  ! hvcoord_t: Hybrid level definitions: p = a*p0 + b*ps
  !            interfaces   p(k) = hyai(k)*ps0 + hybi(k)*ps
  !            midpoints    p(k) = hyam(k)*ps0 + hybm(k)*ps
  !-----------------------------------------------------------------------
  type, public :: hvcoord_t
    real(r8)              :: ps0       ! base state surface-pressure for level definitions
    real(r8), allocatable :: hyai(:)   ! ps0 component of hybrid coordinate - interfaces
    real(r8), allocatable :: hyam(:)   ! ps0 component of hybrid coordinate - midpoints
    real(r8), allocatable :: hybi(:)   ! ps  component of hybrid coordinate - interfaces
    real(r8), allocatable :: hybm(:)   ! ps  component of hybrid coordinate - midpoints
    real(r8), allocatable :: hybd(:)   ! difference in b (hybi) across layers
    real(r8)              :: prsfac    ! log pressure extrapolation factor (time, space independent)
    real(r8), allocatable :: etam(:)   ! eta-levels at midpoints
    real(r8), allocatable :: etai(:)   ! eta-levels at interfaces
    integer               :: nprlev    ! number of pure pressure levels at top
    integer               :: pad
  end type hvcoord_t

end module hybvcoord_mod
