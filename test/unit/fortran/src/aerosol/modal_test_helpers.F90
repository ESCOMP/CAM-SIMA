!-----------------------------------------------------------------------
! Helper module to set up and tear down the 2-mode mock config
! for modal aerosol unit tests.
!
! Mode 1 "accum": 2 species (sulfate, black-c)
! Mode 2 "coarse": 1 species (dust)
!-----------------------------------------------------------------------
module modal_test_helpers
  use shr_kind_mod, only: r8 => shr_kind_r8
  use radiative_aerosol, only: setup_mock_modal_rad_aer, cleanup_mock_rad_aer
  implicit none

  ! Mode properties
  real(r8), parameter :: sigmag_accum  = 1.8_r8
  real(r8), parameter :: sigmag_coarse = 2.0_r8
  real(r8), parameter :: dgnum_accum   = 0.11e-6_r8
  real(r8), parameter :: dgnum_coarse  = 2.0e-6_r8

  ! Species densities
  real(r8), parameter :: dens_sulfate = 1770._r8
  real(r8), parameter :: dens_bc      = 1700._r8
  real(r8), parameter :: dens_dust    = 2600._r8

  ! Species hygroscopicities
  real(r8), parameter :: hygro_sulfate = 0.507_r8
  real(r8), parameter :: hygro_bc      = 1.0e-10_r8
  real(r8), parameter :: hygro_dust    = 0.14_r8

contains

  subroutine setup_2mode_mock()
    character(len=32) :: spec_type(2,2), spec_name(2,2), spec_name_cw(2,2)
    real(r8) :: spec_density(2,2), spec_hygro(2,2)

    spec_type(1,1) = 'sulfate';   spec_type(1,2) = 'black-c'
    spec_type(2,1) = 'dust';      spec_type(2,2) = ''

    spec_name(1,1) = 'so4_a1';    spec_name(1,2) = 'bc_a1'
    spec_name(2,1) = 'dst_a2';    spec_name(2,2) = ''
    spec_name_cw(1,1) = 'so4_c1'; spec_name_cw(1,2) = 'bc_c1'
    spec_name_cw(2,1) = 'dst_c2'; spec_name_cw(2,2) = ''

    spec_density(1,1) = dens_sulfate; spec_density(1,2) = dens_bc
    spec_density(2,1) = dens_dust;    spec_density(2,2) = 0._r8

    spec_hygro(1,1) = hygro_sulfate; spec_hygro(1,2) = hygro_bc
    spec_hygro(2,1) = hygro_dust;    spec_hygro(2,2) = 0._r8

    call setup_mock_modal_rad_aer( &
         nmodes = 2, &
         nspec = [2, 1], &
         mode_type = [character(len=32) :: 'accum', 'coarse'], &
         num_name = [character(len=32) :: 'num_a1', 'num_a2'], &
         num_name_cw = [character(len=32) :: 'num_c1', 'num_c2'], &
         sigmag = [sigmag_accum, sigmag_coarse], &
         dgnum = [dgnum_accum, dgnum_coarse], &
         dgnumlo = [dgnum_accum * 0.1_r8, dgnum_coarse * 0.1_r8], &
         dgnumhi = [dgnum_accum * 10._r8, dgnum_coarse * 10._r8], &
         rhcrystal = [0.35_r8, 0.35_r8], &
         rhdeliques = [0.80_r8, 0.80_r8], &
         spec_type = spec_type, &
         spec_name = spec_name, &
         spec_name_cw = spec_name_cw, &
         spec_density = spec_density, &
         spec_hygro = spec_hygro )
  end subroutine setup_2mode_mock

end module modal_test_helpers
