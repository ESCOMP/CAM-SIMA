! Copyright (C) 2024 National Science Foundation-National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
module musica_ccpp_dependencies
!--------------------------------------------------------------------------
!
! This module temporarily provides data that MUSICA chemistry consumes but
! does not produce. The values are realistic but are not based on the
! actual model state. These should be removed as the producers of this data
! are added to CAM-SIMA or as CCPP-compliant physics schemes.
!
! IMPORTANT: This module must be completely removed before doing any actual
!            science with MUSICA chemistry in CAM-SIMA.
!
!--------------------------------------------------------------------------

  use ccpp_kinds, only: kind_phys

  implicit none
  private

  public :: musica_ccpp_dependencies_init

  !> \section arg_table_musica_ccpp_dependencies Argument Table
  !! \htmlinclude arg_table_musica_ccpp_dependencies.html
  !!
  integer, public, protected :: photolysis_wavelength_grid_section_dimension = 102
  integer, public, protected :: photolysis_wavelength_grid_interface_dimension = 103
  real(kind_phys), allocatable, public, protected :: photolysis_wavelength_grid_interfaces(:)
  real(kind_phys), allocatable, public, protected :: extraterrestrial_radiation_flux(:)
  real(kind_phys), allocatable, public, protected :: surface_albedo(:)
  real(kind_phys), allocatable, public, protected :: blackbody_temperature_at_surface(:)
  real(kind_phys), allocatable, public, protected :: cloud_area_fraction(:,:)

  ! local parameters
  character(len=*), parameter :: module_name = '(musica_ccpp_dependencies)'

!==============================================================================
contains
!==============================================================================

  subroutine musica_ccpp_dependencies_init(horizontal_dimension, &
      vertical_layer_dimension, log_file_unit)

    use cam_abortutils, only: check_allocate

    !-----------------------------------------------------------------------
    !
    ! Initialize the MUSICA scheme dependencies.
    !
    !-----------------------------------------------------------------------

    integer, intent(in) :: horizontal_dimension
    integer, intent(in) :: vertical_layer_dimension
    integer, intent(in) :: log_file_unit

    integer :: error_code
    character(len=*), parameter :: subroutine_name = &
        trim(module_name)//':(musica_ccpp_dependencies_init)'

    write(log_file_unit,*) 'WARNING: Using placeholder data for MUSICA chemistry.'

    allocate(photolysis_wavelength_grid_interfaces(photolysis_wavelength_grid_interface_dimension), &
             stat=error_code)
    call check_allocate(error_code, subroutine_name, &
                        'photolysis_wavelength_grid_interfaces(photolysis_wavelength_grid_interface_dimension)', &
                        file=__FILE__, line=__LINE__)
    allocate(extraterrestrial_radiation_flux(photolysis_wavelength_grid_section_dimension), &
             stat=error_code)
    call check_allocate(error_code, subroutine_name, &
                        'extraterrestrial_radiation_flux(photolysis_wavelength_grid_section_dimension)', &
                        file=__FILE__, line=__LINE__)
    allocate(surface_albedo(horizontal_dimension), stat=error_code)
    call check_allocate(error_code, subroutine_name, &
                        'surface_albedo(horizontal_dimension)', &
                        file=__FILE__, line=__LINE__)
    allocate(blackbody_temperature_at_surface(horizontal_dimension), stat=error_code)
    call check_allocate(error_code, subroutine_name, &
                        'blackbody_temperature_at_surface(horizontal_dimension)', &
                        file=__FILE__, line=__LINE__)
    allocate(cloud_area_fraction(horizontal_dimension, vertical_layer_dimension), stat=error_code)
    call check_allocate(error_code, subroutine_name, &
                        'cloud_area_fraction(horizontal_dimension, vertical_layer_dimension)', &
                        file=__FILE__, line=__LINE__)

    surface_albedo(:) = 0.1_kind_phys
    blackbody_temperature_at_surface(:) = 292.3_kind_phys
    cloud_area_fraction(:,:) = 0.7_kind_phys
    extraterrestrial_radiation_flux(:) = 1.0e14_kind_phys
    photolysis_wavelength_grid_interfaces = (/ &
      120.0e-9_kind_phys, &
      121.4e-9_kind_phys, &
      121.9e-9_kind_phys, &
      123.5e-9_kind_phys, &
      124.3e-9_kind_phys, &
      125.5e-9_kind_phys, &
      126.3e-9_kind_phys, &
      127.1e-9_kind_phys, &
      130.1e-9_kind_phys, &
      131.1e-9_kind_phys, &
      135.0e-9_kind_phys, &
      140.0e-9_kind_phys, &
      145.0e-9_kind_phys, &
      150.0e-9_kind_phys, &
      155.0e-9_kind_phys, &
      160.0e-9_kind_phys, &
      165.0e-9_kind_phys, &
      168.0e-9_kind_phys, &
      171.0e-9_kind_phys, &
      173.0e-9_kind_phys, &
      174.4e-9_kind_phys, &
      175.4e-9_kind_phys, &
      177.0e-9_kind_phys, &
      178.6e-9_kind_phys, &
      180.2e-9_kind_phys, &
      181.8e-9_kind_phys, &
      183.5e-9_kind_phys, &
      185.2e-9_kind_phys, &
      186.9e-9_kind_phys, &
      188.7e-9_kind_phys, &
      190.5e-9_kind_phys, &
      192.3e-9_kind_phys, &
      194.2e-9_kind_phys, &
      196.1e-9_kind_phys, &
      198.0e-9_kind_phys, &
      200.0e-9_kind_phys, &
      202.0e-9_kind_phys, &
      204.1e-9_kind_phys, &
      206.2e-9_kind_phys, &
      208.0e-9_kind_phys, &
      211.0e-9_kind_phys, &
      214.0e-9_kind_phys, &
      217.0e-9_kind_phys, &
      220.0e-9_kind_phys, &
      223.0e-9_kind_phys, &
      226.0e-9_kind_phys, &
      229.0e-9_kind_phys, &
      232.0e-9_kind_phys, &
      235.0e-9_kind_phys, &
      238.0e-9_kind_phys, &
      241.0e-9_kind_phys, &
      244.0e-9_kind_phys, &
      247.0e-9_kind_phys, &
      250.0e-9_kind_phys, &
      253.0e-9_kind_phys, &
      256.0e-9_kind_phys, &
      259.0e-9_kind_phys, &
      263.0e-9_kind_phys, &
      267.0e-9_kind_phys, &
      271.0e-9_kind_phys, &
      275.0e-9_kind_phys, &
      279.0e-9_kind_phys, &
      283.0e-9_kind_phys, &
      287.0e-9_kind_phys, &
      291.0e-9_kind_phys, &
      295.0e-9_kind_phys, &
      298.5e-9_kind_phys, &
      302.5e-9_kind_phys, &
      305.5e-9_kind_phys, &
      308.5e-9_kind_phys, &
      311.5e-9_kind_phys, &
      314.5e-9_kind_phys, &
      317.5e-9_kind_phys, &
      322.5e-9_kind_phys, &
      327.5e-9_kind_phys, &
      332.5e-9_kind_phys, &
      337.5e-9_kind_phys, &
      342.5e-9_kind_phys, &
      347.5e-9_kind_phys, &
      350.0e-9_kind_phys, &
      355.0e-9_kind_phys, &
      360.0e-9_kind_phys, &
      365.0e-9_kind_phys, &
      370.0e-9_kind_phys, &
      375.0e-9_kind_phys, &
      380.0e-9_kind_phys, &
      385.0e-9_kind_phys, &
      390.0e-9_kind_phys, &
      395.0e-9_kind_phys, &
      400.0e-9_kind_phys, &
      405.0e-9_kind_phys, &
      410.0e-9_kind_phys, &
      415.0e-9_kind_phys, &
      420.0e-9_kind_phys, &
      430.0e-9_kind_phys, &
      440.0e-9_kind_phys, &
      450.0e-9_kind_phys, &
      500.0e-9_kind_phys, &
      550.0e-9_kind_phys, &
      600.0e-9_kind_phys, &
      650.0e-9_kind_phys, &
      700.0e-9_kind_phys, &
      750.0e-9_kind_phys &
    /)

  end subroutine musica_ccpp_dependencies_init

end module musica_ccpp_dependencies