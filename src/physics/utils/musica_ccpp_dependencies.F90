! Copyright (C) 2024-2025 University Corporation for Atmospheric Research
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

  ! local parameters
  character(len=*), parameter :: module_name = '(musica_ccpp_dependencies)'

  !> Definition of temporary MUSICA species object
  type, private :: temp_musica_species_t
    character(len=:), allocatable :: name
    real(kind_phys)               :: constituent_value = 0.0_kind_phys ! kg kg-1
  end type temp_musica_species_t

  interface temp_musica_species_t
    procedure species_constructor
  end interface temp_musica_species_t

!==============================================================================
contains
!==============================================================================

  function species_constructor(name, value) result( this )

    !-----------------------------------------------------------------------
    !
    ! Constructor for temporary MUSICA species object
    !
    !-----------------------------------------------------------------------

    character(len=*), intent(in) :: name
    real(kind_phys),  intent(in) :: value
    type(temp_musica_species_t)  :: this

    this%name = name
    this%constituent_value = value

  end function species_constructor

  subroutine initialize_musica_species_constituents(constituents_properties, &
              constituents_array, errmsg, errcode)

    !-----------------------------------------------------------------------
    !
    ! Initialize temporary MUSICA species constituents.
    !
    !-----------------------------------------------------------------------

    use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t
    use ccpp_const_utils,          only: ccpp_const_get_idx
    use cam_logfile,               only: iulog
    use musica_ccpp_namelist,      only: filename_of_micm_configuration

    type(ccpp_constituent_prop_ptr_t), pointer :: constituents_properties(:)
    real(kind_phys),                   pointer :: constituents_array(:,:,:)
    character(len=512),            intent(out) :: errmsg
    integer,                       intent(out) :: errcode

    ! local variables
    type(temp_musica_species_t), allocatable :: species_group(:)
    character(len=*), parameter              :: chapman_config = 'chapman'
    character(len=*), parameter              :: terminator_config = 'terminator'
    logical                                  :: is_chapman = .false.
    logical                                  :: is_terminator = .false.
    integer                                  :: num_micm_species = 0
    integer                                  :: num_tuvx_constituents = 1
    integer                                  :: num_tuvx_only_gas_species = 0
    integer                                  :: position
    integer                                  :: constituent_index
    integer                                  :: i_species

    if (.not. associated(constituents_properties)) then
      errcode = 1
      errmsg = "[MUSICA Error] The pointer to the constituents properties object is not associated."
      return
    end if

    ! Currently, we only support two types of MUSICA configurations: Chapman and Terminator,
    ! until the file I/O object is implemented. If the configuration is neither of these,
    ! an error will be thrown.
    position = index(filename_of_micm_configuration, chapman_config) ! Check if the substring exists
    if (position > 0) then
      is_chapman = .true.
      write(iulog,*) "[MUSICA Info] Using the Chapman configuriation."
    else
      position = index(filename_of_micm_configuration, terminator_config)
      if (position > 0) then
        is_terminator = .true.
        write(iulog,*) "[MUSICA Info] Using the Terminator configuriation."
      else
        errcode = 1
        errmsg = "[MUSICA Error] MUSICA configuration is not found."
        return
      end if
    end if

    if (is_chapman) then
      num_micm_species = 5
      num_tuvx_only_gas_species = 1
    else if (is_terminator) then
      num_micm_species = 2
      num_tuvx_only_gas_species = 3
    end if

    allocate (species_group(num_micm_species + num_tuvx_constituents + num_tuvx_only_gas_species), &
      stat=errcode, errmsg=errmsg)
    if (errcode /= 0) return

    species_group(1) = species_constructor(&
        "cloud_liquid_water_mixing_ratio_wrt_moist_air_and_condensed_water", 0.00060_kind_phys)

    if (is_chapman) then   
      species_group(2) = species_constructor("O2", 0.22474_kind_phys)
      species_group(3) = species_constructor("O", 5.3509e-10_kind_phys)
      species_group(4) = species_constructor("O1D", 5.3509e-10_kind_phys)
      species_group(5) = species_constructor("O3", 0.00016_kind_phys)
      species_group(6) = species_constructor("N2", 0.74015_kind_phys)
      species_group(7) = species_constructor("air", 1.0_kind_phys)

    else if (is_terminator) then
      species_group(2) = species_constructor("Cl", 1.0e-12_kind_phys)
      species_group(3) = species_constructor("Cl2", 1.0e-12_kind_phys)
      species_group(4) = species_constructor("air", 1.0_kind_phys)
      species_group(5) = species_constructor("O2", 0.21_kind_phys)
      species_group(6) = species_constructor("O3", 4.0e-6_kind_phys)
    end if

    do i_species = 1, num_micm_species + num_tuvx_constituents + num_tuvx_only_gas_species
      call ccpp_const_get_idx(constituents_properties, trim(species_group(i_species)%name), &
                              constituent_index, errmsg, errcode)
      if (errcode /= 0) then
        deallocate (species_group)
        return
      end if

      constituents_array(:,:,constituent_index) = species_group(i_species)%constituent_value
    end do

    deallocate (species_group)

  end subroutine initialize_musica_species_constituents

  subroutine musica_ccpp_dependencies_init( &
              horizontal_dimension, vertical_layer_dimension, &
              constituents_properties, constituents_array)

    use cam_abortutils,            only: check_allocate, endrun
    use cam_logfile,               only: iulog
    use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t

    !-----------------------------------------------------------------------
    !
    ! Initialize the MUSICA scheme dependencies.
    !
    !-----------------------------------------------------------------------

    integer,                        intent(in) :: horizontal_dimension
    integer,                        intent(in) :: vertical_layer_dimension
    type(ccpp_constituent_prop_ptr_t), pointer :: constituents_properties(:)
    real(kind_phys),                   pointer :: constituents_array(:,:,:)

    ! local variables
    character(len=*), parameter :: subroutine_name = &
        trim(module_name)//':(musica_ccpp_dependencies_init)'
    character(len=512)          :: errmsg
    integer                     :: errcode

    write(iulog,*) 'WARNING: Using placeholder data for MUSICA chemistry.'

    call initialize_musica_species_constituents(constituents_properties, &
                                constituents_array, errmsg, errcode)
    if (errcode /= 0) then
      call endrun(errmsg, file=__FILE__, line=__LINE__)
    end if

    allocate(photolysis_wavelength_grid_interfaces(photolysis_wavelength_grid_interface_dimension), &
              stat=errcode, errmsg=errmsg)
    call check_allocate(errcode, subroutine_name, &
                        'photolysis_wavelength_grid_interfaces(photolysis_wavelength_grid_interface_dimension)', &
                        file=__FILE__, line=__LINE__)
    allocate(extraterrestrial_radiation_flux(photolysis_wavelength_grid_section_dimension), &
              stat=errcode, errmsg=errmsg)
    call check_allocate(errcode, subroutine_name, &
                        'extraterrestrial_radiation_flux(photolysis_wavelength_grid_section_dimension)', &
                        file=__FILE__, line=__LINE__)
    allocate(surface_albedo(horizontal_dimension), stat=errcode, errmsg=errmsg)
    call check_allocate(errcode, subroutine_name, &
                        'surface_albedo(horizontal_dimension)', &
                        file=__FILE__, line=__LINE__)
    allocate(blackbody_temperature_at_surface(horizontal_dimension), stat=errcode, errmsg=errmsg)
    call check_allocate(errcode, subroutine_name, &
                        'blackbody_temperature_at_surface(horizontal_dimension)', &
                        file=__FILE__, line=__LINE__)

    surface_albedo(:) = 0.1_kind_phys
    blackbody_temperature_at_surface(:) = 292.3_kind_phys
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
