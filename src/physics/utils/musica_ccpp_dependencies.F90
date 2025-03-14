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
  
    use ccpp_kinds,  only: kind_phys
    use cam_logfile, only: iulog
  
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
  
      !> Definition of musica species object
    type, private :: temp_musica_species_t
      character(len=:), allocatable :: name
      real(kind_phys)               :: constituent_value = 0.0_kind_phys
    end type temp_musica_species_t
  
    interface temp_musica_species_t
      procedure species_constructor
    end interface temp_musica_species_t

  !==============================================================================
  contains
  !==============================================================================
  
    !> Constructor for temporary musica species object
    function species_constructor(name, value) result( this )
      character(len=*), intent(in) :: name
      real(kind_phys),  intent(in)  :: value
      type(temp_musica_species_t)   :: this
  
      this%name = name
      this%constituent_value = value
  
    end function species_constructor
  
    !> Constructor for temporary musica species object
    subroutine initialize_musica_species_constituents()
      use cam_ccpp_cap,              only: cam_model_const_properties
      use cam_ccpp_cap,              only: cam_constituents_array
      use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t
      use ccpp_const_utils,          only: ccpp_const_get_idx
      use musica_ccpp_namelist,      only: filename_of_micm_configuration
      use phys_vars_init_check, only: mark_as_initialized

      character(len=*), parameter :: chapman_config = 'chapman'
      character(len=*), parameter :: terminator_config = 'terminator'
      logical                     :: is_chapman = .false.
      logical                     :: is_terminator = .false.
      integer                     :: num_micm_species = 0
      integer                     :: num_tuvx_constituents = 1
      integer                     :: num_tuvx_only_gas_species = 0
      integer                     :: position
      integer                     :: constituent_index
      integer                     :: errcode
      character(len=512)          :: errmsg
      integer                     :: i_elem
  
      real(kind_phys),                   pointer     :: constituent_array(:,:,:)
      type(ccpp_constituent_prop_ptr_t), pointer     :: constituent_props(:)
      type(temp_musica_species_t),       allocatable :: species_group(:)

      constituent_array => cam_constituents_array()
      constituent_props => cam_model_const_properties()
  
      ! Check if the substring exists
      position = index(filename_of_micm_configuration, chapman_config)
  
      if (position > 0) then
        is_chapman = .true.
        write(iulog,*) "   [JW] Config is Chapman "
      else
        position = index(filename_of_micm_configuration, terminator_config)
        if (position > 0) then
          is_terminator = .true.
          write(iulog,*) "   [JW] Config is Terminator "
        else
          write(iulog,*) "   [JW] Config is not found"
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

      allocate (species_group(num_micm_species + num_tuvx_constituents + num_tuvx_only_gas_species))
      ! TODO(jiwon) have to conver to cam-sima unit [mol m-3]
      species_group(1) = species_constructor("cloud_liquid_water_mixing_ratio_wrt_moist_air_and_condensed_water", &
                          1.0e-3_kind_phys)
      if (is_chapman) then   
        species_group(2) = species_constructor("O2", 0.21_kind_phys)
        species_group(3) = species_constructor("O", 1.0e-9_kind_phys)
        species_group(4) = species_constructor("O1D", 1.0e-9_kind_phys)
        species_group(5) = species_constructor("O3", 1.0e-4_kind_phys)
        species_group(6) = species_constructor("N2", 0.79_kind_phys)
        species_group(7) = species_constructor("air", 1.0e-3_kind_phys)
  
      else if (is_terminator) then
        species_group(2) = species_constructor("Cl", 1.05e-4_kind_phys)
        species_group(3) = species_constructor("Cl2", 1.05e-4_kind_phys)
        species_group(4) = species_constructor("air", 1.0e-3_kind_phys)
        species_group(5) = species_constructor("O2", 0.21_kind_phys)
        species_group(6) = species_constructor("O3", 1.0e-4_kind_phys)
      end if

      do i_elem = 1, num_micm_species + num_tuvx_constituents + num_tuvx_only_gas_species
        write(iulog,*) "   [JW] Calling ccpp_const_get_idx "
        call ccpp_const_get_idx(constituent_props, trim(species_group(i_elem)%name), &
                                constituent_index, errmsg, errcode)
        call mark_as_initialized(trim(species_group(i_elem)%name))

        write(iulog,*) "   [JW] constituent_index ", constituent_index
        write(iulog,*) "   [JW] species_group(i_elem)%name ", trim(species_group(i_elem)%name)
        if (errcode /= 0) then 
          write(iulog,*) "   [JW] ccpp_const_get_idx error occurs: ", errmsg
          return
        end if
  
        constituent_array(:,:,constituent_index) = species_group(i_elem)%constituent_value
      end do

      deallocate (species_group)
    end subroutine initialize_musica_species_constituents
  
    !> Constructor for temporary musica species object
    subroutine musica_ccpp_dependencies_init(horizontal_dimension, &
        vertical_layer_dimension, log_file_unit)
  
      use cam_abortutils,       only: check_allocate
  
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

  
      write(log_file_unit,*) '   [JG] Calling initialize_musica_species_constituents.'
      call initialize_musica_species_constituents()
  
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
  