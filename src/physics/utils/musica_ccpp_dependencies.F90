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

  use ccpp_kinds,   only: kind_phys

  implicit none
  private

  public :: musica_ccpp_dependencies_init, set_initial_musica_concentrations

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

  !> Data structure for MUSICA species information
  type, private :: species_t
    character(len=:), allocatable :: name
    real(kind_phys)               :: initial_mixing_ratio = 0.0_kind_phys ! kg kg-1
    integer                       :: constituent_index = -1 ! index in the CCPP constituents array
  end type species_t

  interface species_t
    procedure species_t_constructor
  end interface species_t

  !> Indicator of whether MUSICA suite is being used
  logical :: is_musica_suite = .false.

  !> Set of species for TUV-x
  type(species_t), allocatable :: tuvx_species(:)

!==============================================================================
contains
!==============================================================================

  function species_t_constructor(name, initial_mixing_ratio) result( this )

    !-----------------------------------------------------------------------
    !
    ! Constructor for MUSICA species object
    !
    !-----------------------------------------------------------------------

    character(len=*), intent(in) :: name
    real(kind_phys),  intent(in) :: initial_mixing_ratio ! kg kg-1
    type(species_t)              :: this

    this%name = name
    this%initial_mixing_ratio = initial_mixing_ratio

  end function species_t_constructor

  subroutine initialize_musica_species_constituents(constituents_properties, &
              errmsg, errcode)

    !-----------------------------------------------------------------------
    !
    ! Initialize temporary MUSICA species constituents.
    !
    !-----------------------------------------------------------------------

    use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t
    use ccpp_const_utils,          only: ccpp_const_get_idx
    use cam_logfile,               only: iulog
    use spmd_utils,                only: primary_process => masterproc

    type(ccpp_constituent_prop_ptr_t), pointer :: constituents_properties(:)
    character(len=512),            intent(out) :: errmsg
    integer,                       intent(out) :: errcode

    ! local variables
    integer, parameter           :: num_tuvx_constituents = 4
    integer                      :: position
    integer                      :: constituent_index
    integer                      :: i_species

    if (.not. associated(constituents_properties)) then
      errcode = 1
      errmsg = "[MUSICA Error] The pointer to the constituents properties object is not associated."
      return
    end if

    allocate (tuvx_species(num_tuvx_constituents), stat=errcode, errmsg=errmsg)
    if (errcode /= 0) return

    tuvx_species(1) = species_t(&
        "cloud_liquid_water_mixing_ratio_wrt_moist_air_and_condensed_water", 0.0000060_kind_phys)
    tuvx_species(2) = species_t("air", 1.0_kind_phys)
    tuvx_species(3) = species_t("O2", 0.21_kind_phys)
    tuvx_species(4) = species_t("O3", 4.0e-6_kind_phys)
    
    do i_species = 1, num_tuvx_constituents
      call ccpp_const_get_idx(constituents_properties, trim(tuvx_species(i_species)%name), &
                              tuvx_species(i_species)%constituent_index, errmsg, errcode)
      if (errcode /= 0) return
    end do

  end subroutine initialize_musica_species_constituents

  subroutine set_initial_musica_concentrations(constituents_array, constituent_properties)

    use cam_abortutils,            only: endrun
    use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t
    use musica_ccpp_species,       only: micm_species_set
  
    !-----------------------------------------------------------------------
    !
    ! Set initial concentrations for MUSICA species.
    !
    !-----------------------------------------------------------------------

    real(kind_phys), intent(inout) :: constituents_array(:,:,:)
    type(ccpp_constituent_prop_ptr_t), pointer, intent(inout) :: constituent_properties(:)

    ! local variables
    integer            :: i_species, i_constituent
    character(len=512) :: errmsg
    integer            :: errcode
    real(kind_phys)    :: default_value
    
    ! Don't do anything if MUSICA suite is not being used
    if (.not. is_musica_suite) return

    if (.not. allocated(tuvx_species)) then
      errmsg = "[MUSICA Error] MUSICA species are not initialized."
      call endrun(errmsg, file=__FILE__, line=__LINE__)
    end if

    do i_species = 1, size(tuvx_species)
      constituents_array(:,:,tuvx_species(i_species)%constituent_index) = &
        tuvx_species(i_species)%initial_mixing_ratio
    end do

    do i_species = 1, size(micm_species_set)
      i_constituent = micm_species_set(i_species)%index_constituent_props
      call constituent_properties(i_constituent)%default_value( &
            default_value, errcode, errmsg)
      if (errcode /= 0) then
        call endrun(errmsg, file=__FILE__, line=__LINE__)
      end if
      constituents_array(:,:,i_constituent) = default_value
    end do

  end subroutine set_initial_musica_concentrations

  subroutine musica_ccpp_dependencies_init( &
              horizontal_dimension, vertical_layer_dimension, &
              constituents_properties, phys_suite_name)

    use cam_abortutils,            only: check_allocate, endrun
    use cam_logfile,               only: iulog
    use spmd_utils,                only: primary_process => masterproc
    use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t

    !-----------------------------------------------------------------------
    !
    ! Initialize the MUSICA scheme dependencies.
    !
    !-----------------------------------------------------------------------

    integer,                        intent(in) :: horizontal_dimension
    integer,                        intent(in) :: vertical_layer_dimension
    type(ccpp_constituent_prop_ptr_t), pointer :: constituents_properties(:)
    character(len=*),               intent(in) :: phys_suite_name            ! name of the physics suite being run

    ! local variables
    character(len=*), parameter :: subroutine_name = &
        trim(module_name)//':(musica_ccpp_dependencies_init)'
    character(len=512)          :: errmsg
    integer                     :: errcode

    ! Check if a MUSICA configuration is being used.  If not then just exit.
    if (trim(phys_suite_name) /= "musica") return
    is_musica_suite = .true.

    if (primary_process) then
        write(iulog,*) 'WARNING: Using placeholder data for MUSICA chemistry.'
    end if

    call initialize_musica_species_constituents(constituents_properties, errmsg, errcode)
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
