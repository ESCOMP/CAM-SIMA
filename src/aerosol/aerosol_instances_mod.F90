module aerosol_instances_mod
  ! aerosol_instances_mod owns and manages the concrete aerosol_properties and
  ! aerosol_state objects for every active aerosol model (modal, CARMA, bulk)
  ! and every radiation list (climate + diagnostics).
  !
  ! Lifecycle in CAM-SIMA:
  !   1. aerosol_instances_init()        -- called once during init, after
  !      rad_aer_init().  Creates persistent aerosol_properties objects for
  !      each (aerosol_model, list_idx) pair.
  !   2. aerosol_instances_init_states() -- called once during init, after
  !      aerosol_instances_init().  Creates persistent aerosol_state objects
  !      for each (aerosol_model, list_idx) pair.  States store a pointer
  !      to the CCPP constituents array, which persists for the run.
  !   3. aerosol_instances_get_props()   -- returns a pointer to a properties
  !      object for a given (aerosol_model, list_idx).
  !   4. aerosol_instances_get_state()   -- returns a pointer to a state
  !      object for a given (aerosol_model, list_idx).
  !   5. aerosol_instances_final()       -- deallocates all objects at shutdown.

  use aerosol_properties_mod,        only: aerosol_properties
  use aerosol_state_mod,             only: aerosol_state
  use radiative_aerosol_definitions, only: N_DIAG

  implicit none
  private

  public :: aerosol_instances_init
  public :: aerosol_instances_init_states
  public :: aerosol_instances_get_props
  public :: aerosol_instances_get_state
  public :: aerosol_instances_get_num_models
  public :: aerosol_instances_is_active
  public :: aerosol_instances_final
  public :: aero_state_entry_t

  type :: aero_props_entry_t
     class(aerosol_properties), pointer :: obj => null()
  end type aero_props_entry_t

  type :: aero_state_entry_t
     class(aerosol_state),      pointer :: obj => null()
  end type aero_state_entry_t

  ! Module holds aerosol properties objects, dimensioned (iaermod, 0:N_DIAG).
  type(aero_props_entry_t), allocatable, target :: aero_props_all(:,:)

  ! Persistent aerosol state objects, dimensioned (iaermod, 0:N_DIAG).
  ! States store a pointer to the CCPP constituents array which persists for the run.
  type(aero_state_entry_t), allocatable, target :: aero_states_all(:,:)

  ! Number of aerosol models active at runtime.
  ! Note: Multiple aerosol models can be active at once, e.g., using bulk for volcanic aerosol and modal for others.
  ! When retrieving properties via aerosol_instances_get_props, ensure that the aerosol model
  ! matches what is needed (e.g., aero_props%model_is('MAM') == .true.)
  integer :: num_aero_models_ = 0

  logical :: modal_active_ = .false.
  logical :: carma_active_ = .false.
  logical :: bulk_active_  = .false.

contains
  subroutine aerosol_instances_init()
    use radiative_aerosol, only: rad_aer_get_info
    use radiative_aerosol_definitions, only: active_calls
    use modal_aerosol_properties_mod, only: modal_aerosol_properties
    use carma_aerosol_properties_mod, only: carma_aerosol_properties
    use bulk_aerosol_properties_mod,  only: bulk_aerosol_properties
    use cam_abortutils, only: endrun

    use spmd_utils, only: masterproc
    use cam_logfile, only: iulog

    integer :: nmodes, nbins, nbulk_aerosols
    integer :: iaermod, ilist, istat

    character(len=*), parameter :: subname = 'aerosol_instances_init: '

    num_aero_models_ = 0

    call rad_aer_get_info(0, nmodes=nmodes, nbins=nbins, naero=nbulk_aerosols)
    modal_active_ = nmodes > 0
    carma_active_ = nbins > 0
    bulk_active_  = nbulk_aerosols > 0

    if (masterproc) then
       write(iulog,*) subname,'nmodes,nbins,nbulk_aerosols: ',nmodes,nbins,nbulk_aerosols
    end if

    if (modal_active_) num_aero_models_ = num_aero_models_ + 1
    if (carma_active_) num_aero_models_ = num_aero_models_ + 1
    if (bulk_active_)  num_aero_models_ = num_aero_models_ + 1

    if (num_aero_models_ < 1) return

    allocate(aero_props_all(num_aero_models_, 0:N_DIAG), stat=istat)
    if (istat /= 0) then
       call endrun(subname//'allocation error: aero_props_all')
    end if

    do ilist = 0, N_DIAG
       ! only populate aerosol properties for active climate/diagnostic lists.
       if (.not. active_calls(ilist)) cycle

       call rad_aer_get_info(ilist, nmodes=nmodes, nbins=nbins, naero=nbulk_aerosols)

       iaermod = 0
       if (modal_active_) then
          iaermod = iaermod + 1
          if (nmodes > 0) then
             aero_props_all(iaermod, ilist)%obj => modal_aerosol_properties(ilist)
          end if
       end if
       if (carma_active_) then
          iaermod = iaermod + 1
          if (nbins > 0) then
             aero_props_all(iaermod, ilist)%obj => carma_aerosol_properties(ilist)
          end if
       end if
       if (bulk_active_) then
          iaermod = iaermod + 1
          if (nbulk_aerosols > 0) then
             aero_props_all(iaermod, ilist)%obj => bulk_aerosol_properties(ilist)
          end if
       end if
    end do

  end subroutine aerosol_instances_init

  function aerosol_instances_get_props(iaermod, list_idx) result(props)
    integer, intent(in) :: iaermod
    integer, intent(in) :: list_idx
    class(aerosol_properties), pointer :: props

    props => aero_props_all(iaermod, list_idx)%obj

  end function aerosol_instances_get_props

  pure integer function aerosol_instances_get_num_models()
    aerosol_instances_get_num_models = num_aero_models_
  end function aerosol_instances_get_num_models

  logical function aerosol_instances_is_active(model_name)
    character(len=*), intent(in) :: model_name

    select case (trim(model_name))
    case ('modal')
       aerosol_instances_is_active = modal_active_
    case ('carma')
       aerosol_instances_is_active = carma_active_
    case ('bulk')
       aerosol_instances_is_active = bulk_active_
    case default
       aerosol_instances_is_active = .false.
    end select

  end function aerosol_instances_is_active

  subroutine aerosol_instances_final()
    integer :: iaermod, ilist

    ! Deallocate persistent state objects
    if (allocated(aero_states_all)) then
       do ilist = 0, N_DIAG
          do iaermod = 1, num_aero_models_
             if (associated(aero_states_all(iaermod, ilist)%obj)) then
                deallocate(aero_states_all(iaermod, ilist)%obj)
                nullify(aero_states_all(iaermod, ilist)%obj)
             end if
          end do
       end do
       deallocate(aero_states_all)
    end if

    ! Deallocate properties objects
    if (allocated(aero_props_all)) then
       do ilist = 0, N_DIAG
          do iaermod = 1, num_aero_models_
             if (associated(aero_props_all(iaermod, ilist)%obj)) then
                deallocate(aero_props_all(iaermod, ilist)%obj)
                nullify(aero_props_all(iaermod, ilist)%obj)
             end if
          end do
       end do
       deallocate(aero_props_all)
    end if

    num_aero_models_ = 0

  end subroutine aerosol_instances_final

  ! Initialize persistent aerosol state objects for all active lists
  ! and all active aerosol models.
  !
  ! Called once at init time, after aerosol_instances_init().
  ! States store a pointer to the CCPP constituents array which persists
  ! for the entire run.
  subroutine aerosol_instances_init_states(constituents)
    use radiative_aerosol_definitions, only: active_calls
    use modal_aerosol_state_mod, only: modal_aerosol_state
    use carma_aerosol_state_mod, only: carma_aerosol_state
    use bulk_aerosol_state_mod,  only: bulk_aerosol_state
    use ccpp_kinds,     only: kind_phys
    use cam_abortutils, only: endrun

    real(kind_phys), pointer, intent(in) :: constituents(:,:,:)

    integer :: iaermod, ilist, istat
    character(len=*), parameter :: subname = 'aerosol_instances_init_states: '

    if (num_aero_models_ < 1) return

    allocate(aero_states_all(num_aero_models_, 0:N_DIAG), stat=istat)
    if (istat /= 0) then
       call endrun(subname//'allocation error: aero_states_all')
    end if

    do ilist = 0, N_DIAG
       if (.not. active_calls(ilist)) cycle

       iaermod = 0
       if (modal_active_) then
          iaermod = iaermod + 1
          if (associated(aero_props_all(iaermod, ilist)%obj)) then
             aero_states_all(iaermod, ilist)%obj => &
                  modal_aerosol_state(constituents, ilist)
          end if
       end if
       if (carma_active_) then
          iaermod = iaermod + 1
          if (associated(aero_props_all(iaermod, ilist)%obj)) then
             aero_states_all(iaermod, ilist)%obj => &
                  carma_aerosol_state(constituents, ilist)
          end if
       end if
       if (bulk_active_) then
          iaermod = iaermod + 1
          if (associated(aero_props_all(iaermod, ilist)%obj)) then
             aero_states_all(iaermod, ilist)%obj => &
                  bulk_aerosol_state(constituents, ilist)
          end if
       end if
    end do

  end subroutine aerosol_instances_init_states

  function aerosol_instances_get_state(iaermod, list_idx) result(astate)
    integer, intent(in) :: iaermod
    integer, intent(in) :: list_idx
    class(aerosol_state), pointer :: astate

    astate => aero_states_all(iaermod, list_idx)%obj

  end function aerosol_instances_get_state

end module aerosol_instances_mod
