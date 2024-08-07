module cam_history_support

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!
   !!  cam_history_support is used by cam_history as well as by the dycores
   !!    (for vertical coordinate support).
   !!
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use shr_kind_mod,     only: r8=>shr_kind_r8, shr_kind_cl, shr_kind_cxx
   use pio,              only: var_desc_t, file_desc_t, PIO_MAX_NAME
   use cam_abortutils,   only: endrun
   use cam_logfile,      only: iulog
   use spmd_utils,       only: masterproc
   use cam_grid_support, only: cam_grid_patch_t, cam_grid_header_info_t
   use cam_grid_support, only: max_hcoordname_len
   use cam_pio_utils,    only: cam_pio_handle_error

   implicit none
   private
   save

   integer, parameter, public :: fieldname_len = 32              ! max chars for field name
   integer, parameter, public :: fieldname_suffix_len =  3       ! length of field name suffix ("&IC")
   ! max_fieldname_len = max chars for field name (including suffix)
   integer, parameter, public :: max_fieldname_len    = fieldname_len + fieldname_suffix_len
   ! default fill value for history NetCDF fields
   real(r8), parameter, public :: hist_default_fillvalue = 1.e36_r8
   integer,  parameter, public :: pfiles = 12        ! max number of tapes
   integer, parameter, public :: max_chars = shr_kind_cl  ! max chars for char variables
   integer, parameter, public :: max_string_len = shr_kind_cxx
   real(r8), parameter, public :: fillvalue = 1.e36_r8     ! fill value for netcdf fields
   ! A special symbol for declaring a field which has no vertical or
   ! non-grid dimensions. It is here (rather than cam_history) so that it
   ! be checked by add_hist_coord
   character(len=10), parameter, public :: horiz_only = 'horiz_only'

  !---------------------------------------------------------------------------
  !
  !  formula_terms_t: Information for formula terms (CF convention) variables
  !                   Used to add a formula-terms variable to the history file
  !                   Also adds a string, '<name>: <var_name>' to the parent
  !                   mdim's 'formula_terms' attribute.
  !
  !---------------------------------------------------------------------------
  type, public :: formula_terms_t
    character(len=max_fieldname_len) :: a_name = ''   ! 'A' term variable name
    character(len=max_string_len)    :: a_long_name = '' ! 'A' long name
    real(r8), pointer                :: a_values(:) => null() ! 'A' variable values
    character(len=max_fieldname_len) :: b_name = ''   ! 'B' term variable name
    character(len=max_string_len)    :: b_long_name = '' ! 'B' long name
    real(r8), pointer                :: b_values(:) => null() ! 'B' variable values
    character(len=max_fieldname_len) :: p0_name = ''  ! 'p0' term variable name
    character(len=max_string_len)    :: p0_long_name = '' ! 'p0' long name
    character(len=max_chars)         :: p0_units = '' ! 'p0' variable units
    real(r8)                         :: p0_value = fillvalue ! 'p0' variable values
    character(len=max_fieldname_len) :: ps_name = ''  ! 'ps' term variable name
  end type formula_terms_t

  !---------------------------------------------------------------------------
  !
  !  hist_coord_t: Information for history variable dimension attributes
  !
  !---------------------------------------------------------------------------
  type, public :: hist_coord_t
    character(len=max_hcoordname_len) :: name = ''  ! coordinate name
    integer                  :: dimsize = 0       ! size of dimension
    character(len=max_hcoordname_len) :: dimname = '' ! optional dimension name
    character(len=max_chars) :: long_name = ''    ! 'long_name' attribute
    character(len=max_chars) :: units = ''        ! 'units' attribute
    character(len=max_chars) :: bounds_name = ''  ! 'bounds' attribute (& name of bounds variable)
    character(len=max_chars) :: standard_name = ''! 'standard_name' attribute
    character(len=4)         :: positive = ''     ! 'positive' attribute ('up' or 'down')
    integer,  pointer        :: integer_values(:) => null() ! dim values if integral
    real(r8), pointer        :: real_values(:) => null() ! dim values if real
    real(r8), pointer        :: bounds(:,:) => null() ! dim bounds
    type(formula_terms_t)    :: formula_terms     ! vars for formula terms
    logical                  :: integer_dim       ! .true. iff dim has integral values
    logical                  :: vertical_coord    ! .true. iff dim is vertical
  end type hist_coord_t

  ! Some parameters for use with interpolated output namelist items
  integer,          parameter, public :: interp_type_native            = 0
  integer,          parameter, public :: interp_type_bilinear          = 1
  integer,          parameter, public :: interp_gridtype_equal_poles   = 1
  integer,          parameter, public :: interp_gridtype_gauss         = 2
  integer,          parameter, public :: interp_gridtype_equal_nopoles = 3
  !---------------------------------------------------------------------------
  !
  !  interp_info_t: Information for lat/lon interpolated history output
  !
  !---------------------------------------------------------------------------
  type, public :: interp_info_t
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
  end type interp_info_t

  !! Coordinate variables
  integer,                     public :: registeredmdims = 0
  integer,                     public :: maxvarmdims     = 1
  character(len=9), parameter, public :: mdim_var_name   = 'mdimnames'
  integer,          parameter         :: maxmdims        = 25  ! arbitrary limit
  type(hist_coord_t),          public :: hist_coords(maxmdims)

  public     :: write_hist_coord_attrs
  public     :: write_hist_coord_vars
  public     :: add_hist_coord, add_vert_coord
  public     :: lookup_hist_coord_indices
  public     :: hist_coord_find_levels
  public     :: get_hist_coord_index

  interface add_hist_coord
    module procedure add_hist_coord_regonly
    module procedure add_hist_coord_int
    module procedure add_hist_coord_r8
  end interface

  interface check_hist_coord
    ! NB: This is supposed to be a private interface
    ! check_hist_coord: returns 0 if <name> is not registered as an mdim
    !                   returns i if <name> is registered with compatible values
    !              calls endrun if <name> is registered with incompatible values
    ! Versions without the <name> argument return .true. or .false.
    module procedure check_hist_coord_char
    module procedure check_hist_coord_int
    module procedure check_hist_coord_int1
    module procedure check_hist_coord_r8
    module procedure check_hist_coord_r81
    module procedure check_hist_coord_r82
    module procedure check_hist_coord_ft
    module procedure check_hist_coord_all
  end interface

  !!---------------------------------------------------------------------------
  
  CONTAINS

  integer function get_hist_coord_index(mdimname)
    ! Input variables
    character(len=*), intent(in)            :: mdimname
    ! Local variable
    integer :: i

    get_hist_coord_index = -1
    do i = 1, registeredmdims
      if(trim(mdimname) == trim(hist_coords(i)%name)) then
        get_hist_coord_index = i
        exit
      end if
    end do

  end function get_hist_coord_index


  ! Functions to check consistent term definition for hist coords
  logical function check_hist_coord_char(defined, input)

    ! Input variables
    character(len=*), intent(in)            :: defined
    character(len=*), intent(in), optional  :: input

    if (len_trim(defined) == 0) then
      ! In this case, we assume the current value is undefined so any input OK
      check_hist_coord_char = .true.
    else if (present(input)) then
      ! We have to match definitions
      check_hist_coord_char = (trim(input) == trim(defined))
    else
      ! Not sure here. We have a value and are redefining without one?
      check_hist_coord_char = .false.
    end if
  end function check_hist_coord_char

  logical function check_hist_coord_int(defined, input)

    ! Input variables
    integer, intent(in)            :: defined
    integer, intent(in), optional  :: input

    if (defined == 0) then
      ! In this case, we assume the current value is undefined so any input OK
      check_hist_coord_int = .true.
    else if (present(input)) then
      ! We have to match definitions
      check_hist_coord_int = (input == defined)
    else
      ! Not sure here. We have a value and are redefining without one?
      check_hist_coord_int = .false.
    end if
  end function check_hist_coord_int

  logical function check_hist_coord_int1(defined, input)

    ! Input variables
    integer,             pointer            :: defined(:)
    integer, intent(in),          optional  :: input(:)

    ! Local variables
    integer                                 :: i

    if (.not. associated(defined)) then
      ! In this case, we assume the current value is undefined so any input OK
      check_hist_coord_int1 = .true.
    else if (present(input)) then
      ! We have to match definitions
      check_hist_coord_int1 = (size(input) == size(defined))
    else
      ! Not sure here. We have a value and are redefining without one?
      check_hist_coord_int1 = .false.
    end if
    if (check_hist_coord_int1 .and. associated(defined)) then
      ! Need to check the values
      do i = 1, size(defined)
        if (defined(i) /= input(i)) then
          check_hist_coord_int1 = .false.
          exit
        end if
      end do
    end if
  end function check_hist_coord_int1

  logical function check_hist_coord_r8(defined, input)

    ! Input variables
    real(r8), intent(in)            :: defined
    real(r8), intent(in), optional  :: input

    if (defined == fillvalue) then
      ! In this case, we assume the current value is undefined so any input OK
      check_hist_coord_r8 = .true.
    else if (present(input)) then
      ! We have to match definitions
      check_hist_coord_r8 = (input == defined)
    else
      ! Not sure here. We have a value and are redefining without one?
      check_hist_coord_r8 = .false.
    end if
  end function check_hist_coord_r8

  logical function check_hist_coord_r81(defined, input)

    ! Input variables
    real(r8),             pointer            :: defined(:)
    real(r8), intent(in),          optional  :: input(:)

    ! Local variables
    integer                                  :: i

    if (.not. associated(defined)) then
      ! In this case, we assume the current value is undefined so any input OK
      check_hist_coord_r81 = .true.
    else if (present(input)) then
      ! We have to match definitions
      check_hist_coord_r81 = (size(input) == size(defined))
    else
      ! Not sure here. We have a value and are redefining without one?
      check_hist_coord_r81 = .false.
    end if
    if (check_hist_coord_r81 .and. associated(defined)) then
      ! Need to check the values
      do i = 1, size(defined)
        if (defined(i) /= input(i)) then
          check_hist_coord_r81 = .false.
          exit
        end if
      end do
    end if
  end function check_hist_coord_r81

  logical function check_hist_coord_r82(defined, input)

    ! Input variables
    real(r8),             pointer            :: defined(:,:)
    real(r8), intent(in),          optional  :: input(:,:)

    ! Local variables
    integer                                  :: i, j

    if (.not. associated(defined)) then
      ! In this case, we assume the current value is undefined so any input OK
      check_hist_coord_r82 = .true.
    else if (present(input)) then
      ! We have to match definitions
      check_hist_coord_r82 = ((size(input, 1) == size(defined, 1)) .and.    &
                              (size(input, 2) == size(defined, 2)))
    else
      ! Not sure here. We have a value and are redefining without one?
      check_hist_coord_r82 = .false.
    end if
    if (check_hist_coord_r82 .and. associated(defined)) then
      ! Need to check the values
      do j = 1, size(defined, 2)
        do i = 1, size(defined, 1)
          if (defined(i, j) /= input(i, j)) then
            check_hist_coord_r82 = .false.
            exit
          end if
        end do
      end do
    end if
  end function check_hist_coord_r82

  logical function check_hist_coord_ft(defined, input)

    ! Input variables
    type(formula_terms_t), intent(in)           :: defined
    type(formula_terms_t), intent(in), optional :: input

    ! We will assume that if formula_terms has been defined, a_name has a value
    if (len_trim(defined%a_name) == 0) then
      ! In this case, we assume the current value is undefined so any input OK
      check_hist_coord_ft = .true.
    else if (present(input)) then
      ! We have to match definitions
      ! Need to check the values
      check_hist_coord_ft =                                                   &
           check_hist_coord(defined%a_name,       input%a_name)         .and. &
           check_hist_coord(defined%a_long_name,  input%a_long_name)    .and. &
           check_hist_coord(defined%a_values,     input%a_values)       .and. &
           check_hist_coord(defined%b_name,       input%b_name)         .and. &
           check_hist_coord(defined%b_long_name,  input%b_long_name)    .and. &
           check_hist_coord(defined%b_values,     input%b_values)       .and. &
           check_hist_coord(defined%p0_name,      input%p0_name)        .and. &
           check_hist_coord(defined%p0_long_name, input%p0_long_name)   .and. &
           check_hist_coord(defined%p0_units,     input%p0_units)       .and. &
           check_hist_coord(defined%p0_value,     input%p0_value)       .and. &
           check_hist_coord(defined%ps_name,      input%ps_name)
    else
      ! Not sure here. We have a value and are redefining without one?
      check_hist_coord_ft = .false.
    end if
  end function check_hist_coord_ft

  ! check_hist_coord: returns 0 if <name> is not registered as a hist coord
  !                   returns i if <name> is registered with compatible values
  !                   calls endrun if <name> is registered with incompatible
  !                   values
  integer function check_hist_coord_all(name, vlen, long_name, units, bounds, &
       i_values, r_values, bounds_name, positive, standard_name, formula_terms)

    ! Input variables
    character(len=*),      intent(in)            :: name
    integer,               intent(in)            :: vlen
    character(len=*),      intent(in),  optional :: long_name
    character(len=*),      intent(in),  optional :: units
    character(len=*),      intent(in),  optional :: bounds_name
    integer,               intent(in),  optional :: i_values(:)
    real(r8),              intent(in),  optional :: r_values(:)
    real(r8),              intent(in),  optional :: bounds(:,:)
    character(len=*),      intent(in),  optional :: positive
    character(len=*),      intent(in),  optional :: standard_name
    type(formula_terms_t), intent(in),  optional :: formula_terms

    ! Local variables
    character(len=120)                           :: errormsg
    integer                                      :: i

    i = get_hist_coord_index(trim(name))
    ! If i > 0, this mdim has already been registered
    if (i > 0) then
      check_hist_coord_all = i
      if (.not. check_hist_coord(hist_coords(i)%dimsize, vlen)) then
        write(errormsg, *) 'ERROR: Attempt to register dimension, '//trim(name)//' with incompatible size'
        call endrun(errormsg)
      end if
      if (.not. check_hist_coord(hist_coords(i)%long_name, long_name)) then
        write(errormsg, *) 'ERROR: Attempt to register dimension, ',trim(name),' with a different long_name'
        call endrun(errormsg)
      end if
      if (.not. check_hist_coord(hist_coords(i)%units, units)) then
        write(errormsg, *) 'ERROR: Attempt to register dimension, ',trim(name),' with different units'
        call endrun(errormsg)
      end if
      if (.not. check_hist_coord(hist_coords(i)%bounds_name, bounds_name)) then
        write(errormsg, *) 'ERROR: Attempt to register dimension, ',trim(name),' with a different bounds_name'
        call endrun(errormsg)
      end if
      if (.not. check_hist_coord(hist_coords(i)%standard_name, standard_name)) then
        write(errormsg, *) 'ERROR: Attempt to register dimension, ',trim(name),' with a different standard_name'
        call endrun(errormsg)
      end if
      if (.not. check_hist_coord(hist_coords(i)%positive, positive)) then
        write(errormsg, *) 'ERROR: Attempt to register dimension, ',trim(name),' with a different value of positive'
        call endrun(errormsg)
      end if
      ! Since the integer_dim defaults to .true., double check which to check
      if ((.not. hist_coords(i)%integer_dim) .or.                             &
           associated(hist_coords(i)%real_values)) then
        if (.not. check_hist_coord(hist_coords(i)%real_values, r_values)) then
          write(errormsg, *) 'ERROR: Attempt to register dimension, ',trim(name),' with different values'
          call endrun(errormsg)
        else if (present(i_values)) then
          write(errormsg, *) 'ERROR: Attempt to register integer values for real dimension'
          call endrun(errormsg)
        end if
      else
        if (.not. check_hist_coord(hist_coords(i)%integer_values, i_values)) then
          write(errormsg, *) 'ERROR: Attempt to register dimension, ',trim(name),' with different values'
          call endrun(errormsg)
        else if (present(i_values) .and. present(r_values)) then
          write(errormsg, *) 'ERROR: Attempt to register real values for integer dimension'
          call endrun(errormsg)
        end if
      end if
      if (.not. check_hist_coord(hist_coords(i)%bounds, bounds)) then
        write(errormsg, *) 'ERROR: Attempt to register dimension, ',trim(name),' with different bounds'
        call endrun(errormsg)
      end if
      if (.not. check_hist_coord(hist_coords(i)%formula_terms, formula_terms)) then
        write(errormsg, *) 'ERROR: Attempt to register dimension, ',trim(name),' with different formula_terms'
        call endrun(errormsg)
      end if
    else
      check_hist_coord_all = 0
    end if
  end function check_hist_coord_all

  subroutine add_hist_coord_regonly(name, index)

    ! Input variable
    character(len=*),  intent(in)    :: name
    integer, optional, intent(out)   :: index

    ! Local variables
    character(len=120)               :: errormsg
    integer                          :: i

    if ((trim(name) == trim(horiz_only)) .or. (len_trim(name) == 0)) then
      call endrun('ADD_HIST_COORD: '//trim(name)//' is not a valid coordinate name')
    end if
    i = get_hist_coord_index(trim(name))
    ! If i > 0, this mdim has already been registered
    if (i <= 0) then
      registeredmdims = registeredmdims + 1
      if (registeredmdims > maxmdims) then
        call endrun('Too many dimensions in add_hist_coord.')
      end if
      if (len_trim(name) > max_hcoordname_len) then
        write(errormsg,'(a,i3,a)') 'History coord name exceeds the ',         &
             max_hcoordname_len, ' character length limit'
        call endrun(errormsg)
      end if
      hist_coords(registeredmdims)%name = trim(name)
      hist_coords(registeredmdims)%dimsize = 0
      hist_coords(registeredmdims)%long_name = ''
      hist_coords(registeredmdims)%units = ''
      hist_coords(registeredmdims)%integer_dim = .true.
      hist_coords(registeredmdims)%positive = ''
      hist_coords(registeredmdims)%standard_name = ''
      if (present(index)) then
        index = registeredmdims
      end if
    else
      if (present(index)) then
        index = i
      end if
    end if

  end subroutine add_hist_coord_regonly

  subroutine add_hist_coord_int(name, vlen, long_name, units, values,         &
       positive, standard_name, dimname)

    ! Input variables
    character(len=*), intent(in)                    :: name
    integer,          intent(in)                    :: vlen
    character(len=*), intent(in)                    :: long_name
    character(len=*), intent(in),          optional :: units
    integer,          intent(in),  target, optional :: values(:)
    character(len=*), intent(in),          optional :: positive
    character(len=*), intent(in),          optional :: standard_name
    character(len=*), intent(in),          optional :: dimname

    ! Local variables
    integer                                         :: i

    ! First, check to see if it is OK to add this coord
    i = check_hist_coord(name, vlen=vlen, long_name=long_name, units=units,   &
         i_values=values, positive=positive, standard_name=standard_name)
    ! Register the name if necessary
    if (i == 0) then
       call add_hist_coord(trim(name), i)
       if(masterproc) then
          write(iulog, '(3a,i0,a,i0)') 'Registering hist coord', trim(name),  &
               '(', i, ') with length: ', vlen
       end if
    end if

    ! Set the coord's values
    hist_coords(i)%dimsize = vlen
    if (len_trim(long_name) > max_chars) then
       if(masterproc) then
          write(iulog,*) 'WARNING: long_name for ',trim(name),' too long'
       end if
    end if
    hist_coords(i)%long_name = trim(long_name)
    if (present(units)) then
       hist_coords(i)%units = trim(units)
    else
       hist_coords(i)%units = ''
    end if
    hist_coords(i)%integer_dim = .true.
    if (present(values)) then
       hist_coords(i)%integer_values => values
    endif
    if (present(positive)) then
       hist_coords(i)%positive = trim(positive)
    end if
    if (present(standard_name)) then
       hist_coords(i)%standard_name = trim(standard_name)
    end if
    hist_coords(i)%vertical_coord = .false.
    if (present(dimname)) then
       hist_coords(i)%dimname = trim(dimname)
    else
       hist_coords(i)%dimname = ''
    end if

  end subroutine add_hist_coord_int

  subroutine add_hist_coord_r8(name, vlen, long_name, units, values,         &
       bounds_name, bounds, positive, standard_name, vertical_coord, dimname)

    ! Input variables
    character(len=*),      intent(in)                    :: name
    integer,               intent(in)                    :: vlen
    character(len=*),      intent(in)                    :: long_name
    character(len=*),      intent(in)                    :: units
    real(r8),              intent(in), target            :: values(:)
    character(len=*),      intent(in),          optional :: bounds_name
    real(r8),              intent(in), target,  optional :: bounds(:,:)
    character(len=*),      intent(in),          optional :: positive
    character(len=*),      intent(in),          optional :: standard_name
    logical,               intent(in),          optional :: vertical_coord
    character(len=*),      intent(in),          optional :: dimname

    ! Local variables
    character(len=120)                                   :: errormsg
    integer                                              :: i

    ! First, check to see if it is OK to add this coord
    i = check_hist_coord(name, vlen=vlen, long_name=long_name, units=units,   &
         r_values=values, positive=positive, standard_name=standard_name,     &
         bounds_name=bounds_name, bounds=bounds)
    ! Register the name if necessary
    if (i == 0) then
       call add_hist_coord(trim(name), i)
       if(masterproc) then
          write(iulog, '(3a,i0,a,i0)') 'Registering hist coord', trim(name),  &
               '(', i, ') with length: ', vlen
       end if
    end if

    ! Set the coord's size
    hist_coords(i)%dimsize = vlen
    if (len_trim(long_name) > max_chars) then
       if(masterproc) then
          write(iulog,*) 'WARNING: long_name for ',trim(name),' too long'
       end if
    end if
    hist_coords(i)%long_name = trim(long_name)
    if (len_trim(units) > 0) then
       hist_coords(i)%units = trim(units)
    else
       hist_coords(i)%units = '1'
    end if
    hist_coords(i)%integer_dim = .false.
    hist_coords(i)%real_values => values
    if (present(positive)) then
       hist_coords(i)%positive = trim(positive)
    end if
    if (present(standard_name)) then
       hist_coords(i)%standard_name = trim(standard_name)
    end if
    if (present(bounds_name)) then
       hist_coords(i)%bounds_name = trim(bounds_name)
       if (.not. present(bounds)) then
          write(errormsg,*) 'bounds must be present for ',trim(bounds_name)
          call endrun(errormsg)
       end if
       hist_coords(i)%bounds => bounds
    else if (present(bounds)) then
       write(errormsg,*) 'bounds_name must be present for bounds values'
       call endrun(errormsg)
    else
       hist_coords(i)%bounds_name = ''
    end if
    if (present(vertical_coord)) then
       hist_coords(i)%vertical_coord = vertical_coord
    else
       hist_coords(i)%vertical_coord = .false.
    end if
    if (present(dimname)) then
       hist_coords(i)%dimname = trim(dimname)
    else
       hist_coords(i)%dimname = ''
    end if

  end subroutine add_hist_coord_r8

  subroutine add_vert_coord(name, vlen, long_name, units, values,            &
       positive, standard_name, formula_terms)

    ! Input variables
    character(len=*),      intent(in)                    :: name
    integer,               intent(in)                    :: vlen
    character(len=*),      intent(in)                    :: long_name
    character(len=*),      intent(in)                    :: units
    real(r8),              intent(in), target            :: values(:)
    character(len=*),      intent(in),          optional :: positive
    character(len=*),      intent(in),          optional :: standard_name
    type(formula_terms_t), intent(in),          optional :: formula_terms

    ! Local variable
    integer                                              :: i

    ! First, check to see if it is OK to add this coord
    i = check_hist_coord(name, vlen=vlen, long_name=long_name, units=units,   &
         r_values=values, positive=positive, standard_name=standard_name,     &
         formula_terms=formula_terms)
    ! Register the name and hist_coord values if necessary
    if (i == 0) then
      call add_hist_coord(trim(name), vlen, long_name, units, values,         &
           positive=positive, standard_name=standard_name,                    &
           vertical_coord=.true.)
      i = get_hist_coord_index(trim(name))
      if(masterproc) then
         write(iulog, '(3a,i0,a,i0)') 'Registering hist coord', trim(name),   &
              '(', i, ') with length: ', vlen
      end if
    end if

    if (present(formula_terms)) then
      hist_coords(i)%formula_terms = formula_terms
    end if

  end subroutine add_vert_coord

  subroutine write_hist_coord_attr(File, mdimind, boundsdim, dimonly, mdimid)
    use pio, only: file_desc_t, var_desc_t, pio_put_att, pio_noerr,           &
                   pio_int, pio_double, pio_inq_varid, pio_def_var
    use cam_pio_utils, only: cam_pio_def_dim, cam_pio_def_var

    ! Input variables
    type(file_desc_t), intent(inout) :: File           ! PIO file Handle
    integer,           intent(in)    :: mdimind        ! Internal dim index
    integer,           intent(in)    :: boundsdim      ! Bounds dimension ID
    logical,           intent(in)    :: dimonly        ! No def_var if .true.
    integer, optional, intent(out)   :: mdimid

    ! Local variables
    integer                          :: dimid          ! PIO dimension ID
    type(var_desc_t)                 :: vardesc        ! PIO variable descriptor
    character(len=120)               :: errormsg
    character(len=max_chars)         :: formula_terms  ! Constructed string
    integer                          :: ierr
    integer                          :: dtype
    logical                          :: defvar         ! True if var exists

    ! Create or check dimension for this coordinate
    if (len_trim(hist_coords(mdimind)%dimname) > 0) then
       ! Dim can already exist if different from coord name
       call cam_pio_def_dim(File, trim(hist_coords(mdimind)%dimname),         &
            hist_coords(mdimind)%dimsize, dimid,                              &
            existOK=(trim(hist_coords(mdimind)%dimname) /=                    &
                     trim(hist_coords(mdimind)%name)))
    else
       ! The dimension has the same name as the coord -- must be new dim
       call cam_pio_def_dim(File, trim(hist_coords(mdimind)%name),            &
            hist_coords(mdimind)%dimsize, dimid, existOK=.false.)
    end if
    ! If the caller wants to know the NetCDF dimension ID, set it here
    if (present(mdimid)) then
      mdimid = dimid
    end if
    if (.not. dimonly) then
      ! Time to define the variable (only if there are values)
      if (hist_coords(mdimind)%integer_dim) then
        dtype = pio_int
        defvar = associated(hist_coords(mdimind)%integer_values)
      else
        dtype = pio_double
        defvar = associated(hist_coords(mdimind)%real_values)
      end if
      if (defvar) then
        call cam_pio_def_var(File, trim(hist_coords(mdimind)%name), dtype,    &
             (/dimid/), vardesc, existOK=.false.)
        ! long_name
        ierr=pio_put_att(File, vardesc, 'long_name', trim(hist_coords(mdimind)%long_name))
        call cam_pio_handle_error(ierr, 'Error writing "long_name" attr in write_hist_coord_attr')
        ! units
        if(len_trim(hist_coords(mdimind)%units) > 0) then
          ierr=pio_put_att(File, vardesc, 'units', &
               trim(hist_coords(mdimind)%units))
          call cam_pio_handle_error(ierr, 'Error writing "units" attr in write_hist_coord_attr')
        end if
        ! positive
        if(len_trim(hist_coords(mdimind)%positive) > 0) then
          ierr=pio_put_att(File, vardesc, 'positive', &
               trim(hist_coords(mdimind)%positive))
          call cam_pio_handle_error(ierr, 'Error writing "positive" attr in write_hist_coord_attr')
        end if
        ! standard_name
        if(len_trim(hist_coords(mdimind)%standard_name) > 0) then
          ierr=pio_put_att(File, vardesc, 'standard_name', &
               trim(hist_coords(mdimind)%standard_name))
          call cam_pio_handle_error(ierr, 'Error writing "standard_name" attr in write_hist_coord_attr')
        end if
        ! formula_terms
        if(len_trim(hist_coords(mdimind)%formula_terms%a_name) > 0) then
          write(formula_terms, '("a: ",a," b: ",a," p0: ",a," ps: ",a)') &
               trim(hist_coords(mdimind)%formula_terms%a_name), &
               trim(hist_coords(mdimind)%formula_terms%b_name), &
               trim(hist_coords(mdimind)%formula_terms%p0_name),&
               trim(hist_coords(mdimind)%formula_terms%ps_name)
          ierr=pio_put_att(File, vardesc, 'formula_terms', trim(formula_terms))
          call cam_pio_handle_error(ierr, 'Error writing "formula_terms" attr in write_hist_coord_attr')
        end if
        ! bounds
        if (associated(hist_coords(mdimind)%bounds)) then
          ! Write name of the bounds variable
          ierr=pio_put_att(File, vardesc, 'bounds', trim(hist_coords(mdimind)%bounds_name))
          call cam_pio_handle_error(ierr, 'Error writing "bounds" attr in write_hist_coord_attr')
        end if
      end if

      ! Now, we need to define and populate the associated bounds variable
      ! NB: Reusing vardesc, no longer assocated with main variable
      if (associated(hist_coords(mdimind)%bounds)) then
        if (size(hist_coords(mdimind)%bounds,2) /= hist_coords(mdimind)%dimsize) then
          ! If anyone hits this check, add a new dimension for this case
          write(errormsg, *) 'The bounds variable, ',                         &
               trim(hist_coords(mdimind)%bounds_name),                        &
               ', needs to have dimension (2,', hist_coords(mdimind)%dimsize
          call endrun(errormsg)
        end if
        call cam_pio_def_var(File, trim(hist_coords(mdimind)%bounds_name),    &
             pio_double, (/boundsdim,dimid/), vardesc, existOK=.false.)
      end if

      ! See if we have formula_terms variables to define
      ! Define the "a" variable name
      ! NB: Reusing vardesc, no longer assocated with previous variables
      if (associated(hist_coords(mdimind)%formula_terms%a_values)) then
        if (size(hist_coords(mdimind)%formula_terms%a_values) /= hist_coords(mdimind)%dimsize) then
          write(errormsg, *) 'The forumla_terms variable, ',                  &
               trim(hist_coords(mdimind)%formula_terms%a_name),               &
               ', needs to have dimension', hist_coords(mdimind)%dimsize
          call endrun(errormsg)
        end if
        call cam_pio_def_var(File, trim(hist_coords(mdimind)%formula_terms%a_name), &
             pio_double, (/dimid/), vardesc, existOK=.false.)
        ierr = pio_put_att(File, vardesc, 'long_name', trim(hist_coords(mdimind)%formula_terms%a_long_name))
        call cam_pio_handle_error(ierr, 'Error writing "long_name" attr for "a" formula_term in write_hist_coord_attr')
      end if
      ! Define the "b" variable name
      ! NB: Reusing vardesc, no longer assocated with previous variables
      if (associated(hist_coords(mdimind)%formula_terms%b_values)) then
        if (size(hist_coords(mdimind)%formula_terms%b_values) /= hist_coords(mdimind)%dimsize) then
          write(errormsg, *) 'The forumla_terms variable, ',                  &
               trim(hist_coords(mdimind)%formula_terms%b_name),               &
               ', needs to have dimension', hist_coords(mdimind)%dimsize
          call endrun(errormsg)
        end if
        call cam_pio_def_var(File, trim(hist_coords(mdimind)%formula_terms%b_name), &
             pio_double, (/dimid/), vardesc, existOK=.false.)
        ierr = pio_put_att(File, vardesc, 'long_name', trim(hist_coords(mdimind)%formula_terms%b_long_name))
        call cam_pio_handle_error(ierr, 'Error writing "long_name" attr for "b" formula_term in write_hist_coord_attr')
      end if
      ! Maybe define the p0 variable (this may be defined already which is OK)
      ! NB: Reusing vardesc, no longer assocated with previous variables
      if (hist_coords(mdimind)%formula_terms%p0_value /= fillvalue) then
        ierr = pio_inq_varid(File, trim(hist_coords(mdimind)%formula_terms%p0_name), vardesc)
        if (ierr /= PIO_NOERR) then
          ierr = pio_def_var(File, trim(hist_coords(mdimind)%formula_terms%p0_name), &
               pio_double, vardesc)
          call cam_pio_handle_error(ierr, 'Unable to define "p0" formula_terms variable in write_hist_coord_attr')
          ierr = pio_put_att(File, vardesc, 'long_name', trim(hist_coords(mdimind)%formula_terms%p0_long_name))
          call cam_pio_handle_error(ierr, 'Error writing "long_name" attr for "p0" formula_term in write_hist_coord_attr')
          ierr = pio_put_att(File, vardesc, 'units', trim(hist_coords(mdimind)%formula_terms%p0_units))
          call cam_pio_handle_error(ierr, 'Error writing "units" attr for "p0" formula_term in write_hist_coord_attr')
        end if
      end if
      ! PS is not our responsibility
    end if ! (.not. dimonly)

  end subroutine write_hist_coord_attr
  
  !---------------------------------------------------------------------------
  !
  !  write_hist_coord_attrs
  !
  !  Write the dimension and coordinate attributes for the defined
  !  history
  !  coordinates.
  !
  !---------------------------------------------------------------------------

  subroutine write_hist_coord_attrs(File, boundsdim, mdimids, writemdims_in)
    use pio, only: file_desc_t, var_desc_t, pio_put_att,         &
                   pio_bcast_error, pio_internal_error, pio_seterrorhandling, &
                   pio_char
    use cam_pio_utils, only: cam_pio_def_dim, cam_pio_def_var

    ! Input variables
    type(file_desc_t), intent(inout) :: File           ! PIO file Handle
    integer,           intent(in)    :: boundsdim      ! Bounds dimension ID
    integer, optional, allocatable, intent(out)   :: mdimids(:) ! NetCDF dim IDs
    logical, optional, intent(in)    :: writemdims_in  ! Write mdim variable

    ! Local variables
    integer                          :: i
    integer                          :: ierr
    integer                          :: dimids(2)      ! PIO dimension IDs
    logical                          :: writemdims     ! Define an mdim variable
    type(var_desc_t)                 :: vardesc        ! PIO variable descriptor

    if (present(mdimids)) then
      allocate(mdimids(registeredmdims))
    end if

    ! We will handle errors for this routine
    call pio_seterrorhandling(File, PIO_BCAST_ERROR)

    if (present(writemdims_in)) then
      writemdims = writemdims_in
    else
      writemdims = .false.
    end if

    ! NB: Currently, writemdims is for restart and we don't need to write
    ! these out in a history-restart file. This could change in the future.
    ! which would require a change to the function of the fourth argument
    ! Fill in the attribute information for each mdim
    do i = 1, registeredmdims
      if (present(mdimids)) then
        call write_hist_coord_attr(File, i, boundsdim, writemdims, mdimids(i))
      else
        call write_hist_coord_attr(File, i, boundsdim, writemdims)
      end if
    end do

    if (writemdims) then
      call cam_pio_def_dim(File, 'mdimslen', max_hcoordname_len, dimids(1),   &
           existOK=.true.)
      call cam_pio_def_dim(File, 'num_mdims', registeredmdims, dimids(2),     &
           existOK=.true.)
      call cam_pio_def_var(File, mdim_var_name, pio_char, dimids, vardesc,    &
           existOK=.false.)
      ierr = pio_put_att(File, vardesc, 'long_name', 'mdim dimension names')
      call cam_pio_handle_error(ierr, 'Error writing "long_name" attr for mdimnames in write_hist_coord_attrs')
    end if

    ! Back to I/O or die trying
    call pio_seterrorhandling(File, PIO_INTERNAL_ERROR)

  end subroutine write_hist_coord_attrs

  !---------------------------------------------------------------------------

  subroutine write_hist_coord_var(File, mdimind)
    use pio, only: file_desc_t, var_desc_t, pio_put_var, pio_inq_varid

    ! Input variables
    type(file_desc_t), intent(inout) :: File           ! PIO file Handle
    integer,           intent(in)    :: mdimind        ! Internal dim index

    ! Local variables
    type(var_desc_t)                 :: vardesc        ! PIO variable descriptor
    integer                          :: ierr

    if ((hist_coords(mdimind)%integer_dim .and.                               &
         associated(hist_coords(mdimind)%integer_values)) .or.                &
         ((.not. hist_coords(mdimind)%integer_dim) .and.                      &
         associated(hist_coords(mdimind)%real_values))) then
      ! Check to make sure the variable already exists in the file
      ierr = pio_inq_varid(File, trim(hist_coords(mdimind)%name), vardesc)
      call cam_pio_handle_error(ierr, 'Error writing values for nonexistent dimension variable write_hist_coord_var')
      ! Write out the values for this dimension variable
      if (hist_coords(mdimind)%integer_dim) then
        ierr = pio_put_var(File, vardesc, hist_coords(mdimind)%integer_values)
      else
        ierr = pio_put_var(File, vardesc, hist_coords(mdimind)%real_values)
      end if
      call cam_pio_handle_error(ierr, 'Error writing variable values in write_hist_coord_var')
    end if

    ! Now, we need to possibly write values for the associated bounds variable
    if (associated(hist_coords(mdimind)%bounds)) then
      ! Check to make sure the variable already exists in the file
      ! NB: Reusing vardesc, no longer assocated with previous variables
      ierr = pio_inq_varid(File, trim(hist_coords(mdimind)%bounds_name), vardesc)
      call cam_pio_handle_error(ierr, 'Error writing values for nonexistent bounds variable write_hist_coord_var')
    ! Write out the values for this bounds variable
      ierr = pio_put_var(File, vardesc, hist_coords(mdimind)%bounds)
      call cam_pio_handle_error(ierr, 'Error writing bounds values in write_hist_coord_var')
    end if

    ! Write values for the "a" variable name
    if (associated(hist_coords(mdimind)%formula_terms%a_values)) then
      ! Check to make sure the variable already exists in the file
      ! NB: Reusing vardesc, no longer assocated with previous variables
      ierr = pio_inq_varid(File, trim(hist_coords(mdimind)%formula_terms%a_name), vardesc) 
      call cam_pio_handle_error(ierr, 'Error writing values for nonexistent "a" formula_terms variable write_hist_coord_var')
    ! Write out the values for this "a" formula_terms variable
      ierr = pio_put_var(File, vardesc, hist_coords(mdimind)%formula_terms%a_values)
      call cam_pio_handle_error(ierr, 'Error writing "a" formula_terms values in write_hist_coord_var')
    end if
    ! Write values for the "b" variable name
    if (associated(hist_coords(mdimind)%formula_terms%b_values)) then
      ! Check to make sure the variable already exists in the file
      ! NB: Reusing vardesc, no longer assocated with previous variables
      ierr = pio_inq_varid(File, trim(hist_coords(mdimind)%formula_terms%b_name), vardesc)
      call cam_pio_handle_error(ierr, 'Error writing values for nonexistent "b" formula_terms variable write_hist_coord_var')
    ! Write out the values for this "b" formula_terms variable
      ierr = pio_put_var(File, vardesc, hist_coords(mdimind)%formula_terms%b_values)
      call cam_pio_handle_error(ierr, 'Error writing "b" formula_terms values in write_hist_coord_var')
    end if
    ! Write values for the "p0" variable name (this may be an overwrite, too bad
    if (hist_coords(mdimind)%formula_terms%p0_value /= fillvalue) then
      ! Check to make sure the variable already exists in the file
      ! NB: Reusing vardesc, no longer assocated with previous variables
      ierr = pio_inq_varid(File, trim(hist_coords(mdimind)%formula_terms%p0_name), vardesc)
      call cam_pio_handle_error(ierr, 'Error writing values for nonexistent "p0" formula_terms variable write_hist_coord_var')
    ! Write out the values for this "p0" formula_terms variable
      ierr = pio_put_var(File, vardesc, hist_coords(mdimind)%formula_terms%p0_value)
      call cam_pio_handle_error(ierr, 'Error writing "p0" formula_terms values in write_hist_coord_var')
    end if

  end subroutine write_hist_coord_var

  !---------------------------------------------------------------------------

  subroutine write_hist_coord_vars(File, writemdims_in)
   use pio, only: file_desc_t, var_desc_t, pio_put_var,                       &
                  pio_bcast_error, pio_internal_error,                        &
                  pio_seterrorhandling, pio_inq_varid

    ! Input variables
    type(file_desc_t), intent(inout) :: File           ! PIO file Handle
    logical, optional, intent(in)    :: writemdims_in  ! Write mdim variable

    ! Local variables
    integer                          :: i
    integer                          :: ierr
    logical                          :: writemdims     ! Define an mdim variable
    type(var_desc_t)                 :: vardesc        ! PIO variable descriptor
    character(len=max_hcoordname_len), allocatable :: mdimnames(:)

    ! We will handle errors for this routine
    call pio_seterrorhandling(File, PIO_BCAST_ERROR)

    if (present(writemdims_in)) then
      writemdims = writemdims_in
    else
      writemdims = .false.
    end if

    if (writemdims) then
      allocate(mdimnames(registeredmdims))
    end if

    ! Write out the variable values for each mdim
    do i = 1, registeredmdims
      if (.not. writemdims) then
        ! NB: Currently, writemdims is for restart and we don't need to write
        ! these out in a history-restart file. This could change in the future
        ! which is why it is a separate if block
        ! Fill in the attribute information for each mdim
        call write_hist_coord_var(File, i)
      end if
      if (writemdims) then
        mdimnames(i) = trim(hist_coords(i)%name)
      end if
    end do

    if (writemdims) then
      ierr = pio_inq_varid(File, mdim_var_name, vardesc)
      call cam_pio_handle_error(ierr, 'Error writing values for nonexistent mdimnames variable in write_hist_coord_vars')
      ! Write out the values for mdim names
      ierr = pio_put_var(File, vardesc, mdimnames)
      call cam_pio_handle_error(ierr, 'Error writing values for mdimnames variable in write_hist_coord_vars')
      deallocate(mdimnames)
    end if

    ! Back to I/O or die trying
    call pio_seterrorhandling(File, PIO_INTERNAL_ERROR)

  end subroutine write_hist_coord_vars

  !---------------------------------------------------------------------------

  subroutine lookup_hist_coord_indices(mdimnames, mdimindicies)
    ! Dummy arguments
    character(len=*), intent(in) :: mdimnames(:)
    integer, intent(out) :: mdimindicies(:)

    ! Local variables
    integer :: i, j
    integer :: cnt
    character(len=120) :: errormsg
    character(len=16) :: name


    cnt = size(mdimnames)
    mdimindicies = -1


    do j=1,cnt
      name = mdimnames(j)
      do i = 1, registeredmdims
        if(name .eq. hist_coords(i)%name) then
          mdimindicies(j)=i
        end if
      end do
    end do
    do j = 1, cnt
      if(mdimindicies(j) < 0) then
        do i = 1, registeredmdims
          print *,__FILE__,__LINE__,i,hist_coords(i)%name
        end do
        write(errormsg,*) 'Name ',mdimnames(j),' is not a registered history coordinate'
        call endrun(errormsg)
      end if
    end do

  end subroutine lookup_hist_coord_indices

  !---------------------------------------------------------------------------
  ! Find the vertical dimension (if present) in dimnames and return its size
  !    (which is the number of levels). Return -1 if not found
  !    If dimnames is not present, search all of the registered history coords
  integer function hist_coord_find_levels(dimnames) result(levels)
    ! Dummy argument
    character(len=*), optional, intent(in) :: dimnames(:)

    ! Local variables
    integer i, index, dimcnt

    levels = -1  ! Error return value

    if (present(dimnames)) then
      dimcnt = size(dimnames)
    else
      dimcnt = registeredmdims
    end if

    do i = 1, dimcnt
      if (present(dimnames)) then
        index = get_hist_coord_index(trim(dimnames(i)))
        if (i < 0) then
          call endrun('hist_coord_find_levels: '//trim(dimnames(i))//' is not a registred history coordinate')
        end if
      else
        index = i  ! Just cycle through all the registered mdims
      end if

      if (hist_coords(index)%vertical_coord) then
        levels = hist_coords(index)%dimsize
        exit
      end if
    end do

  end function hist_coord_find_levels


end module cam_history_support
