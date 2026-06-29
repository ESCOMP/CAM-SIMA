module cam_history_support

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!
   !!  cam_history_support is used by cam_history as well as by the dycores
   !!    (for vertical coordinate support).
   !!
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use shr_kind_mod,     only: r8=>shr_kind_r8, shr_kind_cl, shr_kind_cxx
   use cam_grid_support, only: max_hcoordname_len

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
   ! can be checked by add_hist_coord
   character(len=10), parameter, public :: horiz_only        = 'horiz_only'
   real(r8),          parameter         :: error_tolerance   = 1.e-12_r8
   integer,           parameter         :: error_msglen      = 120
   integer,           parameter         :: error_msglen_long = 256

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
    integer,  pointer        :: integer_values(:) => null() ! dim values if integer
    real(r8), pointer        :: real_values(:) => null() ! dim values if real
    real(r8), pointer        :: bounds(:,:) => null() ! dim bounds
    type(formula_terms_t)    :: formula_terms     ! vars for formula terms
    logical                  :: integer_dim       ! .true. iff dim has integer values
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
  public     :: parse_multiplier     ! Parse a repeat count and a token from input

  interface add_hist_coord
    module procedure add_hist_coord_regonly
    module procedure add_hist_coord_int
    module procedure add_hist_coord_r8
  end interface

  interface check_hist_coord
    ! NB: This is a private interface
    ! check_hist_coord: returns 0 if <name> is not registered as an mdim
    !                   returns i if <name> is registered with compatible values
    !              calls endrun if <name> is registered with incompatible values
    ! Versions without the <name> argument return .true. or .false.
    module procedure check_hist_coord_char
    module procedure check_hist_coord_int
    module procedure check_hist_coord_int_1d
    module procedure check_hist_coord_r8
    module procedure check_hist_coord_r8_1d
    module procedure check_hist_coord_r8_2d
    module procedure check_hist_coord_ft
    module procedure check_hist_coord_all
  end interface

  !!---------------------------------------------------------------------------
  
  CONTAINS

  pure integer function get_hist_coord_index(mdimname)
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
  pure logical function check_hist_coord_char(defined, input)

    ! Input variables
    character(len=*), intent(in)            :: defined
    character(len=*), intent(in)            :: input

    if (len_trim(defined) == 0) then
      ! In this case, we assume the current value is undefined so any input OK
      check_hist_coord_char = .true.
    else
      ! We have to match definitions
      check_hist_coord_char = (trim(input) == trim(defined))
    end if
  end function check_hist_coord_char

  pure logical function check_hist_coord_int(defined, input)

    ! Input variables
    integer, intent(in)            :: defined
    integer, intent(in)            :: input

    if (defined == 0) then
      ! In this case, we assume the current value is undefined so any input OK
      check_hist_coord_int = .true.
    else
      ! We have to match definitions
      check_hist_coord_int = (input == defined)
    end if
  end function check_hist_coord_int

  pure logical function check_hist_coord_int_1d(defined, input)

    ! Input variables
    integer,             pointer            :: defined(:)
    integer,             pointer            :: input(:)

    ! Local variables
    integer                                 :: i

    if (.not. associated(defined)) then
      ! In this case, we assume the current value is undefined so any input OK
      check_hist_coord_int_1d = .true.
    else
      ! We have to match definitions
      check_hist_coord_int_1d = (size(input) == size(defined))
    end if
    if (check_hist_coord_int_1d .and. associated(defined)) then
      ! Need to check the values
      do i = 1, size(defined)
        if (defined(i) /= input(i)) then
          check_hist_coord_int_1d = .false.
          exit
        end if
      end do
    end if
  end function check_hist_coord_int_1d

  pure logical function check_hist_coord_r8(defined, input)

    ! Input variables
    real(r8), intent(in)            :: defined
    real(r8), intent(in)            :: input

    if (defined == fillvalue) then
      ! In this case, we assume the current value is undefined so any input OK
      check_hist_coord_r8 = .true.
    else
      ! We have to match definitions (within a tolerance)
      check_hist_coord_r8 = (abs(input - defined) <= error_tolerance)
    end if
  end function check_hist_coord_r8

  pure logical function check_hist_coord_r8_1d(defined, input)

    ! Input variables
    real(r8),             pointer            :: defined(:)
    real(r8),             pointer            :: input(:)

    ! Local variables
    integer                                  :: i

    if (.not. associated(defined)) then
      ! In this case, we assume the current value is undefined so any input OK
      check_hist_coord_r8_1d = .true.
    else
      ! We have to match definitions
      check_hist_coord_r8_1d = (size(input) == size(defined))
    end if
    if (check_hist_coord_r8_1d .and. associated(defined)) then
      ! Need to check the values (within a tolerance)
      do i = 1, size(defined)
        if (abs(defined(i) - input(i)) > error_tolerance) then
          check_hist_coord_r8_1d = .false.
          exit
        end if
      end do
    end if
  end function check_hist_coord_r8_1d

  pure logical function check_hist_coord_r8_2d(defined, input)

    ! Input variables
    real(r8),             pointer            :: defined(:,:)
    real(r8),             pointer            :: input(:,:)

    ! Local variables
    integer                                  :: i, j

    if (.not. associated(defined)) then
      ! In this case, we assume the current value is undefined so any input OK
      check_hist_coord_r8_2d = .true.
    else
      ! We have to match definitions
      check_hist_coord_r8_2d = ((size(input, 1) == size(defined, 1)) .and.    &
                                (size(input, 2) == size(defined, 2)))
    end if
    if (check_hist_coord_r8_2d .and. associated(defined)) then
      ! Need to check the values (within a tolerance)
      do j = 1, size(defined, 2)
        do i = 1, size(defined, 1)
          if (abs(defined(i, j) - input(i, j)) > error_tolerance) then
            check_hist_coord_r8_2d = .false.
            exit
          end if
        end do
      end do
    end if
  end function check_hist_coord_r8_2d

  logical function check_hist_coord_ft(defined, input)

    ! Input variables
    type(formula_terms_t), intent(in)           :: defined
    type(formula_terms_t), intent(in)           :: input

    ! We will assume that if formula_terms has been defined, a_name has a value
    if (len_trim(defined%a_name) == 0) then
      ! In this case, we assume the current value is undefined so any input OK
      check_hist_coord_ft = .true.
    else
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
    end if
  end function check_hist_coord_ft

  ! check_hist_coord: returns 0 if <name> is not registered as a hist coord
  !                   returns i if <name> is registered with compatible values
  !                   calls endrun if <name> is registered with incompatible
  !                   values
  integer function check_hist_coord_all(name, vlen, long_name, units, bounds, &
       i_values, r_values, bounds_name, positive, standard_name, formula_terms)
    use cam_abortutils,   only: endrun
    use string_utils,     only: stringify

    ! Input variables
    character(len=*),      intent(in)            :: name
    integer,               intent(in)            :: vlen
    character(len=*),      intent(in)            :: long_name
    character(len=*),      intent(in)            :: units
    character(len=*),      intent(in)            :: bounds_name
    integer,   pointer,    intent(in)            :: i_values(:)
    real(r8),  pointer,    intent(in)            :: r_values(:)
    real(r8),  pointer,    intent(in)            :: bounds(:,:)
    character(len=*),      intent(in)            :: positive
    character(len=*),      intent(in)            :: standard_name
    type(formula_terms_t), intent(in)            :: formula_terms

    ! Local variables
    character(len=256)                           :: errormsg
    integer                                      :: i

    i = get_hist_coord_index(trim(name))
    ! If i > 0, this mdim has already been registered
    if (i > 0) then
      check_hist_coord_all = i
      if (.not. check_hist_coord(hist_coords(i)%dimsize, vlen)) then
        write(errormsg, *) 'ERROR: Attempt to register dimension, '//trim(name)//', with incompatible size ( ', &
           stringify((/hist_coords(i)%dimsize/)), ' vs vlen= '//stringify((/vlen/))//' )'
        call endrun(errormsg, file=__FILE__, line=__LINE__)
      end if
      if (.not. check_hist_coord(hist_coords(i)%long_name, long_name)) then
        write(errormsg, *) 'ERROR: Attempt to register dimension, ',trim(name),', with a different long_name ( "', &
                trim(hist_coords(i)%long_name)//'" vs long_name= "'//trim(long_name)//'" )'
        call endrun(errormsg, file=__FILE__, line=__LINE__)
      end if
      if (.not. check_hist_coord(hist_coords(i)%units, units)) then
        write(errormsg, *) 'ERROR: Attempt to register dimension, ',trim(name),', with different units ( "',     &
                trim(hist_coords(i)%units)//'" vs units= "'//trim(units)//'" )'
        call endrun(errormsg, file=__FILE__, line=__LINE__)
      end if
      if (.not. check_hist_coord(hist_coords(i)%bounds_name, bounds_name)) then
        write(errormsg, *) 'ERROR: Attempt to register dimension, ',trim(name),', with a different bounds_name ( "', &
                trim(hist_coords(i)%bounds_name)//'" vs bounds_name= "'//trim(bounds_name)//'" )'
        call endrun(errormsg, file=__FILE__, line=__LINE__)
      end if
      if (.not. check_hist_coord(hist_coords(i)%standard_name, standard_name)) then
        write(errormsg, *) 'ERROR: Attempt to register dimension, ',trim(name),', with a different standard_name ( "', &
                trim(hist_coords(i)%standard_name)//'" vs standard_name= "'//trim(standard_name)//'" )'
        call endrun(errormsg, file=__FILE__, line=__LINE__)
      end if
      if (.not. check_hist_coord(hist_coords(i)%positive, positive)) then
        write(errormsg, *) 'ERROR: Attempt to register dimension, ',trim(name),', with a different value of positive ( "', &
                trim(hist_coords(i)%positive)//'" vs positive= "'//trim(positive)//'" )'
        call endrun(errormsg, file=__FILE__, line=__LINE__)
      end if
      ! Since the integer_dim defaults to .true., double check which to check
      if ((.not. hist_coords(i)%integer_dim) .or.                             &
           associated(hist_coords(i)%real_values)) then
        if (.not. check_hist_coord(hist_coords(i)%real_values, r_values)) then
          write(errormsg, *) 'ERROR: Attempt to register dimension, ',trim(name),', with different values (( ', &
                 stringify(hist_coords(i)%real_values),') vs r_values=( '//stringify(r_values)//' ))'
          call endrun(errormsg, file=__FILE__, line=__LINE__)
        else if (associated(i_values)) then
          write(errormsg, *) 'ERROR: Attempt to register integer values for real dimension ',trim(name), ' ( ', &
                  'i_values=(', stringify(i_values), '))'
          call endrun(errormsg, file=__FILE__, line=__LINE__)
        end if
      else
        if (.not. check_hist_coord(hist_coords(i)%integer_values, i_values)) then
          write(errormsg, *) 'ERROR: Attempt to register dimension, ',trim(name),', with different values, (( ', &
                  stringify(hist_coords(i)%integer_values)//') vs i_values= ('//stringify(i_values)//') )'
          call endrun(errormsg, file=__FILE__, line=__LINE__)
        else if (associated(r_values)) then
          write(errormsg, *) 'ERROR: Attempt to register real values for integer dimension ', trim(name), ' ( ', &
                   'r_values=(', stringify(r_values), ') )'
          call endrun(errormsg, file=__FILE__, line=__LINE__)
        end if
      end if
      if (.not. check_hist_coord(hist_coords(i)%bounds, bounds)) then
        write(errormsg, *) 'ERROR: Attempt to register dimension, ',trim(name),', with different bounds'
        call endrun(errormsg, file=__FILE__, line=__LINE__)
      end if
      if (.not. check_hist_coord(hist_coords(i)%formula_terms, formula_terms)) then
        write(errormsg, *) 'ERROR: Attempt to register dimension, ',trim(name),', with different formula_terms'
        call endrun(errormsg, file=__FILE__, line=__LINE__)
      end if
    else
      check_hist_coord_all = 0
    end if
  end function check_hist_coord_all

  subroutine add_hist_coord_regonly(name, index)
    use cam_abortutils,   only: endrun

    ! Input variable
    character(len=*),  intent(in)    :: name
    integer, optional, intent(out)   :: index

    ! Local variables
    character(len=error_msglen)      :: errormsg
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
      hist_coords(registeredmdims)%dimname = ''
      hist_coords(registeredmdims)%vertical_coord = .false.
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
    use cam_logfile,      only: iulog
    use spmd_utils,       only: masterproc

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
    character(len=max_chars)                        :: local_units
    character(len=max_chars)                        :: local_positive
    character(len=max_chars)                        :: local_standard_name
    character(len=max_chars)                        :: local_bounds_name
    character(len=max_hcoordname_len)               :: local_dimname
    integer,        pointer                         :: local_int_values(:)
    real(r8),       pointer                         :: local_real_values(:)
    real(r8),       pointer                         :: local_bounds(:,:)
    type(formula_terms_t)                           :: local_formula_terms

    nullify(local_int_values)
    nullify(local_real_values)
    nullify(local_bounds)
    local_bounds_name = ''

    if (present(units)) then
       local_units = units
    else
       local_units = ''
    end if

    if (present(positive)) then
       local_positive = positive
    else
       local_positive = ''
    end if

    if (present(standard_name)) then
       local_standard_name = standard_name
    else
       local_standard_name = ''
    end if

    if (present(dimname)) then
       local_dimname = dimname
    else
       local_dimname = ''
    end if

    if (present(values)) then
       local_int_values => values
    end if

    ! First, check to see if it is OK to add this coord
    i = check_hist_coord(name, vlen, long_name, local_units, local_bounds, &
            local_int_values, local_real_values, local_bounds_name, local_positive,  &
            local_standard_name, local_formula_terms)

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
          write(iulog,*) 'WARNING: long_name for ',trim(name),' too long', &
             ' and will be truncated on history files'
       end if
    end if
    hist_coords(i)%long_name = trim(long_name)
    hist_coords(i)%units = trim(local_units)
    hist_coords(i)%integer_dim = .true.
    hist_coords(i)%integer_values => local_int_values
    hist_coords(i)%positive = trim(local_positive)
    hist_coords(i)%standard_name = trim(local_standard_name)
    hist_coords(i)%vertical_coord = .false.
    hist_coords(i)%dimname = trim(local_dimname)

  end subroutine add_hist_coord_int

  subroutine add_hist_coord_r8(name, vlen, long_name, units, values,         &
       bounds_name, bounds, positive, standard_name, vertical_coord, dimname)
    use cam_abortutils,   only: endrun
    use cam_logfile,      only: iulog
    use spmd_utils,       only: masterproc

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
    character(len=error_msglen)                     :: errormsg
    integer                                         :: i
    character(len=max_chars)                        :: local_positive
    character(len=max_chars)                        :: local_standard_name
    character(len=max_chars)                        :: local_bounds_name
    character(len=max_hcoordname_len)               :: local_dimname
    integer,        pointer                         :: local_int_values(:)
    real(r8),       pointer                         :: local_bounds(:,:)
    type(formula_terms_t)                           :: local_formula_terms
    real(r8),       pointer                         :: r_ptr(:)

    nullify(local_int_values)
    nullify(local_bounds)

    if (present(positive)) then
       local_positive = positive
    else
       local_positive = ''
    end if

    if (present(standard_name)) then
       local_standard_name = standard_name
    else
       local_standard_name = ''
    end if

    if (present(dimname)) then
       local_dimname = dimname
    else
       local_dimname = ''
    end if

    if (present(bounds)) then
       local_bounds => bounds
       if (.not. present(bounds_name)) then
          write(errormsg,*) 'bounds_name must be present for bounds values'
          call endrun(errormsg)
       end if
    end if

    if (present(bounds_name)) then
       if (.not. present(bounds)) then
          write(errormsg,*) 'bounds must be present for ',trim(bounds_name)
          call endrun(errormsg)
       end if
       local_bounds_name = bounds_name
    else
       local_bounds_name = ''
    end if

    ! First, check to see if it is OK to add this coord
    r_ptr => values
    i = check_hist_coord(name, vlen, long_name, units, local_bounds,     &
            local_int_values, r_ptr, local_bounds_name, local_positive,           &
            local_standard_name, local_formula_terms)

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
          write(iulog,*) 'WARNING: long_name for ',trim(name),' too long', &
             ' and will be truncated on history files'
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
    hist_coords(i)%positive = trim(local_positive)
    hist_coords(i)%standard_name = trim(local_standard_name)
    hist_coords(i)%bounds_name = trim(local_bounds_name)
    hist_coords(i)%bounds => local_bounds
    if (present(vertical_coord)) then
       hist_coords(i)%vertical_coord = vertical_coord
    end if
    hist_coords(i)%dimname = trim(local_dimname)

  end subroutine add_hist_coord_r8

  subroutine add_vert_coord(name, vlen, long_name, units, values,            &
       positive, standard_name, formula_terms)
    use cam_logfile,      only: iulog
    use spmd_utils,       only: masterproc

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
    character(len=max_chars)                             :: local_units
    character(len=max_chars)                             :: local_positive
    character(len=max_chars)                             :: local_standard_name
    character(len=max_chars)                             :: local_bounds_name
    character(len=max_hcoordname_len)                    :: local_dimname
    integer,        pointer                              :: local_int_values(:)
    real(r8),       pointer                              :: local_real_values(:)
    real(r8),       pointer                              :: local_bounds(:,:)
    type(formula_terms_t)                                :: local_formula_terms

    nullify(local_int_values)
    nullify(local_real_values)
    nullify(local_bounds)
    local_bounds_name = ''
    local_dimname = ''

    if (present(positive)) then
       local_positive = positive
    else
       local_positive = ''
    end if

    if (present(standard_name)) then
       local_standard_name = standard_name
    else
       local_standard_name = ''
    end if

    if (present(formula_terms)) then
       local_formula_terms = formula_terms
    end if

    ! First, check to see if it is OK to add this coord
    i = check_hist_coord(name, vlen, long_name, local_units, local_bounds, &
            local_int_values, local_real_values, local_bounds_name, local_positive,  &
            local_standard_name, local_formula_terms)

    ! Register the name and hist_coord values if necessary
    if (i == 0) then
      call add_hist_coord(trim(name), vlen, long_name, units, values,         &
           positive=positive, standard_name=standard_name,                    &
           vertical_coord=.true.)
      i = get_hist_coord_index(trim(name))
    end if

    hist_coords(i)%formula_terms = local_formula_terms

  end subroutine add_vert_coord

  subroutine write_hist_coord_attr(File, mdimind, boundsdim, dimonly, mdimid)
    use pio, only: file_desc_t, var_desc_t, pio_put_att, pio_noerr,           &
                   pio_int, pio_double, pio_inq_varid, pio_def_var
    use cam_pio_utils, only: cam_pio_def_dim, cam_pio_def_var
    use cam_abortutils,   only: endrun
    use cam_pio_utils,    only: cam_pio_handle_error

    ! Input variables
    type(file_desc_t), intent(inout) :: File           ! PIO file Handle
    integer,           intent(in)    :: mdimind        ! Internal dim index
    integer,           intent(in)    :: boundsdim      ! Bounds dimension ID
    logical,           intent(in)    :: dimonly        ! No def_var if .true.
    integer, optional, intent(out)   :: mdimid

    ! Local variables
    integer                          :: dimid          ! PIO dimension ID
    type(var_desc_t)                 :: vardesc        ! PIO variable descriptor
    character(len=error_msglen_long) :: errormsg
    character(len=max_chars)         :: formula_terms  ! Constructed string
    integer                          :: ierr
    integer                          :: dtype
    logical                          :: defvar         ! True if var exists
    character(len=*), parameter      :: subname = 'write_hist_coord_attr'

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
        if(len_trim(hist_coords(mdimind)%long_name) > 0) then
           ierr=pio_put_att(File, vardesc, 'long_name',                       &
                   trim(hist_coords(mdimind)%long_name))
           write(errormsg,*) subname, ': Error writing "long_name" attr for variable "', &
                   trim(hist_coords(mdimind)%name), '" (long_name="', &
                   trim(hist_coords(mdimind)%long_name), '")'
           call cam_pio_handle_error(ierr, errormsg)
        end if
        ! units
        if(len_trim(hist_coords(mdimind)%units) > 0) then
          ierr=pio_put_att(File, vardesc, 'units', &
               trim(hist_coords(mdimind)%units))
          write(errormsg,*) subname, ': Error writing "units" attr for variable "', &
                  trim(hist_coords(mdimind)%name), '" (units="', &
                  trim(hist_coords(mdimind)%units), '")'
          call cam_pio_handle_error(ierr, errormsg)
        end if
        ! positive
        if(len_trim(hist_coords(mdimind)%positive) > 0) then
          ierr=pio_put_att(File, vardesc, 'positive', &
               trim(hist_coords(mdimind)%positive))
          write(errormsg,*) subname, ': Error writing "positive" attr for variable "', &
                  trim(hist_coords(mdimind)%name), '" (positive="', &
                  trim(hist_coords(mdimind)%positive), '")'
          call cam_pio_handle_error(ierr, errormsg)
        end if
        ! standard_name
        if(len_trim(hist_coords(mdimind)%standard_name) > 0) then
          ierr=pio_put_att(File, vardesc, 'standard_name', &
               trim(hist_coords(mdimind)%standard_name))
          write(errormsg,*) subname, ': Error writing "standard_name" attr for variable "', &
                  trim(hist_coords(mdimind)%name), '" (standard_name="', &
                  trim(hist_coords(mdimind)%standard_name), '")'
          call cam_pio_handle_error(ierr, errormsg)
        end if
        ! formula_terms
        if(len_trim(hist_coords(mdimind)%formula_terms%a_name) > 0) then
          write(formula_terms, '("a: ",a," b: ",a," p0: ",a," ps: ",a)') &
               trim(hist_coords(mdimind)%formula_terms%a_name), &
               trim(hist_coords(mdimind)%formula_terms%b_name), &
               trim(hist_coords(mdimind)%formula_terms%p0_name),&
               trim(hist_coords(mdimind)%formula_terms%ps_name)
          ierr=pio_put_att(File, vardesc, 'formula_terms', trim(formula_terms))
          write(errormsg,*) subname, ': Error writing "formula_terms" attr for variable "', &
                  trim(hist_coords(mdimind)%name), '" (formula_terms="', &
                  trim(formula_terms), '")'
          call cam_pio_handle_error(ierr, errormsg)
        end if
        ! bounds
        if (associated(hist_coords(mdimind)%bounds)) then
          ! Write name of the bounds variable
          ierr=pio_put_att(File, vardesc, 'bounds', trim(hist_coords(mdimind)%bounds_name))
          write(errormsg,*) subname, ': Error writing "bounds" attr for variable "', &
                  trim(hist_coords(mdimind)%name), '" (bounds_name="', &
                  trim(hist_coords(mdimind)%bounds_name), '")'
          call cam_pio_handle_error(ierr, errormsg)
        end if
      end if

      ! Now, we need to define and populate the associated bounds variable
      ! NB: Reusing vardesc, no longer assocated with main variable
      if (associated(hist_coords(mdimind)%bounds)) then
        if (size(hist_coords(mdimind)%bounds,2) /= hist_coords(mdimind)%dimsize) then
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
        write(errormsg,*) subname, ': Error writing "long_name" attr for "a" formula_term for variable "', &
                trim(hist_coords(mdimind)%name), '" (a_long_name="', &
                trim(hist_coords(mdimind)%formula_terms%a_long_name), '")'
        call cam_pio_handle_error(ierr, errormsg)
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
        write(errormsg,*) subname, ': Error writing "long_name" attr for "b" formula_term for variable "', &
                trim(hist_coords(mdimind)%name), '" (b_long_name="', &
                trim(hist_coords(mdimind)%formula_terms%b_long_name), '")'
        call cam_pio_handle_error(ierr, errormsg)
      end if
      ! Maybe define the p0 variable (this may be defined already which is OK)
      ! NB: Reusing vardesc, no longer assocated with previous variables
      if (hist_coords(mdimind)%formula_terms%p0_value /= fillvalue) then
        ierr = pio_inq_varid(File, trim(hist_coords(mdimind)%formula_terms%p0_name), vardesc)
        if (ierr /= PIO_NOERR) then
          ierr = pio_def_var(File, trim(hist_coords(mdimind)%formula_terms%p0_name), &
               pio_double, vardesc)
          write(errormsg,*) subname, ': Unable to define "p0" formula_terms variable for "', &
                  trim(hist_coords(mdimind)%name), '" (p0_name="', &
                  trim(hist_coords(mdimind)%formula_terms%p0_name), '")'
          call cam_pio_handle_error(ierr, errormsg)
          ierr = pio_put_att(File, vardesc, 'long_name', trim(hist_coords(mdimind)%formula_terms%p0_long_name))
          write(errormsg,*) subname, ': Error writing "long_name" attr for "p0" formula_term for "', &
                  trim(hist_coords(mdimind)%name), '" (p0_long_name="', &
                  trim(hist_coords(mdimind)%formula_terms%p0_long_name), '")'
          call cam_pio_handle_error(ierr, errormsg)
          ierr = pio_put_att(File, vardesc, 'units', trim(hist_coords(mdimind)%formula_terms%p0_units))
          write(errormsg,*) subname, ': Error writing "units" attr for "p0" formula_term for "', &
                  trim(hist_coords(mdimind)%name), '" (p0_units="', &
                  trim(hist_coords(mdimind)%formula_terms%p0_units), '")'
          call cam_pio_handle_error(ierr, errormsg)
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
    use cam_pio_utils, only: cam_pio_handle_error
    use cam_abortutils,only: check_allocate

    ! Input variables
    type(file_desc_t), intent(inout) :: File           ! PIO file Handle
    integer,           intent(in)    :: boundsdim      ! Bounds dimension ID
    logical, optional, intent(in)    :: writemdims_in  ! Write mdim variable
    integer, optional, allocatable, intent(out)   :: mdimids(:) ! NetCDF dim IDs

    ! Local variables
    integer                          :: i
    integer                          :: ierr
    integer                          :: dimids(2)      ! PIO dimension IDs
    logical                          :: writemdims     ! Define an mdim variable
    type(var_desc_t)                 :: vardesc        ! PIO variable descriptor
    character(len=error_msglen)      :: errormsg
    character(len=*), parameter      :: subname = 'write_hist_coord_attrs'

    if (present(mdimids)) then
      allocate(mdimids(registeredmdims), stat=ierr)
      call check_allocate(ierr, subname, 'mdimids', file=__FILE__, line=__LINE__-1)
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
      write(errormsg, *) subname, ': Error writing "long_name" attr for mdimnames"'
      call cam_pio_handle_error(ierr, errormsg)
    end if

    ! Back to I/O or die trying
    call pio_seterrorhandling(File, PIO_INTERNAL_ERROR)

  end subroutine write_hist_coord_attrs

  !---------------------------------------------------------------------------

  subroutine write_hist_coord_var(File, mdimind)
    use pio, only: file_desc_t, var_desc_t, pio_put_var, pio_inq_varid
    use cam_pio_utils, only: cam_pio_handle_error
    use string_utils,  only: stringify

    ! Input variables
    type(file_desc_t), intent(inout) :: File           ! PIO file Handle
    integer,           intent(in)    :: mdimind        ! Internal dim index

    ! Local variables
    type(var_desc_t)                 :: vardesc        ! PIO variable descriptor
    integer                          :: ierr
    character(len=error_msglen_long) :: errormsg
    character(len=*), parameter      :: subname = 'write_hist_coord_var'

    if ((hist_coords(mdimind)%integer_dim .and.                               &
         associated(hist_coords(mdimind)%integer_values)) .or.                &
         ((.not. hist_coords(mdimind)%integer_dim) .and.                      &
         associated(hist_coords(mdimind)%real_values))) then
      ! Check to make sure the variable already exists in the file
      ierr = pio_inq_varid(File, trim(hist_coords(mdimind)%name), vardesc)
      write(errormsg,*) subname, ': Error writing values for nonexistent dimension variable "', &
              trim(hist_coords(mdimind)%name), '"'
      call cam_pio_handle_error(ierr, errormsg)
      ! Write out the values for this dimension variable
      if (hist_coords(mdimind)%integer_dim) then
        ierr = pio_put_var(File, vardesc, hist_coords(mdimind)%integer_values)
      else
        ierr = pio_put_var(File, vardesc, hist_coords(mdimind)%real_values)
      end if
      write(errormsg,*) subname, ': Error writing variable values for "',     &
              trim(hist_coords(mdimind)%name), '"'
      call cam_pio_handle_error(ierr, errormsg)
    end if

    ! Now, we need to possibly write values for the associated bounds variable
    if (associated(hist_coords(mdimind)%bounds)) then
      ! Check to make sure the variable already exists in the file
      ! NB: Reusing vardesc, no longer assocated with previous variables
      ierr = pio_inq_varid(File, trim(hist_coords(mdimind)%bounds_name), vardesc)
      write(errormsg,*) subname, ': Error writing values for nonexistent bounds for variable "', &
              trim(hist_coords(mdimind)%name), '"'
      call cam_pio_handle_error(ierr, errormsg)
    ! Write out the values for this bounds variable
      ierr = pio_put_var(File, vardesc, hist_coords(mdimind)%bounds)
      write(errormsg,*) subname, ': Error writing bounds values for "',       &
              trim(hist_coords(mdimind)%name), '"'
      call cam_pio_handle_error(ierr, errormsg)
    end if

    ! Write values for the "a" variable name
    if (associated(hist_coords(mdimind)%formula_terms%a_values)) then
      ! Check to make sure the variable already exists in the file
      ! NB: Reusing vardesc, no longer assocated with previous variables
      ierr = pio_inq_varid(File, trim(hist_coords(mdimind)%formula_terms%a_name), vardesc) 
      write(errormsg,*) subname, ': Error writing values for nonexistent "a" formula_terms for variable "', &
              trim(hist_coords(mdimind)%name), '" (formula_terms%a_name="',    &
              trim(hist_coords(mdimind)%formula_terms%a_name), '")'
      call cam_pio_handle_error(ierr, errormsg)
    ! Write out the values for this "a" formula_terms variable
      ierr = pio_put_var(File, vardesc, hist_coords(mdimind)%formula_terms%a_values)
      write(errormsg,*) subname, ': Error writing "a" formula_terms values for variable "', &
              trim(hist_coords(mdimind)%name), '" (formula_terms%a_values="',    &
              stringify(hist_coords(mdimind)%formula_terms%a_values), '")'
      call cam_pio_handle_error(ierr, errormsg)
    end if
    ! Write values for the "b" variable name
    if (associated(hist_coords(mdimind)%formula_terms%b_values)) then
      ! Check to make sure the variable already exists in the file
      ! NB: Reusing vardesc, no longer assocated with previous variables
      ierr = pio_inq_varid(File, trim(hist_coords(mdimind)%formula_terms%b_name), vardesc)
      write(errormsg,*) subname, ': Error writing values for nonexistent "b" formula_terms for variable "', &
              trim(hist_coords(mdimind)%name), '" (formula_terms%b_name="',   &
              trim(hist_coords(mdimind)%formula_terms%b_name), '")'
      call cam_pio_handle_error(ierr, errormsg)
    ! Write out the values for this "b" formula_terms variable
      ierr = pio_put_var(File, vardesc, hist_coords(mdimind)%formula_terms%b_values)
      write(errormsg,*) subname, ': Error writing "b" formula_terms values for variable "', &
              trim(hist_coords(mdimind)%name), '" (formula_terms%b_values="',    &
              stringify(hist_coords(mdimind)%formula_terms%b_values), '")'
      call cam_pio_handle_error(ierr, errormsg)
    end if
    ! Write values for the "p0" variable name (this may be an overwrite, too bad)
    if (hist_coords(mdimind)%formula_terms%p0_value /= fillvalue) then
      ! Check to make sure the variable already exists in the file
      ! NB: Reusing vardesc, no longer assocated with previous variables
      ierr = pio_inq_varid(File, trim(hist_coords(mdimind)%formula_terms%p0_name), vardesc)
      write(errormsg,*) subname, ': Error writing values for nonexistent "p0" formula_terms for variable "', &
              trim(hist_coords(mdimind)%name), '" (formula_terms%p0_name="',   &
              trim(hist_coords(mdimind)%formula_terms%p0_name), '")'
      call cam_pio_handle_error(ierr, errormsg)
    ! Write out the values for this "p0" formula_terms variable
      ierr = pio_put_var(File, vardesc, hist_coords(mdimind)%formula_terms%p0_value)
      write(errormsg,*) subname, ': Error writing "p0" formula_terms value for variable "',   &
              trim(hist_coords(mdimind)%name), '" (formula_terms%p0_value="',  &
              stringify((/hist_coords(mdimind)%formula_terms%p0_value/)), '")'
      call cam_pio_handle_error(ierr, errormsg)
    end if

  end subroutine write_hist_coord_var

  !---------------------------------------------------------------------------

  subroutine write_hist_coord_vars(File, writemdims_in)
   use pio, only: file_desc_t, var_desc_t, pio_put_var,                       &
                  pio_bcast_error, pio_internal_error,                        &
                  pio_seterrorhandling, pio_inq_varid
    use cam_pio_utils, only: cam_pio_handle_error
    use cam_abortutils,only: check_allocate

    ! Input variables
    type(file_desc_t), intent(inout) :: File           ! PIO file Handle
    logical, optional, intent(in)    :: writemdims_in  ! Write mdim variable

    ! Local variables
    integer                          :: i
    integer                          :: ierr
    logical                          :: writemdims     ! Define an mdim variable
    type(var_desc_t)                 :: vardesc        ! PIO variable descriptor
    character(len=max_hcoordname_len), allocatable :: mdimnames(:)
    character(len=*),    parameter                 :: subname = 'write_hist_coord_vars'

    ! We will handle errors for this routine
    call pio_seterrorhandling(File, PIO_BCAST_ERROR)

    if (present(writemdims_in)) then
      writemdims = writemdims_in
    else
      writemdims = .false.
    end if

    if (writemdims) then
      allocate(mdimnames(registeredmdims), stat=ierr)
      call check_allocate(ierr, subname, 'mdimnames', file=__FILE__, line=__LINE__-1)
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
    use cam_abortutils,   only: endrun
    use cam_logfile,      only: iulog
    ! Dummy arguments
    character(len=*), intent(in) :: mdimnames(:)
    integer, intent(out) :: mdimindicies(:)

    ! Local variables
    integer :: i, j
    integer :: cnt
    character(len=error_msglen) :: errormsg


    cnt = size(mdimnames)
    mdimindicies = -1


    do j=1,cnt
      do i = 1, registeredmdims
        if(mdimnames(j) == hist_coords(i)%name) then
          mdimindicies(j)=i
        end if
      end do
    end do
    do j = 1, cnt
      if(mdimindicies(j) < 0) then
        write(iulog,*) 'history coordinate indices and names:'
        do i = 1, registeredmdims
          write(iulog,*) i,hist_coords(i)%name
        end do
        write(errormsg,*) 'Name ',mdimnames(j),' is not a registered history coordinate'
        call endrun(errormsg, file=__FILE__, line=__LINE__)
      end if
    end do

  end subroutine lookup_hist_coord_indices

  !---------------------------------------------------------------------------
  ! Find the vertical dimension (if present) in dimnames and return its size
  !    (which is the number of levels). Return -1 if not found
  !    If dimnames is not present, search all of the registered history coords
  integer function hist_coord_find_levels(dimnames) result(levels)
    use cam_abortutils,   only: endrun
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
          call endrun('hist_coord_find_levels: '//trim(dimnames(i))//' is not a registered history coordinate')
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

  subroutine parse_multiplier(input, multiplier, token, allowed_set, errmsg)
     use shr_string_mod,   only: to_lower => shr_string_toLower
     ! Parse a character string (<input>) to find a token <token>, possibly
     ! multiplied by an integer (<multiplier>).
     ! Return values for <multiplier>:
     !   positive integer: Successful return with <multiplier> and <token>.
     !   zero:             <input> is an empty string
     !   -1:               Error condition (malformed input string)
     ! Return values for <token>
     !   On a successful return, <token> will contain <input> with the
     !      optional multiplier and multiplication symbol removed.
     !   On an error return, <token> will be an empty string
     !
     ! If <allowed_set> is present, then <token> must equal a value in
     !   <allowed_set> (case insensitive)
     ! If <errmsg> is present, it is filled with an error message if <input>
     !   is not an allowed format.
     ! Allowed formats are:
     !   <multiplier>*<token> where <multiplier> is the string representation
     !      a positive integer.
     !   <token> in which case <multiplier> is assumed to be one.
     !

     ! Dummy arguments
     character(len=*),           intent(in)  :: input
     integer,                    intent(out) :: multiplier
     character(len=*),           intent(out) :: token
     character(len=*), optional, intent(in)  :: allowed_set(:)
     character(len=*), optional, intent(out) :: errmsg
     ! Local variables
     integer                     :: mult_ind ! Index of multiplication symbol
     integer                     :: lind     ! Loop index
     integer                     :: alen     ! Number of entries in <allowed_set>
     integer                     :: stat     ! Read status
     character(len=error_msglen) :: ioerrmsg ! Read error message
     logical                     :: match    ! For matching <allowed_set>
     character(len=8)            :: fmt_str  ! Format string

     ! Initialize output
     errmsg = ''
     multiplier = -1
     token = ''
     ! Do we have a multipler?
     mult_ind = index(input, '*')
     if (len_trim(input) == 0) then
        multiplier = 0
     else if (mult_ind <= 0) then
        multiplier = 1
        token = trim(input)
     else
        write(fmt_str, '(a,i0,a)') "(i", mult_ind - 1, ")"
        read(input, fmt_str, iostat=stat, iomsg=ioerrmsg) multiplier
        if (stat == 0) then
           token = trim(input(mult_ind+1:))
        else
           if (present(errmsg)) then
              write(errmsg, *) "Invalid multiplier, '",                      &
                   input(1:mult_ind-1), "' in '", trim(input), "'. ",        &
                   "Error message from read(): '", trim(ioerrmsg), "'"
           end if
           multiplier = -1
           token = ''
        end if
     end if

     if ((multiplier >= 0) .and. present(allowed_set)) then
        alen = size(allowed_set)
        match = .false.
        do lind = 1, alen
           if (trim(to_lower(token)) == trim(to_lower(allowed_set(lind)))) then
              match = .true.
              exit
           end if
        end do
        if (.not. match) then
           if (present(errmsg)) then
              write(errmsg, *) "Error, token, '", trim(token), "' not in (/"
              lind = len_trim(errmsg) + 1
              do mult_ind = 1, alen
                 if (mult_ind == alen) then
                    fmt_str = "' "
                 else
                    fmt_str = "', "
                 end if
                 write(errmsg(lind:), *) "'", trim(allowed_set(mult_ind)),   &
                      trim(fmt_str)
                 lind = lind + len_trim(allowed_set(mult_ind)) +             &
                      len_trim(fmt_str) + 2
              end do
              write(errmsg(lind:), *) "/)"
           end if
           multiplier = -1
           token = ''
        end if
     end if

  end subroutine parse_multiplier

end module cam_history_support
