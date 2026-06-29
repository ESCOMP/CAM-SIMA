module cam_constituents

   use ccpp_kinds,                only: kind_phys
   use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t

   implicit none
   private

   ! Public system functions
   public :: cam_constituents_readnl
   public :: cam_constituents_init
   ! Public accessor functions
   public :: const_name     ! Constituent standard name
   public :: const_longname
   public :: const_molec_weight
   public :: const_get_index
   public :: const_is_advected
   public :: const_is_dry
   public :: const_is_moist
   public :: const_is_wet
   public :: const_is_thermo_active
   public :: const_is_water_species
   public :: const_set_thermo_active
   public :: const_set_water_species
   public :: const_qmin
   public :: const_set_qmin

   ! Private array of constituent properties (for property interface functions)
   type(ccpp_constituent_prop_ptr_t), pointer :: const_props(:) => NULL()

   ! Namelist variable
   ! readtrace: Obtain initial tracer data from IC file if .true.
   logical, public :: readtrace = .true.
   ! Only allow initialization once
   logical, private :: initialized = .false.

   !> \section arg_table_cam_constituents  Argument Table
   !! \htmlinclude cam_constituents.html
   integer, public, protected :: num_advected = 0

   integer, public, protected :: num_constituents = 0

   !! Note: There are no <xxx>_name interfaces in function interfaces below
   !!       because use of this sort of interface is often for optional
   !!       constituents and there is no way to indicate a missing
   !!       constituent in these functions (e.g., a logical).

   interface const_is_advected
      module procedure const_is_advected_obj
      module procedure const_is_advected_index
   end interface const_is_advected

   interface const_is_dry
      module procedure const_is_dry_obj
      module procedure const_is_dry_index
   end interface const_is_dry

   interface const_is_moist
      module procedure const_is_moist_obj
      module procedure const_is_moist_index
   end interface const_is_moist

   interface const_is_wet
      module procedure const_is_wet_obj
      module procedure const_is_wet_index
   end interface const_is_wet

   interface const_is_thermo_active
      module procedure const_is_thermo_active_obj
      module procedure const_is_thermo_active_index
   end interface const_is_thermo_active

   interface const_is_water_species
      module procedure const_is_water_species_obj
      module procedure const_is_water_species_index
   end interface const_is_water_species

   interface const_set_thermo_active
      module procedure const_set_thermo_active_obj
      module procedure const_set_thermo_active_index
   end interface const_set_thermo_active

   interface const_set_water_species
      module procedure const_set_water_species_obj
      module procedure const_set_water_species_index
   end interface const_set_water_species

   interface const_qmin
      module procedure const_qmin_obj
      module procedure const_qmin_index
   end interface const_qmin

   interface const_set_qmin
      module procedure const_set_qmin_obj
      module procedure const_set_qmin_index
   end interface

   ! Private interfaces
   private :: check_index_bounds

CONTAINS

   subroutine cam_constituents_readnl(nlfile)

      use mpi,            only: mpi_logical
      use shr_nl_mod,     only: find_group_name => shr_nl_find_group_name
      use spmd_utils,     only: masterproc, mpicom, mstrid=>masterprocid
      use cam_logfile,    only: iulog
      use cam_abortutils, only: endrun

      ! nlfile: filepath for file containing namelist input
      character(len=*), intent(in) :: nlfile

      ! Local variables
      integer                      :: unitn, ierr
      character(len=*), parameter  :: sub = 'cam_constituents_readnl'

      namelist /constituents_nl/ readtrace
      !------------------------------------------------------------------------

      !!XXgoldyXX: v Need to figure out how to figure out pcnst
      !! Update physconst so that we can use 'dry_air_species' and
      !!   'water_species_in_air' from air_composition_nl.
      !! Register CCPP constituents (see call below)
      !! Count up species from air_composition_nl plus CCPP advected
      !!   constituents not in that namelist.
      !! Make sure there are indices for all thermodynamically-active species
      !!   in runtime DDT object. Pack them at front of state Q array.
      !!XXgoldyXX: ^ Need to figure out how to figure out pcnst

      if (masterproc) then
         open(newunit=unitn, file=trim(nlfile), status='old')
         call find_group_name(unitn, 'constituents_nl', status=ierr)
         if (ierr == 0) then
            read(unitn, constituents_nl, iostat=ierr)
            if (ierr /= 0) then
               call endrun(sub//': FATAL: reading namelist',                  &
                    file=__FILE__, line=__LINE__)
            end if
         end if
         close(unitn)
      end if

      call mpi_bcast(readtrace, 1, mpi_logical, mstrid, mpicom, ierr)
      if (ierr /= 0) then
         call endrun(sub//": FATAL: mpi_bcast: readtrace",                    &
              file=__FILE__, line=__LINE__)
      end if

      if (masterproc) then
         write(iulog,*)'Summary of constituent module options:'
         if (readtrace) then
            write(iulog,*)'  Attempt to read constituent initial values ',    &
                 'from the initial file by default'
         else
            write(iulog,*)'  Do not read constituent initial values ',        &
                 'from the initial file'
         end if
      end if

   end subroutine cam_constituents_readnl

   !#######################################################################

   subroutine cam_constituents_init(cnst_prop_ptr, num_advect)
      use cam_abortutils, only: endrun
      use spmd_utils,     only: masterproc
      use cam_logfile,    only: iulog, debug_output
      use cam_logfile,    only: DEBUGOUT_VERBOSE

      ! Initialize module constituent variables
      type(ccpp_constituent_prop_ptr_t), pointer :: cnst_prop_ptr(:)
      integer, intent(in)                        :: num_advect

      !For log output:
      integer :: cnst_idx

      if (initialized) then
         call endrun("cam_constituents_init: already initialized",            &
              file=__FILE__, line=__LINE__)
      end if
      const_props => cnst_prop_ptr
      num_advected = num_advect
      num_constituents = size(const_props)

      initialized = .true.

      !If log level is verbose, then print out
      !the names/order of all registered constituents:
      if ((debug_output >= DEBUGOUT_VERBOSE) .and. masterproc) then

         write(iulog,*) 'LIST OF REGISTERED CONSTITUENTS:'
         write(iulog,*) '********************************'
         write(iulog,*) ' Constituent index : Standard name : Advected (T or F)'
         do cnst_idx = 1, num_constituents
            write(iulog,'(I0,3A, L)') cnst_idx, ' : ', trim(const_name(cnst_idx)), ' : ', &
                                   const_is_advected(cnst_idx)
         end do
         write(iulog,*) '********************************'

      end if

   end subroutine cam_constituents_init

   !#######################################################################

   logical function check_index_bounds(const_ind, subname)
      use cam_abortutils, only: endrun
      use string_utils,   only: to_str

      ! Return the standard name of the constituent at <const_ind>.
      ! Dummy arguments
      integer,          intent(in) :: const_ind
      character(len=*), intent(in) :: subname
      ! Local variables
      integer            :: err_code
      character(len=256) :: err_msg

      if (const_ind < LBOUND(const_props, 1)) then
         call endrun(subname//"index ("//to_str(const_ind)//") out of "//      &
              "bounds, lower bound is "//to_str(LBOUND(const_props, 1)),      &
              file=__FILE__, line=__LINE__)
         check_index_bounds = .false. ! safety in case abort becomes optionsl
      else if (const_ind > UBOUND(const_props, 1)) then
         call endrun(subname//"index ("//to_str(const_ind)//") out of "//      &
              "bounds, upper bound is "//to_str(UBOUND(const_props, 1)),      &
              file=__FILE__, line=__LINE__)
         check_index_bounds = .false. ! safety in case abort becomes optionsl
      else
         check_index_bounds = .true.
      end if

   end function check_index_bounds

   !#######################################################################

   function const_name(const_ind)
      use cam_abortutils,       only: endrun
      use string_utils,         only: to_str
      use phys_vars_init_check, only: std_name_len

      ! Return the standard name of the constituent at <const_ind>.
      ! Dummy arguments
      integer, intent(in)         :: const_ind
      character(len=std_name_len) :: const_name
      ! Local variables
      integer                     :: err_code
      character(len=256)          :: err_msg
      character(len=*), parameter :: subname = 'const_name: '

      if (check_index_bounds(const_ind, subname)) then
         call const_props(const_ind)%standard_name(const_name,                &
              err_code, err_msg)
         if (err_code /= 0) then
            call endrun(subname//"Error "//to_str(err_code)//": "//           &
                 trim(err_msg), file=__FILE__, line=__LINE__)
         end if
      end if

   end function const_name

   !#######################################################################

   function const_longname(const_ind)
      use cam_abortutils, only: endrun
      use string_utils,   only: to_str
      use shr_kind_mod,   only: CL => shr_kind_cl

      ! Return the long name of the constituent at <const_ind>.
      ! Dummy arguments
      integer, intent(in)         :: const_ind
      character(len=CL)           :: const_longname
      ! Local variables
      integer                     :: err_code
      character(len=256)          :: err_msg
      character(len=*), parameter :: subname = 'const_longname: '

      if (check_index_bounds(const_ind, subname)) then
         call const_props(const_ind)%long_name(const_longname,                &
              err_code, err_msg)
         if (err_code /= 0) then
            call endrun(subname//"Error "//to_str(err_code)//": "//           &
                 trim(err_msg), file=__FILE__, line=__LINE__)
         end if
      end if

   end function const_longname

   !#######################################################################

   function const_molec_weight(const_ind)
      use cam_abortutils, only: endrun
      use string_utils,   only: to_str

      ! Return the long name of the constituent at <const_ind>.
      ! Dummy arguments
      integer, intent(in) :: const_ind
      real(kind_phys)     :: const_molec_weight
      ! Local variables
      integer                     :: err_code
      character(len=256)          :: err_msg
      character(len=*), parameter :: subname = 'const_molec_weight: '

      if (check_index_bounds(const_ind, subname)) then
         call const_props(const_ind)%molar_mass(const_molec_weight,         &
              err_code, err_msg)
         if (err_code /= 0) then
            call endrun(subname//"Error "//to_str(err_code)//": "//           &
                 trim(err_msg), file=__FILE__, line=__LINE__)
         end if
      end if

   end function const_molec_weight

   !#######################################################################

   subroutine const_get_index(name, cindex, abort, warning, caller)
      ! from to_be_ccppized utility routine
      use ccpp_const_utils,     only: ccpp_const_get_idx

      use shr_kind_mod,         only: CX => SHR_KIND_CX
      use cam_abortutils,       only: endrun
      use cam_logfile,          only: iulog
      use phys_vars_init_check, only: std_name_len
      use string_utils,         only: stringify

      ! Get the index of a constituent with standard name, <name>.
      ! Setting optional <abort> argument to .false. returns control to
      !    the caller if the constituent name is not found.
      ! Default behavior is to call endrun when name is not found.
      ! If the optional argument, <caller>, is passed, it is used
      !    instead of <subname> in messages.

      !-----------------------------Arguments---------------------------------
      character(len=*),           intent(in)  :: name    ! constituent name
      integer,                    intent(out) :: cindex  ! global constituent index
      logical,          optional, intent(in)  :: abort   ! flag controlling abort
      logical,          optional, intent(in)  :: warning ! flag controlling warning
      character(len=*), optional, intent(in)  :: caller  ! calling routine

      !---------------------------Local workspace-----------------------------
      logical                     :: warning_on_error
      logical                     :: abort_on_error
      integer                     :: errcode
      character(len=CX)           :: errmsg
      character(len=*), parameter :: subname = 'const_get_index: '
      !-----------------------------------------------------------------------

      call ccpp_const_get_idx(const_props, name, cindex, errmsg, errcode)

      if (errcode /= 0) then
         call endrun(subname//"Error "//stringify((/errcode/))//": "//           &
                 trim(errmsg), file=__FILE__, line=__LINE__)
      endif

      if (cindex == -1) then
         ! Unrecognized name, set an error return and possibly abort
         cindex = -1
         if (present(abort)) then
            abort_on_error = abort
         else
            abort_on_error = .true.
         end if
         if (present(warning)) then
            warning_on_error = warning
         else
            warning_on_error = .true.
         end if

         if (abort_on_error) then
            if (present(caller)) then
               write(iulog, *) caller, 'FATAL: name:', trim(name),            &
                    ' not found in constituent table'
               call endrun(caller//'FATAL: name ('//trim(name)//') not found')
            else
               write(iulog, *) subname, 'FATAL: name:', trim(name),           &
                    ' not found in constituent table'
               call endrun(subname//'FATAL: name ('//trim(name)//') not found')
            end if
         else
            if (warning_on_error) then
               if (present(caller)) then
                  write(iulog, *) caller, 'WARNING: name:', trim(name),          &
                       ' not found in constituent table'
               else
                  write(iulog, *) subname, 'WARNING: name:', trim(name),         &
                       ' not found in constituent table'
               end if
            end if
         end if
      end if

   end subroutine const_get_index

   !#######################################################################

   logical function const_is_advected_obj(const_obj)
      use cam_abortutils, only: endrun
      use string_utils,   only: to_str

      ! Return .true. if the constituent object, <const_obj>, is advected
      ! Dummy argument
      type(ccpp_constituent_prop_ptr_t), intent(in) :: const_obj
      ! Local variables
      integer                     :: err_code
      character(len=256)          :: err_msg
      character(len=*), parameter :: subname = 'const_is_advected_obj: '

      call const_obj%is_advected(const_is_advected_obj, err_code, err_msg)
      if (err_code /= 0) then
         call endrun(subname//"Error "//to_str(err_code)//": "//           &
              trim(err_msg), file=__FILE__, line=__LINE__)
      end if

   end function const_is_advected_obj

   !#######################################################################

   logical function const_is_advected_index(const_ind)

      ! Return .true. if the constituent at <index> is advected
      ! Dummy argument
      integer, intent(in) :: const_ind
      ! Local variable
      character(len=*), parameter :: subname = 'const_is_advected_index: '

      if (check_index_bounds(const_ind, subname)) then
         const_is_advected_index = const_is_advected(const_props(const_ind))
      end if

   end function const_is_advected_index

   !#######################################################################

   logical function const_is_dry_obj(const_obj)
      use cam_abortutils, only: endrun
      use string_utils,   only: to_str

      ! Return .true. if the constituent object, <const_obj>, is dry
      ! Dummy argument
      type(ccpp_constituent_prop_ptr_t), intent(in) :: const_obj
      ! Local variables
      integer                     :: err_code
      character(len=256)          :: err_msg
      character(len=*), parameter :: subname = 'const_is_dry_obj: '

      call const_obj%is_dry(const_is_dry_obj, err_code, err_msg)
      if (err_code /= 0) then
         call endrun(subname//"Error "//to_str(err_code)//": "//           &
              trim(err_msg), file=__FILE__, line=__LINE__)
      end if

   end function const_is_dry_obj

   !#######################################################################

   logical function const_is_dry_index(const_ind)

      ! Return .true. if the constituent at <index> is dry
      ! Dummy argument
      integer, intent(in) :: const_ind
      ! Local variable
      character(len=*), parameter :: subname = 'const_is_dry_index: '

      if (check_index_bounds(const_ind, subname)) then
         const_is_dry_index = const_is_dry(const_props(const_ind))
      end if

   end function const_is_dry_index

   !#######################################################################

   logical function const_is_moist_obj(const_obj)
      use cam_abortutils, only: endrun
      use string_utils,   only: to_str

      ! Return .true. if the constituent object, <const_obj>, is moist
      ! Dummy argument
      type(ccpp_constituent_prop_ptr_t), intent(in) :: const_obj
      ! Local variables
      integer                     :: err_code
      character(len=256)          :: err_msg
      character(len=*), parameter :: subname = 'const_is_moist_obj: '

      call const_obj%is_moist(const_is_moist_obj, err_code, err_msg)
      if (err_code /= 0) then
         call endrun(subname//"Error "//to_str(err_code)//": "//           &
              trim(err_msg), file=__FILE__, line=__LINE__)
      end if

   end function const_is_moist_obj

   !#######################################################################

   logical function const_is_moist_index(const_ind)

      ! Return .true. if the constituent at <index> is moist
      ! Dummy argument
      integer, intent(in) :: const_ind
      ! Local variable
      character(len=*), parameter :: subname = 'const_is_moist_index: '

      if (check_index_bounds(const_ind, subname)) then
         const_is_moist_index = const_is_moist(const_props(const_ind))
      end if

   end function const_is_moist_index

   !#######################################################################

   logical function const_is_wet_obj(const_obj)
      use cam_abortutils, only: endrun
      use string_utils,   only: to_str

      ! Return .true. if the constituent object, <const_obj>, is wet
      ! Dummy argument
      type(ccpp_constituent_prop_ptr_t), intent(in) :: const_obj
      ! Local variables
      integer                     :: err_code
      character(len=256)          :: err_msg
      character(len=*), parameter :: subname = 'const_is_wet_obj: '

      call const_obj%is_wet(const_is_wet_obj, err_code, err_msg)
      if (err_code /= 0) then
         call endrun(subname//"Error "//to_str(err_code)//": "//           &
              trim(err_msg), file=__FILE__, line=__LINE__)
      end if

   end function const_is_wet_obj

   !#######################################################################

   logical function const_is_wet_index(const_ind)

      ! Return .true. if the constituent at <index> is wet
      ! Dummy argument
      integer, intent(in) :: const_ind
      ! Local variable
      character(len=*), parameter :: subname = 'const_is_wet_index: '

      if (check_index_bounds(const_ind, subname)) then
         const_is_wet_index = const_is_wet(const_props(const_ind))
      end if

   end function const_is_wet_index

   !#######################################################################

   logical function const_is_thermo_active_obj(const_obj)
      use cam_abortutils, only: endrun
      use string_utils,   only: to_str

      ! Return .true. if the constituent object, <const_obj>, is
      ! thermodynamically-active
      ! Dummy argument
      type(ccpp_constituent_prop_ptr_t), intent(in) :: const_obj
      ! Local variables
      integer                     :: err_code
      character(len=256)          :: err_msg
      character(len=*), parameter :: subname = 'const_is_thermo_active_obj: '

      call const_obj%is_thermo_active(const_is_thermo_active_obj, err_code, err_msg)
      if (err_code /= 0) then
         call endrun(subname//"Error "//to_str(err_code)//": "//           &
              trim(err_msg), file=__FILE__, line=__LINE__)
      end if

   end function const_is_thermo_active_obj

   !#######################################################################

   logical function const_is_thermo_active_index(const_ind)

      ! Return .true. if the constituent at <index> is
      ! thermodynamically-active
      ! Dummy argument
      integer, intent(in) :: const_ind
      ! Local variable
      character(len=*), parameter :: subname = 'const_is_thermo_active_index: '

      if (check_index_bounds(const_ind, subname)) then
         const_is_thermo_active_index = const_is_thermo_active(const_props(const_ind))
      end if

   end function const_is_thermo_active_index

   !#######################################################################

   logical function const_is_water_species_obj(const_obj)
      use cam_abortutils, only: endrun
      use string_utils,   only: to_str

      ! Return .true. if the constituent object, <const_obj>, is
      ! a type (species) of water
      ! Dummy argument
      type(ccpp_constituent_prop_ptr_t), intent(in) :: const_obj
      ! Local variables
      integer                     :: err_code
      character(len=256)          :: err_msg
      character(len=*), parameter :: subname = 'const_is_water_species_obj: '

      call const_obj%is_water_species(const_is_water_species_obj, err_code, err_msg)
      if (err_code /= 0) then
         call endrun(subname//"Error "//to_str(err_code)//": "//           &
              trim(err_msg), file=__FILE__, line=__LINE__)
      end if

   end function const_is_water_species_obj

   !#######################################################################

   logical function const_is_water_species_index(const_ind)

      ! Return .true. if the constituent at <index> is
      ! a type (species) of water
      ! Dummy argument
      integer, intent(in) :: const_ind
      ! Local variable
      character(len=*), parameter :: subname = 'const_is_water_species_index: '

      if (check_index_bounds(const_ind, subname)) then
         const_is_water_species_index = const_is_water_species(const_props(const_ind))
      end if

   end function const_is_water_species_index

   !#######################################################################

   subroutine const_set_thermo_active_obj(const_obj, thermo_active)
      use cam_abortutils, only: endrun
      use string_utils,   only: to_str

      ! Set the value for the 'thermo_active' property for the constituent
      !object, <const_obj>.
      ! Dummy argument
      type(ccpp_constituent_prop_ptr_t), intent(inout) :: const_obj
      logical, intent(in)                              :: thermo_active
      ! Local variables
      integer                     :: err_code
      character(len=256)          :: err_msg
      character(len=*), parameter :: subname = 'const_set_thermo_active_obj: '

      call const_obj%set_thermo_active(thermo_active, err_code, err_msg)
      if (err_code /= 0) then
         call endrun(subname//"Error "//to_str(err_code)//": "//           &
              trim(err_msg), file=__FILE__, line=__LINE__)
      end if

   end subroutine const_set_thermo_active_obj

   !#######################################################################

   subroutine const_set_thermo_active_index(const_ind, thermo_active)

      ! Set the value for the 'thermo_active' property for the constituent
      !object index, <const_ind>.
      ! Dummy argument
      integer, intent(in) :: const_ind
      logical, intent(in) :: thermo_active
      ! Local variable
      character(len=*), parameter :: subname = 'const_set_thermo_active_index: '

      if (check_index_bounds(const_ind, subname)) then
         call const_set_thermo_active(const_props(const_ind), thermo_active)
      end if

   end subroutine const_set_thermo_active_index

   !#######################################################################

   subroutine const_set_water_species_obj(const_obj, water_species)
      use cam_abortutils, only: endrun
      use string_utils,   only: to_str

      ! Set the value for the 'water_species' property for the constituent
      !object, <const_obj>.
      ! Dummy argument
      type(ccpp_constituent_prop_ptr_t), intent(inout) :: const_obj
      logical, intent(in)                              :: water_species
      ! Local variables
      integer                     :: err_code
      character(len=256)          :: err_msg
      character(len=*), parameter :: subname = 'const_set_water_species_obj: '

      call const_obj%set_water_species(water_species, err_code, err_msg)
      if (err_code /= 0) then
         call endrun(subname//"Error "//to_str(err_code)//": "//           &
              trim(err_msg), file=__FILE__, line=__LINE__)
      end if

   end subroutine const_set_water_species_obj

   !#######################################################################

   subroutine const_set_water_species_index(const_ind, water_species)

      ! Set the value for the 'water_species' property for the constituent
      !object index, <const_ind>.
      ! Dummy argument
      integer, intent(in) :: const_ind
      logical, intent(in) :: water_species
      ! Local variable
      character(len=*), parameter :: subname = 'const_set_water_species_index: '

      if (check_index_bounds(const_ind, subname)) then
         call const_set_water_species(const_props(const_ind), water_species)
      end if

   end subroutine const_set_water_species_index

   !#######################################################################

   real(kind_phys) function const_qmin_obj(const_obj)
      use cam_abortutils, only: endrun
      use string_utils,   only: to_str

      ! Return the minimum allowed mixing ratio for, <const_obj>
      ! Dummy argument
      type(ccpp_constituent_prop_ptr_t), intent(in) :: const_obj
      ! Local variables
      integer                     :: err_code
      character(len=256)          :: err_msg
      character(len=*), parameter :: subname = 'const_qmin_obj: '

      call const_obj%minimum(const_qmin_obj, err_code, err_msg)
      if (err_code /= 0) then
         call endrun(subname//"Error "//to_str(err_code)//": "//           &
              trim(err_msg), file=__FILE__, line=__LINE__)
      end if

   end function const_qmin_obj

   !#######################################################################

   real(kind_phys) function const_qmin_index(const_ind)

      ! Return the minimum allowed mxing ratio for the constituent at <index>
      ! Dummy argument
      integer, intent(in) :: const_ind
      ! Local variable
      character(len=*), parameter :: subname = 'const_qmin_index: '

      if (check_index_bounds(const_ind, subname)) then
         const_qmin_index = const_qmin(const_props(const_ind))
      end if

   end function const_qmin_index

   !#######################################################################

   subroutine const_set_qmin_obj(const_obj, qmin_val)
      use cam_abortutils, only: endrun
      use string_utils,   only: to_str

      ! Set the minimum value property for the constituent
      !object, <const_obj>.
      ! Dummy argument
      type(ccpp_constituent_prop_ptr_t), intent(inout) :: const_obj
      real(kind_phys),                   intent(in)    :: qmin_val
      ! Local variables
      integer                     :: err_code
      character(len=256)          :: err_msg
      character(len=*), parameter :: subname = 'const_set_qmin_obj: '

      call const_obj%set_minimum(qmin_val, err_code, err_msg)
      if (err_code /= 0) then
         call endrun(subname//"Error "//to_str(err_code)//": "//           &
              trim(err_msg), file=__FILE__, line=__LINE__)
      end if

   end subroutine const_set_qmin_obj

   !#######################################################################

   subroutine const_set_qmin_index(const_ind, qmin_val)

      ! Set the value for the minimu value property for the constituent
      !object index, <const_ind>.
      ! Dummy argument
      integer, intent(in)         :: const_ind
      real(kind_phys), intent(in) :: qmin_val
      ! Local variable
      character(len=*), parameter :: subname = 'const_set_qmin_index: '

      if (check_index_bounds(const_ind, subname)) then
         call const_set_qmin(const_props(const_ind), qmin_val)
      end if

   end subroutine const_set_qmin_index

   !#######################################################################

end module cam_constituents
