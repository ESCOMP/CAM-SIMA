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
   public :: const_get_index
   public :: const_is_dry
   public :: const_is_moist
   public :: const_is_wet
   public :: const_qmin

   character(len=46), public, parameter :: water_species_stdnames(5) = (/     &
        "specific_humidity                             ",                     &
        "cloud_liquid_water_mixing_ratio_wrt_total_mass",                     &
        "cloud_ice_mixing_ratio_wrt_total_mass         ",                     &
        "rain_mixing_ratio_wrt_total_mass              ",                     &
        "snow_mixing_ratio_wrt_total_mass              " /)

   ! To keep track of standard name locations above, save the indices
   integer, private :: ind_q_ind       = 1
   integer, private :: ind_cld_liq_ind = 2
   integer, private :: ind_cld_ice_ind = 3
   integer, private :: ind_rain_ind    = 4
   integer, private :: ind_snow_ind    = 5

   ! Private array of constituent properties (for property interface functions)
   type(ccpp_constituent_prop_ptr_t), pointer :: const_props(:) => NULL()

   ! Namelist variable
   ! readtrace: Obtain initial tracer data from IC file if .true.
   logical, private :: readtrace = .true.
   ! Only allow initialization once
   logical, private :: initialized = .false.

   !> \section arg_table_cam_constituents  Argument Table
   !! \htmlinclude cam_constituents.html
   integer, public, protected :: num_advected = 0
   integer, public, protected :: indx_spec_humidity_wet = -1
   integer, public, protected :: indx_cloud_liquid_wet = -1
   integer, public, protected :: indx_cloud_ice_wet = -1
   integer, public, protected :: indx_rain_wet = -1
   integer, public, protected :: indx_snow_wet = -1

   !! Note: There are no <xxx>_name interfaces in function interfaces below
   !!       because use of this sort of interface is often for optional
   !!       constituents and there is no way to indicate a missing
   !!       constituent in these functions (e.g., a logical).

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

   interface const_qmin
      module procedure const_qmin_obj
      module procedure const_qmin_index
   end interface const_qmin

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

   subroutine cam_constituents_init(cnst_prop_ptr, num_advect, ind_water_spec)
      use cam_abortutils, only: endrun

      ! Initialize module constituent variables
      type(ccpp_constituent_prop_ptr_t), pointer :: cnst_prop_ptr(:)
      integer, intent(in)                        :: num_advect
      integer, intent(in)                        :: ind_water_spec(:)

      if (initialized) then
         call endrun("cam_constituents_init: already initialized",            &
              file=__FILE__, line=__LINE__)
      end if
      const_props => cnst_prop_ptr
      num_advected = num_advect
      indx_spec_humidity_wet = ind_water_spec(ind_q_ind)
      indx_cloud_liquid_wet = ind_water_spec(ind_cld_liq_ind)
      indx_cloud_ice_wet = ind_water_spec(ind_cld_ice_ind)
      indx_rain_wet = ind_water_spec(ind_rain_ind)
      indx_snow_wet = ind_water_spec(ind_snow_ind)

      initialized = .true.

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
         call endrun(subname//"index ("//to_str(const_in)//") out of "//      &
              "bounds, lower bound is "//to_str(LBOUND(const_props, 1)),      &
              file=__FILE__, line=__LINE__)
         check_index_bounds = .false. ! safety in case abort becomes optionsl
      else if (const_ind > UBOUND(const_props, 1)) then
         call endrun(subname//"index ("//to_str(const_in)//") out of "//      &
              "bounds, upper bound is "//to_str(UBOUND(const_props, 1)),      &
              file=__FILE__, line=__LINE__)
         check_index_bounds = .false. ! safety in case abort becomes optionsl
      else
         check_index_bounds = .true.
      end if

   end function check_index_bounds

   !#######################################################################

   function const_name(const_ind)
      use cam_abortutils, only: endrun
      use string_utils,   only: to_str

      ! Return the standard name of the constituent at <const_ind>.
      ! Dummy arguments
      integer, intent(in) :: const_ind
      character(len=*)    :: const_name
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

      ! Return the long name of the constituent at <const_ind>.
      ! Dummy arguments
      integer, intent(in) :: const_ind
      character(len=*)    :: const_longname
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

   subroutine const_get_index(name, cindex, abort, caller)
      use shr_kind_mod,   only: CX => SHR_KIND_CX
      use cam_abortutils, only: endrun
      use cam_logfile,    only: iulog
      use cam_ccpp_cap,   only: cam_const_get_index

      ! Get the index of a constituent with standard name, <name>.
      ! Setting optional <abort> argument to .false. returns control to
      !    the caller if the constituent name is not found.
      ! Default behavior is to call endrun when name is not found.
      ! If the optional argument, <caller>, is passed, it is used
      !    instead of <subname> in messages.

      !-----------------------------Arguments---------------------------------
      character(len=*),           intent(in)  :: name   ! constituent name
      integer,                    intent(out) :: cindex ! global constituent ind
      logical,          optional, intent(in)  :: abort  ! flag controlling abort
      character(len=*), optional, intent(in)  :: caller ! calling routine

      !---------------------------Local workspace-----------------------------
      logical                     :: abort_on_error
      integer                     :: errcode
      character(len=CX)           :: errmsg
      character(len=*), parameter :: subname = 'const_get_index: '
      !-----------------------------------------------------------------------

      ! Find tracer name in the master table
      call cam_const_get_index(name, cindex, errcode=errcode, errmsg=errmsg)

      if (errcode /= 0) then
         ! Unrecognized name, set an error return and possibly abort
         cindex = -1
         if (present(abort)) then
            abort_on_error = abort
         else
            abort_on_error = .true.
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
            if (present(caller)) then
               write(iulog, *) caller, 'WARNING: name:', trim(name),          &
                    ' not found in constituent table'
            else
               write(iulog, *) subname, 'WARNING: name:', trim(name),         &
                    ' not found in constituent table'
            end if
         end if
      end if

   end subroutine const_get_index

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


end module cam_constituents
