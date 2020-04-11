module physics_data

   use ccpp_kinds,   only: kind_phys
   use shr_kind_mod, only: SHR_KIND_CS

   implicit none
   private

   public :: physics_read_data

   !Character array containing all CCPP-required vairable standard names:
   !NOTE: should this character array be set to the exact length needed? -JN
   character(len=*), allocatable :: ccpp_required_data(:)

   integer, parameter, private :: fieldname_len = 16

   interface read_field
      module procedure read_field_2d
      module procedure read_field_3d
   end interface read_field

!==============================================================================
CONTAINS
!==============================================================================

   logical function read_standard_name(suite_name, stdname)

      use cam_ccpp_cap, only: ccpp_physics_suite_variables

      ! Determine whether we should read <stdname> from <data_file>.

      character(len=SHR_KIND_CS)       :: suite_name !Name of CCPP physics suite
      character(len=*),  intent(in)    :: stdname    !Variable standard name being checked

      character(len=512)               :: errmsg     !CCPP framework error message
      integer                          :: errflg     !CCPP framework error flag
      integer                          :: index

      read_standard_name = .false.

      !Check if CCPP-required standard names array
      !has already been allocated:
      if (.not. allocated(ccpp_required_data)) then
         !If not, then search for all needed CCPP input variables,
         !so that they can b e read from input file if need be:
         ccpp_physics_suite_variables(suite_name, ccpp_required_data, errmsg, errflg, &
                                      input_vars_in=.true.)
      end if


      !Loop through required CCPP variable standard names:
      do index = 1, size(ccpp_required_data, 1)
         !Check if provided name is in required names array:
         if (trim(stdname) == trim(ccpp_required_data(index))) then
            read_standard_name = .true.
            exit
         end if
      end do

   end function read_standard_name

   function arr2str(name_array)
      ! Dummy arguments
      character(len=*), intent(in) :: name_array(:)
      character(len=256)           :: arr2str
      ! Local variables
      integer          :: index
      integer          :: str_pos
      character(len=2) :: sep

      arr2str(1:1) = '('
      sep = '/ '
      str_pos = 2
      do index = 1, size(name_array, 1)
         write(arr2str(str_pos:), '(2a)') sep, trim(name_array(index))
         str_pos = len_trim(arr2str) + 1
         sep = ', '
      end do
      write(arr2str(str_pos:), *) ' /)'
   end function arr2str


   subroutine read_field_2d(file, var_names, timestep, buffer)
      use shr_assert_mod, only: shr_assert_in_domain
      use shr_sys_mod,    only: shr_sys_flush
      use pio,            only: file_desc_t, var_desc_t
      use spmd_utils,     only: masterproc
      use cam_pio_utils,  only: cam_pio_find_var
      use cam_abortutils, only: endrun
      use cam_logfile,    only: iulog
      use cam_field_read, only: cam_read_field

      ! Dummy arguments
      type(file_desc_t), intent(inout) :: file
      character(len=*),  intent(in)    :: var_names(:)
      integer,           intent(in)    :: timestep
      real(kind_phys),   intent(inout) :: buffer(:)
      ! Local variables
      logical                          :: var_found
      character(len=fieldname_len)     :: found_name
      type(var_desc_t)                 :: vardesc
      character(len=*), parameter      :: subname = 'read_field_2d: '

      call cam_pio_find_var(file, var_names, found_name, vardesc, var_found)

      if (var_found) then
         if (masterproc) then
            write(iulog, *) 'Reading input field, ', trim(found_name)
            call shr_sys_flush(iulog)
         end if
         call cam_read_field(found_name, file, buffer, var_found,             &
              timelevel=timestep)
      else
         call endrun(subname//'No variable found in '//arr2str(var_names))
      end if
      if (var_found) then
         call shr_assert_in_domain(buffer, is_nan=.false.,                    &
              varname=trim(found_name),                                       &
              msg=subname//'NaN found in '//trim(found_name))
      else
         call endrun(subname//'Mismatch variable found in '//arr2str(var_names))
      end if
   end subroutine read_field_2d

   subroutine read_field_3d(file, var_names, vcoord_name, timestep, buffer)
      use shr_assert_mod, only: shr_assert_in_domain
      use shr_sys_mod,    only: shr_sys_flush
      use pio,            only: file_desc_t, var_desc_t
      use spmd_utils,     only: masterproc
      use cam_pio_utils,  only: cam_pio_find_var
      use cam_abortutils, only: endrun
      use cam_logfile,    only: iulog
      use cam_field_read, only: cam_read_field
      use physics_grid,   only: pver, pverp

      ! Dummy arguments
      type(file_desc_t), intent(inout) :: file
      character(len=*),  intent(in)    :: var_names(:)
      character(len=*),  intent(in)    :: vcoord_name
      integer,           intent(in)    :: timestep
      real(kind_phys),   intent(inout) :: buffer(:,:)
      ! Local variables
      logical                          :: var_found
      integer                          :: num_levs
      character(len=fieldname_len)     :: found_name
      type(var_desc_t)                 :: vardesc
      character(len=*), parameter      :: subname = 'read_field_3d: '

      call cam_pio_find_var(file, var_names, found_name, vardesc, var_found)

      if (var_found) then
         if (trim(vcoord_name) == 'lev') then
            num_levs = pver
         else if (trim(vcoord_name) == 'ilev') then
            num_levs = pverp
         else
            call endrun(subname//'Unknown vcoord_name, '//trim(vcoord_name))
         end if
         if (masterproc) then
            write(iulog, *) 'Reading input field, ', trim(found_name)
            call shr_sys_flush(iulog)
         end if
         call cam_read_field(found_name, file, buffer, var_found,             &
              timelevel=timestep, dim3name=trim(vcoord_name),                 &
              dim3_bnds=(/1, num_levs/))
      else
         call endrun(subname//'No variable found in '//arr2str(var_names))
      end if
      if (var_found) then
         call shr_assert_in_domain(buffer, is_nan=.false.,                    &
              varname=trim(found_name),                                       &
              msg=subname//'NaN found in '//trim(found_name))
      else
         call endrun(subname//'Mismatch variable found in '//arr2str(var_names))
      end if
   end subroutine read_field_3d

   subroutine physics_read_data(file, suite_name, timestep)
      use pio,           only: file_desc_t
      use physics_types, only: phys_state, pdel, pdeldry, zm, lnpint, lnpmid
      use physics_types, only: pint, pmid, pmiddry, rpdel
      use physics_types, only: ix_qv, ix_cld_liq, ix_rain
      use physics_types, only: ic_var_num, ic_lookup_table

      ! Dummy argument
      type(file_desc_t), intent(inout) :: file
      character(len=SHR_KIND_CS)       :: suite_name !Name of CCPP physics suite
      integer,           intent(in)    :: timestep

      !Local variables:
      integer :: v !loop control variable

      !Loop over possible file input variables:
      do v = 1, ic_var_num

         !Check if variable is required by CCPP physics suites:
         if(read_standard_name(suite_name, ic_lookup_table(v)%standard_name)) then

            if (ic_lookup_table(v)%standard_name == 'pressure_thickness') then
               call read_field(file, ic_lookup_table(i)%input_names, 'lev', timestep, pdel)
            end if
            if (ic_lookup_table(v)%standard_name == 'pressure_thickness_of_dry_air') then
               call read_field(file, ic_lookup_table(i)%input_names, 'lev',           &
                               timestep, pdeldry)
            end if
            if (ic_lookup_table(v)%standard_name == 'water_vapor_specific_humidity') then
               call read_field(file, ic_lookup_table(i)%input_names, 'lev', timestep, &
                               phys_state%q(:,:,ix_qv))
            end if
            if (ic_lookup_table(v)%standard_name == 'cloud_liquid_water_mixing_ratio') then
               call read_field(file, ic_lookup_table(i)%input_names, 'lev', timestep, &
                               phys_state%q(:,:,ix_cld_liq))
            end if
            if (ic_lookup_table(v)%standard_name == 'rain_water_mixing_ratio') then
               call read_field(file, ic_lookup_table(i)%input_names, 'lev', timestep, &
                               phys_state%q(:,:,ix_rain))
            end if
            if (ic_lookup_table(v)%standard_name == 'geopotential') then
               call read_field(file, ic_lookup_table(i)%input_names, 'lev', timestep, zm)
            end if
            if (ic_lookup_table(v)%standard_name == 'temperature') then
               call read_field(file, ic_lookup_table(i)%input_names, 'lev', timestep, &
                               phys_state%T)
            end if
            if (ic_lookup_table(v)%standard_name == 'surface_geopotential') then
               call read_field(file, ic_lookup_table(i)%input_names, timestep, phys_state%phis)
            end if
            if (ic_lookup_table(v)%standard_name == &
                'natural_log_of_air_pressure_at_interface') then
               call read_field(file, ic_lookup_table(i)%input_names, 'ilev', timestep, &
                               lnpint)
            end if
            if (ic_lookup_table(v)%standard_name == 'natural_log_of_air_pressure') then
               call read_field(file, ic_lookup_table(i)%input_names, 'lev', timestep, lnpmid)
            end if
            if (ic_lookup_table(v)%standard_name == 'air_pressure_at_interface') then
               call read_field(file, ic_lookup_table(i)%input_names, 'ilev', timestep, pint)
            end if
            if (ic_lookup_table(v)%standard_name == 'air_pressure') then
               call read_field(file, ic_lookup_table(i)%input_names, 'lev', timestep, pmid)
            end if
            if (ic_lookup_table(v)%standard_name == 'air_pressure_of_dry_air') then
               call read_field(file, ic_lookup_table(i)%input_names, 'lev', timestep, &
                               pmiddry)
            end if
            if (ic_lookup_table(v)%standard_name == 'reciprocal_pressure_thickness') then
               call read_field(file, ic_lookup_table(i)%input_names, 'lev', timestep, rpdel)
            end if

         end if !Variable required by CCPP suites?

      end do !input variables

   end subroutine physics_read_data

end module physics_data
