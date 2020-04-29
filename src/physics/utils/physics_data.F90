module physics_data

   use ccpp_kinds,    only: kind_phys

   implicit none
   private

   public :: physics_read_data

   interface read_field
      module procedure read_field_2d
      module procedure read_field_3d
   end interface read_field

!==============================================================================
CONTAINS
!==============================================================================

   integer function find_input_name_idx(stdname, input_stdnames)

      ! Determine whether we should read <stdname> from <data_file>.

      ! Input (dummy) arguments:
      character(len=*),  intent(in) :: stdname            !Variable standard name being checked
      character(len=*),  intent(in) :: input_stdnames(:)  !Array of variable standard names with file inputs
      integer                       :: idx                !standard names array index

      !Initialize function:
      find_input_name_idx = -1

      !Loop through required CCPP variable standard names:
      do idx = 1, size(input_stdnames, 1)
         !Check if provided name is in required names array:
         if (trim(stdname) == trim(input_stdnames(idx))) then
            find_input_name_idx = idx
            exit
         end if
      end do

   end function find_input_name_idx

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
      use physics_types,  only: ic_name_len

      ! Dummy arguments
      type(file_desc_t), intent(inout) :: file
      character(len=*),  intent(in)    :: var_names(:)
      integer,           intent(in)    :: timestep
      real(kind_phys),   intent(inout) :: buffer(:)
      ! Local variables
      logical                          :: var_found
      character(len=ic_name_len)       :: found_name
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
      use physics_types,  only: ic_name_len

      ! Dummy arguments
      type(file_desc_t), intent(inout) :: file
      character(len=*),  intent(in)    :: var_names(:)
      character(len=*),  intent(in)    :: vcoord_name
      integer,           intent(in)    :: timestep
      real(kind_phys),   intent(inout) :: buffer(:,:)
      ! Local variables
      logical                          :: var_found
      integer                          :: num_levs
      character(len=ic_name_len)       :: found_name
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

   subroutine physics_read_data(file, suite_names, timestep)
      use pio,            only: file_desc_t
      use cam_logfile,    only: iulog
      use shr_kind_mod,   only: SHR_KIND_CS
      use physics_types,  only: phys_state, pdel, pdeldry, zm, lnpint, lnpmid
      use physics_types,  only: pint, pmid, pmiddry, rpdel
      use physics_types,  only: ix_qv, ix_cld_liq, ix_rain
      use physics_types,  only: input_var_stdnames, input_var_names
      use physics_types,  only: std_name_len
      use cam_ccpp_cap,   only: ccpp_physics_suite_variables

      ! Dummy argument
      type(file_desc_t), intent(inout) :: file
      character(len=SHR_KIND_CS)       :: suite_names(:) !Names of CCPP suites
      integer,           intent(in)    :: timestep

      !Local variables:

      !Character array containing all CCPP-required vairable standard names:
      character(len=std_name_len), allocatable :: ccpp_required_data(:)

      character(len=512) :: errmsg  !CCPP framework error message
      integer            :: errflg  !CCPP framework error flag
      integer            :: idx     !Input variable array index
      integer            :: i, s    !loop control variables

      !Loop over CCPP physics/chemistry suites:
      do s = 1, size(suite_names, 1)

         !Search for all needed CCPP input variables,
         !so that they can b e read from input file if need be:
         call ccpp_physics_suite_variables(suite_names(s), ccpp_required_data, errmsg, errflg, &
                                              input_vars_in=.true.)

         !Loop over all required variables as specified by CCPP suite:
         do i = 1, size(ccpp_required_data, 1)

            !Find IC file input name array index for required variable:
            idx = find_input_name_idx(ccpp_required_data(i), input_var_stdnames)

            !If an index was never found, then print error and check the rest
            !of the variables, after which the model simulation will end:
            if(idx == -1) then
               write(iulog,*) 'Required variable missing from registered list of input variables: ',&
                              trim(ccpp_required_data(i))
               cycle
            end if

            if (trim(input_var_stdnames(idx)) == 'pressure_thickness') then
               call read_field(file, input_var_names(:,idx), 'lev', timestep, pdel)
            end if
            if (trim(input_var_stdnames(idx)) == 'pressure_thickness_of_dry_air') then
               call read_field(file, input_var_names(:,idx), 'lev',           &
                               timestep, pdeldry)
            end if
            if (trim(input_var_stdnames(idx)) == 'water_vapor_specific_humidity') then
               call read_field(file, input_var_names(:,idx), 'lev', timestep, &
                               phys_state%q(:,:,ix_qv))
            end if
            if (trim(input_var_stdnames(idx)) == 'cloud_liquid_water_mixing_ratio') then
               call read_field(file, input_var_names(:,idx), 'lev', timestep, &
                               phys_state%q(:,:,ix_cld_liq))
            end if
            if (trim(input_var_stdnames(idx)) == 'rain_water_mixing_ratio') then
               call read_field(file, input_var_names(:,idx), 'lev', timestep, &
                               phys_state%q(:,:,ix_rain))
            end if
            if (trim(input_var_stdnames(idx)) == 'geopotential_height') then
               call read_field(file, input_var_names(:,idx), 'lev', timestep, zm)
            end if
            if (trim(input_var_stdnames(idx)) == 'temperature') then
               call read_field(file, input_var_names(:,idx), 'lev', timestep, &
                               phys_state%T)
            end if
            if (trim(input_var_stdnames(idx)) == 'geopotential_at_surface') then
               call read_field(file, input_var_names(:,idx), timestep, phys_state%phis)
            end if
            if (trim(input_var_stdnames(idx)) == &
                'natural_log_of_air_pressure_at_interface') then
               call read_field(file, input_var_names(:,idx), 'ilev', timestep, &
                               lnpint)
            end if
            if (trim(input_var_stdnames(idx)) == 'natural_log_of_air_pressure') then
               call read_field(file, input_var_names(:,idx), 'lev', timestep, lnpmid)
            end if
            if (trim(input_var_stdnames(idx)) == 'air_pressure_at_interface') then
               call read_field(file, input_var_names(:,idx), 'ilev', timestep, pint)
            end if
            if (trim(input_var_stdnames(idx)) == 'air_pressure') then
               call read_field(file, input_var_names(:,idx), 'lev', timestep, pmid)
            end if
            if (trim(input_var_stdnames(idx)) == 'air_pressure_of_dry_air') then
               call read_field(file, input_var_names(:,idx), 'lev', timestep, &
                               pmiddry)
            end if
            if (trim(input_var_stdnames(idx)) == 'reciprocal_pressure_thickness') then
               call read_field(file, input_var_names(:,idx), 'lev', timestep, rpdel)
            end if

         end do !Suite-required variables

         !Deallocate required variables array for use in next suite:
         deallocate(ccpp_required_data)

      end do !CCPP suites

   end subroutine physics_read_data

end module physics_data
