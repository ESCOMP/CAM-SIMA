module physics_data

   implicit none
   private

   public :: find_input_name_idx
   public :: read_field
   public :: read_constituent_dimensioned_field
   public :: check_field

   !Non-standard variable indices:
   integer, public, parameter :: no_exist_idx     = -1
   integer, public, parameter :: init_mark_idx    = -2
   integer, public, parameter :: prot_no_init_idx = -3
   integer, public, parameter :: const_idx        = -4

   interface read_field
      module procedure read_field_2d  !horizontal columns only (1-D buffer)
      module procedure read_field_3d  !horizontal columns + vertical levels (2-D buffer)
      module procedure read_field_4d  !horizontal columns + vertical levels + extra dimension (3-D buffer)
   end interface read_field

   interface check_field
      module procedure check_field_2d  !horizontal columns only (1-D buffer)
      module procedure check_field_3d  !horizontal columns + vertical levels (2-D buffer)
      module procedure check_field_4d  !horizontal columns + vertical levels + extra dimension (3-D buffer)
   end interface check_field

   interface read_constituent_dimensioned_field
      module procedure read_constituent_dimensioned_field_2d
   end interface read_constituent_dimensioned_field

!==============================================================================
CONTAINS
!==============================================================================

   integer function find_input_name_idx(stdname, use_init_variables, constituent_index)

      !Finds the 'input_var_names' array index for a given
      !variable standard name.

      use phys_vars_init_check, only: protected_vars
      use phys_vars_init_check, only: phys_var_stdnames
      use phys_vars_init_check, only: phys_const_stdnames
      use phys_vars_init_check, only: phys_var_num, phys_const_num
      use phys_vars_init_check, only: is_initialized
      use phys_vars_init_check, only: is_read_from_file
      use cam_constituents,     only: const_get_index

      ! Dummy arguments
      ! Variable standard name being checked:
      character(len=*),  intent(in)  :: stdname
      ! Logical for whether or not to read initialized variables
      logical,           intent(in)  :: use_init_variables
      ! Variable to store constituent index if necessary:
      integer,           intent(out) :: constituent_index

      ! Local variables
      ! standard names array index:
      integer                       :: idx
      ! to test read_from_file status
      logical                       :: is_read
      logical                       :: is_constituent

      !Initialize function:
      find_input_name_idx = no_exist_idx
      constituent_index = no_exist_idx
      is_constituent = .false.

      !First check if quantity is a constituent:
      call const_get_index(trim(stdname), find_input_name_idx, abort=.false., warning=.false.)
      if (find_input_name_idx >= 0) then
         constituent_index = find_input_name_idx
         is_constituent = .true.
      else
         find_input_name_idx = no_exist_idx
      end if

      !Loop through physics variable standard names:
      do idx = 1, phys_var_num
         !Check if provided name is in required names array:
         if (trim(phys_var_stdnames(idx)) == trim(stdname)) then
            !Check if this variable has already been initialized.
            !If so, then set the index to a quantity that will be skipped:
            if (is_initialized(stdname)) then
               if (use_init_variables) then
                  call is_read_from_file(stdname, is_read)
               else
                  is_read = .false.
               end if
               if (is_read) then
                  !If reading initialized variables, set to idx:
                  if (is_constituent) then
                     find_input_name_idx = const_idx
                  else
                     find_input_name_idx = idx
                  end if
               else
                  !Otherwise, set to init_mark_idx:
                  find_input_name_idx = init_mark_idx
               end if
            else if (protected_vars(idx)) then
               find_input_name_idx = prot_no_init_idx
            else
               !If not already initialized, then pass on the real array index:
               if (is_constituent) then
                  find_input_name_idx = const_idx
               else
                  find_input_name_idx = idx
               end if
            end if
            !Exit physics variable name loop:
            exit
         end if
      end do
      ! If not found, loop through the excluded variable standard names
      if (find_input_name_idx == no_exist_idx) then
         do idx = 1, phys_const_num
            if (trim(phys_const_stdnames(idx)) == trim(stdname)) then
               ! Set to initialized because we can't check here.
               ! The relevant modules (e.g., cam_constituents) will check.
               find_input_name_idx = init_mark_idx
            end if
         end do
      end if

   end function find_input_name_idx

   subroutine read_field_2d(file, std_name, var_names, timestep, buffer, mark_as_read, error_on_not_found, var_found)
      use ccpp_kinds,           only: kind_phys
      use shr_assert_mod,       only: shr_assert_in_domain
      use shr_sys_mod,          only: shr_sys_flush
      use shr_kind_mod,         only: cl=>shr_kind_cl
      use pio,                  only: file_desc_t, var_desc_t
      use spmd_utils,           only: masterproc
      use string_utils,         only: stringify
      use cam_pio_utils,        only: cam_pio_find_var
      use cam_abortutils,       only: endrun
      use cam_logfile,          only: iulog
      use cam_logfile,          only: debug_output
      use cam_logfile,          only: DEBUGOUT_INFO
      use cam_field_read,       only: cam_read_field
      use phys_vars_init_check, only: mark_as_read_from_file

      !Max possible length of variable name in input (IC) file:
      use phys_vars_init_check, only: std_name_len

      ! Dummy arguments
      type(file_desc_t), intent(inout) :: file
      character(len=*),  intent(in)    :: std_name     ! Standard name
      character(len=*),  intent(in)    :: var_names(:) ! var name on file
      integer,           intent(in)    :: timestep
      real(kind_phys),   intent(inout) :: buffer(:)
      logical, optional, intent(in)    :: mark_as_read       ! Mark field as read if found
      logical, optional, intent(in)    :: error_on_not_found ! Flag to error and exit if not found
      logical, optional, intent(out)   :: var_found          ! Flag to mark variable was found
      ! Local variables
      logical                          :: mark_as_read_local
      logical                          :: error_on_not_found_local
      logical                          :: var_found_local
      character(len=std_name_len)      :: found_name
      type(var_desc_t)                 :: vardesc
      character(len=*), parameter      :: subname = 'read_field_2d: '
      character(len=cl)                :: strerr

      if (present(mark_as_read)) then
         mark_as_read_local = mark_as_read
      else
         mark_as_read_local = .true.
      end if

      if (present(error_on_not_found)) then
         error_on_not_found_local = error_on_not_found
      else
         error_on_not_found_local = .true.
      end if

      var_found_local = .false.
      call cam_pio_find_var(file, var_names, found_name, vardesc, var_found_local)
      if (.not. var_found_local) then
          call cam_pio_find_var(file, [std_name], found_name, vardesc, var_found_local)
      end if

      if (var_found_local) then
         if (masterproc) then
            write(iulog, *) 'Reading input field, ', trim(found_name)
            call shr_sys_flush(iulog)
         end if
         call cam_read_field(found_name, file, buffer, var_found_local,             &
              timelevel=timestep)

         if (mark_as_read_local) then
            call mark_as_read_from_file(std_name)
         end if

         if (var_found_local) then
            call shr_assert_in_domain(buffer, is_nan=.false.,                    &
                 varname=trim(found_name),                                       &
                 msg=subname//'NaN found in '//trim(found_name))
         else
            write(strerr,*) subname//'Unable to properly check the found variable "', trim(found_name), '" in the IC file. &
                           &Please double-check if the variable exists in the file and that the file is not corrupted or damaged.'
            call endrun(strerr)
         end if
      else if (.not. error_on_not_found_local) then
         if (masterproc .and. (debug_output >= DEBUGOUT_INFO)) then
            write(iulog, *) trim(std_name), ' not found, also looked for: [', stringify(var_names), ']'
            call shr_sys_flush(iulog)
         end if
      else
         call endrun(subname//'No variable found in ['//stringify(var_names)//']')
      end if
      if (present(var_found)) then
         var_found = var_found_local
     end if
   end subroutine read_field_2d

   subroutine read_field_3d(file, std_name, var_names, vcoord_name,           &
        timestep, buffer, mark_as_read, error_on_not_found, var_found)
      use ccpp_kinds,           only: kind_phys
      use shr_assert_mod,       only: shr_assert_in_domain
      use shr_sys_mod,          only: shr_sys_flush
      use shr_kind_mod,         only: cl=>shr_kind_cl
      use pio,                  only: file_desc_t, var_desc_t
      use spmd_utils,           only: masterproc
      use string_utils,         only: stringify
      use cam_pio_utils,        only: cam_pio_find_var
      use cam_abortutils,       only: endrun
      use cam_logfile,          only: iulog
      use cam_logfile,          only: debug_output
      use cam_logfile,          only: DEBUGOUT_INFO
      use cam_field_read,       only: cam_read_field
      use vert_coord,           only: pver, pverp
      use phys_vars_init_check, only: mark_as_read_from_file

      !Max possible length of variable name in input (IC) file:
      use phys_vars_init_check, only: std_name_len

      ! Dummy arguments
      type(file_desc_t), intent(inout) :: file
      character(len=*),  intent(in)    :: std_name     ! Standard name
      character(len=*),  intent(in)    :: var_names(:) ! var name on file
      character(len=*),  intent(in)    :: vcoord_name
      integer,           intent(in)    :: timestep
      real(kind_phys),   intent(inout) :: buffer(:,:)
      logical, optional, intent(in)    :: mark_as_read       ! Mark field as read if found
      logical, optional, intent(in)    :: error_on_not_found ! Flag to error and exit if not found
      logical, optional, intent(out)   :: var_found          ! Flag to mark variable was found
      ! Local variables
      logical                          :: mark_as_read_local
      logical                          :: error_on_not_found_local
      logical                          :: var_found_local
      integer                          :: num_levs
      character(len=std_name_len)      :: found_name
      type(var_desc_t)                 :: vardesc
      character(len=*), parameter      :: subname = 'read_field_3d: '
      character(len=cl)                :: strerr

      if (present(mark_as_read)) then
         mark_as_read_local = mark_as_read
      else
         mark_as_read_local = .true.
      end if

      if (present(error_on_not_found)) then
         error_on_not_found_local = error_on_not_found
      else
         error_on_not_found_local = .true.
      end if

      var_found_local = .false.
      call cam_pio_find_var(file, var_names, found_name, vardesc, var_found_local)

      if (.not. var_found_local) then
         call cam_pio_find_var(file, [std_name], found_name, vardesc, var_found_local)
      end if

      if (var_found_local) then
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
         call cam_read_field(found_name, file, buffer, var_found_local,             &
              timelevel=timestep, dim3name=trim(vcoord_name),                 &
              dim3_bnds=[1, num_levs])

         if (mark_as_read_local) then
            call mark_as_read_from_file(std_name)
         end if

         if (var_found_local) then
            call shr_assert_in_domain(buffer, is_nan=.false.,                    &
                 varname=trim(found_name),                                       &
                 msg=subname//'NaN found in '//trim(found_name))
         else
            write(strerr,*) subname//'Unable to properly check the found variable "', trim(found_name), '" in the IC file. &
                           &Please double-check if the variable exists in the file and that the file is not corrupted or damaged.'
            call endrun(strerr)
         end if
      else if (.not. error_on_not_found_local) then
         if (masterproc .and. (debug_output >= DEBUGOUT_INFO)) then
            write(iulog, *) trim(std_name), ' not found, also looked for: [', stringify(var_names), ']'
            call shr_sys_flush(iulog)
         end if
      else
         call endrun(subname//'No variable found in ['//stringify(var_names)//']')
      end if
      if (present(var_found)) then
         var_found = var_found_local
      end if
   end subroutine read_field_3d

   subroutine read_field_4d(file, std_name, var_names, vcoord_name,           &
        timestep, buffer, mark_as_read, error_on_not_found, var_found)
      use ccpp_kinds,           only: kind_phys
      use shr_assert_mod,       only: shr_assert_in_domain
      use shr_sys_mod,          only: shr_sys_flush
      use shr_kind_mod,         only: cl=>shr_kind_cl
      use pio,                  only: file_desc_t, var_desc_t
      use spmd_utils,           only: masterproc
      use string_utils,         only: stringify
      use cam_pio_utils,        only: cam_pio_find_var
      use cam_abortutils,       only: endrun
      use cam_logfile,          only: iulog
      use cam_logfile,          only: debug_output
      use cam_logfile,          only: DEBUGOUT_INFO
      use cam_field_read,       only: cam_read_field
      use vert_coord,           only: pver, pverp
      use phys_vars_init_check, only: mark_as_read_from_file

      !Max possible length of variable name in input (IC) file:
      use phys_vars_init_check, only: std_name_len

      ! Dummy arguments
      type(file_desc_t), intent(inout) :: file
      character(len=*),  intent(in)    :: std_name     ! Standard name
      character(len=*),  intent(in)    :: var_names(:) ! var name on file
      character(len=*),  intent(in)    :: vcoord_name
      integer,           intent(in)    :: timestep
      real(kind_phys),   intent(inout) :: buffer(:,:,:)
      logical, optional, intent(in)    :: mark_as_read       ! Mark field as read if found
      logical, optional, intent(in)    :: error_on_not_found ! Flag to error and exit if not found
      logical, optional, intent(out)   :: var_found          ! Flag to mark variable was found
      ! Local variables
      logical                          :: mark_as_read_local
      logical                          :: error_on_not_found_local
      logical                          :: var_found_local
      integer                          :: num_levs
      character(len=std_name_len)      :: found_name
      type(var_desc_t)                 :: vardesc
      character(len=*), parameter      :: subname = 'read_field_4d: '
      character(len=cl)                :: strerr

      if (present(mark_as_read)) then
         mark_as_read_local = mark_as_read
      else
         mark_as_read_local = .true.
      end if

      if (present(error_on_not_found)) then
         error_on_not_found_local = error_on_not_found
      else
         error_on_not_found_local = .true.
      end if

      var_found_local = .false.
      call cam_pio_find_var(file, var_names, found_name, vardesc, var_found_local)

      if (.not. var_found_local) then
         call cam_pio_find_var(file, [std_name], found_name, vardesc, var_found_local)
      end if

      if (var_found_local) then
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

         !Note:  The "dim3" inputs are specifically for the vertical
         !dimension, not a generic third dimension:
         call cam_read_field(found_name, file, buffer, var_found_local,  &
              timelevel=timestep, dim3name=trim(vcoord_name),            &
              dim3_bnds=[1, num_levs], dim3_pos=2,                       &
              dim4_bnds=[1, size(buffer, 3)])


         if (mark_as_read_local) then
            call mark_as_read_from_file(std_name)
         end if

         if (var_found_local) then
            call shr_assert_in_domain(buffer, is_nan=.false.,             &
                 varname=trim(found_name),                                &
                 msg=subname//'NaN found in '//trim(found_name))
         else
            write(strerr,*) subname//'Unable to properly check the found variable "', trim(found_name), '" in the IC file. &
                           &Please double-check if the variable exists in the file and that the file is not corrupted or damaged.'
            call endrun(strerr)
         end if
      else if (.not. error_on_not_found_local) then
         if (masterproc .and. (debug_output >= DEBUGOUT_INFO)) then
            write(iulog, *) trim(std_name), ' not found, also looked for: [', stringify(var_names), ']'
            call shr_sys_flush(iulog)
         end if
      else
         call endrun(subname//'No variable found in ['//stringify(var_names)//']')
      end if
      if (present(var_found)) then
         var_found = var_found_local
      end if
   end subroutine read_field_4d

   subroutine read_constituent_dimensioned_field_2d(const_props, file, std_name, base_var_names, timestep, field_array, error_on_not_found)
      use ccpp_kinds,           only: kind_phys
      use shr_assert_mod,       only: shr_assert_in_domain
      use shr_sys_mod,          only: shr_sys_flush
      use pio,                  only: file_desc_t, var_desc_t
      use spmd_utils,           only: masterproc
      use cam_pio_utils,        only: cam_pio_find_var
      use cam_abortutils,       only: endrun, check_allocate
      use cam_logfile,          only: iulog
      use cam_field_read,       only: cam_read_field
      use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t
      use phys_vars_init_check, only: mark_as_read_from_file
      use phys_vars_init_check, only: phys_var_stdnames, input_var_names, phys_var_num

      ! Dummy arguments
      type(ccpp_constituent_prop_ptr_t),     intent(in)    :: const_props(:)      ! Constituent properties
      type(file_desc_t),                     intent(inout) :: file   !Parallel I/O (PIO) file type.
      character(len=*),  intent(in)                        :: std_name            ! Standard name of base variable.
      character(len=*),                      intent(in)    :: base_var_names(:)   ! "Base" name(s) used to construct variable name (base_constname)
      integer,                               intent(in)    :: timestep            ! Timestep to read [count]
      real(kind_phys),                       intent(inout) :: field_array(:,:)    ! Output field array (ncol, pcnst)
      logical, optional,                     intent(in)    :: error_on_not_found  ! Flag to error and exit if not found

      ! Local variables
      logical                          :: var_found
      character(len=128)               :: constituent_name
      character(len=256)               :: file_var_name
      character(len=256)               :: found_name
      character(len=512)               :: missing_vars
      type(var_desc_t)                 :: vardesc
      real(kind_phys), allocatable     :: buffer(:)
      integer                          :: const_idx, base_idx
      integer                          :: ierr
      logical                          :: error_on_not_found_local
      logical                          :: any_missing

      ! For construction of constituent short name mapping
      character(len=128), allocatable  :: constituent_short_names(:)
      character(len=128)               :: constituent_std_name
      integer                          :: n
      integer                          :: const_input_idx

      character(len=256)               :: errmsg

      character(len=*), parameter      :: subname = 'read_constituent_dimensioned_field: '

      if (present(error_on_not_found)) then
         error_on_not_found_local = error_on_not_found
      else
         error_on_not_found_local = .true.
      end if

      ! Initialize tracking variables
      any_missing = .false.
      missing_vars = ''

      ! Allocate temporary buffer
      allocate(buffer(size(field_array, 1)), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'buffer', errmsg=errmsg)

      !REMOVECAM:
      ! Because the constituent properties pointer contains standard names, and not input constituent names
      ! (e.g., Q, CLDLIQ, ...) which are used in the input file names,
      ! we have to construct a mapping of the standard names to the short input IC file names
      ! When CAM is retired and only standard names are used for constituents, this mapping can be removed.
      allocate(constituent_short_names(size(const_props)), stat=ierr, errmsg=errmsg)
      call check_allocate(ierr, subname, 'constituent_short_names', errmsg=errmsg)

      const_shortmap_loop: do const_idx = 1, size(const_props)
         ! Get constituent standard name.
         call const_props(const_idx)%standard_name(constituent_std_name)

         ! Check if constituent standard name is in the registry to look up its IC name
         ! n.b. this assumes that the first IC name specified in the registry for this constituent
         ! is the short name
         const_input_idx = -1
         phys_inputvar_loop: do n = 1, phys_var_num
            if (trim(phys_var_stdnames(n)) == trim(constituent_std_name)) then
               const_input_idx = n
               exit phys_inputvar_loop
            end if
         end do phys_inputvar_loop

         if (const_input_idx > 0) then
            ! Use the first entry from the input_var_names -- assumed to be short name.
            constituent_short_names(const_idx) = trim(input_var_names(1, const_input_idx))
         else
            ! Use the standard name itself if not found in registry.
            constituent_short_names(const_idx) = trim(constituent_std_name)
         end if
      end do const_shortmap_loop
      !END REMOVECAM

      ! Loop through all possible base names to find correct base name.
      ! Note this assumes that the same base name is used for all constituents.
      ! i.e., there cannot be something like cam_in_cflx_Q & cflx_CLDLIQ in one file.
      base_idx_loop: do base_idx = 1, size(base_var_names)
         ! Loop through all constituents
         const_idx_loop: do const_idx = 1, size(const_props)
            ! Get constituent short name
            constituent_name = constituent_short_names(const_idx)

            ! Create file variable name: <base_var_name>_<constituent_name>
            file_var_name = trim(base_var_names(base_idx)) // '_' // trim(constituent_name)

            ! Try to find variable in file
            var_found = .false.
            call cam_pio_find_var(file, [file_var_name], found_name, vardesc, var_found)

            if(var_found) then
               exit base_idx_loop
            endif
         end do const_idx_loop
      end do base_idx_loop

      if(.not. var_found) then
         if(error_on_not_found_local) then
            !End model run with appropriate error message:
            call endrun(subname // 'Required constituent-dimensioned variables not found: No match for ' // trim(std_name))
         else
            !Write message to log file, then exit subroutine:
            if (masterproc) then
               write(iulog, *) subname // 'Required constituent-dimensioned variables not found: No match for ' // trim(std_name)
               call shr_sys_flush(iulog)
            end if
            return !Nothing more to do here
         end if
      end if

      ! Once base_idx is identified, use it in the actual constituent loop:
      const_read_loop: do const_idx = 1, size(const_props)
         ! Get constituent short name
         constituent_name = constituent_short_names(const_idx)

         ! Create file variable name: <base_var_name>_<constituent_name>
         file_var_name = trim(base_var_names(base_idx)) // '_' // trim(constituent_name)

         ! Try to find variable in file
         var_found = .false.
         call cam_pio_find_var(file, [file_var_name], found_name, vardesc, var_found)

         if (var_found) then
            ! Read the variable
            if (masterproc) then
               write(iulog, *) 'Reading constituent-dimensioned input field, ', trim(found_name)
               call shr_sys_flush(iulog)
            end if

            call cam_read_field(found_name, file, buffer, var_found, timelevel=timestep)

            if (var_found) then
               ! Copy to correct constituent index in field array
               field_array(:, const_idx) = buffer(:)

               ! Check for NaN values
               call shr_assert_in_domain(field_array(:, const_idx), is_nan=.false., &
                    varname=trim(found_name), &
                    msg=subname//'NaN found in '//trim(found_name))
            else
               ! Failed to read even though variable was found
               any_missing = .true.
               if (len_trim(missing_vars) > 0) then
                  missing_vars = trim(missing_vars) // ', ' // trim(file_var_name)
               else
                  missing_vars = trim(file_var_name)
               end if
            end if
         else
            ! Variable not found in file
            any_missing = .true.
            if (len_trim(missing_vars) > 0) then
               missing_vars = trim(missing_vars) // ', ' // trim(file_var_name)
            else
               missing_vars = trim(file_var_name)
            end if

            if (.not. error_on_not_found_local) then
               ! Use default value (already set at initialization)

               if (masterproc) then
                  write(iulog, *) 'Constituent-dimensioned field ', trim(file_var_name), &
                                 ' not found, using default value for constituent ', trim(constituent_name)
                  call shr_sys_flush(iulog)
               end if
            end if
         end if
      end do const_read_loop

      ! Check if we should fail due to missing variables
      if (any_missing .and. error_on_not_found_local) then
         call endrun(subname//'Required constituent-dimensioned variables not found: ' // trim(missing_vars) // &
                     'Make sure the constituent short name is the first in the <ic_file_input_names> list in the registry.')
      end if

      ! Mark the base variable as read from file (only if no errors)
      call mark_as_read_from_file(std_name)

      ! Clean up
      deallocate(constituent_short_names)
      deallocate(buffer)

   end subroutine read_constituent_dimensioned_field_2d

   subroutine check_field_2d(file, var_names, timestep, current_value,        &
      stdname, min_difference, min_relative_value, is_first, diff_found)
      use ccpp_kinds,     only: kind_phys
      use pio,            only: file_desc_t, var_desc_t
      use spmd_utils,     only: masterproc, masterprocid
      use spmd_utils,     only: mpicom, iam
      use cam_pio_utils,  only: cam_pio_find_var
      use cam_abortutils, only: endrun, check_allocate
      use cam_field_read, only: cam_read_field
      use mpi,            only: mpi_maxloc, mpi_sum, mpi_status_size
      use mpi,            only: mpi_2double_precision, mpi_integer

      !Max possible length of variable name in file:
      use phys_vars_init_check, only: std_name_len

      !Dummy arguments:
      real(kind_phys),   intent(in)    :: current_value(:)
      type(file_desc_t), intent(inout) :: file
      character(len=*),  intent(in)    :: var_names(:)
      integer,           intent(in)    :: timestep
      character(len=*),  intent(in)    :: stdname
      real(kind_phys),   intent(in)    :: min_difference
      real(kind_phys),   intent(in)    :: min_relative_value
      logical,           intent(inout) :: is_first
      logical,           intent(out)   :: diff_found

      !Local variables:
      logical                          :: var_found
      character(len=std_name_len)      :: found_name
      type(var_desc_t)                 :: vardesc
      character(len=*),  parameter     :: subname = 'check_field_2d'
      real(kind_phys)                  :: diff
      integer                          :: col
      integer                          :: ierr      !For MPI
      integer                          :: mpi_stat(mpi_status_size) !For MPI
      real(kind_phys), allocatable     :: buffer(:)
      integer                          :: diff_count
      real(kind_phys)                  :: max_diff(2)    !Stores the local max diff and its MPI rank
      integer                          :: max_diff_col
      real(kind_phys)                  :: max_diff_gl(2) !Stores the global max diff and its MPI rank
      integer                          :: max_diff_gl_col
      integer                          :: diff_count_gl

      !Initialize output variables
      ierr = 0
      allocate(buffer(size(current_value)), stat=ierr)
      call check_allocate(ierr, subname, 'buffer')
      max_diff_col  = 0
      diff_count    = 0
      diff          = 0._kind_phys
      max_diff(1) = 0._kind_phys
      max_diff(2) = real(iam, kind_phys) !MPI rank for this task
      diff_found = .false.

      call cam_pio_find_var(file, var_names, found_name, vardesc, var_found)
      if (.not. var_found) then
         !Try searching again using the variable standard name:
         call cam_pio_find_var(file, [stdname], found_name, vardesc, var_found)
      end if

      if (var_found) then
         call cam_read_field(found_name, file, buffer, var_found,             &
              timelevel=timestep, log_output=.false.)
         if (var_found) then
            do col = 1, size(buffer)
               if (abs(current_value(col)) < min_relative_value) then
                  !Calculate absolute difference:
                  diff = abs(current_value(col) - buffer(col))
               else
                  !Calculate relative difference:
                  diff = abs(current_value(col) - buffer(col)) /              &
                     abs(current_value(col))
               end if
               if (diff > max_diff(1)) then
                  max_diff(1)  = diff
                  max_diff_col = col
               end if
               if (diff > min_difference) then
                  diff_count = diff_count + 1
               end if
            end do
            !Gather results across all nodes to get global values
            call mpi_reduce(diff_count, diff_count_gl, 1, mpi_integer,        &
                            mpi_sum, masterprocid,  mpicom, ierr)

            call mpi_allreduce(max_diff, max_diff_gl, 1,                      &
                               MPI_2DOUBLE_PRECISION,                         &
                               mpi_maxloc, mpicom, ierr)

            if (iam == int(max_diff_gl(2)) .and. .not. masterproc) then
               !The largest diff happened on this task, so the local max is
               !the global max. So send the local max value's dimension
               !index (usually column index) to the root task:
               call mpi_send(max_diff_col, 1, mpi_integer, masterprocid, 0,   &
                             mpicom, ierr)
            else if (iam /= int(max_diff_gl(2)) .and. masterproc) then
               !The root task needs to receive the relevant max diff index
               !from a different task:
               call mpi_recv(max_diff_gl_col, 1, mpi_integer,                 &
                             int(max_diff_gl(2)), 0, mpicom,                &
                             mpi_stat, ierr)
            else if (masterprocid == int(max_diff_gl(2))) then
               !The biggest difference is on the root MPI task already,
               !so just set directly:
               max_diff_gl_col = max_diff_col
            end if

            !Print difference stats to log file
            if (masterproc) then
               if (diff_count_gl > 0) then
                  call write_check_field_entry(stdname, diff_count_gl,        &
                                               max_diff_gl(1),              &
                                               int(max_diff_gl(2)),         &
                                               max_diff_gl_col, is_first)
                  is_first = .false.
                  diff_found = .true.
               end if
            end if
         end if
      end if
      deallocate(buffer)
   end subroutine check_field_2d

   subroutine check_field_3d(file, var_names, vcoord_name, timestep,          &
      current_value, stdname, min_difference, min_relative_value, is_first,   &
      diff_found)
      use ccpp_kinds,     only: kind_phys
      use shr_sys_mod,    only: shr_sys_flush
      use pio,            only: file_desc_t, var_desc_t
      use spmd_utils,     only: masterproc, masterprocid
      use spmd_utils,     only: mpicom, iam
      use cam_pio_utils,  only: cam_pio_find_var
      use cam_abortutils, only: endrun, check_allocate
      use cam_field_read, only: cam_read_field
      use mpi,            only: mpi_maxloc, mpi_sum, mpi_status_size
      use mpi,            only: mpi_2double_precision, mpi_integer
      use vert_coord,     only: pver, pverp

      !Max possible length of variable name in file:
      use phys_vars_init_check, only: std_name_len

      !Dummy arguments:
      real(kind_phys),   intent(in)    :: current_value(:,:)
      type(file_desc_t), intent(inout) :: file
      character(len=*),  intent(in)    :: var_names(:)
      integer,           intent(in)    :: timestep
      character(len=*),  intent(in)    :: vcoord_name
      character(len=*),  intent(in)    :: stdname
      real(kind_phys),   intent(in)    :: min_difference
      real(kind_phys),   intent(in)    :: min_relative_value
      logical,           intent(inout) :: is_first
      logical,           intent(out)   :: diff_found

      !Local variables:
      logical                          :: var_found = .true.
      character(len=std_name_len)      :: found_name
      type(var_desc_t)                 :: vardesc
      character(len=*),  parameter     :: subname = 'check_field_3d'
      real(kind_phys)                  :: diff
      integer                          :: ierr                      !For MPI
      integer                          :: mpi_stat(mpi_status_size) !For MPI
      integer                          :: num_levs
      integer                          :: col
      integer                          :: lev
      real(kind_phys), allocatable     :: buffer(:,:)
      integer                          :: diff_count
      real(kind_phys)                  :: max_diff(2)    !Stores the local max diff and its MPI rank
      integer                          :: max_diff_col
      integer                          :: max_diff_lev
      real(kind_phys)                  :: max_diff_gl(2) !Stores the global max diff and its MPI rank
      integer                          :: max_diff_gl_col
      integer                          :: max_diff_gl_lev
      integer                          :: diff_count_gl

      !Initialize output variables
      ierr = 0
      allocate(buffer(size(current_value, 1), size(current_value, 2)),        &
        stat=ierr)
      call check_allocate(ierr, subname, 'buffer')
      max_diff_col  = 0
      max_diff_lev  = 0
      diff_count    = 0
      diff          = 0._kind_phys
      max_diff(1)   = 0._kind_phys
      max_diff(2)   = real(iam, kind_phys) !MPI rank for this task
      diff_found = .false.

      call cam_pio_find_var(file, var_names, found_name, vardesc, var_found)
      if (.not. var_found) then
         !Try searching again using the variable standard name:
         call cam_pio_find_var(file, [stdname], found_name, vardesc, var_found)
      end if

      if (var_found) then
         if (trim(vcoord_name) == 'lev') then
            num_levs = pver
         else if (trim(vcoord_name) == 'ilev') then
            num_levs = pverp
         else
            call endrun(subname//'Unknown vcoord_name, '//trim(vcoord_name))
         end if
         call cam_read_field(found_name, file, buffer, var_found,             &
              timelevel=timestep, dim3name=trim(vcoord_name),                 &
              dim3_bnds=[1, num_levs], log_output=.false.)
         if (var_found) then
            do lev = 1, num_levs
               do col = 1, size(buffer(:,lev))
                  if (abs(current_value(col, lev)) < min_relative_value) then
                     !Calculate absolute difference:
                     diff = abs(current_value(col, lev) - buffer(col, lev))
                  else
                     !Calculate relative difference:
                     diff = abs(current_value(col, lev) - buffer(col, lev)) / &
                        abs(current_value(col, lev))
                  end if
                  if (diff > max_diff(1)) then
                     max_diff(1) = diff
                     max_diff_col = col
                     max_diff_lev = lev
                  end if
                  !Determine if diff is large enough to be considered a "hit"
                  if (diff > min_difference) then
                     diff_count = diff_count + 1
                  end if
               end do
            end do

            !Make relevant MPI calls to get global values:
            call mpi_reduce(diff_count, diff_count_gl, 1, mpi_integer,        &
                 mpi_sum, masterprocid, mpicom, ierr)

            call mpi_allreduce(max_diff, max_diff_gl, 1,                      &
                               MPI_2DOUBLE_PRECISION,                         &
                               mpi_maxloc, mpicom, ierr)

            if (iam == int(max_diff_gl(2)) .and. .not. masterproc) then
               !The largest diff happened on this task, so the local max is
               !the global max. So send the local max value's dimension
               !indices (usually column and level index) to the root task:
               call mpi_send(max_diff_col, 1, mpi_integer, masterprocid, 0,   &
                             mpicom, ierr)
               call mpi_send(max_diff_lev, 1, mpi_integer, masterprocid, 0,   &
                             mpicom, ierr)
            else if (iam /= int(max_diff_gl(2)) .and. masterproc) then
               !The root task needs to receive the relevant max diff indices
               !from a different task:
               call mpi_recv(max_diff_gl_col, 1, mpi_integer,                 &
                             int(max_diff_gl(2)), 0, mpicom,                &
                             mpi_stat, ierr)
               call mpi_recv(max_diff_gl_lev, 1, mpi_integer,                 &
                             int(max_diff_gl(2)), 0, mpicom,                &
                             mpi_stat, ierr)
            else if (masterprocid == int(max_diff_gl(2))) then
               !The biggest difference is on the root MPI task already, so just
               !set directly:
               max_diff_gl_col = max_diff_col
               max_diff_gl_lev = max_diff_lev
            end if

            !Print difference stats to log file
            if (masterproc) then
               if (diff_count_gl > 0) then
                  call write_check_field_entry(stdname, diff_count_gl,        &
                                               max_diff_gl(1),              &
                                               int(max_diff_gl(2)),         &
                                               max_diff_gl_col,               &
                                               is_first,                      &
                                               max_diff_lev=max_diff_gl_lev)
                  is_first = .false.
                  diff_found = .true.
               end if
            end if
         end if
      end if
      deallocate(buffer)

   end subroutine check_field_3d

   subroutine check_field_4d(file, var_names, vcoord_name, timestep,          &
      current_value, stdname, min_difference, min_relative_value, is_first,   &
      diff_found)
      use ccpp_kinds,     only: kind_phys
      use shr_sys_mod,    only: shr_sys_flush
      use pio,            only: file_desc_t, var_desc_t
      use spmd_utils,     only: masterproc, masterprocid
      use spmd_utils,     only: mpicom, iam
      use cam_pio_utils,  only: cam_pio_find_var
      use cam_abortutils, only: endrun, check_allocate
      use cam_field_read, only: cam_read_field
      use mpi,            only: mpi_maxloc, mpi_sum, mpi_status_size
      use mpi,            only: mpi_2double_precision, mpi_integer
      use vert_coord,     only: pver, pverp

      !Max possible length of variable name in file:
      use phys_vars_init_check, only: std_name_len

      !Dummy arguments:
      real(kind_phys),   intent(in)    :: current_value(:,:,:)
      type(file_desc_t), intent(inout) :: file
      character(len=*),  intent(in)    :: var_names(:)
      integer,           intent(in)    :: timestep
      character(len=*),  intent(in)    :: vcoord_name
      character(len=*),  intent(in)    :: stdname
      real(kind_phys),   intent(in)    :: min_difference
      real(kind_phys),   intent(in)    :: min_relative_value
      logical,           intent(inout) :: is_first
      logical,           intent(out)   :: diff_found

      !Local variables:
      logical                          :: var_found = .true.
      character(len=std_name_len)      :: found_name
      type(var_desc_t)                 :: vardesc
      character(len=*),  parameter     :: subname = 'check_field_4d'
      real(kind_phys)                  :: diff
      integer                          :: ierr                      !For MPI
      integer                          :: mpi_stat(mpi_status_size) !For MPI
      integer                          :: num_levs
      integer                          :: col
      integer                          :: lev
      integer                          :: extra_dim
      real(kind_phys), allocatable     :: buffer(:,:,:)
      integer                          :: diff_count
      real(kind_phys)                  :: max_diff(2)    !Stores the local max diff and its MPI rank
      integer                          :: max_diff_col
      integer                          :: max_diff_lev
      integer                          :: max_diff_extra_dim
      real(kind_phys)                  :: max_diff_gl(2) !Stores the global max diff and its MPI rank
      integer                          :: max_diff_gl_col
      integer                          :: max_diff_gl_lev
      integer                          :: max_diff_gl_extra_dim
      integer                          :: diff_count_gl

      !Initialize output variables
      ierr = 0
      allocate(buffer(size(current_value, 1), size(current_value, 2),         &
                      size(current_value, 3)),  stat=ierr)
      call check_allocate(ierr, subname, 'buffer')
      max_diff_col       = 0
      max_diff_lev       = 0
      max_diff_extra_dim = 0
      diff_count         = 0
      diff               = 0._kind_phys
      max_diff(1)        = 0._kind_phys
      max_diff(2)        = real(iam, kind_phys) !MPI rank for this task
      diff_found         = .false.

      call cam_pio_find_var(file, var_names, found_name, vardesc, var_found)
      if (.not. var_found) then
         !Try searching again using the variable standard name:
         call cam_pio_find_var(file, [stdname], found_name, vardesc, var_found)
      end if

      if (var_found) then
         if (trim(vcoord_name) == 'lev') then
            num_levs = pver
         else if (trim(vcoord_name) == 'ilev') then
            num_levs = pverp
         else
            call endrun(subname//'Unknown vcoord_name, '//trim(vcoord_name))
         end if
         !Note:  The "dim3" inputs are specifically for the vertical
         !dimension, not a generic third dimension:
         call cam_read_field(found_name, file, buffer, var_found,             &
              timelevel=timestep, dim3name=trim(vcoord_name),                 &
              dim3_bnds=[1, num_levs], dim3_pos=2,                            &
              dim4_bnds=[1, size(buffer, 3)],log_output=.false.)
         if (var_found) then
            do extra_dim = 1, size(buffer, 3)
               do lev = 1, num_levs
                  do col = 1, size(buffer(:,lev,extra_dim))
                     if (abs(current_value(col, lev, extra_dim)) < min_relative_value) then
                        !Calculate absolute difference:
                        diff = abs(current_value(col, lev, extra_dim) - buffer(col, lev, extra_dim))
                     else
                        !Calculate relative difference:
                        diff = abs(current_value(col, lev, extra_dim) - buffer(col, lev, extra_dim)) / &
                           abs(current_value(col, lev, extra_dim))
                     end if
                     if (diff > max_diff(1)) then
                        max_diff(1) = diff
                        max_diff_col = col
                        max_diff_lev = lev
                        max_diff_extra_dim = extra_dim
                     end if
                     !Determine if diff is large enough to be considered a "hit"
                     if (diff > min_difference) then
                        diff_count = diff_count + 1
                     end if
                  end do
               end do
            end do

            !Make relevant MPI calls to get global values:
            call mpi_reduce(diff_count, diff_count_gl, 1, mpi_integer,        &
                 mpi_sum, masterprocid, mpicom, ierr)

            call mpi_allreduce(max_diff, max_diff_gl, 1,                      &
                               MPI_2DOUBLE_PRECISION,                         &
                               mpi_maxloc, mpicom, ierr)

            if (iam == int(max_diff_gl(2)) .and. .not. masterproc) then
               !The largest diff happened on this task, so the local max is
               !the global max. So send the local max value's dimension
               !indices (column, level, and extra dimension index) to the root task:
               call mpi_send(max_diff_col, 1, mpi_integer, masterprocid, 0,    &
                             mpicom, ierr)
               call mpi_send(max_diff_lev, 1, mpi_integer, masterprocid, 0,    &
                             mpicom, ierr)
               call mpi_send(max_diff_extra_dim, 1, mpi_integer, masterprocid, &
                             0, mpicom, ierr)
            else if (iam /= int(max_diff_gl(2)) .and. masterproc) then
               !The root task needs to receive the relevant max diff indices
               !from a different task:
               call mpi_recv(max_diff_gl_col, 1, mpi_integer,                  &
                             int(max_diff_gl(2)), 0, mpicom,                   &
                             mpi_stat, ierr)
               call mpi_recv(max_diff_gl_lev, 1, mpi_integer,                  &
                             int(max_diff_gl(2)), 0, mpicom,                   &
                             mpi_stat, ierr)
               call mpi_recv(max_diff_gl_extra_dim, 1, mpi_integer,            &
                             int(max_diff_gl(2)), 0, mpicom,                   &
                             mpi_stat, ierr)
            else if (masterprocid == int(max_diff_gl(2))) then
               !The biggest difference is on the root MPI task already, so just
               !set directly:
               max_diff_gl_col       = max_diff_col
               max_diff_gl_lev       = max_diff_lev
               max_diff_gl_extra_dim = max_diff_extra_dim
            end if

            !Print difference stats to log file
            if (masterproc) then
               if (diff_count_gl > 0) then
                  call write_check_field_entry(stdname, diff_count_gl,        &
                                               max_diff_gl(1),                &
                                               int(max_diff_gl(2)),           &
                                               max_diff_gl_col,               &
                                               is_first,                      &
                                               max_diff_lev=max_diff_gl_lev,  &
                                               max_diff_extra_dim=max_diff_gl_extra_dim)
                  is_first = .false.
                  diff_found = .true.
               end if
            end if
         end if
      end if
      deallocate(buffer)

   end subroutine check_field_4d

   subroutine write_check_field_entry(stdname,      diff_count,               &
                                      max_diff,     max_diff_rank,            &
                                      max_diff_col, is_first,                 &
                                      max_diff_lev, max_diff_extra_dim)

      use ccpp_kinds,   only: kind_phys
      use cam_logfile,  only: iulog
      use shr_kind_mod, only: cs=>shr_kind_cs

      !Dummy variables:
      character(len=*),  intent(in) :: stdname
      integer,           intent(in) :: diff_count
      real(kind_phys),   intent(in) :: max_diff
      integer,           intent(in) :: max_diff_rank      !MPI rank max diff occurred on
      integer,           intent(in) :: max_diff_col       !max diff column (1st) dimension value
      integer, optional, intent(in) :: max_diff_lev       !max diff level (2nd) dimension value
      integer, optional, intent(in) :: max_diff_extra_dim !max diff extra (3rd) dimension value
      logical,           intent(in) :: is_first

      !Local variables:
      character(len=cs)            :: fmt_str
      character(len=cs)            :: index_str
      integer                      :: slen
      integer, parameter           :: indent_level = 50

      slen = len_trim(stdname)

      !Write check_field log header:
      if (is_first) then
         write(iulog, *) ''
         write(fmt_str, '(a,i0,a)') "(1x,a,t",indent_level+1,",1x,a,2x,a,3x,a)"
         write(iulog, fmt_str) 'Variable', '# Diffs', 'Max Diff', 'Max Diff loc (rank, col, lev, extra_dim)'
         write(iulog, fmt_str) '--------', '-------', '--------', '-----------------------------'
      end if

      !Write standard name separately if longer
      !than the indent level:
      if (slen > indent_level) then
         write(iulog, '(a)') trim(stdname)
         slen = 0
      end if

      !Write out difference and index valuesa:
      if (present(max_diff_lev)) then
         if(present(max_diff_extra_dim)) then
             write(index_str, '(a,i0,a,i0,a,i0,a,i0,a)') "(",max_diff_rank,",",max_diff_col,",",max_diff_lev,",",max_diff_extra_dim,")"
         else
             write(index_str, '(a,i0,a,i0,a,i0,a)') "(",max_diff_rank,",",max_diff_col,",",max_diff_lev,")"
         end if
      else
         write(index_str, '(a,i0,a,i0,a)') "(",max_diff_rank,",",max_diff_col,")"
      end if
      write(fmt_str, '(a,i0,a)') "(1x,a,t",indent_level+1,",1x,i7,2x,e8.2,3x,a)"
      write(iulog, fmt_str) stdname(1:slen), diff_count, max_diff, index_str

   end subroutine write_check_field_entry

end module physics_data
