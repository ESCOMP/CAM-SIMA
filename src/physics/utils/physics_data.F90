module physics_data

   use ccpp_kinds,   only: kind_phys

   implicit none
   private

   public :: find_input_name_idx
   public :: read_field
   public :: check_field

   !Non-standard variable indices:
   integer, public, parameter :: no_exist_idx     = -1
   integer, public, parameter :: init_mark_idx    = -2
   integer, public, parameter :: prot_no_init_idx = -3
   
   integer, public, parameter :: MIN_DIFFERENCE = 0._r8
   integer, public, parameter :: MIN_RELATIVE_VALUE = 10E-6

   interface read_field
      module procedure read_field_2d
      module procedure read_field_3d
   end interface read_field

   interface check_field
      module procedure check_field_2d
      module procedure check_field_3d
   end interface check_field

!==============================================================================
CONTAINS
!==============================================================================

   integer function find_input_name_idx(stdname)

      !Finds the 'input_var_names' array index for a given
      !variable standard name.

      use phys_vars_init_check, only: protected_vars
      use phys_vars_init_check, only: phys_var_stdnames
      use phys_vars_init_check, only: phys_var_num
      use phys_vars_init_check, only: is_initialized

      !Variable standard name being checked:
      character(len=*),  intent(in) :: stdname

      !standard names array index:
      integer                       :: idx

      !Initialize function:
      find_input_name_idx = no_exist_idx

      !Loop through physics variable standard names:
      do idx = 1, phys_var_num
         !Check if provided name is in required names array:
         if (trim(phys_var_stdnames(idx)) == trim(stdname)) then
            !Check if this variable has already been initialized.
            !If so, then set the index to a quantity that will be skipped:
            if (is_initialized(stdname)) then
               find_input_name_idx = init_mark_idx
            else if (protected_vars(idx)) then
               find_input_name_idx = prot_no_init_idx
            else
               !If not already initialized, then pass on the real array index:
               find_input_name_idx = idx
            end if
            !Exit function:
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
      use phys_vars_init_check, only: mark_as_read_from_file
      
      !Max possible length of variable name in input (IC) file:
      use phys_vars_init_check, only: ic_name_len

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
         call mark_as_read_from_file(found_name)
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
      use vert_coord,     only: pver, pverp
      use phys_vars_init_check,  only: mark_as_read_from_file

      !Max possible length of variable name in input (IC) file:
      use phys_vars_init_check,  only: ic_name_len

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
         call mark_as_read_from_file(found_name)
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

   subroutine check_field_2d(file, var_names, current_value, timestep, buffer,&
        diff, is_diff, is_relative_diff, diff_squared)
      use shr_assert_mod, only: shr_assert_in_domain
      use shr_sys_mod,    only: shr_sys_flush
      use pio,            only: file_desc_t, var_desc_t
      use spmd_utils,     only: masterproc
      use cam_pio_utils,  only: cam_pio_find_var
      use cam_abortutils, only: endrun
      use cam_logfile,    only: iulog
      use cam_field_read, only: cam_read_field
      use mpi,            only: MPI_MAX, MPI_SUM, MPI_REAL, MPI_INTEGER
      use spmd_utils,     only: npes, mpicom

      !Max possible length of variable name in file:
      use phys_vars_init_check, only: ic_name_len

      !Dummy arguments:
      type(file_desc_t), intent(inout) :: file
      character(len=*),  intent(in)    :: var_names(:)
      real(kind_phys),   intent(in)    :: current_value
      integer,           intent(in)    :: timestep
      real(kind_phys),   intent(in)    :: buffer(:)
      logical,           intent(out)   :: var_found
      real(kind_phys),   intent(out)   :: diff
      integer,           intent(out)   :: is_diff
      logical,           intent(out)   :: is_relative_diff
      real(kind_phys),   intent(out)   :: diff_squared

      !Local variables:
      character(len=ic_name_len)       :: found_name
      type(var_desc_t)                 :: vardesc
      character(len=*), parameter      :: subname = 'check_field_2d'
      real(kind_phys)                  :: temp_diff !For MPI
      integer                          :: ierr      !For MPI

      !Initialize output variables
      is_diff = 0
      is_relative_diff = .true.
      diff = 0
      diff_squared = 0

      call cam_pio_find_var(file, var_names, found_name, vardesc, var_found)

      if (var_found) then
         if (masterproc) then
            write(iulog, *) subname, ': Checking read-in field, ', trim(found_name)
            call shr_sys_flush(iulog)
         end if
         call cam_read_field(found_name, file, buffer, var_found,             &
              timelevel=timestep)
      else
         end
      end if
      if (var_found) then
         if (current_value < MIN_RELATIVE_DIFFERENCE) then
            !Calculate absolute difference:
            diff = abs(current_value - buffer)
            is_relative_diff = .false.
         else
            !Calculate relative difference:
            diff = abs(current_value - buffer) / abs(current_value)
         end if
         !Determine if diff is large enough to be considered a "hit"
         if (diff > MIN_DIFFERENCE) then
            is_diff = 1
            !Calculate square of diff
            diff_squared = diff ** 2
         end if
         !Gather results across all nodes to get global values
         temp = diff
         ierr = 0
         call MPI_Allreduce(temp, diff, 1, MPI_REAL, MPI_MAX,                 &
              mpicom, ierr)
         temp = is_diff
         call MPI_Allreduce(temp, is_diff, 1, MPI_INTEGER, MPI_SUM,           &
              mpicom, ierr)
         temp = diff_squared
         call MPI_Allreduce(temp, diff_squared, 1, MPI_INTEGER, MPI_SUM,      &
              mpicom, ierr)
      else
         end
      end if
   end subroutine check_field_2d

end module physics_data
