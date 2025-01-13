module cam_abortutils

   use shr_sys_mod,  only: shr_sys_abort, shr_sys_flush
   use shr_kind_mod, only: max_chars=>shr_kind_cx, msg_len=>SHR_KIND_CS
   use shr_kind_mod, only: r8 => shr_kind_r8
   use shr_mem_mod,  only: shr_mem_getusage
   use pio,          only: file_desc_t
   use cam_logfile,  only: iulog

   implicit none
   private
   save

   public :: endrun
   public :: safe_endrun
   public :: check_allocate
   public :: cam_register_open_file
   public :: cam_register_close_file

   type :: open_file_pointer
      type(file_desc_t),       pointer :: file_desc => NULL()
      character(len=max_chars)         :: file_name = ''
      type(open_file_pointer), pointer :: next => NULL()
   end type open_file_pointer

   type(open_file_pointer), pointer :: open_files_head => NULL()
   type(open_file_pointer), pointer :: open_files_tail => NULL()
   type(open_file_pointer), pointer :: open_files_pool => NULL()

CONTAINS

   subroutine check_allocate(errcode, subname, fieldname, file, line, errmsg)
      ! If <errcode> is not zero, call endrun with an error message

      ! Dummy arguments
      integer,                    intent(in) :: errcode
      character(len=*),           intent(in) :: subname
      character(len=*),           intent(in) :: fieldname
      character(len=*), optional, intent(in) :: file
      integer,          optional, intent(in) :: line
      character(len=*), optional, intent(in) :: errmsg

      ! Local variables
      character(len=max_chars) :: abort_msg
      real(r8)                 :: mem_val, mem_hw_val

      if (errcode /= 0) then
         ! Get memory values
         call shr_mem_getusage(mem_hw_val, mem_val)

         ! Write error message with memory stats
         write(abort_msg, '(4a,i0,a,f10.2,a,f10.2,a)') &
              trim(subname), ": Allocation of '",  &
              trim(fieldname), "' failed with code ", errcode, &
              ". Memory highwater is ", mem_hw_val, &
              " mb, current memory usage is ", mem_val, " mb"

         ! If the optional fortran allocate error message is passed in, include it in the abort message
         if(present(errmsg)) then
            write(abort_msg, '(a)') trim(abort_msg) // new_line('a') // "Allocation failed with: " // trim(errmsg)
         endif

         ! End the simulation
         call endrun(abort_msg, file=file, line=line)
      end if

   end subroutine check_allocate

   subroutine cam_register_open_file(file, file_name)
      ! Dummy arguments
      type(file_desc_t), target, intent(in) :: file
      character(len=*),          intent(in) :: file_name
      ! Local variables
      type(open_file_pointer), pointer :: of_ptr
      type(open_file_pointer), pointer :: of_new
      integer                          :: ierr
      character(len=*),  parameter     :: subname = 'cam_register_open_file'

      nullify(of_new)
      ! First, make sure we do not have this file
      of_ptr => open_files_head
      do while (associated(of_ptr))
         if (file%fh == of_ptr%file_desc%fh) then
            call endrun(subname//': Cannot register '//trim(file_name)//', file already open as '//trim(of_ptr%file_name))
         end if
         of_ptr => of_ptr%next
      end do
      ! If we get here, go ahead and register the file
      if (associated(open_files_pool)) then
         ! Reuse pooled structure and point to the next pool entry
         of_new => open_files_pool
         open_files_pool => open_files_pool%next
         allocate(of_new%file_desc, stat=ierr)
         call check_allocate(ierr, subname, 'of_file%file_desc', file=__FILE__, &
             line=__LINE__)
         of_new%file_desc = file
         of_new%file_name = file_name
         nullify(of_new%next)
      else
         allocate(of_new)
         allocate(of_new%file_desc)
         of_new%file_desc = file
         of_new%file_name = file_name
         nullify(of_new%next)
      end if

      ! Add the registered file to the tail of the open files list
      if(associated(open_files_tail)) then
         open_files_tail%next => of_new
         open_files_tail      => of_new
      else
         open_files_head      => of_new
         open_files_tail      => of_new
      endif
   end subroutine cam_register_open_file

   subroutine cam_register_close_file(file, log_shutdown_in)
      ! Dummy arguments
      type(file_desc_t), target,   intent(in) :: file
      character(len=*),  optional, intent(in) :: log_shutdown_in
      ! Local variables
      type(open_file_pointer), pointer :: of_ptr
      type(open_file_pointer), pointer :: of_prev
      character(len=msg_len)           :: log_shutdown
      character(len=*),  parameter     :: subname = 'cam_register_close_file'
      logical                          :: file_loop_var

      nullify(of_prev)
      ! Are we going to log shutdown events?
      if (present(log_shutdown_in)) then
         log_shutdown = trim(log_shutdown_in)
      else
         log_shutdown = ''
      end if
      ! Look to see if we have this file
      of_ptr => open_files_head

      !Set while-loop control variable
      file_loop_var = .false.
      if (associated(of_ptr)) then
          if(associated(of_ptr%file_desc)) then
             file_loop_var = .true.
          end if
      end if

      do while (file_loop_var)
         if (file%fh == of_ptr%file_desc%fh) then
            ! Remove this file from the list
            if (associated(of_prev)) then
               of_prev%next => of_ptr%next
            else
               open_files_head => of_ptr%next
            end if
            ! Log closure?
            ! Note, no masterproc control because this could be any PE
            if (len_trim(log_shutdown) > 0) then
               write(iulog, '(a,": ",a," of ",a)') subname, &
                    trim(log_shutdown), trim(of_ptr%file_name)
               call shr_sys_flush(iulog)
            end if
            ! Push this object on to free pool
            nullify(of_ptr%file_desc)
            of_ptr%next => open_files_pool
            open_files_pool => of_ptr
            nullify(of_ptr)
            exit
         else
            of_prev => of_ptr
            of_ptr => of_ptr%next
         end if
         !Check if loop needs to continue
         if (.not.associated(of_ptr)) then
            file_loop_var = .false.
         else
            if(.not.associated(of_ptr%file_desc)) then
               file_loop_var = .false.
            end if
         end if

      end do
   end subroutine cam_register_close_file

   subroutine endrun(message, file, line)
      ! Parallel emergency stop
      ! Dummy arguments
      character(len=*),           intent(in) :: message
      character(len=*), optional, intent(in) :: file
      integer,          optional, intent(in) :: line
      ! Local variables
      character(len=max_chars)               :: abort_msg
      if (present(file) .and. present(line)) then
         write(abort_msg, '(4a,i0)') trim(message),' at ',trim(file),':',line
      else if (present(file)) then
         write(abort_msg, '(3a)') trim(message),' at ',trim(file)
      else if (present(line)) then
         write(abort_msg, '(2a,i0)') trim(message),' on line ',line
      else
         write(abort_msg, '(a)') trim(message)
      end if
      call shr_sys_abort(abort_msg)

   end subroutine endrun

   subroutine safe_endrun(message, file, line)
      ! Sequential/global emergency stop
      use pio, only : pio_closefile
      ! Dummy arguments
      character(len=*),           intent(in) :: message
      character(len=*), optional, intent(in) :: file
      integer,          optional, intent(in) :: line

      ! Local variables
      character(len=max_chars)               :: abort_msg
      type(open_file_pointer), pointer       :: of_ptr
      logical                                :: keep_loop

      ! First, close all open PIO files
      of_ptr => open_files_head

      !Check if needed pointers are associated:
      keep_loop = .false.
      if (associated(of_ptr)) then
         if (associated(of_ptr%file_desc)) then
            keep_loop = .true.
         end if
      end if

      do while (keep_loop)
         call pio_closefile(of_ptr%file_desc)
         call cam_register_close_file(of_ptr%file_desc,                       &
              log_shutdown_in="Emergency close")
         of_ptr => of_ptr%next
         !End loop if new pointers aren't associated:
         if (.not. associated(of_ptr)) then
            keep_loop = .false.
         else if (.not. associated(of_ptr%file_desc)) then
            keep_loop = .false.
         end if
      end do

      call endrun(message, file, line)

   end subroutine safe_endrun
end module cam_abortutils
