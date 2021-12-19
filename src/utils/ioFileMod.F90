module ioFileMod
!---------------------------------------------------------------------
!
! Purpose:
!
!	Input/Output file manipulations. Mind file on archival system, or local
!	disk etc.
!
! Author: Mariana Vertenstein
!
!---------------------------------------------------------------------

   use shr_kind_mod,     only: CL => shr_kind_cl
   use cam_abortutils,   only: endrun
   use spmd_utils,       only: masterproc
   use cam_logfile,      only: iulog

   implicit none

!--------------------------------------------------------------------------
! Public interfaces
!--------------------------------------------------------------------------

   private
   save

   public :: cam_get_file      ! Get file from archive
   public :: cam_open_file     ! Open file

!=======================================================================
CONTAINS
!=======================================================================

   subroutine cam_get_file(full_path, file_out, allow_fail, lexist, log_info)

      ! --------------------------------------------------------------------
      ! Determine whether file is on local disk.
      ! . first check current working directory
      ! . next check full pathname[full_path] on disk
      ! . by default, abort if file not found.
      ! Setting optional allow_fail to .true. overrides this behavior,
      ! . and in that case the optional lexist
      ! . arg is used to return status of whether the file was found or not.
      ! full_path is the full pathname on local disk
      ! file_out is the local file name if found in working directory,
      !       otherwise it is set to <full_path>.
      ! --------------------------------------------------------------------

      ! Dummy arguments
      character(len=*),  intent(in)  :: full_path
      character(len=*),  intent(out) :: file_out
      logical, optional, intent(in)  :: allow_fail ! abort unless .true.
      logical, optional, intent(out) :: lexist ! .true. if the file is found
      logical, optional, intent(in)  :: log_info ! if .false. don't print info

      ! ------------------------ local variables ---------------------------
      integer            :: i               ! loop index
      integer            :: klen            ! num chars in full_path string
      integer            :: maxlen          ! length of file_out input variable
      logical            :: lexist_in       ! .true. if local file exists
      logical            :: abort_on_failure
      character(len=CL)  :: errmsg
      logical            :: log_information
      character(len=*), parameter :: subname = 'cam_get_file'
      ! --------------------------------------------------------------------

      if (present(allow_fail)) then
         abort_on_failure = .not. allow_fail
      else
         abort_on_failure = .true.
      end if
      maxlen = len(file_out)
      if (present(log_info)) then
         log_information = log_info
      else
         log_information = .true.
      end if

      ! first check if file is in current working directory.
      ! get local file name from full name: start at end. look for first "/"
      klen = len_trim(full_path)
      i = index(full_path, '/', back=.true.)

      if ((klen-i) > maxlen) then
         write(errmsg, '(2a,i0,a,i0)') subname,                               &
              ': local filename variable is too short for path length',       &
              klen-i, ' > ', maxlen
         if (abort_on_failure) then
            call endrun(trim(errmsg))
         else
            if (masterproc) then
               write(iulog, *) trim(errmsg)
               if (present(lexist)) then
                  lexist = .false.
               end if
            end if
            return
         end if
      end if

      file_out = full_path(i+1:klen)
      if (len_trim(file_out) == 0) then
         call endrun (subname//': local filename has zero length')
      else if (masterproc .and. log_information) then
         write(iulog, *) subname//': attempting to find local file ',         &
              trim(file_out)
      end if

      inquire(file=file_out, exist=lexist_in)
      if (present(lexist)) then
         lexist = lexist_in
      end if
      if (lexist_in) then
         if (masterproc .and. log_information) then
            write(iulog, *) subname//': using ', trim(file_out),              &
                 ' in current working directory'
         end if
         return
      end if

      ! second check for full pathname on disk

      if (klen > maxlen) then
         write(errmsg, '(2a,i0,a,i0)') subname,                               &
              ': local filename variable is too short for path length',       &
              klen, ' > ', maxlen
         if (abort_on_failure) then
            call endrun(errmsg)
         else
            if (masterproc) then
               write(iulog, *) errmsg
            end if
            if (present(lexist)) then
               lexist = .false.
            end if
            return
         end if
      end if

      file_out = trim(full_path)
      inquire(file=file_out, exist=lexist_in)
      if (present(lexist)) then
         lexist = lexist_in
      end if
      if (lexist_in) then
         if (masterproc .and. log_information) then
            write(iulog, *) subname, ': using ', trim(full_path)
         end if
         return
      else
         if (masterproc) then
            write(iulog, *) subname, ': all tries to get file have been ',    &
                 'unsuccessful: ',trim(full_path)
         end if
         if (abort_on_failure) then
            call endrun (subname//': FAILED to get '//trim(full_path))
         end if
      end if

   end subroutine cam_get_file

   !=======================================================================

   subroutine cam_open_file (file_path, iun, form, status)

      !-----------------------------------------------------------------------
      ! open file file_path in unformatted or formatted form on unit iun
      ! form = 'u' for unformatted, 'f' for formatted
      !-----------------------------------------------------------------------

      ! Dummy arguments
      character(len=*),           intent(in)  :: file_path  !file name
      integer,                    intent(out) :: iun    !fortran unit number
      character(len=1),           intent(in)  :: form   !file format
      character(len=*), optional, intent(in)  :: status !file status
      ! Local variables
      integer                     :: ioe ! error return from fortran open
      character(len=11)           :: ft  ! format type: formatted. unformatted
      character(len=11)           :: st  ! file status: old or unknown
      character(len=CL)           :: errmsg
      character(len=*), parameter :: subname = 'cam_open_file'
      ! --------------------------------------------------------------------

      if (len_trim(file_path) == 0) then
         call endrun(subname//': local filename has zero length')
      end if
      if (form=='u' .or. form=='U') then
         ft = 'unformatted'
      else
         ft = 'formatted  '
      end if
      if (present(status)) then
         st = status
      else
         st = "unknown"
      end if
      open(newunit=iun, file=file_path, status=st, form=ft, iostat=ioe)
      if (ioe /= 0) then
         write(errmsg, '(3a,2(a,i0))') subname, ": failed to open file '",    &
              trim(file_path), "' on unit ", iun, " ierr = ", ioe
         if(masterproc) then
            write(iulog, *) trim(errmsg)
         end if
         call endrun(trim(errmsg))
      else
         if(masterproc) then
            write(iulog, '(4a,i0)') subname, ": Successfully opened file '",  &
                 trim(file_path), "' on unit = ", iun
         end if
      end if

   end subroutine cam_open_file

end module ioFileMod
