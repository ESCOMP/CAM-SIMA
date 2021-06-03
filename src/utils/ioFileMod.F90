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

   subroutine cam_get_file(fulpath, locfn, iflag, lexist, log_info)

      ! --------------------------------------------------------------------
      ! Determine whether file is on local disk.
      ! . first check current working directory
      ! . next check full pathname[fulpath] on disk
      ! . by default, abort if file not found.  Setting optional iflag arg
      !   to 1 overrides this behavior, and in that case the optional lexist
      !   arg is used to return status of whether the file was found or not.
      ! fulpath is the full pathname on local disk
      ! locfn is the local file name if found in working directory,
      !       otherwise it is set to <fulpath>.
      ! --------------------------------------------------------------------

      ! Dummy arguments
      character(len=*),  intent(in)  :: fulpath
      character(len=*),  intent(out) :: locfn
      integer, optional, intent(in)  :: iflag  ! abort unless iflag=1
      logical, optional, intent(out) :: lexist ! .true. if the file is found
      logical, optional, intent(in)  :: log_info ! if .false. don't print info

      ! ------------------------ local variables ---------------------------
      integer            :: i               ! loop index
      integer            :: klen            ! length of fulpath character string
      integer            :: maxlen          ! length of locfn input variable
      logical            :: lexist_in       ! true if local file exists
      logical            :: abort_on_failure
      character(len=192) :: errmsg
      logical            :: log_information
      character(len=*), parameter :: subname = 'cam_get_file'
      ! --------------------------------------------------------------------

      abort_on_failure = .true.
      if (present(iflag)) then
         if (iflag==1) then
            abort_on_failure = .false.
         end if
      end if
      maxlen = len(locfn)
      if (present(log_info)) then
         log_information = log_info
      else
         log_information = .true.
      end if

      ! first check if file is in current working directory.
      ! get local file name from full name: start at end. look for first "/"
      klen = len_trim(fulpath)
      i = index(fulpath, '/', back=.true.)

      if ((klen-i) > maxlen) then
         write(errmsg, '(2a,i0,a,i0)') subname,                               &
              ': local filename variable is too short for path length',       &
              klen-i, ' > ', maxlen
         if (abort_on_failure) then
            call endrun(errmsg)
         else
            if (masterproc) then
               write(iulog, *) errmsg
               if (present(lexist)) then
                  lexist = .false.
               end if
            end if
            return
         end if
      end if

      locfn = fulpath(i+1:klen)
      if (len_trim(locfn) == 0) then
         call endrun (subname//': local filename has zero length')
      else if (masterproc .and. log_information) then
         write(iulog, *) subname//': attempting to find local file ',         &
              trim(locfn)
      end if

      inquire(file=locfn, exist=lexist_in)
      if (present(lexist)) then
         lexist = lexist_in
      end if
      if (lexist_in) then
         if (masterproc .and. log_information) then
            write(iulog, *) subname//': using ', trim(locfn),                 &
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

      locfn = trim(fulpath)
      inquire(file=locfn, exist=lexist_in)
      if (present(lexist)) then
         lexist = lexist_in
      end if
      if (lexist_in) then
         if (masterproc .and. log_information) then
            write(iulog, *) subname, ': using ', trim(fulpath)
         end if
         return
      else
         if (masterproc) then
            write(iulog, *) subname, ': all tries to get file have been ',    &
                 'unsuccessful: ',trim(fulpath)
         end if
         if (abort_on_failure) then
            call endrun (subname//': FAILED to get '//trim(fulpath))
         end if
      end if

   end subroutine cam_get_file

   !=======================================================================

   subroutine cam_open_file (locfn, iun, form, status)

      !-----------------------------------------------------------------------
      ! open file locfn in unformatted or formatted form on unit iun
      ! form = 'u' for unformatted, 'f' for formatted
      !-----------------------------------------------------------------------

      ! Dummy arguments
      character(len=*),           intent(in)  :: locfn  !file name
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

      if (len_trim(locfn) == 0) then
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
      open(newunit=iun, file=locfn, status=st, form=ft, iostat=ioe)
      if (ioe /= 0) then
         write(errmsg, '(3a,2(a,i0))') subname, ": failed to open file '",    &
              trim(locfn), "' on unit ", iun, " ierr = ", ioe
         if(masterproc) then
            write(iulog, *) errmsg
         end if
         call endrun(errmsg)
      else
         if(masterproc) then
            write(iulog, '(4a,i0)') subname, ": Successfully opened file '",  &
                 trim(locfn), "' on unit = ", iun
         end if
      end if

   end subroutine cam_open_file

end module ioFileMod
