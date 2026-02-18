module vert_coord

   use shr_kind_mod,        only: r8 => shr_kind_r8
   use physics_column_type, only: physics_column_t
   use perf_mod,            only: t_adj_detailf, t_startf, t_stopf
   use cam_abortutils,      only: endrun


   implicit none
   private
   save

   public :: vert_coord_readnl ! Read vertical coordinate namelist inputs
   public :: vert_coord_init   ! Initialize the vertical coordinate

   ! These variables are last to provide a limited table to search

   !> \section arg_table_vert_coord  Argument Table
   !! \htmlinclude arg_table_vert_coord.html
   !!
   integer,          protected, public :: pver = -1
   integer,          protected, public :: pverp = -1
   integer,          protected, public :: index_top_layer = -1
   integer,          protected, public :: index_bottom_layer = -1
   integer,          protected, public :: index_top_interface = -1
   integer,          protected, public :: index_bottom_interface = -1

   logical                             :: coord_initialized = .false.

!==============================================================================
CONTAINS
!==============================================================================

   subroutine vert_coord_readnl(nlfile)

      use spmd_utils,      only: masterproc, masterprocid, mpicom, npes
      use cam_abortutils,  only: endrun
      use shr_nl_mod,      only: find_group_name => shr_nl_find_group_name
      use mpi,             only: mpi_integer
      use string_utils,    only: to_str

      character(len=*), intent(in) :: nlfile  ! filepath for file containing namelist input

      ! Local variables
      integer :: unitn, ierr
      character(len=*), parameter :: subname = 'vert_coord_readnl'

      namelist /vert_coord_nl/ pver
      !-----------------------------------------------------------------------------

      ! Read "vert_coord" namelist:
      if (masterproc) then
         open( newunit=unitn, file=trim(nlfile), status='old' )
         call find_group_name(unitn, 'vert_coord_nl', status=ierr)
         if (ierr == 0) then
            read(unitn, vert_coord_nl, iostat=ierr)
            if (ierr /= 0) then
               call endrun(subname // ':: ERROR reading namelist')
            end if
         end if
         close(unitn)

         ! Perform sanity checks on provided values:
         if (pver < 0) then
            call endrun(subname//": ERROR: pver must not be negative:"//&
                        " pver = "//to_str(pver))
         end if
      end if

      ! Broadcast namelist variables
      call mpi_bcast(pver, 1, mpi_integer, masterprocid, mpicom, ierr)

   end subroutine vert_coord_readnl

!====================================================================================

   subroutine vert_coord_init(index_top_layer_in, index_bottom_layer_in)

      use cam_abortutils,   only: endrun

      integer,            intent(in) :: index_top_layer_in
      integer,            intent(in) :: index_bottom_layer_in

      ! Prevent initialization from running twice:
      if (coord_initialized) then
        call endrun('vert_coord_init: This routine may only be called once and has already been called')
      end if

      ! Initialize number of interface layers:
      pverp = pver + 1

      ! Set vertical level index order:
      index_top_layer    = index_top_layer_in
      index_bottom_layer = index_bottom_layer_in

      if (index_top_layer < index_bottom_layer) then
         index_top_interface    = index_top_layer
         index_bottom_interface = index_bottom_layer + 1
      else
         index_bottom_interface = index_bottom_layer
         index_top_interface    = index_top_layer + 1
      end if

      ! Prevent initialization from occurring again:
      coord_initialized = .true.

   end subroutine vert_coord_init

end module vert_coord
