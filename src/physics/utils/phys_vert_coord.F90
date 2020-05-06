module phys_vert_coord

   use shr_kind_mod,        only: r8 => shr_kind_r8
   use physics_column_type, only: physics_column_t
   use perf_mod,            only: t_adj_detailf, t_startf, t_stopf

   implicit none
   private
   save

   public :: phys_vert_coord_init ! initialize the physics grid

   ! dynamics field grid information
   ! hdim1_d and hdim2_d are dimensions of rectangular horizontal grid
   ! data structure, If 1D data structure, then hdim2_d == 1.
   integer                             :: hdim1_d, hdim2_d
   ! Dycore name and properties
   character(len=8), protected, public :: dycore_name = ''

   ! These variables are last to provide a limited table to search

   !> \section arg_table_phys_vert_coord  Argument Table
   !! \htmlinclude arg_table_phys_vert_coord.html
   !!
   integer,          protected, public :: pver = 0
   integer,          protected, public :: pverp = 0
   integer,          protected, public :: index_top_layer = 0
   integer,          protected, public :: index_bottom_layer = 0
   integer,          protected, public :: index_top_interface = 1
   integer,          protected, public :: index_bottom_interface = 0

!==============================================================================
CONTAINS
!==============================================================================

   subroutine phys_vert_coord_init(pver_in, pverp_in)
!      use mpi,              only: MPI_reduce ! XXgoldyXX: Should this work?
      use mpi,              only: MPI_INTEGER, MPI_MIN
      use cam_abortutils,   only: endrun
      use spmd_utils,       only: npes, mpicom
      use dyn_grid,         only: get_dyn_grid_info, physgrid_copy_attributes_d
      use cam_grid_support, only: cam_grid_register, cam_grid_attribute_register
      use cam_grid_support, only: iMap, hclen => max_hcoordname_len
      use cam_grid_support, only: horiz_coord_t, horiz_coord_create
      use cam_grid_support, only: cam_grid_attribute_copy, cam_grid_attr_exists

      ! Local variables
      integer, intent(in) :: pver_in
      integer, intent(in) :: pverp_in

   end subroutine phys_vert_coord_init

end module phys_vert_coord
