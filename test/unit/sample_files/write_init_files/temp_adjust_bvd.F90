!parameterization to test bad vertical dimension
!

MODULE temp_adjust

  USE ccpp_kinds, ONLY: kind_phys

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: temp_adjust_init
  PUBLIC :: temp_adjust_run
  PUBLIC :: temp_adjust_finalize

CONTAINS

!> \section arg_table_temp_adjust_run  Argument Table
!! \htmlinclude arg_table_temp_adjust_run.html
!!
  SUBROUTINE temp_adjust_run(nbox, lev, temp_layer,    &
    slp, timestep, errmsg, errflg)
!----------------------------------------------------------------
   IMPLICIT NONE
!----------------------------------------------------------------

   integer,            intent(in)    :: nbox, lev
   REAL(kind_phys),    intent(inout) :: temp_layer(:, :)
   real(kind_phys),    intent(in)    :: slp(:,:)
   real(kind_phys),    intent(in)    :: timestep
   character(len=512), intent(out)   :: errmsg
   integer,            intent(out)   :: errflg
!----------------------------------------------------------------

   integer :: box_index
   integer :: lev_index

    errmsg = ''
    errflg = 0

    do box_index = 1, nbox
       do lev_index = 1, lev
          temp_layer(box_index, lev_index) = temp_layer(box_index, lev_index) &
               + 1.0_kind_phys

          !Add a made-up term which uses slp:
          temp_layer(box_index, lev_index) = temp_layer(box_index, lev_index) &
               + 0._kind_phys*slp(box_index, lev_index)
       end do
    end do

  END SUBROUTINE temp_adjust_run

!> \section arg_table_temp_adjust_init  Argument Table
!! \htmlinclude arg_table_temp_adjust_init.html
!!
  subroutine temp_adjust_init (errmsg, errflg)

    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine temp_adjust_init

!> \section arg_table_temp_adjust_finalize  Argument Table
!! \htmlinclude arg_table_temp_adjust_finalize.html
!!
  subroutine temp_adjust_finalize (errmsg, errflg)

    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine temp_adjust_finalize

END MODULE temp_adjust
