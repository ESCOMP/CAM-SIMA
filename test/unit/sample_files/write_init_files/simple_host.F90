module simple_host

  use ccpp_kinds, only: kind_phys

  implicit none
  private

  public simple_sub

CONTAINS

  !> \section arg_table_simple_sub  Argument Table
  !! \htmlinclude arg_table_simple_sub.html
  !!
  subroutine simple_sub()

    use simple_ccpp_cap, only: HelloWorld_ccpp_physics_initialize
    use simple_ccpp_cap, only: HelloWorld_ccpp_physics_timestep_initial
    use simple_ccpp_cap, only: HelloWorld_ccpp_physics_run
    use simple_ccpp_cap, only: HelloWorld_ccpp_physics_timestep_final
    use simple_ccpp_cap, only: HelloWorld_ccpp_physics_finalize
    use simple_ccpp_cap, only: ccpp_physics_suite_list
    use simple_ccpp_cap, only: ccpp_physics_suite_part_list
    use simple_mod,     only: init_temp, compare_temp


    !integer                         :: col_start, col_end
    integer                         :: index
    character(len=128), allocatable :: part_names(:)
    character(len=512)              :: errmsg
    integer                         :: errflg
    integer                         :: pcols
    integer                         :: col_start
    integer                         :: col_end
    integer                         :: ncols
    integer, protected              :: pver
    real(kind_phys)                 :: dtime_phys

    ! Declare a horizontal dimension:
    pcols = 5

    ! Initialize our 'data'
    call init_temp()

    ! Use the suite information to setup the run
    call simple_ccpp_physics_initialize('simple_suite', errmsg, errflg)
    if (errflg /= 0) then
      write(6, *) trim(errmsg)
      stop
    end if

    ! Initialize the timestep
    call simple_ccpp_physics_timestep_initial('simple_suite', errmsg, errflg)
    if (errflg /= 0) then
      write(6, *) trim(errmsg)
      stop
    end if

    do col_start = 1, ncols, 5
      col_end = MIN(col_start + 4, ncols)

      call simple_ccpp_physics_run('simple_suite', 'physics', col_start, col_end, errmsg, errflg)
      if (errflg /= 0) then
        write(6, *) trim(errmsg)
        call ccpp_physics_suite_part_list('simple_suite', part_names, errmsg, errflg)
        write(6, *) 'Available suite parts are:'
        do index = 1, size(part_names)
          write(6, *) trim(part_names(index))
        end do
        stop
      end if
    end do

    call simple_ccpp_physics_timestep_final('simple_suite', errmsg, errflg)

    call simple_ccpp_physics_finalize('simple_suite', errmsg, errflg)
    if (errflg /= 0) then
      write(6, *) trim(errmsg)
      write(6,'(a)') 'An error occurred in ccpp_timestep_final, Exiting...'
      stop
    end if

    if (compare_temp()) then
      write(6, *) 'Answers are correct!'
    else
      write(6, *) 'Answers are not correct!'
    end if

  end subroutine simple_sub

end module simple_host

program simple
  use simple_host, only: simple_sub
  call simple_sub()
end program simple
