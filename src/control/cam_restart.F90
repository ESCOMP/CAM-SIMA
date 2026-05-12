module cam_restart

  implicit none
  private

  public :: cam_write_restart

CONTAINS
  subroutine cam_write_restart(dyn_out, yr_spec, mon_spec, day_spec, sec_spec)
    use cam_filenames,    only: interpret_filename_spec
    use cam_pio_utils,    only: cam_pio_createfile, cam_pio_set_fill
    use restart_dynamics, only: write_restart_dynamics, init_restart_dynamics
    use restart_physics,  only: restart_physics_write, restart_physics_init
    use cam_history,      only: history_restart_init, history_restart_write
    use cam_instance,     only: inst_suffix
    use pio,              only: file_desc_t, io_desc_t, pio_double, pio_global
    use pio,              only: pio_put_att, pio_enddef, pio_closefile
    use cam_grid_support, only: cam_grid_write_attr, cam_grid_id
    use cam_grid_support, only: cam_grid_header_info_t, cam_grid_write_var
    use cam_grid_support, only: cam_grid_dimensions, cam_grid_get_decomp
    use physics_grid,     only: phys_decomp, num_global_phys_cols
    use dyn_comp,         only: dyn_export_t
    use cam_control_mod,  only: caseid
    use cam_abortutils,   only: endrun, check_allocate
    use shr_kind_mod,     only: cl=>shr_kind_cl

    ! Arguments
    type(dyn_export_t),      intent(in) :: dyn_out
    integer,       optional, intent(in) :: yr_spec         ! Simulation year
    integer,       optional, intent(in) :: mon_spec        ! Simulation month
    integer,       optional, intent(in) :: day_spec        ! Simulation day
    integer,       optional, intent(in) :: sec_spec        ! Seconds into current simulation day

    ! Local variables
    character(len=cl) :: rfilename_spec ! filename specifier for primary restart file
    character(len=cl) :: fname          ! Restart filename
    type(file_desc_t) :: fh
    integer           :: ierr, i, errflg
    character(len=512) :: errmsg
    integer           :: grid_id
    integer           :: dims(3), gdims(3)
    integer           :: nhdims, ndims
    type(io_desc_t), pointer     :: iodesc
    type(cam_grid_header_info_t) :: info

    ! Set template for primary restart filename based on instance suffix
    ! (%c = caseid, $y = year, $m = month, $d = day, $s = seconds in day, %t = number)
    rfilename_spec = '%c.cam' // trim(inst_suffix) //'.r.%y-%m-%d-%s.nc'
    fname = interpret_filename_spec(rfilename_spec, yr_spec=yr_spec, &
            mon_spec=mon_spec, day_spec=day_spec, sec_spec= sec_spec)

    call cam_pio_createfile(fh, trim(fname), 0)
    ierr = cam_pio_set_fill(fh)

    call init_restart_dynamics(fh, dyn_out)

    ! TODO: initialize ionosphere restart

    ! Initialize physics grid variable
    grid_id = cam_grid_id('physgrid')
    call cam_grid_write_attr(fh, grid_id, info)

    call restart_physics_init(fh, errmsg, errflg)
    if (errflg /= 0) then
       call endrun(errmsg)
    end if
    call history_restart_init(fh)

    ierr = pio_put_att(fh, pio_global, 'caseid', caseid)
    ierr = pio_enddef(fh)

    call write_restart_dynamics(fh, dyn_out)

    ! TODO: write ionosphere restart

    ! Write physics grid info
    call cam_grid_write_var(fh, phys_decomp)

    ! Write physics restart variables
    call restart_physics_write(fh, grid_id, errmsg, errflg)
    if (errflg /= 0) then
       call endrun(errmsg)
    end if

!    call write_restart_history(fh, yr_spec=yr_spec, mon_spec=mon_spec, &
!            day_spec=day_spec, sec_spec= sec_spec )
    call history_restart_write(fh)

    ! Close the primary restart file
    call pio_closefile(fh)

    ! Update the restart pointer file
!    call write_rest_pfile(fname, yr_spec=yr_spec, mon_spec=mon_spec, &
!                          day_spec=day_spec, sec_spec= sec_spec )


  end subroutine cam_write_restart

end module cam_restart
