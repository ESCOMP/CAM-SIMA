module cam_restart

  implicit none
  private

  public :: cam_write_restart

CONTAINS
  subroutine cam_write_restart(dyn_out, yr_spec, mon_spec, day_spec, sec_spec)
    use cam_filenames,    only: interpret_filename_spec
    use cam_pio_utils,    only: cam_pio_createfile, cam_pio_set_fill, cam_pio_closefile
    use restart_dynamics, only: write_restart_dynamics, init_restart_dynamics
    use restart_physics,  only: write_restart_physics, init_restart_physics
    use cam_history,      only: init_restart_history, write_restart_history
    use cam_instance,     only: inst_suffix
    use pio,              only: file_desc_t, io_desc_t, pio_double, pio_global
    use pio,              only: pio_put_att, pio_enddef
    use cam_grid_support, only: cam_grid_write_attr, cam_grid_id
    use cam_grid_support, only: cam_grid_header_info_t, cam_grid_write_var
    use cam_grid_support, only: cam_grid_dimensions, cam_grid_get_decomp
    use physics_grid,     only: phys_decomp, num_global_phys_cols
    use dyn_comp,         only: dyn_export_t
    use cam_control_mod,  only: caseid
    use cam_abortutils,   only: safe_endrun
    use shr_kind_mod,     only: cl=>shr_kind_cl
    use runtime_obj,      only: cam_runtime_opts

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

    call cam_pio_createfile(fh, trim(fname))
    ierr = cam_pio_set_fill(fh)

    call init_restart_dynamics(fh, dyn_out)

    ! TODO: initialize ionosphere restart

    ! Initialize physics grid variable
    grid_id = cam_grid_id('physgrid')
    call cam_grid_write_attr(fh, grid_id, info)

    call init_restart_physics(fh, errmsg, errflg)
    if (errflg /= 0) then
       call safe_endrun(errmsg)
    end if
    call init_restart_history(fh)

    ierr = pio_put_att(fh, pio_global, 'caseid', caseid)

    ! The MPAS dycore will end the definition phase for us
    if (cam_runtime_opts%get_dycore() /= 'mpas') then
       ierr = pio_enddef(fh)
    end if

    ! Restart dynamics must come first in write phase
    ! because MPAS restart write includes the definitions
    call write_restart_dynamics(fh, dyn_out)

    ! TODO: write ionosphere restart

    ! Write physics grid info
    call cam_grid_write_var(fh, phys_decomp)

    ! Write physics restart variables
    call write_restart_physics(fh, grid_id, errmsg, errflg)
    if (errflg /= 0) then
       call safe_endrun(errmsg)
    end if

    call write_restart_history(fh)

    ! Close the primary restart file
    call cam_pio_closefile(fh)

    ! Update the restart pointer file
    call write_rest_pfile(fname, yr_spec=yr_spec, mon_spec=mon_spec, &
                          day_spec=day_spec, sec_spec= sec_spec )


  end subroutine cam_write_restart

!========================================================================================

  subroutine write_rest_pfile(restart_file, yr_spec, mon_spec, day_spec, sec_spec)

     ! Write the restart pointer file
     use cam_instance,     only: inst_suffix
     use cam_filenames,    only: interpret_filename_spec
     use shr_kind_mod,     only: cl=>shr_kind_cl
     use spmd_utils,       only: masterproc
     use cam_logfile,      only: iulog
     use cam_abortutils,   only: endrun
     use ioFileMod,        only: cam_open_file

     character(len=*),        intent(in) :: restart_file
     integer,       optional, intent(in) :: yr_spec         ! Simulation year
     integer,       optional, intent(in) :: mon_spec        ! Simulation month
     integer,       optional, intent(in) :: day_spec        ! Simulation day
     integer,       optional, intent(in) :: sec_spec        ! Seconds into current simulation day

     integer :: ierr, unitn
     character(len=CL) :: rest_pfile
     character(len=256) :: errmsg
     character(len=*), parameter :: sub='write_rest_pfile'
     !---------------------------------------------------------------------------

     if (masterproc) then
        rest_pfile = interpret_filename_spec('rpointer.cam'//trim(inst_suffix)//'.'//'%y-%m-%d-%s',&
             yr_spec=yr_spec, mon_spec=mon_spec, day_spec=day_spec, sec_spec= sec_spec )
        call cam_open_file(rest_pfile, unitn, 'f', status="unknown")
        rewind unitn
        write(unitn, '(a)', iostat=ierr, iomsg=errmsg) trim(restart_file)
        if (ierr /= 0) then
           call endrun(sub//': ERROR: writing rpointer file: '//trim(errmsg))
        end if
        close(unitn)

        write(iulog,*)'(WRITE_REST_PFILE): successfully wrote local restart pointer file ',&
           trim(rest_pfile)
        write(iulog,'("---------------------------------------")')
     end if

  end subroutine write_rest_pfile

!========================================================================================


end module cam_restart
