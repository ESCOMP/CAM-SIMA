module dyn_comp
    use runtime_obj, only: runtime_options

    implicit none

    private
    ! Provide APIs required by CAM Control.
    public :: dyn_import_t
    public :: dyn_export_t
    public :: dyn_readnl
    public :: dyn_init
    ! public :: dyn_run
    ! public :: dyn_final

    type dyn_import_t
    end type dyn_import_t

    type dyn_export_t
    end type dyn_export_t
contains

! Called by `read_namelist` in `src/control/runtime_opts.F90`.
subroutine dyn_readnl(namelist_path)
    character(*), intent(in) :: namelist_path
end subroutine dyn_readnl

! Called by `cam_init` in `src/control/cam_comp.F90`.
subroutine dyn_init(cam_runtime_opts, dyn_in, dyn_out)
    type(runtime_options), intent(in)  :: cam_runtime_opts
    type(dyn_import_t),    intent(out) :: dyn_in
    type(dyn_export_t),    intent(out) :: dyn_out
end subroutine dyn_init

! Not used for now. Intended to be called by `stepon_run*` in `src/dynamics/mpas/stepon.F90`.
! subroutine dyn_run()
! end subroutine dyn_run

! Not used for now. Intended to be called by `stepon_final` in `src/dynamics/mpas/stepon.F90`.
! subroutine dyn_final()
! end subroutine dyn_final

end module dyn_comp
