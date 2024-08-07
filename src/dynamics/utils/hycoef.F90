module hycoef

use shr_kind_mod,     only: r8 => shr_kind_r8
use ccpp_kinds,       only: kind_phys
use spmd_utils,       only: masterproc
use vert_coord,       only: pver, pverp
use cam_logfile,      only: iulog
use cam_abortutils,   only: endrun
use pio,              only: file_desc_t, var_desc_t, &
                            pio_inq_dimid, pio_inq_dimlen, pio_inq_varid, &
                            pio_double, pio_def_dim, pio_def_var, &
                            pio_put_var, pio_get_var, &
                            pio_seterrorhandling, PIO_BCAST_ERROR, PIO_NOERR

implicit none
private
save

!-----------------------------------------------------------------------
!
! Purpose: Hybrid level definitions: p = a*p0 + b*ps
!          interfaces   p(k) = hyai(k)*ps0 + hybi(k)*ps
!          midpoints    p(k) = hyam(k)*ps0 + hybm(k)*ps
!
!-----------------------------------------------------------------------

real(r8), public, allocatable, target :: hyai(:)  ! ps0 component of hybrid coordinate - interfaces
real(r8), public, allocatable, target :: hyam(:)  ! ps0 component of hybrid coordinate - midpoints
real(r8), public, allocatable, target :: hybi(:)  ! ps component of hybrid coordinate - interfaces
real(r8), public, allocatable, target :: hybm(:)  ! ps component of hybrid coordinate - midpoints

real(r8), public, allocatable :: hybd(:)    ! difference  in b (hybi) across layers
real(r8), public, allocatable :: hypi(:)    ! reference pressures at interfaces
real(r8), public, allocatable :: hypm(:)    ! reference pressures at midpoints
real(r8), public, allocatable :: hypd(:)    ! reference pressure layer thickness

real(r8), public, protected :: ps0    ! Base state surface pressure (pascals)

real(r8), allocatable, target :: alev(:)   ! level values (pascals) for 'lev' coord
real(r8), allocatable, target :: ailev(:)  ! interface level values for 'ilev' coord

integer, public :: nprlev             ! number of pure pressure levels at top

public hycoef_init

type(var_desc_t) :: hyam_desc, hyai_desc, hybm_desc, hybi_desc, p0_desc
public init_restart_hycoef, write_restart_hycoef

!> \section arg_table_hycoef  Argument Table
!! \htmlinclude hycoef.html
real(kind_phys), allocatable, public :: etamid(:)  ! hybrid coordinate - midpoints

!=======================================================================
contains
!=======================================================================

subroutine hycoef_init(file, psdry)

   use cam_history_support, only: add_hist_coord, add_vert_coord, formula_terms_t
   use physconst,    only: pref
   use string_utils, only: to_str
   use phys_vars_init_check, only: mark_as_initialized

   !-----------------------------------------------------------------------
   !
   ! Purpose:
   ! Defines the locations of model interfaces from input data in the
   ! hybrid coordinate scheme.  Actual pressure values of model level
   ! interfaces are determined elsewhere from the fields set here.
   !
   ! Method:
   ! the following fields are set:
   ! hyai     fraction of reference pressure used for interface pressures
   ! hyam     fraction of reference pressure used for midpoint pressures
   ! hybi     fraction of surface pressure used for interface pressures
   ! hybm     fraction of surface pressure used for midpoint pressures
   ! hybd     difference of hybi's
   ! hypi     reference state interface pressures
   ! hypm     reference state midpoint pressures
   ! hypd     reference state layer thicknesses
   ! hypdln   reference state layer thicknesses (log p)
   ! hyalph   distance from interface to level (used in integrals)
   ! prsfac   log pressure extrapolation factor (used to compute psl)
   !
   ! Author: B. Boville
   !
   !-----------------------------------------------------------------------

   ! arguments
   type(file_desc_t), intent(inout) :: file
   logical, optional, intent(in)    :: psdry  ! set true when coordinate is based
                                              ! on dry surface pressure

   ! local variables
   integer  :: k        ! Level index
   integer  :: iret     ! Return status integer
   logical  :: dry_coord
   real(r8) :: amean, bmean, atest, btest, eps
   type(formula_terms_t) :: formula_terms ! For the 'lev' and 'ilev' coords

   character(len=*), parameter :: subname = 'hycoef_init'

   !-----------------------------------------------------------------------

   ! Initalize reference pressure:
   ps0 = real(pref, r8)  ! Reference pressure (pascals)

   ! Allocate public variables:

   allocate(hyai(pverp), stat=iret)
   if (iret /= 0) then
      call endrun(subname//': allocate hyai(pverp) failed with stat: '//to_str(iret))
   end if

   allocate(hyam(pver), stat=iret)
   if (iret /= 0) then
      call endrun(subname//': allocate hyam(pver) failed with stat: '//to_str(iret))
   end if

   allocate(hybi(pverp), stat=iret)
   if (iret /= 0) then
      call endrun(subname//': allocate hybi(pverp) failed with stat: '//to_str(iret))
   end if

   allocate(hybm(pver), stat=iret)
   if (iret /= 0) then
      call endrun(subname//': allocate hybm(pver) failed with stat: '//to_str(iret))
   end if

   allocate(etamid(pver), stat=iret)
   if (iret /= 0) then
      call endrun(subname//': allocate etamid(pver) failed with stat: '//to_str(iret))
   end if

   allocate(hybd(pver), stat=iret)
   if (iret /= 0) then
      call endrun(subname//': allocate hybd(pver) failed with stat: '//to_str(iret))
   end if

   allocate(hypi(pverp), stat=iret)
   if (iret /= 0) then
      call endrun(subname//': allocate hypi(pverp) failed with stat: '//to_str(iret))
   end if

   allocate(hypm(pver), stat=iret)
   if (iret /= 0) then
      call endrun(subname//': allocate hypm(pver) failed with stat: '//to_str(iret))
   end if

   allocate(hypd(pver), stat=iret)
   if (iret /= 0) then
      call endrun(subname//': allocate hypd(pver) failed with stat: '//to_str(iret))
   end if

   allocate(alev(pver), stat=iret)
   if (iret /= 0) then
      call endrun(subname//': allocate alev(pver) failed with stat: '//to_str(iret))
   end if

   allocate(ailev(pverp), stat=iret)
   if (iret /= 0) then
      call endrun(subname//': allocate ailev(pverp) failed with stat: '//to_str(iret))
   end if

   ! check for dry pressure coordinate (default is moist)
   dry_coord = .false.
   if (present(psdry)) dry_coord = psdry

   ! read hybrid coeficients
   call hycoef_read(file)

   ! Set layer locations
   nprlev = 0
   do k=1,pver

      ! Interfaces. Set nprlev to the interface above, the first time a
      ! nonzero surface pressure contribution is found. "nprlev"
      ! identifies the lowest pure pressure interface.

! Remove this line once determine its replacement doesn't cause answer changes
!      if (nprlev==0 .and. hybi(k).ne.0.0_r8) nprlev = k - 1
      if (hybi(k) /= 0.0_r8) then
        nprlev = k - 1
        exit
      end if
   end do

   ! Set nprlev if no nonzero b's have been found. All interfaces are
   ! pure pressure. A pure pressure model requires other changes as well.
   if (nprlev==0) nprlev = pver + 2

   ! Set delta sigma part of layer thickness and reference state midpoint
   ! pressures
   do k=1,pver
      hybd(k) = hybi(k+1) - hybi(k)
      hypm(k) = hyam(k)*ps0 + hybm(k)*ps0
      etamid(k) = hyam(k) + hybm(k)
   end do

   ! Reference state interface pressures
   do k=1,pverp
      hypi(k) = hyai(k)*ps0 + hybi(k)*ps0
   end do

   ! Reference state layer thicknesses
   do k=1,pver
      hypd(k) = hypi(k+1) - hypi(k)
   end do

   ! Test that A's and B's at full levels are arithmetic means of A's and
   ! B's at interfaces
   eps    = 1.e-05_r8
   do k = 1,pver
      amean = ( hyai(k+1) + hyai(k) )*0.5_r8
      bmean = ( hybi(k+1) + hybi(k) )*0.5_r8
      if(amean == 0._r8 .and. hyam(k) == 0._r8) then
         atest = 0._r8
      else
         atest = abs( amean - hyam(k) )/ ( 0.5_r8*( abs(amean + hyam(k)) ) )
      endif
      if(bmean == 0._r8 .and. hybm(k) == 0._r8) then
         btest = 0._r8
      else
         btest = abs( bmean - hybm(k) )/ ( 0.5_r8*( abs(bmean + hybm(k)) ) )
      endif
      if (atest > eps) then
         if (masterproc) then
            write(iulog,9850)
            write(iulog,*)'k,atest,eps=',k,atest,eps
         end if
      end if

      if (btest > eps) then
         if (masterproc) then
            write(iulog,9850)
            write(iulog,*)'k,btest,eps=',k,btest,eps
         end if
      end if
   end do

   ! Add the information for the 'lev' and 'ilev' mdim history coordinates
   !
   ! The hybrid coordinate used by the SE dycore is based on a dry surface
   ! pressure.  Hence it is the dry pressure rather than actual pressure
   ! that is computed by the formula_terms attribute.  This coordinate is
   ! not described by the formula
   ! atmosphere_hybrid_sigma_pressure_coordinate since the formula
   ! associated with that name uses actual pressure values.  Furthermore,
   ! the actual pressure field cannot be reconstructed from the hybrid
   ! coefficients and the surface pressure field.  Hence in the case of a
   ! dry coordinate we add neither the standard_name nor the formula_terms
   ! attributes to the lev and ilev coordinates.

   ! 0.01 converts Pascals to millibars
   alev(:pver) = 0.01_r8*ps0*(hyam(:pver) + hybm(:pver))
   ailev(:pverp) = 0.01_r8*ps0*(hyai(:pverp) + hybi(:pverp))

   if (dry_coord) then
      call add_vert_coord('lev', pver,                                       &
         'hybrid level at midpoints (1000*(A+B))', 'hPa', alev,              &
         positive='down')
      call add_hist_coord('hyam', pver, &
         'hybrid A coefficient at layer midpoints', '1', hyam, dimname='lev')
      call add_hist_coord('hybm', pver, &
         'hybrid B coefficient at layer midpoints', '1', hybm, dimname='lev')
   else

      formula_terms%a_name       =  'hyam'
      formula_terms%a_long_name  =  'hybrid A coefficient at layer midpoints'
      formula_terms%a_values     => hyam
      formula_terms%b_name       =  'hybm'
      formula_terms%b_long_name  =  'hybrid B coefficient at layer midpoints'
      formula_terms%b_values     => hybm
      formula_terms%p0_name      =  'P0'
      formula_terms%p0_long_name = 'reference pressure'
      formula_terms%p0_units     =  'Pa'
      formula_terms%p0_value     =  ps0
      formula_terms%ps_name      =  'PS'

      call add_vert_coord('lev', pver,                                       &
         'hybrid level at midpoints (1000*(A+B))', 'hPa', alev,              &
         positive='down',                                                    &
         standard_name='atmosphere_hybrid_sigma_pressure_coordinate',        &
         formula_terms=formula_terms)
   end if

   if (dry_coord) then
      call add_vert_coord('ilev', pverp,                                     &
         'hybrid level at interfaces (1000*(A+B))', 'hPa', ailev,            &
         positive='down')
      call add_hist_coord('hyai', pverp, &
         'hybrid A coefficient at layer interfaces', '1', hyai, dimname='ilev')
      call add_hist_coord('hybi', pverp, &
         'hybrid B coefficient at layer interfaces', '1', hybi, dimname='ilev')
   else
      formula_terms%a_name       =  'hyai'
      formula_terms%a_long_name  =  'hybrid A coefficient at layer interfaces'
      formula_terms%a_values     => hyai
      formula_terms%b_name       =  'hybi'
      formula_terms%b_long_name  =  'hybrid B coefficient at layer interfaces'
      formula_terms%b_values     => hybi
      formula_terms%p0_name      =  'P0'
      formula_terms%p0_long_name = 'reference pressure'
      formula_terms%p0_units     =  'Pa'
      formula_terms%p0_value     =  ps0
      formula_terms%ps_name      =  'PS'

      call add_vert_coord('ilev', pverp,                                     &
         'hybrid level at interfaces (1000*(A+B))', 'hPa', ailev,            &
         positive='down',                                                    &
         standard_name='atmosphere_hybrid_sigma_pressure_coordinate',        &
         formula_terms=formula_terms)
   end if

   if (masterproc) then
      write(iulog,'(a)')' Layer Locations (*1000) '
      do k=1,pver
         write(iulog,9800)k,hyai(k),hybi(k),hyai(k)+hybi(k)
         write(iulog,9810) hyam(k), hybm(k), hyam(k)+hybm(k)
      end do

      write(iulog,9800)pverp,hyai(pverp),hybi(pverp),hyai(pverp)+hybi(pverp)
      write(iulog,9820)
      do k=1,pver
         write(iulog,9830) k, hypi(k)
         write(iulog,9840) hypm(k), hypd(k)
      end do
      write(iulog,9830) pverp, hypi(pverp)
    end if

    ! Mark etamid (input name) as initialized (by standard name sum_of_sigma_...)
    call mark_as_initialized( &
      'sum_of_sigma_pressure_hybrid_coordinate_a_coefficient_and_sigma_pressure_hybrid_coordinate_b_coefficient')


9800 format( 1x, i3, 3p, 3(f10.4,10x) )
9810 format( 1x, 3x, 3p, 3(10x,f10.4) )
9820 format(1x,'reference pressures (Pa)')
9830 format(1x,i3,f15.4)
9840 format(1x,3x,15x,2f15.4)
9850 format('HYCOEF: A and/or B vertical level coefficients at full',/, &
         ' levels are not the arithmetic mean of half-level values')

end subroutine hycoef_init

!=======================================================================

subroutine init_restart_hycoef(File, vdimids)

   type(file_desc_t), intent(inout) :: File
   integer,           intent(out)   :: vdimids(:)

   ! PIO traps errors internally, no need to check ierr

   integer :: ierr

   ierr = PIO_Def_Dim(File, 'lev',  pver,  vdimids(1))
   ierr = PIO_Def_Dim(File, 'ilev', pverp, vdimids(2))

   ierr = pio_def_var(File, 'hyai', pio_double, vdimids(2:2), hyai_desc)
   ierr = pio_def_var(File, 'hyam', pio_double, vdimids(1:1), hyam_desc)
   ierr = pio_def_var(File, 'hybi', pio_double, vdimids(2:2), hybi_desc)
   ierr = pio_def_var(File, 'hybm', pio_double, vdimids(1:1), hybm_desc)

   ierr = pio_def_var(File, 'P0', pio_double, p0_desc)

end subroutine init_restart_hycoef

!=======================================================================

subroutine write_restart_hycoef(file)

   type(file_desc_t), intent(inout) :: File

   ! PIO traps errors internally, no need to check ierr

   integer :: ierr

   ierr = pio_put_var(File, hyai_desc, hyai)
   ierr = pio_put_var(File, hyam_desc, hyam)
   ierr = pio_put_var(File, hybi_desc, hybi)
   ierr = pio_put_var(File, hybm_desc, hybm)

   ierr = pio_put_var(File, p0_desc,   ps0)

end subroutine write_restart_hycoef

!=======================================================================

subroutine hycoef_read(File)

   ! This code is used both for initial and restart reading.

   type(file_desc_t), intent(inout) :: File

   integer :: flev, filev, lev_dimid, ierr
   integer :: pio_errtype

   type(var_desc_t) :: p0_desc

   character(len=*), parameter :: routine = 'hycoef_read'
   !----------------------------------------------------------------------------

   ! Set PIO to return error codes.
   call pio_seterrorhandling(file, PIO_BCAST_ERROR, pio_errtype)

   ! PIO traps errors internally, no need to check ierr

   ierr = PIO_Inq_DimID(File, 'lev', lev_dimid)
   if (ierr /= PIO_NOERR) then
      ierr = PIO_Inq_DimID(File, 'reference_pressure_in_atmosphere_layer', lev_dimid)
      if (ierr /= PIO_NOERR) then
         call endrun(routine//': ERROR: unable to find lev dimension in ncdata or restart file.')
      end if
   end if
   ierr = PIO_Inq_dimlen(File, lev_dimid, flev)
   if (ierr /= PIO_NOERR) then
      call endrun(routine//': ERROR: Failed to inquire dimension length of flev in ncdata or restart file.')
   end if
   if (pver /= flev) then
      write(iulog,*) routine//': ERROR: file lev does not match model. lev (file, model):',flev, pver
      call endrun(routine//': ERROR: file lev does not match model.')
   end if

   ierr = PIO_Inq_DimID(File, 'ilev', lev_dimid)
   if (ierr /= PIO_NOERR) then
      ierr = PIO_Inq_DimID(File, 'reference_pressure_in_atmosphere_layer_at_interfaces', lev_dimid)
      if (ierr /= PIO_NOERR) then
         call endrun(routine//': ERROR: unable to find ilev dimension in ncdata or restart file')
      end if
   end if
   ierr = PIO_Inq_dimlen(File, lev_dimid, filev)
   if (ierr /= PIO_NOERR) then
      call endrun(routine//': ERROR: Failed to inquire dimension length of filev in ncdata or restart file.')
   end if
   if (pverp /= filev) then
      write(iulog,*) routine//':ERROR: file ilev does not match model ilev (file, model):',filev, pverp
      call endrun(routine//':ERROR: file ilev does not match model.')
   end if

   ierr = pio_inq_varid(File, 'hyai', hyai_desc)
   if (ierr /= PIO_NOERR) then
      ierr = pio_inq_varid(File, 'sigma_pressure_hybrid_coordinate_a_coefficient_at_interfaces', hyai_desc)
      if (ierr /= PIO_NOERR) then
         call endrun(routine//': ERROR: unable to find hyai variable in ncdata or restart file')
      end if
   end if

   ierr = pio_inq_varid(File, 'hyam', hyam_desc)
   if (ierr /= PIO_NOERR) then
      ierr = pio_inq_varid(File, 'sigma_pressure_hybrid_coordinate_a_coefficient', hyam_desc)
      if (ierr /= PIO_NOERR) then
         call endrun(routine//': ERROR: unable to find hyam variable in ncdata or restart file')
      end if
   end if

   ierr = pio_inq_varid(File, 'hybi', hybi_desc)
   if (ierr /= PIO_NOERR) then
      ierr = pio_inq_varid(File, 'sigma_pressure_hybrid_coordinate_b_coefficient_at_interfaces', hybi_desc)
      if (ierr /= PIO_NOERR) then
         call endrun(routine//': ERROR: unable to find hybi variable in ncdata or restart file')
      end if
   end if

   ierr = pio_inq_varid(File, 'hybm', hybm_desc)
   if (ierr /= PIO_NOERR) then
      ierr = pio_inq_varid(File, 'sigma_pressure_hybrid_coordinate_b_coefficient', hybm_desc)
      if (ierr /= PIO_NOERR) then
         call endrun(routine//': ERROR: unable to find hybm variable in ncdata or restart file')
      end if
   end if

   ierr = pio_get_var(File, hyai_desc, hyai)
   if (ierr /= PIO_NOERR) then
     call endrun(routine//': ERROR: Unable to get hyai variable in ncdata or restart file.')
   end if
   ierr = pio_get_var(File, hybi_desc, hybi)
   if (ierr /= PIO_NOERR) then
     call endrun(routine//': ERROR: Unable to get hybi variable in ncdata or restart file.')
   end if
   ierr = pio_get_var(File, hyam_desc, hyam)
   if (ierr /= PIO_NOERR) then
     call endrun(routine//': ERROR: Unable to get hyam variable in ncdata or restart file.')
   end if
   ierr = pio_get_var(File, hybm_desc, hybm)
   if (ierr /= PIO_NOERR) then
     call endrun(routine//': ERROR: Unable to get hybm variable in ncdata or restart file.')
   end if

   if (masterproc) then
      write(iulog,*) routine//': read hyai, hybi, hyam, hybm'
   end if

   ! Check whether file contains value for P0.  If it does then use it
   ierr = pio_inq_varid(file, 'P0', p0_desc)
   if (ierr /= PIO_NOERR) then
      ierr = pio_inq_varid(File, 'surface_reference_pressure', p0_desc)
   end if
   if (ierr == PIO_NOERR) then
      ierr = pio_get_var(file, p0_desc, ps0)
      if (ierr /= PIO_NOERR) then
         call endrun(routine//': reading P0.')
      end if
      if (masterproc) then
         write(iulog,*) routine//': read P0 value: ', ps0
      end if
   end if

   ! Put the error handling back the way it was
   call pio_seterrorhandling(file, pio_errtype)

#if ( defined OFFLINE_DYN )
   ! make sure top interface is non zero for fv dycore
   if (hyai(1) .eq. 0._r8) then
      if (hybm(1) .ne. 0.0_r8) then
         hyai(1) = hybm(1)*1.e-2_r8
      else if (hyam(1) .ne. 0.0_r8) then
         hyai(1) = hyam(1)*1.e-2_r8
      else
         call endrun('Not able to set hyai(1) to non-zero.')
      end if
   end if
#endif

end subroutine hycoef_read

!=======================================================================

end module hycoef
