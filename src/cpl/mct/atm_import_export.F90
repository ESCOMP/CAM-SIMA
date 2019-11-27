module atm_import_export

   use shr_kind_mod, only: r8 => shr_kind_r8, cl => shr_kind_cl
   use time_manager, only: get_nstep
   use cam_logfile,  only: iulog
   use spmd_utils,   only: masterproc

   implicit none
   private
   save

   ! Public interfaces
   public :: atm_import
   public :: atm_export

   ! Private module data
   integer, parameter :: debug = 0 ! internal debug level
   character(len=*), parameter :: modname = '(cam_import_export) '
   character(len=*), parameter :: F01 = "("//modname//",a, i8,2x,i8,2x,d21.14)"

!============================================================================
CONTAINS
!============================================================================

   subroutine atm_import(x2a, cam_in, restart_init)

      !-----------------------------------------------------------------------
      use cam_cpl_indices ! Temporary exception to naked use
      use camsrfexch,        only: cam_in_t
      use shr_const_mod,     only: shr_const_stebol
      use shr_sys_mod,       only: shr_sys_abort
      use time_manager,      only: is_first_step
      use physics_grid,      only: columns_on_task
!!XXgoldyXX: v figure out what to do with constituents
!      use seq_drydep_mod,    only: n_drydep
!      use shr_fire_emis_mod, only: shr_fire_emis_mechcomps_n
!      use co2_cycle,         only: c_i, co2_readFlux_ocn, co2_readFlux_fuel
!      use co2_cycle,         only: co2_transport, co2_time_interp_ocn
!      use co2_cycle,         only: co2_time_interp_fuel
!      use co2_cycle,         only: data_flux_ocn, data_flux_fuel
!      use physconst,         only: mwco2
!!XXgoldyXX: ^ figure out what to do with constituents
      !
      ! Arguments
      !
      real(r8),          intent(in)    :: x2a(:,:)
      type(cam_in_t),    intent(inout) :: cam_in
      logical, optional, intent(in)    :: restart_init
      !
      ! Local variables
      !
      integer            :: index
      logical, save      :: first_time = .true.
      integer, parameter :: ndst = 2
      integer, target    :: spc_ndx(ndst)
      integer, pointer   :: dst_a5_ndx, dst_a7_ndx
      integer, pointer   :: dst_a1_ndx, dst_a3_ndx
      integer            :: nstep
      logical            :: overwrite_flds
      !-----------------------------------------------------------------------

      overwrite_flds = .true.
      ! don't overwrite fields if invoked during the initialization phase
      ! of a 'continue' or 'branch' run type with data from .rs file
      if (present(restart_init)) then
         overwrite_flds = .not. restart_init
      end if

!!XXgoldyXX: v figure out what to do with constituents
#if 0
      ! CESM sign convention is that fluxes are positive downward
      do index = 1, columns_on_task
            if (overwrite_flds) then
               cam_in%wsx(index)    = -x2a(index_x2a_Faxx_taux, index)
               cam_in%wsy(index)    = -x2a(index_x2a_Faxx_tauy, index)
               cam_in%shf(index)    = -x2a(index_x2a_Faxx_sen,  index)
               cam_in%cflx(index,1) = -x2a(index_x2a_Faxx_evap, index)
            end if
            cam_in%lhf(index)       = -x2a(index_x2a_Faxx_lat,  index)
            cam_in%lwup(index)      = -x2a(index_x2a_Faxx_lwup, index)
            cam_in%asdir(index)     =  x2a(index_x2a_Sx_avsdr,  index)
            cam_in%aldir(index)     =  x2a(index_x2a_Sx_anidr,  index)
            cam_in%asdif(index)     =  x2a(index_x2a_Sx_avsdf,  index)
            cam_in%aldif(index)     =  x2a(index_x2a_Sx_anidf,  index)
            cam_in%ts(index)        =  x2a(index_x2a_Sx_t,      index)
            cam_in%sst(index)       =  x2a(index_x2a_So_t,      index)
            cam_in%snowhland(index) =  x2a(index_x2a_Sl_snowh,  index)
            cam_in%snowhice(index)  =  x2a(index_x2a_Si_snowh,  index)
            cam_in%tref(index)      =  x2a(index_x2a_Sx_tref,   index)
            cam_in%qref(index)      =  x2a(index_x2a_Sx_qref,   index)
            cam_in%u10(index)       =  x2a(index_x2a_Sx_u10,    index)
            cam_in%icefrac(index)   =  x2a(index_x2a_Sf_ifrac,  index)
            cam_in%ocnfrac(index)   =  x2a(index_x2a_Sf_ofrac,  index)
            cam_in%landfrac(index)  =  x2a(index_x2a_Sf_lfrac,  index)

            if ( associated(cam_in%ram1) ) &
                 cam_in%ram1(index) =  x2a(index_x2a_Sl_ram1, index)
            if ( associated(cam_in%fv) ) &
                 cam_in%fv(index)   =  x2a(index_x2a_Sl_fv, index)
            if ( associated(cam_in%soilw) ) &
                 cam_in%soilw(index) =  x2a(index_x2a_Sl_soilw, index)
            if ( associated(cam_in%dstflx) ) then
               cam_in%dstflx(index,1) = x2a(index_x2a_Fall_flxdst1, index)
               cam_in%dstflx(index,2) = x2a(index_x2a_Fall_flxdst2, index)
               cam_in%dstflx(index,3) = x2a(index_x2a_Fall_flxdst3, index)
               cam_in%dstflx(index,4) = x2a(index_x2a_Fall_flxdst4, index)
            endif
            if ( associated(cam_in%meganflx) ) then
               cam_in%meganflx(i,1:shr_megan_mechcomps_n) = &
                    x2a(index_x2a_Fall_flxvoc:index_x2a_Fall_flxvoc+shr_megan_mechcomps_n-1, index)
            endif

            ! Fire emission fluxes
            if ( associated(cam_in%fireflx) .and. associated(cam_in%fireztop) ) then
               cam_in%fireflx(i,:shr_fire_emis_mechcomps_n) = &
                    x2a(index_x2a_Fall_flxfire:index_x2a_Fall_flxfire+shr_fire_emis_mechcomps_n-1, index)
               cam_in%fireztop(index) = x2a(index_x2a_Sl_ztopfire, index)
            endif

            ! dry dep velocities
            if ( index_x2a_Sl_ddvel/= 0 .and. n_drydep>0 ) then
               cam_in%depvel(index,:n_drydep) = &
                    x2a(index_x2a_Sl_ddvel:index_x2a_Sl_ddvel+n_drydep-1, index)
            endif
            !
            ! fields needed to calculate water isotopes to ocean evaporation processes
            !
            cam_in%ustar(index) = x2a(index_x2a_So_ustar,index)
            cam_in%re(index)    = x2a(index_x2a_So_re,index)
            cam_in%ssq(index)   = x2a(index_x2a_So_ssq,index)
            !
            ! bgc scenarios
            !
            if (index_x2a_Fall_fco2_lnd /= 0) then
               cam_in%fco2_lnd(index) = -x2a(index_x2a_Fall_fco2_lnd,index)
            end if
            if (index_x2a_Faoo_fco2_ocn /= 0) then
               cam_in%fco2_ocn(index) = -x2a(index_x2a_Faoo_fco2_ocn,index)
            end if
            if (index_x2a_Faoo_fdms_ocn /= 0) then
               cam_in%fdms(index)     = -x2a(index_x2a_Faoo_fdms_ocn,index)
            end if
      end do

      ! Get total co2 flux from components,
      ! Note - co2_transport determines if cam_in%cflx(i,c_i(1:4)) is
      !        allocated
      if (co2_transport().and.overwrite_flds) then

         ! Interpolate in time for flux data read in
         if (co2_readFlux_ocn) then
            call co2_time_interp_ocn
         end if
         if (co2_readFlux_fuel) then
            call co2_time_interp_fuel
         end if

         ! from ocn : data read in or from coupler or zero
         ! from fuel: data read in or zero
         ! from lnd : through coupler or zero
         do c = begchunk,endchunk
            ncols = get_ncols_p(c)
            do i = 1,ncols

               ! all co2 fluxes in unit kgCO2/m2/s ! co2 flux from ocn
               if (index_x2a_Faoo_fco2_ocn /= 0) then
                  cam_in%cflx(i,c_i(1)) = cam_in%fco2_ocn(index)
               else if (co2_readFlux_ocn) then
                  ! convert from molesCO2/m2/s to kgCO2/m2/s
                  cam_in%cflx(i,c_i(1)) = &
                       -data_flux_ocn%co2flx(i,c)*(1._r8- cam_in%landfrac(index)) &
                       *mwco2*1.0e-3_r8
               else
                  cam_in%cflx(i,c_i(1)) = 0._r8
               end if

               ! co2 flux from fossil fuel
               if (co2_readFlux_fuel) then
                  cam_in%cflx(i,c_i(2)) = data_flux_fuel%co2flx(i,c)
               else
                  cam_in%cflx(i,c_i(2)) = 0._r8
               end if

               ! co2 flux from land (cpl already multiplies flux by land fraction)
               if (index_x2a_Fall_fco2_lnd /= 0) then
                  cam_in%cflx(i,c_i(3)) = cam_in%fco2_lnd(index)
               else
                  cam_in%cflx(i,c_i(3)) = 0._r8
               end if

               ! merged co2 flux
               cam_in%cflx(i,c_i(4)) = cam_in%cflx(i,c_i(1)) + &
                    cam_in%cflx(i,c_i(2)) + &
                    cam_in%cflx(i,c_i(3))
            end do
         end do
      end if
      !
      ! if first step, determine longwave up flux from the surface temperature
      !
      if (first_time) then
         if (is_first_step()) then
            do c = begchunk, endchunk
               ncols = get_ncols_p(c)
               do i = 1,ncols
                  cam_in%lwup(index) = shr_const_stebol*(cam_in%ts(index)**4)
               end do
            end do
         end if
         first_time = .false.
      end if

      !-----------------------------------------------------------------
      ! Debug output
      !-----------------------------------------------------------------

      if (debug > 0 .and. masterproc) then
         nstep = get_nstep()
         ig = 1
         do c = begchunk, endchunk
            ncols = get_ncols_p(c)
            do i = 1,ncols
               write(iulog,F01)'import: nstep, ig, Faxx_tauy = ',nstep,ig,x2a(index_x2a_Faxx_tauy,ig)
               write(iulog,F01)'import: nstep, ig, Faxx_taux = ',nstep,ig,x2a(index_x2a_Faxx_taux,ig)
               write(iulog,F01)'import: nstep, ig, Faxx_shf  = ',nstep,ig,x2a(index_x2a_Faxx_sen,ig)
               write(iulog,F01)'import: nstep, ig, Faxx_lhf  = ',nstep,ig,x2a(index_x2a_Faxx_lat,ig)
               write(iulog,F01)'import: nstep, ig, Sx_asdir  = ',nstep,ig,x2a(index_x2a_Sx_avsdr,ig)
               write(iulog,F01)'import: nstep, ig, Sx_aldir  = ',nstep,ig,x2a(index_x2a_Sx_anidr,ig)
               write(iulog,F01)'import: nstep, ig, Sx_asdif  = ',nstep,ig,x2a(index_x2a_Sx_avsdf,ig)
               write(iulog,F01)'import: nstep, ig, Sx_aldif  = ',nstep,ig,x2a(index_x2a_Sx_anidf,ig)
               write(iulog,F01)'import: nstep, ig, Sx_t      = ',nstep,ig,x2a(index_x2a_Sx_t,ig)
               write(iulog,F01)'import: nstep, ig, Sl_snowh  = ',nstep,ig,x2a(index_x2a_Sl_snowh,ig)
               write(iulog,F01)'import: nstep, ig, Si_snowh  = ',nstep,ig,x2a(index_x2a_Si_snowh,ig)
               write(iulog,F01)'import: nstep, ig, Sf_ifrac  = ',nstep,ig,x2a(index_x2a_Sf_ifrac,ig)
               write(iulog,F01)'import: nstep, ig, Sf_ofrac  = ',nstep,ig,x2a(index_x2a_Sf_ofrac,ig)
               write(iulog,F01)'import: nstep, ig, Sf_lfrac  = ',nstep,ig,x2a(index_x2a_Sf_lfrac,ig)
               if (.not. first_time .and. .not. is_first_step()) then
                  write(iulog,F01)'import: nstep, ig, Faxa_lwup = ',nstep,ig,x2a(index_x2a_Faxx_lwup, ig)
               else
                  write(iulog,F01)'import: nstep, ig, Faxa_lwup = ',nstep,ig,cam_in%lwup(index)
               end if
               ig = ig + 1
            end do
         end do
      end if
#endif
!!XXgoldyXX: ^ figure out what to do with constituents

   end subroutine atm_import

   !============================================================================

   subroutine atm_export(cam_out, a2x)

      !-------------------------------------------------------------------
      use camsrfexch,   only: cam_out_t
      use physics_grid, only: columns_on_task
      use cam_cpl_indices  ! Temporary exception to naked use
      !
      ! Arguments
      !
      type(cam_out_t), intent(in)    :: cam_out
      real(r8),        intent(inout) :: a2x(:,:)
      !
      ! Local variables
      !
      integer :: avsize, avnat
      integer :: index
      integer :: nstep
      !-----------------------------------------------------------------------

      ! Copy from component arrays into chunk array data structure
      ! Rearrange data from chunk structure into lat-lon buffer and subsequently
      ! create attribute vector

!!XXgoldyXX: v figure out what to do with constituents
#if 0
      do index = 1, columns_on_task
         a2x(index_a2x_Sa_pslv,ig) = cam_out%psl(i)
            a2x(index_a2x_Sa_z,ig) = cam_out%zbot(i)
            a2x(index_a2x_Sa_topo,ig) = cam_out%topo(i)
            a2x(index_a2x_Sa_u,ig) = cam_out%ubot(i)
            a2x(index_a2x_Sa_v,ig) = cam_out%vbot(i)
            a2x(index_a2x_Sa_tbot,ig) = cam_out%tbot(i)
            a2x(index_a2x_Sa_ptem,ig) = cam_out%thbot(i)
            a2x(index_a2x_Sa_pbot,ig) = cam_out%pbot(i)
            a2x(index_a2x_Sa_shum,ig) = cam_out%qbot(i,1)
            a2x(index_a2x_Sa_dens,ig) = cam_out%rho(i)
            a2x(index_a2x_Faxa_swnet,ig) = cam_out%netsw(i)
            a2x(index_a2x_Faxa_lwdn,ig) = cam_out%flwds(i)
            a2x(index_a2x_Faxa_rainc,ig) = (cam_out%precc(i)-cam_out%precsc(i))*1000._r8
            a2x(index_a2x_Faxa_rainl,ig) = (cam_out%precl(i)-cam_out%precsl(i))*1000._r8
            a2x(index_a2x_Faxa_snowc,ig) = cam_out%precsc(i)*1000._r8
            a2x(index_a2x_Faxa_snowl,ig) = cam_out%precsl(i)*1000._r8
            a2x(index_a2x_Faxa_swndr,ig) = cam_out%soll(i)
            a2x(index_a2x_Faxa_swvdr,ig) = cam_out%sols(i)
            a2x(index_a2x_Faxa_swndf,ig) = cam_out%solld(i)
            a2x(index_a2x_Faxa_swvdf,ig) = cam_out%solsd(i)

            ! aerosol deposition fluxes
            a2x(index_a2x_Faxa_bcphidry,ig) = cam_out%bcphidry(i)
            a2x(index_a2x_Faxa_bcphodry,ig) = cam_out%bcphodry(i)
            a2x(index_a2x_Faxa_bcphiwet,ig) = cam_out%bcphiwet(i)
            a2x(index_a2x_Faxa_ocphidry,ig) = cam_out%ocphidry(i)
            a2x(index_a2x_Faxa_ocphodry,ig) = cam_out%ocphodry(i)
            a2x(index_a2x_Faxa_ocphiwet,ig) = cam_out%ocphiwet(i)
            a2x(index_a2x_Faxa_dstwet1,ig)  = cam_out%dstwet1(i)
            a2x(index_a2x_Faxa_dstdry1,ig)  = cam_out%dstdry1(i)
            a2x(index_a2x_Faxa_dstwet2,ig)  = cam_out%dstwet2(i)
            a2x(index_a2x_Faxa_dstdry2,ig)  = cam_out%dstdry2(i)
            a2x(index_a2x_Faxa_dstwet3,ig)  = cam_out%dstwet3(i)
            a2x(index_a2x_Faxa_dstdry3,ig)  = cam_out%dstdry3(i)
            a2x(index_a2x_Faxa_dstwet4,ig)  = cam_out%dstwet4(i)
            a2x(index_a2x_Faxa_dstdry4,ig)  = cam_out%dstdry4(i)

            if (index_a2x_Sa_co2prog /= 0) then
               a2x(index_a2x_Sa_co2prog,ig) = cam_out%co2prog(i) ! atm prognostic co2
            end if
            if (index_a2x_Sa_co2diag /= 0) then
               a2x(index_a2x_Sa_co2diag,ig) = cam_out%co2diag(i) ! atm diagnostic co2
            end if
            if (index_a2x_Faxa_nhx > 0 ) then
               a2x(index_a2x_Faxa_nhx,ig) = cam_out%nhx_nitrogen_flx(i)
            endif
            if (index_a2x_Faxa_noy > 0 ) then
               a2x(index_a2x_Faxa_noy,ig) = cam_out%noy_nitrogen_flx(i)
            endif

            ig = ig + 1
         end do
      end do

      !-----------------------------------------------------------------
      ! Debug output
      !-----------------------------------------------------------------

      if (debug > 0 .and. masterproc) then
         nstep = get_nstep()
         ig = 1
         do c = begchunk, endchunk
            ncols = get_ncols_p(c)
            do i = 1,ncols
               write(iulog,F01)'export: nstep, ig, Sa_z          = ',nstep,ig,a2x(index_a2x_Sa_z,ig)
               write(iulog,F01)'export: nstep, ig, Sa_topo       = ',nstep,ig,a2x(index_a2x_Sa_topo,ig)
               write(iulog,F01)'export: nstep, ig, Sa_u          = ',nstep,ig,a2x(index_a2x_Sa_u,ig)
               write(iulog,F01)'export: nstep, ig, Sa_v          = ',nstep,ig,a2x(index_a2x_Sa_v,ig)
               write(iulog,F01)'export: nstep, ig, Sa_tbot       = ',nstep,ig,a2x(index_a2x_Sa_tbot,ig)
               write(iulog,F01)'export: nstep, ig, Sa_ptem       = ',nstep,ig,a2x(index_a2x_Sa_ptem,ig)
               write(iulog,F01)'export: nstep, ig, Sa_pbot       = ',nstep,ig,a2x(index_a2x_Sa_pbot,ig)
               write(iulog,F01)'export: nstep, ig, Sa_shum       = ',nstep,ig,a2x(index_a2x_Sa_shum,ig)
               write(iulog,F01)'export: nstep, ig, Sa_dens       = ',nstep,ig,a2x(index_a2x_Sa_dens,ig)
               write(iulog,F01)'export: nstep, ig, Faxa_swnet    = ',nstep,ig,a2x(index_a2x_Faxa_swnet,ig)
               write(iulog,F01)'export: nstep, ig, Faxa_lwdn     = ',nstep,ig,a2x(index_a2x_Faxa_lwdn,ig)
               write(iulog,F01)'export: nstep, ig, Faxa_rainc    = ',nstep,ig,a2x(index_a2x_Faxa_rainc,ig)
               write(iulog,F01)'export: nstep, ig, Faxa_rainl    = ',nstep,ig,a2x(index_a2x_Faxa_rainl,ig)
               write(iulog,F01)'export: nstep, ig, Faxa_snowc    = ',nstep,ig,a2x(index_a2x_Faxa_snowc,ig)
               write(iulog,F01)'export: nstep, ig, Faxa_snowl    = ',nstep,ig,a2x(index_a2x_Faxa_snowl,ig)
               write(iulog,F01)'export: nstep, ig, Faxa_swndr    = ',nstep,ig,a2x(index_a2x_Faxa_swndr,ig)
               write(iulog,F01)'export: nstep, ig, Faxa_swvdr    = ',nstep,ig,a2x(index_a2x_Faxa_swvdr,ig)
               write(iulog,F01)'export: nstep, ig, Faxa_swndf    = ',nstep,ig,a2x(index_a2x_Faxa_swndf,ig)
               write(iulog,F01)'export: nstep, ig, Faxa_swvdf    = ',nstep,ig,a2x(index_a2x_Faxa_swvdf,ig)
               write(iulog,F01)'export: nstep, ig, Faxa_bcphidry = ',nstep,ig,a2x(index_a2x_Faxa_bcphidry,ig)
               write(iulog,F01)'export: nstep, ig, Faxa_bcphodry = ',nstep,ig,a2x(index_a2x_Faxa_bcphodry,ig)
               write(iulog,F01)'export: nstep, ig, Faxa_bcphiwet = ',nstep,ig,a2x(index_a2x_Faxa_bcphiwet,ig)
               write(iulog,F01)'export: nstep, ig, Faxa_ocphidry = ',nstep,ig,a2x(index_a2x_Faxa_ocphidry,ig)
               write(iulog,F01)'export: nstep, ig, Faxa_ocphodry = ',nstep,ig,a2x(index_a2x_Faxa_ocphodry,ig)
               write(iulog,F01)'export: nstep, ig, Faxa_ocphidry = ',nstep,ig,a2x(index_a2x_Faxa_ocphiwet,ig)
               write(iulog,F01)'export: nstep, ig, Faxa_dstwet1  = ',nstep,ig,a2x(index_a2x_Faxa_dstwet1,ig)
               write(iulog,F01)'export: nstep, ig, Faxa_dstwet1  = ',nstep,ig,a2x(index_a2x_Faxa_dstdry1,ig)
               write(iulog,F01)'export: nstep, ig, Faxa_dstwet1  = ',nstep,ig,a2x(index_a2x_Faxa_dstwet2,ig)
               write(iulog,F01)'export: nstep, ig, Faxa_dstwet1  = ',nstep,ig,a2x(index_a2x_Faxa_dstdry2,ig)
               write(iulog,F01)'export: nstep, ig, Faxa_dstwet1  = ',nstep,ig,a2x(index_a2x_Faxa_dstwet3,ig)
               write(iulog,F01)'export: nstep, ig, Faxa_dstwet1  = ',nstep,ig,a2x(index_a2x_Faxa_dstdry3,ig)
               write(iulog,F01)'export: nstep, ig, Faxa_dstwet1  = ',nstep,ig,a2x(index_a2x_Faxa_dstwet4,ig)
               write(iulog,F01)'export: nstep, ig, Faxa_dstwet1  = ',nstep,ig,a2x(index_a2x_Faxa_dstdry4,ig)
               if (index_a2x_Sa_co2prog /= 0) then
                  write(iulog,F01)'export: nstep, ig, Sa_co2prog = ',nstep,ig,a2x(index_a2x_Sa_co2prog,ig)
               end if
               if (index_a2x_Sa_co2diag /= 0) then
                  write(iulog,F01)'export: nstep, ig, Sa_co2diag  = ',nstep,ig,a2x(index_a2x_Sa_co2diag,ig)
               end if
               if (index_a2x_Faxa_nhx > 0 ) then
                  write(iulog,F01)'export: nstep, ig, Faxa_nhx    = ',nstep,ig,a2x(index_a2x_Faxa_nhx,ig)
               endif
               if (index_a2x_Faxa_noy > 0 ) then
                  write(iulog,F01)'export: nstep, ig, Faxa_noy    = ',nstep,ig,a2x(index_a2x_Faxa_noy,ig)
               endif
               ig = ig + 1
            end do
         end do
      end if
#endif
!!XXgoldyXX: ^ figure out what to do with constituents

   end subroutine atm_export

end module atm_import_export
