! Copyright (C) 2024-2025 University Corporation for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
module musica_sima_namelist
!--------------------------------------------------------------------------
!
! This module provides namelist functionality for the "musica_ccpp_dependencies.F90"
! file, which can't easily contain namelist read functionality directly due to CCPP
! requirements.
!
! IMPORTANT: This module should be removed once "musica_ccpp_dependencies.F90",
!            which is a temporary file, has also been removed.
!
!--------------------------------------------------------------------------

  use shr_kind_mod, only: shr_kind_cl
  use runtime_obj,  only: unset_str

  implicit none
  private

  ! public routines
  public :: musica_ccpp_dependencies_readnl

  ! public variables
  character(len=shr_kind_cl), public :: musica_config_str = unset_str

!==============================================================================
contains
!==============================================================================

  subroutine musica_ccpp_dependencies_readnl(nlfile)

    use shr_nl_mod,     only: find_group_name => shr_nl_find_group_name
    use shr_kind_mod,   only: shr_kind_cm
    use mpi,            only: mpi_character
    use spmd_utils,     only: mpicom
    use cam_logfile,    only: iulog
    use cam_abortutils, only: endrun
    use spmd_utils,     only: masterproc
    use string_utils,   only: to_lower

    !-----------------------------------------------------------------------
    !
    ! Read CAM-SIMA's MUSICA dependencies namelist
    !
    !-----------------------------------------------------------------------

    ! filepath for file containing namelist input
    character(len=*), intent(in) :: nlfile

    ! Local variables
    integer                      :: unitn, ierr
    character(len=*), parameter  :: subname = 'musica_ccpp_dependencies_readnl'
    character(len=shr_kind_cm)   :: errmsg
    character(len=shr_kind_cl)   :: musica_config

    namelist /chemistry_nl/ musica_config

    errmsg = ''

    if (masterproc) then
       open(newunit=unitn, file=trim(nlfile), status='old')
       call find_group_name(unitn, 'chemistry_nl', status=ierr)
       if (ierr == 0) then
          read(unitn, chemistry_nl, iostat=ierr, iomsg=errmsg)
          if (ierr /= 0) then
             call endrun(subname // ':: ERROR reading namelist:' // errmsg)
          end if
       end if
       close(unitn)
    end if

    ! Broadcast namelist variable
    call mpi_bcast(musica_config, len(musica_config), mpi_character, 0, mpicom, ierr)

    ! Set module variable with lower case namelist-provided string
    musica_config_str = to_lower(musica_config)

    ! Print out namelist variables
    if (masterproc) then
      write(iulog,*) subname, ' options:'
      write(iulog,*) '  MUSICA Configuration option: ', trim(musica_config_str)
    endif

  end subroutine musica_ccpp_dependencies_readnl

end module musica_sima_namelist
