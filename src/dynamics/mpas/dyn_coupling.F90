! Copyright (C) 2025 University Corporation for Atmospheric Research (UCAR)
! SPDX-License-Identifier: Apache-2.0

!> This module, part of the MPAS interface, integrates MPAS dynamical core with CAM-SIMA by
!> implementing the necessary APIs and managing their interaction.
!>
!> It implements the bidirectional coupling between dynamics and physics states.
module dyn_coupling
    implicit none

    private
    ! Provide APIs required by CAM-SIMA.
    public :: dynamics_to_physics_coupling
    public :: physics_to_dynamics_coupling

    interface
        module subroutine dynamics_to_physics_coupling()
        end subroutine dynamics_to_physics_coupling

        module subroutine physics_to_dynamics_coupling()
        end subroutine physics_to_dynamics_coupling
    end interface
contains
end module dyn_coupling
