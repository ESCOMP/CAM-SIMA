! Copyright (C) 2025 University Corporation for Atmospheric Research (UCAR)
! SPDX-License-Identifier: Apache-2.0

!> This module, part of the MPAS interface, integrates MPAS dynamical core with CAM-SIMA by
!> implementing the necessary APIs and managing their interaction.
!>
!> It implements the bidirectional coupling between dynamics and physics states.
!> For constituent states, their coupling is handled separately as a special case due to
!> complications in CAM-SIMA.
module dyn_coupling
    implicit none

    private
    ! Provide APIs required by CAM-SIMA.
    public :: dyn_exchange_constituent_states
    public :: dynamics_to_physics_coupling
    public :: physics_to_dynamics_coupling

    interface
        module subroutine dyn_exchange_constituent_states(direction, exchange, conversion)
            implicit none
            character(*), intent(in) :: direction
            logical, intent(in) :: exchange
            logical, intent(in) :: conversion
        end subroutine dyn_exchange_constituent_states

        module subroutine dynamics_to_physics_coupling()
            implicit none
        end subroutine dynamics_to_physics_coupling

        module subroutine physics_to_dynamics_coupling()
            implicit none
        end subroutine physics_to_dynamics_coupling
    end interface
end module dyn_coupling
