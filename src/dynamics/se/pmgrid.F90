module pmgrid

! PLON and PLAT do not correspond to the number of latitudes and longitudes in
! this version of dynamics.

implicit none
save

integer, parameter :: plev   = PLEV      ! number of vertical levels
integer, parameter :: plevp  = plev + 1

end module pmgrid
