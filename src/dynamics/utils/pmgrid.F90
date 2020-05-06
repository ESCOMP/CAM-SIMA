module pmgrid

! CACNOTE -- This was copied from SE and hardwired the plev value -- WILL NEED TO BE CHANGED

! PLON and PLAT do not correspond to the number of latitudes and longitudes in
! this version of dynamics.

implicit none
save

integer, parameter :: plev   = 30      ! number of vertical levels
integer, parameter :: plevp  = plev + 1

integer, parameter :: plon   = 1
integer, parameter :: plat   = 1

end module pmgrid
