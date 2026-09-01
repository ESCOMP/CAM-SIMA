module fvm_overlap_mod
  use shr_kind_mod,   only: r8=>shr_kind_r8
  use cam_logfile,    only: iulog

  implicit none

  real (kind=r8),parameter, private  :: bignum = 1.0e20_r8
  real (kind=r8),parameter, private  :: tiny   = 1.0e-12_r8
  real (kind=r8),parameter, private  :: fuzzy_width = 10.0_r8*tiny

  public :: compute_weights_cell

  private
  integer, parameter :: max_cross = 10
contains
  subroutine compute_weights_cell(nvertex,lexact_horizontal_line_integrals,&
       xcell_in,ycell_in,jx,jy,nreconstruction,xgno,ygno,igno_min,igno_max,&
       jx_min, jx_max, jy_min, jy_max,&
       ngauss,gauss_weights,abscissae,weights,weights_eul_index,jcollect,jmax_segments)
    use cam_abortutils, only: endrun

    integer , intent(in) :: nvertex
    logical, intent(in) :: lexact_horizontal_line_integrals
    integer , intent(in) :: nreconstruction, jx,jy,ngauss,jmax_segments
    !
    ! dimension(nvertex)
    !
    real (kind=r8), intent(in) :: xcell_in(4),ycell_in(4)
    !
    integer , intent(in)               :: jx_min, jy_min, jx_max, jy_max,igno_min,igno_max
    !
    ! dimension(-ihalo:nc+2+ihalo)
    !
    real (kind=r8), intent(in) :: xgno(igno_min:igno_max), ygno(igno_min:igno_max)
    !
    ! for Gaussian quadrature
    !
    real (kind=r8), intent(in) :: gauss_weights(:), abscissae(:) !dimension(ngauss)
    !
    ! Number of Eulerian sub-cell integrals for the cell in question
    !
    integer , intent(out)       :: jcollect
    !
    ! local workspace
    !
    !
    ! max number of line segments is:
    !
    ! (number of longitudes)*(max average number of crossings per line segment = 3)*ncube*2
    !
    real (kind=r8), intent(out) :: weights(jmax_segments,nreconstruction)
    integer,        intent(out) :: weights_eul_index(jmax_segments,2)

    integer :: jsegment
    !
    ! variables for registering crossings with Eulerian latitudes and longitudes
    !
    integer :: jcross_lat
    !
    ! max. crossings per side is 2*ihalo
    !
    real (kind=r8) :: r_cross_lat(max_cross,2)
    integer        :: cross_lat_eul_index(max_cross,2)
    real (kind=r8) :: xcell(nvertex),ycell(nvertex)

    character(len=256) :: errmsg

    xcell = xcell_in(1:nvertex)
    ycell = ycell_in(1:nvertex)

    jsegment   = 0
    weights    = 0.0_r8
    jcross_lat = 0

    call side_integral(lexact_horizontal_line_integrals,xcell,ycell,nvertex,jsegment,jmax_segments,&
         weights,weights_eul_index,nreconstruction,jx,jy,xgno,ygno,igno_min,igno_max,jx_min, jx_max, jy_min, jy_max,&
         ngauss,gauss_weights,abscissae,&
         jcross_lat,r_cross_lat,cross_lat_eul_index)
    !
    !**********************
    !
    ! Do inner integrals
    !
    !**********************
    !
    call compute_inner_line_integrals_lat(lexact_horizontal_line_integrals,&
         r_cross_lat,cross_lat_eul_index,&
         jcross_lat,jsegment,xgno,igno_min,igno_max,jx_min, jx_max, jy_min, jy_max,&
         weights,weights_eul_index,&
         nreconstruction,ngauss,gauss_weights,abscissae)

    if (abs((jcross_lat/2)-dble(jcross_lat)/2.0_r8)>tiny) then
      write(errmsg,*) 'number of latitude crossings are not even: ABORT',jcross_lat,jx,jy
      call endrun(errmsg)
    end if

    !
    ! collect line-segment that reside in the same Eulerian cell
    !
    if (jsegment>0) then
      call collect(weights,weights_eul_index,nreconstruction,jcollect,jsegment,jmax_segments)
    else
      jcollect = 0
    end if
  end subroutine compute_weights_cell
  !
  !****************************************************************************
  !
  ! organize data and store it
  !
  !****************************************************************************
  !
  subroutine collect(weights,weights_eul_index,nreconstruction,jcollect,jsegment,jmax_segments)
    integer,           intent(in) :: jsegment,jmax_segments
    integer,           intent(in) :: nreconstruction
    !
    real (kind=r8), intent(inout) :: weights(:,:) !dimension(jmax_segments,nreconstruction)
    integer,        intent(inout) :: weights_eul_index(:,:) !dimension(jmax_segments,2)
    integer,          intent(out) :: jcollect
    !
    ! local workspace
    !
    integer :: imin, imax, jmin, jmax, i,j,k,h
    logical                 :: ltmp

    real (kind=r8) :: weights_out(jmax_segments,nreconstruction)
    integer :: weights_eul_index_out(jmax_segments,2)

    weights_out           = 0.0_r8
    weights_eul_index_out = -100

    imin = minval(weights_eul_index(1:jsegment,1))
    imax = maxval(weights_eul_index(1:jsegment,1))
    jmin = minval(weights_eul_index(1:jsegment,2))
    jmax = maxval(weights_eul_index(1:jsegment,2))

    ltmp = .false.

    jcollect = 1

    do j=jmin,jmax
       do i=imin,imax
          do k=1,jsegment
             if (weights_eul_index(k,1)==i.and.weights_eul_index(k,2)==j) then
                weights_out(jcollect,1:nreconstruction) = &
                weights_out(jcollect,1:nreconstruction) + weights(k,1:nreconstruction)
                ltmp = .true.
                h = k
             end if
          end do
          if (ltmp) then
             weights_eul_index_out(jcollect,:) = weights_eul_index(h,:)
             jcollect = jcollect+1
          end if
          ltmp = .false.
       end do
    end do
    jcollect = jcollect-1
    weights           = weights_out
    weights_eul_index = weights_eul_index_out
  end subroutine collect
  !
  !*****************************************************************************************
  !
  ! compute crossings with Eulerian latitudes and longitudes
  !
  !*****************************************************************************************
  !
  subroutine compute_inner_line_integrals_lat(lexact_horizontal_line_integrals,r_cross_lat,&
       cross_lat_eul_index,&
       jcross_lat,jsegment,xgno,igno_min,igno_max,jx_min,jx_max,jy_min, jy_max,weights,weights_eul_index,&
       nreconstruction,ngauss,gauss_weights,abscissae)
    logical, intent(in) :: lexact_horizontal_line_integrals
    !
    ! variables for registering crossings with Eulerian latitudes and longitudes
    !
    integer ,         intent(in) :: jcross_lat, nreconstruction,ngauss,igno_min,igno_max
    integer ,         intent(inout) :: jsegment
    !
    ! for Gaussian quadrature
    !
    real (kind=r8),   intent(in) :: gauss_weights(ngauss), abscissae(ngauss)
    !
    ! max. crossings per side is 2*ihalo
    !

    real (kind=r8),   intent(in) :: r_cross_lat(:,:) ! dimension(8*ihalo,2)
    integer,          intent(in) :: cross_lat_eul_index(:,:) ! ! dimension(8*ihalo,2)
    integer,          intent(in) :: jx_min, jx_max, jy_min, jy_max

    real (kind=r8),   intent(in) :: xgno(igno_min:igno_max) !dimension(-ihalo:nc+2+ihalo)
    !
    ! dimension(jmax_segments,nreconstruction)
    !
    real (kind=r8),   intent(inout) :: weights(:,:)
    !
    ! dimension(jmax_segments,2)
    !
    integer,          intent(inout) :: weights_eul_index(:,:)

    real (kind=r8) :: weights_tmp(nreconstruction)
    integer :: imin,imax,i,j,k,h
    real (kind=r8) :: rstart(2),rend(2),rend_tmp(2)
    real (kind=r8) :: xseg(2), yseg(2)
    5   format(10e14.6)
    if (jcross_lat>0) then
       do i=minval(cross_lat_eul_index(1:jcross_lat,2)),maxval(cross_lat_eul_index(1:jcross_lat,2))
          !
          ! find "first" crossing with Eulerian cell i
          !
          first_crossing:do k=1,jcross_lat
             if (cross_lat_eul_index(k,2)==i) exit first_crossing
          end do first_crossing
          do j=k+1,jcross_lat
             !
             ! find "second" crossing with Eulerian cell i
             !
             if (cross_lat_eul_index(j,2)==i) then
                if (r_cross_lat(k,1)<r_cross_lat(j,1)) then
                   rstart = r_cross_lat(k,1:2)
                   rend   = r_cross_lat(j,1:2)
                   imin   = cross_lat_eul_index(k,1)
                   imax   = cross_lat_eul_index(j,1)
                else
                   rstart = r_cross_lat(j,1:2)
                   rend   = r_cross_lat(k,1:2)
                   imin   = cross_lat_eul_index(j,1)
                   imax   = cross_lat_eul_index(k,1)
                end if
                do h=imin,imax
                   if (h==imax) then
                      rend_tmp = rend
                   else
                      rend_tmp(1) = xgno(h+1)
                      rend_tmp(2) = r_cross_lat(k,2)
                   end if
                   xseg(1) = rstart(1)
                   xseg(2) = rend_tmp(1)
                   yseg(1) = rstart(2)
                   yseg(2) = rend_tmp(2)
                   call get_weights_exact(lexact_horizontal_line_integrals, weights_tmp,xseg,yseg,&
                        nreconstruction,ngauss,gauss_weights,abscissae)

                   if (i<=jy_max-1.and.i>=jy_min.and.h<=jx_max-1.and.h>=jx_min) then
                      jsegment=jsegment+1
                      weights_eul_index(jsegment,1) = h
                      weights_eul_index(jsegment,2) = i
                      weights(jsegment,1:nreconstruction) = -weights_tmp
                   end if
                   !
                   ! subtract the same weights on the west side of the line
                   !
                   if (i<=jy_max.and.i>=jy_min+1.and.h<=jx_max-1.and.h>=jx_min) then
                      jsegment = jsegment+1
                      weights_eul_index(jsegment,1) = h
                      weights_eul_index(jsegment,2) = i-1
                      weights(jsegment,1:nreconstruction) = weights_tmp
                   end if
                   !
                   ! prepare for next iteration
                   !
                   rstart = rend_tmp
                end do
             end if
          end do
       end do
    end if
  end subroutine compute_inner_line_integrals_lat

  !
  ! line integral from (a1_in,a2_in) to (b1_in,b2_in)
  ! If line is coniciding with an Eulerian longitude or latitude the routine
  ! needs to know where an adjacent side is located to determine which
  ! reconstruction must be used. therefore (c1,c2) is passed to the routine
  !
  !

  subroutine side_integral(lexact_horizontal_line_integrals,&
       x_in,y_in,nvertex,jsegment,jmax_segments,&
       weights,weights_eul_index,nreconstruction,jx,jy,xgno,ygno,igno_min,igno_max,&
       jx_min,jx_max,jy_min,jy_max,&
       ngauss,gauss_weights,abscissae,&!)!phl add jx_min etc.
       jcross_lat,r_cross_lat,cross_lat_eul_index)
    use cam_abortutils, only: endrun

    logical, intent(in) :: lexact_horizontal_line_integrals
    integer ,            intent(in)    :: nreconstruction,jx,jy,jmax_segments,ngauss
    integer , intent(in)               :: nvertex,igno_min,igno_max
    !
    ! for Gaussian quadrature
    !
    real (kind=r8), intent(in) :: gauss_weights(:), abscissae(:) !dimension(ngauss)
    real (kind=r8), intent(in) :: x_in(:),y_in(:) !dimension(1:nvertex)

    integer ,       intent(in) :: jx_min, jy_min, jx_max, jy_max
    real (kind=r8), intent(in) :: xgno(igno_min:igno_max), ygno(igno_min:igno_max) !dimension(-ihalo:nc+2+ihalo)
    integer ,    intent(inout) :: jsegment
!    integer ,dimension(0:2),intent(in)    :: jx_eul_in, jy_eul_in
    real(kind=r8), intent(out) :: weights(:,:) !dimension(jmax_segments,nreconstruction)
    integer,       intent(out) :: weights_eul_index(jmax_segments,2)

    !
    ! variables for registering crossings with Eulerian latitudes and longitudes
    !
    integer ,         intent(inout) :: jcross_lat
    !
    ! max. crossings per side is 2*ihalo
    !
    real (kind=r8), intent(inout) :: r_cross_lat(max_cross,2)
    integer,        intent(inout) :: cross_lat_eul_index(max_cross,2)
    !
    ! local variables
    !
    real (kind=r8) :: xseg(2),yseg(2)
    real (kind=r8) :: x(0:3),y(0:3)
    real (kind=r8) :: xeul,yeul,xcross,ycross,slope
    integer ::    jx_eul_tmp,jy_eul_tmp
    integer :: xsgn1,ysgn1,xsgn2,ysgn2
    integer :: iter
    logical :: lcontinue, lsame_cell_x, lsame_cell_y

    integer :: jx_eul, jy_eul, side_count
    real (kind=r8) :: xcell(0:nvertex+2),ycell(0:nvertex+2)
    character(len=256) :: errmsg


5   format(10e14.6)
    !
    !***********************************************
    !
    ! find jx_eul and jy_eul for (x(1),y(1))
    !
    !***********************************************
    !
    jx_eul = jx
    jy_eul = jy
    xcell(1:nvertex)=x_in
    ycell(1:nvertex)=y_in
    do iter=1,nvertex
      call truncate_vertex(xcell(iter),jx_eul,xgno,igno_min,igno_max)
      call truncate_vertex(ycell(iter),jy_eul,ygno,igno_min,igno_max)
    end do
    xcell(0) = xcell(nvertex)
    xcell(nvertex+1)=xcell(1)
    xcell(nvertex+2)=xcell(2)
    ycell(0) = ycell(nvertex)
    ycell(nvertex+1)=ycell(1)
    ycell(nvertex+2)=ycell(2)


    if ((&
         maxval(xcell)<=xgno(jx_min).or.minval(xcell)>=xgno(jx_max).or.&
         maxval(ycell)<=ygno(jy_min).or.minval(ycell)>=ygno(jy_max))) then
      !
      ! entire cell off panel
      !
    else
      jx_eul = min(max(jx,jx_min),jx_max-1)
      jy_eul = min(max(jy,jy_min),jy_max-1)
      call which_eul_cell(xcell(1:3),jx_eul,xgno,igno_min,igno_max)
      call which_eul_cell(ycell(1:3),jy_eul,ygno,igno_min,igno_max)

      side_count = 1
      do while (side_count<nvertex+1)
        iter = 0
        lcontinue = .true.
        x(0:3) = xcell(side_count-1:side_count+2)
        y(0:3) = ycell(side_count-1:side_count+2)
        do while (lcontinue)
          iter = iter+1
          if (iter>10) then
            write(errmsg,*) 'search not converging',iter
            call endrun(errmsg)
          end if
          lsame_cell_x = (x(2)>=xgno(jx_eul).and.x(2)<=xgno(jx_eul+1))
          lsame_cell_y = (y(2)>=ygno(jy_eul).and.y(2)<=ygno(jy_eul+1))
          if (lsame_cell_x.and.lsame_cell_y) then
            !
            !****************************
            !
            ! same cell integral
            !
            !****************************
            !
            xseg(1) = x(1)
            yseg(1) = y(1)
            xseg(2) = x(2)
            yseg(2) = y(2)
            jx_eul_tmp = jx_eul
            jy_eul_tmp = jy_eul
            lcontinue = .false.
            !
            ! prepare for next side if (x(2),y(2)) is on a grid line
            !
            if (x(2)==xgno(jx_eul+1).and.x(3)>xgno(jx_eul+1)) then
              !
              ! cross longitude jx_eul+1
              !
              jx_eul=jx_eul+1
            else if (x(2)==xgno(jx_eul).and.x(3)<xgno(jx_eul)) then
              !
              ! cross longitude jx_eul
              !
              jx_eul=jx_eul-1
            end if
            if (y(2)==ygno(jy_eul+1).and.y(3)>ygno(jy_eul+1)) then
              !
              ! register crossing with latitude: line-segments point Northward
              !
              jcross_lat = jcross_lat + 1
              jy_eul     = jy_eul     + 1
              cross_lat_eul_index(jcross_lat,1) = jx_eul
              cross_lat_eul_index(jcross_lat,2) = jy_eul
              r_cross_lat(jcross_lat,1) = x(2)
              r_cross_lat(jcross_lat,2) = y(2)
!              write(*,*) "A register crossing with latitude",x(2),y(2),jx_eul,jy_eul
            else if (y(2)==ygno(jy_eul).and.y(3)<ygno(jy_eul)) then
              !
              ! register crossing with latitude: line-segments point Southward
              !
              jcross_lat = jcross_lat+1
              cross_lat_eul_index(jcross_lat,1) = jx_eul
              cross_lat_eul_index(jcross_lat,2) = jy_eul
              r_cross_lat(jcross_lat,1) = x(2)
              r_cross_lat(jcross_lat,2) = y(2)
!              write(*,*) "B register crossing with latitude",x(2),y(2),jx_eul,jy_eul
              !
              jy_eul=jy_eul-1
            end if
            lcontinue=.false.
          else
            !
            !****************************
            !
            ! not same cell integral
            !
            !****************************
            !
            if (lsame_cell_x) then
              ysgn1 = (1+int(sign(1.0_r8,y(2)-y(1))))/2 !"1" if y(2)>y(1) else "0"
              ysgn2 = int(sign(1.0_r8,y(2)-y(1)))       !"1" if y(2)>y(1) else "-1"
              !
              !*******************************************************************************
              !
              ! there is at least one crossing with latitudes but no crossing with longitudes
              !
              !*******************************************************************************
              !
              yeul   = ygno(jy_eul+ysgn1)
              if (x(1)==x(2)) then
                !
                ! line segment is parallel to longitude (infinite slope)
                !
                xcross = x(1)
              else
                slope  = (y(2)-y(1))/(x(2)-x(1))
                xcross = x_cross_eul_lat(x(1),y(1),yeul,slope)
                !
                ! constrain crossing to be "physically" possible
                !
                xcross = min(max(xcross,xgno(jx_eul)),xgno(jx_eul+1))
                !
                ! debugging
                !
                if (xcross>xgno(jx_eul+1).or.xcross<xgno(jx_eul)) then
                  write(iulog,*) 'xcross is out of range',jx,jy
                  write(iulog,*) 'xcross-xgno(jx_eul+1), xcross-xgno(jx_eul))',&
                       xcross-xgno(jx_eul+1), xcross-ygno(jx_eul)
                  write(errmsg,*) 'xcross is out of range', jx, jy
                  call endrun(errmsg)
                end if
              end if
              xseg(1) = x(1)
              yseg(1) = y(1)
              xseg(2) = xcross
              yseg(2) = yeul
              jx_eul_tmp = jx_eul
              jy_eul_tmp = jy_eul
              !
              ! prepare for next iteration
              !
              x(0) = x(1)
              y(0) = y(1)
              x(1) = xcross
              y(1) = yeul
              jy_eul = jy_eul+ysgn2
              !
              ! register crossing with latitude
              !
              jcross_lat = jcross_lat+1
              cross_lat_eul_index(jcross_lat,1) = jx_eul
              if (ysgn2>0) then
                cross_lat_eul_index(jcross_lat,2) = jy_eul
              else
                cross_lat_eul_index(jcross_lat,2) = jy_eul+1
              end if
              r_cross_lat(jcross_lat,1) = xcross
              r_cross_lat(jcross_lat,2) = yeul
            else if (lsame_cell_y) then
              !
              !*******************************************************************************
              !
              ! there is at least one crossing with longitudes but no crossing with latitudes
              !
              !*******************************************************************************
              !
              xsgn1 = (1+int(sign(1.0_r8,x(2)-x(1))))/2 !"1" if x(2)>x(1) else "0"
              xsgn2 = int(sign(1.0_r8,x(2)-x(1))) !"1" if x(2)>x(1) else "-1"
              xeul   = xgno(jx_eul+xsgn1)
              if (abs(x(2)-x(1))<fuzzy_width) then
                ! fuzzy crossing
                ycross = 0.5_r8*(y(2)-y(1))
              else
                slope  = (y(2)-y(1))/(x(2)-x(1))
                ycross = y_cross_eul_lon(x(1),y(1),xeul,slope)
              end if
              !
              ! constrain crossing to be "physically" possible
              !
              ycross = min(max(ycross,ygno(jy_eul)),ygno(jy_eul+1))
              !
              ! debugging
              !
              if (ycross>ygno(jy_eul+1).or.ycross<ygno(jy_eul)) then
                write(iulog,*) 'ycross is out of range'
                write(iulog,*) 'jx,jy,jx_eul,jy_eul',jx,jy,jx_eul,jy_eul
                write(iulog,*) 'ycross-ygno(jy_eul+1), ycross-ygno(jy_eul))',&
                     ycross-ygno(jy_eul+1), ycross-ygno(jy_eul)
                write(errmsg,*) 'ycross is out of range'
                call endrun(errmsg)
              end if
              xseg(1) = x(1)
              yseg(1) = y(1)
              xseg(2) = xeul
              yseg(2) = ycross
              jx_eul_tmp = jx_eul
              jy_eul_tmp = jy_eul
              !
              ! prepare for next iteration
              !
              x(0) = x(1)
              y(0) = y(1)
              x(1) = xeul
              y(1) = ycross
              jx_eul = jx_eul+xsgn2
            else
              !
              !*******************************************************************************
              !
              ! there are crossings with longitude(s) and latitude(s)
              !
              !*******************************************************************************
              !
              xsgn1 = (1+int(sign(1.0_r8,x(2)-x(1))))/2 !"1" if x(2)>x(1) else "0"
              xsgn2 = (int(sign(1.0_r8,x(2)-x(1)))) !"1" if x(2)>x(1) else "0"
              xeul   = xgno(jx_eul+xsgn1)
              ysgn1 = (1+int(sign(1.0_r8,y(2)-y(1))))/2 !"1" if y(2)>y(1) else "0"
              ysgn2 = int(sign(1.0_r8,y(2)-y(1)))       !"1" if y(2)>y(1) else "-1"
              yeul   = ygno(jy_eul+ysgn1)

              slope  = (y(2)-y(1))/(x(2)-x(1))
              if (abs(x(2)-x(1))<fuzzy_width) then
                ycross = 0.5_r8*(y(2)-y(1))
              else
                ycross = y_cross_eul_lon(x(1),y(1),xeul,slope)
              end if
              xcross = x_cross_eul_lat(x(1),y(1),yeul,slope)


              if ((xsgn2>0.and.xcross<=xeul).or.(xsgn2<0.and.xcross>=xeul)) then
                !
                ! cross latitude
                !
                xseg(1) = x(1)
                yseg(1) = y(1)
                xseg(2) = xcross
                yseg(2) = yeul
                jx_eul_tmp = jx_eul
                jy_eul_tmp = jy_eul
                !
                ! prepare for next iteration
                !
                x(0) = x(1)
                y(0) = y(1)
                x(1) = xcross
                y(1) = yeul
                jy_eul = jy_eul+ysgn2
                !
                ! register crossing with latitude
                !
                jcross_lat = jcross_lat+1
                cross_lat_eul_index(jcross_lat,1) = jx_eul
                if (ysgn2>0) then
                  cross_lat_eul_index(jcross_lat,2) = jy_eul
                else
                  cross_lat_eul_index(jcross_lat,2) = jy_eul+1
                end if
                r_cross_lat(jcross_lat,1) = xcross
                r_cross_lat(jcross_lat,2) = yeul
!              write(*,*) "D register crossing with latitude",xcross,yeul,jx_eul,cross_lat_eul_index(jcross_lat,2)
              else
                !
                ! cross longitude
                !
                xseg(1) = x(1)
                yseg(1) = y(1)
                xseg(2) = xeul
                yseg(2) = ycross
                jx_eul_tmp = jx_eul
                jy_eul_tmp = jy_eul
                !
                ! prepare for next iteration
                !
                x(0) = x(1)
                y(0) = y(1)
                x(1) = xeul
                y(1) = ycross
                jx_eul = jx_eul+xsgn2
              end if

            end if
          end if
          !
          ! register line-segment (don't register line-segment if outside of panel)
          !
          if (jx_eul_tmp>=jx_min.and.jy_eul_tmp>=jy_min.and.&
               jx_eul_tmp<=jx_max-1.and.jy_eul_tmp<=jy_max-1) then
            jsegment=jsegment+1
            weights_eul_index(jsegment,1) = jx_eul_tmp
            weights_eul_index(jsegment,2) = jy_eul_tmp

            call get_weights_exact(lexact_horizontal_line_integrals.and.abs(yseg(2)-yseg(1))<tiny,&
                 weights(jsegment,:),&
                 xseg,yseg,nreconstruction,ngauss,gauss_weights,abscissae)
!old            call get_weights_gauss(weights(jsegment,1:nreconstruction),&
!old                 xseg,yseg,nreconstruction,ngauss,gauss_weights,abscissae)
          else
            !
            ! segment outside of panel
            !
          end if

        end do
        side_count = side_count+1
      end do
    end if
  end subroutine side_integral


  real (kind=r8) function y_cross_eul_lon(x,y,xeul,slope) result(y_cross_eul)
    real (kind=r8), intent(in) :: x,y
    real (kind=r8), intent(in) :: xeul,slope
    !
    ! line: y=a*x+b
    !
    real (kind=r8) :: b

    b = y-slope*x
    y_cross_eul = slope*xeul+b
  end function y_cross_eul_lon

  real (kind=r8) function x_cross_eul_lat(x,y,yeul,slope) result(x_cross_eul)
    real (kind=r8), intent(in) :: x,y
    real (kind=r8), intent(in) :: yeul,slope

    if (fuzzy(abs(slope),fuzzy_width)>0) then
        x_cross_eul = x+(yeul-y)/slope
    else
      x_cross_eul = bignum
    end if
  end function x_cross_eul_lat

  subroutine get_weights_exact(lexact_horizontal_line_integrals,weights,xseg,yseg,nreconstruction,&
       ngauss,gauss_weights,abscissae)
    use fvm_analytic_mod, only: I_00, I_10, I_01, I_20, I_02, I_11
    use cam_abortutils, only: endrun
    logical, intent(in) :: lexact_horizontal_line_integrals
    integer , intent(in) :: nreconstruction, ngauss
    real (kind=r8), intent(out) :: weights(:)
    real (kind=r8),  intent(in) :: gauss_weights(:), abscissae(:) !dimension(ngauss)


    real (kind=r8),  intent(in) :: xseg(:),yseg(:) !dimension(2)
    character(len=256) :: errmsg
    !
    ! compute weights
    !
    if(lexact_horizontal_line_integrals) then
      weights(1) = ((I_00(xseg(2),yseg(2))-I_00(xseg(1),yseg(1))))
      if (abs(weights(1))>1.0_r8) then
        write(errmsg,*) '1 exact weights(jsegment)',weights(1),xseg,yseg
        call endrun(errmsg)
      end if
      if (nreconstruction>1) then
         weights(2) = ((I_10(xseg(2),yseg(2))-I_10(xseg(1),yseg(1))))
         weights(3) = ((I_01(xseg(2),yseg(2))-I_01(xseg(1),yseg(1))))
      end if
      if (nreconstruction>3) then
         weights(4) = ((I_20(xseg(2),yseg(2))-I_20(xseg(1),yseg(1))))
         weights(5) = ((I_02(xseg(2),yseg(2))-I_02(xseg(1),yseg(1))))
         weights(6) = ((I_11(xseg(2),yseg(2))-I_11(xseg(1),yseg(1))))
      end if
    else
      call get_weights_gauss(weights,xseg,yseg,nreconstruction,ngauss,gauss_weights,abscissae)
    end if
  end subroutine get_weights_exact



  subroutine get_weights_gauss(weights,xseg,yseg,nreconstruction,ngauss,gauss_weights,abscissae)
    use fvm_analytic_mod, only: F_00, F_10, F_01, F_20, F_02, F_11
    integer , intent(in) :: nreconstruction,ngauss
    real (kind=r8), intent(out) :: weights(:)
    real (kind=r8),  intent(in) :: xseg(2),yseg(2)
    real (kind=r8) :: slope
    !
    ! compute weights
    !
    !
    ! for Gaussian quadrature
    !
    real (kind=r8), intent(in) :: gauss_weights(ngauss), abscissae(ngauss)

    ! if line-segment parallel to x or y use exact formulaes else use qudrature
    !
    real (kind=r8) :: b,integral,dx2,xc,x,y
    integer :: i

!    if (fuzzy(abs(xseg(1) -xseg(2)),fuzzy_width)==0)then
    if (xseg(1)==xseg(2))then
      weights = 0.0_r8
    else
      slope    = (yseg(2)-yseg(1))/(xseg(2)-xseg(1))
      b        = yseg(1)-slope*xseg(1)
      dx2      = 0.5_r8*(xseg(2)-xseg(1))
      xc       = 0.5_r8*(xseg(1)+xseg(2))
      integral = 0.0_r8
      do i=1,ngauss
        x        = xc+abscissae(i)*dx2
        y        = slope*x+b
        integral = integral+gauss_weights(i)*F_00(x,y)
      end do
      weights(1) = integral*dx2
      if (nreconstruction>1) then
        integral = 0.0_r8
        do i=1,ngauss
          x        = xc+abscissae(i)*dx2
          y        = slope*x+b
          integral = integral+gauss_weights(i)*F_10(x,y)
        end do
        weights(2) = integral*dx2
        integral = 0.0_r8
        do i=1,ngauss
          x        = xc+abscissae(i)*dx2
          y        = slope*x+b
          integral = integral+gauss_weights(i)*F_01(x,y)
        end do
        weights(3) = integral*dx2
      end if
      if (nreconstruction>3) then
        integral = 0.0_r8
        do i=1,ngauss
          x        = xc+abscissae(i)*dx2
          y        = slope*x+b
          integral = integral+gauss_weights(i)*F_20(x,y)
        end do
        weights(4) = integral*dx2
        integral = 0.0_r8
        do i=1,ngauss
          x        = xc+abscissae(i)*dx2
          y        = slope*x+b
          integral = integral+gauss_weights(i)*F_02(x,y)
        end do
        weights(5) = integral*dx2
        integral = 0.0_r8
        do i=1,ngauss
          x        = xc+abscissae(i)*dx2
          y        = slope*x+b
          integral = integral+gauss_weights(i)*F_11(x,y)
        end do
        weights(6) = integral*dx2
      end if
    end if
  end subroutine get_weights_gauss

  subroutine truncate_vertex(x,j_eul,gno,igno_min,igno_max)
    use cam_abortutils, only: endrun
    integer , intent(inout) :: j_eul
    integer , intent(in)    :: igno_min,igno_max

    real (kind=r8), intent(inout) :: x
    real (kind=r8),    intent(in) :: gno(igno_min:igno_max) !dimension(-ihalo:nc+2+ihalo)
!    real (kind=r8), intent(in)    :: eps

    logical                 :: lcontinue
    integer :: iter, xsgn
    real (kind=r8) :: dist,dist_new,tmp
    character(len=256) :: errmsg

    lcontinue = .true.
    iter = 0
    dist = bignum

    xsgn     = int(sign(1.0_r8,x-gno(j_eul)))

    do while (lcontinue)
      if ((j_eul<igno_min) .or. (j_eul>igno_max)) then
        write(errmsg,*) 'something is wrong', j_eul,igno_min,igno_max, iter
        call endrun(errmsg)
      end if
      iter     = iter+1
      tmp      = x-gno(j_eul)
      dist_new = abs(tmp)
      if (dist_new>dist) then
        lcontinue = .false.
      else if (abs(tmp)<1.0E-9_r8) then
        x = gno(j_eul)
        lcontinue = .false.
      else
        j_eul = j_eul+xsgn
        dist = dist_new
      end if
      if (iter>100) then
        write(errmsg,*) 'truncate vertex not converging'
        call endrun(errmsg)
      end if
    end do
  end subroutine truncate_vertex

  subroutine which_eul_cell(x,j_eul,gno,igno_min,igno_max)
    use cam_abortutils, only: endrun
    integer , intent(inout) :: j_eul
    integer , intent(in)    :: igno_min,igno_max
    real (kind=r8), intent(in) :: x(:) !dimension(3)
    real (kind=r8), intent(in) :: gno(igno_min:igno_max) ! dimension(-ihalo:nc+2+ihalo)

    logical :: lcontinue
    integer :: iter
    character(len=256) :: errmsg

    lcontinue = .true.
    iter = 0

    do while (lcontinue)
      iter = iter+1
      if (x(1)>=gno(j_eul).and.x(1)<gno(j_eul+1)) then
        lcontinue = .false.
        !
        ! special case when x(1) is on top of grid line
        !
        if (x(1)==gno(j_eul)) then
          !
          ! x(1) is on top of gno(J_eul)
          !
          if (x(2)>gno(j_eul)) then
            j_eul = j_eul
          else if (x(2)<gno(j_eul)) then
            j_eul = j_eul-1
          else
            !
            ! x(2) is on gno(j_eul) grid line; need x(3) to determine Eulerian cell
            !
            if (x(3)>gno(j_eul)) then
              !
              ! x(3) to the right
              !
              j_eul = j_eul
            else if (x(3)<gno(j_eul)) then
              !
              ! x(3) to the left
              !
              j_eul = j_eul-1
            else
              write(errmsg,*) 'inconsistent cell: x(1)=x(2)=x(3)',x(1),x(2),x(3)
              call endrun(errmsg)
            end if
          end if
        end if
      else
        !
        ! searching - prepare for next iteration
        !
        if (x(1)>=gno(j_eul+1)) then
          j_eul = j_eul + 1
        else
          !
          ! x(1).LT.gno(j_eul)
          !
          j_eul = j_eul - 1
        end if
      end if
      if (iter>1000.or.j_eul<igno_min.or.j_eul>igno_max) then
        write(iulog,*) 'search is which_eul_cell not converging!', iter, j_eul,igno_min,igno_max
        write(iulog,*) 'gno', gno(igno_min), gno(igno_max)
        write(iulog,*) gno
        write(errmsg,*) 'search in which_eul_cell not converging!', iter, j_eul, igno_min, igno_max
        call endrun(errmsg)
      end if
    end do
  end subroutine which_eul_cell


  function fuzzy(x,epsilon) result(fuzzy_out)

    integer :: fuzzy_out
    real (kind=r8), intent(in) :: epsilon
    real (kind=r8), intent(in) :: x

    if (abs(x)<epsilon) then
      fuzzy_out = 0
    else if (x >epsilon) then
      fuzzy_out = 1
    else !if (x < fuzzy_width) then
      fuzzy_out = -1
    end if
  end function fuzzy

end module fvm_overlap_mod
