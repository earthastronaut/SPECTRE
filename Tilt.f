
      subroutine tilt (pts,wl,npt,ltype,splitd)

c*****this routine takes the highest points from the first and last 15% of the
c     array and renormalizes the array fluxes to a linear fit between these
c     two levels

      include 'Chars.com'
      include 'Plotval.com'
      include 'Mathval.com'
      real*4 pts(131072), wl(131072)
      logical splitd

c     determine the number of points in the first and last 15% of the array
c     and figure out how many points to employ in the average (it will be 20%
c     of the 15% of the points)
      ipt = max0(int(0.15*npt),50)
      lpt = max0(int(0.20*ipt),5)

c     transfer the data to a scratch array; sort the beginning and ending
c     "ipt" chunks of data
      do i=1,npt
         t(i) = pts(i)
      enddo
      call sort (ipt,t)
      call sort (ipt,t(npt-ipt+1))

c     average the "lpt" number of values in the scratch array beginning/ending
c     chunks; arbitrarily assign point position to 7.5% of the way in from 
c     either end of the array.
      avg1 = 0.
      istart = ipt - lpt + 1
      iend = ipt
      do i=istart,iend
         avg1 = avg1 + t(i)
      enddo
      avg1 = avg1/lpt
      avg2 = 0.
      xpos1 = wl(ipt/2)
      istart = npt - lpt + 1
      iend = npt
      do i=istart,iend
         avg2 = avg2 + t(i)
      enddo
      avg2 = avg2/lpt
      xpos2 = wl(npt-ipt/2)
      
c     set up the plot
      if (xleft.ne.wl(1) .or. right.ne.wl(npt)
     .    .or. splitd) then
         xleft = wl(1)
         right = wl(npt)
      endif
      call minimax (pts,xmin,xmax,npt)
      up = 1.12*xmax
      down = 0.8*xmin
      call plotxy (1,1,wl,pts,npt,ltype)

c     now make an linear array with which to divide the original data
      slope = (avg2-avg1)/(xpos2-xpos1)
      xint = avg1 - slope*xpos1
      do i=1,npt
         t(i) = wl(i)
         ss(i) = xint + slope*wl(i)
      enddo
      call plotxy (1,-1,t,ss,npt,4)

c     If the continuum tilt is acceptable, divide and plot the quotient array
      message = 'IS THE CONTINUUM TILT OK ([y]/n)? '
      nchars = 34
      call getasci (nchars)
      if (array(1:1).eq.'y' .or. nchars.le.0) then
         do i=1,npt
            pts(i) = pts(i)/ss(i)
         enddo
         call minimax (pts,xmin,xmax,npt)
         up = 1.12*xmax
         down = 0.
         call plotxy (1,1,wl,pts,npt,ltype)
      endif

      return
      end






