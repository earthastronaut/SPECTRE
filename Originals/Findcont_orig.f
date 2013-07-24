      
      subroutine findcont (nogo,splitd)

c*****this routine flattens the continuum by seeking the highest points
c     in wavelength intervals whose widths are set by the user.

      include 'Dataval.com'
      include 'Plotval.com'
      include 'Mathval.com'
      include 'Chars.com'
      real*8 xnum, wend
      logical nogo,splitd

c     clear the arrays, set some parameters
      do i=1,25
         s2(i) = 0.
         spcoy(i) = 0.
         spx(i) = 0.
         spy(i) = 0.
      enddo
      do i=1,24
         do j=1,3
            spcoc(i,j) = 0.
         enddo
      enddo
      nknots = 0
      numit = npx
      ier = 0
      nogo = .false.

c     get the wavelength intervals to seek continua.
50    message = 'GIVE THE WAVELENGTH INTERVALS FOR CONTINUA: '
      nchars = 44
10    call getnum (nchars,xnum)
      if (xnum .ne. -9999.) go to 30
      go to 10
30    deltalam = sngl(xnum)
 
c     get the fraction of points in the intervals to use.
      message = 'GIVE THE FRACTION OF POINTS TO USE IN EACH INTERVAL: '
      nchars = 53
45    call getnum (nchars,xnum)
      if (xnum .ne. -9999.) go to 40
      go to 45
40    fraction = sngl(xnum)

c     ignore the masked-off ends of the array
      ilo = 2
15    if (x(ilo).gt.0.999*x(1) .and. x(ilo).lt.1.001*x(1)) then
         ilo = ilo + 1
         go to 15
      endif
      ihi = npx -1
20    if (x(ihi).gt.0.999*x(npx) .and. x(ihi).lt.1.001*x(npx)) then
         ihi = ihi - 1
         go to 20
      endif

c     step in wavelength, setting the next wavelength interval
      j = 1
      ipt = ilo
35    wend = wlx(ipt) + deltalam
      call estim (sngl(wend),sngl(wend),dispx,wlx,npx,jpt,jpt)
      if (ihi .gt. ihi) jpt = ihi
      npt = jpt - ipt + 1
      spx(j) = (wlx(jpt)+wlx(ipt))/2.

c     use highest points in the interval to set continuum level
      if (npt .eq. 1) then
         spy(j) = x(ipt)
      elseif (npt .eq. 2) then
         spy(j) = (x(ipt)+x(jpt))/2.
      else
         lpt = max0(int(fraction*npt),2)
         do i=ipt,jpt
            ss(i) = x(i)
         enddo
         call sort (npt,ss(ipt))
         spy(j) = 0.
         istart = jpt - lpt + 1
         iend = jpt
         do i=istart,iend
            spy(j) = spy(j) + ss(i)
         enddo
         spy(j) = spy(j)/lpt
      endif
      j = j + 1
      ipt = jpt + 1
      if (ipt .le. ihi) go to 35

c     set up the plot
320   if (xleft.ne.wlx(1) .or. right.ne.wlx(npx)
     .    .or. splitd) then
         xleft = wlx(1)
         right = wlx(npx)
      endif
      call minimax (x,xmin,xmax,npx)
      up = 1.12*xmax
      down = 0.8*xmin
      call plotxy (1,1,wlx,x,npx,1)

c     draw in the continuum points, and fit a smooth curve through them
      nknots = j - 1
      if (nknots .lt. 2) then
         errmess = 'AT LEAST 2 POINTS ARE NEEDED FOR CONTINUUM FIT!'
         nchars = 47
         call puterr (nchars)
         return
      else
         do j=1,nknots
            call plus (spx(j),spy(j))
         enddo
         call shuffle
         if (nknots .eq. 3) then
            call parab (ier)
            do i=1,npx
               t(i) = wlx(i)
               ss(i) = splint(t(i))
            enddo
         else
            call splnc (nknots,0.0001)
            do i=1,npx
               t(i) = wlx(i)
            enddo
            call spln (nknots,npx)
         endif
      endif

c     plot the proposed continuum
      call plotxy (1,-1,t,ss,npx,4)
      message = 'IS THE CONTINUUM OK ([y]/n/a)? '
      nchars = 29
      call getasci (nchars)
      if (array(1:1) .eq. 'a') return
      if (array(1:1) .eq. 'n') go to 50

c     if the continuum is ok, divide, replot, and exit
      do  i=1,npx
         x(i) = x(i)/ss(i)
      enddo
      call minimax (x,xmin,xmax,npx)
      up = 1.12*xmax
      down = 0.
      call plotxy (1,1,wlx,x,npx,1)
      return


      end


