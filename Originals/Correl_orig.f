      subroutine correl
c*****this routine cross-correlates arrays x and y using any part of the
c     arrays, as long as there are enough data points in each array. 
c     The result is printed out in terms of the shift in the abscissa
c     of the y array needed to algin it with the x array.
 
      include 'Dataval.com'
      include 'Plotval.com'
      include 'Mathval.com'
      include 'Chars.com'
      real*8 xnum
      integer first
      data first/0/
 

c     this section is for user commands
      if (first .eq. 0) then
         mpt = npx
         npt = npx - 100
         mxshft = 10
         first = 1
      endif
1     prompt = 'CORREL:   '
      call getcom 
      if (command .eq. 'ab' .or. command(1:1) .eq. 'a') return
      if (command .eq. 'go') go to 5
      if (command .eq. 'he') go to 120
      if (command .eq. 'qu' .or. command(1:1) .eq. 'q') return
      if (command .eq. 'ra') go to 25
      write (errmess,1020) command
1020  format (a2,' IS AN UNKNOWN CORRELATE COMMAND; TRY AGAIN! ')
      nchars = 47
      call puterr (nchars)
      go to 1

c     here a complete cross-correlation is performed
c     first, plot the arrays to be correlated:
5     message = 'GIVE THE CENTRAL DATA POINT: '
      nchars = 29
      call getnum (nchars,xnum)
      xusr = sngl(xnum)
      if (xusr.lt.wlx(1) .or. xusr.gt.wlx(npx)) then
         errmess = 'THIS VALUE IS OUT OF RANGE! '
         nchars = 28
         call puterr (nchars)
         go to 5
      endif
      call estim (xusr,xusr,dispx,wlx,npx,ipt,ipt)
      ileft = max0(ipt-npt/2,1)
      iright = min0(ileft+npt-1,npx)
      npt = iright - ileft + 1
      call minimax(x(ileft),xxmin,xxmax,npt)
      up = 1.2*xxmax
      down = 0.5*xxmin
      xleft = wlx(ileft)
      right = wlx(iright)
      call plotxy (1,1,wlx(ileft),x(ileft),npt,1)
      call plotxy (1,-1,wly(ileft),y(ileft),npt,-2)
      imax = 2*mxshft + 1

c     next, do the cross-correlation:
      do i=1,imax
         ishift = i - 1 - mxshft 
         spx(i) = real(ishift)
         call cross (ishift,spy(i),x(ileft),y(ileft),mpt,mpt)
      enddo
      do i=1,imax
         spy(i) = 100.*spy(i)
      enddo

c     next, compute a spline fit and find the maximum of spline:
      call splnc (imax,0.0001)
      jmax = 10*(imax - 1) + 1
      do i=1,jmax
         t(i) = -mxshft + real(i-1)/10.
      enddo
      call spln (imax,jmax)
      ymaxi = ss(1)
      do i=1,jmax
         if (ss(i) .gt. ymaxi) then
            ymaxi = ss(i)
            xmaxi = t(i)
         endif
      enddo       
      call sm_ctype (colors(2))
      call sm_expand (0.8)
      write (errmess,1002) ymaxi
1002  format ('MAX(R) = ',f6.1)
      call sm_relocate (xleft+0.05*(right-xleft),down+0.25*(up-down))
      call sm_label (errmess)
      write (errmess,1003) xmaxi
1003  format ('OFFSET = ',f6.2)
      call sm_relocate (xleft+0.05*(right-xleft),down+0.18*(up-down))
      call sm_label (errmess)
      call sm_gflush
      call sm_alpha

c     do a plot of the correlation, if desired
      message = 'DO YOU WANT A CORRELATION PLOT ([y]/n)? '
      nchars = 40
      call getasci (nchars)
      if (array(1:1).eq.'y' .or. nchars.le.0) then
         xleft = real(-(mxshft + 1))
         right = real(mxshft + 1)
         call minimax (spy,pmin,pmax,imax)
         up = real(int(pmax + 5.))
         down = real(int(pmin - 5.))
         call plotxy (1,1,spx,spy,imax,30)
         call plotxy (1,-1,t,ss,jmax,-1)
         call sm_ctype (colors(2))
         call sm_expand (0.8)
         write (errmess,1002) ymaxi
         call sm_relocate (xleft+0.50*(right-xleft),down+0.25*(up-down))
         call sm_putlabel (5,errmess)
         write (errmess,1003) xmaxi
         call sm_relocate (xleft+0.50*(right-xleft),down+0.18*(up-down))
         call sm_putlabel (5,errmess)
         call sm_gflush
         call sm_alpha
      endif

c     shift the y-spectrum by the indicated amount
      message = 'SHOULD THE Y-ARRAY BE SHIFTED ([y]/n)? '
      nchars = 39
      call getasci (nchars)
      if (array(1:1).eq.'y' .or. nchars.le.0) then
         call yshift (xmaxi)
      endif
      go to 1

c     here the correlation and window widths are set
25    message = 'NUMBER OF POINTS FOR THE CORRELATION = '
      nchars = 39
      call getnum (nchars,xnum)
      if (xnum .eq. -9999.) go to 25
      mpt = int(xnum)
      if (mpt .gt. npx) then
         errmess = 'THIS VALUE IS OUT OF RANGE! '
         nchars = 28
         call puterr (nchars)
         go to 25
      endif
30    message = 'NUMBER OF POINTS FOR THE DISPLAY = '
      nchars = 35
      call getnum (nchars,xnum)
      npt = int(xnum)
      if (npt .gt. npx) then
         errmess = 'THIS VALUE IS OUT OF RANGE! '
         nchars = 28
         call puterr (nchars)
         go to 30
      endif
35    message = 'MAXIMUM SHIFT FOR THE CORRELATION = '
      nchars = 36
      call getnum (nchars,xnum)
      mxshft = int(xnum)
      if (mxshft .gt. 25) then
         message = 'TOO LARGE! (MAXIMUM = 25) '
         nchars = 26
         call puterr (nchars)
         go to 35
      endif
      go to 1

c---Here help is sought
120   call printh ('corhelp')
      go to 1

      end






      


