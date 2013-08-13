      subroutine interp
c*****this routine replaces the x-array by an array whose number
c     of points is a factor of (approximately) "ptmult" larger than 
c     the original number. The resulting spectrum is linear in the 
c     interpolated wavelength scale.

      include 'Chars.com'
      include 'Dataval.com'
      include 'Datachr.com'
      include 'Mathval.com'
      include 'Plotval.com'
      real*8 wavestart, ptmult, h, p, disp

c     ask for the multiplication factor for the number of points
      ptmult = 1.
2     write (message,1001)
1001  format ('GIVE THE DESIRED POINT MULTIPLICATION FACTOR [1.0]: ')
      nchars = 52
1     call getnum (nchars,ptmult)
      if (ptmult .ne. -9999.) then
         if (ptmult .le. 0.) then
            errmess = 'MULTIPLICATION FACTOR MUST BE >= 0! TRY AGAIN'
            nchars = 45
            go to 1 
         elseif (int(xnum*npx) .gt. 131072) then
            errmess = 'NEW ARRAY WOULD BE > 131072 POINTS; TRY AGAIN'
            nchars = 43
            call puterr (nchars)
            go to 1 
         else
            disp = (wlx(npx)-wlx(1))/(npx-1)/ptmult
            disp = int(1000.*disp)/1000.
         endif
      endif

c     interpolate the x-array
      ilow = 1
      ipt = 1
      wavestart = int(100.*(wlx(1)+0.0001))/100.
5     t(ipt) = wavestart + (ipt-1)*disp
10    if (t(ipt) .gt. wlx(npx)) then
         go to 30
      elseif (t(ipt) .gt. wlx(ilow+3)) then
         if (ilow+4 .eq. npx) then
            go to 15
         else
            ilow = ilow + 1
            go to 10
         endif
      endif
15    h = (wlx(ilow+4) - wlx(ilow))/4.
      p = (t(ipt) - wlx(ilow+2))/h
      ss(ipt) = x(ilow)*(p*p-1.)*p*(p-2.)/24. +
     .          x(ilow+1)*(p-1.)*(-p)*(p*p-4.)/6. +
     .          x(ilow+2)*(p*p-1.)*(p*p-4.)/4. +
     .          x(ilow+3)*(p+1.)*(-p)*(p*p-4.)/6. +
     .          x(ilow+4)*(p*p-1.)*p*(p+2.)/24.
      ipt = ipt + 1
      if (ipt .gt. 131072) go to 30
      go to 5

c     plot the result
30    ipt = ipt - 1
      ilow = max0(1,ipt/2-100)
      ihigh = min0(ipt,ilow+200)
      npt = ihigh - ilow + 1
      xleft = t(ilow)
      right = t(ihigh)
      call minimax (ss(ilow),xmin,xmax,npt)
      up = 1.05*xmax
      down= amax1(0.0,0.90*xmin)
      call plotxy (1,1,wlx,x,npx,1)
      call plotxy (1,-1,t(ilow),ss(ilow),npt,-40)

c     see if this is the desired result
510   message = 'HAPPY WITH THE INTERPOLATED SPECTRUM [y],n? '
      nchars = 44
      call getasci (nchars)
      if (array(1:1).eq.'y' .or. nchars.le.0) then 
         go to 600
      elseif (array(1:1) .eq. 'n') then
         go to 2
      else
         go to 510
      endif

c     replace the x-array with the interpolated spectrum, and plot it
600   do 610 i=1,ipt
         x(i) = ss(i)
610      wlx(i) = t(i)
      npx = ipt
      dispx(1) = t(1) - disp
      dispx(2) = disp
         do 625 i=3,9
625         dispx(i) = 0.
      call minimax (x,xmin,xmax,npx)
      xleft = wlx(1)
      right = wlx(npx)
      up = 1.12*xmax
      down = 0.
      call plotxy (1,1,wlx,x,npx,1)

      return
      end

      






