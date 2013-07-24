      subroutine merge
c*****this routine merges the x- and y-array.  It first rebins the
c     spectra to standard wavelength steps (linear dispersion). The
c     interpolated and merged spectrum will replace the x-array

      include 'Chars.com'
      include 'Dataval.com'
      include 'Datachr.com'
      include 'Mathval.com'
      include 'Plotval.com'

c     make sure the y-array is the long wavelength one
      if (wly(npy) .lt. wlx(npx)) then
         call flip (x,y,wlx,wly,dispx,dispy,npx,npy,xobj,yobj, 
     .              xkfnam,ykfnam,xfname,yfname,xary,yary,
     .              xfile,yfile) 
         call labset (1)
         call screenstat (1)
      endif
      call minimax (x,xmin,xmax,npx)
      up = 1.12*xmax
      down = 0.0
      xleft = wlx(1)
      right = wly(npy)
      
c     get the desired wavelength step size in the interpolated spectrum
      disp = 0.5*((wlx(npx)-wlx(1))/(npx-1) + (wly(npy)-wly(1))/(npy-1))
      disp = int(1000.*disp)/1000.

c     interpolate the x-array
      ilow = 1
      ipt = 1
      wavestart = int(100.*(wlx(1)+0.01))/100.
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
      go to 5
30    ipt = ipt - 1
      call plotxy (1,1,wlx,x,npx,1)
      do 40 i=1,ipt
         wlx(i) = t(i)
40       x(i) = ss(i)
      npx = ipt
      dispx(1) = t(1) - disp
      dispx(2) = disp
         do 42 i=3,9
42          dispx(i) = 0.
      call plotxy (1,1,wlx,x,npx,1)

c     interpolate the y-array
      do 45 i=1,npx
         if (wlx(i) .ge. wly(1)) go to 50
45    continue
      errmess = 'X- AND Y-ARRAYS DO NOT OVERLAP; NO MERGER!'
      nchars = 42
      call puterr (nchars)
      return
50    ilow = 1
      ipt = 1
      wavestart = wlx(i)
      mergept = i
55    t(ipt) = wavestart + (ipt-1)*disp
60    if (t(ipt) .gt. wly(npy)) then
         go to 70
      elseif (t(ipt) .gt. wly(ilow+3)) then
         if (ilow+4 .eq. npy) then
            go to 65
         else
            ilow = ilow + 1
            go to 60
         endif
      endif
65    h = (wly(ilow+4) - wly(ilow))/4.
      p = (t(ipt) - wly(ilow+2))/h
      ss(ipt) = y(ilow)*(p*p-1.)*p*(p-2.)/24. +
     .          y(ilow+1)*(p-1.)*(-p)*(p*p-4.)/6. +
     .          y(ilow+2)*(p*p-1.)*(p*p-4.)/4. +
     .          y(ilow+3)*(p+1.)*(-p)*(p*p-4.)/6. +
     .          y(ilow+4)*(p*p-1.)*p*(p+2.)/24.
      ipt = ipt + 1
      go to 55
70    ipt = ipt - 1
      do 80 i=1,ipt
         wly(i) = t(i)
80       y(i) = ss(i)
      npy = ipt
      dispy(1) = t(1)
      dispy(2) = disp
         do 85 i=3,9
85          dispy(i) = 0.
      call plotxy (1,-1,wly,y,npy,-2)

c     decide what kind of merge weights to use
90    message = 'DESIRED MERGE TYPE [m],l,q,a? '
      nchars = 30
      call getasci (nchars)
      if (array(1:1).eq.'m' .or. nchars.le.0) then 
         go to 100
      elseif (array(1:1) .eq. 'l') then
         go to 200
      elseif (array(1:1) .eq. 'q') then
         go to 300
      elseif (array(1:1) .eq. 'a') then
         return
      else
         go to 90
      endif

c     do a straight-mean interpolation
100   jpt = 0
      ipt = mergept - 1
      do 105 i=1,ipt
         ss(i) = x(i)
105      t(i) = wlx(i)
110   ipt = ipt + 1
      jpt = jpt + 1
      if (ipt .gt. npx) go to 115
      ss(ipt) = 0.5*(x(ipt) + y(jpt))
      t(ipt) = wlx(ipt)
      go to 110
115   ipt = ipt - 1
      do 120 i=jpt,npy
         ipt = ipt + 1
         ss(ipt) = y(i)
120      t(ipt) = wly(i)
      go to 500

c     do a weight linearly increasing from the edge of the array
200   jpt = 0
      ipt = mergept - 1
      do 205 i=1,ipt
         ss(i) = x(i)
205      t(i) = wlx(i)
      factor = npx - mergept + 1
210   ipt = ipt + 1
      jpt = jpt + 1
      if (ipt .gt. npx) go to 215
      ss(ipt) = x(ipt)*(npx-ipt)/factor + y(jpt)*jpt/factor
      t(ipt) = wlx(ipt)
      go to 210
215   ipt = ipt - 1
      do 220 i=jpt,npy
         ipt = ipt + 1
         ss(ipt) = y(i)
220      t(ipt) = wly(i)
      go to 500

c     do a weight quadratically increasing from the edge of the array
300   jpt = 0
      ipt = mergept - 1
      boost = (y(1)+x(ipt+1))/x(ipt)
      do i=1,npy
         y(i) = boost*y(i)
      enddo
      do i=1,ipt
         ss(i) = x(i)
         t(i) = wlx(i)
      enddo
310   ipt = ipt + 1
      jpt = jpt + 1
      if (ipt .le. npx) then
         ss(ipt) = (x(ipt)*(npx-ipt)**2 + y(jpt)*jpt**2)/
     .             ((npx-ipt)**2 + jpt**2)
         t(ipt) = wlx(ipt)
         go to 310
      endif
      ipt = ipt - 1
      do i=jpt,npy
         ipt = ipt + 1
         ss(ipt) = y(i)
         t(ipt) = wly(i)
      enddo
 
c     plot the result
500   ileft = max0(1,mergept-50)
      iright = min0(ipt,npx+50)
      npts = iright - ileft + 1
      xleft = t(ileft)
      right = t(iright)
      call minimax (ss(ileft),xmin,xmax,npts)
      up = 1.12*xmax
      down = 0.8*xmin
      call plotxy (1,1,wlx(ileft),x(ileft),npx-ileft+1,1)
      call plotxy (1,-1,wly(1),y(1),npts,-2)
      call plotxy (1,-1,t(ileft),ss(ileft),npts,-4)
510   message = 'HAPPY WITH THE MERGED SPECTRUM [y],n? '
      nchars = 38
      call getasci (nchars)
      if (array(1:1).eq.'y' .or. nchars.le.0) then 
         go to 600
      elseif (array(1:1) .eq. 'n') then
         go to 90
      else
         go to 510
      endif

c     replace the x-array with the merged spectrum, and plot it
600   do 610 i=1,ipt
         x(i) = ss(i)
610      wlx(i) = t(i)
      npx = ipt
      dispx(1) = t(1)
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

      














