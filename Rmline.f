      subroutine Rmline
c*****Almost the same as Mkline.f
c*****this routine manually creates a Gaussian line profile in array 'y'
      include 'Dataval.com'
      include 'Plotval.com'
      include 'Datachr.com'
      include 'Widpar.com'
      include 'Chars.com'
      real*8 wavout, xnum, wave
 
      message = 'WAVELENGTH = '
      nchars = 16
      call getnum (nchars,wavout)
      idelta = 6
      ndelt = 50
      wave = waveout
      call estim (sngl(wavout),sngl(wavout),dispx,wlx,npx,ipt,ipt,xfile)
c*****changes to manual keeps the input line in 'wave'
c25    call manual (1,ipt,wavout,depth,halfl,halfr,eqwdth)
25    call manual (1,ipt,wave,wavout,depth,halfl,halfr,eqwdth)
      ileft = ipt - ndelt
      npt =  2*ndelt + 1
      call plotxy (1,-1,wly(ileft),y(ileft),npt,-2)
 
20    message = 'IS THE PROFILE OK ([y],n,v,d,a,#)? '
      nchars = 33
      call getasci (nchars)
      if (array(1:1).eq.'y' .or. nchars.le.0) then
         message = 'DIVIDE THE X-ARRAY BY THE Y-ARRAY (y,[n])? '
         nchars = 43
         call getasci (nchars)
         if (array(1:1) .eq. 'y') then
            go to 50
         elseif (array(1:1).eq.'n' .or. nchars.le.0) then
            return
         endif
      elseif (array(1:1) .eq. 'n') then
         go to 25
      elseif (array(1:1) .eq. 'v') then
         call voigtfit (1,wavout,ipt,depth,halfl,halfr,eqwdth)
         go to 20
      elseif (array(1:1) .eq. 'd') then
         go to 50
      elseif (array(1:1) .eq. '#') then
35       message = 'DESIRED NUMBER OF POINTS TO BE PLOTTED = '
         nchars = 41
         call getnum (nchars,xnum)
         if (xnum .eq. -9999.) go to 35
         ndelt = int(sngl(xnum))
         go to 25
      else
         return
      endif
 
50    call move (x,z,wlx,wlz,dispx,dispz,ixh,izh,iaxisx,
     .           iaxisz,npx,npz,xobj,zobj,xkfnam,zkfnam,
     .           xfname,zfname,xary,zary)
      call screenstat (1)
      do 55 i=1,npx
55       x(i) = x(i)/y(i)
      call plotxy (1,1,wlz(ileft),z(ileft),npt,3)
      call plotxy (1,-1,wly(ileft),y(ileft),npt,-2)
      call plotxy (1,-1,wlx(ileft),x(ileft),npt,-1)
 
      return
      end
     




