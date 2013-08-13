      subroutine rebin (wl,pts,disp,npt,filetype) 
c*****this routine rebins the data into desired step sizes. The ends of 
c     the arrays will be lost in this procedure. The original data 
c     will be interpolated to 10 points/original step, and the closest
c     point to the desired step will be adopted for the rebinned spectrum

      include 'Chars.com'
      include 'Mathval.com'
      include 'Plotval.com'
      real*8 wave1,wave2,step1,wavenow,xnum
      real*4 wl(131072),pts(131072)
      real*8 disp(9)
      character*4 filetype


      if (filetype .eq. 'text') then
         write (6,*) "CAN'T REBIN ON TEXT FILE YET"
         return
      endif


c     first get the desired starting wavelength and step size of the  
c     interpolated spectrum
10    message = 'DESIRED STARTING WAVELENGTH = '
      nchars = 30
      call getnum (nchars,wave1)
      if (wave1 .eq. -9999.) go to 10
      if (wave1.lt.wl(1) .or. wave1.gt.wl(npt)) then
         errmess = 'STARTING WAVELENGTH OUT OF RANGE; TRY AGAIN!'
         nchars = 44
         call puterr (nchars)
         go to 10
      endif
12    message = 'DESIRED ENDING WAVELENGTH = '
      nchars = 28
      call getnum (nchars,wave2)
      if (wave2 .eq. -9999.) go to 10
      if (wave2.lt.wl(1) .or. wave2.gt.wl(npt)) then
         errmess = 'ENDING WAVELENGTH OUT OF RANGE; TRY AGAIN!'
         nchars = 42
         call puterr (nchars)
         go to 12
      endif
15    message = 'DESIRED WAVELENGTH STEP = '
      nchars = 26
      call getnum (nchars,step1)
      if (step1 .eq. -9999.) go to 15
      if (step1.lt.0.0 .or. step1.gt.(wl(npt)-wl(1))/50.) then
         errmess = 'WAVELENGTH STEP SIZE OUT OF RANGE; TRY AGAIN!'
         nchars = 45
         call puterr (nchars)
         go to 15
      endif

c     decide what type of interpolation function is needed
      message = 'INTERPOLATION FUNCTION ([s]/l)? '
      nchars = 32
      call getasci (nchars)
      if (array(1:1) .ne. 'l') go to 29

c     make a 4-point lagrangian interpolation
c     get the order of the function
c     find the place in the wavelength array to begin
      interp = 1
      wavenow = wave1
47    message = '# OF POINTS IN INTERPOLATION FORMULA (2,3,4,5,6)? '
      nchars = 50
      call getnum (nchars,xnum)
      norder = int(xnum+0.001)
      if (norder.lt.2 .or. norder.gt.6) go to 47
41    call estim (sngl(wavenow),sngl(wavenow),disp,wl,npt,ipt,ipt,
     .     filetype)

c     do the lagrangian interpolation
      go to (51,52,53,54,57),norder-1
51    ilow = min0(max0(1,ipt),npt-1)
      h = wl(ilow+1) - wl(ilow)
      p = (wavenow - wl(ilow))/h
      scratch(interp) = pts(ilow)*(1.-p) + pts(ilow+1)*p
      go to 56
52    ilow = min0(max0(1,ipt-1),npt-2)
      h = (wl(ilow+2) - wl(ilow))/2.
      p = (wavenow - wl(ilow+1))/h
      scratch(interp) = pts(ilow)*p*(p-1.)/2. + pts(ilow+1)*(1.-p*p) +
     .                  pts(ilow+2)*p*(p+1.)/2.
      go to 56
53    ilow = min0(max0(1,ipt-1),npt-3)
      h = (wl(ilow+3) - wl(ilow))/3.
      p = (wavenow - wl(ilow+1))/h
      scratch(interp) = pts(ilow)*(-p)*(p-1.)*(p-2.)/6. +
     .                  pts(ilow+1)*(p*p-1.)*(p-2.)/2. +
     .                  pts(ilow+2)*(-p)*(p+1.)*(p-2.)/2. +
     .                  pts(ilow+3)*p*(p*p-1.)/6.
      go to 56
54    ilow = min0(max0(1,ipt-2),npt-4)
      h = (wl(ilow+4) - wl(ilow))/4.
      p = (wavenow - wl(ilow+2))/h
      scratch(interp) = pts(ilow)*(p*p-1.)*p*(p-2.)/24. +
     .                  pts(ilow+1)*(p-1.)*(-p)*(p*p-4.)/6. +
     .                  pts(ilow+2)*(p*p-1.)*(p*p-4.)/4. +
     .                  pts(ilow+3)*(p+1.)*(-p)*(p*p-4.)/6. +
     .                  pts(ilow+4)*(p*p-1.)*p*(p+2.)/24.
      go to 56
57    ilow = min0(max0(1,ipt-2),npt-5)
      h = (wl(ilow+5) - wl(ilow))/5.
      p = (wavenow - wl(ilow+2))/h
      scratch(interp) = pts(ilow)*(-p)*(p*p-1.)*(p-2.)*(p-3.)/120. +
     .                  pts(ilow+1)*p*(p-1.)*(p*p-4.)*(p-3.)/24. +
     .                  pts(ilow+2)*(-1.)*(p*p-1.)*(p*p-4.)*(p-3.)/12. +
     .                  pts(ilow+3)*p*(p+1.)*(p*p-4.)*(p-3.)/12. +
     .                  pts(ilow+4)*(-p)*(p*p-1)*(p+2)*(p-3.)/24. +
     .                  pts(ilow+5)*p*(p*p-1.)*(p*p-4.)/120.
  
c     increment the desired wavelength
56    wavenow = wavenow + step1
      interp = interp + 1
      if (wavenow .le. wave2) go to 41
      interp = interp - 1
      go to 100


c     make a spline interpolation around that point
c     find the place in the wavelength array to begin
29    interp = 1
      ilow = 1
      imax = 15
      jmax = 20*(imax-1) + 1
      wavenow = wave1
      do 20 i=ilow,npt
         if (wavenow .lt. wl(i)) go to 25
20       continue

25    ilow = max0(1,i-3)
26    ihigh = min0(ilow + 15,npt)
      do 30 j=1,15
         spx(j) = wl(ilow+j-1)
30       spy(j) = pts(ilow+j-1)
      call splnc (imax,0.001)
      do 35 k=2,imax
         delta = (wl(k) - wl(k-1))/20.
         kount = 20*(k-2)
         do 35 kk=1,20
35          t(kk+kount) = spx(k-1) + delta*(kk-1)
      t(jmax) = spx(imax)
      call spln (imax,jmax)

c     find the desired interploated point
      jlow = 1
38    do 40 j=jlow,jmax-10
         if (wavenow .lt. t(j)) go to 45
40       continue
      ilow = ihigh - 3
      go to 26
45    scratch(interp)= ss(j)
      jlow = j

c     increment the desired wavelength
      wavenow = wavenow + step1
      interp = interp + 1
      if (wavenow .le. wave2) go to 38
      interp = interp - 1

c     plot the results
100   do 50 i=1,interp
50       t(i) = wave1 + step1*(i-1)
      mpt = min0(300,interp)
      istart = interp/2 - mpt/2
      call minimax (scratch(istart),xmin,xmax,mpt)
      xleft = t(istart)
      right = t(istart+mpt-1)
      up = xmax*1.10
      down = 0.9*xmin
      call plotxy (1,1,t(istart),scratch(istart),mpt,3)
      call plotxy (1,-1,wl,pts,npt,-10)

c     find whether the interpolation is OK
55    message = 'SATISFIED WITH THE INTERPOLATION ([y]/n)?'
      nchars = 41
      call getasci (nchars)
      
      if (array(1:1).eq.'y' .or. nchars.le.0) then
         do 60 i=1,interp
            wl(i) = t(i)
60          pts(i) = scratch(i)
         npt = interp
         disp(1) = wave1
         disp(2) = step1
         do 65 i=3,9
65          disp(i) = 0.
         return
      elseif (array(1:1) .eq. 'n') then
         return 
      else
         go to 55
      endif

      
      end








 
