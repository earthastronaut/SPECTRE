      subroutine fts (no,mode)
c*****fourier transform smoothing
c              (datar is the real part, datai is the imaginary part)
c              (npx must be greater than 16)
c              (jpts is the number of transform points: x[j1..j2])
c              (iremain is the space on each end of the array
c                              into which reflected data will be inserted)
 
      include 'Dataval.com'
      include 'Datachr.com'

      include 'Plotval.com'
      include 'Chars.com'
      include 'Mathval.com'
      include 'Plotchr.com'
      real*4 datar(131072), datai(131072), freq(2048), filt(7),
     .       filter(2048),power(2048)
      equivalence (datar(1),wly(1)),(filter(1),x(2049)),
     .            (power(1),wlz(1)),(datai(1),wlx(1)), 
     .            (freq(1),wlz(2049))
      logical no,happy

c*****copy x-array to scratch array for later use
      npscrx = npx
      npscry = npy
      npscrz = npz
      do 12 i = 1,npx
12       scratch(i) = x(i)

c*****initialize the variables
      no = .false.
      ift = .true.
      happy = .true.
13    jpts = npx
      j1 = 1
      j2 = npx
      if (jpts .lt. 16) go to 260
      ixh = jpts
      do i=1,131072
         datar(i) = 0.0
         datai(i) = 0.0
      enddo
      do i=1,2048
         power(i) = 0.0
         freq(i) = 0.0
      enddo

c*****find the power of two "i2pt" such that i2pt >= jpts
      do i=4,12
         i2pt=2**i
         if (i2pt .ge. jpts) go to 80
      enddo    

c*****center the data in the transform arrays
80    iremain = (i2pt-jpts)/2
      do i=j1,j2
         datar(iremain+i+1-j1) = x(i)
      enddo
      if (iremain .eq. 0) go to 120
 
c*****reflect the ends of the array
      j2d = iremain + jpts
      do i=1,iremain
         datar(i) = x(j1+iremain-i)
         datar(j2d+i) = x(j2+1-i)
      enddo
 
c*****compute the dc level
120   dc = 0.0
      do i=1,i2pt
         dc = dc + datar(i)
      enddo
      fi2pt = float(i2pt)
      dc = dc/fi2pt
 
c*****apply the dc bias
      do i=1,i2pt
         datar(i) = datar(i) - dc
      enddo

c*****apply the cosine bell data window
      call cosbell (datar,i2pt,-1)

c*****do the fourier transform
      j2d = int(alog(fi2pt)/alog(2.0))
      message = 'COMPUTING FFT POWER SPECTRUM...'
      nchars =  31
      call putasci (nchars)
      call fft (datar,datai,j2d)

c*****compute the power spectrum and find the maximum power point 
      pmax = alog10(datar(1)*datar(1) + datai(1)*datai(1))
      pmin = alog10(datar(1)*datar(1) + datai(1)*datai(1))
      i2ptd2 = i2pt/2
      do i=1,i2ptd2
         power(i) = alog10(datar(i)*datar(i) + datai(i)*datai(i))
         if (power(i) .gt. pmax) then
            pmax = power(i)
            ipmax = i
         elseif (power(i) .lt. pmin) then
            pmin = power(i)
            ipmin = i
         endif
      enddo    
      
c*****normalize the power
      do i=1,i2ptd2
         power(i) = power(i) - pmax
      enddo

c*****initialize the frequency array
      do i=1,i2ptd2
         freq(i) = float(i)/fi2pt
      enddo

c*****plot the power spectrum
      ift = .true.
      oldlft = xleft
      oldrgt = right
      olddown = down
      oldup = up
      up = 0.5
      call minimax (power,ylo,yhi,i2ptd2)
      range = yhi - ylo
      if (range .lt. 4.5) then
         down = -5.0
      elseif (range.ge.4.5 .and. range.lt.7.5) then
         down = -8.0
      elseif (range.ge.7.5 .and. range.lt.10.5) then
         down = -8.0
      elseif (range.ge.10.5) then
         down = -12.0
      endif
      xleft = 0.
      right = 0.5
      call labset (3)
      call plotxy (1,1,freq,power,i2ptd2,1)
      call labset (1)

c*****if mode = 2, then the user simply wants to put the power spectrum in
c     the z-array and exit
      if (mode .eq. 2) then
         message = 'REPLACE Z-ARRAY WITH THE POWER SPECTRUM ([y]/n)? '
         nchars = 49
         call getasci (nchars)
         if (array(1:1) .eq. 'y' .or. nchars .le. 0) then
            do i=1,i2ptd2
               z(i) = power(i)
            enddo
            do i=1,i2ptd2
               wlz(i) = freq(i)
            enddo
            npz = i2ptd2
            npx = npscrx
            npy = npscry
            do i=1,npx
               wlx(i) = wave(real(i),npx,dispx)
            enddo
            do i=1,npy
               wly(i) = wave(real(i),npy,dispy)
            enddo
            xleft = oldlft
            right = oldrgt
            down = olddown
            up = oldup
            return
         else
            go to 265
         endif
      endif

c*****make a first guess at the filter and noise levels
      call pfilt (freq,power,i2ptd2,filt,happy)
      if (happy) then
         ayval = filt(7)
         anoise = dex(ayval)
         go to 1900
      endif
      if (array(1:1) .eq. 'a') then
         go to 265
      elseif (array(1:1) .eq. 'n') then
         go to 986
      endif

c*****fit the power spectrum with a user-defined spline or parabola
c                    if not happy with the guess
986   message = 'FIT POWER SPECTRUM WITH PARABOLA (3 MARKED POINTS)'
      nchars = 50
      call putasci (nchars)
      ier = 3
989   call cont (ier)
      if (ier .ne. 0) then
         errmess = '3 MARKED POINTS ARE NEEDED.  TRY AGAIN! '
         nchars = 40
         call puterr (nchars)
         go to 989
      endif
      call parab (ier)
      if (ier .ne. 0) then
         errmess = 'PARABOLA FITTING ERROR.  DO THE POINTS AGAIN! '
         nchars = 46
         call puterr (nchars)
         go to 989
      endif
      do i=1,i2ptd2
         ss(i) = splint(freq(i))
      enddo
      call plotxy (1,-1,freq,ss,i2ptd2,4)
988   message = 'IS THIS CURVE OK ([y]/n/a)? '
      nchars = 28
      call getasci (nchars)
      if (array(1:1) .eq. 'a') then
         go to 265
      elseif (array(1:1) .eq. 'n') then
         call labset(3)
         call plotxy (1,1,freq,power,i2ptd2,1)
         call labset(1)
         go to 986
      elseif (array(1:1).eq.'y' .or. nchars.le.0) then
         go to 187
      else
         go to 988
      endif
 
c*****set and plot the noise level
187   message = 'SET THE NOISE LEVEL '
      nchars = 20
      call putasci (nchars)
      call sm_curs (xxx,yyy,ichr)
      call sm_gflush
      call sm_alpha
      call plus (xxx,yyy)
      call sm_ctype (colors(2))
      call sm_relocate (xleft,yyy)
      call sm_draw (right,yyy)
      call sm_ctype (colors(1))
      anoise = dex(yyy)
     
c*****compute and plot the filter (picks up here after automated guess)   
1900  message = 'COMPUTING FILTER... '
      nchars = 20
      call putasci (nchars)
      do i=1,i2ptd2
         if (ss(i) .lt. -30.) ss(i) = -30.
         psmodel = dex(ss(i))
         filter(i) = psmodel/(psmodel+anoise)
         ss(i) = alog10(filter(i))
      enddo
      call plotxy (1,-1,t,ss,i2ptd2,-2)
 
c*****compute the signal to noise
      s2n = 0.0
      do i=1,i2ptd2
         s2n = s2n + dex(power(i))
      enddo
      tnoise = anoise*float(i2ptd2)
      s2n = sqrt((s2n-tnoise)/tnoise)
      write (errmess,1001) s2n
1001  format('SIGNAL/NOISE = ',f10.3)
      call sm_ctype (colors(2))
      call sm_expand (0.8)
      call sm_relocate (0.05,down+0.10*(up-down))
      call sm_label (errmess)
      call sm_gflush
      call sm_alpha
 
c*****verify the filter (is it ok?)
      message = 'IS THE FILTER OK ([y]/n/a)? '
      nchars = 28
      call getasci (nchars)
      if (array(1:1).eq.'y' .or. nchars.le.0) then
         call sm_ctype (colors(1))
      elseif (array(1:1) .eq. 'a') then
         go to 265
      elseif (array(1:1) .eq. 'n') then
        call labset(3)
        call plotxy (1,1,freq,power,i2ptd2,1)
        call labset(1)
        goto 986
      endif
 
c*****apply the filter to the data
      do i=1,i2ptd2
         j1 = i + i2ptd2
         j2 = i2ptd2 + 1 - i
         datar(i) = datar(i)*filter(i)/fi2pt
         datai(i) = datai(i)*filter(i)/fi2pt
         datar(j1) = datar(j1)*filter(j2)/fi2pt
         datai(j1) = datai(j1)*filter(j2)/fi2pt
      enddo
 
c*****transform back to wavelengths
      message = 'REVERSE TRANSFORMING ARRAY...'
      nchars = 29
      call putasci (nchars)
      call fft (datar,datai,j2d)
      call cosbell (datar,i2pt,1)
 
c*****transfer the data back to the display area and rebias the data 
      do i=1,jpts
         x(jpts+1-i) = datar(i+iremain+1) + dc
      enddo
      message = 'RETRIEVING DATA...'
      nchars = 18
      call putasci (nchars)
      npx = npscrx
      npy = npscry
      npz = npscrz
      do i=1,npx
         wlx(i) = wave(real(i),npx,dispx)
      enddo
      do i=1,npy
         wly(i) = wave(real(i),npy,dispy)
      enddo
      do i=1,npz
         wlz(i) = wave(real(i),npz,dispz)
      enddo

c*****Check to see if we're okay by plotting a section
      mid = npx/2
      xleft = wlx(mid-75)
      right = wlx(mid+75)
      call estim (xleft,right,dispx,wlx,npx,ileft,iright,xfile)
      npt = iright - ileft + 1
      call minimax (x(ileft),xmin,xmax,npt)
      up = 1.05*xmax
      down = 0.85*xmin
      call labset(1)
      call plotxy (1,1,wlx,x,npx,1)
      idum = -30
      call plotxy (1,-1,wlx,scratch,npx,-30)
      errmess = 'LINE => SMOOTHED DATA IN X-ARRAY'
      call sm_ctype (colors(2)) 
      call sm_expand (0.8)
      call sm_relocate (xleft+0.10*(right-xleft),down+0.25*(up-down))
      call sm_label (errmess)
      errmess = 'MARKERS => ORIGINAL DATA POINTS'
      call sm_relocate (xleft+0.10*(right-xleft),down+0.18*(up-down))
      call sm_label (errmess)
      call sm_gflush
      call sm_alpha
      message = 'SATISFIED WITH THE SMOOTHING ([y]/n/a)?'
      nchars = 39
      call getasci (nchars)
      call minimax (x,xmin,xmax,npx)
      xleft = wlx(1)
      right = wlx(npx)
      down = 0.0
      up = 1.12*xmax
      if (array(1:1).eq.'y' .or. nchars.le.0) then
         go to 299
      elseif (array(1:1) .eq. 'a') then
         go to 265
      elseif (array(1:1) .eq. 'n') then
         do i=1,npx
            x(i) = scratch(i)
         enddo
         call plotxy(1,1,wlx,x,npx,1)
         go to 13
      endif

c*****cleanup after a normal termination
299   ift=.false.
      no=.false.
      return

c*****error message - not enough points
260   errmess = 'CANT DO IT: NPX MUST BE > 16'
      nchars = 28
      call puterr (nchars)
      no=.true.
      return

c*****a total abort
265   npx = npscrx
      npy = npscry
      npz = npscrz
      do i=1,npx
         wlx(i) = wave(real(i),npx,dispx)
      enddo
      do i=1,npy
         wly(i) = wave(real(i),npy,dispy)
      enddo
      do i=1,npz
         wlz(i) = wave(real(i),npz,dispz)
      enddo
      xleft = oldlft
      right = oldrgt
      down = olddown
      up = oldup
      return

      end





