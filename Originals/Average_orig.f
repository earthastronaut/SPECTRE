      subroutine average
c*****this routine does simple n-point smoothing of the data points
 
      include 'Dataval.com'
      include 'Chars.com'
      include 'Mathval.com'
      double precision xnum
      character*1 choice
 
c*****copy the x-array temporarily to the z-array (save the z-array)
      if (npz .ne. 0) then
         do i=1,npz
            scratch(i) = z(i)
      enddo
      endif
      nphold = npz
      do i=1,npx
         wlz(i) = wlx(i)
         z(i) = x(i)
      enddo
      npz = npx
 
c*****find the smoothing method
2     write (message,1001)
1001  format ('SIMPLE MEAN, TRIANGULAR MEAN, OR GAUSSIAN MEAN ',
     .        ' ([m]/t/g)? ')
      nchars = 57
      call getasci (nchars)
      if (nchars .le. 0) then
         choice = 'm'
      else
         choice = array(1:1)
      endif
 
c*****find the smoothing width
      if (choice.eq.'m' .or. choice.eq.'t') then
1        message = 'NUMBER OF PIXELS TO AVERAGE = '
         nchars = 30
4        call getnum (nchars,xnum)
         if (xnum .eq. -9999.) go to 1
         inum = ifix(sngl(xnum+0.0001))
         if (mod(inum,2) .eq. 0) then
            message = 'ODD NUMBERS ONLY! TRY AGAIN: '
            nchars = 29
            go to 4
         endif
      elseif (choice .eq. 'g') then
3        message = 'GAUSSIAN FWHM (IN PIXELS) = '
         nchars = 28
         call getnum (nchars,xnum)
         if (xnum .eq. -9999.) go to 3
         sigma = xnum
      else
         go to 2
      endif
      
c*****fill in the smoothing function array
      if (choice .eq. 'm') then
         jdel = inum - (inum/2) - 1
         if (jdel .gt. 30) go to 2000
         power = 1.
         do i=1,jdel
            ss(i) = 1.
            power = power + 2.*ss(i)
         enddo
      elseif (choice .eq. 't') then
         jdel = inum - (inum/2) - 1
         if (jdel .gt. 30) go to 2000
         power = 1.
         do i=1,jdel
            ss(i) = real(jdel+1-i)/real(jdel+1)
            power = power + 2.*ss(i)
         enddo
      elseif (choice .eq. 'g') then
         sigma = sigma/2
         aa = 0.6932/sigma**2
         power = 1.0
         do i=1,30
            ss(i) = exp(-aa*real(i)**2 )
            power = power + 2*ss(i)
            if (ss(i) .lt. 0.05) go to 160
         enddo
         go to 2000
160      jdel = i
      else
         return
      endif
 
c*****smooth the spectrum in the x-array
      min = jdel + 1
      max = npx - jdel
      do i=min,max
         x(i) = z(i)
         do j=1,jdel
            x(i) = x(i) + ss(j)*(z(i-j) + z(i+j))
         enddo
            x(i) = x(i)/power
      enddo
      do i=1,jdel
         x(i) = x(min)
         x(npx-i+1) = x(max)
      enddo
 
c*****plot the smoothed and raw spectra
      call estim (left,right,dispx,wlx,npx,ileft,iright)
      call plotxy (1,1,wlz(ileft),z(ileft),npz,30)
      call plotxy (1,-1,wlx(ileft),x(ileft),npx,-1)
 
c*****see if the smoothing is OK
50    message = 'IS THE SMOOTHING OK ([y]/n/a)? '
      nchars = 31
      call getasci (nchars)
      if (array(1:1).eq.'y' .or. nchars.le.0) then
         go to 2001
      elseif (array(1:1) .eq. 'n') then
         go to 2
      elseif (array(1:1) .eq. 'a') then
         go to 2002
      else
         go to 50
      endif

c*****depart if the smoothing size looks too big
2000  errmess = 'SMOOTHING SIZE TOO BIG!  I QUIT!'
      nchars = 41
      call puterr (nchars)
2002  do i=1,npz
         x(i) = z(i)
      enddo
2001  npz = nphold
      if (npz .ne. 0) then
         do i=1,npz
            wlz(i) = wave(real(i),npz,dispz)
            z(i) = scratch(i)
         enddo
      endif
      return

      end



