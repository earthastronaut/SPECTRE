      subroutine deconv()
c*****this routine deconvolves a spectrum to try to recover the
c     original resolution.  It uses the precepts of Gilliland et al,
c     1992, PASP, 367.

      include 'Dataval.com'
      include 'Datachr.com'
     
      include 'Plotval.com'
      include 'Mathval.com'
      include 'Chars.com'
      real*8 fwhm, delmax, sigma, aa, power
      real*4 p(50), minval, maxval, relax
      real*4 lastspec(10000), newspec(10000), smeared(10000)
      equivalence (lastspec,ss), (newspec,t), (smeared,scratch)

c   get the input parameters of the fwhm of the assumed gaussian
c   smoothing function, and the relaxation constant which controls
c   how fast the changes to the input spectrum will be made
1     message = 'GIVE THE (PIXEL) FWHM OF THE SMOOTHING FUNCTION: '
      nchars = 48
      call getnum (nchars,fwhm)
      if (fwhm .eq. -9999.) go to 1
      if (fwhm .le. 0.0) return
2     message = 'GIVE THE MAXIMUM "RELAXATION" SIZE: '
      nchars = 36
      call getnum (nchars,delmax)
      if (delmax .eq. -9999.) go to 2

c  put the original spectrum from the x-array into the scratch array
      do i=1,npx
         lastspec(i) = x(i)
      enddo
      call minimax (x,xmin,xmax,npx)
      minval = 0.
      maxval = xmax + 0.05

c  compute a Gaussian smoothing function
      sigma = fwhm/2
      aa = 0.6932/sigma**2
      power = 1.0
      do i=1,50
         p(i) = dexp(-aa*i**2 )
         power = power + 2*p(i)
         if (p(i) .lt. 0.05) go to 10
      enddo
      write (errmess,1001) fwhm
1001  format ('GAUSSIAN TOO BIG! HALF WIDTH=',f6.2)
      nchars = 35
      call puterr (nchars)
      return
10    jdel = i

c  run the Gaussian through the last iteration of the deconvolved spectrum
20    min = jdel + 1
      max = npx - jdel
      do i=1,jdel
         smeared(i) = 1.
         smeared(npx-i+1) = 1.
      enddo
      do i=min,max
         smeared(i) = lastspec(i)
         do j=1,jdel
            smeared(i) = smeared(i) + 
     .                   p(j)*(lastspec(i-j) + lastspec(i+j))
         enddo
         smeared(i) = smeared(i)/power
      enddo

c  create the new iteration by essentially subtracting off the amount of 
c  the Gaussian-smeared last iteration, attenuating (relaxing) the 
c  corrections near the minimum and maximum original data values
          open(35,file='dump')
          write (35,1002) fwhm, delmax, maxval, minval
1002      format (4f10.3)
      do i=1,npx
         relax = delmax*(1. - abs(lastspec(i)-(maxval+minval)/2.)*
     .           2./(maxval-minval))
         newspec(i) = lastspec(i) + relax*(x(i) - smeared(i))
          write (35,1003) i, x(i), lastspec(i), smeared(i),
     .                    relax, newspec(i)
1003      format (i5, 6f10.4)
      enddo
          close (35)

c  plot a small portion of the spectrum to see if it is ok
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
      call plotxy (1,-1,wlx,newspec,npx,-4)
      call plotxy (1,-1,wlx,newspec,npx,-40)
      message = 'SATISFIED WITH THE DECONVOLUTION ([y]/n/a)?'
      nchars = 43
      call getasci (nchars)
      call minimax (x,xmin,xmax,npx)
      xleft = wlx(1)
      right = wlx(npx)
      down = 0.0
      up = 1.12*xmax
      if (array(1:1).eq.'y' .or. nchars.le.0) then
         do i=1,npx
            x(i) = newspec(i)
         enddo
         return
      elseif (array(1:1) .eq. 'a') then
         return
      elseif (array(1:1) .eq. 'n') then
         do i=1,npx
             lastspec(i) = newspec(i)
         enddo
         go to 20
      endif


      end





  


