
      subroutine velset (voverc)
c*****this routine sets a radial velocity or wavelength offset
c     via the user's input
 
      include 'Chars.com'
      include 'Dataval.com'
      real*8 xnum, delta
      real*4 voverc
 
c*****first, find out whether it is a velocity or wavlength shift,
c     then read in the number.
      message = 'RADIAL VELOCITY OR WAVELENGTH SHIFT (v,w,a)? '
      nchars = 45
      call getasci (nchars)
      if     (array(1:1).eq.'v') then 
47       message = 'RADIAL VELOCITY SHIFT = '
         nchars = 24
         call getnum (nchars,xnum)
         if (xnum .eq. -9999.) go to 47
         voverc = sngl(xnum/3.0d+5)
      elseif (array(1:1) .eq. 'w') then
48       message = 'WAVELENGTH SHIFT = '
         nchars = 19
         call getnum (nchars,xnum)
         if (xnum .eq. -9999.) go to 48
         delta = xnum
49       message = 'AT OBSERVED WAVELENGTH = '
         nchars = 25
         call getnum (nchars,xnum)
         if (xnum .eq. -9999.) go to 49
         voverc = sngl(delta/(xnum-delta))
      elseif (array(1:1) .eq. 'a') then
         return
      endif

      return
      end







