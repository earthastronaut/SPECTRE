      subroutine logit (pts,wl,npt,ltype,mode)
c*****this routine converts data arrays to and from base ten logarithms,
c     depending on the value of mode
 
      include 'Chars.com'
      include 'Plotchr.com'
      include 'Plotval.com'
      real*4 pts(131072),wl(131072)
 
      if (mode .eq. 1) go to 20

c*****take the base10 logarithm
      warn = 0
      plylab = 'LOG INTENSITY'
      ylcnt = 13
      do 10 i=1,npt
         if (pts(i) .le. 0) then
            warn = 1
            pts(i) = 0.001
         endif
10       pts(i) = alog10(pts(i))
40    call minimax(pts,xxmin,xxmax,npt)
      xleft = wl(1)
      right = wl(npt)
      up = 1.12*xxmax
      down = 0.80*xxmin
      call plotxy (1,1,wl,pts,npt,ltype)
      if (warn .gt. 0) then
         errmess = 'WARNING: POINTS <= 0 WERE SET TO 0.001 !'
         nchars = 39
         call puterr (nchars)
      endif
      return
 
20    warn = 0
      message = 'WARNING: POINTS > 37.5 WERE SET TO 37.5/'
      plylab = 'INTENSITY'
      ylcnt = 9
      do 30 i=1,npt
         if (abs(pts(i)) .gt. 37.5) then
            warn = 1
            pts(i) = sign(37.5,pts(i))
         endif
30       pts(i) = 10.**pts(i)
      go to 40
 
      end






