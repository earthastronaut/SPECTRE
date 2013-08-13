      subroutine const (mode,pts,npt)
c*****this routine adds (mode = 1) or multiplies (mode = 2) the values
c     in "pts".
 
      include 'Chars.com'
      real*4 pts(131072)
      real*8 xnum

c*****first shift the pts if desired
      message = 'GIVE THE # OF POINTS TO SHIFT THE ARRAY RIGHT: '
      nchars = 47
      call Getasci (nchars)
      call number (nchars,xnum)
      ishift = int(sngl(xnum))
      if (ishift .ge. npt) then
         errmess = 'CANT HAVE THE SHIFT > THE # OF POINTS! '
         nchars = 39
         call puterr (nchars)
         return
      elseif (ishift .gt. 0) then
         do 15 i=npt,1+ishift,-1
15          pts(i) = pts(i-ishift)
         do 35 i=1,ishift
35          pts(i) = 0.
      elseif (ishift .lt. 0) then
         ishift = iabs(ishift)
         do 20 i=1,npt-ishift
20          pts(i) = pts(i+ishift)
         do 25 i=npt-ishift+1,npt
25          pts(i) = 0.
      endif

c*****addition mode :
      if (mode .eq. 1) then
9        message = 'GIVE CONSTANT TO BE ADDED TO THE POINTS: '
         nchars = 41
         call getnum (nchars,xnum)
         if (xnum .eq. -9999.) go to 9
         xconst = sngl(xnum)
         do 10 i=1,npt
10          pts(i) = pts(i) + xconst

c*****multiplication mode:
      else
12       message = 'GIVE CONSTANT TO MULTIPLY TO THE POINTS: '
         nchars = 41
         call getnum (nchars,xnum)
         if (xnum .eq. -9999.) go to 12
         xconst = sngl(xnum)
         do 11 i=1,npt
11          pts(i) = pts(i)*xconst
      endif
      return

      end



