      subroutine zot (nogo,option,wl,pts,npt,disp,istyle)
c*****this routine either a) replaces the array ends with values
c     of zero, or b) chops points off the array ends to end up with
c     fewer points

      include 'Chars.com'
      include 'Plotval.com'
      real*4 wl(10000),pts(10000)
      real*8 disp(9)
      real*8 xnum
      character option*2

      if (option .eq. 'ch') go to 100

c*****replace the end point values of the array with zeros
      write (message,1001)
1001  format ('GIVE # OF POINTS AT THE BEGINNING TO REPLACE',
     .        ' WITH ZEROS: ')
      nchars = 57
      call getnum (nchars,xnum)
      ipt = int(sngl(xnum))
      if (ipt .gt. npt) go to 300
      do 10 i=1,ipt
10       pts(i) = 0.
      write (message,1002)
1002  format ('GIVE # OF POINTS AT THE END TO REPLACE',
     .        ' WITH ZEROS: ')
      nchars = 51
      call getnum (nchars,xnum)
      ipt = int(sngl(xnum))
      if (ipt .le. npt) then
         do 20 i=npt-ipt+1,npt
20          pts(i) = 0.
      endif
      call plotxy (1,1,wl,pts,npt,istyle)
      return

c*****chop off the ends of the array, leaving with fewer npts
100   write (message,1003)
1003  format ('GIVE # OF POINTS AT THE BEGINNING TO ELIMINATE: ')
      nchars = 48
      call getnum (nchars,xnum)
      ipt = int(sngl(xnum))
      if (ipt .gt. npt-1) go to 300
      mpt = npt - ipt
      do 110 i=1,mpt
         wl(i) = wl(i+ipt)
110      pts(i) = pts(i+ipt)
      write (message,1004)
1004  format ('GIVE # OF POINTS AT THE END TO ELIMINATE: ')
      nchars = 42
      call getnum (nchars,xnum)
      ipt = int(sngl(xnum))
      if (ipt .gt. 0) mpt = mpt - ipt
      do 120 i=mpt+1,npt
         wl(i) = 0.
120      pts(i) = 0.
      npt = mpt
      do 310 i=1,9
310      disp(i) = 0.
      do 320 i=1,npt
320      call wave (real(i),npt,disp)
      xleft = wl(1)
      right = wl(npt)
300   call plotxy (1,1,wl,pts,npt,istyle)
      write (errmess,2001)
2001  format ('WARNING: ANY EXTANT DISPERSION SOLUTION HAS ',
     .        'BEEN DESTROYED! ')
      nchars = 60
      call puterr (nchars)
      return

      end





