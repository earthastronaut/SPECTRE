      subroutine rnorm (pts,wl,npt,icol)
c*****renormalize to a given number, after division

      include 'Chars.com'
      include 'Plotval.com'
      real*4 pts(10000),wl(10000)
      real*8 xnum

c*****get the normalization point
c      call mongohairs (ichr,xcont,cont)
      call sm_curs (xcont,cont,ichr)
      call sm_gflush
      call sm_alpha
c      call tidle

c*****ask the user to declare a value for that point
      message = 'DECLARE A VALUE FOR THIS POINT [1.0]: '
      nchars = 38
      call getnum (nchars,xnum)
      if (xnum .ne. -9999.) then
         contin = sngl(xnum)
      else
         contin = 1.0
      endif

c*****now renormalize
      do 10 i = 1,npt
            pts(i) = contin*pts(i)/cont
10      continue

c*****find a new max/min and reset up/down for the plot
      call minimax (pts,xmin,xmax,npt)
      up = 1.12*xmax
      down = 0.
      call plotxy (1,1,wl,pts,npt,icol)
      return

      end



