      subroutine charith (icode)
c*****this routine performs arithmetic operations on spectrum arrays.
c     whole data-point operations are assumed throughout.
c     if the y-array is shifted, the operation is performed only
c     on the overlap region.
c        icode = 1 for addition   :  x + y -> x
c        icode = 2 for subtraction:  x - y -> x
c        icode = 3 for division   :  x / y -> x
 
      include 'Dataval.com'
      include 'Plotval.com'
      include 'Chars.com'
      double precision xnum

c*****check to see that the arrays are not empty, and the # of points
c     are equal in the 2 arrays
      empty = 0
      do 10 i=1,50
10       if (y(i) .eq. 0.) empty = empty + 1
      if (empty .gt. 25) then
         message = 'CANT DO ARRAY ARITHMETIC: Y-ARRAY IS EMPTY!'
         nchars = 43
         call Putasci (nchars)
         return
      endif
      empty = 0
      do 11 i=1,50
11       if (x(i) .eq. 0.) empty = empty + 1
      if (empty .gt. 10) then
         message = 'CANT DO ARRAY ARITHMETIC: X-ARRAY IS EMPTY!'
         nchars = 43
         call Putasci (nchars)
         return
      endif
      if (npx .ne. npy) then
         message = 'CANT DO ARRAY ARITHMETIC: npx .NE. npy!'
         nchars = 38
         call Putasci (nchars)
         return
      endif
      
c*****enter a y-array shift, if desired
      write (message,1001)
1001  format ('SHIFT THE Y-ARRAY BY HOW MANY POINTS ',
     .        'TO THE RIGHT? ')
      nchars = 51
      call Getasci (nchars)
      call number (nchars,xnum)
      ishift = int(xnum)
      ilo = max0(ishift+1,1)
      ihi = min0(npx+ishift,npx)
      go to (100,200,300),icode

c*****Do the arithmetic operation
100   do 101 i=ilo,ihi
101      x(i) = x(i) + y(i-ishift)
      go to 400
200   do 201 i=ilo,ihi
201      x(i) = x(i) - y(i-ishift)
      go to 400
300   do 301 i=ilo,ihi
301      x(i) = x(i) / y(i-ishift)

c*****get new data min, max for the x-array
400   call minimax (x,xmin,xmax,npx)
      return

      end







