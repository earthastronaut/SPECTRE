      subroutine  setav (choice,xnum)
c****this routine runs a section of Average.f 
c****created Dylan Gregersen
      include 'Dataval.com'
      include 'Chars.com'
      include 'Mathval.com'
      double precision xnum
      character*1 choice

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
         xnum = inum
      elseif (choice .eq. 'g') then
3        message = 'GAUSSIAN FWHM (IN PIXELS) = '
         nchars = 28
         call getnum (nchars,xnum)
         if (xnum .eq. -9999.) go to 3
         sigma = xnum
      else
         go to 2
      endif
      return
      end
