      subroutine getnum (nchars,xnum)
c*****this routine gets a double precision number from screen input

      real*8 xnum

      call getasci (nchars)
1     call number (nchars,xnum)
      return

      end

