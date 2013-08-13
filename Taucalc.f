      subroutine taucalc (pts,wl,npt,ltype)
c*****this routine computes values of optical depth in lines,
c     by computing ln(Icont/I).  Practically, this turns out
c     to be ln(1/pts)
c     depending on the value of mode
 
      include 'Chars.com'
      include 'Plotchr.com'
      include 'Plotval.com'
      real*4 pts(131072),wl(131072)
 
c*****compute the tau values
      warn = 0
      plylab = 'TAU'
      ylcnt = 3
      do 10 i=1,npt
         if (pts(i) .le. 0) then
            warn = 1
            pts(i) = 0.001
         endif
10       pts(i) = alog(1./pts(i))
40    call minimax (pts,xxmin,xxmax,npt)
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
 
      end






