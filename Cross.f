      subroutine cross (ishift,rxy,x,y,npx,npy)
c*****this routine determines the correlation of array 'x' and array 'y',
c     comparing array elements 'i' in 'x' with elements 'i+ishift'.
 
      dimension x(10000),y(10000)
      real*8 xsum,ysum,xysum,x2sum,y2sum,covar,varx,vary
 
      xsum = 0.
      ysum = 0.
      xysum = 0.
      x2sum = 0.
      y2sum = 0.

      if (ishift .ge. 0) then
         minpt = 1
         maxpt = npx - ishift
      else
         minpt = iabs(ishift) + 1
         maxpt = npx
      endif

      do 10 i=minpt,maxpt
         xx = x(i)
         yy = y(i+ishift)
         xsum = xsum + xx
         ysum = ysum + yy
         xysum = xysum + xx*yy
         x2sum = x2sum + xx*xx
10       y2sum = y2sum + yy*yy

      xn = real(maxpt - minpt + 1)
      covar = xn*xysum - xsum*ysum
      varx = dsqrt(xn*x2sum - xsum*xsum)
      vary = dsqrt(xn*y2sum - ysum*ysum)
      rxy = sngl(covar/(varx*vary))
      return

      end





