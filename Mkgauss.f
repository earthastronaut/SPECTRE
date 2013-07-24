      subroutine mkgauss (pt,depth,halfl,halfr)
c*****this routine simply fills array 'y' with a Gaussian line profile
c     which has been set in subroutine 'manual'
 
      include 'Dataval.com'
      include 'Datachr.com'
 
      if (depth.eq.0.0 .or. 
     .    (halfl.eq.0.0 .and. halfr.eq.0)) return

      factl = -0.69315/halfl**2
      factr = -0.69315/halfr**2

      do 10 i=1,npx
         wly(i) = wlx(i)
         if (wlx(i) .le. pt) then
            arg = factl*(wlx(i)-pt)**2
            if (arg .le. -15.) then
               y(i) = 1.
            else
               y(i) = 1. - depth*exp(arg)
            endif
         else
            arg = factr*(wlx(i)-pt)**2
            if (arg .le. -15.) then
               y(i) = 1.
            else
               y(i) = 1. - depth*exp(arg)
            endif
         endif
10    continue

      do 20 i=1,9
20       dispy(i) = dispx(i)
      npy = npx
      yary = '  0'
      yobj = 'Gaussian            '
      ykfnam = '--------            '
      yfname = '--------                                '
      call screenstat (1)
      return

      end




