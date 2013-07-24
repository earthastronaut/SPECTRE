      subroutine findmin (xline,x,npx,pmin,qmin,emflag)
c*****this routine searches for the minimum of array x, in the vicinity
c     of position xline. A spline fit enables fractional values to be
c     returned
 
      dimension x(1),dummy(21)
      integer emflag
 
      if (xline.lt.7.0 .or. xline.gt.real(npx)-7.0) then
         pmin = -9999.
         return
      endif
      ipt = int(xline)
      imin = ipt
      qmin = x(imin)
      idel = 6
      if (emflag .eq. 1) then
         if (x(imin-2).lt.qmin .and. x(imin+2).lt.qmin) idel = 2
         do 26 i=ipt-idel,ipt+idel
            if (x(i) .le. qmin) go to 26
            imin = i
            qmin = x(imin)
26       continue
      else
         if (x(imin-2).gt.qmin .and. x(imin+2).gt.qmin) idel = 2
         do 25 i=ipt-idel,ipt+idel
            if (x(i) .ge. qmin) go to 25
            imin = i
            qmin = x(imin)
25       continue
      endif
      pmin = real(imin)
      do 30 i=1,21
         p = -1. + 0.1*(i-1)
30       dummy(i) = p*(p-1.)/2.*x(imin-1) + (1.-p*p)*x(imin) +
     1           p*(p+1.)/2.*x(imin+1)
      qmin = dummy(1)
      do 35 i=2,21
         if (emflag .eq. 1) then
            if (dummy(i) .le. qmin) go to 35
         else
            if (dummy(i) .ge. qmin) go to 35
         endif
         qmin = dummy(i)
         p = pmin - 1 + 0.1*(i-1)
35       continue
 
      pmin = p
      return

      end



 
 
 
