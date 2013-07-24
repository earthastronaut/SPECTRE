      subroutine hunt (wave,idelta,ipt)
c*****this routine hunts for the point nearest to the line center (deepest
c     point) of the input requested "wave".
 
      include 'Dataval.com'
      
      real*8 wave
 
      call estim (sngl(wave),sngl(wave),dispx,wlx,npx,ipt,jpt)

      if (ipt.ge.npx .or. ipt.le.1) then
         ipt = -1
         return
      endif

      low = max0(1,ipt-idelta)
      ihigh = min0(npx,ipt+idelta)
      xlow = x(ipt)
      do 10 i=low,ihigh
         if (x(i) .ge. xlow) go to 10
         xlow = x(i)
         ipt = i
10       continue


      if (ipt.ne.low .and. ipt.ne.ihigh) return
      ipt = -ipt
      return

      end




