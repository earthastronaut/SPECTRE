      subroutine prdata (wl,pts,npt)

c*****this routine prints out (wl,pts) pairs of spectrum data

      include 'Chars.com'
      real*4 wl(131072),pts(131072)
      
      write (message,1005)
1005  format (30x,'SPECTRUM DATA PAIRS' ,30x)
      write (errmess,1000)
1000  format (80(' '))

      wlmax = -1.0e6
      ptmax = -1.0e6
      do 10 i=1,npt
         ptmax = amax1(ptmax,pts(i))
10       wlmax = amax1(wlmax,wl(i))
      logwl = int(alog10(wlmax))
      logpt = int(alog10(ptmax))

      iafter1 = 5 - logwl 
      iafter2 = 1
      if (logwl.lt.6 .and. logpt.lt.6) then
         write (errmess,1001) iafter1,iafter1
1001     format ('(4("(",f7.',i1,',",",f7.',i1,',")  "))')
      elseif (logwl.lt.6 .and. logpt.ge.6) then
         write (errmess,1002) iafter1,iafter2
1002     format ('(4("(",f7.',i1,',",",e7.',i1,',")  "))')
      elseif (logwl.ge.6 .and. logpt.lt.6) then
         write (errmess,1003) iafter2,iafter1
1003     format ('(4("(",e7.',i1,',",",f7.',i1,',")  "))')
      elseif (logwl.ge.6 .and. logpt.ge.6) then
         write (errmess,1004) iafter2,iafter2
1004     format ('(4("(",e7.',i1,',",",e7.',i1,',")  "))')
      endif

      kount = 1
      do 30 i=1,npt,4
         write (array,errmess) (wl(i-1+j),pts(i-1+j),j=1,4)  
         call prinfo (kount)
         if (errmess(1:9) .eq. 'stopinfo!') go to 35
30       kount = kount + 1
35    return

      end

