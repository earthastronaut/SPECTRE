      subroutine dark (data,ndata,ndark,mask)
c*****this routine subtracts dark counts from reticon data 
c     and then masks it.
 
      include 'Chars.com'
      integer ndark(4),mask(2)
      real*4 data(131072)

      if ((ndark(1).le.ndark(2)) .and. (ndark(2).gt.0 .and.
     1   mask(1).gt.0))  go to 101
      if ((ndark(4).le.ndark(3)) .and. (ndark(3).gt.0 .and.
     1   mask(2).gt.0))  go to 101

101   if (ndark(1) .eq. 0) ndark(1) = 1
      if (ndark(4) .eq. 0) ndark(4) = 1
      avblu = 0.0
      avred = 0.0
      if (ndark(2).eq.0 .or. ndark(1).eq.ndark(2)) go to 103
      n1 = ndark(1)
      n2 = ndark(2)
      do 102 i = n1,n2
102      avblu = avblu + data(i)
      avblu = avblu/float(n2-n1+1)

103   if (ndark(3).eq.0 .or. ndark(1).eq.ndark(2)) go to 105
      n3 = ndata-ndark(3)+1
      n4 = ndata-ndark(4)+1
      do 104 i = n3,n4
104      avred = avred+data(i)
      avred = avred/float(n4-n3+1)
 
105   if (ndark(2) .eq. 0) avblu = avred
      if (ndark(3) .eq. 0) avred = avblu
      write (array,905) avblu,avred
905   format ('AVERAGE DARK COUNT ON LEFT AND RIGHT:',2f8.1,27x)
      call prinfo (3)

      xn = float(ndata+1)-float(ndark(1)+ndark(2)+ndark(3)+ndark(4))/2.
      slope = (avred-avblu)/xn
      xn = float(ndark(1)+ndark(2)-1)/2.
      dstart = avblu-slope*xn
      do 106 i = 1,ndata
         xn = i-1
106      data(i) = data(i) - dstart - slope*xn
 
c*****move numbers in data array mask(1) places to left.
      n = ndata-mask(1)-mask(2)
      nshift = mask(1)
      ndata = n
      do 107 i = 1,n
107      data(i) = data(i+nshift)

      write (array,908) ndata
908   format ('THE NEW NUMBER OF POINTS IS ',i4,48x)
      call prinfo (4)
      return

      end



  
