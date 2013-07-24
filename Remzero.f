      subroutine remzero (data,npt)
c*****this routine automatically removes zeroes from data arrays. It cannot
c     handle the zeroes in the first or last parts of the arrays; that must 
c     be done from the 4p command

      include 'Chars.com'
      real*4 data(10000)

      write (message,1001)
1001  format (28(' '),'ZERO REMOVAL INFORMATION',27(' '))
      kount = 1
 
c*****search for and clean out zeros at the array start
      do 30 i=1,npt
         if (data(i) .ne. 0) go to 35
30       continue
      errmess = 'THIS ARRAY CONTAINS NOTHING BUT ZEROS ! '
      nchars = 40
      call puterr (nchars)
      return
35    ilow = i
      if (ilow .gt. 1) then
         do 36 i=1,ilow-1
36          data(i) = data(ilow)
         write (array,1002) ilow-1
1002     format ('ZEROS REPLACED AT START FROM POINTS 1 TO ',
     .           i3,36x)
         call prinfo (kount)
         kount = kount + 1
      endif

c*****search for and clean out zeros at the array end
      do 40 i=npt,ilow,-1
         if (data(i) .ne. 0) go to 45
40       continue
45    ihigh = i
      if (ihigh .lt. npt) then
         do 46 i=npt,ihigh+1,-1
46          data(i) = data(ihigh)
         write (array,1003) ihigh+1,npt
1003     format ('ZEROS REPLACED AT END FROM POINTS ',i4,' TO ',
     .           i4,34x)
         call prinfo (kount)
         kount = kount + 1
      endif

c*****search for and clean out zeros in between non-zero points
      do 10 i=ilow,ihigh
         if (data(i) .gt. 0.) go to 10
         nblu = i
         do 5 j=nblu+1,npt-1
            if (data(j) .le. 0.) go to 5
            nred = j
            go to 15
5           continue
15       slope = (data(nred+1) -data(nblu-1))/float(nred-nblu+2)
         do 20 j = nblu,nred
            xn = i - nblu + 1
20          data(i) = data(nblu-1)+xn*slope
         write (array,1004) ihigh+1,npt
1004     format ('ZEROS REPLACED IN MIDDLE FROM POINTS ',i4,' TO ',
     .           i4,31x)
         call prinfo (kount)
         kount = kount + 1
10    continue
      return

      end





