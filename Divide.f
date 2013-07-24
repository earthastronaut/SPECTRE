      subroutine divide (linpos,nline,avg,mode)
c*****this routine does the actual work of the division, making a first
c     attempt with automatically chosen lines, or with a manual adjustment
c     of the telluric lines.
 
      include 'Dataval.com'
      include 'Mathval.com'
      real*4 ratio(25),dump(25)
      equivalence (ratio,spx),(dump,spy)
      integer linpos(25)
 
      xxmax = 0.
      yymax = 0.
      do 40 i=1,npx
40       xxmax = amax1(xxmax,x(i))
      do 45 i=1,npy
45       yymax = amax1(yymax,y(i))
      if (mode .eq. 2) go to 35
      if (nline .eq. 0) go to 70
      do 10 i=1,nline
      iline = linpos(i)
10    ratio(i) = (xxmax-x(iline))/xxmax/(yymax-y(iline))*yymax
      iline = 0
      do 12 i=1,nline
         if (ratio(i).lt.0.75 .or. ratio(i).gt.1.3333) go to 12
         iline = iline + 1
         dump(iline) = ratio(i)
12       continue
      do 14 i=1,iline
14    ratio(i) = dump(i)
      nline = iline
      if (nline .eq. 0) go to 70
      do 15 i=1,nline
         ratmin = ratio(i)
         do 16 j=i,nline
            if(ratio(j) .ge. ratmin) go to 16
            ratmin = ratio(j)
            jmin = j
16          continue
         ratio(jmin) = ratio(i)
15       ratio(i) = ratmin
      iline = 0
      avg = 0.
      do 20 i=1,nline
         iline = iline + nline + 1 - i
20       avg = avg + ratio(i)*(nline+1-i)
70    if (avg.eq.0.0 .or. nline.eq.0) then
         avg = 1.
      else
         avg = avg/iline
      endif
35    do 50 i=1,npy
         if (scratch(i) .gt. 0.) then
            y(i) = yymax*(1. - (yymax-scratch(i))/yymax*avg)
            x(i) = x(i)/y(i)*xxmax
         else
            x(i) = 0.
         endif
50       continue
      xxmax = 0.
      do 60 i=1,npx
60       xxmax = amax1(xxmax,x(i))
      do 65 i=1,npx
65       x(i) = x(i)/xxmax
      return

      end



