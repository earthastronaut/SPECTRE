      subroutine rescal (mode)
c*****this routine produces a new normalized data set. It notes the points
c     that the user had designated as continuum points, and forces the average
c     height of those points to be 1.
 
      include 'Dataval.com'
      include 'Plotval.com'
      include 'Mathval.com'
 
      numpts = right - xleft + 1
      if (mode .eq. 0) go to 15
 
      avg = 0.
      do 10 i=1,nknots
         ipt = int(real(spx(i)))
10       avg = avg + x(ipt) 
      avg = avg/nknots
      do 20 j=1,npx
20       x(j) = x(j)/avg
      return

15    do 25 j=1,npx
25       x(j) = x(j)/spy(1)
      return

      end   
