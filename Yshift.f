      subroutine yshift (ssmax)
c*****this routine shifts the channels of the y-array by an amount ssmax, to
c     prepare for a division of the x-array by the y-array.
 
      include 'Dataval.com'
      include 'Mathval.com'
 
      if (ssmax .eq. 0.) return

c     initialize some variables
      point = ssmax + 1.
      i = 1
      j = 2
      py = real(npy)

c     this starts a loop
c 1do
c     if the point is not with in the array 
5     if (point.lt.1.0 .or. point.gt.py) go to 20

c     shift the point by an index 
c 2do
6     p = point - real(j)
      if (p.le.0.0 .or. j.eq.npy-1) go to 10
c     break from do loop 2 and move to the shifting
      j = min0(j+1,npy-1)
c     set j equal to the second value or the last-1
      go to 6
c 2enddo

c     do the shifting of the scratch array
c     -----------------------------------
10    scratch(i) = p*(p-1.0)/2.0*y(j-1) + (1.0-p*p)*y(j) +
     1           p*(p+1.0)/2.0*y(j+1)
      go to 40

c     point was outside of the range so set the value equal to zero
20    scratch(i) = 0.
c     -----------------------------------

c     proceed in integer
40    point = point + 1.
      i = i + 1

c     if the value of i is less than the max do another loop
      if (i .le. npy) go to 5
c 1enddo

c     set the y array == scratch array (now shifted)
      do 25 i=1,npy
25       y(i) = scratch(i)
      return
 
      end




