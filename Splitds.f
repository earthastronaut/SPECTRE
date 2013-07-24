      subroutine splitds (x,y,npt,ltype)
c*****display the array X on a split screen.  Does the same as a
c*****call to split with 2 quadrants but makes coding easier
 
      include 'Plotval.com'
      real*4 x(10000),y(10000)

c*****plot left half on first quadrant
      oldlft = xleft
      oldrgt = right
      right = (right-xleft)/2. + xleft
      if (ltype .lt. 0) then
         iquad = -2
      else
         iquad = 2
      endif
      call plotxy (2,iquad,x,y,npt,ltype)

c*****plot right half on second quadrant
      xleft = right
      right = oldrgt
      iquad = -1
      call plotxy (2,iquad,x,y,npt,ltype)
      xleft = oldlft
      right = oldrgt
      return

      end


