      subroutine rswitch (x,y)
c*****switch the values of the real variables x and y
      
      real*4 x,y,z

      z=x
      x=y
      y=z
      return

      end




      subroutine dswitch (x,y)
c*****switch the values of the real variables x and y
      
      real*8 x,y,z

      z=x
      x=y
      y=z
      return

      end










      subroutine iswitch (i,j)
c*****switch the values of the integer variables i and j
 
      integer i,j,k

      k=j
      j=i
      i=k
      return

      end




