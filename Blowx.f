      subroutine blowx
c*****this routine blows up the plot to x-limits set by the cursor

      include 'Dataval.com'
      include 'Plotval.com'
 
c*****get the left and then the right bounds - replot
c      call mongohairs (ichr,xleft,yyy)
c      call mongohairs (ichr,right,yyy)
      call sm_curs (xleft,yyy,ichr)
      call sm_curs (right,yyy,ichr)
      call sm_gflush
      call sm_alpha

      if (right.lt.left) call rswitch(left,right)

c*****replot the data of the x-array
      call plotxy (1,1,wlx,x,npx,1)
      return

      end      






