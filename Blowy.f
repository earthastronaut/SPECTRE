      subroutine blowy
c*****this routine blows up the plot to y-limits set by the cursor

      include 'Dataval.com'
      include 'Plotval.com'
 
c*****get the up and the down boundaries
c      call mongohairs (ichr,xxx,down)
c      call mongohairs (ichr,xxx,up)
      call sm_curs (xxx,down,ichr)
      call sm_curs (xxx,up,ichr)
      call sm_gflush
      call sm_alpha
      if (up .lt. down) call rswitch(up,down)
 
c*****replot the data
      call plotxy (1,1,wlx,x,npx,1)
      return

      end      



