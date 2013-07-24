      subroutine blowbox
c*****this routine blows up the plot in both dimensions.  The
c     cursor is used to mark 2 opposite corners of the desired box

      include 'Dataval.com'
      include 'Plotval.com'
 
c*****get lower-left and upper-right corners
       call sm_curs (xleft,down,ichr)
       call sm_curs (right,up,ichr)
       call sm_gflush
       call sm_alpha

c*****check corners - switch or use old values if needed
      if (right .lt. xleft) call rswitch (xleft,right)
      if (up .lt. down) call rswitch (up,down)
 
c*****replot the data
      call plotxy (1,1,wlx,x,npx,1)
      return

      end





