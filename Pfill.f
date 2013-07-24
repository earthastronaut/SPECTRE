      subroutine pfill (ileft,iright)
c*****this routine simply paints in the total profile of a line between two
c     markers on the screen.
 
      include 'Dataval.com'
      include 'Chars.com' 
      call sm_ctype (colors(4))
      do 10 i=ileft,iright
         call sm_relocate (wlx(i),1.0)
10       call sm_draw (wlx(i),x(i))
c      call setcolor (1)
      call sm_ctype (colors(1))
      return

      end



