      subroutine zoom (mode)
c*****this routine zoooms into out out of a particular spectrum portion.
c     In zooming in, a quarter of the screen, centered about the cursor,
c     is displayed.
 
      include 'Chars.com'
      include 'Dataval.com'
      include 'Plotval.com'
      character mode*3
      real*4 l(6),r(6),u(6),d(6)
      data kount/0/
 
      if (mode .eq. 'out') go to 100
 
c*****let's zoom in:  make sure it is allowed, and save old boundaries
      kount = kount + 1
      if (kount .gt. 6) then
         errmess = 'CANNOT ZOOM IN ANY MORE! '
         nchars = 25
         call puterr (nchars)
         return
      endif
      l(kount) = xleft
      r(kount) = right
      u(kount) = up
      d(kount) = down
 
c*****point to the middle of the desired zoom in window
      message = 'USE THE CURSOR TO SHOW THE MIDDLE OF THE ZOOM WINDOW'
      nchars =  52
      call putasci (nchars)
c      call mongohairs (ichr,xusr,yusr)
      call sm_curs (xusr,yusr,ichr)
      call sm_gflush
      call sm_alpha
      delx = (right - xleft)/4.
      dely = (up - down)/4.
 
c*****set the zoom window boundaries
      if (xusr-delx .gt. xleft) then
         if (xusr+delx .lt. right) then
            xleft = xusr - delx
            right = xusr + delx
         else
            xleft = right - 2.*delx
         endif
      else
         right = xleft + 2.*delx
      endif
      if (yusr-dely .gt. down) then
         if(yusr+dely .lt. up) then
            up = yusr + dely
            down = yusr - dely
         else
            down = up - 2.*dely
         endif
      else
         up = down + 2.*dely
      endif

c*****make the zoomed plot
50    call plotxy (1,1,wlx,x,npx,1)
      return
 
c*****let's zoom out back to the last set of plot boundaries; make sure it
c     is allowed
100   if (kount .le. 0) then
         errmess = 'CANNOT ZOOM OUT ANY MORE! '
         nchars = 26
         call puterr (nchars)
         return
      endif

c*****remember the old boundaries
      xleft = l(kount)
      right = r(kount)
      up = u(kount)
      down = d(kount)
      kount = kount - 1
      go to 50
 
      end      







