      subroutine noise
c*****this routine calculates the signal-to-noise of a region of
c     the spectrum.  The user points to the included region with 
c     the cursor.
 
      include 'Dataval.com'
      include 'Datachr.com'
      include 'Plotval.com'
      include 'Chars.com'
      double precision total
 
c*****first mark the desired region of the x-array
      message = 'MARK LEFT AND RIGHT BOUNDARIES WITH THE CURSOR'
      nchars = 46
      call putasci (nchars)
      call sm_curs (xleft,yyy,ichr)
      call sm_curs (right,yyy,ichr)
      call sm_gflush
      call sm_alpha
      call estim (xleft,right,dispx,wlx,npx,ileft,iright,xfile)
      npt = iright - ileft + 1
 
c*****compute the mean and standard deviation
      total = 0.
      do 20 i=ileft,iright
20       total = total + x(i)
      xmean = total/npt
      total = 0.
      do 30 i=ileft,iright
30       total = total + (x(i)-xmean)**2
      dev = dsqrt(total/(npt-1))
      ston = xmean/dev

c*****plot the section of spectrum
      up = xmean + 5.*dev
      down = xmean - 9.*dev
      xleft = wlx(ileft)
      right = wlx(iright)
      call plotxy (1,1,wlx,x,npx,1)

c*****write out the calculated information on the plot
      call sm_ctype (colors(4)) 
      call sm_expand (0.8)
      write (errmess,1001) xmean
1001  format ('MEAN = ',f10.3)
      xloc = xleft + 0.1*(right-xleft)
      yloc = down + 0.23*(up-down)
      call sm_relocate (xloc,yloc)
      call sm_label (errmess)
      write (errmess,1002) dev
1002  format ('SAMPLE DEVIATION = ',f8.3)
      yloc = down + 0.17*(up-down)
      call sm_relocate (xloc,yloc)
      call sm_label (errmess)
      write (errmess,1003) ston
1003  format ('SIGNAL-TO-NOISE = ',f8.1)
      yloc = down + 0.11*(up-down)
      call sm_relocate (xloc,yloc)
      call sm_label (errmess)

c*****draw lines representing the mean and standard deviation
      call sm_relocate (xleft,xmean)
      call sm_draw (right,xmean)
      call sm_ltype (2)
      call sm_relocate (xleft,xmean+dev)
      call sm_draw (right,xmean+dev)
      call sm_relocate (xleft,xmean-dev)
      call sm_draw (right,xmean-dev)

c*****flush the buffer and return
      call sm_ltype (0)
      call sm_gflush
      call sm_alpha

      end      






