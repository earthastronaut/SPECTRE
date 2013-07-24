      subroutine positon
c*****this routine shows the region around a particular channel or
c     wavelength that the user specifies
 
      include 'Dataval.com'
      include 'Datachr.com'
      include 'Plotval.com'
      include 'Chars.com'
      double precision wave
 
      message = 'GIVE THE DESIRED CHANNEL OR WAVELENGTH: '
      nchars = 40
      call getnum (nchars,wave)
      xpoint = sngl(wave)
      if (xpoint .lt. wlx(1) .or. xpoint .gt. wlx(npx)) then
         errmess = 'THIS VALUE IS OUT OF RANGE! '
         nchars = 28
         call puterr (nchars)
         return
      else

         call estim (xpoint,xpoint,dispx,wlx,npx,ipt,ipt,xfile)
         call estim (xleft,right,dispx,wlx,npx,ileft,iright,xfile)


         npt = iright - ileft + 1
         if (wlx(ipt) .gt. xpoint) ipt = max0(ipt-1,1)
         if (wlx(ipt+1) .lt. xpoint) ipt = min0(ipt+1,npx)
         if (xpoint.gt.xleft .and. xpoint.lt.right .and. 
     .        npt.lt.203) go to 15
         ileft = max0(ipt-100,1)
         iright = min0(ipt+100,npx-1)
         npt = iright - ileft + 1
         call minimax (x(ileft),xxmin,xxmax,npt)
         up = 1.12*xxmax
         down = 0.8*xxmin
         xleft = wlx(ileft)
         right = wlx(iright)
         call plotxy (1,1,wlx(ileft),x(ileft),npt,1)
15       yusr = (xpoint-wlx(ipt))/(wlx(ipt+1)-wlx(ipt))*
     .      (x(ipt+1)-x(ipt))  + x(ipt)
         call hit (xpoint,real(ipt),yusr)
      endif
      return

      end
         



