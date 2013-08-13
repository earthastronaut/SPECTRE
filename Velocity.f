
      subroutine velocity (type,voverc,disp,wl,npt)
c*****this routine applies or cancels a radial velocity shift.  The
c     shift must have been read in via the FITS header, or the velocity
c     muct have been declared by the user

      character*3 type
      real*8 disp(9)
      real*4 wl(131072), voverc, delta

      if (type .eq. 'yes') then
         delta = voverc
      else
         delta = -voverc
      endif

      disp(1) = disp(1)*(1. + delta)
      do 10 i=1,npt
10       wl(i) = wl(i)*(1. + delta)
      return

      end





