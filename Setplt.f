      subroutine setplt (ipt,wavout,centint,ileft,iright,emflag)
c*****this routine sets up the plot parameters for an individual line.
 
      include 'Dataval.com'
      include 'Plotval.com'
      include 'Widpar.com'
      real*8 wavout,m,b
      integer ilower,iupper

      call findmin (real(ipt),x,npx,point,centint,emflag)
c     fix for dealing with text files
      if (dispx(9) .eq. 0 .and. 
     .     (dispx(1) .eq. 0 .or. dispx(2) .eq. 0 )) then
c         wavout = wlx(ipt)
c     interpolate for subpixel
c        NOTE: int() rounds down
         ilower = int(point)
         iupper = int(point + 1.0)

         if (ilower .le. 1) then ilower = 1
         if (iupper .ge. npx) then iupper = npx
         
         m = (wlx(iupper)-wlx(ilower))/(real(iupper)-real(ilower))
         b = wlx(ilower)-m*real(ilower)
         wavout = m*point+b
      else
         wavout = wave(point,npx,dispx)
      endif
      ileft = ipt - ndelt
      iright = ipt + ndelt
  
      if (ileft .le. 0) then
         ileft = 1
         iright = 2*ndelt + 1
      endif
      if (iright .gt. npx) then
         ileft = npx - 2*ndelt
         iright = npx
      endif

      call minimax (x(ileft),xmin,xmax,2*ndelt+1)
      up = 1.05*xmax
      down = amax1(0.,0.90*xmin)
      xleft = wlx(ileft)
      right = wlx(iright)
      return

      end




