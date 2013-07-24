      subroutine setplt (ipt,wavout,centint,ileft,iright,emflag)
c*****this routine sets up the plot parameters for an individual line.
 
      include 'Dataval.com'
      include 'Plotval.com'
      include 'Widpar.com'
      real*8 wavout
 
      call findmin (real(ipt),x,npx,point,centint,emflag)
      wavout = wave(point,npx,dispx)
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




