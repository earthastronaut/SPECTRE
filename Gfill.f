      subroutine gfill (pt,depth,half,mode)
c     This routine simply paints in one side of a Gaussian 
c     area on the screen.
      include 'Chars.com'
      include 'Dataval.com'
      include 'Plotval.com'
      character*4 mode
 
      if (depth.eq.0.0 .or. half.eq.0.0) return

      call sm_ctype (colors(4))
      sgn = +1.
      if (mode .eq. 'left') sgn = -1.
      factor = -0.69315/half**2
      step = sign(half/7.,sgn)
      ppt = pt

      do 10 i=1,30
         ppt = ppt + step
         call sm_relocate (ppt,1.0)
         yyy = 1.-depth*exp(factor*(step*i)**2)
10       call sm_draw (ppt,yyy)
c      call setcolor (1)
      call sm_ctype (colors(1))
c      call tidle
      call sm_gflush
      call sm_alpha
      return

      end





