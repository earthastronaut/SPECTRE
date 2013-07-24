      subroutine contin (mode,nogo,splitd)
c*****this routine does continuum division
 
      include 'Plotval.com'
      include 'Plotchr.com'
      include 'Dataval.com'
      include 'Datachr.com'
      include 'Mathval.com'
      include 'Chars.com'
      logical nogo,splitd
      character mode*2
 
c*****set up the plot
      if (xleft.ne.wlx(1) .or. right.ne.wlx(npx) 
     .    .or. splitd) then
         xleft = wlx(1)
         right = wlx(npx)
      endif
      call minimax (x,xmin,xmax,npx)
      up = 1.12*xmax
      down = 0.8*xmin
      call plotxy (1,1,wlx,x,npx,1)
 
c*****set the continuum
      message = 'MARK THE CONTINUUM POINTS WITH THE CURSOR ' 
      nchars = 42
      call putasci (nchars)
      nogo = .false.  
      call setcont (nogo)
      if (nogo) return

c*****divide the original spectrum by the continuum, and replot
      if (mode .eq. 'sc') then
         npy = npx
         do 20 i=1,npy
            wly(i) = t(i)
20          y(i) = ss(i)
         yobj = '(continuum)         '
         call screenstat (1)
      else
         do 10 i=1,npx
10          x(i) = x(i)/ss(i)
         call minimax (x,xmin,xmax,npx)
         up = 1.12*xmax
         down = 0.
         call plotxy (1,1,wlx,x,npx,1)
      endif

      splitd = .false.
      return

      end



