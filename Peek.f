      subroutine peek (nspec)
c*****this routine simply is a loop which lets the user compare small
c     parts of arrays x, y, and z through plots of those spectra. The
c     number of arrays to be plotted are given by parameter "nspec".
 
      include 'Dataval.com'
      include 'Plotval.com'
      include 'Mathval.com'
      include 'Chars.com'
      real*4 pixel(10000)
 
      ileft = 1
      iright = min0(ileft+299,npx)
      do 20 i=1,npx
20       pixel(i) = i
5     npts = iright - ileft + 1
      xleft = ileft
      right = iright
      call minimax (x(ileft+6),xxmin,xxmax,iright-ileft-11)
      call minimax (y(ileft+6),xymin,xymax,iright-ileft-11)
      call minimax (z(ileft+6),xzmin,xzmax,iright-ileft-11)
      down = 0.9*amin1(xxmin,xymin,xzmin)
      up = 1.1*amax1(xxmax,xymax,xzmax)

      if (nspec .eq. 3) then
         call plotxy (1,1,pixel,z,npz,3)
         call plotxy (1,-1,pixel,y,npy,-2)
         go to 6
      else
         call plotxy (1,1,pixel,y,npy,2)
      endif
6     call plotxy (1,-1,pixel,x,npx,-1)

      ileft = ileft + 300
      if (ileft .ge. npx-50) return
      iright = min0(ileft+299,npx)
      message = 'WANT TO SEE ANOTHER SECTION OF SPECTRA ([y]/n)? '
      nchars = 48
      call getasci (nchars)
      if (array(1:1).eq.'y' .or. nchars.le.0) go to 5
      return
      
      end
