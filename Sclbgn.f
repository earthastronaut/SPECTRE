      subroutine sclbgn
c*****this routine begins the "scale" displays and rechecks to see whether
c    the wavelength scaling already has been done.
 
      include 'Dataval.com'
      include 'Plotval.com'
      include 'Chars.com'
 
      nchars = 0

c*****print the banner on the information screen
      write (message,3002)
3002  format(23(' '),'DISPERSION COEFFICIENT INFORMATION',22(' '))
      write (array,3003)
3003  format (80(' '))
      call prinfo (1)
       
c*****make sure that the wavelength scaling is desired by the user
 199  if (wlx(1) .ne. 1.) then
         message = 'WANT TO REDO THE WAVLENGTH SCALING ([y]/n)? '
         nchars = 44
         call getasci (nchars)
      endif

c*****if so, clear the channel array and plot
      if (array(1:1).eq.'y' .or. wlx(1).eq.1. .or. nchars.le.0) then
         do 200 i=1,9
200         dispx(i) = 0.
         do 201 i=1,npx
201         wlx(i) = real(i)
         call labset(1)
         call minimax (x,xmin,xmax,npx)
         xleft = 1.
         right = real(npx)
         up = 1.12*xmax
         down = 0.90*xmin
         call plotxy (1,1,wlx,x,npx,1)
         return
      elseif (array(1:1) .eq. 'n') then
         return
      else
         go to 199
      endif
 
      end







