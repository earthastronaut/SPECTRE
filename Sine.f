      subroutine sine 
c*****this routine creates a sine wave in the y-array and compares
c     it to the data in the x-array
 
      include 'Dataval.com'
      include 'Plotval.com'
      include 'Chars.com'
      double precision xnum

c*****ask for the frequency of the sine wave
      message = 'NUMBER OF POINTS PER SINE WAVE CYCLE: '
      nchars = 38
      call Getnum (nchars,xnum)
      beatnum = xnum

c*****ask for the change in frequency of the sine wave
      message = 'FREQUENCY CHANGE OF POINTS PER SINE WAVE CYCLE: '
      nchars = 48
      call Getnum (nchars,xnum)
      beatch = xnum

c*****ask for the amplitude of the sine wave
      message = 'PERCENTAGE AMPLITUDE OF THE SINE WAVE: '
      nchars = 39
      call Getnum (nchars,xnum)
      ampnum = xnum

c*****ask for the point shift of the sine wave
      message = 'POINT SHIFT OF THE SINE WAVE: '
      nchars = 30
      call Getnum (nchars,xnum)
      shifnum = xnum

c*****Create the sine wave, with the number of points of the x-array
      npy = npx
      do 100 i=1,npy
      y(i) = 1.0 + ampnum*sin((i-1+shifnum)*2.0*3.14159/
     .                  (beatnum+i*beatch/beatnum))
100   wly(i) = wlx(i)

c*****plot x- and y-arrays
      call plotxy (1,1,wly,y,npy,2)
      call plotxy (1,-1,wlx,x,npx,-1)
      return

      end



