      subroutine savecf (disp,chan,wavl,npts)
c*****save the dispersion coefficients and channel/wavelength information
c     to a disk file to be used for automatic scaling later on.
 
      include 'Chars.com'
      real*8 chan(25), wavl(25)
      real*8 disp(9)
      character charstat*7

c*****first get the filename
      message = 'SAVE THE DISPERSION SOLUTION INFORMATION (y/[n])? '
      nchars = 50
      call getasci (nchars)
      if (array (1:1).eq.'n' .or. nchars.le.0) return
      message = 'ENTER THE FILENAME FOR THE NEW SOLUTION: '
      nchars = 41
      call getasci (nchars)
      call dskfil(15,iostat,array(1:nchars),charstat,'sequential',
     .            'formatted  ',0)
      if (iostat .ne. 0) return

c*****now write out the coefficients
      write (15,1001) (disp(i),i=1,9)
1001  format (4(1pe13.5))

      do 20 i=1,npts
20       write (15,1021) chan(i), wavl(i)
1021  format (f12.2,2x,f12.3)

c*****close the file and return
      close(15)
      return

      end







