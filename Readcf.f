      subroutine readcf (disp,chan,wavl,npts)
c*****read the dispersion coefficients and channel/wavelength information
c     from a disk file
 
      include 'Chars.com'
      character charstat*7
      real*8 chan(25), wavl(25), disp(9)

c*****first get the filename
      message = 'ENTER THE FILENAME WITH THE OLD COEFFICIENTS: '
      nchars =  46
      call getasci (nchars)
      charstat = 'old    '
      call dskfil(15,iostat,array(1:nchars),charstat,'sequential',
     .      'formatted  ',0)
      if (iostat .ne. 0) return

c*****now read the coefficients
      read (15,1001) (disp(i),i=1,9)
1001  format (4(1pd13.5))
      write (array,3001) (disp(i),i=1,4)
3001  format ('OLD COEFFS = ',1p4d13.5)
      call prinfo (2)

c*****now read the old set of wavelengths and channels
      i = 1
19    read (15,1021,end=20) chan(i),wavl(i)
1021  format (f12.2,2x,f12.3)
      i = i + 1
      go to 19
20    npts = i - 1

c*****close the file and return
      close(15)
      return

      end






