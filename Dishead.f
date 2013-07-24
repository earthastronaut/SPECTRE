      subroutine dishead
c*****display the header for a given disk file
 
      include 'Chars.com'
      character fname*80
      character charstat*7

c*****get the file name
      call blank
      message = 'ENTER THE FILENAME: '
      nchars =  20
      call getasci (nchars)

c*****open the file
      charstat = 'old    '
      iunit = 8
      fname = array(1:80)
      call dskfil (iunit,jostat,fname,charstat,'direct    ',
     .      'unformatted',2880)
      if (jostat .ne. 0) return

c*****display the FITS header
      nrec = 1
      kount = 1
101   read(unit=iunit,rec=nrec,err=1002,iostat=ierr) head(1:2880)
      write (message,1015)
1015  format (28(' '),'FITS HEADER INFORMATION',28(' '))
      do 190 j=1,36
         k = 80*(j-1)
         write (array,1050) head(k+1:k+80)
1050     format (a80)
         call prinfo (kount)
         if (errmess(1:9) .eq. 'stopinfo!') go to 100
         if (head(k+1:k+3) .eq. 'END') go to 100
190      kount = kount + 1
      nrec = nrec + 1
      go to 101

c*****a bad read occurred
1002  write(errmess,1003) fname,ierr
1003  format('ERROR in reading the header:',
     .          '     FILE= ',a20,'   ERROR =',i4)
      nchars = 73
      call puterr (nchars)
      return

c*****quit the header dump
100   close (iunit)
      return

      end

      







