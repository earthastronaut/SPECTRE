      subroutine dump
c*****this routine dumps the spectra to MONGO-compatible (x,y) pairs
 
      include 'Chars.com'
      include 'Datachr.com'
      include 'Dataval.com'
      character fname*80
 
c*****open the file
      write (fname,1004)
1004  format (80(' '))
      message = 'GIVE A FILENAME PREFIX FOR THE DUMPED SPECTRA: '
      nchars = 47
      call getasci (nchars)
      nchr = nchars
      fname(1:nchr) = array(1:nchr)

c*****dump the x-array
      if (npx .eq. 0) go to 20
      message = 'DUMP THE X-ARRAY ([y]/n)? '
      nchars = 26
      call getasci (nchars)
      if (array(1:1).eq.'y' .or. nchars.le.0) then
         fname(nchr+1:nchr+2) = '.1'
         nch = nchr + 2
         call dskfil (75,jostat,fname(1:nch),'new    ','sequential',
     .                'formatted  ',0)
         if (jostat .ne. 0) return
c         write (75,1002) xobj(1:20), npx
1002     format (a20,10x,'npts =',i5)
         write (75,1003) (wlx(i),x(i),i=1,npx)
1003     format (1p2e20.8)
c1003     format (f20.10,f20.10)
         close (75)
      endif

c*****dump the y-array
20    if (npy .eq. 0) go to 30
      message = 'DUMP THE Y-ARRAY ([y]/n)? '
      nchars = 26
      call getasci (nchars)
      if (array(1:1).eq.'y' .or. nchars.le.0) then
         fname(nchr+1:nchr+2) = '.2'
         nch = nchr + 2
         call dskfil (75,jostat,fname(1:nch),'new    ','sequential',
     .                'formatted  ',0)
         if (jostat .ne. 0) return
         write (75,1002) yobj(1:20), npy
         write (75,1003) (wly(i),y(i),i=1,npy)
         close (75)
      endif

c*****dump the z-array
30    if (npz .eq. 0) go to 40
      message = 'DUMP THE Z-ARRAY ([y]/n)? '
      nchars = 26
      call getasci (nchars)
      if (array(1:1).eq.'y' .or. nchars.le.0) then
         fname(nchr+1:nchr+2) = '.3'
         nch = nchr + 2
         call dskfil (75,jostat,fname(1:nch),'new    ','sequential',
     .                'formatted  ',0)
         if (jostat .ne. 0) return
         write (75,1002) zobj(1:20), npz
         write (75,1003) (wlz(i),z(i),i=1,npz)
         close (75)
      endif

40    return

      end






