
      subroutine stext (mode,waves,pts,npt,fname,object)
c*****save the data and header information to the specified TEXT file
 
      include 'Chars.com'
      character fname*80,newname*80,object*60
      character charstat*7
      real*4 pts(4096), waves(4096)

      ifnew = 21
 
c*****get a new file name, if desired
      if (mode .eq. 0) then
         newname = fname
      else
         message = 'ENTER THE NEW FILENAME: '
         nchars = 24
         call getasci (nchars)
         nchars = min0(nchars,40)
         newname = array(1:nchars)
      endif
 
c*****open the new file
      if (mode .eq. 0) then
         charstat = 'unknown'
         call dskfil (ifnew,iostat,newname,charstat,'sequential',
     .                'formatted  ',2880)
         if (iostat .ne. 0) go to 1003
      else
         charstat = 'new    '
         call dskfil (ifnew,iostat,newname,charstat,'sequential',
     .                'formatted  ',2880)
         if (iostat .ne. 0) go to 1003
      endif
 
c*****write the object name on the first line of the file
      write (ifnew,1001) object(1:60), npt
1001  format (a60,6x,'npts =',i5)
 
c*****write out the data
      write (ifnew,1004) (waves(i),pts(i),i=1,npt)
1004  format (1p2e20.8)
      close (21)
      return

 
c*****write error messages
1003  mode = -1
      return

      end




