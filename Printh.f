      subroutine printh (section)
c*****this routine prints out the contents of a given 'section' of 
c     file 'helpfile'.
 
      include 'Chars.com'
      character*7 section
      integer qtemp, nchars

     
c*** fix length of code path
      nchars = 80
      call cropstr (codepath,nchars)

c***** Declare path to the helpfile
c==============================================================================c
c     **USER** ====> helpfile
      filepath = codepath(1:nchars)//"helpfile"

c==============================================================================c

c**** Open the file
      call dskfil (10,jostat,filepath,'old    ','sequential',
     .             'formatted  ',0)
      rewind 10

c**** Find the desired section
1     read (10,1001,end=50) array(1:80)
1001  format (a80)
      if (array(1:1) .eq. '^') then
         if (array(2:8) .eq. section) go to 30
      endif
      go to 1

c**** Write out the desired section
30    kount = 1
      write (message,1002)
1002  format (32x,'COMMAND SUMMARY',32x)

10    read (10,1001,end=50) array(1:80)
      if (array(1:1) .eq. '^') go to 50
      call prinfo (kount)

      if (errmess(1:9) .eq. 'stopinfo!') go to 50
      kount = kount + 1
      go to 10
      
50    close (10)
      return 

      end









