      subroutine fillist
c*****list the contents of the named file by the standard 'ls' command

      include 'Chars.com'
      logical ex

      inquire (file='/tmp/lsfile',exist=ex)
      if (ex) then
         open (33,file='/tmp/lsfile')
         close (33,status='delete')
      endif

      write (errmess,1001)
1001  format ('ls -C > /tmp/lsfile')
      call system (errmess)
      open (33,file='/tmp/lsfile')

      write (message,1003)
1003  format (33(' '),'THE FILE LIST',33(' '))
      kount = 1
10    read (33,1002,end=50) array
1002  format (a80)
      call prinfo (kount)
      if (errmess(1:9) .eq. 'stopinfo!') go to 50
      kount = kount + 1
      go to 10

50    close (33,status='delete')
      return

      end






