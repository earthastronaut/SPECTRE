      subroutine killf
c*****this routine removes disk files from inside the program
 
      include 'Scrnval.com'
      include 'Chars.com'
      logical ex

1     message = 'GIVE THE NAME OF THE FILE TO DELETE: '
      nchars = 37
      call getasci (nchars)

      inquire (file=array(1:nchars),exist=ex)
      if (ex) then
         open (24,file=array(1:nchars))
         close (24,status='delete')
      else
         write (errmess,1002) array(1:nchars)
1002     format ('FILE DOES NOT EXIST: ',a69)
         call puterr (nchars)
         go to 2
      endif

      istat = ivmove(maxline-1,1)
      istat = ivcleol()
      istat = ivmove(maxline-1,1)
      call system (errmess)

2     message = 'DELETE ANY MORE FILES (y/[n])? '
      nchars = 29
      call getasci (nchars)
      if (array(1:1).eq.'n' .or. nchars.le.0) return
      if (array(1:1) .eq. 'y') go to 1
      return

      end










