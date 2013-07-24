      subroutine puterr (num)
c*****this routine prints out the characters contained in
c     'errmess'

      include 'Chars.com'
      include 'Scrnval.com'

      istat = ivmove(maxline-1,1)
      istat = ivcleol()
      istat = ivmove(maxline-1,1)
      write (*,1001) errmess(1:79)
1001  format (' ',a79)
      return

      end







