      subroutine putasci (num)
c*****this routine prints out the characters contained in
c     'message'

      include 'Chars.com'
      include 'Scrnval.com'

      do 10 i=1,2
         istat = ivmove(maxline-2+i,1)
10       istat = ivcleol()
      istat = ivmove(maxline,1)
      write (*,*) message(1:78)
      return

      end







