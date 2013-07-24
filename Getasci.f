      subroutine getasci (num)
c*****this routine asks for information, by printing out the
c     characters contained in 'message', and returns what the
c     user types out in array 'array'.  Variable 'num' on input
c     is the number of characters in 'message', and on output is
c     the number of characters in 'array'

      include 'Chars.com'
      include 'Scrnval.com'

      do 1 i=1,2
         istat = ivmove(maxline-2+i,1)
1        istat = ivcleol()
      istat = ivmove(maxline,1)
      if (num .lt. 10) then
         write (errmess,1001) num
1001     format ('(a',i1,'$)')
      else
         write (errmess,1002) num
1002     format ('(a',i2,'$)')
      endif
      write (*,errmess) message
      num = 80 - num
      if (num .lt. 10) then
         write (errmess,1003) num
1003     format ('(a',i1,')')
      else
         write (errmess,1004) num
1004     format ('(a',i2,')')
      endif
      read (*,errmess) array
      do 10 i=num,1,-1
         if (array(i:i) .ne. ' ') go to 11
10       continue
      num = -1
      return
11    num = i
      return

      end







