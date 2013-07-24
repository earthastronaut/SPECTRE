      subroutine blank
c*****this routine blanks out arrays 'message' and 'array'

      include 'Chars.com'

      write (array,1001)
      write (message,1001)
1001  format (80(' '))
      return

      end





