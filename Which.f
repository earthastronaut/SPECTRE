      subroutine which (xyz)
c*****this routine returns the name of the desired array, from scrren input
 
      include 'Chars.com'
      character xyz*1

      message = 'WHICH ARRAY (x, y, or z)? '
      nchars = 26
      call getasci (nchars)
      xyz = array(1:1)
      return

      end


      
