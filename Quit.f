      subroutine quit
c*****quit SPECTRE
 
      include 'Chars.com'

      write (errmess,1004)
1004  format ('clear',75(' '))
      call system (errmess)
      write (array,1001)
1001  format ('SPECTRE HAS ENDED! THANK YOU AND HAVE A GREAT DAY!')
      istat = ivwrite(1,1,array,18)

c*****exit without the error messages
c     ieeer = ieee_flags ("clear", "exception", "all", out)
      stop

      end




