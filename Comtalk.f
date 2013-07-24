  
      subroutine comtalk (section)
c*****this routine generates detailed help messages about individual commands
c
c NOTE: the section must be 7 long
c
      include 'Chars.com'
      character section*7, subsection*10
 
c*****find out the command for which is desired
10    message = 'WHICH COMMAND DO YOU WANT INFORMATION ON? '
      nchars = 42
      call getasci (nchars)
      if (nchars .ne. 2) go to 10
      write (subsection,1001) array(1:2),section
1001  format (a2,' ',a7)

c*****print out the appropriate help message
      call printh (subsection)

      return

      end


