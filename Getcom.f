      subroutine getcom
c*****this routine returns a 2-letter user command
    
      include 'Chars.com'
      include 'Scrnval.com'
     
      istat = ivmove (maxline-2,1)
      istat = ivcleol()
      istat = ivmove (maxline,1)
      istat = ivcleol()
      istat = ivmove (maxline-2,1)
      write (*,1001) prompt
1001  format (a10,$)
      read (*,1002) commandInput
1002  format (a20)
      command = commandInput(1:2)
      return
      end



