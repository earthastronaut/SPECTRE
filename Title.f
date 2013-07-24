      subroutine title (arr)
c*****change the OBJECT keyword of one of the spectra
 
      include 'Chars.com'
      include 'Datachr.com'
      character arr*1
 
      message = 'ENTER THE NEW OBJECT NAME: '
      nchars = 27
      call getasci (nchars)
      if (nchars .lt. 20) then
         do 10 i=nchars+1,20
10          array(i:i) = ' '
      endif
      if (arr.eq.'x' .or. arr.eq.'X') then
         xobj = array(1:20)
      elseif (arr.eq.'y' .or. arr.eq.'Y') then
         yobj = array(1:20)
      elseif (arr.eq.'z' .or. arr.eq.'Z') then
         zobj = array(1:20)
      endif
      return

      end


