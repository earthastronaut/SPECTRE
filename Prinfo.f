      subroutine prinfo (kount)
c*****this routine prints out tnes of information on the lower
c     part of the information screen

      include 'Chars.com'
      include 'Scrnval.com'
      character errm1*80,array1*80
       
      if (kount .eq. 1) then
         istat = ivcleof(10,1)
         istat = ivwrite(10,1,message,79)
      endif

10    if (kount .eq. maxline-12) then
         errm1 = errmess
         array1 = array
         message = 'WANT TO SEE MORE ([y]/n)? '
         nchars = 26
         call getasci (nchars)
         if (array(1:1).eq.'y' .or. nchars.le.0) then
            istat = ivcleof(11,1)
            kount = 1
            array = array1
            errmess = errm1
         elseif (array(1:1) .eq. 'n') then
            errmess = 'stopinfo!'
            return
         else
            go to 10
         endif
      endif

      istat = ivwrite(10+kount,1,array,79)
      return
      
      end












