      subroutine crays
c*****this routine erases radiation event spikes from 
c     absorption line spectra
 
      include 'Plotval.com'
      include 'Dataval.com'
      include 'Chars.com'
 
c*****find the chopping point and the window boundaries
      message = 'MARK THE DISCRIMINATOR LEVEL '
      nchars = 29
      call putasci (nchars)
c      call mongohairs (ichr,xxx,ycutoff)
       call sm_curs (xxx,ycutoff,ichr)
       call sm_gflush
       call sm_alpha
c      call tidle
      call estim (xleft,right,dispx,wlx,npx,ileft,iright)
      npt = iright - ileft + 1
 
c*****chop any points in the window higher than ycutoff
      write (message,3002)
3002  format(22(' '),'RADIATION EVENT EXCISION INFORMATION',21(' '))
      i = ileft
      kount = 1
5      if (x(i) .ge. ycutoff) then
         nblu = i
         do 10 j=nblu,iright
            if (x(j) .lt. ycutoff) go to 15
10          continue
         nred = iright
         go to 16
15       nred = j - 1
16       if (nblu.eq.1 .or. nred.eq.npx) go to 12
         slope = (x(nred+1) -x(nblu-1))/float(nred-nblu+2)
         do 11 i=nblu,nred
            xn = i-nblu+1
11          x(i) = x(nblu-1)+xn*slope
         go to 30
12       if(nblu .eq. 1) counts = x(nred+1)
         if(nred .eq. npx) counts = x(nblu-1)
         do 13 i = nblu,nred
13          x(i) = counts
30       i = nred + 1
         write (array,1003) nblu,nred
1003     format ('POINTS ',i4,' THROUGH ',i4,' HAVE BEEN FIXED!',39x)
         call prinfo (kount)
         kount = kount + 1
      else
         i = i + 1
      endif
      if (i .le. iright) go to 5
 
c*****make a cleaned-up plot
      call plotxy (1,1,wlx(ileft),x(ileft),npt,1)
      return

      end


  

