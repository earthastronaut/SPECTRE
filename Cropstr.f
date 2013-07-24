      subroutine cropstr (string, num)
c**** this routine asks for a string of length 80 and then finds where the 
c     first place the string is spaced

      character string*80

      do i=num,1,-1
         if (string(i:i) .ne. ' ') then
            num = i
            goto 11
         endif
      enddo
 11   return
      end
