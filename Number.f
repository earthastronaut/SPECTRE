      subroutine number (nchar,xnum)
c*****this routine decodes a character string into a double precision
c     floating point number.
 
      include 'Chars.com'
      real*8 xnum
      character form*10
 
      if (nchar .le. 0) go to 101
c*****set the conversion format
20    if (nchar .lt. 10) then
         write(form,1001) nchar
1001     format('(f',i1,'.0)    ')
      else
         write(form,1002) nchar
1002     format('(f',i2,'.0)   ')
      endif
 
c*****now do the conversiton to a number
      read (unit=array,fmt=form,iostat=ierr,err=100) xnum
      return
 
c*****here an error has been detected
100   write (errmess,1004) ierr,array(1:nchar)
1004  format ('ERROR IN NUMBER INPUT: ERROR=',i4,5x,'NUMBER=',a20) 
      nchars = 65
      call puterr (nchars)
101   xnum = -9999.
      return      

      end






