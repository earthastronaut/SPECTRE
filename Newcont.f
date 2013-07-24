      subroutine newcont
c*****this routine adjusts the continuum level to the user's specification.
 
      include 'Dataval.com'
      include 'Chars.com'
 
      message = 'PUT CURSOR AT THE NEW CONTINUUM '
      nchars = 32
      call putasci (nchars)
c      call mongohairs (ichr,xval,ycont)
      call sm_curs (xval,ycont,ichr)
      call sm_gflush
      call sm_alpha
      ycont = ycont
      call setre (1,ycont)
      do 98 i=1,npx
98    x(i) = x(i)/ycont
      return

      end
      

