      subroutine mrklin (wave,wavout)
c*****this routine marks the screen position of a line and draws the current
c     continuum position.
c    
c     Edited: Dylan Gregersen
c     added section for line information from the solar atlas
c     version: 2.3.2
c


      include 'Chars.com' 
      include 'Dataval.com'
      include 'Datachr.com'
      include 'Plotval.com'
      real*8 wavout,wave

      call estim (sngl(wavout),sngl(wavout),dispx,wlx,npx,ipt,ipt,xfile)
c      call estim (sngl(wave),sngl(wave),dispx,wlx,npx,ipt,ipt)
c*****mark position of line
      call sm_ctype (colors(4))
      ylength = 0.13*(up-down)
      call sm_relocate (sngl(wave),x(ipt)-ylength)
      call sm_draw (sngl(wave),x(ipt))  



c*************************  ADDED. this is where it can go and look for solar lines.
      if (oplotlines .gt. 0.0) then
         call plotlines()
      endif
c****************************************************************


c*****below is the original code  
c      call estim (sngl(wavout),sngl(wavout),dispx,wlx,npx,ipt,ipt)
 2    call sm_ctype (colors(2))
      ylength = 0.13*(up-down)
      call sm_relocate (sngl(wavout),x(ipt)-ylength)
      call sm_draw (sngl(wavout),x(ipt))    
      call sm_ctype (colors(7))
      call sm_ltype (2)
      call sm_relocate (xleft,1.0)
      call sm_draw (right,1.0)
      call sm_angle (0.)
      call sm_ltype (0)
      call sm_expand (1.2)
      call sm_ctype (colors(1))

      return

      end 






