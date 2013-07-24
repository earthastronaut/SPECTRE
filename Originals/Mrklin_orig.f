      subroutine mrklin (wave)
c*****this routine marks the screen position of a line and draws the current
c     continuum position.
      include 'Chars.com' 
      include 'Dataval.com'
      include 'Plotval.com'
      real*8 wave
 
      call estim (sngl(wave),sngl(wave),dispx,wlx,npx,ipt,ipt)
      call sm_ctype (colors(2))
      ylength = 0.13*(up-down)
      call sm_relocate (sngl(wave),x(ipt)-ylength)
      call sm_draw (sngl(wave),x(ipt))    
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






