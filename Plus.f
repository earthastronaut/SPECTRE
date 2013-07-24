      subroutine plus (xusr,yusr)
c*****this routine puts a plus sign (in red) at the named xusr,yusr
c     location on the screen

      include 'Chars.com'
c      integer color

      call sm_expand (2.5)
      call sm_angle (45.)
      call sm_ctype (colors(2))
c      if (color .eq. 2) call sm_ctype (colors(2))
c      if (color .eq. 5) call sm_ctype (colors(5))
c      if (color .eq. 8) call sm_ctype (colors(8))
      call sm_ptype (41.0,1)
      call sm_points(xusr,yusr,1)
      call sm_angle (0.)
      call sm_gflush
      call sm_alpha
 
      return
        
      end






