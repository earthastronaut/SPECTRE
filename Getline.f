      subroutine getline (emflag,ichr,izoom,channel,wave)
c*****this routine is used to mark lines on the screen and name wavelengths
 
      include 'Chars.com'
      include 'Dataval.com'
      include 'Datachr.com'


      include 'Plotval.com'
      real*8 channel,wave
      integer emflag
 
c*****first mark the desired line
25    write (errmess,1001)
1001  format ('USE MOUSE TO MARK POINTS; TYPE q ON THIS SCREEN TO QUIT')
      call sm_ctype (colors(2))
      call sm_expand (0.7)
      call sm_relocate (xleft+0.5*(right-xleft),up-0.08*(up-down))
      call sm_putlabel (5,errmess)
      call sm_ctype (colors(1))
      call sm_expand (1.2)
      call sm_curs (xline,yline,ichr)
      if (ichr .eq. 0) call sm_curs (xline,yline,ichr)
      call sm_gflush
      call sm_alpha
      if (char(ichr) .eq. 'q') return

c*****zoom in to see more detail, if desired
      if (char(ichr) .eq. 'z') then
         call zoom ('in ')
         izoom = izoom + 1
         go to 25
      endif
 
c*****use a spline interpolation to find the min or max of the marked line
30    call estim (xline,xline,dispx,wlx,npx,ipt,ipt,xfile) 
      call findmin (real(ipt),x,npx,pmin,qmin,emflag)
      channel = dble(pmin)
      message = 'WAVELENGTH = '
      nchars = 13
      call getnum (nchars,wave)
      if (izoom .eq. 1) then
         do 10 i=1,izoom
10          call zoom ('out')
      endif
      return

      end









