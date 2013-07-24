      subroutine pick (mode)
c*****this routine picks out data points on the scrren, using
c     cursor input from the user.  Finds the nearest data point.
c     if mode = 2, pick will continue until the user quits via 'q"
 
      include 'Dataval.com'
      include 'Chars.com'
      include 'Plotval.com'

      if (mode .eq. 2) then
         write (errmess,1001)
1001     format ('USE MOUSE TO MARK POINTS; TYPE q ON THIS ',
     .           'SCREEN TO QUIT')
         call sm_ctype (colors(2))
         call sm_expand (0.7)
         call sm_relocate (xleft+0.5*(right-xleft),up-0.08*(up-down))
         call sm_putlabel (5,errmess)
         call sm_ctype (colors(1))
         call sm_expand (1.2)
      endif

1     call sm_curs (xusr,yusr,ichr)
      if (char(ichr) .eq. 'q') return
      call sm_gflush
      call sm_alpha     
      call estim (xusr,xusr,dispx,wlx,npx,ipt,ipt)
      if (wlx(ipt) .gt. xusr) ipt = max0(ipt-1,1)
      if (abs(xusr-wlx(ipt)) .gt. abs(wlx(ipt+1)-xusr))
     .    ipt = min0(ipt+1,npx)
      xusr = wlx(ipt)
      yusr = x(ipt)
      call hit (xusr,real(ipt),yusr)
      if (mode .eq. 1) return
      go to 1

      end



