      subroutine manual (mode,ipt,wave,wavout,depth,halfl,halfr,eqwdth)
c*****this routine computes the appropriate Gaussian from line information
c     entered from the keyboard.
 
      include 'Dataval.com'
      include 'Datachr.com'
      include 'Plotval.com'
      include 'Chars.com'
      real*8 wavout,wave
 
      call setplt (ipt,wavout,centint,ileft,iright,0)
      npts = iright - ileft + 1
      if (mode .ne. 0) call plotxy (1,1,wlx,x,npx,1) 
      call newcont
      call plotxy (1,1,wlx,x,npx,1)

      message = 'PUT CURSOR AT DESIRED LINE BOTTOM '
      nchars = 34
      call putasci (nchars)
      call sm_curs (xline,yline,ichr)
      call sm_gflush
      call sm_alpha
      call plus (xline,yline)
      depth = 1. - yline
      wavout = dble(xline)
      kount = 0
      xcheck = 0

      message = 'MARK LEFT PROFILE, NEAR THE HALF POWER POINT ' 
      nchars = 45
99    call putasci (nchars)
      call sm_curs (xval,yval,ichr)
      call sm_gflush
      call sm_alpha
      call plus (xval,yval)
      if (abs(xline-xval)/(right-xleft) .lt. 0.02) then
         if (kount .eq. 0) then
            halfl = 0.
            go to 101
         else
            halfr = 0.
            go to 101
         endif
      endif
      if (xval .lt. xline) then
         widel = xline - xval
         depl = 1. - yval
         halfl = sqrt(-0.69315*widel**2/alog(depl/depth))
         eql = depth*widel*sqrt(3.14159/alog(depth/depl))
         go to 101
      else
         wider = xval - xline
         depr = 1. - yval
         halfr = sqrt(-0.69315*wider**2/alog(depr/depth))
         eqr = depth*wider*sqrt(3.14159/alog(depth/depr))
      endif

101   if (kount .gt. 0) go to 96
      kount = 1
      message = 'MARK RIGHT PROFILE, NEAR THE HALF POWER POINT '
      nchars = 46
      call putasci (nchars)
      go to 99

96    if (halfl .eq. 0.) then
         halfl = halfr
         eql = eqr
      else if (halfr .eq. 0.) then
         halfr = halfl
         eqr = eql
      endif
      eqwdth = (eql + eqr)/2.
      wid = 1000.*eqwdth

      call estim (xline,xline,dispx,wlx,npx,ipt,ipt,xfile)

      call setplt (ipt,wavout,centint,ileft,iright,0)
      wavout = dble(xline)
      call plotxy (1,1,wlx,x,npx,1)
c      call mrklin (wavout)
      call mrklin (wave,wavout)

      call sm_ctype (colors(2))
      call sm_expand (0.9)
      write (errmess,1001) depth
1001  format ('DEPTH = ',f5.3,5x)
      call sm_relocate (right-0.25*(right-xleft),down+0.30*(up-down))
      call sm_putlabel (6,errmess)
      write (errmess,1002) halfl
1002  format ('H.W.(l) = ',f5.3,3x)
      call sm_relocate (right-0.25*(right-xleft),down+0.23*(up-down))
      call sm_putlabel (6,errmess)
      write (errmess,1003) halfr
1003  format ('H.W.(r) = ',f5.3,3x)
      call sm_relocate (right-0.25*(right-xleft),down+0.16*(up-down))
      call sm_putlabel (6,errmess)
      write (errmess,1004) wid
1004  format ('E.W. = ',f7.1,' mA',3x)
      call sm_relocate (right-0.25*(right-xleft),down+0.09*(up-down))
      call sm_putlabel (6,errmess)
      write (errmess,1006) wavout
1006  format ('LINE FOUND = ',f8.3)
      call sm_relocate (xleft+0.02*(right-xleft),down+0.25*(up-down))
      call sm_putlabel (6,errmess)
      call sm_ctype (colors(4))
      write (errmess,1005) wave
c The goal of the following was to keep the "line Sought" visible through all measurement attempts
1005  format ('LINE SOUGHT = ',f8.3)
      call sm_relocate (xleft+0.02*(right-xleft),down+0.17*(up-down))
      call sm_putlabel (6,errmess)


c***** added in to extra line information
      if (mode .eq. 0) then
         call sm_expand (0.5)
         call sm_ctype (colors(1))
         write (errmess,1007) linfo
 1007    format('INFO:',A70)
         call sm_relocate (xleft+0.01*(right-xleft),down+0.10*(up-down))
         call sm_putlabel (6,errmess)
      endif
c******************************

      call sm_ctype (colors(1))
      call sm_expand (1.2)
      call sm_gflush
      call sm_alpha
      if (eqwdth .eq. 0.) return

      call sm_relocate (xline,yline)
      call sm_draw (xline,1.0)
      if (mode .eq. 0) then 
         call gfill (xline,depth,halfl,'left')
         call gfill (xline,depth,halfr,'rite')
      else
         call mkgauss (xline,depth,halfl,halfr)
      endif


      return

      end




