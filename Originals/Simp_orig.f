      subroutine simp (ipt,centint,halfl,halfr,depth,eqwdth)
c*****this routine performs a simpson's rule integration to get an equivalent
c     width of the line
 
      include 'Dataval.com'
      include 'Plotval.com'
      include 'Chars.com'
 
c*****first set the limits of integration
      message = 'MARK THE LEFT INTEGRATION LIMIT ' 
      nchars = 32
      call putasci (nchars)
c      call mongohairs (ichr,xline,yline)
      call sm_curs (xline,yline,ichr)
      call sm_gflush
      call sm_alpha
      call estim (xline,xline,dispx,wlx,npx,ileft,ileft)
      ileft = ileft + 1
      message = 'MARK THE RIGHT INTEGRATION LIMIT '
      nchars = 33
      call putasci (nchars)
c      call mongohairs (ichr,xline,yline)
      call sm_curs (xline,yline,ichr)
      call sm_gflush
      call sm_alpha 
      call estim (xline,xline,dispx,wlx,npx,iright,iright)
 
c*****next, set up, and calculate the central depth
      npt = iright - ileft + 1
      if (mod(jpt,2) .ne. 1) jpt = jpt - 1
      idelta = 48
      depth = 1. - centint
c      call setcolor (2)
       call sm_ctype (colors(2))
c      call setexpand (0.9)
       call sm_expand (0.9)
      write (errmess,1001) depth
1001  format ('DEPTH = ',f5.3,5x)
      call sm_relocate (right-0.40*(right-xleft),down+0.30*(up-down))
      call sm_putlabel (6,errmess)
      hafint = 1. - 0.5*depth
 
c*****calculate the two half-widths
      halfl = 0.
      do 20 i=1,idelta
         jpt = ipt - i
         if(x(jpt) .lt. hafint) go to 20
         halfl = (hafint-x(jpt+1))/(x(jpt)-x(jpt+1))*
     .           (wlx(jpt+1)-wlx(jpt)) + wlx(ipt) - wlx(jpt+1)
         go to 16
20       continue
16    write (errmess,1002) halfl
1002  format ('H.W.(l) = ',f5.3,3x)
      call sm_relocate (right-0.40*(right-xleft),down+0.23*(up-down))
      call sm_putlabel (6,errmess)
      halfr = 0.
      do 30 i=1,idelta
         jpt = ipt + i
         if(x(jpt) .lt. hafint) go to 30
         halfr = (hafint-x(jpt-1))/(x(jpt)-x(jpt-1))*
     .           (wlx(jpt)-wlx(jpt-1)) + wlx(jpt-1) - wlx(ipt)
         go to 26
30       continue
26    write (errmess,1003) halfr
1003  format ('H.W.(r) = ',f5.3,3x)
      call sm_relocate (right-0.40*(right-xleft),down+0.16*(up-down))
      call sm_putlabel (6,errmess)
 
c*****finally, do a Simpson's Rule integration
      eqwdth = 1. - x(ileft)
      do 50 i=ileft+1,iright-2,2
50       eqwdth = eqwdth + 4.*(1. - x(i)) + 2.*(1. - x(i+1)) 
      eqwdth = eqwdth + 4.*(1. - x(iright-1)) + (1. - x(iright)) 
      disp = (wlx(iright) - wlx(ileft))/(npt - 1)
      eqwdth = eqwdth/3.*disp
      wid = 1000.*eqwdth
      write (errmess,1004) wid
1004  format ('E.W. = ',f7.1,' mA',3x)
      call sm_relocate (right-0.40*(right-xleft),down+0.09*(up-down))
      call sm_putlabel (6,errmess)
c      call setcolor (1)
      call sm_ctype (colors(1))
c      call setexpand (1.2)
      call sm_expand (1.2)

      call pfill (ileft,iright)
c      call tidle
      call sm_gflush
      call sm_alpha
      return

      end





