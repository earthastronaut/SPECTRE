      subroutine gauss (ipt,centint,halfl,halfr,depth,eqwdth)
c*****this routine performs a gaussian integration to get an equivalent
c     width of the line
 
      include 'Dataval.com'
      include 'Plotval.com'
      include 'Chars.com'
 
      idelta = 48
      depth = 1. - centint
      call sm_ctype (colors(2))
      call sm_expand (0.9)  
      write (errmess,1001) depth
1001  format ('DEPTH = ',f5.3,5x)
      call sm_relocate (right-0.25*(right-xleft),down+0.30*(up-down))
      call sm_putlabel (6,errmess)

      hafint = 1. - 0.5*depth
      halfl = 0.
      iflag = 0
      do 20 i=1,idelta
         jpt = ipt - i
         if (x(jpt-1) .le. x(jpt)) go to 12
         if (x(jpt) .ge. hafint) go to 14
         go to 20
12       if (iflag .eq. 1) go to 16
         iflag = 1
         go to 20
14       halfl = (hafint-x(jpt+1))/(x(jpt)-x(jpt+1))*
     .           (wlx(jpt+1)-wlx(jpt)) + wlx(ipt) - wlx(jpt+1)
         go to 16
20       continue
16    write (errmess,1002) halfl
1002  format ('H.W.(l) = ',f5.3,3x)
      call sm_relocate (right-0.25*(right-xleft),down+0.23*(up-down))
      call sm_putlabel (6,errmess)

      halfr = 0.
      iflag = 0
      do 30 i=1,idelta
         jpt = ipt + i
         if (x(jpt+1) .le. x(jpt)) go to 22
         if (x(jpt) .ge. hafint) go to 24
         go to 30
22       if (iflag .eq. 1) go to 26
         iflag = 1
         go to 30
24       halfr = (hafint-x(jpt-1))/(x(jpt)-x(jpt-1))*
     .           (wlx(jpt)-wlx(jpt-1)) + wlx(jpt-1) - wlx(ipt)
         go to 26
30       continue
26    write (errmess,1003) halfr
1003  format ('H.W.(r) = ',f5.3,3x)
      call sm_relocate (right-0.25*(right-xleft),down+0.16*(up-down))
      call sm_putlabel (6,errmess)

      if (halfl .eq. 0.) go to 31
      if (halfr .eq. 0.) go to 32
      eqwdth = 1.06447*depth*(halfl+halfr)
      go to 33
31    eqwdth = 2.12894*depth*halfr
      halfl = halfr
      go to 33
32    eqwdth = 2.12894*depth*halfl
      halfr = halfl
33    wid = 1000.*eqwdth
      write (errmess,1004) wid
1004  format ('E.W. = ',f7.1,' mA',3x)
      call sm_relocate (right-0.25*(right-xleft),down+0.09*(up-down))
      call sm_putlabel (6,errmess)
      call sm_ctype (colors(1))
      call sm_expand (1.2)
  
      call sm_gflush
      call sm_alpha

      if (eqwdth .eq. 0.) return
      call sm_ctype (colors(1))
      call sm_relocate (wlx(ipt),x(ipt))
      call sm_draw (wlx(ipt),1.0)
      call gfill (wlx(ipt),depth,halfl,'left')
      call gfill (wlx(ipt),depth,halfr,'rite')
      return

      end





