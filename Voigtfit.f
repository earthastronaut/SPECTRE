
      subroutine voigtfit (mode,wavout,ipt,depth,halfl,halfr,eqwdth)
c*****this routine interactively fits Voigt functions to lines

      include 'Dataval.com'
      include 'Plotval.com'
      include 'Mathval.com'
      include 'Chars.com'
      real*8 wavout,deltaw,v,a,voigtcal,oldhwhm,hwhm

c     initialize the scratch arrays and some variables
      do 1 i=1,10000
         ss(i) = 0.
1        t(i) = 0.
      oldhwhm = (halfl + halfr)/2.
      hwhm = oldhwhm
      go to 50

c     decide what to do next
5     message = 'IS THE LINE OK ([y],n,h)? '
      nchars = 26
      call getasci (nchars)
      if (array(1:1).eq.'y' .or. nchars.le.0) then 
         call sm_ctype (colors(1))
         call sm_expand (1.2)
         call sm_gflush
         call sm_alpha
         halfl = hwhm
         halfr = hwhm
         if (mode .eq. 1) then
            do 75 i=ileft,iright
               wly(i) = t(i)
75             y(i) = ss(i)
         endif
         return
      elseif (array(1:1) .eq. 'h') then
         go to 40 
      elseif (array(1:1) .eq. 'n') then
         go to 50
      else
         go to 5
      endif

c     redraw the original Gaussian fit to the line
50    call setplt (ipt,wavout,centint,ileft,iright,0)
      npts = iright - ileft + 1
      call plotxy (1,1,wlx,x,npx,10)
      a = 0.
      v = 0.
      depthnorm = depth/voigtcal(a,v)
      do i=ileft,iright
         deltaw = wlx(i) - wavout
         v = dabs(0.832555*deltaw/oldhwhm)
         ss(i) =  1. - depthnorm*voigtcal(a,v)
         t(i) = wlx(i)
      enddo
      call plotxy (1,-1,t(ileft),ss(ileft),npts,-4)
      call sm_ctype (colors(2))
      call sm_expand (0.9)
      write (errmess,1001) depth
1001  format ('DEPTH = ',f5.3)
      call sm_relocate (right-0.35*(right-xleft),down+0.30*(up-down))
      call sm_putlabel (6,errmess)
      write (errmess,1002) oldhwhm
1002  format ('HWHM(old) = ',f5.3)
      call sm_relocate (right-0.35*(right-xleft),down+0.23*(up-down))
      call sm_putlabel (6,errmess)
      eqwdth = 1. - x(ileft)
      do i=ileft+1,iright-2,2
         eqwdth = eqwdth + 4.*(1. - ss(i)) + 2.*(1. - ss(i+1)) 
      enddo
      eqwdth = eqwdth + 4.*(1. - ss(iright-1)) + (1. - ss(iright)) 
      disp = (t(iright) - t(ileft))/(npts - 1)
      eqwdth = eqwdth/3.*disp
      wid = 1000.*eqwdth
      write (errmess,1004) wid
1004  format ('E.W.(old) = ',f7.1,' mA')
      call sm_relocate (right-0.35*(right-xleft),down+0.16*(up-down))
      call sm_putlabel (6,errmess)
      call sm_gflush
      call sm_alpha
   
c     a different value of a is desired
30    message = 'DESIRED DAMPING CONSTANT a = '
      nchars = 29
      call getnum (nchars,a)
      if (a .eq. -9999.) go to 30
      v = 0.
      depthnorm = depth/voigtcal(a,v)
35    do 20 i=ileft,iright
         deltaw = wlx(i) - wavout
         v = dabs(0.832555*deltaw/hwhm)
         ss(i) =  1. - depthnorm*voigtcal(a,v)
         t(i) = wlx(i)
20    continue
      call plotxy (1,-1,t(ileft),ss(ileft),npts,-3)
      call sm_ctype (colors(2))
      call sm_expand (0.9)
      write (errmess,1007) a
1007  format ('a = ',f5.3)
      call sm_relocate (xleft+0.12*(right-xleft),down+0.30*(up-down))
      call sm_putlabel (6,errmess)
      write (errmess,1008) hwhm
1008  format ('HWHM(new) = ',f5.3)
      call sm_relocate (xleft+0.12*(right-xleft),down+0.23*(up-down))
      call sm_putlabel (6,errmess)
      eqwdth = 1. - x(ileft)
      do 25 i=ileft+1,iright-2,2
25       eqwdth = eqwdth + 4.*(1. - ss(i)) + 2.*(1. - ss(i+1)) 
      eqwdth = eqwdth + 4.*(1. - ss(iright-1)) + (1. - ss(iright)) 
      eqwdth = eqwdth/3.*disp
      wid = 1000.*eqwdth
      write (errmess,1005) wid
1005  format ('E.W.(new) = ',f7.1,' mA')
      call sm_relocate (xleft+0.12*(right-xleft),down+0.16*(up-down))
      call sm_putlabel (6,errmess)
c      call tidle
      call sm_gflush
      call sm_alpha
      go to 5

40    message = 'DESIRED HWHF = '
      nchars = 15
      call getnum (nchars,hwhm)
      if (hwhm .eq. -9999.) go to 40
      go to 50


      end









