      subroutine single (mode,wave,wavout,ipt,depth,halfl,halfr,eqwdth)
c*****this routine automatically searches for a line, plots it out, and
c     commands a Gaussian fit to be made.
 
      include 'Dataval.com'
      include 'Plotval.com'
      include 'Chars.com'
      real*8 wave,wavout
 
      idelta = 6
      call hunt (wave,idelta,ipt)
      if (ipt .eq. -1) then
         errmess = 'THIS LINE POSITION IS OUT OF RANGE ! '
         nchars = 37
         call puterr (nchars)
         return
      endif

      jpt = ipt
      ipt = iabs(ipt)
      call setplt (ipt,wavout,centint,ileft,iright,0)
      npts = iright - ileft + 1
      call plotxy (1,1,wlx,x,npx,1)
      call mrklin (wavout)
      call sm_ctype (colors(2))
      call sm_expand (0.9)
      write (errmess,1005) wave
1005  format ('LINE SOUGHT = ',f8.2)
      call sm_relocate (xleft+0.05*(right-xleft),down+0.25*(up-down))
      call sm_putlabel (6,errmess)
      write (errmess,1006) wavout
1006  format ('LINE FOUND = ',f8.2)
      call sm_relocate (xleft+0.05*(right-xleft),down+0.18*(up-down))
      call sm_putlabel (6,errmess)
      if (jpt .le. 0) then
         errmess = 'WARNING: THIS MAY NOT BE AT THE LINE CENTER! '
         nchars = 45
         call puterr (nchars)
      endif
      call sm_ctype (colors(1))
      call sm_expand (1.2)
      call sm_gflush
      call sm_alpha

      if (mode .eq. 1) call gauss (ipt,centint,halfl,halfr,depth,eqwdth)
      if (mode .eq. 2) call simp (ipt,centint,helfl,halfr,depth,eqwdth)
      return

      end






