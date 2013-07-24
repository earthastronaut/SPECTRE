
      subroutine paperpl 
c*****this routine produces a paper plot from an attached on-line plotter
 
      include 'Dataval.com'
      include 'Datachr.com'
      include 'Plotval.com'
      include 'Plotchr.com'
      include 'Chars.com'
      include 'Scrnval.com'
      character title*59
      integer sm_device 

      smterm = ' '
c*****open the hardcopy sm_device
      istat = ivmove (maxline-2,1)
      if (sm_device('postland') .lt. 0) then
         write (nwrite,1008)
1008     format ('DEVICE OPENING ERROR FOR:  postland')
         stop 
      endif
      call sm_graphics
      call sm_erase

c*****give a title for the plot
      message = 'GIVE THE PLOT TITLE: '
      nchars = 21
      call getasci (nchars)
      title(1:nchars) = array(1:nchars)
      ntitle = nchars

c*****dump the data from the x-array
      istart = 1
      if (dispx(2) .eq. 0.) go to 10
      message = 'PLOT THE X-ARRAY ([l],p,b,n)? '
      nchars = 30
      call getasci (nchars)
      if (array(1:1) .eq. 'n') go to 10
      ltype = 1
      if (array(1:1).eq.'l' .or. nchars.le.0)
     .   call plotxy (10,istart,wlx,x,npx,istart*ltype)
      if (array(1:1) .eq. 'p') 
     .   call plotxy (10,istart,wlx,x,npx,10*istart*ltype)
      if (array(1:1) .eq. 'b') then
         call plotxy (10,istart,wlx,x,npx,istart*ltype)
         call plotxy (10,-1,wlx,x,npx,-10*ltype)
      endif
      call sm_ltype (0)
      call sm_expand (0.9)
      call sm_relocate (xleft+0.05*(right-xleft),down+0.15*(up-down)) 
      call sm_draw (xleft+0.15*(right-xleft),down+0.15*(up-down)) 
      call sm_relocate (xleft+0.17*(right-xleft),down+0.15*(up-down)) 
      call sm_label (xobj)
      istart = -1

c*****dump the data from the y-array
10    if (dispy(2) .eq. 0.) go to 20
      message = 'PLOT THE Y-ARRAY ([l],p,b,n)? '
      nchars = 30
      call getasci (nchars)
      if (array(1:1) .eq. 'n') go to 20
      ltype = 2
      if (array(1:1).eq.'l' .or. nchars.le.0)
     .   call plotxy (10,istart,wly,y,npy,istart*ltype)
      if (array(1:1) .eq. 'p') 
     .   call plotxy (10,istart,wly,y,npy,10*istart*ltype)
      if (array(1:1) .eq. 'b') then
         call plotxy (10,istart,wly,y,npy,istart*ltype)
         call plotxy (10,-1,wly,y,npy,-10*ltype)
      endif
      call sm_ltype (2)
      call sm_expand (0.9)
      call sm_relocate (xleft+0.05*(right-xleft),down+0.10*(up-down)) 
      call sm_draw (xleft+0.15*(right-xleft),down+0.10*(up-down)) 
      call sm_ltype (0)
      call sm_relocate (xleft+0.17*(right-xleft),down+0.10*(up-down)) 
      call sm_label (yobj)
      istart = -1

c*****dump the data from the z-array
20    if (dispz(2) .eq. 0.) go to 30
      message = 'PLOT THE Z-ARRAY ([l],p,b,n)? '
      nchars = 30
      call getasci (nchars)
      if (array(1:1) .eq. 'n') go to 30
      ltype = 3
      if (array(1:1).eq.'l' .or. nchars.le.0)
     .   call plotxy (10,istart,wlz,z,npz,istart*ltype)
      if (array(1:1) .eq. 'p') 
     .   call plotxy (10,istart,wlz,z,npz,10*istart*ltype)
      if (array(1:1) .eq. 'b') then
         call plotxy (10,istart,wlz,z,npz,istart*ltype)
         call plotxy (10,-1,wlz,z,npz,-10*ltype)
      endif
      call sm_ltype (1)
      call sm_expand (0.9)
      call sm_relocate (xleft+0.05*(right-xleft),down+0.05*(up-down)) 
      call sm_draw (xleft+0.15*(right-xleft),down+0.05*(up-down)) 
      call sm_ltype (0)
      call sm_relocate (xleft+0.17*(right-xleft),down+0.05*(up-down)) 
      call sm_label (zobj)
      istart = -1

c*****write out a title
30    if (istart .eq. 1) return
      call sm_ltype (0)
      call sm_expand (1.2)
      call sm_relocate (xleft+0.50*(right-xleft),up+0.05*(up-down)) 
      call sm_putlabel (5,title)

c*****flush the plot and return
      nchars = 27
      call putasci (nchars)
      call sm_graphics
      call sm_erase
      call sm_gflush
      call sm_alpha
      return

      end






