      subroutine remove
c*****this routine removes telluric absorption features (O2 and H2O) by
c     a combination of cross-correlation and division techniques. It 
c     allows user intervention and definition of parameters at various 
c     steps along the way.
 
      include 'Dataval.com'
      include 'Plotval.com'
      include 'Plotchr.com'
      include 'Mathval.com'
      include 'Chars.com'
      double precision xnum
      integer linpos(25),copflg
      real*8 dhold(7)
      real*4 dum1(131072)
      character zcopy*1
 
c*****this section is for user commands
      avg = 0.
      copflg = 0
      zcopy = 'n'
      mxshft = 6
      if (npz .ne. 0) then
         do 2 i=1,npz
2           dum1(i) = z(i)
         do 3 i=1,7
            dhold(i) = dispz(i)
3           dispz(i) = 0.
         nphold = npz
      endif
1     prompt = 'DIVISION: '
      call getcom
      if (command .eq. 'ab') go to 100
      if (command .eq. 'au') go to 5
      if (command .eq. 'cr') go to 5
      if (command .eq. 'ds' .or. command .eq. 'db') go to 60
      if (command .eq. 'dv') go to 90
      if (command .eq. 'fs') go to 35
      if (command .eq. 'he') go to 120
      if (command .eq. 'pe') go to 75
      if (command .eq. 'qu' .or. command(1:1) .eq. 'q') go to 100
      if (command .eq. 'rn') go to 220
      if (command .eq. 'sh') go to 35
      go to 1

c*****do the cross-correlation and plot the results
5     copflg = 0
      imax = 2*mxshft + 1
      xleft = real(-(mxshft + 1))
      right = real(mxshft + 1)
      do 10 i=1,imax
         ishift = i - 1 - mxshft 
         spx(i) = real(ishift)
10       call cross (ishift,spy(i),x,y,npx,npy)
      do 15 i=1,imax
15       spy(i) = 100.*spy(i)
      call minimax (spy,pmin,pmax,imax)
      up = int(pmax+5.)
      down = int(pmin-5.)
      call labset (2)
      call plotxy (1,1,spx,spy,imax,30)
      call labset (1)

c*****compute and plot a spline fit; offset is the point of spline maximum
      call splnc (imax,0.0001)
      jmax = 10*(imax - 1) + 1
      do 20 i=1,jmax
20       t(i) = -mxshft + real(i-1)/10.
      call spln (imax,jmax)
      ymaxi = 0.
      do 30 i=1,jmax
         if (ss(i) .lt. ymaxi) go to 30
         ymaxi = ss(i)
         xmaxi = i
30       continue
      call plotxy (1,-1,t,ss,jmax,-1)
      call sm_ctype (colors(2))
      call sm_expand (0.8)
      write (errmess,1002) ymaxi
1002  format ('MAX (R) = ',f5.1)
      call sm_relocate (xleft+0.5*(right-xleft),down+0.25*(up-down))
      call sm_putlabel (5,errmess)
      xmaxi = (xmaxi-1.)/10. - real(mxshft)
      write (errmess,1003) xmaxi
1003  format ('OFFSET = ',f6.2)
      call sm_relocate (xleft+0.5*(right-xleft),down+0.18*(up-down))
      call sm_putlabel (5,errmess)
      call sm_ctype (colors(1))
      call sm_gflush
      call sm_alpha
      message = 'IS THE PROPOSED OFFSET OK ([y],n,s,a)? '
      nchars = 39
      call getasci (nchars)
      if (array(1:1).eq.'y' .or. nchars.le.0) go to 40
      if (array(1:1) .eq. 'a') go to 100
      if (array(1:1) .eq. 's') go to 1
      if (array(1:1) .eq. 'n') go to 45
      go to 40

c*****here, the user forces a desired shift, using keyboard input
45    if (copflg .gt. 0) then
         do 46 i=1,npy
            wly(i) = wlz(i)
46          y(i) = z(i)
      endif
35    message = 'GIVE THE DESIRED POINT SHIFT: '
      nchars = 30
      call getnum (nchars,xnum)
      if (xnum .eq. -9999.) go to 35
      xmaxi = sngl(xnum)

c*****shift the y-array and display both arrays for approval
40    copflg = 1
      zcopy = 'y'
      do 224 i=1,npy
         wlz(i) = wly(i)
224      z(i) = y(i)
      npz = npy
      call yshift (xmaxi)
      call peek (2) 
      message = 'IS THE APPLIED OFFSET OK ([y]/n/s/a)? '
      nchars = 38
      call getasci (nchars)
      if (array(1:1).eq.'y' .or. nchars.le.0) go to 50
      if (array(1:1) .eq. 'a') go to 100
      if (array(1:1) .eq. 's') go to 1
      if (array(1:1) .eq. 'n') go to 45
50    if (command.eq.'cr' .or. command.eq.'sh') go to 1

c*****identify the probable telluric features in the divisor spectrum
      mode = 1
      do 57 i=1,npy
57       scratch(i) = y(i)
      copflg = 0
      call tellur (linpos,nline)
      message = 'IS THE DIVISOR SPECTRUM OK ([y]/n/s/a)? '
      nchars = 40
      call getasci (nchars)
      if (array(1:1).eq.'y' .or. nchars.le.0) go to 55
      if (array(1:1) .eq. 'a') go to 100
      if (array(1:1) .eq. 's') go to 1
      if (array(1:1) .eq. 'n') go to 1

c*****now try an automatic telluric feature division
55    if (copflg .gt. 0) then
         do 56 i=1,npx
            wlx(i) = wlz(i)
56          x(i) = z(i)
         npx = npz
      else
         do 225 i=1,npx
            wlz(i) = wlx(i)
225         z(i) = x(i)
         npz = npx
      endif
      copflg = 1
      call divide (linpos,nline,avg,mode)
      call peek (3)

c*****see whether this is acceptable to the user
      call sm_ctype (colors(2))      
      call sm_expand (0.8)
      write (errmess,1004) nline
1004  format ('# OF LINES = ',i5)
      call sm_relocate (xleft+0.1*(right-xleft),down+0.25*(up-down))
      call sm_label (errmess)
      write (errmess,1005) avg
1005  format ('RATIO = ',f7.2)
      call sm_relocate (xleft+0.1*(right-xleft),down+0.18*(up-down))
      call sm_label (errmess)
      call sm_ctype (colors(1))
      call sm_gflush
      call sm_alpha
      message = 'IS THE DIVISION OK ([y]/n/s/a)? '
      nchars = 32
      call getasci (nchars)
      if (array(1:1).eq.'y' .or. nchars.le.0) go to 128
      if (array(1:1) .eq. 'a') go to 100
      if (array(1:1) .eq. 's') go to 1 
      if (array(1:1) .eq. 'n') go to 65
128   do 127 i=1,npx
         if (x(i) .ne. 0.) go to 121
127      continue
121   if(i .eq. 1) go to 126
      do 122 j=1,i-1
122      x(j) = x(i)
126   do 123 i=1,npx
         if(x(npx+1-i) .ne. 0.) go to 124
123      continue
124   if(i .eq. 1) go to 1
      do 125 j=1,i-1
125      x(npx+1-j) = x(npx+1-i)
      go to 1

c*****here the user specifies a dividend/divisor telluric line ratio
90    copflg = 0
      do 91 i=1,npy
91       scratch(i) = y(i)
65    mode = 2
      nline = 0
66    message = 'GIVE THE DESIRED LINE RATIO: '
      nchars = 29
      call getnum (nchars,xnum)
      if (xnum .eq. -9999.) go to 66
      avg = sngl(xnum)
      go to 55

c-----display the data from one or both arrays
60    xleft = wlx(1)
      right = wlx(npx)
      up = 1.12*xmax
      down = 0.0
      call plotxy (1,1,wlx,x,npx,1)
      if (command .eq. 'db')
     .    call plotxy (1,1,wly,y,npy,-2)
      go to 1

c*****display blown-up sections of both spectra
75    call peek (2)
      go to 1

c*****reset and exit the routine
100   xleft = wlx(1)
      right = wlx(npx)
      if (nphold .ne. 0) then
         do 227 i=1,7
227         dispz(i) = dhold(i)
         do 223 i=1,nphold
            wlz(i) = wave(real(i),npz,dispz)
223         z(i) = dum1(i)
         npz = nphold
      endif
      return

c*****display the comands on the other screen
120   call printh ('divhelp')
      go to 1

c*****renormalize both arrays to 1.0 at maximum
220   xtop = 0.
      ytop = 0.
      do 200 i=1,npx
         xtop = amax1(x(i),xtop)
200      ytop = amax1(y(i),ytop)
      do 205 i=1,npx
205      x(i) = x(i)/xtop
      do 210 i=1,npy
210      y(i) = y(i)/ytop
      xmax = 1.
      go to 1

      end
      

