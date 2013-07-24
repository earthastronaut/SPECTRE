      subroutine flatcont (nogo,splitd)

c*****this routine flattens the continuum via predetermined continuum
c     wavelength intervals.

      include 'Dataval.com'
      include 'Datachr.com'

      include 'Plotval.com'
      include 'Mathval.com'
      include 'Chars.com'
      real*8 wave1, wave2, choplevel
      logical nogo,splitd
      character charstat*7
      character filehold*80

c     clear the arrays, set some parameters
      do i=1,25
         s2(i) = 0.
         spcoy(i) = 0.
         spx(i) = 0.
         spy(i) = 0.
      enddo
      do i=1,24
         do j=1,3
            spcoc(i,j) = 0.
         enddo
      enddo
      nknots = 0
      numit = npx
      ier = 0
      nogo = .false.

c     open the file with the continuum wavelength ranges 
      message = 'GIVE THE FILE NAME OF THE CONTINUUM POINTS: '
      nchars = 44
      call getasci (nchars)
      charstat = 'old    '
      filehold = array
111   call dskfil (40,iostat,array(1:nchars),charstat,'sequential',
     .            'formatted  ',0)
      if (iostat .ne. 0) return

c     read in the wavelength intervals of continuum points
      j = 1
310   read (40,*,end=320) wave1, wave2
      if (wave1 .le. wlx(1)) go to 310
      if (wave2 .ge. wlx(npx)) go to 320
      spx(j) = (wave1+wave2)/2.
      call estim (sngl(wave1),sngl(wave2),dispx,wlx,npx,ipt,jpt,xfile)
      npt = jpt - ipt + 1

c     use highest points in the wavelength intervals to set continuum level
      if (npt .eq. 1) then
         spy(j) = x(ipt)
            pause
      elseif (npt .eq. 2) then
         spy(j) = (x(ipt)+x(jpt))/2.
            pause
      else
         lpt = max0(int(0.30*npt),2)
         do i=ipt,jpt
            ss(i) = x(i)
         enddo
         call sort (npt,ss(ipt))
         spy(j) = 0.
         istart = jpt - lpt + 1
         iend = jpt
         do i=istart,iend
            spy(j) = spy(j) + ss(i)
         enddo
         spy(j) = spy(j)/lpt
      endif
      j = j + 1
      go to 310

c     set up the plot
320   if (xleft.ne.wlx(1) .or. right.ne.wlx(npx)
     .    .or. splitd) then
         xleft = wlx(1)
         right = wlx(npx)
      endif
      call minimax (x,xmin,xmax,npx)
      up = 1.12*xmax
      down = 0.8*xmin
      call plotxy (1,1,wlx,x,npx,1)

c     draw in the continuum points, and fit a smooth curve through them
      nknots = j - 1
      if (nknots .lt. 2) then
         errmess = 'AT LEAST 2 POINTS ARE NEEDED FOR CONTINUUM FIT!'
         nchars = 47
         call puterr (nchars)
         return
      else
         do j=1,nknots
            call plus (spx(j),spy(j))
         enddo
         call shuffle
         if (nknots .eq. 3) then
            call parab (ier)
            do i=1,npx
               t(i) = wlx(i)
               ss(i) = splint(t(i))
            enddo
         else
            call splnc (nknots,0.0001)
            do i=1,npx
               t(i) = wlx(i)
            enddo
            call spln (nknots,npx)
         endif
      endif

c     plot the proposed continuum
      call plotxy (1,-1,t,ss,npx,4)
      close (40)
      message = 'IS THE CONTINUUM OK ([y]/i/c/s)? '
      nchars = 33
      call getasci (nchars)

c     not happy?  then stop the procedure and return without changing points
      if (array(1:1) .eq. 's') return
      do i=1,npx
         x(i) = x(i)/ss(i)
      enddo
      splitd = .false.
      xleft = wlx(1)
      right = wlx(npx)
      call minimax (x,xmin,xmax,npx)
      up = 1.12*xmax
      down = 0.
      call plotxy (1,1,wlx,x,npx,1)

c     happy with proposed continuum?  Then normalize, and exit routine
      if     (array(1:1).eq.'y' .or. nchars.le.0) then
         return

c     want to flatten with the present continuum, and try a new fit?
      elseif (array(1:1) .eq. 'i') then
         nchars = 44
         array = filehold
         go to 111

c     want to flatten, chop points about a certain relative flux level,
c     and try a new fit?
      elseif (array(1:1) .eq. 'c') then
         call plotxy (1,1,wlx,x,npx,1)
         message = 'GIVE THE FLUX DISCRIMINATOR LEVEL: '
         nchars = 35
600      call getnum (nchars,choplevel)
         if (choplevel .lt. 0.) go to 600
         i = 1
         kount = 1
5        if (x(i) .ge. choplevel) then
            nblu = i
            do j=nblu,npx
               if (x(j) .lt. choplevel) then
                  nred = j - 1
                  go to 15
               endif
            enddo
            nred = npx
15          if (nblu.eq.1 .or. nred.eq.npx) then
12             if (nblu .eq. 1) counts = x(nred+1)
               if (nred .eq. npx) counts = x(nblu-1)
               do i = nblu,nred
                  x(i) = counts
               enddo
               go to 30
            endif
            slope = (x(nred+1) -x(nblu-1))/float(nred-nblu+2)
            do i=nblu,nred
               xn = i-nblu+1
               x(i) = x(nblu-1)+xn*slope
            enddo
30          i = nred + 1
            write (array,1003) nblu,nred
1003        format ('POINTS ',i4,' THROUGH ',i4,' HAVE BEEN FIXED!',39x)
            call prinfo (kount)
            kount = kount + 1
         else
            i = i + 1
         endif
         if (i .le. npx) go to 5
         nchars = 44
         array = filehold
         go to 111
      endif


      end





