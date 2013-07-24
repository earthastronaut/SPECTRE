      subroutine setcont (nogo)
c*****this routine sets the continuum
 
      include 'Dataval.com'
      include 'Mathval.com'
      include 'Chars.com'
      logical nogo

c*****clear the arrays
      do 1001 i=1,25
         s2(i) = 0.
         spcoy(i) = 0.
         spx(i) = 0.
1001     spy(i) = 0.
      do 1002 i=1,24
         do 1002 j=1,3
1002        spcoc(i,j) = 0.
      nknots = 0

      numit = npx
      ier = 0
      nogo = .false.

c*****define the continuum points with the cursor
c              ier is a continuum fitting error condition:
c              ier=1 is a parabola fitting error
c              ier=5 program detected error - nknots < 2
c              ier=10 fit a cubic spline
101   call cont (ier)
      if (ier .eq. 5) go to 100
      if (ier .eq. 10) go to 40
      
c*****draw the continuum with a parabola
10    call parab (ier)
      do 20 i=1,numit
         t(i) = wlx(i)
20       ss(i) = splint(t(i))
      go to 41
 
c*****or fit a cubic spline to the data
40    call splnc (nknots,0.0001)
      do 120 i=1,numit
120      t(i) = wlx(i)
      call spln (nknots,numit)

c*****plot the proposed continuum
41    call plotxy (1,-1,t,ss,npx,4)
      message = 'IS THE CONTINUUM OK ([y],n,a)? '
      nchars = 29
      call getasci (nchars)
      if (array(1:1).eq.'y' .or. nchars.le.0) nogo = .false.
c*****original has n as a which will cancel and take to out to spectre main
      if (array(1:1) .eq. 'n') goto 101
      if (array(1:1) .eq. 'a') nogo = .true.
      return
 
c*****error message - ier<>0  continuum curve wasn't fit
100   errmess = 'AT LEAST 2 POINTS ARE NEEDED FOR CONTINUUM FIT!'
      nchars = 47
      call puterr (nchars)
      nogo = .true.
      return
 
      end




