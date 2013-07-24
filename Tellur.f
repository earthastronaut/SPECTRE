      subroutine tellur (linpos,nline)
c*****this routine identifies telluric features for division. First, it 
c     sets a discriminator level for recognition of real lines. Then it 
c     identifies up to 25 of the lines by finding "clean" minima of 
c     the spectrum.

      include 'Dataval.com'
      include 'Plotval.com'
      include 'Chars.com'
      double precision sum
      integer linpos(25)

 
c*****first, plot the divisor spectrum
      xleft = wly(1)
      right = wly(npy)
      down = 0
      up = 1.2
      call plotxy (1,1,wly,y,npy,2)


c*****next, set the discriminator level
      n = 0
      sum = 0
      do 10 i=1,npy
         if (y(i) .eq. 0.) go to 10
         sum = sum + y(i)
         n = n + 1
10       continue
      avg = sum/n
      call sm_ctype (colors(2))
      call sm_relocate (xleft,avg)
      call sm_draw (right,avg)


c*****find real lines below this level
      nline = 0
      max = npy - 20
      i = 21
15    if (y(i) .lt. 0.98*avg) go to 25
20    i = i + 1
      if (i .ge. max) go to 50
      go to 15
25    if (y(i) .lt. y(i+1)) go to 30
      i = i + 1
      if (i .ge. max) go to 50
      go to 25
30    do 35 j=1,5
      if (y(i).ge.y(i-j) .or. y(i).ge.y(i+j)) go to 20
35    continue
      nline = nline + 1
      linpos(nline) = i
      if (nline .eq. 25) go to 50
      i = i + 10
      go to 20
50    if (nline .eq. 0) go to 70


c*****now mark these lines on the screen
      call sm_angle (45.)
      call sm_ptype (40.0,1)
      do i=1,nline
         j = linpos(i)
         call sm_points (float(j),amax1(y(j)-0.025,0.050),1)
      enddo
      call sm_angle (0.)
      errmess = 'CHOSEN DIVISOR LINES ARE MARKED'
      call sm_relocate (xleft+0.1*(right-xleft),down+0.25*(up-down))
      call sm_label (errmess)
      go to 80


c*****an error message if no lines were available
70    errmess = 'NO CLEAN TELLURIC LINES WERE FOUND'
      call sm_relocate (xleft+0.1*(right-xleft),down+0.25*(up-down))
      call sm_label (errmess)

c*****clean up and leave
80    call sm_ctype (colors(1))
      call sm_gflush
      call sm_alpha

      end





