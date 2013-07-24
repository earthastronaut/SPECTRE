      subroutine cont (ierror)
c*****this routine collects the cursor marks to generate a continuum.
c     it sets the spline coefficients into block /s/


      include 'Chars.com'
      include 'Mathval.com'
      include 'Plotval.com'
      include 'Plotchr.com'
      logical iflag

c*****put the cursor instruction message on the plot screen
      call sm_graphics
      call sm_ctype (colors(2))
      call sm_expand (0.8)
      call sm_relocate (xleft+0.5*(right-xleft),up-0.08*(up-down))
      if (ierror .eq. 3) then
         write (errmess,1001)
1001     format ('USE THE MOUSE TO MARK 3 POINTS ONLY')
         call sm_putlabel (5,errmess)
      else
         write (errmess,1002)
1002     format ('USE MOUSE TO MARK POINTS;',
     .           ' TYPE q ON THIS SCREEN TO QUIT')
         call sm_putlabel (5,errmess)
      endif
       call sm_expand (1.2)
       call sm_ctype (colors(1))

c*****here the number of points is forced to be 3, for the fit
c     desired by the FTS routine
      if (ierror .eq. 3)  then
          do 8 i = 1,3
             call sm_curs (spx(i),spy(i),ichr)
             call sm_gflush
             call sm_alpha
8            call plus (spx(i),spy(i))
          i = 3
          go to 20
      endif

c*****set the spectrum continuum points with the cursor; 
c     the "q" key means quit; 
c     the "-" key means erase/ignore the last point
      
      do 9 i=1,25
9         df(i) = 1.0
      iflag = .false.
      sm = 0.
c      i = 1   c this will allow you to keep going
      i = nknots+1
10    if (i .gt. 25) go to 20
5      call sm_curs (spx(i),spy(i),ichr)
       if (ichr .eq. 0) call sm_curs (spx(i),spy(i),ichr)
       call sm_gflush
       call sm_alpha
      if (char(ichr) .eq. 'q') then
         i = i - 1
         go to 20
      elseif (char(ichr) .eq. '-') then
         if (i .eq. 1) go to 10
         i = i - 1
c        there should be a way to declare color in plus and then just call it here
         call sm_angle (45.)
         call sm_expand (2.5)
         call sm_ptype (41.0,1)
         call sm_ctype (colors(8))
         call sm_points (spx(i),spy(i),1)
         call sm_angle (0.)
         call sm_expand (1.2)
         call sm_ctype (colors(1))
         go to 5
      else
         if (spx(i) .lt. sm) iflag = .true.
         sm = spx(i)
         call plus (spx(i),spy(i))
         i = i + 1
         go to 10
      endif

 
c*****fit a parabola for 3 points, else fit a cubic spline
20    nknots = i
      if (iflag) call shuffle
      if (nknots .lt. 2) then
         ierror = 5
      elseif (nknots .eq. 3) then
         ierror = 0
      else
         ierror = 10
      endif
      return

      end





