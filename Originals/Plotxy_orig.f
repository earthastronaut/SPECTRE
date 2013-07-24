
      subroutine plotxy (nquad,iquad,x,y,npt,ltype)
c*****the basic plotting routine, showing data with arbitrary x and y axes
c     INPUT:  nquad         number of quadrants (1,2,4)
c             iquad         quadrant number we're plotting in
c             x             x data array to be plotted
c             y             y data array to be plotted
c             npt           number of points in the X array
c             ltype         flag - used for line type as well as others
c             xleft         left edge of the box
c             right         right edge of the box
c             up            maximum data value displayed (usually > xmax)
c             down          minimum data value displayed
c
c     QUADRANT NUMBERS:
c     ----------------------------         ------------------------------
c     |                          |         |              |             | 
c     |            2             |         |      3       |      4      | 
c     |                          |         |              |             | 
c     ----------------------------         ------------------------------
c     |                          |         |              |             | 
c     |            1             |         |      1       |      2      | 
c     |                          |         |              |             | 
c     ----------------------------         ------------------------------

      include 'Chars.com'
      include 'Plotval.com'
      include 'Plotchr.com'
      include 'Scrnval.com'
      real*4 smlxtic,bigxtic,smlytic,bigytic
      real*4 x(7000),y(7000),style(1)
      logical markers,hardcop
      real*4 cols(3)
      integer mycolor


c*****check to be sure xleft and right are set
      if (xleft .eq. right) then
         errmess = 'WARNING: LEFT AND RIGHT ARE NOT SET'
         nchars = 35
         call puterr (nchars)
         return
      endif

c*****decide whether markers or lines are desired
      mtype = ltype
      jquad = iquad
      mquad = nquad
      markers = .false.
      if (iabs(mtype) .ge. 10) then
      	   markers = .true.
      	   mtype = mtype/10
      endif

c*****decide whether a hardcopy is desired
      if (mquad .ge. 10) then
         hardcop = .true.
         mquad = mquad/10
      else
         hardcop = .false.
      endif

c*****open the sm_window to the desired area
      call sm_graphics
      if (jquad .gt. 0) call sm_erase
      jquad = iabs(jquad)
      if     (mquad .eq. 1) then
           nx = 1
           ny = 1
           ix = 1
           iy = jquad
      elseif (mquad .eq. 2) then
           nx = 1 
           ny = 2 
           ix = 1 
           iy = jquad 
      elseif (mquad .eq. 4 .and. jquad .lt. 2) then
           nx = 2 
           ny = 2 
           ix = 1 
           iy = jquad 
      else
           nx = 2 
           ny = 2 
           ix = 1 
           iy = jquad - 2
      endif
      call sm_window (nx,ny,ix,iy,ix,iy)

c*****draw and label the box
      call sm_expand (1.2)
      call sm_lweight (1.4)
      if (hardcop) then
         call sm_ctype (colors(8))
      else
         call sm_ctype (colors(1))
      endif
      call sm_limits (xleft,right,down,up)
      if (mtype .gt. 0) then
         call findtic (xleft,right,bigxtic,smlxtic)
         call findtic (down,up,bigytic,smlytic)
         call sm_ticksize (smlxtic,bigxtic,smlytic,bigytic)
         call sm_box (0,0,0,0)
         call sm_expand (0.8)
         call sm_box (1,2,4,4)
         call sm_xlabel (plxlab)
         call sm_ylabel (plylab)
         call sm_expand (1.2)
      else
         mtype = iabs(mtype)
      endif
      if (hardcop) then
         if (mtype .eq. 1) then
             call sm_ltype (0)
         elseif (mtype .eq. 2) then
            call sm_ltype (2)
         elseif (mtype .eq. 3) then
             call sm_ltype (1)
         elseif (mtype .eq. 4) then
            call sm_ltype (3)
         endif
      else
         if (mtype .eq. 1) then
             call sm_ctype (colors(3))
         elseif (mtype .eq. 2) then
             call sm_ctype (colors(5)) 
         elseif (mtype .eq. 3) then
             call sm_ctype (colors(4))
         elseif (mtype .eq. 4) then
             call sm_ctype (colors(2)) 
         endif
      endif

c*****now do the plot
c*****plot markers if fewer than 50 points
      if(npt.le.50 .or. markers) then
         style(1) = 41.0
         call sm_ptype (style,1)
         call sm_angle (45.)
         call sm_points (x,y,npt)
         call sm_angle (0.)
      else
         call sm_conn (x,y,npt)
      endif
 
c*****flush the buffer and leave
       call sm_gflush
       call sm_alpha 
      return

      end








