      subroutine fourpn (mode,ltype,wl,pts,npt)
c*****this routine does 4 or 8 point normalization for Reticon data
c     On entry:    MODE = 1    -> Four point noise removal
c     On entry:    MODE = 2    -> Eight point noise removal
c     On entry:    MODE = 3    -> Four-by-Four point noise removal
c
c     On exit:     MODE = 1, 2, or 3, for arrays x, y, or z respectively
c
c Edited by Dylan Gregersen 1/6/12
c to fix some warnings
c
 
      include 'Plotval.com'
      include 'Chars.com'
      real*4 wl(10000),pts(10000)
      integer  ndark(4), mask(2), ltype, ltypeB
      double precision xnum 
      logical nogo
      data ndark,mask/1,40,40,1,110,90/

      nogo=.false.
 
c*****ask if the current mask and dark values are OK
      write (message,1001) ndark(2),ndark(3),mask
1001  format ('CURRENT NDARK AND MASK ARE ',2i3,2x,2i4,
     .        '; OK ([y]/n)? ')
      nchars = 57
      call getasci (nchars)
      if (array(1:1).eq.'y' .or. nchars.le.0) go to 3

c*****display the array beginning to set the mask and dark for it
      xleft = wl(1)
      right = wl(150)
      down = -50.
      ltypeB = -10.*ltype
      call plotxy (1,1,wl,pts,npt,ltype)
      call plotxy (1,-1,wl,pts,npt,ltypeB)
      message = 'GIVE THE NUMBER ON THE LEFT TO BE USED FOR DARK: '
      nchars = 48
      call getnum (nchars,xnum)
      ndark(2) = int(sngl(xnum))
      message = 'GIVE THE NUMBER FROM THE LEFT TO RETAIN: '
      nchars = 41
      call getnum (nchars,xnum)
      mask(1) = int(sngl(xnum))

c*****display the array end to set the mask and dark for it
      xleft = wl(npt-149)
      right = wl(npt)
      ltypeB = -10.*ltype
      call plotxy (1,1,wl,pts,npt,ltype)
      call plotxy (1,-1,wl,pts,npt,ltypeB)
      message = 'GIVE THE NUMBER ON THE RIGHT TO BE USED FOR DARK: '
      nchars = 49
      call getnum (nchars,xnum)
      ndark(3) = int(sngl(xnum))
      message = 'GIVE THE NUMBER FROM THE RIGHT TO RETAIN: '
      nchars = 42
      call getnum (nchars,xnum)
      mask(2) = int(sngl(xnum))

c*****if the dark or mask values are out of bounds, abort
      do 10 i=1,4
         if (ndark(i).ge.0 .and. ndark(i).le.npt) go to 10
         nogo = .true.
         errmess = 'ERROR: NDARK VALUES ARE OUT OF BOUNDS '
         nchars = 38
         call puterr (nchars)
         return
   10    continue
      do 11 i=1,2
         if (mask(i) .ge. 0) go to 11
         nogo = .true.
         errmess = 'ERROR: MASK VALUES ARE OUT OF BOUNDS '
         nchars = 37
         call puterr (nchars)
         return
   11    continue

c*****write the first information to the information screen
3     write (message,2001)
2001  format (23(' '),'RESULTS OF RETICON NORMALIZATION',24(' '))
      write (array,2002) ndark
2002  format ('THE CHANNEL LIMITS FOR THE DARK COUNTS ARE: ',4i4)
      call prinfo (1)
      write (array,2003) mask
2003  format ('THE NUMBER OF CHANNELS WHICH ARE MASKED ARE: ',2i4)
      call prinfo (2)

c*****remove the dark counts and remask the data
      call dark (pts,npt,ndark,mask)

c*****now do the n-point normalizations
      if (mode .eq. 1) then
         call fourpt (1,npt,pts)
      elseif (mode .eq. 2) then
         call eight (pts,npt)
      elseif (mode .eq. 3) then
         call fourpt (4,npt,pts)
      endif

c*****plot the result and return
      call minimax (pts,pmin,pmax,npt)
      up = 1.12*pmax
      down = 0.
      xleft = wl(1)
      right = wl(npt)
      call plotxy (1,1,wl,pts,npt,ltype)
      return

      end


      

