      subroutine rtext (pts,wl,disp,fname,obsname,object,npt,
     .                 xmin,xmax,voverc,arrayn,filestyle)
c*****read a MONGO-style spectrum file into the program and provide as much
c     header information as possible
 
      include 'Chars.com'
      character fname*80, obsname*20, object*20, arrayn*3
      character fname1*80, obsnam1*20, object1*20, arrayn1*3
      character charstat*7, filestyle*4
      real*4 pts(131072), wl(131072)
      real*8 disp(9), disp1(9)
      real*8 pixel(25), wavelength(25)
      integer MaxNpts

      MaxNpts = 131072


c*****get ready to open a spectrum file
      fname1 = fname
      obsnam1 = obsname
      object1 = object
      arrayn1 = arrayn
      npt1 = npt
      do 22 i=1,9
         disp1(i) = disp(i)
22       disp(i) = 0.



c*****Get the file name and open the file
      call blank
      message = 'ENTER THE FILENAME: '
      nchars =  20
      call getasci (nchars)
      charstat = 'old    '
      iunit = 8
      fname = array(1:80)
      call dskfil (iunit,jostat,fname,charstat,'sequential',
     .             'formatted  ',80)
      if (jostat .ne. 0) then
         fname = fname1
         close (iunit)
         return
      endif

c*****initialize the arrays
         do 52 i=1,MaxNpts
            wl(i) = 0.
52          pts(i) = 0.
         wl(1) = -9999.

c*****get the header line, call it the object name
      read (iunit,1002) array
1002  format (a80)
      do 20 i=1,80
         if (array(i:i) .ne. ' ') go to 30
20    continue

c     blank line
      
30    k = 1
      do 35 j=i,80
         array(k:k) = array(j:j)
35       k = k + 1
      i = 80 - i
      do 40 j=i,80
40       array(j:j) = ' '
      object(1:20) = array(1:20)

c*****now read the data in (x,y) pairs, one pair per line, til the EOF
      i = 1
10    read (8,*,end=100) wl(i), pts(i)
      i = i + 1

c     added this because sometimes you get some wierd behavior if > max number 
      if (i .gt. MaxNpts) then
         write(errmess,1001) MaxNpts
 1001    format("HeadsUp: reached max number of points ",i8)
         nchars=44
         call puterr (nchars)
         go to 100
      endif
      go to 10

100   npt = i - 1
 
c*****write the first two dispersion coefficients into disp
      if     (npt .gt. 2000) then
         itot = 21
         npoly = 4
      elseif (npt .gt. 1000) then
         itot = 16
         npoly = 4
      elseif (npt .gt.  500) then
         itot = 11
         npoly = 3
      else   
         itot =  6
         npoly = 3
      endif
      interval = npt/(itot-1)
      do i=1,itot-1
         ipix = interval*(i-1) + 1
         pixel(i) = ipix
         wavelength(i) = wl(ipix)
      enddo

c*****20110511: iii commented out the fit that "rt" attempts to make
c      call solve (pixel,wavelength,itot-1,npoly)

c     disp(2) = (wl(npt) - wl(1))/npt
c     disp(1) = wl(1) - disp(2)

      call minimax(pts,xmin,xmax,npt)
      voverc = 0.
      obsname = '                    '
      
      filestyle = 'text'
      close (iunit)
      return

      end
      




