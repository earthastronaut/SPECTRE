      subroutine changelinelist (adopt)
c*****This routine changes which linelist will be plotted based on defaults
c     Author: Dylan Gregersen
c     Author Email: dylan.gregersen@gmail.com
c     Date: June 12, 2013
c

      include 'Chars.com'
      include 'Plotval.com'
      character adopt*10,fname*80
      integer StrLen

c*** fix length of code path
      StrLen = 80
      call cropstr (codepath,StrLen)

c*****allow for skipping over prompt section
      if (adopt .eq. 'prompt') then
         go to 1
      else 
         array(1:10) = adopt
         goto 2
      endif
     
    
c*****get the filename you want to use
c     =========================================================================c
c     **USER** ====> add new linelist command
c     update the message
c        message = "CHOOSE A LINELIST TO OVERPLOT (solar/apogee/new/h/a): "
c
c     and change the number of characters
c         nchars = 54
c     =========================================================================c
 1    message = "CHOOSE A LINELIST TO OVERPLOT (solar/apogee/h/a): "
      nchars = 50
      call getasci (nchars)
 2    if (array(1:2) .eq. 'so') then
         fname=codepath(1:StrLen)//"L_solar"
         go to 3

      elseif (array(1:2) .eq. 'ap') then
         fname=codepath(1:StrLen)//"L_apogee"
         go to 3


c     =========================================================================c
c     **USER** ====> add linelist file location here
c     elseif (array(1:2) .eq. 'new') then
c         fname='/path/to/linelist.txt'
c         go to 3
c
c      file must be formated with one line to skip
c      then the subsequent follow  format(f11.3,A)
c       5000.0  Extra Information About Line
c      make sure you check the length of the file name
c      character codepath*60, fname*80
c      
c      Note the limitation given at the top of this file
c     =========================================================================c

      elseif (array(1:2) .eq. 'h') then
         write (6,1000)
 1000    format ("LINE LIST OPTIONS:",/,
     .        " solar : Line list taken from Hinkle et al. (2000) ",
     .        "'Visible and Near Infrared ",
     .        "Atlas of the Arcturus Spectrum 3727-9300A'",/,
     .        "apogee : Line list from APOGEE linelist.201202161204",/,

c     =========================================================================c
c     **USER** ====> add information about the new linelist:
c     .        "new list : information about linelist",/,
c     =========================================================================c


     .        "     h : This help screen",/,
     .        "     a : Abort")
         go to 1
      elseif (array(1:2) .eq. 'a') then
         go to 100
      else
         go to 1
      endif


c*****put that filename into read file 23
 3    call dskfil(23,iostat,fname,'old    ','sequential',
     .     'formatted  ',0)
      if (iostat .ne. 0) then
         write (6,*) "PROBLEM READING LINELIST FILE:",iostat
      endif

c****exit
 100  return
      end
