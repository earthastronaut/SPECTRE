      subroutine width
c*****This routine calculates equivalent widths for spectral lines
c####################################################################
c This version keeps the basic function from original, to measure
c equivalent widths for spectral lines
c
c Most initial features (while still available) have been incorporated
c into the commands 'mm', 'ml', and 'ln'
c
c Use 'mm' as an option to read in a list of fits files for each order
c and use the entire linelist to march through and measure lines
c for each order
c
c Use 'ml' to measure a previously read in order from
c an entire linelist
c
c Use 'ln' to measure for a single line
c
c Please see notes on the "RUN INTERACTIVE" section for options on 
c line measurement (i.e. manual gaussian, simpsons rule, etc)
c
c 
c#######################################################################

      include 'Chars.com'
      include 'Plotval.com'
      include 'Dataval.com'
      include 'Datachr.com'
      include 'Mathval.com'
      include 'Widpar.com'
      character fname*80,mess*80,oname*80, tmp*70, widthnote*80
      character charstat*7
      real*8 wave,wavout,xnums(5),nub,spe,ep,loggf
      logical nogo,onelin, linopt, multo, firstmm
      integer kin, kout
      data ndel/50/

c*****initialize some variables
      onelin = .true.
      linopt = .false.
      multo = .false.

c     kount was replaced by kin and kout
c     This counter follows the input line list
      kin = 0
c     This counter follows the ouput file
      kout = 0

      ep = 0
      loggf = 0
      xleft = wlx(1)
      right = wlx(npx)
      up = 1.12*xmax
      down = 0.
      ndelt = ndel


c*****get the command and branch to the appropriate section of code
 1    prompt = 'WIDTH >>> '
      call getcom
      if (command .eq. 'ab' .or. command(1:1) .eq. 'a') go to 2
      if (command .eq. 'cd') go to 10
      if (command .eq. 'd0') go to 20
      if (command .eq. 'lf') go to 30
      if (command .eq. 'ln') go to 40
      if (command .eq. 'ml') go to 50
      if (command .eq. 'mm') go to 60
      if (command .eq. 'ul') go to 70
      if (command .eq. 'he') go to 80
      if (command .eq. 'rc') go to 90
      if (command .eq. 'sr') go to 100
      if (command .eq. 'np') go to 110
      if (command .eq. 'lt') go to 120
      if (command .eq. 'qu' .or. command(1:1) .eq. 'q') go to 2
      errmess = 'UNKNOWN WIDTH COMMAND; TRY AGAIN! '
      nchars = 34
      call puterr (nchars)
      go to 1


c*****here the continuum is defined for the width measures
c     command 'cd'
 10   call contin ('dv',nogo,.false.)
      go to 1

c*****here the whole plot is redone: all points, intensities from zero
c     command 'd0'
 20   call minimax (x,xmin,xmax,npx)
      xleft = wlx(1)
      right = wlx(npx)
      up = 1.12*xmax
      down = 0.
      call plotxy (1,1,wlx,x,npx,1)
      go to 1

c*****here the user can create a file containing line positions 'lf'
c     command 'lf'
30    write (message,2001) 
2001  format (31(' '),'LINE LIST CREATION',30(' '))
      write (array,2002)
2002  format (80(' '))
      call prinfo (1)
      message = 'GIVE THE FILE NAME FOR THE LINELIST: '
      nchars = 37
      call getasci (nchars)
      nchars = min0(nchars,80)
      fname = array(1:nchars)
      charstat = 'new    '
      call dskfil(30,iostat,fname,charstat,'sequential',
     .      'formatted  ',0)
      if (iostat .ne. 0) go to 1
      write (array,2003)
2003  format ('BELOW, ENTER THE WAVELENGTH LIST.  ANY NEGATIVE ',
     .        'NUMBER ENDS THE LIST. ',10x)
      call prinfo (4)
 31   message = 'LINE WAVELENGTH = '
      nchars = 18
      call getnum (nchars,wave)
      if (wave .eq. -9999.) go to 31
      if (wave .lt. 0.) go to 32
      write (30,1001) wave
1001  format(f15.3)
      go to 31
 32   rewind 30
      write (array,2005) fname
2005  format ('HERE IS THE LIST OF LINES IN FILE ',a40,6x)
      call prinfo (4)
      jount = 1
 33   do 34 i=1,5
 34      read (30,1001,end=35) xnums(i)
      write (array,2004) (xnums(j),j=1,5)
2004  format (5f15.3)
      call prinfo (4+jount)
      jount = jount + 1
      go to 33
 35   i = i - 1
      write (array,2004) (xnums(j),j=1,i)
      call prinfo (4+jount)
      close (30)
      go to 1


c*****here the equivalent width of a single line is determined 'ln'
c     command 'ln'
 40   mode = 1
 41   onelin = .true.
 42   message = 'LINE WAVELENGTH = '
      nchars = 18
      call getnum (nchars,wave)
      if (wave .eq. -9999.) go to 42
      eqwdth = -9999.
      call single (mode,wave,wavout,ipt,depth,halfl,halfr,eqwdth)
c     this 'mode' statement may be unnecessary. I rerouted 'sr' to use the main interactive section which has all options, including simpson's rule
      if (mode .eq. 2) go to 100
      if (ipt .eq. -1) go to 1
      go to 503
c     Edited this to used the main 'RUN INTERACTIVE' instead of it's own


c*****here the equivalent widths of a whole set of lines, read from a
c     disk file, will be done.
c     command 'ml'
 50   mode = 1
      onelin = .false.
      linopt = .false.

      if (linopt) then
         write (6,998)
      endif      
      message = 'GIVE THE FILE NAME OF THE INPUT LINELIST: '
      nchars = 42
      call getasci (nchars)
      nchars = min0(nchars,80)
      fname = array(1:nchars)
      charstat = 'old    '
      call dskfil(30,iostat,fname,charstat,'sequential',
     .      'formatted  ',0)
      if (iostat .ne. 0) go to 1
      kin = 0

      message = 'GIVE THE FILE NAME FOR EQUIVALENT WIDTH OUTPUT: '
      nchars = 48
      call getasci (nchars)
      nchars = min0(nchars,80)
      fname = array(1:nchars)
      charstat = 'new    '
      call dskfil(31,iostat,fname,charstat,'sequential',
     .      'formatted  ',0)
      if (iostat .ne. 0) go to 1
      kout = 0
      go to 51


c*****This will march through a series of order files, performing 
c     multi-line EW measurements for a given line list
c     command 'mm'
 60   mode = 1
      onelin = .false.
      multo = .true.

c*****read the file list
      message = 'GIVE THE FILE NAME OF THE ORDER LIST: '
      nchars = 39
      call getasci (nchars)
      nchars = min0(nchars,80)
      fname = array(1:nchars)
      charstat = 'old    '
      call dskfil(29,iostat,fname,charstat,'sequential',
     .      'formatted  ',0)
      if (iostat .ne. 0) go to 1

      message = 'GIVE THE FILE NAME OF THE INPUT LINELIST: '
      nchars = 42
      call getasci (nchars)
      nchars = min0(nchars,80)
      fname = array(1:nchars)
      charstat = 'old    '
      call dskfil(30,iostat,fname,charstat,'sequential',
     .      'formatted  ',0)
      if (iostat .ne. 0) go to 1
      firstmm = .True.

c***** *** START OF MULTIPLE FILE LOOP ***
 61   read (29,1111,end=65) fname
 1111 format (A80)
      oname = 'ew'//fname
      
c***** if it's not the first read in then move the current x array to the y array
      if (.not. firstmm) then 
         call move (x,y,wlx,wly,dispx,dispy,
     .        npx,npy,xobj,yobj,xkfnam,ykfnam,
     .        xfname,yfname,xary,yary,xfile,yfile)
      endif

      firstmm = .False.

c*****read in the data from the file to the x array
 66   call reed (2,x,wlx,dispx,fname,xkfnam,xobj,npx,
     .     xmin,xmax,vovercx,xary,xfile)
      if (wlx(1) .ne. -9999.) then
         up = 1.12*xmax
         down = 0.0
         xleft = wlx(1)
         right = wlx(npx)
         call labset (1)
         call plotxy (1,1,wlx,x,npx,1)
      else 
         up = 0.
         down = 0.
         xleft = 0.
         right = 0.
      endif

c*****Check file from overview
      write (6,903) fname,oname
 903  format (/,/,'VIEWING FILE: ',A20,'  SAVE OUTPUT AS: ',A20,
     .     /,/,'CAUTION: WILL OVERWRITE AN EXISTING OUTPUT FILE')
 62   message = 'MEASURE THIS ORDER (y,n,r,b,a)? '
      nchars = 33
      call getasci (nchars)
c      if (array(1:1).eq.'y' .or. nchars.le.0) then 
      if (array(1:1).eq.'y') then 
         go to 63
c     Skip        
      elseif (array(1:1) .eq. 'n') then
         write (6,*) 'SKIPPED  FILE: ',fname
         go to 61
c     replot
      elseif (array(1:1) .eq. 'r') then
         write (6,*) 'REPLOT'
         go to 66
c     Go back
      elseif (array(1:1) .eq. 'b') then
         backspace (29,err=67)
         backspace (29,err=67)
c     To create a smarter system it would be better for the program to
c     know when it was going to overwrite and warn, but I don't feel
c     like putting that work in right now
         go to 61
c     abort
 67      write (6,*) 'BACKSPACE ERROR'
         go to 61

      elseif (array(1:1) .eq. 'a') then
         go to 65

      elseif (array(1:1) .eq. 'h') then
         write (6,1010)
 1010    format ("COMMAND HELP:",/,
     .        "y(yes), n(skip file), r(replot), b(back one file), ",
     .        "a(abort)",/)
         go to 62
      else
         write (6,*) 'PLEASE ENTER VALID OPTION'
         go to 62
      endif

c**** return to start of line list
 63   rewind (30)
      kin = 0

c**** open save file, Waits until you've decided on the order
c     This is where it will crash if the file already exists, iostat=17,5002
c     change 'new' to 'unknown' but then overwriting will occur.
      call dskfil (31,jostat,oname,'unknown','sequential',
     .     'formatted  ',0)
      if (jostat .ne. 0) go to 1
      kout = 0

c*****let's go to the multi-line loop, 'RUN INTERACTIVE' w/ header
      go to 51

c*****ends the 'mm' and takes back to the Width prompt
 65   kout = 0
      kin = 0
      multo = .false.
      onelin = .true.
      close (29)
      close (30)
      go to 1


c*****Here the equivalent width of a line is done manually from
c     keyboard cursor placement
c
c     It seems to do the same thing as 'ln'
c     I think originally this was meant to measure based on
c     an automatic gaussian and one based upon a manual.
c     setting the continnum will do the automatic
c     using the no will do the manual
c     there still may be some autamation this was meant to work for
 70   write (6,*) 'DOES SAME AS "ln"'
      go to 40
c=========> original code
c     
c70    mode = 0   # this bypasses the manual gaussian fit call in
c                   the subroutine single
c      onelin = .true.
c      kount = kount + 1
c71    message = 'LINE POSITION = '
c      nchars = 16
c      call getnum (nchars,wave)
c      if (wave .eq. -9999.) go to 71
c      call single (mode,wave,wavout,ipt,depth,halfl,halfr,eqwdth)
c      call manual (0,ipt,wave,wavout,depth,halfl,halfr,eqwdth)
c     Changed to record on the 'y'
c      call record (kout,linopt,onelin,wave,wavout,depth,
c     .     halfl,halfr,eqwdth,widthnote)
c      go to 503
c      go to 1


c*****here a list of WIDTH commands are displayed on the screen
c     command 'he'
 80   call printh('widhelp')
      go to 1

c*****here the original continuum is reset
c     command 'rc'
 90   call setre (0,ycont)
      do 91 i=1,npx
 91      x(i) = x(i)*ycont
      go to 1

c*****Here the line integration is done brute-force by Simpson's Rule
c     command 'sr'
 100  write (6,*) 'APPLY SIMPSONs RULE DURING "ln"'
      go to 40
 102  mode = 2
      call single (mode,wave,wavout,ipt,depth,halfl,halfr,eqwdth)
101   message = 'IS THE LINE OK ([y],n,l,a)? '
      nchars = 29
      call getasci (nchars)

      if (array(1:1).eq.'y' .or. nchars.le.0) then
         mode = 1
         go to 503
      elseif (array(1:1) .eq. 'n') then
         call newcont
         call single (mode,wave,wavout,ipt,depth,halfl,halfr,eqwdth)
         go to 101
c     enter new wavelength sought
      elseif (array(1:1) .eq. 'l') then
         if (onelin) go to 42
         go to 101
      elseif (array(1:1) .eq. 'a') then
         mode = 1
         go to 2
      else
         go to 101
      endif

c*******to adjust the visiable number of points
c     This feature has been built into the interactive version 
c     It has been kept here for people who are use to using it
c     command 'np'

 110  message = 'DESIRED NUMBER OF POINTS TO BE PLOTTED = '
      nchars = 41
      call getnum (nchars,wave)
      if (wave .eq. -9999.) go to 110
      ndelt = int(sngl(wave))
      ndel = ndelt
      go to 1


c***** Read in MOOG formated linelist and display the information
c      currently not available
c      command 'lt'
 
 120  message = 'ARE YOU USING MOOG FORMAT ([y]/n)? '
      nchars = 40
      call getasci (nchars)

      if (array(1:1).eq.'y' .or. nchars.le.0) then 
         linopt = .true.
         
         write (6,998)
 998     format('READ IN MUST BE FORMATTED (7e10.3):',/
     .        '4390.029        11.0     2.102    -1.944',
     .        '                         999.9     NOTES Etc',/)
         go to 1
      elseif (array(1:1) .eq. 'n') then
         linopt = .false.
         go to 1
      else
         write (6,*) 'PLEASE ENTER VALID OPTION'
         go to 120
      endif

c     read in the linelist
c      if (linopt) then
c         read (nfslines,1002,end=340) swave(j),satom(j),sep(j),
c     .        sloggf(j),sdampnum(j),sd0(j),swidth(j)
c      else
c         read (nfslines,*,end=340)  swave(j),satom(j),sep(j),
c     .        sloggf(j),sdampnum(j),sd0(j),swidth(j)
c      endif
c
c1002  format (7e10.3)





c==============================================================================c
c***** RUN INTERACTIVE (The heart of the width subroutine)
c      This provides all the options for measuring a given line

c**** Header for the output file
 51   call record (kout,linopt,onelin,wave,wavout,depth,
     .     halfl,halfr,eqwdth,widthnote)
      kout = 1
      kin = 1

c**** what I'm going to record/display
      linopt = .false.

 1005 format("Can't currenlty read moog format, using default")
 500  if (linopt) then
         write (errmess,1005)
         nchars = 47
         call puterr (nchars)
c        If you want this to work see code under the 'lt' command
      endif

      read (30,1002,end=510) wave,linfo
 1002 format(f15.3,A70)

c     the following checks for the first line of the order
      if ((wave .lt. wlx(1)) .or. (wave .gt. wlx(npx))) go to 500
      eqwdth = -9999.

      call single (mode,wave,wavout,ipt,depth,halfl,halfr,eqwdth)

c***** The Prompt
 503  message = 'RECORD THIS LINE ([y],n,c,v,s,o,a,b,li,t,ll,dy,dz,p)? '
      nchars = 54
      call getasci (nchars)

c**** The Prompt Options
      if (array(1:1).eq.'y' .or. nchars.le.0) then
c        record and go onto the next line
         call record (kout,linopt,onelin,wave,wavout,depth,
     .        halfl,halfr,eqwdth,widthnote)
         if (onelin) go to 1
         kin = kin + 1
         kout = kout + 1
         go to 500

      elseif (array(1:1) .eq. '.') then
         message = 'Add a note:'
         nchars = 12
         call getasci (nchars)
         widthnote = array
         write (*,*) "you gave me",widthnote
         go to 503

      elseif (array(1:1) .eq. 'n') then
c        recalculate the fit manually
         call manual (0,ipt,wave,wavout,depth,halfl,halfr,eqwdth)
         go to 503

      else if (array(1:1) .eq. 'c') then
c        recalculate the fit by just setting the continuum
         call newcont
         call single (mode,wave,wavout,ipt,depth,halfl,halfr,eqwdth)
         go to 503   

      elseif (array(1:1) .eq. 'v') then
c        adjust line profile by voigt
         call voigtfit (0,wavout,ipt,depth,halfl,halfr,eqwdth)
         go to 503

      elseif (array(1:1) .eq. 's') then
c        go to the fit with simpsons rule         
         go to 102

      elseif (array(1:1) .eq. 'o') then
c        omit current line and go onto the next
         if (onelin) go to 43
         eqwdth = -9999.
         call record (kout,linopt,onelin,wave,wavout,depth,
     .        halfl,halfr,eqwdth,widthnote)
         kout = kout + 1
         kin = kin + 1
         go to 500

      elseif ((array(1:1).eq.'a').or.(array(1:4).eq.'quit')) then
c        abort and return to SP>eq
         if (onelin) go to 43
         if (multo) close (29)
         close (30)
         close (31)
         go to 2

      elseif (array(1:1) .eq. 'b') then
c        go back one line
         if (onelin) then
            write (6,*) 'NOT VALID OPTION FOR ONE LINE'
            go to 503
         endif
         kout = kout - 1
         kin = kin - 1
         if (kout .eq. 0) then
            kout = 1
            kin = 1
            write (6,*) '*AT THE BEGINNING LINE FOR THIS ORDER*'
            go to 503
         endif
         backspace (30,err=505)
         backspace (30,err=505)
c        back up the output file by one
c        backspace (31,err=40)
         backspace (31,err=505)         
         go to 500
 505    write (6,*) 'BACKSPACE ERROR'
         go to 510

      elseif (array(1:1) .eq. 'r') then
c        replot the data (not very useful
         call plotxy (1,1,wlx,x,npx,1) 
         go to 503

      elseif (array(1:2) .eq. 'li') then
c        remove a gaussian profile from the data using 'li'
         tmp=linfo
         call mkline
         linfo='  '
         call single (mode,wave,wavout,ipt,depth,halfl,halfr,eqwdth)
         linfo=tmp
         go to 503

      elseif (array(1:2) .eq. 'll') then
c        change the overplotted linelist
         call changelinelist('prompt')
         go to 503

      else if (array(1:2) .eq. 'dz') then
c        plot what is in the z array
         istyle = -3
         call plotxy (1,-1,wlz,z,npz,istyle)
         go to 503

      else if (array(1:2) .eq. 'dy') then
c        plot what is in the y array
         istyle = -2
         call plotxy (1,-1,wly,y,npy,istyle)
         go to 503

      elseif (array(1:1) .eq. 'p') then
c        change the number of points being shown

         write (6,*) 'CURRENT NUMBER OF POINTS =',ndel
 502     message = 'DESIRED NUMBER OF POINTS TO BE PLOTTED = '
         nchars = 41
         call getnum (nchars,nub)
         if (nub .eq. -9999.) go to 502
          ndelt = int(sngl(nub))
         ndel = ndelt
         call single (mode,wave,wavout,ipt,depth,halfl,halfr,eqwdth)
         write (6,*) 'NOTE: CONTINUUM PLACEMENT PRESERVED'
         go to 503

      else if (array(1:1) .eq. 't') then
c        toggle on and off the overplotting of lines
         if (oplotlines .eq. 1) then
            oplotlines = 0.0
            write (6,*) 'OVERPLOT LINES TURNED OFF'
         else
            oplotlines = 1.0
            write (6,*) 'OVERPLOT LINES TURNED ON'
            call plotlines()
         endif
         go to 503

      elseif (array(1:1) .eq. 'h') then
c     some on the fly help
         write (6,3003)
 3003    format ('OPTIONS FOR RECORD',/,
     .        'y(yes), n(no), c(continuum), v(voit), s(simpsons int), ',
     .        'o(omit)',/,
     .        'a(abort), b(back), li(Gauss line), r(redraw),',/,
     .        't(toggle overplot lines), ll(choose overplot lines), ',/,
     .        'dy(display Y-array), dz(display Z-array, ',
     .        'p(#points/scale)')
         go to 503
      else
c        Whoops, not an appropriate command 
         write (6,*) "PLEASE ENTER VALID OPTION",
     .        "(use 'h' to view options)"
         go to 503
      endif


c**** Close out and go to width prompt
 510  kout = 0
      kin = 0
      close (31)
      if (multo) then
         write (6,*) 'Read a new file in'
         go to 61
      endif
      onelin = .true.
      close (30)
      go to 1

c**** END OF RUN INTERACTIVE
c==============================================================================c

c**** ends the 'ln', onelin option
 43   call record (kout,linopt,onelin,wave,wavout,depth,
     .     halfl,halfr,eqwdth,widthnote)

c**** reset the plot boundaries and return
 2    xleft = wlx(1)
      right = wlx(npx)
      up = 1.12*xmax
      down = 0.
      linfo='  '
      oplotlines = 0.0
      return

      end
