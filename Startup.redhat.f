      subroutine startup
c*****initialize the program
      
      include 'Scrnval.com'
      include 'Dataval.com'
      include 'Datachr.com'
      include 'Plotval.com'
      include 'Plotchr.com'
      include 'Chars.com'
      character bl20*20,bl40*40,fname*80
      character systemcall*100
      character line*80
      logical ex

      bl20 = '                    '
      bl40 = '                                        '

c*****clear the text screen and define the number of lines available 
c     on the screen; these coding lines work for Solaris machines
      write (systemcall,*) 'stty -a > /tmp/tmpsize'
      call system (systemcall)
      open (99,file='/tmp/tmpsize')
      read (99,1010) line
1010  format (a80)
      do i=1,80
cc         if (line(i:i+3) .eq. 'rows') then
cc            read (line(i+4:i+6),1011) maxline
1011        format (i3)
            maxline = 20
            go to 1009
cc         endif
      enddo
1008  maxline = 24
1009  close (99,status='delete')
      write (systemcall,*) '\\rm -f /tmp/tmpsize'
      call system (systemcall)
      write (systemcall,*) 'clear'
      call system (systemcall)

c*****ask for the mongo terminal type
      array = 'GIVE THE MONGO TERMINAL TYPE: '
      istat = ivwrite(1,1,array,30)
c      read (*,*) smterm
      smterm = 'x11'
c*****open the graphics screen
      if     (smterm.eq.'x11' .or. smterm.eq.'X11') then
c*****this configuration works pretty well on my Redhat linux laptop
c        if (sm_device('x11 -title SPECTREplot -geom 900x300+50+380') 
c*****this configuration works pretty well on my Redhat two-screen
         if (sm_device('x11 -bg black -geom 2000x600+20+600') 
c*****this configuration works pretty well on my 13' Macbook Pro
c         if (sm_device('x11 -bg black -geom 1280x400+0+350') 
     .      .lt. 0) then
            write (array,1006) smterm
1006        format ('DEVICE OPENING ERROR FOR:',a30)
            istat = ivwrite(1,1,array,55)
            stop
         endif
      else   
         if (sm_device(smterm) .lt. 0) then
            write (array,1006) smterm
            istat = ivwrite(1,1,array,55)
            stop
         endif
      endif
      call sm_graphics
      call sm_erase

c*****define colors: white, red, blue, yellow, green, cyan, magenta, black
       colors(1) = 'white  '
       colors(2) = 'red    '
c      colors(3) = 'blue   '
       colors(3) = 'cyan   '
       colors(4) = 'yellow '
       colors(5) = 'green  '
c      colors(6) = 'cyan   '
       colors(6) = 'blue   '
       colors(7) = 'magenta'
       colors(8) = 'black  '

c*****define arbitrary colors
c     ncol = 50
c     realcolors(2) = 255
c     realcolors(3) = 256*255
c     realcolors(4) = 256*256*255
c     do i=5,50
c        realcolors(i) = int(255*(i-1)/real(ncol - 2)) +
c    .                   256*(int(255*(i-1)/real(ncol - 2)) +
c    .                   256*0)
c     enddo
c     call sm_set_ctypes (realcolors,ncol)



c*****open the file with the Solar Atlas
c     can open others just need to change the file code but
c     do NOT use: 5,10,15,20,21,27,30,31,75, (others?)
c     Mrklin.f uses this
       call changelinelist('solar     ')

c*****initialize some variables
c      oplotlines = .True.
      oplotlines = 0.0
      linfo='  '
      xleft = 0.0
      right = 0.0 
      up = 0.0
      down = 0.0
      plxlab = 'CHANNEL'
      plylab = 'INTENSITY'
      ixlcnt = 7
      iylcnt = 9
      npx = 0
      npy = 0
      npz = 0
      xary = '  0'
      yary = '  0' 
      zary = '  0'
      xobj = bl20
      yobj = bl20
      zobj = bl20
      xfname = bl40
      yfname = bl40
      zfname = bl40
      xkfnam = bl20
      ykfnam = bl20
      zkfnam = bl20
      do 10 i=1,9
         dispx(i) = 0.
         dispy(i) = 0.
10       dispz(i) = 0.
      do 20 i=1,10000
         x(i) = 0.
         y(i) = 0.
         z(i) = 0.
         wlx(i) = 0.
         wly(i) = 0.
20       wlz(i) = 0.




c*****initialize the auxiliary information screen
      call screenstat (0)

c*****print some startup messages
      array = 'WELCOME TO SPECTRE!        (VERSION OF 9/19/94)'
      call prinfo (2)
      array = 'FOR A SPECTRE COMMAND LOOP SUMMARY, TYPE:  he'
      call prinfo (4)
      array = 'FOR EXPLANATION OF A PARTICULAR COMMAND, TYPE: ??'
      call prinfo (6)


c*****initialize the scroll back history
c These file numbers have been used
c dskfil open 23, 8, 75, 40, 30,31,32, 15, 21, 29, 35
C$$$c     Open a new file called '.spectre_history'
C$$$      istat = 'new    '
C$$$ 31   open (35,iostat,err=30,file='.spectre_history',
C$$$     .     status=istat,access='sequential',form='formatted  ')
      
C$$$c     If there's an error cause it already exists
C$$$ 30   if (jostat .eq. 117 .or. jostat.eq.1017 .or.
C$$$     .     jostat .eq. 128 .or. jostat .eq. 17) then
C$$$         istat = 'old    '
C$$$         go to 31
C$$$      endif

C$$$      if (istat .eq. 'old    ') then
C$$$c         read in old lines
C$$$         continue
C$$$      endif
c The idea is that when you arrow up to read lines from the file and arrow down you can go back through them.
c use write, read, and backspace to go through the commands 

      return

      end






