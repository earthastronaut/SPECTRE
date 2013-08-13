      subroutine startup
c*****initialize the program
      
      include 'Scrnval.com'
      include 'Dataval.com'
      include 'Datachr.com'
      include 'Plotval.com'
      include 'Plotchr.com'
      include 'Chars.com'
      character bl20*20,bl40*40
      character systemcall*100
      logical ex

      bl20 = '                    '
      bl40 = '                                        '

c*****clear the text screen and define the number of lines available 
c     on the screen; these coding lines work for SUNos machines
      inquire (file='/tmp/tmpsize',exist=ex)
      if (ex) then
         open (99,file='/tmp/tmpsize')
         close (99,status='delete')
      endif
      write (systemcall,*) 'stty size > /tmp/tmpsize'
      call system (systemcall)
      open (99,file='/tmp/tmpsize')
      read (99,*,err=1008) maxline
      maxline = maxline - 2
      go to 1009
1008  maxline = 24
1009  close (99,status='delete')
      write (systemcall,*) 'clear'
      call system (systemcall)

c*****ask for the mongo terminal type
      array = 'GIVE THE MONGO TERMINAL TYPE: '
      istat = ivwrite(1,1,array,30)
      read (*,*) smterm

c*****open the graphics screen
      if (sm_device(smterm) .lt. 0) then
         write (array,1006) smterm
1006     format ('DEVICE OPENING ERROR FOR:',a30)
         istat = ivwrite(1,1,array,55)
         stop
      endif
      call sm_graphics
      call sm_erase

c*****define colors: white, red, blue, yellow, green, cyan, magenta, black
       colors(1) = 'white  '
       colors(2) = 'red    '
       colors(3) = 'blue   '
       colors(4) = 'yellow '
       colors(5) = 'green  '
       colors(6) = 'cyan   '
       colors(7) = 'magenta'
       colors(8) = 'black  '

c*****initialize some variables
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
      do 20 i=1,131072
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

      return

      end






