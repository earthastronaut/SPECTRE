      subroutine multicmd
c***** This routine allows you to perform specific commands over a list of files and save the results
c     
c**** Declare variables
      include 'Dataval.com'
      include 'Datachr.com'
      include 'Chars.com'
      include 'Mathval.com'
      include 'Plotchr.com'
      include 'Plotval.com'
      include 'Scrnval.com'

      character fname*80,outname*80,charstat*7
      integer check, contall, firstgo
      double precision xnum
      character*1 choice
      logical splitd
      real*4 voverset

c***** Initalize variables
      contall = 0

c*****read the file list
 9    message = 'GIVE THE FILE NAME OF THE INPUT FILELIST: '
      nchars = 42
      call getasci (nchars)
      nchars = min0(nchars,80)
      fname = array(1:nchars)
      charstat = 'old    '
      call dskfil(30,iostat,fname,charstat,'sequential',
     .      'formatted  ',0)
      if (iostat .ne. 0) go to 2

c*****read the file list
      message = 'GIVE THE FILE NAME OF THE OUTPUT FILELIST: '
      nchars = 43
      call getasci (nchars)
      nchars = min0(nchars,80)
      fname = array(1:nchars)
      charstat = 'old    '
      call dskfil(31,iostat,fname,charstat,'sequential',
     .      'formatted  ',0)
      if (iostat .ne. 0) go to 2

c****Enter command
 1    prompt= "MULTI >>> "
      call getcom
      if (command .eq. 'av') goto 5
      if (command .eq. 'du') goto 5
      if (command .eq. 'vs') goto 5
      if (command .eq. 'he' .or. command(1:1) .eq. 'h') goto 6
      if (command(1:1) .eq. '?') goto 7
      if (command .eq. 'qu' .or. command(1:1) .eq. 'q') goto 2
      errmess = "UNKNOWN MULTI COMMAND; 'he' for help! "
      nchars = 38
      call puterr (nchars)
      go to 1

c**** Write help message
 6    call printh('multihelp')
      goto 1

 7    call comtalk('mulhelp')
      goto 1


c**** And...GO!
 5    continue

c******************************************************************************c
c**** This is part of the automatic command sequence prior to file loop

c=============== 'vs' start section ===========================c
      if (command .eq. 'vs') then
c**** set up what value to be shifting by
         voverset = 999.9
         call velset (voverset)
         if (voverset .eq. 999.9) goto 2
      endif 
c=============== 'vs' end section =============================c



c**** THIS WHERE THE LOOP FOR FILES STARTS ****c
      firstgo = 1

c**** Get file input filename
 100  read (30,1001,end=2) fname
c**** Get file outputinput filename
      read (31,1001,end=2) outname

c**** if the files are the same error out
      if (fname .eq. outname) then 
         goto 201
      endif


c**** read in the file automatically to the xarray
      call reed (2,x,wlx,dispx,fname,xkfnam,xobj,npx,
     .     xmin,xmax,vovercx,xary,xfile)
      if (wlx(1) .ne. -9999.) then
         up = 1.12*xmax
         down = 0.0
         xleft = wlx(1)
         right = wlx(npx)
         call labset (1)
      else 
         up = 0.
         down = 0.
         xleft = 0.
         right = 0.
      endif
c     call screenstat (1)


c***** This is where the automatic command sequences begin
c##############################################################
c=============== 'vs' start section ===========================c
      if (command .eq. 'vs') then
c        apply the radial velocity to the current data
 11      vovercx = voverset
         call velocity ('yes',vovercx,dispx,wlx,npx)

c**** Save out the file
         call save (2,x,wlx,npx,dispx,fname,outname,xary,xobj)
c=============== 'vs' end  section ===========================c

c##############################################################

c=============== 'du' start section ===========================c
c**** Open file to write into
      elseif (command .eq. 'du') then
         call dskfil (32,jostat,outname,'new    ','sequential',
     .        'formatted  ',0)
         if (jostat .ne. 0) goto 8
            

c*****dump the x-array input into the file
         if (npx .eq. 0) go to 200
         write (32,1002) (wlx(i),x(i),i=1,npx)
         close (32)
c=============== 'du' end section ===========================c

c##############################################################

c=============== 'av' start section ===========================c
      elseif (command .eq. 'av') then
c*****copy the x-array temporarily to the z-array (save the z-array)
         if (npz .ne. 0) then
            do i=1,npz
               scratch(i) = z(i)
            enddo
         endif
         nphold = npz
         do i=1,npx
            wlz(i) = wlx(i)
            z(i) = x(i)
         enddo
         npz = npx
   
c*****this sets up the smoothing that will happen automatically
         if (firstgo .eq. 1) then
   
c*****set smoothing for files
            call setav(choice,xnum)
            firstgo = 0
         endif
   
c*****see what you've got in the x-array
         call plotxy (1,1,wlx,x,npx,1)
         if (contall .eq. 0) then
            call plotxy (1,1,wlx,x,npx,1)
 30         message = 'CONTINUE ([y]/i/a)?'
            nchars = 19
            call getasci (nchars)
            if (array(1:1).eq.'y' .or. nchars.le.0) then 
               go to 31
            elseif (array(1:1) .eq. 'i') then
               write (6,1003) fname,outname
               go to 30       
            elseif (array(1:1) .eq. 'a') then
               go to 3
            endif
         endif
   
    
c*****fill in the smoothing function array
 31      if (choice .eq. 'm') then
            jdel = xnum - (xnum/2) - 1
            if (jdel .gt. 30) go to 205
            power = 1.
            do i=1,jdel
               ss(i) = 1.
               power = power + 2.*ss(i)
            enddo
         elseif (choice .eq. 't') then
            jdel = xnum - (xnum/2) - 1
            if (jdel .gt. 30) go to 205
            power = 1.
            do i=1,jdel
               ss(i) = real(jdel+1-i)/real(jdel+1)
               power = power + 2.*ss(i)
            enddo
         elseif (choice .eq. 'g') then
            sigma = xnum/2
            aa = 0.6932/sigma**2
            power = 1.0
            do i=1,30
               ss(i) = exp(-aa*real(i)**2 )
               power = power + 2*ss(i)
               if (ss(i) .lt. 0.05) go to 32
            enddo
            go to 205
 32         jdel = i
         endif
    
c*****smooth the spectrum in the x-array
         min = jdel + 1
         max = npx - jdel
         do i=min,max
            x(i) = z(i)
            do j=1,jdel
               x(i) = x(i) + ss(j)*(z(i-j) + z(i+j))
            enddo
               x(i) = x(i)/power
         enddo
         do i=1,jdel
            x(i) = x(min)
            x(npx-i+1) = x(max)
         enddo
      
         if (contall .eq. 1) then
            go to 33
         endif
   
c*****plot the smoothed and raw spectra
         call estim (left,right,dispx,wlx,npx,ileft,iright,xfile)
         call plotxy (1,1,wlz(ileft),z(ileft),npz,30)
         call plotxy (1,-1,wlx(ileft),x(ileft),npx,-1)
   
c*****see if the smoothing is OK
 34      message = 'IS THE SMOOTHING OK ([y]/n/s/i/a/help)? '
         nchars = 41
         call getasci (nchars)
         if (array(1:1).eq.'y' .or. nchars.le.0) then
             go to 33
         elseif (array(1:1) .eq. 'n') then
            call setav(choice,xnum)
            go to 31
         elseif (array(1:1) .eq. 's') then
            contall = 1
            go to 33
         elseif (array(1:1) .eq. 'd') then
            write (6,1003) fname,outname
            go to 34         
         elseif (array(1:1) .eq. 'a') then
            go to 3
         elseif (array(1:4) .eq. 'help') then
            write (6,1004)
         else
            go to 34
         endif
   
 33      npz = nphold
         if (npz .ne. 0) then
            do i=1,npz
               wlz(i) = wave(real(i),npz,dispz)
               z(i) = scratch(i)
            enddo
         endif
 
         call save (2,x,wlx,npx,dispx,fname,outname,xary,xobj)
c=============== 'av' end  section ===========================c

c##############################################################


c***** This is where the automatic command sequences end
      endif
      goto 100
c**** THIS WHERE THE LOOP FOR FILES ENDS  ****c
c******************************************************************************c

c**** Formats
 1000 format ("Commands which can be repeated on mulitple files:",/,
     .     "av - Average data",/,
     .     "du - Dump data to text files",/,
     .     "he - Display this help screen",/,
     .     "vs - Apply a radial velocity to the data",/,
     .     "qu - Quit and return to SPECTRE")
 1001 format (A)
 1002 format (f20.10,f20.10)
 1003 format ('INFORMATION==> INPUT FILE:',a80,'OUTPUT FILE:',a80)
 1004 format ("[y]: goto next file, n: change smoothing,",
     .     "s: finish all files, i: information on files",
     .     "a: abort")


c**** Error messages
 200  write (errmess,2000) 
 2000 format('Error: File read problem or no points in x array')
      nchars = 48
      call puterr (nchars)
      goto 2

 201  write (errmess,2001) 
 2001 format('Error: output filename equals input filename')
      nchars = 44
      call puterr (nchars)
      goto 2

 8    write (errmess,*) "Whoops, Try again"
      nchars = 17
      call puterr (nchars)
      close(30) 
      close(31)
      goto 9


c=============== 'av' begin  section ===========================c
c*****depart if the smoothing size looks too big
 205  errmess = 'SMOOTHING SIZE TOO BIG!  I QUIT!'
      nchars = 41
      call puterr (nchars)

c***** Finish up the averaging
 3    do i=1,npz
         x(i) = z(i)
      enddo
      npz = nphold
      if (npz .ne. 0) then
         do i=1,npz
            wlz(i) = wave(real(i),npz,dispz)
            z(i) = scratch(i)
         enddo
      endif
c=============== 'av' end  section ===========================c

c*****close and exit
 2    close(30) 
      close(31)
      return
      end






