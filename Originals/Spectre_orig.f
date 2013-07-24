      program spectre 
c*****A data manipulation program designed for the coude spectrographs.
c     Programmers: Michael Fitzpatrick and Chris Sneden
c     Programming begun March 1, 1985 ---- ended....sometime in the future
 
c     x, y, and z are the three main spectrum arrays. 
 
      include 'Dataval.com'
      include 'Datachr.com'
      include 'Chars.com'
      include 'Plotchr.com'
      include 'Plotval.com'
      logical nogo,splitd
      character arr*1,arr1*1,arr2*1

c*****start the program and open files
      istat = 1
      nogo = .false.
      splitd = .false.
      exact = 0
      call startup 
 
c*****here is the main loop of commands
c     first get command abreviation
10    write (array,1001)
      write (errmess,1001)
      write (message,1001)
1001  format (80(' '))
      prompt= 'SPECTRE > '
      call getcom
 
c*****ac = addition of a constant
      if     (command .eq. 'ac') then
4028        call which (arr)
            if (arr.eq.'x' .or. arr.eq.'X') then
               call const (1,x,npx)
               call minimax (x,ptslo,ptshi,npx)
               xmin = ptslo
               xmax = ptshi
               down = amin1(xmin,ptslo,0.)
               up = 1.12*amax1(xmax,ptshi)
               call plotxy (1,1,wlx,x,npx,1)
            elseif (arr.eq.'y' .or. arr.eq.'Y') then
               call const (1,y,npy)
               call minimax (y,ptslo,ptshi,npy)
               down = amin1(xmin,ptslo,0.)
               up = 1.12*amax1(xmax,ptshi)
               call plotxy (1,1,wly,y,npy,2)
            elseif (arr.eq.'z' .or. arr.eq.'Z') then
               call const (1,z,npz)
               call minimax (z,ptslo,ptshi,npz)
               down = amin1(xmin,ptslo,0.)
               up = 1.12*amax1(xmax,ptshi)
               call plotxy (1,1,wlz,z,npz,3)
            else
               go to 4028
            endif
            go to 10

c*****ad = addition of the x-array and the y-array
      elseif (command .eq. 'ad') then
            call charith (1)
            up = 1.12*xmax
            down = 0.
            xleft = wlx(1)
            right = wlx(npx)
            call plotxy (1,1,wlx,x,npx,1)
            go to 10
 
c*****ag = a = plot the x-array again without changing plot boundaries
      elseif (command.eq.'a' .or. command.eq.'ag') then
            if (left.eq.0.0 .and. right.eq.0.0 .and. up.eq.0.0
     .          .and. down.eq.0.0) then
               errmess = 'ERROR: L,R,U,D BOUNDS ARE NOT SET! '
               nchars = 35
               call puterr (nchars)
               go to 10
            endif        
            call plotxy (1,1,wlx,x,npx,1)
            splitd = .false.
            go to 10
 
c*****al = antilog of the points
      elseif (command .eq. 'al') then
            mode = 1
4024        call which (arr)
            if (arr.eq.'x' .or. arr.eq.'X') then
               call logit (x,wlx,npx,1,mode)
            elseif (arr.eq.'y' .or. arr.eq.'Y') then
               call logit (y,wly,npy,2,mode)
            elseif (arr.eq.'z' .or. arr.eq.'Z') then
               call logit (z,wlz,npz,3,mode)
            else
                   go to 4024
            endif
            go to 10
 
c*****av = average adjacent points (a crude smoothing)
      elseif (command .eq. 'av') then
            call average
            go to 10  

c*****bb = blowup a box in both dimensions.  Cursor sets the bounds
      elseif (command.eq.'b ' .or. command.eq.'bb') then
            if (splitd) go to 9995
            call blowbox
            splitd = .false.
            go to 10
 
c*****bx = blowup only in the x-dimension, according to cursor input
      elseif (command .eq. 'bx') then
            if (splitd) go to 9995
            call blowx
            splitd = .false.
            go to 10
 
c*****by = blowup only in the y-dimension, according to cursor input
      elseif (command .eq. 'by') then
            if (splitd) go to 9995
            call blowy 
            splitd = .false.
            go to 10
 
c*****cc = cross correlate arrays x and y
      elseif (command .eq. 'cc') then
            call correl
            go to 10
 
c*****cd = continuum division by a user-defined continuum
      elseif (command .eq. 'cd') then
            call contin ('dv',nogo,splitd)
            go to 10
 
c*****ch = chop off the ends of the array
      elseif (command .eq. 'ch') then
4009        call which (arr)
            if (arr.eq.'x' .or. arr.eq.'X') then
               call zot (nogo,command,wlx,x,npx,dispx,1)
            elseif (arr.eq.'y' .or. arr.eq.'Y') then
               call zot (nogo,command,wly,y,npy,dispy,2)
            elseif (arr.eq.'z' .or. arr.eq.'Z') then
               call zot (nogo,command,wlz,z,npz,dispz,3)
            else
                   go to 4009
            endif
            call screenstat (1)
            go to 10

c*****cl = clear the information screen
      elseif (command .eq. 'cl') then
            call screenstat (0)
            go to 10
 
c*****cr = cosmic ray (or emission spike) removal
      elseif (command .eq. 'cr') then
            call crays
            go to 10

c*****d0 = display the whole x-array data, with default plot boundaries
      elseif (command .eq. 'd0') then
            splitd = .false.
            xleft = wlx(1)
            right = wlx(npx)
            up = 1.12*xmax
            down = 0.
            call plotxy (1,1,wlx,x,npx,1)
            go to 10
 
c*****dc = make a dispersion curve for the spectrum
      elseif (command .eq. 'dc') then
            call scale
            splitd = .false.
            go to 10
 
c*****de = deconvolve the spectrum, to increase resolution
      elseif (command .eq. 'de') then
            call deconv
            splitd = .false.
            go to 10
 
c*****df = delete a named files
      elseif (command .eq. 'df') then
            call killf
            go to 10

c*****du = dump the spectra to a file in MONGO style of x,y point pairs
      elseif (command .eq. 'du') then
            call dump
            go to 10

c*****dv = division of the x-array by the y-array
      elseif (command .eq. 'dv') then
            call charith (3)
            up = 1.12*xmax
            down = 0.
            xleft = wlx(1)
            right = wlx(npx)
            call plotxy (1,1,wlx,x,npx,1)
            go to 10
 
c*****dx = display the x-array as a line
c*****px = display the x-array as a set of points
      elseif (command.eq.'dx' .or. command.eq.'px') then
            if (command .eq. 'dx') then
               istyle = -1
            else
               istyle = -10
            endif
            if (splitd) then
                call splitds (wlx,x,npx,istyle)
            else  
                call plotxy (1,-1,wlx,x,npx,istyle)
            endif
            go to 10
 
c*****dy = display the y-array as a line
c*****py = display the y-array as a set of points
      elseif (command.eq.'dy' .or. command.eq.'py') then
            if (command .eq. 'dy') then
               istyle = -2
            else
               istyle = -20
            endif
            if (splitd) then
                call splitds (wly,y,npy,istyle)
            else  
                call plotxy (1,-1,wly,y,npy,istyle)
            endif
            go to 10
 
c*****dz = display the z-array as a line
c*****pz = display the z-array as a set of points
      elseif (command.eq.'dz' .or. command.eq.'pz') then
            if (command .eq. 'dz') then
               istyle = -3
            else
               istyle = -30
            endif
            if (splitd) then
                call splitds (wlz,z,npz,istyle)
            else  
                call plotxy (1,-1,wlz,z,npz,istyle)
            endif
            go to 10
 
c*****eb = explicitly blowup a plot according to user-named boundaries
      elseif (command .eq. 'eb') then
            call expblo
            splitd = .false.
            go to 10

c*****ed = edit bad data points from the x array
      elseif (command .eq. 'ed') then
            if (splitd) go to 9995
            call edit
            go to 10
 
c*****el = emission line plots
      elseif (command .eq. 'el') then
4023        call which (arr)
            if (arr.eq.'x' .or. arr.eq.'X') then
               call elines (wlx,x,1,npx)
            elseif (arr.eq.'y' .or. arr.eq.'Y') then
               call elines (wly,y,2,npy)
            elseif (arr.eq.'z' .or. arr.eq.'Z') then
               call elines (wlz,z,3,npz)
            else
                   go to 4023
            endif
 
c*****eq = calculate equivalent widths
      elseif (command .eq. 'eq') then
            call width
            splitd = .false.
            go to 10
 
c*****fc = flatten the continuum in regular wavelenth intervals
      elseif (command .eq. 'fc') then
            call findcont (nogo,splitd)
            go to 10

c*****fl = flatten the continuum via a file of designated continuum regions
      elseif (command .eq. 'fl') then
            call flatcont (nogo,splitd)
            go to 10

c*****ft = fourier transform smoothing of the spectrum
      elseif (command .eq. 'ft') then
           call fts (nogo,1)
           if (.not.nogo) call plotxy (1,1,wlx,x,npx,1)
           splitd = .false.
           go to 10
 
c*****fz = force the lower y-limit to be zero
      elseif (command .eq. 'fz') then
            down = 0.
            if (splitd) then
                call splitds (wlx,x,npx,1)
            else  
                call plotxy (1,1,wlx,x,npx,1)
            endif
            go to 10
                                                    
c*****gl = remove bad points (glitches)
      elseif (command .eq. 'gl') then
4016        call which (arr)
            if (arr.eq.'x' .or. arr.eq.'X') then
               call glitch (x,npx)
            elseif (arr.eq.'y' .or. arr.eq.'Y') then
               call glitch (y,npy)
            elseif (arr.eq.'z' .or. arr.eq.'Z') then
               call glitch (z,npz)
            else
                   go to 4016
            endif
            go to 10
 
c*****hc = make a hardcopy plot
      elseif (command .eq. 'hc') then
            call paperpl
            go to 10
 
c*****hd = display header from a disk FITS file
      elseif (command .eq. 'hd') then
            call dishead
            go to 10
 
c*****he = display the help file for SPECTRE commands
      elseif (command .eq. 'he') then
            call printh ('comhelp')
            go to 10
 
c*****lg = base10 logarithm of the points
      elseif (command .eq. 'lg') then
            mode = 0
            go to 4024
 
c*****li = make a Gaussian line
      elseif (command .eq. 'li') then
            call mkline
            splitd = .false.
            go to 10

c*****ls = list the contents of the named directory
      elseif (command .eq. 'ls') then
            call fillist
            go to 10
 
c*****me = merge the x- and y-arrays of data on a wavelength scale
      elseif (command .eq. 'me') then
            call merge
            call screenstat (1)
            go to 10
 
c*****mo = move one spectrum array to another
      elseif (command .eq. 'mo') then
4012        call which (arr1)
            if (arr1.ne.'x' .and. arr1.ne.'X' .and. arr1.ne.'y'
     .          .and. arr1.ne.'Y' .and. arr1.ne.'z' .and.
     .          arr1.ne.'Z') go to 4012
4013        message = 'MOVE IT TO WHERE (x, y, or z)?: '
            nchars = 32
            call getasci (nchars)
            arr2 = array(1:1)
            if (arr2.ne.'x' .and. arr2.ne.'X' .and. arr2.ne.'y'
     .          .and. arr2.ne.'Y' .and. arr2.ne.'z' .and.
     .          arr2.ne.'Z') go to 4013
            if (arr1.eq.'x' .or. arr1.eq.'X') then            
               if (arr2.eq.'y' .or. arr2.eq.'Y') then
                  call move (x,y,wlx,wly,dispx,dispy,
     .                       npx,npy,xobj,yobj,xkfnam,ykfnam,
     .                       xfname,yfname,xary,yary,xfile,yfile)
                  call screenstat (1)
               else
                  call move (x,z,wlx,wlz,dispx,dispz,
     .                       npx,npz,xobj,zobj,xkfnam,zkfnam,
     .                       xfname,zfname,xary,zary,xfile,zfile)
                  call screenstat (1)
               endif
            elseif (arr1.eq.'y' .or. arr1.eq.'Y') then            
               if (arr2.eq.'z' .or. arr2.eq.'Z') then
                  call move (y,z,wly,wlz,dispy,dispz,
     .                       npy,npz,yobj,zobj,ykfnam,zkfnam,
     .                       yfname,zfname,yary,zary,yfile,zfile)
                  call screenstat (1)
               else
                  call move (y,x,wly,wlx,dispy,dispx,
     .                       npy,npx,yobj,xobj,ykfnam,xkfnam,
     .                       yfname,xfname,yary,xary,yfile,xfile)
                  call minimax (x,xmin,xmax,npx)
                  up = 1.12*xmax
                  down = 0.0
                  xleft = wlx(1)
                  right = wlx(npx)
                  call labset (1)
                  call screenstat (1)
               endif
            elseif (arr2.eq.'y' .or. arr2.eq.'Y') then
                  call move (z,y,wlz,wly,dispz,dispy,
     .                       npz,npy,zobj,yobj,zkfnam,ykfnam,
     .                       zfname,yfname,zary,yary,zfile,yfile)
                  call screenstat (1)
            else 
                  call move (z,x,wlz,wlx,dispz,dispx,
     .                       npz,npx,zobj,xobj,zkfnam,xkfnam,
     .                       zfname,xfname,zary,xary,zfile,xfile)
                  call minimax (x,xmin,xmax,npx)
                  up = 1.12*xmax
                  down = 0.0
                  xleft = wlx(1)
                  right = wlx(npx)
                  call labset (1)
                  call screenstat (1)
            endif
            go to 10
 
c*****mu = multiplication by a constant
      elseif (command .eq. 'mu') then
4027        call which (arr)
            if (arr.eq.'x' .or. arr.eq.'X') then
               call const (2,x,npx)
               call minimax (x,ptslo,ptshi,npx)
               xmin = ptslo
               xmax = ptshi
               down = amin1(xmin,ptslo,0.)
               up = 1.12*amax1(xmax,ptshi)
               call plotxy (1,1,wlx,x,npx,1)
            elseif (arr.eq.'y' .or. arr.eq.'Y') then
               call const (2,y,npy)
               call minimax (y,ptslo,ptshi,npy)
               down = amin1(xmin,ptslo,0.)
               up = 1.12*amax1(xmax,ptshi)
               call plotxy (1,1,wly,y,npy,2)
            elseif (arr.eq.'z' .or. arr.eq.'Z') then
               call const (2,z,npz)
               call minimax (z,ptslo,ptshi,npz)
               down = amin1(xmin,ptslo,0.)
               up = 1.12*amax1(xmax,ptshi)
               call plotxy (1,1,wlz,z,npz,3)
            else
               go to 4027
            endif
            go to 10
 
c*****no = calculate the signal-to-noise of a section of spectrum
      elseif (command .eq. 'no') then
            call noise
            splitd = .false.
            go to 10

c*****pi = display the channel/wavelength number pointed by cursor
      elseif (command .eq. 'pi') then
c           if (splitd) go to 9995
            call pick (1)
            splitd = .false.
            go to 10
 
c*****pl = REPEATEDLY display the channel/wavelength number pointed by cursor
      elseif (command .eq. 'pl') then
c           if (splitd) go to 9995
            call pick (2)
            splitd = .false.
            go to 10
 
c*****po = point at a named abscissa position
      elseif (command .eq. 'po') then
            if (splitd) go to 9995
            call positon
            splitd = .false.
            go to 10
 
c*****pr = display the data values on the information screen
      elseif (command .eq. 'pr') then
4020        call which (arr)
            if (arr.eq.'x' .or. arr.eq.'X') then
                   call prdata (wlx,x,npx)
            else if (arr.eq.'y' .or. arr.eq.'Y') then
                   call prdata (wly,y,npy)
            else if (arr.eq.'z' .or. arr.eq.'Z') then
                   call prdata (wlz,z,npz)
            else
                   go to 4020
            endif
            go to 10

c*****ps = compute the power spectrum of the x-array
      elseif (command .eq. 'ps') then
           call fts (nogo,2)
           if (.not.nogo) call plotxy (1,1,wlx,x,npx,1)
           splitd = .false.
           go to 10
 
c*****qu = quit the program
      elseif (command.eq.'q ' .or. command.eq.'qu') then
            call quit
 
c*****r0 = remove zeros from spectra
      elseif (command .eq. 'r0') then
4019        call which (arr)
            if (arr.eq.'x' .or. arr.eq.'X') then
               call remzero (x,npx)
            elseif (arr .eq.'y'.or. arr.eq.'Y') then
               call remzero (y,npy)
            elseif (arr.eq.'z' .or. arr.eq.'Z') then
               call remzero (z,npz)
            else
                   go to 4019
            endif
            go to 10

c*****rb = re-bin the spectra at desired pixel/wavelength steps
      elseif (command .eq. 'rb') then
4031        call which (arr)
            if (arr.eq.'x' .or. arr.eq.'X') then
                   call rebin (wlx,x,dispx,npx)
            else if (arr.eq.'y' .or. arr.eq.'Y') then
                   call rebin (wly,y,dispy,npy)
            else if (arr.eq.'z' .or. arr.eq.'Z') then
                   call rebin (wlz,z,dispz,npz)
            else
                   go to 4031
            endif
            call screenstat (1)
            go to 10

c*****rd = read a spectrum file in the FITS format
      elseif (command .eq. 'rd') then
4033        call which (arr)
            if (arr.eq.'x'.or.arr.eq.'X') then
                  call reed (x,wlx,dispx,xfname,xkfnam,xobj,npx,
     .                       xmin,xmax,vovercx,xary,xfile)
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
                 call screenstat (1)
            else if (arr.eq.'y'.or.arr.eq.'Y') then
                 call reed (y,wly,dispy,yfname,ykfnam,yobj,npy,
     .                      dummy1,dummy2,vovercy,yary,yfile)
                 call screenstat (1)
            else if (arr.eq.'z'.or.arr.eq.'Z') then
                 call reed (z,wlz,dispz,zfname,zkfnam,zobj,npz,
     .                      dummy1,dummy2,vovercz,zary,zfile)
                 call screenstat (1)
            else
                  go to 4033
            endif
            go to 10
 
c*****re = replace the spectrum array in the old file name
      elseif (command .eq. 're') then
4007        call which (arr)
            if (arr.eq.'x' .or. arr.eq.'X') then
               if (xfile .eq. 'text') then
                  call stext (0,wlx,x,npx,xfname,xobj)
               else
                  call save (0,x,npx,dispx,xfname,xary,xobj)
               endif
            else if (arr.eq.'y'.or.arr.eq.'Y') then
               if (yfile .eq. 'text') then
                  call stext (0,wly,y,npy,yfname,yobj)
               else 
                  call save (0,y,npy,dispy,yfname,yary,yobj)
               endif
            else if (arr.eq.'z'.or.arr.eq.'Z') then
               if (zfile .eq. 'text') then
                  call stext (0,wlz,z,npz,zfname,zobj)
               else
                  call save (0,z,npz,dispz,zfname,zary,zobj)
               endif
            else
               go to 4007
            endif
               call screenstat (1)
            go to 10
 
c*****rm = remove telluric lines
      elseif (command .eq. 'rm') then
            call remove
            splitd = .false.
            go to 10
 
c*****rn = renormalize a spectrum.  The default normalization is 1.0
      elseif (command .eq. 'rn') then
4030        call which (arr)
            if (arr.eq.'x' .or. arr.eq.'X') then
                 call rnorm (x,wlx,npx,1)
            elseif (arr.eq.'y' .or. arr.eq.'Y') then
                 call rnorm (y,wly,npy,2)
            elseif (arr.eq.'z' .or. arr.eq.'Z') then
                 call rnorm (z,wlz,npz,3)
            else
                   go to 4030
            endif
            splitd = .false.
            go to 10
 
c*****rt = read a MONGO-style spectrum file
      elseif (command .eq. 'rt') then
4010        call which (arr)
            if (arr.eq.'x'.or.arr.eq.'X') then
                  call rtext (x,wlx,dispx,xfname,xkfnam,xobj,npx,
     .                       xmin,xmax,vovercx,xary,xfile)
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
                 call screenstat (1)
                 call plotxy (1,1,wlx,x,npx,1)
            else if (arr.eq.'y'.or.arr.eq.'Y') then
                 call rtext (y,wly,dispy,yfname,ykfnam,yobj,npy,
     .                      dummy,dummy,vovercy,yary,yfile)
                 call screenstat (1)
            else if (arr.eq.'z'.or.arr.eq.'Z') then
                 call rtext (z,wlz,dispz,zfname,zkfnam,zobj,npz,
     .                      dummy,dummy,vovercz,zary,zfile)
                 call screenstat (1)
            else
                  go to 4010
            endif
            go to 10

c*****sa = save spectrum aray to a new file name
      elseif (command .eq. 'sa') then
4011        call which (arr)
            if (arr.eq.'x' .or. arr.eq.'X') then
               if (xfile .eq. 'text') then
                  call stext (1,wlx,x,npx,xfname,xobj)
               else
                  call save (1,x,npx,dispx,xfname,xary,xobj)
               endif
            else if (arr.eq.'y'.or.arr.eq.'Y') then
               if (yfile .eq. 'text') then
                  call stext (1,wly,y,npy,yfname,yobj)
               else
                  call save (1,y,npy,dispy,yfname,yary,yobj)
               endif
            else if (arr.eq.'z'.or.arr.eq.'Z') then
               if (zfile .eq. 'text') then
                  call stext (1,wlz,z,npz,zfname,zobj)
               else
                  call save (1,z,npz,dispz,zfname,zary,zobj)
               endif
            else
               go to 4011
            endif
               call screenstat (1)
            go to 10
 
c*****sc = set the continuum, marked by the user, into the y-array
      elseif (command .eq. 'sc') then
            call contin ('sc',nogo,splitd)
            go to 10

c*****si = creation of a sine wave in the y-array
      elseif (command .eq. 'si') then
            call sine
            go to 10
 
c*****sp = split the display into 2 parts
      elseif (command .eq. 'sp') then
            call splitds (wlx,x,npx,1)
            splitd = .true.
            go to 10
 
c*****st = redisplay the spectrum status part of the information screen
      elseif (command .eq. 'st') then
            call screenstat(1)
            go to 10
 
c*****su = subtraction of the x-array and the y-array
      elseif (command .eq. 'su') then
            call charith (2)
            up = 1.12*xmax
            down = 0.
            xleft = wlx(1)
            right = wlx(npx)
            call plotxy (1,1,wlx,x,npx,1)
            go to 10
 
c*****tc = computation of the optical depths of the points
      elseif (command .eq. 'tc') then
4029        call which (arr)
            if (arr.eq.'x' .or. arr.eq.'X') then
               call taucalc (x,wlx,npx,1)
            elseif (arr.eq.'y' .or. arr.eq.'Y') then
               call taucalc (y,wly,npy,2)
            elseif (arr.eq.'z' .or. arr.eq.'Z') then
               call taucalc (z,wlz,npz,3)
            else
                   go to 4029
            endif
            go to 10

c*****ti = give a new OBJECT keyword to the spectrum
      elseif (command .eq. 'ti') then
4018        call which (arr)
            if (arr.ne.'x' .and. arr.ne.'X' .and. arr.ne.'y'
     .          .and. arr.ne.'Y' .and. arr.ne.'z' .and.
     .          arr.ne.'Z') go to 4018
            call title (arr)
            call screenstat (1)
            go to 10
 
c*****tl = correct an overall spectrum flux tilt with a linear correction
      elseif (command .eq. 'tl') then
4032        call which (arr)
            if (arr.eq.'x' .or. arr.eq.'X') then
               call tilt (x,wlx,npx,1,splitd)
            elseif (arr.eq.'y' .or. arr.eq.'Y') then
               call tilt (y,wly,npy,2,splitd)
            elseif (arr.eq.'z' .or. arr.eq.'Z') then
               call tilt (z,wlz,npz,3,splitd)
            else
                   go to 4032
            endif
            splitd = .false.
            go to 10

c*****vn = cancel a velocity shift that currently applies to an array
      elseif (command .eq. 'vn') then
4015        call which (arr)
            if (arr.eq.'x' .or. arr.eq.'X') then
               call velocity ('no ',vovercx,dispx,wlx,npx)
            elseif (arr.eq.'y' .or. arr.eq.'Y') then
               call velocity ('no ',vovercy,dispy,wly,npy)
            elseif (arr.eq.'z' .or. arr.eq.'Z') then
               call velocity ('no ',vovercz,dispz,wlz,npz)
            else
                   go to 4015
            endif
            go to 10
 
c*****vs = declare a radial velocity or wavelength shift
      elseif (command .eq. 'vs') then
4034        call which (arr)
            if (arr.eq.'x' .or. arr.eq.'X') then
               call velset (vovercx)
            elseif (arr.eq.'y' .or. arr.eq.'Y') then
               call velset (vovercy)
            elseif (arr.eq.'z' .or. arr.eq.'Z') then
               call velset (vovercy)
            else
                   go to 4034
            endif
            go to 10
 
c*****vy = apply a velocity shift that has been read in the FITS header
      elseif (command .eq. 'vy') then
4017        call which (arr)
            if (arr.eq.'x' .or. arr.eq.'X') then
               call velocity ('yes',vovercx,dispx,wlx,npx)
            elseif (arr.eq.'y' .or. arr.eq.'Y') then
               call velocity ('yes',vovercy,dispy,wly,npy)
            elseif (arr.eq.'z' .or. arr.eq.'Z') then
               call velocity ('yes',vovercz,dispz,wlz,npz)
            else
                   go to 4017
            endif
            go to 10
 
c*****wv = compute a water vapor spectrum near the Na D lines
      elseif (command .eq. 'wv') then
            call h2omake
            call screenstat (1)
            go to 10

c*****xp = expand the mumber of points by a named factor
      elseif (command .eq. 'xp') then
            call interp
            call screenstat (1)
            go to 10

c*****xy = exchange the x and y spectrum arrays
      elseif (command .eq. 'xy') then
            call flip (x,y,wlx,wly,dispx,dispy,npx,npy,xobj,yobj, 
     .                 xkfnam,yfkfnam,xfname,yfname,xary,yary,
     .                 xfile,yfile) 
            call minimax (x,xmin,xmax,npx)
            up = 1.12*xmax
            down = 0.0
            xleft = wlx(1)
            right = wlx(npx)
            call labset (1)
            call screenstat (1)
            go to 10
 
c*****xz = exchange the x and z spectrum arrays
      elseif (command .eq. 'xz') then
            call flip (x,z,wlx,wlz,dispx,dispz,npx,npz,xobj,zobj, 
     .                 xkfnam,zkfnam,xfname,zfname,xary,zary,
     .                 xfile,zfile) 
            call minimax (x,xmin,xmax,npx)
            up = 1.12*xmax
            down = 0.0
            xleft = wlx(1)
            right = wlx(npx)
            call labset (1)
            call screenstat (1)
            go to 10
 
c*****yz = exchange the y and z spectrum arrays
      elseif (command .eq. 'yz') then
            call flip (y,z,wly,wlz,dispy,dispz,npy,npz,yobj,zobj, 
     .                 ykfnam,zkfnam,yfname,zfname,yary,zary,
     .                 yfile,zfile) 
            call screenstat (1)
            go to 10
 
c*****ze = zero array ends
      elseif (command .eq. 'ze') then
4008        call which (arr)
            if (arr.eq.'x' .or. arr.eq.'X') then
               call zot (nogo,command,wlx,x,npx,dispx,1)
            elseif (arr.eq.'y' .or. arr.eq.'Y') then
               call zot (nogo,command,wly,y,npy,dispy,2)
            elseif (arr.eq.'z' .or. arr.eq.'Z') then
               call zot (nogo,command,wlz,z,npz,dispz,3)
            else
               go to 4008
            endif
            go to 10
 
c*****zi = zoom in for a more detailed look at a spectrum
      elseif (command .eq. 'zi') then
            call zoom ('in ')
            go to 10
 
c*****zo = zoom out for a less detailed look at a spectrum
      elseif (command .eq. 'zo') then
            call zoom ('out')
            go to 10
 
c*****4p = four point normalization for Reticon data
      elseif (command .eq. '4p') then
4006        call which (arr)
            if (arr.eq.'x' .or. arr.eq.'X') then
               call fourpn (mode,1,wlx,x,npx)
            elseif (arr.eq.'y' .or. arr.eq.'Y') then
               call fourpn (mode,1,wly,y,npy)
            elseif (arr.eq.'z' .or. arr.eq.'Z') then
               call fourpn (mode,1,wlz,z,npz)
            else
               go to 4006
            endif
            splitd = .false.
            call screenstat (1)
            go to 10
 
c*****8p = eight point normalization for Reticon data
      elseif (command .eq. '8p') then
            mode = 2
            go to 4006
 
c*****4n = four by four point normalizatio for Reticon data
      elseif (command .eq. '4n') then
            mode = 3
            go to 4006

c*****?? = get help for a particular command
      elseif (command .eq. '??') then
            call comtalk
            go to 10

c*****here are the error messages
      else 
          write (errmess,9998) command
9998      format('THIS COMMAND WAS NOT UNDERSTOOD: ',a2)
          nchars = 34
          call puterr (nchars)
          go to 10
      endif

9996  write (errmess,9997) command
9997  format('THIS COMMAND HAS NOT BEEN IMPLEMENTED: ',a2)
      nchars = 41
      call puterr (nchars)
      go to 10

9995  errmess = 'INVALID FUNCTION ON SPLIT SCREENS! '
      nchars = 35
      call puterr (nchars)
      go to 10

      end


