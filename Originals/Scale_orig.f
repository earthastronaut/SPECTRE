      subroutine scale
c*****this routine does the conversion from channel space to wavelength
c     space.
 
      include 'Dataval.com'
      include 'Datachr.com'
      include 'Plotval.com'
      include 'Chars.com'
      real*8 wavl(25),chan(25),channew(1),wavlnew(1),xpoly,
     .                 wavlold
      integer emflag
      data npoly/0/
 
      emflag = 0
      do 1215 i=1,25
1215         wavl(i) = 0.0

c*****here is the section for user commands
20    prompt = 'SCALE :   '
      call getcom 
      if (command .eq. 'au') go to 115      
      if (command .eq. 'em') go to 615
      if (command .eq. 'he') go to 415     
      if (command .eq. 'ma') go to 15      
      if (command .eq. 'px') go to 1015      
      if (command .eq. 'qu' .or. command(1:1).eq.'q') return
      if (command .eq. 'rv') go to 715     
      if (command .eq. 'sa') go to 215     
      if (command .eq. 'sh') go to 915
      if (command .eq. 'so') go to 815     
      write (errmess,1020) command
1020  format (a2,' IS AN UNKNOWN SCALE COMMAND; TRY AGAIN! ')
      nchars = 43
      call puterr (nchars)
      go to 20

c*****here the dispersion curve is computed manually from cursor-marked lines 
15    call sclbgn 
      if (array(1:1) .eq. 'n') go to 20
      message = 'MARK LINES AND NAME THE WAVELENGTHS '
      nchars = 36
      call putasci (nchars)
      j = 1
40    izoom = 0
      call getline (emflag,key,izoom,chan(j),wavl(j))
      if (char(key) .ne. 'q') call wavelst (j,izoom,chan,wavl)
      if (char(key) .eq. 'q' .and. j.ge.3) then
         j = j - 1
         call solve (chan,wavl,j,npoly)
         go to 20
      elseif (j .eq. 25) then
         call solve (chan,wavl,j,npoly)
         go to 20
      else
         j = j + 1
         go to 40
      endif

c*****compute a dispersion curve from old constants and one new line
115   call sclbgn
      if (array(1:1) .eq. 'n') go to 20
      message = 'MARK ONE LINE ONLY WITH THE CURSOR '
      nchars = 35
      call putasci (nchars)
      j = 1
      izoom = 0
      call getline (emflag,key,izoom,channew(1),wavlnew(1))
      call wavelst (j,izoom,channew(1),wavlnew(1))
      call readcf (dispx,chan,wavl,j)
      do 120 i=1,npx
120      wlx(i) = wave(real(i),npx,dispx)
      call estim (sngl(wavlnew(1)),sngl(wavlnew(1)),dispx,
     .            wlx,npx,ipt,ipt)
      ipt = int(sngl(channew(1)))
      delta = wavlnew(1) - dble(wlx(ipt)) + (channew(1) - dble(ipt))*
     .        dble(wlx(ipt+1)-wlx(ipt))
      dispx(1) = dispx(1) + delta
      do 130 i=1,npx
130      wlx(i) = wlx(i) + delta
      xleft = wlx(1)
      right = wlx(npx)
      do 140 i=1,j
         call estim (sngl(wavl(i)),sngl(wavl(i)),dispx,wlx,
     .              npx,ipt,ipt)
         call findmin (real(ipt),x,npx,pmin,qmin,emflag)
         chan(i) = dble(pmin)
         if (chan(i) .ne. -9999.) then
            call wavelst (i,izoom,chan,wavl)
         endif
140   continue
      do 142 i=1,j
         if (chan(i) .eq. -9999.) then
            if (i .eq. j) then 
               j = j - 1
               go to 142
            else
               do 144 k=i,j-1
               chan(k) = chan(k+1)
144            wavl(k) = wavl(k+1)
               j = j - 1
            endif
         endif
142      continue
      call solve (chan,wavl,j,npoly)
      go to 20

c*****adopt the coefficients from another star's dispersion solution
215   call sclbgn
      if (array(1:1) .eq. 'n') go to 20
      call readcf (dispx,chan,wavl,j)
      do 220 i=1,npx
220      wlx(i) = wave(real(i),npx,dispx)
      xleft = wlx(1)
      right = wlx(npx)
      call labset (1)
      call plotxy (1,1,wlx,x,npx,1)
      go to 20

c*****here a list of SCALE commands are displayed on the screen
415   call printh ('scahelp')
      go to 20
 
c*****here a flag for emission lines is set
615   emflag = 1
      go to 20
 
c*****here the points are reversed
715   npt = npx/2
      do 720 i=1,npt
         xtemp = x(i)
         x(i) = x(npx+1-i)
720      x(npx+1-i) = xtemp
      call plotxy (1,1,wlx,x,npx,1)
      go to 20
 
c*****here the user can manually set the order of the polynomial to be fit
815   message = 'GIVE THE DESIRED POLYNOMIAL ORDER TO USE: '
      nchars = 42
      call getnum (nchars,xpoly)
      npoly = int(sngl(xpoly))
      go to 20
 
c*****here the user can use an old dispersion solution but shift in
c     wavelength
915   call sclbgn
      if (array(1:1) .eq. 'n') go to 20
      message = 'MARK ONE LINE WITH THE CURSOR; DECLARE ITS WAVELENGTH'
      nchars = 53
      call putasci (nchars)
      j = 1
      izoom = 0
      call getline (emflag,key,izoom,channew(1),wavlnew(1))
      call wavelst (j,izoom,channew(1),wavlnew(1))
      call readcf (dispx,chan,wavl,j)
      chnew = sngl(channew(1))
      wavlold = wave(chnew,npx,dispx)
      delta = wavlnew(1) - wavlold
      dispx(1) = dispx(1) + delta
      do 920 i=1,npx
920      wlx(i) = wave(real(i),npx,dispx)
      xleft = wlx(1)
      right = wlx(npx)
      call labset (1)
      call plotxy (1,1,wlx,x,npx,1)
      go to 20

c*****kill the dispersion solution entirely; go back to a pixel scale
1015  call sclbgn
      dispx(1) = 1.
      dispx(2) = 1.
      go to 20

      end


 
 







