      subroutine glitch (data,ndata)
c*****counts got from linear interpolation between nblu-1 and nred+1
c     are substituted for counts in nblu thru nred to remove glitch.

      include 'Chars.com'
      real*4 data(131072)
      real*8 xnum
      character charstat*7
      character yesno*1
      data nblu,nred/0,0/
 
      write (message,1001) nblu,nred
1001  format ('BAD DATA POINTS GO FROM ',i3,' to ',i3,
     .        '; OK? ([y],n,f) ')
      nchars = 52
      call getasci (nchars)
      yesno = array(1:1)
      if (yesno.eq.'y' .or. nchars.le.0) go to 100
      if (yesno .eq. 'f') go to 300

1     write (message,1002)
1002  format ('CHANNEL # OF FIRST BAD POINT = ')
      nchars = 31
      call getnum (nchars,xnum)
      if (xnum .eq. -9999.) go to 1
      nblu = int(xnum)
2     write (message,1003)
1003  format ('CHANNEL # OF LAST BAD POINT =  ')
      nchars = 32
      call getnum (nchars,xnum)
      if (xnum .eq. -9999.) go to 2
      nred = int(xnum)

100   if (nblu.eq.1 .or. nred.eq.ndata) go to 102
      slope = (data(nred+1) -data(nblu-1))/float(nred-nblu+2)
      do 101 i = nblu,nred
      xn = i-nblu+1
101   data(i) = data(nblu-1)+xn*slope
      go to 200

102   if (nblu.eq.1) counts = data(nred+1)
      if (nred.eq.ndata) counts = data(nblu-1)
      do 103 i = nblu,nred
103   data(i) = counts
200   if (yesno .eq. 'f') go to 310
      return
 
300   message = 'GIVE THE FILE NAME OF THE BAD POINTS: '
      nchars = 38
      call getasci (nchars)
      charstat = 'old    '
      call dskfil (40,iostat,array(1:nchars),charstat,'sequential',
     .            'formatted  ',0)
      if (iostat .ne. 0) return
310   read (40,3001,end=320) nblu,nred
3001  format (2i5)
      go to 100
320   yesno = 'y'
      close (40)
      go to 200

      end




