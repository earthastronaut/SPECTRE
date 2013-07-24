
      subroutine save (mode,pts,wl,npt,disp,fname,newname,arrayn,object)
c*****save the data and header information to the specified file
c
c                   call save (1,x,npx,dispx,xfname,newfile,xary,xobj)
c
c   added the a pass for the wavelength, into wl
c
c
      include 'Chars.com'
      character fname*80,newname*80,object*20,arrayn*3,newhead*2880
c      character newfile*80
      real*4 pts(10000),wl(10000)
      real*8 disp(9)
      character charstat*7,keyword*8
      byte      int1(2880), onebyte
      integer*4 int4(720)
      equivalence (int1,int4)
      integer*4 iwhen(3)
      double precision bzero, bscale

      ifold = 20
      ifnew = 21

c*****get a new file name, if desired
c     does a replacement of the old file
      if (mode .eq. 0) then
         newname = 'savescratch'

c     this gets the new name from fname and then
c     write out a new file
      elseif (mode .eq. 2) then
c         newname = newfile
         continue
c     This prompts for the name then writes out a new file
      else
         message = 'ENTER THE NEW FILENAME: '
         nchars = 24
         call getasci (nchars)
         nchars = min0(nchars,40)
         newname = array(1:nchars)
      endif
 

c*****open the old and new files
      if (mode .eq. 0) then
         charstat = 'unknown'
         call dskfil (ifnew,iostat,newname,charstat,'direct    ',
     .                'unformatted',2880)
         if (iostat .ne. 0) go to 1003

      else
         charstat = 'new    '
         call dskfil (ifnew,iostat,newname,charstat,'direct    ',
     .                'unformatted',2880)
         if (iostat .ne. 0) go to 1003
      endif

      charstat = 'old    '
      if (fname(1:9) .ne. '(no file)') then
         call dskfil (ifold,iostat,fname,charstat,'direct    ',
     .                'unformatted',2880)
         if (iostat .ne. 0) go to 1003
      endif
 
c*****get the data scaling parameters
      call minimax (pts,ptmin,ptmax,npt)
      bscale = dble(ptmax-ptmin)/(2.*dble(2**30))
      bzero = dble(ptmin+0.5*(ptmax-ptmin))
 
c*****write out the first part of the new header
      write (newhead(1:80),1031)
1031  format('SIMPLE  = ',19x,'T /',48x)
      write (newhead(81:160),1032)
1032  format('BITPIX  = ',18x,'32 /',48x)
      write (newhead(161:240),1033)
1033  format('NAXIS   = ',19x,'1 /',48x)
      write (newhead(241:320),1034) npt
1034  format('NAXIS1  = ',15x,i5,' /',48x)
      write (newhead(321:400),1035) bzero
1035  format('BZERO   = ',1pd20.10,' /',48x)
      write (newhead(401:480),1036) bscale
1036  format('BSCALE  = ',1pd20.10,' /',48x)
      if (fname(1:9) .eq. '(no file)') then
         write (newhead(481:560),1037) object(1:18)
         write (newhead(561:640),1040)
     .         (iwhen(ii),ii=1,3),(disp(ii),ii=1,3)
         write (newhead(641:720),1041)
     .         (iwhen(ii),ii=1,3),(disp(ii),ii=4,6)
         write (newhead(721:800),1042)
     .         (iwhen(ii),ii=1,3),(disp(ii),ii=7,9)
         write (newhead(801:880),1043)
1043     format ('END',77(' '))
         do i=12,36
            jchar = 80*(i-i)
            write (newhead(jchar+1:jchar+80),1039)
         enddo
         jrec = 1
          write (unit=ifnew,rec=jrec,err=1004,iostat=ierr) 
     .           newhead
         go to 101
      endif
 
c*****add the rest of the old header information to the new header 
      irec = 1
      jrec = 1
      jline = 7
15    read (unit=ifold,rec=irec,err=1002,iostat=ierr) head(1:2880)
      do 16 iline=1,36
         ichar = 80*(iline-1)
         jchar = 80*(jline-1)
         keyword= head(ichar+1:ichar+6)
         if     (keyword      .eq. 'SIMPLE' .or.
     .           keyword      .eq. 'BITPIX' .or.
     .           keyword      .eq. 'NAXIS ' .or.
     .           keyword      .eq. 'NAXIS1' .or.
     .           keyword      .eq. 'NAXIS2' .or.
     .           keyword      .eq. 'BZERO ' .or.
     .           keyword      .eq. 'BSCALE' .or.
     .           keyword      .eq. '      ' .or.
     .           keyword(1:3) .eq. 'WAT'    .or.
     .           keyword(1:5) .eq. 'APNUM') then
            go to 16
         elseif (keyword .eq. 'OBJECT') then 
            if (object(1:5) .eq. '     ') then
               message = 'PLEASE GIVE AN OBJECT NAME! '
               nchars = 28
               call getasci (nchars)
               nchars = min0(nchars,18)
               object(1:nchars) = array(1:nchars)
            endif
            write (newhead(jchar+1:jchar+80),1037) object(1:18)
1037        format('OBJECT  = ',1h',a18,1h',' /',48x)
            go to 34
         elseif (keyword .eq. 'END   ') then 
            call idate (iwhen)
            write (newhead(jchar+1:jchar+80),1040)
     .            (iwhen(ii),ii=1,3),(disp(ii),ii=1,3)
1040        format ('HISTORY ',i2,':',i2,':',i4,' D1,2,3:',
     .             1p3d18.11)
            if (jline .eq. 36) then
               write (unit=ifnew,rec=jrec,err=1004,iostat=ierr) 
     .               newhead
               jrec = jrec + 1
               jline = 0
               jchar = 0
            endif
            jline = jline + 1
            jchar = 80*(jline-1)
            write (newhead(jchar+1:jchar+80),1041)
     .            (iwhen(ii),ii=1,3),(disp(ii),ii=4,6)
1041        format ('HISTORY ',i2,':',i2,':',i4,' D4,5,6:',
     .             1p3d18.11)
            if (jline .eq. 36) then
               write (unit=ifnew,rec=jrec,err=1004,iostat=ierr) 
     .               newhead
               jrec = jrec + 1
               jline = 0
               jchar = 0
            endif
            jline = jline + 1
            jchar = 80*(jline-1)
            write (newhead(jchar+1:jchar+80),1042)
     .            (iwhen(ii),ii=1,3),(disp(ii),ii=7,9)
1042        format ('HISTORY ',i2,':',i2,':',i4,' D7,8,9:',
     .             1p3d18.11)
            if (jline .eq. 36) then
               write (unit=ifnew,rec=jrec,err=1004,iostat=ierr) 
     .               newhead
               jrec = jrec + 1
               jline = 0
               jchar = 0
            endif
            jline = jline + 1
            jchar = 80*(jline-1)
            write (newhead(jchar+1:jchar+80),1038)
     .             head(ichar+1:ichar+80) 
1038        format (a80)
            if (jline .eq. 36) go to 39
            do 38 kline=jline+1,36
               jchar = 80*(kline-1)
38             write (newhead(jchar+1:jchar+80),1039)
1039           format (80(' '))
39          write (unit=ifnew,rec=jrec,err=1004,iostat=ierr) newhead
            go to 37
         else
            write (newhead(jchar+1:jchar+80),1038)
     .             head(ichar+1:ichar+80) 
         endif
34       jline = jline + 1
         if (jline .gt. 36) then
            write (unit=ifnew,rec=jrec,err=1004,iostat=ierr) newhead
            jrec = jrec + 1
            jline = 1
         endif
16       continue
      irec = irec + 1
      go to 15

c*****close the old file after getting the header stuff from it
37    close (ifold)
 

      call minimax (pts,ptmin,ptmax,npt)
c      write (*,*) "----------------"
c      write (*,*) "<",disp(1),"><",disp(2),">"
c      write (*,*) "<",disp(3),"><",disp(4),">"
c      write (*,*) "<",disp(5),"><",disp(6),">"
c      write (*,*) "<",disp(7),"><",disp(8),">"
c      write (*,*) "<",disp(9),">"
c      write (*,*) "----------------"

c      write (*,*) "npt,bzero,bscale"
c      write (*,*) npt,bzero,bscale
c      write (*,*) "----------------" 
c      write (*,*) "ptmin,ptmax"
c      write (*,*) ptmin,ptmax

c      write (*,*) "----------------"
c      pause

c*****now save the data
101   num = npt
      mrec = (num+719)/720
           open (55,file='mytemp')
      do i=1,mrec
         ndat = min0(720,num)
         istart = 720*(i-1)
         do j=1,ndat
            int4(j) = int((pts(istart+j)-bzero)/bscale)
         enddo
         if (ndat .lt. 720) then
            do j=ndat+1,720
               int4(j) = 0
            enddo
         endif
         do j=4,2880,4
            onebyte = int1(j)
            int1(j) = int1(j-3)
            int1(j-3) = onebyte
            onebyte = int1(j-1)
            int1(j-1) = int1(j-2)
            int1(j-2) = onebyte
         enddo
         irec = i+jrec
         write (unit=ifnew,rec=irec,err=1004,iostat=ierr) 
     .         (int1(j),j=1,2880)
                write (55,1111) int1
1111            format (20i4)
         num = num - 720
      enddo

c*****for a replacement operation, write the records from the scratch file
c     to the old file name
      if (mode .eq. 0) then
         charstat = 'old    '
         call dskfil (ifold,iostat,fname,charstat,'direct    ',
     .                'unformatted',2880)
         if (iostat .ne. 0) go to 1003
         nrec = jrec + mrec
         do 80 irec=1,nrec
            read (unit=ifnew,rec=irec,err=1002,iostat=ierr) int4
80          write (unit=ifold,rec=irec,err=1004,iostat=ierr) int4
         close (ifnew,status='delete')
      else
         fname = newname
         close (ifnew)
      endif
      close (ifold)
      return
 
c*****write error messages
1002  write (errmess,1005) ierr
1005  format('ERROR ',i3,' IN READ DURING A SAVE OPERATION ')
      nchars = 42
      call puterr (nchars)
      go to 1003
1004  write (errmess,1006)
1006  format('ERROR IN WRITE DURING A SAVE OPERATION ')
      nchars = 39
      call puterr (nchars)

1003  mode = -1
      return

      end




