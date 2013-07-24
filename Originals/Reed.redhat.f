      subroutine reed (pts,wl,disp,fname,obsname,object,npt,
     .                 xxmin,xxmax,voverc,arrayn,filestyle)
c*****read the spectrum file into the program and parse the header 
c     for information

      include 'Plotval.com' 
      include 'Chars.com'
      include 'Scrnval.com'
      character fname*80, obsname*20, object*20, arrayn*3
      character fname1*80, obsnam1*20, object1*20, arrayn1*3
      character charstat*7, filestyle*4
      character*2 orderchar
      character*1 longspec
      real*4 pts(10000), wl(10000), ptxx, wlxx
      real*8 disp(9), disp1(9)
      integer apline, waveiraf, oned
      integer apnum, specnum, dcflag, order, polytype
      byte      int1(2880), onebyte
      integer*2 int2(1440), twobyte
      integer*4 int4(720)
      equivalence (int1,int2,int4,real4)
      real*4 real4(720)
      real*8 wavlo, wavhi
      real*8 bzero, bscale, xnum, ltv1, ltm11, crval1, crpix1, cdelt1
      real*8 cd11
      real*8 w0, wpc
      character waveinfo*300



c*****get ready to open a spectrum file
      fname1 = fname
      obsnam1 = obsname
      object1 = object
      arrayn1 = arrayn
      npt1 = npt
      do i=1,9
         disp1(i) = disp(i)
         disp(i) = 0.
      enddo

      waveinfo = " "

c*****Get the file name and open the file
      call blank
      message = 'ENTER THE FILENAME: '
      nchars =  20
      call getasci (nchars)
      charstat = 'old    '
      iunit = 8
      fname = array(1:80)
      call dskfil (iunit,jostat,fname,charstat,'direct    ',
     .             'unformatted',2880)
      if (jostat .ne. 0) then
         fname = fname1
         close (iunit)
         return
      endif



c*****clear some parameters
      longspec = 'n'
      ibits = -1
      ifloat = 0
      bzero = 0.
      bscale = 1.
      naxis = 0
      naxis1 = 0
      naxis2 = 0
      nech1 = 0
      nech2 = 0
      apline = 0
      waveiraf = 0
      oned = 1
      ltv1 = 0.
      ltm11 = 1.
      crval1 = 0.
      crpix1 = 0.
      cdelt1 = 0.
      cd11 = 0.
      w0 = 0.
      wpc = 0.



c*****read the entire header into array "head"
      do jrec=1,30
         m = 2880*(jrec-1)
         read (unit=iunit,rec=jrec,err=1002,iostat=ierr) 
     .        head(m+1:m+2880)
         do j=1,36
            k = 80*(j-1)
            if (head(m+k+1:m+k+8) .eq. 'END     ') then
               nlines = (m+k)/80
               nrec = jrec
               go to 103
            endif
         enddo      
      enddo
      write(array,1039) head(k+1:k+58)
1039  format('TOO MANY RECORDS IN HEADER; FILE CANNOT BE READ!')
      nchars = 48
      call puterr (nchars)
      go to 1007



c*****display the first part of the FITS header on the screen
103      write (message,1015)
1015     format(28(' '),'FITS HEADER INFORMATION',28(' '))
         kount = 1
         mlines = min0(nlines,maxline-13)
         do j=1,mlines
            k = 160*(j-1)
            write(array,1050) head(k+1:k+30),head(k+81:k+110)
1050        format(a30,19(' '),a30)
            call prinfo (kount)
            if (errmess(1:9) .eq. 'stopinfo!') go to 191
            kount = kount + 1
         enddo



c*****now get header information of relevance to this program
c*****header information: is this a true FITS file?
191   do 150 j=1,nlines
         k = 80*(j-1)
         if (head(k+1:k+8) .eq. 'SIMPLE  ') then
            if (head(k+30:k+30) .ne. 'T') then
               write(array,1029) head(k+1:k+58)
1029           format('ILLEGAL FILE FORMAT: ',a58)
               nchars = 79
               call puterr (nchars)
               go to 1007
            endif
c*****header information: how many bits per pixel in the data?
         elseif (head(k+1:k+8) .eq. 'BITPIX  ') then
               read (head(k+1:k+80),1025) ibits
1025           format (10x,i20)
               if (ibits .lt. 0) then
                  ifloat = 1
                  ibits = iabs(ibits)
               endif
               if (ibits .eq. 16) then
                  nblock = 1440
               elseif (ibits .eq. 32) then
                  nblock = 720
               else
                  write(array,1026) ibits
1026              format('SORRY: I CANT HANDLE BITPIX=',i4)
                  nchars = 32
                  call puterr (nchars)
                  go to 1007
               endif
c*****header information: how many dimensions in the data?
         elseif (head(k+1:k+8) .eq. 'NAXIS   ') then
               read (head(k+1:k+80),1025) naxis
               write(arrayn,1101) naxis
1101           format(i3)
c*****header information: first dimension (number of points per order)
         elseif (head(k+1:k+8) .eq. 'NAXIS1  ') then
               read (head(k+1:k+80),1025) naxis1
c*****header information: second dimension (number of orders)
         elseif (head(k+1:k+8) .eq. 'NAXIS2  ') then
               read (head(k+1:k+80),1025) naxis2
c*****header information: third dimension (number of different spectra)
         elseif (head(k+1:k+8) .eq. 'NAXIS3  ') then
               read (head(k+1:k+80),1025) naxis3
c*****header information: here is the object name
         elseif (head(k+1:k+8) .eq. 'OBJECT  ') then
               write (object,1027) head(k+12:k+31)
1027           format (a20)
c*****header information: here is an alternate object name from ESO VLT data
         elseif (head(k+1:k+21) .eq. 'HIERARCH ESO OBS NAME') then
               write (object,1027) head(k+27:k+46)
c*****header information: for scaled integer data, here is the zero point
         elseif (head(k+1:k+8) .eq. 'BZERO   ') then
               read (head(k+1:k+80),1024) bzero
1024           format (10x,d20.10)
c*****header information: for scaled integer data, here is the multiplier
         elseif (head(k+1:k+8) .eq. 'BSCALE  ') then
               read (head(k+1:k+80),1024) bscale
c*****header information: for linear dispersions, the starting wavelength
         elseif (head(k+1:k+8) .eq. 'W0      ') then
               read (head(k+1:k+80),1024) w0
c*****header information: for linear dispersions, here is the dispersion
         elseif (head(k+1:k+8) .eq. 'WPC     ') then
               read (head(k+1:k+80),1024) wpc
c*****header information: for linear dispersions, the starting wavelength
         elseif (head(k+1:k+8) .eq. 'CRVAL1  ') then
               read (head(k+1:k+80),1024) crval1
c*****header information: for linear dispersions, here is the pixel to which
c     CRVALV1 refers
         elseif (head(k+1:k+8) .eq. 'CRPIX1  ') then
               read (head(k+1:k+80),1024) crpix1
c*****header information: special, if CD1_1 exists, use it instead of CRPIX1
         elseif (head(k+1:k+8) .eq. 'CD1_1   ') then
               read (head(k+1:k+80),1024) crpix1
c*****header information: for linear dispersions, here is the dispersion
         elseif (head(k+1:k+6) .eq. 'CDELT1'  ) then
               read (head(k+1:k+80),1024) cdelt1
c*****header information: get the file name of the observation
         elseif (head(k+1:k+8) .eq. 'FILENAME') then
               write (obsname,1023) head(k+12:k+31)
1023           format (a20)
c*****header information: aperture information, first line marker
         elseif (head(k+1:k+8) .eq. 'APNUM1  ') then
               apline = j
c*****header information: is this a 1-dimensional spectrum?
         elseif (head(k+1:k+8) .eq. 'APFORMAT') then
            if (head(k+12:k+19) .eq. 'onedspec') oned = 0
c*****header information: information about IRAF wavelength solutions
         elseif (head(k+1:k+8) .eq. 'WAT0_001') then
            if (head(k+12:k+27) .eq. 'system=physical ') then
               waveiraf = +1
               oned = 0
            endif
            if (head(k+12:k+27) .eq. 'system=multispec') then
               if (head(k+81:k+88) .eq. 'WAT1_001') then
                  if (head(k+92:k+106).eq.'wtype=multispec' .or.
     .                head(k+92:k+106).eq.'wtype=multipsec') then
                     waveiraf = -1
                  else
                     waveiraf = +1
                  endif
               else
                  waveiraf = +1
               endif
            endif
c*****header information: IRAF auxiliary wavelength solution parameters
         elseif (head(k+1:k+8) .eq. 'LTV1    ') then
               read (head(k+1:k+80),1024) ltv1
         elseif (head(k+1:k+8) .eq. 'LTM1_1  ') then
               read (head(k+1:k+80),1024) ltm11
c*****header information: old SPECTRE-style dispersion information
         elseif (head(k+1:k+8) .eq. 'HISTORY ') then
               if (head(k+24:k+28) .eq. 'DISP=') then
                  read (head(k+1:k+80),1022) (disp(i),i=1,4)
1022              format(28x,1p4e13.5)
                  waveiraf = 0
c*****header information: new SPECTRE-style dispersion information
               elseif (head(k+20:k+26) .eq. 'D1,2,3:') then
                  read (head(k+1:k+80),1042) (disp(i),i=1,3)
                  read (head(k+81:k+160),1042) (disp(i),i=4,6)
                  read (head(k+161:k+240),1042) (disp(i),i=7,9)
1042              format(26x,1p3d18.11)
                  waveiraf = 0
               endif
         endif
150   continue




c*****name the desired wavelength/point region of a very long single 
c*****order spectrum
      if (naxis1 .gt. 10000) then
         if (naxis .gt. 1) then
            write(errmess,1020) naxis, naxis1
1020        format('ERROR: CANT DO NAXIS1 =',i6,', NAXIS1 =',i6)
            nchars = 47
            call puterr (nchars)
            go to 1007
         else
            longspec = 'y'
            nord = 1
            message = 
     .        'LONG SPECTRUM: NAME LOWER WAVELENGTH/POINT LIMIT: '
            nchars = 53
            call getnum (nchars,xnum)
            wavlo = xnum
            message = 
     .        'LONG SPECTRUM: NAME UPPER WAVELENGTH/POINT LIMIT: '
            nchars = 53
            call getnum (nchars,xnum)
            wavhi = xnum
            if (crval1.eq.0.0 .and. crpix1.eq.0.0 .and. cdelt1.eq.0.0
     .          .and. w0.eq.0.0 .and. wpc.eq.0.0) go to 130
            if (crval1.ne.0 .and. disp(1).eq.0 .and.
     .          waveiraf.eq.0) then
                disp(2) = cdelt1
                disp(1) = crval1 - crpix1*cdelt1
            elseif (w0.ne.0 .and. wpc.ne.0) then
                disp(2) = wpc
                disp(1) = w0
            endif
            naxlo = max0(1,int(sngl((wavlo-disp(1))/disp(2))))
            naxhi = min0(naxis1,int(sngl((wavhi-disp(1))/disp(2))))
            if (naxhi-naxlo+1 .gt. 10000) then
               write(errmess,1021) naxlo, naxhi
               nchars = 46
               call puterr (nchars)
               go to 1007
            else
               go to 301
            endif
         endif
      endif



c*****choose the desired spectrum from the NAXIS3 possibilities
      if (naxis .eq. 3) then
         if (naxis3 .eq. 1) then
            nspec = 1
         else
            write (message,1019) naxis3
1019        format('THERE ARE ',i2,' DIFFERENT SPECTRA HERE. ',
     .             'CHOOSE ONE: ')       
            nchars = 49
110         call getnum (nchars,xnum)
            nspec = ifix(sngl(xnum))
            if (nspec.lt.1 .or. nspec.gt.naxis3) then
               message = 'SPECTRUM # IS OUT OF RANGE! RETRY: '
               nchars = 35
               go to 110
            endif
         endif
      else
         nspec = 1
      endif


c*****choose the desired order number from the NAXIS2 possibilities
      if (naxis .ge. 2) then
         if (naxis2 .eq. 1) then
            nord = 1
         else
            write (message,1009) naxis2
1009        format('THERE ARE ',i2,' ORDERS IN THIS SPECTRUM. ',
     .             'PICK ONE: ')
            nchars = 48
111         call getnum (nchars,xnum)
            nord = ifix(sngl(xnum))
            if (nord.lt.1 .or. nord.gt.naxis2) then
               message = 'ORDER # IS OUT OF RANGE! RETRY: '
               nchars = 32
               go to 111
            endif
         endif
      else
         nord = 1
      endif


c*****with naxis1, nord, and nspec now known, compute the proper
c*****beginning and ending points within the data array
      write (arrayn,1101) nord
      naxlo = naxis1*naxis2*(nspec-1) + naxis1*(nord-1) + 1
      naxhi = naxlo + naxis1 - 1



c*****initialize the arrays
         do i=1,10000
            wl(i) = 0.
            pts(i) = 0.
         enddo
         wl(1) = -9999.



c*****now read the blocks of data
301   lrec = nrec + naxlo/nblock + 1
      mrec = nrec + naxhi/nblock + 1
      ipt = 1
      do irec=lrec,mrec
         istart = nblock*(irec-nrec-1) + 1
         istop = istart + nblock - 1
         if (naxlo .gt. istart) then
            ibegin = naxlo - istart + 1
         else
            ibegin = 1
         endif
         if (naxhi .lt. istop) then
            iend = naxhi - istart + 1
         else
            iend = nblock
         endif
         lpt = iend - ibegin + 1
         if (ifloat .ne. 0) go to 124
         if (ibits .eq. 16) then
            read (unit=iunit,rec=irec,err=1006,iostat=ierr) int1
            do k=2,2880,2
                  onebyte = int1(k)
                  int1(k) = int1(k-1)
                  int1(k-1) = onebyte
            enddo
            do k=1,lpt
               pts(ipt-1+k) = bzero + bscale*real(int2(ibegin-1+k))
            enddo
         elseif (ibits .eq. 32) then
            read (unit=iunit,rec=irec,err=1006,iostat=ierr) int1
            do k=4,2880,4
               onebyte = int1(k)
               int1(k) = int1(k-3)
               int1(k-3) = onebyte
               onebyte = int1(k-1)
               int1(k-1) = int1(k-2)
               int1(k-2) = onebyte
            enddo
            do k=1,lpt
               pts(ipt-1+k) = bzero + bscale*real(int4(ibegin-1+k))
            enddo
         endif
         go to 127
124      if (ibits .eq. 16) then
            write(array,1028) 
1028        format('SORRY: 16-BIT REAL NUMBERS DONT EXIST')
            nchars = 37
            call puterr (nchars)
            go to 1007
         elseif (ibits .eq. 32) then
            read (unit=iunit,rec=irec,err=1006,iostat=ierr) int1  
            do k=4,2880,4
               onebyte = int1(k)
               int1(k) = int1(k-3)
               int1(k-3) = onebyte
               onebyte = int1(k-1)
               int1(k-1) = int1(k-2)
               int1(k-2) = onebyte
            enddo
            do k=1,lpt
               pts(ipt-1+k) = real4(ibegin-1+k)
            enddo
         endif
127      ipt = ipt + lpt
      enddo      
      npt = ipt - 1
      if (longspec .eq. 'y') go to 148



c*****convert the dispersion coefficients if CRVAL1, CDELT1 have been read

      if (crval1.ne.0 .and. disp(1).eq.0 .and.
     .    waveiraf .eq. 0) then
          disp(2) = cdelt1
          disp(1) = crval1 - crpix1*cdelt1
          go to 148
      endif



c*****convert the dispersion coefficients if W0, WPC have been read
      if (w0.ne.0 .and. wpc.ne.0) then
          disp(2) = wpc
          disp(1) = w0
          go to 148
      endif


 
c*****get the dispersion coefficients for the Las Campanas echelle data
      if (waveiraf .lt. 0) go to 130
      if (waveiraf.ge.0 .and. apline.eq.0) go to 146
      if (oned .eq. 0) go to 146
      k = 80*(apline+nord-2)
      read (head(k+1:k+80),1030) disp(1), disp(2)
1030  format (22x,f8.2,3x,e12.5)
      go to 146



c*****IRAF multispec data: put the information stream into array "wavedata"
130   nwave = 0
      do j=1,nlines
         k = 80*(j-1)
         if (head(k+1:k+4) .eq. 'WAT2') then
            wavedata(nwave+1:nwave+68) = head(k+12:k+79)
            nwave = nwave + 68
         endif
      enddo
c*****A special condition: if the original multi-order IRAF header
c     still exists but there is only a single order of spectrum,
c     then query the user on the virtual order number.  I am not sure
c     whether this damages other header reads
      if (disp(1).eq.0.0 .and. disp(2).eq.0.0 .and.
     .    crval1.eq.0.0 .and. crpix1.eq.0.0 .and.
     .    cdelt1.le.1.0 .and. w0.eq.0.0 .and.
     .    wpc.eq.0.0 .and. naxis.eq.1) then
         write (message,1011) naxis2
1011     format('MULTI-ORDER WAVELENGTH INFORMATION IN THE HEADER; ',
     .             'NAME THE ONE YOU WANT: ')
         nchars = 73 
         call getnum (nchars,xnum) 
         nord = ifix(sngl(xnum))
      endif
c*****IRAF multispec data: search for the information on desired order
      iquote = 0
      nquote = 2*nord - 1
      do i=1,nwave
         if (wavedata(i:i) .eq. '"') then
            iquote = iquote + 1
            if (iquote .eq. nquote) go to 144
         endif
      enddo
c*****IRAF multispec data: put that information into array "waveinfo"
144   kount = 0
      do ii=i+1,nwave
         if (wavedata(ii:ii) .eq. '"') then
            go to 145
         else
            kount = kount + 1
            waveinfo(kount:kount) = wavedata(ii:ii)
         endif
      enddo
      write (*,*) 'waveinfo = '
      write (*,*) waveinfo
c111  format ('   waveinfo = ',waveinfo)
c*****IRAF multispec data: decide what dispersion solution has been done
145   read (waveinfo,*) apnum,specnum,dcflag
c*****IRAF multispec data: no dispersion solution exists
      if (dcflag .lt. 0) then
         go to 146
c*****IRAF multispec data: a linear dispersion function
      elseif (dcflag .eq. 0) then
         read (waveinfo,*) apnum,specnum,dcflag,disp(1),disp(2),
     .                     a1,voverc
c*****IRAF multispec data: a polynomial function
      else 
         read (waveinfo,*) apnum,specnum,dcflag,a1,a2,a3,voverc,
     .                     a4,a5,a6,a7,polytype,order
c*****IRAF multispec data: this is a Chebyshev or a Legendre
         if (polytype .eq. 1 .or. polytype .eq. 2) then
            if (order .gt. 5) go to 1002
            read (waveinfo,*) apnum,specnum,dcflag,a1,a2,a3,voverc,
     .                        a4,a5,a6,a7,polytype,order,xxmin,
     .                        xxmax,(disp(i),i=1,order)
            disp(6) = ltv1
            disp(7) = ltm11
c*****IRAF multispec data: this is a spline
         elseif (polytype .eq. 3) then
            if (order+3 .gt. 7) go to 1002
            read (waveinfo,*) apnum,specnum,dcflag,a1,a2,a3,voverc,
     .                        a4,a5,a6,a7,polytype,order,xxmin,
     .                        xxmax,(disp(i),i=1,order+3)
            if (xxmin .ne. 1 .or. xxmax .ne. npt) go to 1002
         else
            go to 1002
         endif
         disp(9) = real(polytype)
         disp(8) = real(order)
      endif
c*****IRAF multispec data: find a new object name, if available
146   orderchar = '  '
      if (naxis .eq. 1) go to 148
      if (nord .ge. 10) then
         write (orderchar,1001) nord
1001     format (i2)
      else
         write (orderchar,1010) nord
1010     format (i1)
      endif
      do j=1,nlines
         k = 80*(j-1)
         if (head(k+1:k+4) .eq. 'APID') then
            if (head(k+5:k+6) .eq. orderchar) then
               write (object,1023) head(k+12:k+31)
               go to 148
            endif
         endif
      enddo
131   if (longspec .eq. 'y') then
         naxlo = max0(1,int(sngl((wavlo-disp(1))/disp(2))))
         naxhi = min0(naxis1,int(sngl((wavhi-disp(1))/disp(2))))
         if (naxhi-naxlo+1 .gt. 10000) then
            write(errmess,1021) naxlo, naxhi
1021        format('ERROR: CANT DO POINT =',i6,', TO POINT =',i6)
            nchars = 46
            call puterr (nchars)
            go to 1007
         else
            go to 301
         endif
      endif




c*****fill the channel/wavelength array, find data minimum and maximum
148   if (disp(2) .eq. 0.) disp(2) = 1.
      if (disp(7).ne.0.0 .and. disp(8).eq.0.0 .and. 
     .   disp(9).eq.0.0) then
         disp(8) = 1.0
         disp(9) = npt
      endif
      if (longspec .eq. 'y') disp(1) = disp(1) + naxlo*disp(2)
      do i=1,npt
         wl(i) = wave(real(i),npt,disp)
      enddo

      write (*,*) 'hey ',wl(5),wl(50)
      write (*,*) 'hey ',wave(5),wave(50)
 

      if (disp(2).ne.1.0 .and. wl(npt).lt.wl(1)) then
         do i=1,npt/2
            j = npt + 1 - i
            ptxx = pts(i)
            wlxx = wl(i)
            pts(i) = pts(j)
            wl(i) = wl(j)
            pts(j) = ptxx
            wl(j) = wlxx
         enddo
      endif
      call minimax(pts,xxmin,xxmax,npt)
      close (iunit)
      return



c*****a bad header read has been detected
1002  write(errmess,1003) ierr
1003  format('ERROR IN FITS HEADER:  ERROR=',i4)
      nchars = 33
      call puterr (nchars)
1007  fname = fname1
      obsname = obsnam1
      object = object1
      arrayn = arrayn1
      npt = npt1
      do i=1,9
         disp(i) = disp1(i)
      enddo
      close (iunit)
      return



c*****a bad data read has been detected
1006  write(errmess,1008) ierr
1008  format('ERROR IN FITS DATA:  ERROR=',i4)
      nchars = 31
      call puterr (nchars)
      fname = '                                        '
      object = '                    '
      obsname = '                    '
      arrayn = '  0'
      npt = 0      
      do i=1,9
         disp(i) = disp1(i)
      enddo

      filestyle = 'fits'
      close (iunit)
      return



      end
      




