      subroutine dskfil (iunit,jostat,fname,istat,acc,fmt,irec)
c*****this routine opens files for reading/writing and does some
c     simple error checking
 
      include 'Chars.com'
      character fname*60,istat*7,acc*10,fmt*11

1     if (acc .eq. 'sequential') then
         open (unit=iunit,iostat=jostat,err=10,file=fname,
     .        status=istat,access=acc,form=fmt)
      else
         open (unit=iunit,iostat=jostat,err=10,file=fname,
     .        status=istat,access=acc,form=fmt,recl=irec)
      endif
      return

c*****Added jostat .eq. 17 which deals with direct access of previous
c     file. I'm fairly sure this is just fine
 10   if (jostat .eq. 117 .or. jostat.eq.1017 .or.
     .    jostat .eq. 128 .or. jostat .eq. 17) then
         message = 'FILE ALREADY EXISTS! OVERWRITE IT (y/[n])? '
         nchars = 43
         call getasci (nchars)
         if (array(1:1).eq.'y') then
            istat = 'old    '
            go to 1
         else if (array(1:1) .eq. 'a') then
            return
         else if (array(1:1).eq.'n' .or. nchars.le.0) then
            message = 'GIVE ANOTHER NAME FOR THE NEW FILE: '
            fname = '                                       '
            nchars = 36
            call getasci (nchars)
            nchars = min0(nchars,40)
            fname = array(1:nchars)
            go to 1
         endif
      else if (jostat.eq.118 .or. jostat.eq.1018 .or. 
     .         jostat.eq.2) then
         message = 'FILE DOES NOT EXIST! TRY AGAIN ([y]/n)? '
         nchars = 40
         call getasci (nchars)
         if (array(1:1).eq.'y' .or. nchars.le.0) then
            message = 'RETYPE THE NAME OF THE FILE YOU WANT: '
            fname = '                                       '
            nchars = 38
            call getasci (nchars)
            nchars = min0(nchars,40)
            fname = array(1:nchars)
            go to 1
         else if (array(1:1).eq.'n' .or. array(1:1).eq.'a') then
            fname = '                                       '
            return
         endif
      else
         write (errmess,1003) fname,jostat
1003     format('FILE ERROR: FILE= ',a17,'ERROR=',i4)
         nchars = 45
         call puterr (nchars)
         return
      endif
      return

      end





      
