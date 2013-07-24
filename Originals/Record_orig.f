      subroutine record (kount,onelin,wave,wavout,depth,
     .     halfl,halfr,eqwdth)
c*****this routine records on the screen the parameters of the 
c     equivalent width fits
 
      include 'Datachr.com'
      include 'Chars.com'
      real*8 wave,wavout
      logical onelin
      data kkount/0/
 
      if (kount .eq. 0) then
         kkount = 0
         return
      endif
 
      if (kount.eq.1 .and. kkount.eq.0) then
         if (.not.onelin) then
            write (31,1001)
1001        format (27(' '),'EQUIVALENT WIDTH RESULTS',28(' '))
            write (31,1009)  xobj,xkfnam,xfname
1009        format (1h /a20,a20,a40)
            write (31,1005)
1005        format (1h )
            write (31,1003)
1003        format (2(3x,'WAVELENGTH'),2(2x,'HALFWDTH'),
     .              5x,'DEPTH',6x,'E.W.(mA)')
            write (31,1002)
1002        format (4x,'(DESIRED)',6x,'(FOUND)',4x,'(LEFT)',
     .              3x,'(RIGHT)')
         endif
         write (message,1001)
         write (array,1003)
         call prinfo (1)
         write (array,1002)
         call prinfo (2)
      endif
 
      if (eqwdth .eq. -9999.) then
         write (array,1007) wave,wavout
1007     format (2f13.3,3(4x,'------'),9x,'------')
         if (.not.onelin) then
            if (kount .eq. kkount) backspace (unit=31)
            write (31,1007) wave,wavout
         endif
      else
         wid = 1000.*eqwdth
         if (depth .gt. 0.05) then
            write (array,1004) wave,wavout,halfl,halfr,depth,wid
1004        format (2f13.3,2f10.4,f10.3,f14.1)
         else
            write (array,1008) wave,wavout,halfl,halfr,depth,wid
1008        format (2f13.3,2f10.4,f10.4,f14.2)
         endif
         if (.not.onelin) then
            if (kount .eq. kkount) backspace (unit=31)
            if (depth .gt. 0.05) then
               write (31,1004) wave,wavout,halfl,halfr,depth,wid
            else
               write (31,1008) wave,wavout,halfl,halfr,depth,wid
            endif
         endif
      endif

      call prinfo (2+kount)
      kkount = kount
      return
 
      end



