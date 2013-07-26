      subroutine record (kout,linopt,onelin,wave,wavout,depth,
     .     halfl,halfr,eqwdth,widthnote)
c*****Currenly this is only called from the Width Subroutine
c*****this routine records on the screen the parameters of the 
c     equivalent width fits
 
      include 'Datachr.com'
      include 'Chars.com'
      real*8 wave,wavout
      logical onelin
      data kkount/0/
 
c##############################################################
c edited by Dylan Gregersen 11.26.11
c
c##############################################################

c*****Header Info
      if (.not.onelin .and. kout .eq. 0) then
         write (31,1001)
 1001    format (27(' '),'EQUIVALENT WIDTH RESULTS',28(' '))
         write (31,1009)  xobj,xkfnam,xfname
 1009    format (1h /a20,a20,a40)
         write (31,1005)
 1005    format (1h )
         write (31,1003)
 1003    format (2(3x,'WAVELENGTH'),2(2x,'HALFWDTH'),
     .        5x,'DEPTH',3x,'E.W.(mA)')
         write (31,1002)
 1002    format (4x,'(DESIRED)',6x,'(FOUND)',4x,'(LEFT)',
     .        3x,'(RIGHT)')

      endif
c      elseif (kout .eq. 0) then

c*****Just to display information
      write (message,1010)
 1010 format (23(' '),'PREVIOUS EQUIVALENT WIDTH RESULT',24(' '))
      write (array,1003)
      call prinfo (1)
      write (array,1002)
      call prinfo (2)
c     endif

c*****If line is to be omitted
      if (eqwdth .eq. -9999.) then
         write (array,1007) wave,wavout
 1007    format (2f13.3,4(4x,'------'),2x,a)
c     the difference being this is written to a file
         if (.not.onelin .and. kout .gt. 0 ) then
c     if (kount .eq. kkount) backspace (unit=31)
            write (31,1007) wave,wavout,widthnote
         endif

      else


c***** If the line is not omitted
         wid = 1000.*eqwdth
c     Happens everytime line is not omitted
         if (depth .gt. 0.05) then
            write (array,1004) wave,wavout,halfl,halfr,depth,wid,
     .           widthnote
 1004       format (2f13.3,2f10.4,f10.3,f10.1,2x,a)
         else
            write (array,1008) wave,wavout,halfl,halfr,depth,wid,
     .           widthnote

 1008       format (2f13.3,2f10.4,f10.4,f10.2,2x,a)
         endif

c     I think onelin corresponds to a single measurement, here .not. is 
c     the fact that we're using a multiple line file
c     Happens only when the line is recorded in a file

         if (.not.onelin .and. kout .gt. 0) then
            if (depth .gt. 0.05) then
               write (31,1110) wave,wavout,halfl,halfr,depth,wid,
     .           widthnote
            else
               write (31,1110) wave,wavout,halfl,halfr,depth,wid,
     .           widthnote
            endif
         endif
      endif
 1110 format (2f13.3,2f10.4,f10.3,f10.2,2x,a)

      call prinfo (3)
      return
 
      end



