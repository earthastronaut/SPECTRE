      subroutine plotlines ()
c*****This routine changes which linelist will be plotted based on defaults
      include 'Chars.com'
      include 'Dataval.com'
      include 'Plotval.com'
      real*8 infolam,offset
      integer koffset
      character infolines*45,lineinfo*10

c## put into Plotval.com: || put it into a *.com of it's own and then have a routine in Width.f to set each of these to the desired setting prior to running 'ml'
c    variable: oplotsun = on/off      = 1/0   
c    variable: oplotlines = on/off    = 1/0
c    variable: oplotarcturus = on/off = 1/0
c


 10   format(f11.3,A)
      rewind 23
c     skip the first header line
      read (23,*)

      call sm_expand(0.5)
      call sm_ctype (colors(5))
      ylength = 0.10*(up-down)
      koffset=0
c     Read lines from linelist and plot them

 1    read (23,10,end=2) infolam,lineinfo
      
      if ((infolam .gt. xleft) .and. (infolam .lt. right)) then
         if (koffset .eq. 0) then
            koffset = 1
         elseif (koffset .eq. 1) then
            koffset = 2
         elseif (koffset .eq. 2) then
            koffset = 0
         endif

         call sm_relocate (sngl(infolam),1.00-0.1*(up-down))
         call sm_draw(sngl(infolam),
     .        1.00+(0.025+0.04*koffset)*(up-down))
         
         call sm_relocate (sngl(infolam)-0.03*(right-xleft),
     .        1.00+(0.047+0.04*koffset)*(up-down))
         call sm_putlabel (6,lineinfo)

      endif
      goto 1

 2    call sm_gflush
      return

      end
