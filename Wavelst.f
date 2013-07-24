      subroutine wavelst (jpt,izoom,chan,wavl)
c*****this routine plots the wavelengths on the screen, redrawing the screen
c     if necessary

      include 'Chars.com'
      include 'Plotval.com'
      include 'Dataval.com'
      real*8 chan(25),wavl(25)

      if (izoom .ne. 0) then
         call plotxy (1,1,wlx,x,npx,1)
         ilo = 1
      else
         ilo = jpt
      endif

      call sm_ctype (colors(7))
      ylength = 0.10*(up-down)
      do 10 i=ilo,jpt
         xpt = sngl(chan(i))
         ipt = int(xpt)
         call sm_expand (1.2)
         call sm_expand (0.8)
         call sm_relocate (sngl(chan(i)),amax1(x(ipt)-ylength,down))
         write (errmess,1001) wavl(i)
1001     format (f8.2)
10       call sm_putlabel (2,errmess)
  
      call sm_expand (1.2)
      call sm_gflush
      call sm_alpha
      return

      end


         


