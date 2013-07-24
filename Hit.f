      subroutine hit (xusr,pusr,yusr)
c*****this routine points at, and labels the coordinates of a point 
c     on the screen
 
      include 'Plotval.com'
      include 'Chars.com'
 
      call sm_ctype (colors(4))
      call sm_lweight (1.4)
      ylength = 0.10*(up-down)
      yoffset = yusr - ylength
      call sm_relocate (xusr,amax1(yusr-ylength,down)) 
      call sm_relocate (xusr,yoffset)
      call sm_draw (xusr, yoffset+0.9*ylength)
      call sm_expand (2.0)
      call sm_putlabel (2,'^')
      
      call sm_expand (0.7)
      write (errmess,1003) xusr
1003  format(f8.2)
      call sm_relocate (xusr,amax1(yoffset-0.2*ylength,
     .               down-0.5*ylength))
      call sm_putlabel (2,errmess)
      if (abs(xusr-pusr) .gt. 1) then
         write (errmess,1004) pusr
1004     format ('(',f7.2,')')
         call sm_relocate (xusr,amax1(yoffset-0.9*ylength,
     .                  down-0.5*ylength))
         call sm_putlabel (2,errmess)
      endif

      if (yusr .ne. -9999.) then
         if (abs(yusr) .lt. 10.) then
            write (errmess,1002) yusr
1002        format (f7.4)
         elseif (abs(yusr) .lt. 100.) then
            write (errmess,1005) yusr
1005        format (f7.3)
         elseif (abs(yusr) .lt. 1000.) then
            write (errmess,1006) yusr
1006        format (f7.2)
         elseif (abs(yusr) .lt. 10000.) then
            write (errmess,1007) yusr
1007        format (f7.1)
         elseif (abs(yusr) .lt. 100000.) then
            write (errmess,1008) yusr
1008        format (f7.0)
         else
            write (errmess,1009) yusr
1009        format (1pe10.3)
         endif

         if (abs(xusr-pusr) .gt. 1) then
            call sm_relocate (xusr,amax1(yoffset-1.6*ylength,
     .                     down-0.5*ylength))
         else
            call sm_relocate (xusr,amax1(yoffset-0.9*ylength,
     .                     down-0.5*ylength))
         endif
         if (abs(yusr) .lt. 100000.) then
            call sm_putlabel (2,errmess)
         else
            call sm_putlabel (2,errmess)
         endif
      endif

      call sm_gflush
      call sm_alpha
      return
      
      end








