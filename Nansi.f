


      integer function ivcleof(y,x)
c
c    This routine clears to the end of the screen, beginning with the
c    position row=y, column=x. The cursor is then placed at (y,x)
c
      include 'Scrnval.com'
      character blank*79
      integer y,x,ypos,xpos
c
      write(blank,1001)
1001  format(79(' '))
      istat = ivwrite(y,x,blank,79)
      xpos = 1
      ypos = y

      do 10 ipos=ypos,maxline
          istat = ivmove(ipos,xpos)
10        istat = ivcleol()
      istat = ivmove(y,x)
c
      ivcleof = 0
      return
      end





      integer function ivwrite(y,x,array,ccount)
c
c    This routine writes out a string of characters from array,
c    with the first character beginning at row=y, column=x. The length
c    of the string may be at most 79 characters, and this routine
c    will not write on the 80th column or the 25th row of the screen.
c
      include 'Scrnval.com'
      integer y,x,count,ccount
      character array*80,dummy*80,string(80)*1,esc*1
      equivalence (dummy,string(1))
c
      count = ccount
      if (y .gt. maxline .or. x .gt. 79) then
         ivwrite = -1
         return
      endif
c
      esc = char(27)
      dummy = array
      count = min0(80-x,count)
c       
      if (x .lt. 10) then
         if (y .lt. 10) then
            write (*,1007) esc,y,x,(string(i),i=1,count)
1007        format(1x,a1,'[',i1,';',i1,'H',80a1)
         else
            write (*,1006) esc,y,x,(string(i),i=1,count)
1006        format(1x,a1,'[',i2,';',i1,'H',80a1)
         endif
      else 
         if (y .lt. 10) then
            write (*,1005) esc,y,x,(string(i),i=1,count)
1005        format(1x,a1,'[',i1,';',i2,'H',80a1)
         else
            write (*,1004) esc,y,x,(string(i),i=1,count)
1004        format(1x,a1,'[',i2,';',i2,'H',80a1)
         endif
      endif
c
      ivwrite = 0
      return
      end






       integer function ivmove(y,x)
c
c    This routine moves the cursor to position row=y, column=x.
c    It checks to be sure that y does not exceed 24, and x does not
c    exceed 79 (the screen limits).
c
      include 'Scrnval.com'
      integer y,x
      character esc*1
c
      if (y .gt. maxline .or. x .gt. 79) then
         ivmove = -1
         return
      endif
c
      esc = char(27)
c      
      if (x .lt. 10) then
         if (y .lt. 10) then
            write (*,1007) esc,y,x
1007        format(1x,a1,'[',i1,';',i1,'H')
         else
            write (*,1006) esc,y,x
1006        format(1x,a1,'[',i2,';',i1,'H')
         endif
      else 
         if (y .lt. 10) then
            write (*,1005) esc,y,x
1005        format(1x,a1,'[',i1,';',i2,'H')
         else
            write (*,1004) esc,y,x
1004        format(1x,a1,'[',i2,';',i2,'H')
         endif
      endif
c
      ivmove = 0
      return
      end



    


      integer function ivcleol()
c    This routine clears to the end of a line, beginning with the current
c    cursor position.
c
      character*1 esc
c
      esc = char(27)
      write(*,1001) esc
1001  format(1x,a1,'[K')
c
      ivcleol = 0
      return
      end



