      subroutine expblo()
c*****this routine changes the plot boundaries by having the user
c     explicitly name the desired boundary values

      include 'Dataval.com'
      include 'Plotval.com'
      include 'Chars.com'
      real*8 l,r,u,d

      write (message,1000) xleft
1000  format ('LEFT  = [',f9.2,'] ')
      nchars = 20
      call getnum (nchars,l)
      if (l .ne. -9999.) xleft = sngl(l)

      write (message,1001) right
1001  format ('RIGHT = [',f9.2,'] ')
      nchars = 20
      call getnum (nchars,r)
      if (r .ne. -9999.) right = sngl(r)
 
      write (message,1002) up
1002  format ('UP    = [',f9.3,'] ')
      nchars = 20
      call getnum (nchars,u)
      if (u .ne. -9999.) up = sngl(u)

      write (message,1003) down
1003  format ('DOWN  = [',f9.3,'] ')
      nchars = 20
      call getnum (nchars,d)
      if (d .ne. -9999.) down = sngl(d)

c*****recheck the sense of the boundaries
      if (right .lt. xleft) call rswitch (xleft,right)
      if(up .lt. down) call rswitch (up,down)
 
c*****replot the data
      call plotxy (1,1,wlx,x,npx,1)
      return

      end



