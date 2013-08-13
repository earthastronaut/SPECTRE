      subroutine move (p1,p2,wl1,wl2,disp1,disp2,np1,np2, 
     .                 ob1,ob2,fn1,fn2,xf1,xf2,xa1,xa2,type1,type2)
c*****this routine moves the contents of spectrum array "1" to "2"
 
      real*4 p1(131072), p2(131072), wl1(131072), wl2(131072)
      real*8 disp1(9), disp2(9)
      character*80 xf1, xf2
      character*20 fn1, fn2, ob1, ob2
      character*4 type1, type2
      character*3 xa1, xa2
 
      xa2 = xa1
      np2 = np1
      ob2 = ob1
      fn2 = fn1
      xf2 = xf1
      type2 = type1

      do i=1,131072
         p2(i) = p1(i)
         wl2(i) = wl1(i)
      enddo

      do i=1,9
         disp2(i) = disp1(i)
      enddo

      return
      end


