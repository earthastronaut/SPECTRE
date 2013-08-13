      subroutine flip (p1,p2,wl1,wl2,disp1,disp2,np1,np2,ob1,ob2, 
     .     fn1,fn2,xf1,xf2,xa1,xa2,
     .     type1,type2)
c*****this routine flips the contents of spectrum arrays "1" and "2"
 
      real*4 p1(131072), p2(131072), wl1(131072), wl2(131072)
      real*8 disp1(9), disp2(9)
      character*80 xf1, xf2, dum80
      character*20 fn1, fn2, ob1, ob2, dum20
      character*4 type1, type2, dum4
      character*3 xa1, xa2, dum3
 
      call iswitch (np1,np2)

      dum3 = xa1
      xa1 = xa2
      xa2 = dum3

      dum20 = fn1
      fn1 = fn2
      fn2 = dum20

      dum20 = ob1
      ob1 = ob2
      ob2 = dum20

      dum80 = xf1
      xf1 = xf2
      xf2 = dum80

      dum4 = type1
      type1 = type2
      type2 = dum4

      do i=1,131072
         call rswitch (p1(i),p2(i))
         call rswitch (wl1(i),wl2(i))
      enddo

      do i=1,9
         call dswitch (disp1(i),disp2(i))
      enddo

      return
      end




