      subroutine parab (ier)
c*****parabolic fit to three data points.
c     three points must be in arrays spx and spy (x and y values).
c     the data are fit and the coefficients are put in
c     the spline common block /mathval/ and are readable by splint.
c
c     ier=1  =>  determinant is zero
 
      include 'Mathval.com'
      real*4 a(3,3),b(3,3)
 
c*****initialize a and find its determinant
      a(3,3)=1.0d+00
      a(2,3)=a(3,3)
      a(1,3)=a(2,3)
      do 60 iknot=1,3
         do 10 i=1,3
            a(i,2)=spx(i)-spx(iknot)
            a(i,1)=a(i,2)*a(i,2)
   10    continue
         do 20 i=1,3
         do 20 j=1,3
            b(i,j)=a(i,j)
   20    continue
         d=detrm4(b,3,3)
         if (abs(d).lt.1.e-50) go to 70
 
c*****compute the coefficients and put them in the common block
         do 50 ico=1,3
            do 30 i=1,3
            do 30 j=1,3
               b(i,j)=a(i,j)
   30       continue
            do 40 i=1,3
               b(i,ico)=spy(i)
   40       continue
            dummy=detrm4(b,3,3)/d
            spcoc(iknot,ico)=dummy
   50    continue
         spcoy(iknot)=spcoc(iknot,3)
         h=spcoc(iknot,1)
         spcoc(iknot,1)=spcoc(iknot,2)
         spcoc(iknot,2)=h
         spcoc(iknot,3)=0.0
   60 continue
      ier=0
      go to 80
   70 ier=1
   80 return

      end





