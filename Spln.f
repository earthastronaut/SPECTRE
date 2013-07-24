      subroutine spln (n,m)
c*****interpolate using spline function
c     coefficients calculated in splnc
c     original version from m. macfarlane
c
      include 'Mathval.com'

      do 61 j=1,m
      if(t(j)-spx(1)) 55,16,55
   55 if (t(j)-spx(n)) 57,59,57
   16 i = 1
      go to 17
   57 kk = 1
      jj = n
   23 i = (kk+jj)/2
      if(jj-kk-1) 17,17,22
   22 if (t(j) - spx(i)) 20,17,21
   20 jj=i
      go to 23
   21 kk = i
      go to 23
   58 ss(j) = 0.
      go to 61
   59 i = n-1
   17 ht1 = t(j) - spx(i)
      ht2 = t(j) - spx(i+1)
      prod = ht1*ht2
      s3 = (s2(i+1)-s2(i))/(spx(i+1)-spx(i))
      dely = (spy(i+1)-spy(i))/(spx(i+1)-spx(i))
      ss2 = s2(i)+ht1*s3
      dlsqs = (s2(i)+s2(i+1)+ss2)/6.
      ss(j) = spy(i)+ht1*dely+prod*dlsqs
   61 continue
      return

      end







