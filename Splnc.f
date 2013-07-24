      subroutine splnc (n,epsl)
c*****general remarks from original spline routine
c     spln1 is a third order spline fitting routine
c     a general interpolation quadrature routine.  
c     there are n input points y(x) and m
c     interpolated points ss(t). epsln is the error of the fit.
c     it is a preselected actual value and not a percent.
c     algorithm from mathematical methods for digital computers --
c     vol. 2 edited by ralston and wilf.
c           splnc calculates coefficients
c           spln performs interpolation
 

      include 'Chars.com'
      include 'Mathval.com'
      data itmax/50/


      epsln = abs(epsl)
      n1 = n - 1
      do i=2,n1
         h = spx(i+1) - spx(i)
         if (h .le. 0.) go to 25
         h2 = spx(i+1) - spx(i-1)
         dlsqy = (((spy(i+1)-spy(i))/h)-(spy(i)-spy(i-1))/
     .           (spx(i)-spx(i-1)))/h2
         s2(i) = 2.*dlsqy
      enddo
      s2(1) = 0.
      s2(n) = 0.
      omega = 1.0717968
      it = 0
5     eta = 0.
      it = it+1
      if (it .gt. itmax) go to 20
      do i=2,n1
         h = spx(i+1) - spx(i)
         h2 = spx(i+1) - spx(i-1)
         dlsqy = (((spy(i+1)-spy(i))/h)-(spy(i)-spy(i-1))/
     .           (spx(i)-spx(i-1)))/h2
         c = dlsqy*3.
         b = 0.5*(spx(i)-spx(i-1))/(spx(i+1)-spx(i-1))
         w = (c-b*s2(i-1)-(0.5-b)*s2(i+1)-s2(i))*omega
         if (abs(w)-eta .gt. 0.) eta = abs(w)
         s2(i) = s2(i) + w
      enddo
      if (eta-epsln .ge. 0.) go to 5
      return 


c     here are the error messages
20    write (errmess,950) it 
950   format ('SPLNC FAILED TO CONVERGE AFTER ',i5,' ITERATIONS')
      nchars = 47
      call puterr (nchars)
      return
25    write (errmess,951)
951   format('INPUT DATA TO SPLNC DOES NOT INCREASE AS NEEDED')
      nchars = 47
      call puterr (nchars)
      return

      end






