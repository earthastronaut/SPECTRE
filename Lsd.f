      subroutine lsd (a,c,w,neqs,nxs,x,e,r,sigma,nx1,neq1)

1001  format (1h1/
     *'   call lsd(a,c,w,neqs,nxs,x,e,r,sigma,nx1,neq1)               '/
     *'   performs a least squares solution.   good for normal least  '/
     *'   squares and for differential corrector problems             '/
     *'   for the i th observation, write:                            '/
     *50h        a(1,i)*x(1) + a(2,i)*x(2) + ... = c(i)                /
     *'   where a and c are known and x is to be solved for           '/
     *'   lsd solves for the x(j), the corresponding mean errors, e(j)'/
     *'   the residuals of each observation: observed - calculated,   '/
     *'   r(i), and for sigma, the standard deviation of the fit (the '/
     *'   square root of the sum of the squares of the residuals)     '/
     *'   lsd must be supplied with a, c, w(i) (an array of weights   '/
     *'   associated with each observation...set all to 1. for no     '/
     *'   weights), neqs (the number of observations), nxs (the number'/
     *'   of unknowns to solve for), and nx1,neq1 (the size of array a'/
     *'   as defined on its dimension statement                       '/
     *'   a write up illustrating the use of lsd is available         ')

      include 'Chars.com'
      real*8 a(nx1,neq1),c(neqs),x(nxs),e(nxs),r(neqs),b(100),
     1          d(6),w(neqs),s,sw,std2,sigma
      real*4 b0(6,6)

      if (nxs.gt.6) then
         write (errmess,1000) nxs
1000     format (i10,' IS TOO BIG FOR THE NUMBER OF UNKNOWNS',
     .           ' (MAX IS 20) ')
         nchars = 61
         call puterr (nchars)
         return
      endif

      neq = nxs
      eqs = neqs
      xs = nxs
      do 25 i=1,nxs
         d(i) = 0.
         do  5 m=1, neqs
5           d(i) = d(i) + a(i,m)*c(m)*w(m)
         do 25 j=1,nxs
            n = i + nxs*(j-1)
            if (i.gt.j) go to 20
            b(n) = 0.
            do 15 m=1,neqs
               b(n) = b(n) + a(i,m)*a(j,m)*w(m)
15             b0(i,j) = b(n)
            go to 25
20          k = j + nxs*(i-1)
            b(n) = b(k)
            b0(i,j) = b(n)
25       continue

      call syminv (b,nxs)

      do 26 i=1,nxs
         x(i) = 0.
         do 26 j=1,nxs
            ij = nxs*(i-1) + j
26          x(i) = x(i) + b(ij)*d(j)

      do 28 i=1,neqs
         r(i) = 0.
         do 28 j=1,nxs
28          r(i) = r(i) + a(j,i)*x(j)

      s = 0.
      sw = 0.
      do 30 i=1,neqs
         sw = sw + w(i)
         r(i) = c(i) - r(i)
30       s = s + r(i)*r(i)*w(i)

      std2 = s/(sw*(1.-xs/eqs))
      sigma = dsqrt(std2)
      do 50 i=1,nxs
         n = (nxs+1)*i-nxs
50       e(i) = dsqrt(b(n)*std2)
      return

      end





