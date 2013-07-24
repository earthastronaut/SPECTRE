      subroutine syminv (a,nn)

1001  format (1h1/
     *'   call syminv(a,n,m)        inverts the n x n symmetric matrix'/
     *'   a by gausian elimination.  m is the size of a as defined    '/
     *'   in its dimension statement.  if omitted, m = n is assumed   '/
     *'   a is replaced by its inverse.   n must be @ 50       ')

      include 'Chars.com'
      real*8 a(1)
      real*8 p(25),q(25),r(25),zero,one,big,ab

90    zero = 0.0
      one = 1.0
      nnn = nn
      n = nnn
      if (n .gt. 25) then
         write(errmess,1000) n
1000     format (i10,' IS TOO BIG FOR N (MAX = 50) ')
         nchars = 39
         call puterr (nchars)
         return
      endif

      do 1 m = 1,n
    1  r(m) = one
       do 2 m = 1,n
        big = zero
        do 3 l = 1,n
         ab = dabs(a(l+nnn*(l-1)))
         if (ab - big) 3,3,4
    4    if (r(l)) 14,3,14
   14    big = ab
         k = l
    3   continue

        if (big .eq. 0.) then
           write (errmess,1013)
1013       format ('SYMINV HAS FAILED! ')
           nchars = 19
           call puterr (nchars)
           return
         endif

    6   r(k) = zero
        q(k) = one/a(k+nnn*(k-1))
        p(k) = one
        a(k+nnn*(k-1)) = zero
        km1 = k - 1
        if(km1.eq.0) go to 16
   18   do 7 l = 1,km1
         p(l) = a(l+nnn*(k-1))
         if (r(l)) 9,8,9
    8    q(l) = a(l+nnn*(k-1))*q(k)
         go to 7
    9    q(l) = -a(l+nnn*(k-1))*q(k)
    7    a(l+nnn*(k-1)) = zero
   16   continue
        kp1 = k + 1
       if(kp1.gt.n) go to 17
   19  do 15 l=kp1,n
         if (r(l)) 12,11,12
   12   p(l) = a(k+nnn*(l-1))
         go to 10
   11   p(l) = -a(k+nnn*(l-1))
   10   q(l) = (-a(k+nnn*(l-1)))*q(k)
   15   a(k+nnn*(l-1)) = zero
   17   continue
         do 2 l = 1,n
         do 2 k = l,n
    2    a(l+nnn*(k-1)) = a(l+nnn*(k-1)) + p(l)*q(k)
      m = n + 1
      l = n
      do 27 k = 2,n
      m = m-1
      l = l-1
      do 27 j = 1,l
   27 a(m+nnn*(j-1)) = a(j+nnn*(m-1))
      return

      end



