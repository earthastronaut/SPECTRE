      subroutine fft (xr,xi,m)
c*****fast fourier transform
c     this was borrowed from hagai netzer who borrowed it
c     from someone else. . .
 
      integer p,q,r,s
      dimension xr(131072),xi(131072),ir(4,6),imx(7)
      equivalence (i2max,imx(1)),(i3max,imx(2)),(i4max,imx(3)),
     .            (i5max,imx(4)),(i6max,imx(5)),(i7max,imx(6)),
     .            (i8max,imx(7))

      if (m .le. 0) return
      z = float(m)/2.
      p = int(z)
      z = z - float(p)

      if (z .lt. 0.) then
         return
      elseif (z .eq. 0) then
         p = 1
      else
         p = 2
      endif

      if (m/2-7 .lt. 0) then
         q = m/2
      else
         q = 7
      endif

      if (q-7 .gt. 0) then
         return
      elseif (q-7 .eq. 0) then
         p = 1
      endif
      n = p*(4**q)
      pix2 = 6.28318530
      do 800 i=1,7
800   imx(i) = 1
      if (q .eq. 0) go to 62
      s = n
      do 2 i=1,q
         r = s
         s = s/4
         kmax = n - r
         dj = pix2/float(r)
         do 2 j=1,s
            if (j-1 .ne. 0) then
               t1i = sin(dj*float(j-1))
               fc = 1. - t1i*t1i
               t1r = sqrt(fc)
               fc = fc + fc - 1.
               t2r = fc
               fc = fc + fc
               t3r = t1r*(fc-1.)
               t3i = t1i*(fc+1.)
               t2i = t1r*(t1i+t1i)
            endif
            klim=j+kmax
            do 2 k=j,klim,r
               indx2 = k + s
               indx3 = indx2 + s
               indx4 = indx3 + s
               ar = xr(k) + xr(indx3)
               ai = xi(k) + xi(indx3)
               br = xr(indx2) + xr(indx4)
               bi = xi(indx2) + xi(indx4)
               cr = xr(k) - xr(indx3)
               ci = xi(k) - xi(indx3)
               dr = xi(indx4) - xi(indx2)
               di = xr(indx2) - xr(indx4)
               xr(indx2) = cr + dr
               xi(indx2) = ci + di
               xr(indx3) = ar - br
               xi(indx3) = ai - bi
               xr(indx4) = cr - dr
               xi(indx4) = ci - di
               if (j-1 .ne. 0) then
                  cr = t1r*xr(indx2) - t1i*xi(indx2)
                  xi(indx2) = t1r*xi(indx2) + t1i*xr(indx2)
                  xr(indx2) = cr
                  cr = t2r*xr(indx3) - t2i*xi(indx3)
                  xi(indx3) = t2r*xi(indx3) + t2i*xr(indx3)
                  xr(indx3) = cr
                  cr = t3r*xr(indx4) - t3i*xi(indx4)
                  xi(indx4) = t3r*xi(indx4) + t3i*xr(indx4)
                  xr(indx4) = cr
               endif
               xr(k) = ar + br
2        xi(k) = ai + bi

      if (p-1 .eq. 0) go to 5
62    klim = n - 1
      do 4 k=1,klim,2
         xr(k) = xr(k) + xr(k+1)
         xi(k) = xi(k) + xi(k+1)
         xr(k+1) = xr(k) - xr(k+1) - xr(k+1)
4        xi(k+1) = xi(k) - xi(k+1) - xi(k+1)
5     if (n-4 .le. 0) return

      if (p-1 .eq. 0) go to 13
      r = 7-q
      do 6 i=r,6
6        imx(i) = 4
      ir(1,6) = 0
      ir(2,6) = 4
      ir(3,6) = 2
      ir(4,6) = 6
      do 7 i=1,5
         s = 6-i
         do 7 k=1,4
7           ir(k,s) = 4*ir(k,s+1)
      s = 1
      j = 2
8     i = 0
      do 9 i2=1,i2max
         do 9 i3=1,i3max
            i3s = ir(i3,2) + ir(i2,1)
            do 9 i4=1,i4max
               i4s = ir(i4,3) + i3s
               do 9 i5=1,i5max
                  i5s = ir(i5,4) + i4s
                  do 9 i6=1,i6max
                     i6s = ir(i6,5) + i5s
                     do 9 i7=1,i7max
                        i7s = ir(i7,6) + i6s
                        do 9 p=1,j,s
                           r = i7s + p
                           i = i + 1
                           if (i-r .ge. 0) go to 9
                           ar = xr(i)
                           ai = xi(i)
                           xr(i) = xr(r)
                           xi(i) = xi(r)
                           xr(r) = ar
                           xi(r) = ai
9     continue

      if (s-1 .ne. 0) return
      do 10 i=1,q
10       imx(i) = 4
      if (q-6 .lt. 0) then
         q = q+1
         do 11 i=q,6
11          imx(i) = 1
      endif

      ir(2,1) = 2
      ir(3,1) = 1
      ir(4,1) = 3
      do 12 i=1,5
         do 12 k=2,4
12          ir(k,i+1) = 4*ir(k,i)
      s = n/2
      j = s + 1
      go to 8

13    r=1
      do 14 i=1,q
         imx(i) = 3*r + 1
14       r = 4*r
      i=0
      do 15 i2=1,4
         do 15 i3=1,i3max,4
            i3s = i3 + i2 - 2
            do 15 i4=1,i4max,16
               i4s = i4 + i3s - 1
               do 15 i5=1,i5max,64
                  i5s = i5 + i4s - 1
                  do 15 i6=1,i6max,256
                     i6s = i6 + i5s - 1
                     do 15 i7=1,i7max,1024
                        i7s = i7 + i6s - 1
                        do 15 i8=1,i8max,131072
                           r = i8 + i7s
                           i = i + 1
                           if(i-r .lt. 0) then
                              ar = xr(i)
                              ai = xi(i)
                              xr(i) = xr(r)
                              xi(i) = xi(r)
                              xr(r) = ar
                              xi(r) = ai
                           endif
15    continue
      return

      end


