      subroutine pfilt (px,py,npxy,filt,happy)
c*****make a first guess at the filter * * * * * 
c
c  INPUT:   px      - frequency array
c           py       - log P array
c           npxy     - number of points in frequency array
c           filt     - array containing filter-guess parameters such that:
c               1  - x of first point
c               2  - y of first point
c               3  - x of second point
c               4  - y of second point
c               5  - x of third point
c               6  - y of third point
c               7  - noise level
 
      include 'Mathval.com'
      include 'Chars.com'
      logical happy
      real*4 px(1024),py(1024),filt(7)

c*****first, find the portion of the power array that is flat
      avg1=0.0
      do 5 i=npxy-15,npxy
5        avg1 = avg1 + py(i)
      avg1 = avg1/16.
      i = npxy + 1
      kount = 1
      do 2 k=1,npxy-16,16
            avg2=0.0
            do 3 j=1,16
3                 avg2 = avg2 + py(i-16-j)
            avg2 = avg2/16.0
            if(avg2.ge.(avg1/1.25)) then
                  anoise = avg1
                  ilast = npxy - k
                  goto 44
            endif
            i = i - 16
            avg1 = (avg2 + avg1*kount)/(kount+1)
            kount = kount + 1
2      continue
44     continue

c*****next, estimate background from last pts (npxy-ilast)
      sum = 0.
      do 1 i=ilast,npxy-2
    1    sum = sum + py(i)
      filt(7) = sum/real(npxy-1-ilast)

c*****next, do least squares over 6 to ilast to get:     y=a + b*x
      sumx = 0.
      sumy = 0.
      sumxx = 0.
      sumyy = 0.
      sumxy = 0.
      do 4 i=6,ilast
           sumx = sumx + px(i)
           sumy = sumy + py(i)
           sumxx = sumxx + px(i) * px(i)
           sumyy = sumyy + py(i) * py(i)
           sumxy = sumxy + px(i) * py(i)
 4    continue
      delta = (ilast-1)*sumxx - sumx*sumx
      a = (sumxx*sumy - sumx*sumxy)/ delta
      b = (sumxy*(ilast-1) - sumx*sumy ) / delta

c*****now get first filter pair
      avg = 0.0
      do 43 i=5,21
  43     avg = avg + py(i)
      filt(2) = avg/16.0
      filt(1) = 0.0

c*****now do last pair
      filt(6) = filt(7)
      filt(5) = (filt(6) - a)/b - .01

c*****now second filter parameter
      i = (ilast-6)/2
      filt(3) = (real(i)/real(npxy))*0.5
      avg=0.0
      do 99 j=i-8,i+8
99       avg = avg + py(j)
      filt(4) = avg/16.0

c*****check for curvature of the fit - force it to a line in upward curve
      testx = filt(3)
      testy = a + b*filt(3)
      if (filt(4).lt.testy) filt(4) = testy + 0.10
      write (message,1005)
1005  format (24(' '),'FOURIER TRANSFORM NOISE REMOVAL',24(' '))
      write (array,1006)
1006  format('inferred filter:',64x)
      call prinfo (1)
      write(array,1007) (filt(i) , i = 1,7 )
1007  format(1x,3('(',f5.2,',',f5.2,') '),f5.2,32x)
      call prinfo (2)

c*****now plot the noise level computed
c      call setcolor (7)
      call sm_ctype (colors(7))
      call sm_relocate (0.0,filt(7))
      call sm_draw (0.5,filt(7))

c*****now plot the 3 points that define the parabola and see if we're happy
      do 1001 i=1,3
         j = 2*(i-1) + 1
         spx(i) = filt(j)
         spy(i) = filt(j+1)
1001     call plus (spx(i),spy(i))

c*****now fit the parabola
      nknots = 3
      call parab (ier)
      do 2020 i=1,npxy
         t(i) = px(i)
2020     ss(i) = splint(t(i))
      call plotxy (1,-1,t,ss,npxy,-3)

c*****see if the user is happy with the guess, and return
3003  message = 'HAPPY WITH THE INITIAL GUESS AT THE FIT ([y]/n/a)?'
      nchars = 50
      call getasci (nchars)
      if (array(1:1).eq.'y' .or. nchars.le.0) then
         happy = .true.
      elseif (array(1:1).eq.'n' .or. array(1:1).eq.'a') then
         ift = .true.
         happy = .false.
      else
         go to 3003
      endif
      call labset (1)
      return 

      end





