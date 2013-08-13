      subroutine elines (wl,pts,icol,npt)
c*****this routine displays emission line profiles, and calculates the
c     halfwidths of the lines from spline interpolations.
 
      include 'Plotval.com'
      include 'Mathval.com'
      include 'Chars.com'
      real*4 pts(131072),wl(131072)
      real*8 total,square

c*****copy array into ss for sort
      do 88 i = 1,101
88        ss(i) = pts(npt/2-50+i)

c*****do an insertion sort algorithm
      do 100 i=1,101
          arrmin = ss(i)
          imin = i
          do 200 j = i+1,101
              if(ss(j) .lt. arrmin) then
              	  arrmin = ss(j)
              	  imin = j
              endif
200       continue
100       call rswitch(ss(i),ss(imin))   

c*****find the average of all the points in the array to set a
c     noise discriminator level
      total = 0.
      square = 0.
      do 10 i=1,101
         square = square + ss(i)**2
10       total = total + ss(i)
      avg = total/101.
      dev = sngl(dsqrt((square-101.*avg**2)/100.))
      ylevel = amax1(1.5*dev+avg,10.+avg)
      dlevl = 2.

c*****go through the array and find emission lines
      iplot = 1
      ipt = 21
15    do 20 i=ipt,npt-20
      if (pts(i) .le. ylevel) then
         go to 20
      elseif (pts(i+1).le.ylevel .or. pts(i+3).le.ylevel) then
         go to 20
      else
         ipt = i
35       if (pts(ipt+1) .le. pts(ipt)) go to 25
         ipt = ipt + 1
         go to 35
      endif
20    continue
      return

c*****now plot the lines
25    ilow = ipt - 20
      xleft = real(ilow - 1)
      ihigh = ipt + 20
      right = real(ihigh + 1)
      num = ihigh - ilow + 1
      call minimax (pts(ilow),xmin,xmax,num)
      up = 1.12*xmax
      down = xmin - 30.
      if (iplot .eq. 1) then
         call plotxy (4,iplot,wl(ilow),pts(ilow),num,1)
      else
         call plotxy (4,-iplot,wl(ilow),pts(ilow),num,1)
      endif

c*****now set the line limits
c     (AVG is the average for the noise)
      do 40 i=ipt,ilow,-1
         if (pts(i) .lt. ylevel) go to 45
40       continue
45    markl = i + 1
      total = 0.
      kount = 0
      do 46 i=ilow,markl-1
         kount = kount + 1
46       total = total + pts(i)
      avgl = total/kount
      markl = max0(markl-4,ilow)
      do 50 i=ipt,ihigh
         if (pts(i) .lt. ylevel) go to 55
50       continue
55    markr = i - 1
      total = 0.
      kount = 0
      do 56 i=markr+1,ihigh
         kount = kount + 1
56       total = total + pts(i)
      avgr = total/kount
      markr = min0(markr+4,ihigh)
      if ((markr-ipt)-(ipt-markl) .gt. 5) then
         ipt = ipt + 10
         go to 90
      endif
      if (avgl .gt. dlevl*avgr) avgl = avgr
      if (avgr .gt. dlevl*avgl) avgr = avgl
      avg = (avgl+avgr)/2.
      num = markr - markl + 1

c*****now plot a spline fit to the line data
      call splnc (num,0.001)
      numspl = (num-1)*10 + 1
      do 60 i=1,numspl
60       t(i) = wl(markl) + 0.1*(i-1)
      call spln (num,numspl)
      call plotxy (4,-iplot,wl(ilow),pts(ilow),num,-icol)

c*****now compute the 0.5 and 0.1 power points
      call minimax (ss,splmin,splmax,numspl)
      half = avg + 0.5*(splmax-avg)
      tenth = avg + 0.1*(splmax-avg)
      call fracton (1,numspl,half,hleft)
      call fracton (numspl,1,half,hright)
      call fracton (1,numspl,tenth,tleft)
      call fracton (numspl,1,tenth,tright)

105   fwhm = hright-hleft + 0.1
      write (errmess,1006) fwhm
1006  format ('FW.5M = ',f4.1)
c      call setexpand (0.9)
      call sm_expand (0.9)
      call sm_relocate (xleft + 0.5*(right-xleft),down+0.20*(up-down))
      call sm_putlabel (5,errmess)
      fwtm = tright-tleft + 0.1
      write (errmess,1007) fwtm
1007  format ('FW.1M = ',f4.1)
      call sm_relocate (xleft + 0.5*(right-xleft),down+0.10*(up-down))
      call sm_putlabel (5,errmess)

      ipt = markr
90    if (ipt .ge. npt-20) return
      if (iplot .lt. 4) then
         iplot = iplot + 1
         go to 15
      else
         iplot = 1
         message = 'DO YOU WANT TO SEE MORE LINES ([y]/n)? '
         nchars = 39
         call getasci (nchars)
         if (array(1:1).eq.'y' .or. nchars.le.0) go to 15
         return
      endif

      end

      
