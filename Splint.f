      function splint (x)
c*****spline interpolating routine * * * *
c     returns the value of the spline at x
c     the spline coefficients must be in block /mathval/
 
      include 'Mathval.com'

c     for 10 i=nk,1
      do 10 i=nknots,1,-1
           d=x-spx(i)
           if (d) 10,20,20
10    continue
      if(i.eq.0) i = 1
      go to 30
20    if (i.ne.nknots) go to 30
      i=nknots-1
      d=x-spx(i)
30    if (i.eq.0) i = 1
      splint=((abs(spcoc(i,3))*d+spcoc(i,2))*d+spcoc(i,1))*d+spcoy(i)
      return

      end


  





