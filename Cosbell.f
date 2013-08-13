      subroutine cosbell (data,npoints,icod)
c*****cosine  bells
c
c     icod  < 0  =>  forward application of data window
c     icod >= 0  =>  reverse application of data window
 
      real*4 data(131072)
      data pi/3.14159265358979/
 
      factor(i,nd10,pi)=(1.0-cos(pi*float(i-1)/float(nd10)))/2.0
      npts=npoints
      nd10=npts/10
      if (icod) 10,30,30
   10 do 20 i=1,nd10
         f=factor(i,nd10,pi)
         data(i)= f*data(i)
         index=npts-i+1
         data(index)= f*data(index)
   20 continue
      go to 50
   30 do 40 i=1,nd10
         f=factor(i,nd10,pi)
         if (abs(f).lt.0.001) f=0.001
         data(i)=data(i)/f
         index=npts-i+1
         data(index)=data(index)/f
   40 continue
   50 return
      end





