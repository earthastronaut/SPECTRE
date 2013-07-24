      subroutine shuffle
c*****arranges knots in ascending order
 
      include 'Mathval.com'

      n=nknots
      do 20 m=1,n-1
         xmin=spx(m)
         isw=m
         do 10 i=m+1,n
            if (xmin.lt.spx(i)) go to 10
            xmin=spx(i)
            isw=i
   10    continue
         if (isw.eq.m) go to 20   
         hx=spx(isw)
         hy=spy(isw)
         spx(isw)=spx(m)
         spy(isw)=spy(m)
         spx(m)=hx
         spy(m)=hy
   20 continue
      return

      end






