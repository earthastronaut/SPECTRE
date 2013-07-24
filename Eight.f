      subroutine eight (d,n)
c*****this routine does 8-point normalizations

      include 'Chars.com'
      dimension d(n),save(8)

      do 1 i=1,8
1        save(i) = 0.

      nn = (n/8) * 8   - 1
      do 2 i=1,nn,8
         j = i - 1      
         do 2 k=1,8
2           save(k) = save(k) + d( j+k)

      i = nn/ 8
      do 3 j=2,8
3        save(j) = (save(j)-save(1) ) / i
      save (1) = 0.

      write (array,1001)
1001  format ('8 POINT NORMALIZATION FACTORS:',51x)
      call prinfo (5)
      write (array,1002) save
1002  format (8f7.4)
      call prinfo (6)

      do 4 i=1,nn,8    
         do 4 j=2,8
4           d(i+j-1) = d(i+j-1) - save( j)
      return

      end






  
