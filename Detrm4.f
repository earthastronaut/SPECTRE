      function detrm4 (array,iorder,is)
c*****compute the real*4 determinant of "array"
c     borrowed from bevington
 
      real*4 array(is,is),save

      norder=iorder
      detrm4=1.
      do 80 k=1,norder

c*****interchange columns if diagonal element is zero
         if (array(k,k)) 50,10,50
   10    do 20 j=k,norder
            if (array(k,j)) 30,20,30
   20    continue
         detrm4=0.
         go to 90
   30    do 40 i=k,norder
            save=array(i,j)
            array(i,j)=array(i,k)
         array(i,k)=save
   40    continue
         detrm4=-detrm4
 
c*****subtract row k from lower rows to get diagonal matrix
   50    detrm4=detrm4*array(k,k)
         if (k-norder) 60,80,80
   60    k1=k+1
         do 70 i=k1,norder
         do 70 j=k1,norder
   70    array(i,j)=array(i,j)-array(i,k)*array(k,j)/array(k,k)
   80 continue
   90 return

      end





