      subroutine setre (mode,factor)
c*****this routine simply keeps track of the changes in continuum height
c     indicated by the user (mode=1), or returns the factor for reseting
c     the continuum to original values (mode = 0)
 
      data fac/1.0/
 
      if (mode .eq. 1) go to 10
      factor = fac
      fac = 1.
      return

10    fac = fac*factor
      return 

      end  


