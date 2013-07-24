      subroutine fracton (ilo,ihi,ylevel,xans)
c*****this routine finds fractional power points in line profile arrays
 
      include 'Mathval.com'

      int = +1
      if (ilo .gt. ihi) int = -1
      do 102 i=ilo,ihi,int
         if (ss(i) .gt. ylevel) then
         	  xans = t(i)
         	  return
         endif
102   continue

      xans = -1.
      return
 
      end



