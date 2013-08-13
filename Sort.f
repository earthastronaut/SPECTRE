

      subroutine sort (npt,pts)
c*****this routine, from Numerical Recipes (called "shell" in that book)
c     sorts an array "pts" of length "npt" into ascending numerical order,
c     by the Shell-Mezgar algorithm (diminishing increment sort).
      
      real*4 pts(131072), t
      real*8 aln2i, tiny
            
      aln2i = 1.0/0.69314718
      tiny = 1.0e-5
      lognb2 = int(dlog(dfloat(npt))*aln2i+tiny)
      m = npt

      do nn=1,lognb2
         m = m/2
         k = npt - m
         do j=1,k
            i = j
3           continue
            l = i + m
            if (pts(l) .lt. pts(i)) then
               t = pts(i)
               pts(i) = pts(l)
               pts(l) = t
               i = i - m
               if (i .ge. 1) go to 3
            endif
         enddo
      enddo

      return
      end
     


 
