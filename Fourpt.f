      subroutine fourpt (nfour,nchan,a)
c*****this routine removes 4 point noise by breaking scan 
c     into nfour chunks and normalising each separately.
c     to ensure there are no leftover channels at the end of each chunk
c     the number of channels in each chunk, nch, is made a multiple
c     of 4.  the (nchan-nch*nfour) channels remaining at the end of the
c     scan are not normalised.
 
      include 'Chars.com'
      real*4 a(nchan),av(4,32),adj(4,32)

      do 101 k = 1,nfour
         do 101 j = 1,4
101         av(j,k) = 0.0

      nch = nchan/(4*nfour)
      nch = 4*nch
      do 102 k = 1,nfour
         iup = nch*k-3  
         ilow = nch*(k-1)+1
         do 102 i = ilow,iup,4
            do 102 j = 1,4
               n = i+j-1
102            av(j,k) = av(j,k)+a(n)
  
      do 103 k = 1,nfour
         do 103 j = 1,4
103         av(j,k) = av(j,k)/float(nch/4)

      do 104 k = 1,nfour
         avchunk = (av(1,k)+av(2,k)+av(3,k)+av(4,k))/4.0
         do 104 j = 1,4
104         adj(j,k) = (av(j,k)-avchunk)/avchunk

      write (array,901) 
901   format ('CHUNK',5x,'(AV-AVCHUNK)/AVCHUNK [4 CHANNEL SETS]',33x)
      call prinfo (5)

      do 905 k=1,nfour
         write (array,906) k,(adj(j,k),j = 1,4)
906      format (i3,5x,4f10.6,32x)
905      call prinfo (5+k)
 
c*****do additive normalisation of the 4 sets of chan in each chunk.
      do 105 k = 1,nfour
         iup = nch*k-3  
         ilow = nch*(k-1)+1
         avchunk = (av(1,k)+av(2,k)+av(3,k)+av(4,k))/4.0
         do 105 i = ilow,iup,4
            do 105 j = 1,4
               n = i+j-1
  105          a(n) = a(n)-adj(j,k)*avchunk
      return

      end






