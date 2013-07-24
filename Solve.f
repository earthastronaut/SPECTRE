      subroutine solve (chan,wavl,j,npoly)
c*****this routine sets up, solves, and stores the least-squares fits to
c     the channel-wavelength sets
 
      include 'Chars.com'
      include 'Dataval.com'
      include 'Plotval.com'
      real*8 wavl(25),chan(25),ans(4),err(4),resid(25),
     .       weight(25),a(4,25),sigma
 
c*****first write out the measured channels and input wavelengths
      write (array,3003)
3003  format('CHANNEL AND WAVELENGTH PAIRS:    ',46x)
      call prinfo (1)
      do 86 i=1,j,3
         write(array,3004) (chan(k),wavl(k),k=i,i+2)
3004     format (3('(',f8.2,',',f8.2,')     '),7x)
86       call prinfo(2+(i-1)/3)

c*****solve the simple cse in which the number of points equals 2
      if (j .gt. 2) go to 85
      ans(2) = (wavl(2) - wavl(1))/(chan(2) - chan(1))
      ans(1) = wavl(1) - ans(2)*chan(1)
      err(1) = 0.
      err(2) = 0.
      nterms = 2
      go to 90

c*****do a least squares solution: first find the number of terms
85    if (npoly .eq. 0) then
         nterms = min0(4,j-1)
      else
         nterms = min0(npoly,j-1)
      endif

c*****fill the input arays and find the solution
      do 60 jj=1,j
         weight(jj) = 1.
         do 60 i=1,nterms
60       a(i,jj) = chan(jj)**(i-1)
      call lsd (a,wavl,weight,j,nterms,ans,err,resid,sigma,4,25)

c*****write the solution on the information screen
90    istat = ivcleof(15,1)
      do 65 i=1,nterms
         write (array,1001) i,ans(i),err(i)
1001     format ('      COEFF(',i1,') =',1pe12.5,' +/-',e12.5)
65       call prinfo (4+i)
      write (array,3011)
3011  format ('RESIDUALS FOR THE POINTS') 
      call prinfo (10)
      ll = 0
      do i=1,j,10
         ll = ll + 1
         l = min0(i+9,j)
         write (array,3010) (resid(k),k=i,l)
3010     format (10f7.3)
         call prinfo (10+ll)
      enddo
      message = 'IS THE WAVELENGTH SOLUTION OK ([y]/n)? '
      nchars = 39
      call getasci (nchars)
      if (array(1:1) .eq. 'n') return

c*****save the coefficients and channel/wavelengths to disk
69    do 70 i=1,nterms
70       dispx(i) = ans(i)
      call savecf (dispx,chan,wavl,j)
      do 75 i=nterms+1,9
75       dispx(i) = 0.
      do 95 i=1,npx
         wlx(i) = wave(real(i),npx,dispx)
95       continue

c*****plot the results
96    xleft = wlx(1)
      right = wlx(npx)
      call labset (1)
      call plotxy (1,1,wlx,x,npx,1)
      return

      end







