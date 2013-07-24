      function wave(point,npt,c)
c*****this routine returns the wavelength of a given channel number
c
c    EDITED: Dylan Gregersen  
c    To allow SPECTRE to read the MAKEE HIRES Fits files I added the 7th order polynomial
c
c    Version: 2.3.0
      real*8 c(9), z0, z1, z2, z3


      if (c(9) .eq. 1.) go to 20
      if (c(9) .eq. 2.) go to 40
      if (c(9) .eq. 3.) go to 30
      if (c(9) .eq. 4.) go to 50

c*****no wavelength solution exists
      if (c(1).eq.0. .or. c(2).eq.0.) then
         wave = point

c*****an ordinary polynomial solution
      else
         dn = point
         wave = c(1)+dn*(c(2)+dn*(c(3)+dn*(c(4)+dn*(c(5)+dn*c(6)))))
      endif
      return

c*****a Chebyshev polynomial solution
c20    p = (point - c(6))/c(7)
c      xpt = (2.*p-(c(9)+c(8)))/(c(9)-c(8))
20    xpt = (2.*point-real(npt+1))/real(npt-1)
      wave = c(1) + c(2)*xpt + c(3)*(2.*xpt**2-1.) +
     .       c(4)*xpt*(4.*xpt**2-3.) + c(5)*(8.*xpt**4-8.*xpt**2+1.) 
      return

c*****a spline3 solution
30    s = (point-1.)/(real(npt)-1.)*c(8)
      j = int(s)
      a = real(j+1) - s
      b = s - real(j)
      z0 = a**3
      z1 = 1. + 3.*a*(1.+a*b)
      z2 = 1. + 3.*b*(1.+a*b)
      z3 = b**3
      wave = c(j+1)*z0 + c(j+2)*z1 + c(j+3)*z2 + c(j+4)*z3
      return

c*****a Legendre polynomial solution
40    xpt = (2.*point-real(npt+1))/real(npt-1)
      wave = c(1) + c(2)*xpt
      return

c*****an ordinary 7th order polynomial
 50   dn = point
      wave=c(1)+dn*(c(2)+dn*(c(3)+dn*(
     .     c(4)+dn*(c(5)+dn*(c(6)+dn*c(7))))))
      return

      end




