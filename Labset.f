      subroutine labset (ival)
c*****This routine simply sets up the appropriate labels for the plotting
c     routines.
 
      include 'Dataval.com'
      include 'Plotval.com'
      include 'Plotchr.com'
 
      go to (1,2,3,4,5,6,7,8),ival

 1    plylab = 'INTENSITY'
      iylcnt = 9
      if (wlx(1) .ne. 1.) then
         plxlab = 'WAVELENGTH'
         ixlcnt = 10
      else
         plxlab = 'PIXEL'
         ixlcnt = 5
      endif
      return

2     plxlab = 'CHANNEL SHIFT'
      ixlcnt = 13
      plylab = 'CORRELATION'
      iylcnt = 11
      return

3     plxlab = 'FREQUENCY'
      ixlcnt = 9
      plylab = 'LOG POWER'
      iylcnt = 9
      return

4     return

5     return

6     return

7     return

8     return

      end






