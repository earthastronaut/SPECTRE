      subroutine edit
c*****this routine does interactive editing of data points
 
      include 'Chars.com'
      include 'Dataval.com'
 
c*****first mark the offending data point
      message = 'MARK THE DATA POINT TO BE EDITED '
      nchars = 33
      call putasci (nchars)
c     call mongohairs (ichr,xusr,yusr)
      call sm_curs (xusr,yusr,ichr)
      call sm_gflush
      call sm_alpha
      call estim (xusr,xusr,dispx,wlx,npx,ipt,ipt)
      if (wlx(ipt) .gt. xusr) ipt = max0(ipt-1,1)
      if (abs(xusr-wlx(ipt)) .gt. abs(wlx(ipt+1)-xusr))
     .     ipt = min0(ipt+1,npx)
      xsave = wlx(ipt)
      ysave = x(ipt)
      call plus (xsave,ysave)
 
c*****then mark the desired location of the point
      message = 'MARK THE NEW DATA POINT VALUE '
      nchars = 30
      call putasci (nchars)
c      call mongohairs (ichr,xusr,yusr)
      call sm_curs (xusr,yusr,ichr)
      call sm_gflush
      call sm_alpha
      x(ipt) = yusr
      call plus (xsave,yusr)
      return

      end




