      common /dataval/ dispx(9),  dispy(9),  dispz(9),
     .                 x(10000),   y(10000),   z(10000),
     .                 wlx(10000), wly(10000), wlz(10000),
     .                 vovercx,   vovercy,   vovercz,
     .                 npx,       npy,       npz,   
     .		       linfo
      real*8 dispx, dispy, dispz
      real*4 x, y, z, wlx, wly, wlz, vovercx, vovercy, vovercz
      integer npx, npy, npz
      character linfo*80

