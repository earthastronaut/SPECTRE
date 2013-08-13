      common /dataval/ dispx(9),  dispy(9),  dispz(9),
     .                 x(131072),   y(131072),   z(131072),
     .                 wlx(131072), wly(131072), wlz(131072),
     .                 vovercx,   vovercy,   vovercz,
     .                 npx,       npy,       npz,   
     .		       linfo
      real*8 dispx, dispy, dispz
      real*4 x, y, z, wlx, wly, wlz, vovercx, vovercy, vovercz
      integer npx, npy, npz
      character linfo*80

