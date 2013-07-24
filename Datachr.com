      common /datachr/ xobj,xfname,xkfnam,xary,xfile,
     .                 yobj,yfname,ykfnam,yary,yfile,
     .                 zobj,zfname,zkfnam,zary,zfile
      character*3  xary,   yary,   zary
      character*4  xfile,  yfile,  zfile
      character*20 xobj,   yobj,   zobj
      character*80 xfname, yfname, zfname
      character*20 xkfnam, ykfnam, zkfnam
