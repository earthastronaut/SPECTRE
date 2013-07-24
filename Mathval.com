      common /mathval/ nknots,spx(25),spy(25),df(25),spcoy(25),
     .                 spcoc(24,3),work(189),ss(10000),t(10000),
     .                 s2(25),scratch(10000),i2ptd2,ift 
      logical ift
