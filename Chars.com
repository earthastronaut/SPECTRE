      common /chars/ array,errmess,message,prompt,command,
     .               head,wavedata,colors(8),codepath, filepath
      character array*80,errmess*80,message*80,prompt*10,codepath*80,
     .          command*2,head*86400,wavedata*25000,colors*7,
     .          commandInput*20, filepath*80
