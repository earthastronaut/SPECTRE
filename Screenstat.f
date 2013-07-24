      subroutine screenstat (mode)
c*****mode = 0 ->  initialize status part of screen
c          = anything else, just update the status screen

      include 'Dataval.com'
      include 'Datachr.com'
      include 'Chars.com'

      if (mode .eq. 0) then
         istat = ivcleof(1,1)
         write(array,1000)
1000     format(31('-'),'REGISTER CONTENTS',31('-'))
         istat = ivwrite(1,1,array,80)
         write(array,1010)
1010     format('X                      Y                        Z')
         istat = ivwrite(2,20,array,59)
1017     format (80('-'))
         write(array,1017)
         istat = ivwrite(3,1,array,80)
      endif

      write (array,1012) xobj(1:16),yobj(1:16),zobj(1:16)
1012  format (3('OBJECT  :',a16,' ')) 
      istat = ivwrite(4,1,array,78)
      write (array,1013) npx,npy,npz
1013  format (3('NAXIS1  :',i8,'         ')) 
      istat = ivwrite(5,1,array,78)
      write (array,1014) xfname(1:16),yfname(1:16),zfname(1:16)
1014  format (3('SUNFILE :',a16,' '))
      istat = ivwrite(6,1,array,78)
      write (array,1015) xkfnam(1:16),ykfnam(1:16),zkfnam(1:16)
1015  format (3('FILENAME:',a16,' '))
      istat = ivwrite(7,1,array,78)
      write (array,1016) xary,yary,zary
1016  format (3('ARRAY#  :     ',a3,'         '))
      istat = ivwrite(8,1,array,78)
 
      if (mode .eq. 0) then
         write (array,1017)
         istat = ivwrite(9,1,array,80)
      endif
      return

      end







