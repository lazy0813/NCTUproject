c    This program is to find the maximum and minimum value for
c    the following parameters:
c    1. SD, signal duration
c    2. SK, skewness
c    3. KT, kurtosis
c    4. ML/MD, magnitude ratio
c    5. Spectral amplitude ratio
c
c----------------------------------------------------------------------------------------------------
c     March 16. 2021 created by vvn weian chao, associate professor
c     Email: vvnchao@gmail.com; vvnchao@nctu.edu.tw
c     Department of Civil Engineering, National Chiao Tung University
c--------------------------------------------------------------------

      PROGRAM findmaxmin

      type para
       character(len=4) stn
	 real epi,azi,pgv,t1,t2
	 real SD,SK,KT,M_ratio,LFHF
	 real A0,AE,Ml,MD
      end type para

      type(para) :: d

c    parameters
      character(len=100) inp,oup,fn,fninp
	character(len=100) par,par1
	character(len=300) header
      logical EXT0
c    command mode
      integer*4 bat,system
	character(len=300) cmd
c    max. min. values
      real SD1,SD2
	real SK1,SK2
	real KT1,KT2
	real M1,M2
	real F1,F2



c    INPUTS:
c-----------
c    Get number of arguments:
       narg=iargc()
       if(narg.lt.1) then
	  print*,'find.max.min -I.\ -O.\'
	  stop 'ERROR: arguments?' 
	 endif

c    DEFAULT VALUE:
c-------------------
       inp='./'
	 oup='./'

c    Get arguments (optional):
c-----------------------------
       do iarg=1,narg
        call getarg(iarg,par,ic)
        l=len(par)
	   if(par(1:2).eq.'-I') then
	   inp=par(3:l)
	   else if(par(1:2).eq.'-O') then
	   oup=par(3:l)
	   else
	   stop 'ERROR: arguments?' 
	   endif
	 enddo

c  Check Input Directory Is Exist Or Not:
c------------------------------------------
       li=index(inp,' ')-1
       IF(inp(li:li).ne.'\') THEN
       li=li+1
       inp(li:li)='\'
       ENDIF
       
       OPEN(1,file=inp(1:li)//'tmp',IOSTAT=istat)
       IF(istat.NE.0) STOP 'ERROR: INPUT DIRECTORY IS NOT EXIST...'
       CLOSE(1,STATUS='delete')
       
c  Check Output Directory Is Exist Or Not:
c------------------------------------------
       lo=index(oup,' ')-1
       IF(oup(lo:lo).ne.'\') THEN
       lo=lo+1
       oup(lo:lo)='\'
       ENDIF
       
   10  INQUIRE(file=oup(1:lo)//'tmp',exist=EXT0)   
       IF(EXT0) THEN
        ELSE
       OPEN(1,file=oup(1:lo)//'tmp',status='unknown',err=11)
       CLOSE(1,status='delete')
       ENDIF
       GOTO 15
   11  cmd=' '   
       cmd='mkdir '//oup(1:lo)
       bat=systemQQ(cmd)
       GOTO 10

c    Create output txt file
   15  open(1,file=oup(1:lo)//'merge_result.txt',status='unknown')


c    Find txt filename list in input directory
       cmd='dir '//inp(1:li)//'*.txt /b /ON > TXTFILE.list'
       bat=system(cmd)

       open(2,file='TXTFILE.list',status='old')


       SD1=1.E6
       SK1=1.E6
	 KT1=1.E6
       M1=1.E6
	 F1=1.E6

       SD2=-1.E6
       SK2=-1.E6
	 KT2=-1.E6
       M2=-1.E6
	 F2=-1.E6

	 ndata=0
       ievent=1
       read(2,*) fn
	 fninp=inp(1:li)//trim(fn)
	 open(10,file=fninp,status='old')
	 read(10,'(a300)') header
	 write(1,'(a115,a20)') trim(header),'txtfile' 
	  do while(.true.)
	   read(10,*,iostat=istat) d
	   if(istat.ne.0) exit
c    find max. min.
          if(SD1.gt.d%SD) SD1=d%SD
          if(SK1.gt.d%SK) SK1=d%SK
          if(KT1.gt.d%KT) KT1=d%KT
          if(M1.gt.d%M_ratio) M1=d%M_ratio
          if(F1.gt.d%LFHF) F1=d%LFHF

          if(SD2.lt.d%SD) SD2=d%SD
          if(SK2.lt.d%SK) SK2=d%SK
          if(KT2.lt.d%KT) KT2=d%KT
          if(M2.lt.d%M_ratio) M2=d%M_ratio
          if(F2.lt.d%LFHF) F2=d%LFHF

	   write(1,44) d,trim(fn)
	   ndata=ndata+1
	  enddo 
	 close(10)
	 
       do while(.true.)
       read(2,*,iostat=istat) fn
	 if(istat.ne.0) exit
	 ievent=ievent+1
	 fninp=inp(1:li)//trim(fn)
	 open(10,file=fninp,status='old')
	 read(10,*) !-- read header
	  do while(.true.)
	   read(10,*,iostat=istat) d
	   if(istat.ne.0) exit
c    find max. min.
          if(SD1.gt.d%SD) SD1=d%SD
          if(SK1.gt.d%SK) SK1=d%SK
          if(KT1.gt.d%KT) KT1=d%KT
          if(M1.gt.d%M_ratio) M1=d%M_ratio
          if(F1.gt.d%LFHF) F1=d%LFHF

          if(SD2.lt.d%SD) SD2=d%SD
          if(SK2.lt.d%SK) SK2=d%SK
          if(KT2.lt.d%KT) KT2=d%KT
          if(M2.lt.d%M_ratio) M2=d%M_ratio
          if(F2.lt.d%LFHF) F2=d%LFHF

	   write(1,44) d,trim(fn)
	   ndata=ndata+1
	  enddo 
	 close(10)

       enddo

       close(1)
       close(2)

   44 FORMAT(a4,1X,2f9.3,e13.3,3f8.2,2f7.2,1x,2f7.3,2(1x,e10.3),2f7.2
     1,a20)

       write(*,*) ' Total event number : ',ievent
	 write(*,*) ' Total data number : ',ndata

       open(1,file=oup(1:lo)//'max_min.txt',status='unknown')
       write(1,*) ' Parameters          maximum  minimum'
       write(1,'(a15,5x,2f8.2)') '     SD (sec) ',SD2,SD1
       write(1,'(a15,5x,2f8.2)') '     SK (---) ',SK2,SK1
       write(1,'(a15,5x,2f8.2)') '     KT (---) ',KT2,KT1
       write(1,'(a15,5x,2f8.2)') 'M_ratio (---) ',M2,M1
       write(1,'(a15,5x,2f8.2)') 'logLFHF (---) ',F2,F1

       close(1)

	END PROGRAM findmaxmin