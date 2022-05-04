c    This program is to create grid matric for image feature
c-------------------------------------------------------------
c     March 16. 2021 created by vvn weian chao, associate professor
c     Email: vvnchao@gmail.com; vvnchao@nctu.edu.tw
c     Department of Civil Engineering, National Chiao Tung University
c--------------------------------------------------------------------

      PROGRAM image
      parameter(npmax=65536)


      type para
       character(len=4) stn
	 real epi,azi,pgv,t1,t2
	 real SD,SK,KT,M_ratio,SRLH
	 real A0,AE,Ml,MD
	 character(len=30) fn
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

c    ps plot
      integer pgopen,istat
	character(len=200) ps,txt

c    grid
      real dx,dy
	integer ngrid,nx,ny
	real gx(npmax),gy(npmax)
	real cx(npmax),cy(npmax)
	real ex(npmax),ey(npmax)
      real pi,pi2rad,theta
      real bx(5),by(5)   !-- size depends on the number of parameters

c    MAX. MIN.
      real SD1,SD2,SK1,SK2,KT1,KT2
      real M1,M2,F1,F2
	real SD,SK,KT,M_ratio,SRLH

      character(len=300) head

	 pi=atan(1.0)*4.0
	 pi2rad=pi/180.0


c    INPUTS:
c-----------
c    Get number of arguments:
       narg=iargc()
       if(narg.lt.1) then
	  print*,'image_matric -I.\ -O.\'
	  stop 'ERROR: arguments?' 
	 endif

c    DEFAULT VALUE:
c-------------------
       inp='./'
	 oup='./'
       SD1=0.
	 SD2=180.
	 SK1=-1.0
	 SK2=6.0
	 KT1=-2.0
       KT2=32.0
	 M1=0.0
	 M2=7.0
	 F1=-3.0
	 F2=5.0

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

   15  dx=0.01
	 dy=0.01
	 nx=nint(2.0/dx)
       ny=nint(2.0/dy)
       ngrid=nx*ny
	 ig=0
c    create grid   !-- row first and colume second
       do j=1,ny
	  do i=1,nx
        ig=ig+1
	  gx(ig)=-1.0+(i-1)*dx
	  gy(ig)=1.0-(j-1)*dy
	  enddo
       enddo
       write(*,*) ' Total grid number: ', ig
	 write(*,'(a20,i5,a3,i5)') ' Matrix size  ',nx,' x ',ny


c    read input txt file    
       open(1,file=inp(1:li)//'merge_result.txt',status='old')
       read(1,'(a300)') head !-- read header information

      ishape=0
      do while(.true.)

	 read(1,*,iostat=istat) d
	 if(istat.ne.0) exit
	 ishape=ishape+1
c       print*,d%SD,d%SK,d%KT,d%M_ratio,d%SRLH

c    plot image background
       ps=oup(1:lo)//trim(d%fn)//'_'//trim(d%stn)//'.ps/vcps'
	 write(*,'(i4.4,a30,a40)') ishape,' Event plotting....: ',trim(ps)
       istat=pgopen(ps)
       call pgenv(-1.0,1.0,-1.0,1.0,1,-2)
       x1=-1.0
	 x2=1.0
	 y1=-1.0
	 y2=1.0
	 call pgrect(x1,x2,y1,y2)
 
c    plot circle boundary
       cx=0.
	 cy=0.
	 pi=atan(1.0)*4.0
	 pi2rad=pi/180.0
	 do i=1,360
        theta=real(i)
	  cx(i)=cos(theta*pi2rad)
	  cy(i)=sin(theta*pi2rad)
	 enddo

c    plot circle line
	 call pgscr(30,1.,1.,1.)
	 call pgsci(30)
       call pgmove(cx(1),cy(1))
	 do i=2,360
       call pgdraw(cx(i),cy(i))
	 enddo

c    plot shape boundary
c    1: SD  2: SK  3: KT  4:M_ratio   5: logeSRLH
       theta=360./5.0
       bx(1)=0.
	 by(1)=1.
       bx(2)=cos((90-theta)*pi2rad)*1.0
       by(2)=sin((90-theta)*pi2rad)*1.0
       bx(3)=sin((180.0-2*theta)*pi2rad)*1.0
       by(3)=cos((180.0-2*theta)*pi2rad)*(-1.0)
       bx(4)=sin((3*theta-180.0)*pi2rad)*(-1.0)
       by(4)=cos((3*theta-180.0)*pi2rad)*(-1.0)
       bx(5)=cos((4*theta-270.0)*pi2rad)*(-1.0)
       by(5)=sin((4*theta-270.0)*pi2rad)*1.0
       call pgmove(bx(1),by(1))
	 do i=2,5
       call pgdraw(bx(i),by(i))
	 enddo
       call pgdraw(bx(1),by(1))

c    plot event shape
       SD=(d%SD-SD1)/(SD2-SD1)
       SK=(d%SK-SK1)/(SK2-SK1)
       KT=(d%KT-KT1)/(KT2-KT1)
       M_ratio=(d%M_ratio-M1)/(M2-M1)
       SRLH=(d%SRLH-F1)/(F2-F1)
       if(SD.gt.1.0) SD=1.0
       if(SK.gt.1.0) SK=1.0
       if(KT.gt.1.0) KT=1.0
       if(M_ratio.gt.1.0) M_ratio=1.0
       if(SRLH.gt.1.0) SRLH=1.0

       bx(1)=0.
	 by(1)=SD*1.
       bx(2)=SK*cos((90-theta)*pi2rad)*1.0
       by(2)=SK*sin((90-theta)*pi2rad)*1.0
       bx(3)=KT*sin((180.0-2*theta)*pi2rad)*1.0
       by(3)=KT*cos((180.0-2*theta)*pi2rad)*(-1.0)
       bx(4)=M_ratio*sin((3*theta-180.0)*pi2rad)*(-1.0)
       by(4)=M_ratio*cos((3*theta-180.0)*pi2rad)*(-1.0)
       bx(5)=SRLH*cos((4*theta-270.0)*pi2rad)*(-1.0)
       by(5)=SRLH*sin((4*theta-270.0)*pi2rad)*1.0

	 call pgscr(30,.5,.5,.5)
	 call pgsci(30)
       call pgmove(bx(1),by(1))
	 do i=2,5
       call pgdraw(bx(i),by(i))
	 enddo
       call pgdraw(bx(1),by(1))

c    plot white color for grids inside event shape
c    and write the station shape color matrix
       txt=' '
       txt=oup(1:lo)//trim(d%fn)//'_'//trim(d%stn)//'.txt'
	 open(2,file=txt,status='unknown')
       write(2,*) ' Total grid number: ', ig
	 write(2,'(a20,i5,a3,i5)') ' Matrix size  ',nx,' x ',ny

       ng=0
	 do i=1,ig
       call locpt(gx(i),gy(i),bx,by,5,l,m)
	  if(l.ge.0) then
        write(2,*) '1.0'
	  x1=gx(i)-dx*0.7
        x2=gx(i)+dx*0.7
        y1=gy(i)-dy*0.7
        y2=gy(i)+dy*0.7
	  call pgscr(30,1.,1.,1.)
	  call pgsci(30)
        call pgrect(x1,x2,y1,y2)
	  else 
	  write(2,*) '0.0'	  
	  endif 
       enddo

       close(2)
       
       call pgend


       enddo

	END PROGRAM image
