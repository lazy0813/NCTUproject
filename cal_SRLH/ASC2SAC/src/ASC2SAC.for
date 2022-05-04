c     Convert ASC File to Intel SAC File
c
c     ASC File Format:
c----------------------------------------------------
c     *Original time      - yyyymmddhhmiss.ss
c     *Lat.  Lon.  Dep.   - Event location
c     *Station name
c     *Lat.  Lon.  Elv.   - Station location
c     *Comp.              - Z/N/E
c     *Data point
c     * T(1)  A(1)        - Time and amplitude data 
c     * T(2)  A(2)
c     *   .    . 
c     *   .    .
c     * T(np) A(np)
c----------------------------------------------------      
c     Oct. 31. 2012 created by vvn, Ph.D student (f97224202@ntu.edu.tw;vvnchao@gmail.com)
c     Department of Geosecience, National Taiwan University
c------------------------------------------------------------------------

      PROGRAM ASC2SAC
	PARAMETER(npmax=8640000)
       CHARACTER(len=100) ascin*30,ascoup*30,sacf
       CHARACTER(len=200) fn
       CHARACTER*100 par,par1
	 CHARACTER(len=17) otime 
	 CHARACTER(len=5) stn*4,STA*4,stmp*4,secd
	 CHARACTER(len=4) yr,date,time
       CHARACTER(len=3) comp,com
	 INTEGER yy,hh,mn,day,doy
       INTEGER*4 iy,iday,im,id,ih,imn
	 INTEGER ncom
	 REAL*8 f,XMAX,XMIN,SUM,MEAN
       REAL sec,rd,f1,f2
	 REAL t(npmax),xc(npmax,3)
	 REAL, ALLOCATABLE :: xp(:),xt(:)
	 REAL s(3),e(3)
	 INTEGER PGOPEN
	 CHARACTER psraw*50,staf*50
c---------------------------
c   For Command Mode:
c---------------------
       LOGICAL(4) bat
	 CHARACTER*300 cmd
c---------------------------

       LOGICAL linp,loup,lf,lbp,lhp,llp
 
       linp=.false.
	 loup=.false.
       f=1.
	 nd=1
       ncom=1

c   INPUTS:
c----------
c   Get number of arguments:
      narg=iargc()
	IF (narg.lt.1) THEN
        write(*,*)' '
	  call usage
      ENDIF

c   Get arguments (optional):
c----------------------------
      DO iarg=1,narg
        call getarg(iarg,par,ic)
        l=len(par)
	  IF(par(1:2).eq.'-I') THEN
	   ascin=par(3:l)
	   linp=.true.
	  ELSE IF(par(1:2).eq.'-O') THEN
	   ascoup=par(3:l)
	   loup=.true.
	  ELSE IF(par(1:2).eq.'-C') THEN
         par1=par(3:l)
         read(par1,*) f
	  ELSE IF(par(1:2).eq.'D=') THEN
         par1=par(3:l)
         read(par1,*) nd
	  ELSE IF(par(1:2).eq.'BP') THEN
	   lbp=.true.
	  ELSE IF(par(1:2).eq.'LP') THEN
	   llp=.true.
	  ELSE IF(par(1:2).eq.'HP') THEN
	   lhp=.true.
	  ELSE IF(par(1:3).eq.'f1=') THEN
         par1=par(4:l)
         read(par1,*) f1
	  ELSE IF(par(1:3).eq.'f2=') THEN
         par1=par(4:l)
         read(par1,*) f2
	  ELSE IF(par(1:4).eq.'com=') THEN
         par1=par(5:l)
         read(par1,'(i1)') ncom
	  ELSE 
	   call usage
	  ENDIF
      ENDDO

       IF(linp .eq. .false.) call usage

	 fn=trim(ascin)
	 np=npmax
	 OPEN(1,file=fn,status='old',iostat=istat)
	 IF(istat.ne.0) STOP ' ERROR: CAN NOT FIND INPUT FILE! '
c   READ DATA HEADER
       READ(1,'(I4.4,4I2.2,f5.2)') iy,im,id,hh,mn,sec 
       READ(1,'(3F12.4)') e(2),e(1),e(3)
       READ(1,*) stn
       READ(1,'(3F12.4)') s(2),s(1),s(3)
	 READ(1,*) comp
       READ(1,*) np

       XMAX=0.0
	 XMIN=1.E10
	 SUM=0.0
c   READ DATA POINT
       IF(ncom.eq.1) then
	 nc=1	    
       DO i=1,np
	 READ(1,*) t(i),xc(i,nc)
       xc(i,nc)=xc(i,nc)*f
	 IF(xc(i,nc).eq.0.000) xc(i,nc)=xc(i-1,nc)
	 SUM=SUM+xc(i,nc)
       ENDDO
       MEAN=SUM/real(np)
c   De-Mean
       DO i=1,np
       xc(i,nc)=xc(i,nc)-MEAN
	 IF(XMAX.lt.xc(i,nc)) XMAX=xc(i,nc)
	 IF(XMIN.gt.xc(i,nc)) XMIN=xc(i,nc)
	 ENDDO
       write(*,'(2(a,e12.4))') ' MAX. ',XMAX,' MIN. ',XMIN
	 
	 else if(ncom.eq.3) then
	 
        DO i=1,np
	  READ(1,*) t(i),xc(i,1),xc(i,2),xc(i,3)
	  ENDDO
  
       DO nc=1,3
	  DO i=1,np
        xc(i,nc)=xc(i,nc)*f
	  IF(xc(i,nc).eq.0.000) xc(i,nc)=xc(i-1,nc)
	  SUM=SUM+xc(i,nc)
        ENDDO
        MEAN=SUM/real(np)
c   De-Mean
        DO i=1,np
        xc(i,nc)=xc(i,nc)-MEAN
	  IF(XMAX.lt.xc(i,nc)) XMAX=xc(i,nc)
	  IF(XMIN.gt.xc(i,nc)) XMIN=xc(i,nc)
	  ENDDO
        write(*,'(2(a,e12.4))') ' MAX. ',XMAX,' MIN. ',XMIN
       ENDDO
	 endif
	 	 
       dt=t(2)-t(1)

	 write(otime(1:4),'(I4.4)') iy
	 write(otime(5:8),'(2I2.2)') im,id
	 write(otime(9:12),'(2I2.2)') hh,mn
	 write(otime(13:17),'(f5.2)') sec

      if(ncom.eq.1) then
	 nc1=1
	 nc2=1
	 com=comp
	else
	 nc1=1
	 nc2=3
	endif

      DO nc=nc1,nc2
c   APPLY FILTER:
c----------------
c   Bandpass filter with zero phase
c----------------------------------
        IF(lbp) THEN
        call IIRFILT(xc(1:np,nc),np,'BUTTER',4,'BP',f1,f2,dt,2) 
        ELSE IF(lhp) THEN
        call IIRFILT(xc(1:np,nc),np,'BUTTER',4,'HP',f1,f2,dt,2)	  
        ELSE IF(llp) THEN
        call IIRFILT(xc(1:np,nc),np,'BUTTER',4,'LP',f1,f2,dt,2)
	  ELSE
	  ENDIF


      IF(ncom.eq.1) then
       IF(loup) THEN
	 sacf=trim(ascoup)
	  ELSE
       sacf=trim(ascin)//'.sac'
	 ENDIF
      else
       sacf=trim(stn)//'_'//otime(1:8)//'_'//otime(9:12)
     &//'.HH'//trim(comp(nc:nc))
	 com='HH'//comp(nc:nc)
	ENDIF

c   RESAMPLING DATA FOR PLOT & WRITE DATA
       nt=np/nd
	 dtt=dt*nd
       ALLOCATE(xp(nt),xt(nt))
       k=0
	 DO i=1,np,nd
	 pmax=-1.E-10
	 pmin=1.E10
	 k=k+1
        DO j=i,i+nd-1
	   IF(pmax.lt.xc(j,nc)) THEN 
	   pmax=xc(j,nc)
	   tmax=real(j-1)*dt
	   ENDIF
	   IF(pmin.gt.xc(j,nc)) THEN 
	   pmin=xc(j,nc)
	   tmin=real(j-1)*dt
	   ENDIF
	  ENDDO

	   IF(abs(pmin).lt.pmax) THEN
	   xp(k)=pmax
	   xt(k)=tmax
	   else
	   xp(k)=pmin
	   xt(k)=tmin
	   ENDIF
	 ENDDO
       
	   
c   PLOT RAW SEISMOGRAM:
c----------------------- 
      write(*,'(a /)')' COPY: rgb.txt and grfont.dat FOR PGPLOT'
	   cmd='copy c:\pgplot\* .\'
         bat=systemQQ(cmd)
         psraw='CHECK.ps/cps'
         jstat=PGOPEN(trim(psraw))
         call pgsubp(1,2)
	   call pgsls(1)
	   call pgsci(1)
	   call pgslw(3)
	   call pgsch(2.5)
	   call pgscf(2)
	   call pgenv(0.,np*dt,REAL(XMIN),REAL(XMAX),2,1)
         call pglab('Time','Amplitude',trim(fn))
	   call pgslw(2)
	   call pgsci(1)
c	   DO k=1,np-1,100
c	   x1=real(k-1)*dt
c	   x2=x1+dt
c	    call pgmove(x1,xc(k))
c	    call pgdraw(x2,xc(k+1))
c	   ENDDO
         call pgline(nt,xt,xp)
         call pgend
	   PAUSE '   CHECKING SEISMOGRAM (CHECK.ps)......'

c   Save original time information from SAC file
c   WRITE SAC 

	  fn=trim(sacf)
	  yy=iy
        call greg2doy(im,id,yy,doy)
        write (*,'(a,I3)') ' Day of year = ', DOY 
	  iday=doy
        imn=mn
        dt=dtt
      write(*,*) 'WRITE: ',trim(sacf),' SAC FILE ' 
      call writesac(fn,nt,dt,xp,iy,iday,ih,imn,sec,stn,com,e,s)
       
	 DEALLOCATE(xp,xt)
      
	ENDDO
  
	END PROGRAM ASC2SAC


c--------------------------------------------------------------
      SUBROUTINE usage
      write(*,*)'      '
      write(*,*)'USAGE:  ASC2SAC.exe  parameter_list '
      write(*,*)'      '
      write(*,*)'# INPUT ASC FILE FORMAT: '
      write(*,*)'  --------------------------------------------- '
      write(*,*)' * Original time      - yyyymmddhhmiss.ss'
      write(*,*)' * Lat.  Lon.  Dep.   - Event location'
      write(*,*)' * Station name'
      write(*,*)' * Lat.  Lon.  Elv.   - Station location'
      write(*,*)' * Comp.              - Z/N/E'
      write(*,*)' * Data point         - np'
      write(*,*)' * T(1)  A(1)         - Time and amplitude data'
      write(*,*)' * T(2)  A(2) '
      write(*,*)' *   .    .   '
      write(*,*)' *   .    .   '
      write(*,*)' * T(np) A(np)'
	write(*,*)'  --------------------------------------------- '
	write(*,*)' '
      write(*,*)'# parameter_list: '
      write(*,*)' '
	write(*,*)'-C     : Convert Factor (Defualt:1.0) ' 
	write(*,*)'-I     : Input Filename '
	write(*,*)'-O     : Output Filename '
	write(*,*)'D=     : Decimate data point (Default:1)'
      write(*,*)'BP     : apply bandpass filter with f1 & f2 '
      write(*,*)'LP     : apply lowpass filter with f2 corner fre.'
      write(*,*)'HP     : apply lowpass filter with f1 corner fre.'
	write(*,*)'f1,f2= : corner frequency (Hz) for butterworth filter'
	write(*,*)'com=   : com=3 for three components data'
      write(*,*)' '
      write(*,*)'OUTPUT: SAC file (XXXXXX.SAC)'
      write(*,*)' '
      write(*,*)'AUTHOR: Vvn Weian Chao, Oct. 31. 2012'
      write(*,*)' '
      write(*,*)'NOTES: Please, send bugs, comments, improvements'
      write(*,*)' .... to f97224202@ntu.edu.tw;vvnchao@gmail.com '
	write(*,*)' '
	write(*,*)'WEBSITE: http://seismology.gl.ntu.edu.tw/' //
     &'main'
      write(*,*)' '
	write(*,*) 'EXAMPLE: '
      write(*,'(a)') ' @>ASC2SAC -Iexample.txt -C1.0 D=1 BP f1=1. f2=5.'
      write(*,'(a)') ' @>ASC2SAC -IALS_BB.txt com=3'
      STOP ' '
       RETURN
      END SUBROUTINE usage