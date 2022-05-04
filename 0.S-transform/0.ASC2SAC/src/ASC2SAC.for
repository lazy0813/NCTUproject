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
      use MSFLIB
	PARAMETER(npmax=8640000)
       CHARACTER(len=100) ascin*30,ascoup*30,sacf
       CHARACTER(len=200) fn
       CHARACTER*100 par,par1
	 CHARACTER(len=17) otime 
	 CHARACTER(len=5) stn*3,STA*4,stmp*4,secd
	 CHARACTER(len=4) yr,date,time
       CHARACTER(len=3) comp
	 INTEGER yy,hh,mn,day,doy
       INTEGER*4 iy,iday,im,id,ih,imn
	 REAL*8 f,XMAX,XMIN,SUM,MEAN
       REAL sec,rd,f1,f2
	 REAL t(npmax),xc(npmax)
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

       LOGICAL linp,loup,lf,lbp,lhp,llp,move
 
       linp=.false.
	 loup=.false.
       f=1.
	 nd=1


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
       DO i=1,np
	 READ(1,*) t(i),xc(i)
       xc(i)=xc(i)*f
	 IF(xc(i).eq.0.000) xc(i)=xc(i-1)
	 SUM=SUM+xc(i)
       ENDDO
       MEAN=SUM/real(np)
c   De-Mean
       DO i=1,np
       xc(i)=xc(i)-MEAN
	 IF(XMAX.lt.xc(i)) XMAX=xc(i)
	 IF(XMIN.gt.xc(i)) XMIN=xc(i)
	 ENDDO
       write(*,'(2(a,e12.4))') ' MAX. ',XMAX,' MIN. ',XMIN	 
       dt=t(2)-t(1)

	 write(otime(1:4),'(I4.4)') iy
	 write(otime(5:8),'(2I2.2)') im,id
	 write(otime(9:12),'(2I2.2)') hh,mn
	 write(otime(13:17),'(f5.2)') sec

c   APPLY FILTER:
c----------------
c   Bandpass filter with zero phase
c----------------------------------
        IF(lbp) THEN
        call IIRFILT(xc(1:np),np,'BUTTER',4,'BP',f1,f2,dt,2) 
        ELSE IF(lhp) THEN
        call IIRFILT(xc(1:np),np,'BUTTER',4,'HP',f1,f2,dt,2)	  
        ELSE IF(llp) THEN
        call IIRFILT(xc(1:np),np,'BUTTER',4,'LP',f1,f2,dt,2)
	  ELSE
	  ENDIF


       IF(loup) THEN
	 sacf=trim(ascoup)
	  ELSE
c       sacf=trim(stn)//'.'//otime(1:4)//'.'//otime(5:12)
c     &//'.'//trim(comp)//'.sac'
       sacf=trim(ascin)//'.sac'
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
	   IF(pmax.lt.xc(j)) THEN 
	   pmax=xc(j)
	   tmax=real(j-1)*dt
	   ENDIF
	   IF(pmin.gt.xc(j)) THEN 
	   pmin=xc(j)
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

c   PLOT RAW SEISMOGRAM:  psplot.f

       call newdev('CHECK.ps',8)
       call psinit(.false.)
       call setfnt(26)
       call factor(0.7)
       xlen=8.0
       ylen=3.0
       dy0=0.8
       dx0=0.8
       x0=1.0
       y0=8.0   
       t1=xt(1)
       t2=xt(nt)
       dx=real(nint(t2-t1))
       y2=maxval(abs(xp(1:nt)))
       y1=-1.0*y2
       dy=y2-y1
       y0=y0-ylen-dy0
       call setcolr(0.,0.,0.)
       call setlw(0.03)
       call axis(x0,y0,10hTime (sec),-10,xlen,0.,0.0,dx/xlen) !-- X-axis
       call axis(x0,y0,9hAmplitude,10,ylen,90.,y1,dy/ylen) !-- Y-axis

       call setcolr(0.,0.,0.0)
       call setlw(0.03)
       call plot(x0,y0+ylen,3)
       call plot(x0+xlen,y0+ylen,2)
       call plot(x0+xlen,y0,2)
 
       call setlw(0.01)
       move=.true.
       do i=1,np
       x1=xt(i)
       call convert_xy(xp,yp,x1,xp(i),x0,y0,xlen,ylen,t1,t2,y1,y2)
       if(move) then
       call plot(xp,yp,3)
       move=.false.
       else
       call plot(xp,yp,2)
       endif
       enddo
	
       call plotnd
   
c   PLOT RAW SEISMOGRAM:  pgplot.f
c----------------------- 
c      write(*,'(a /)')' COPY: rgb.txt and grfont.dat FOR PGPLOT'
c	   cmd='copy c:\pgplot\* .\'
c         bat=systemQQ(cmd)
c         psraw='CHECK.ps/cps'
c         jstat=PGOPEN(trim(psraw))
c         call pgsubp(1,2)
c	   call pgsls(1)
c	   call pgsci(1)
c	   call pgslw(3)
c	   call pgsch(2.5)
c	   call pgscf(2)
c	   call pgenv(0.,np*dt,REAL(XMIN),REAL(XMAX),2,1)
c         call pglab('Time','Amplitude',trim(fn))
c	   call pgslw(2)
c	   call pgsci(1)
c	   DO k=1,np-1,100
c	   x1=real(k-1)*dt
c	   x2=x1+dt
c	    call pgmove(x1,xc(k))
c	    call pgdraw(x2,xc(k+1))
c	   ENDDO
c         call pgline(nt,xt,xp)
c         call pgend
	   PAUSE '   CHECKING SEISMOGRAM (CHECK.ps)......'

c   Save original time information from SAC file
c   WRITE SAC 

	  fn=trim(sacf)
	  yy=iy
        call greg2doy(im,id,yy,doy)
c        write (*,'(a,I3)') ' Day of year = ', DOY 
	  iday=doy
        imn=mn
        dt=dtt
      write(*,*) 'WRITE: ',trim(sacf),' SAC FILE ' 
      call writesac(fn,nt,dt,xp,iy,iday,ih,imn,sec,stn,comp,e,s)
 


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
      write(*,*)' '
      write(*,*)'OUTPUT: SAC file (XXXXXX.SAC)'
      write(*,*)' '
      write(*,*)'AUTHOR: Vvn Weian Chao, Oct. 31. 2012'
      write(*,*)' '
      write(*,*)'NOTES: Please, send bugs, comments, improvements'
      write(*,*)' .... to f97224202@ntu.edu.tw;vvnchao@gmail.com '
	write(*,*)' '
	write(*,*)'WEBSITE: http://vvnchao.blogspot.tw/' //
     &'main'
      write(*,*)' '
	write(*,*) 'EXAMPLE: '
      write(*,'(a)') ' @>ASC2SAC -I*.txt -C1.0 D=1 BP f1=1. f2=5.'
      STOP ' '
       RETURN
      END SUBROUTINE usage

      subroutine convert_xy(xp,yp,x,y,x0,y0,xlen,ylen,xmin,xmax,ymin,
     &ymax)
       xp=x0+xlen/(xmax-xmin)*(x-xmin)
       yp=y0+ylen/(ymax-ymin)*(y-ymin)
      end subroutine convert_xy