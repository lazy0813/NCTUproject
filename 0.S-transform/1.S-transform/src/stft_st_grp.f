      PROGRAM stft_st_grp
c   Spectrogram using short-time Fourier transform [stft] 
c   & S-Transform [st] for the time-frequency 
c   analysis of a time-series with Gaussian window
c   Also determine the group velocity from maxima of time-frequency amplitude spectra
c
c     JAN. 24. 2011 created by vvn, Ph.D student (vvnchao@ntu.edu.tw;vvnchao@gmail.com)
c     Department of Geosecience, National Taiwan University
c------------------------------------------------------------------------
c   Dimensions & parameters:
c---------------------------
c   Sampling-rate = 200 pt/s, one-day = 17280000
	 PARAMETER(npmax=17280000,nsmax=100)
	 REAL x(npmax),y(npmax)
	 REAL loo(nsmax),laa(nsmax),elv(nsmax),F(nsmax)
	 REAL slon(nsmax),slat(nsmax),selv(nsmax),Fac(nsmax)
	 REAL T0
	 REAL,ALLOCATABLE:: xr(:),yr(:),yf(:)
	 REAL,ALLOCATABLE:: XREAL(:),XIMAG(:)
	 REAL,ALLOCATABLE:: wei(:)
	 INTEGER cday(nsmax)
	 DOUBLE PRECISION GAIN,GAIN1
	 CHARACTER*100 par,par1,inpdir,oupdir,fn,fn1*300,rawf*200
	 CHARACTER*100 oupf*200,oupfg1,oupfg2,oupfall
       CHARACTER*50 staf
	 CHARACTER*3 cday1,cday2,str,comp
	 CHARACTER*1 com
	 CHARACTER*4 sta(nsmax),stn(nsmax)
       LOGICAL LFALSE,LTRUE,EXT0
       DATA LFALSE,LTRUE /.false.,.true./
       LOGICAL lrm,lrtr,ltap,lbp,lhp,llp,lsec,lmin,lhour,lday,lcheck
	 LOGICAL lstft,lst,lfix,lflimit,lfplot,lnor,ldB,lgrp,lbug
	 LOGICAL lLsac,lgain,lsta
c---------------------------
c   For S-Transform
       COMPLEX,ALLOCATABLE:: ctf(:,:),csig(:),cdum(:)
	 REAL twpi,pi
	 REAL dff,dtt
c---------------------------
c   For Command Mode:
c---------------------
       LOGICAL(4) bat
	 CHARACTER*300 cmd
c---------------------------
c   For Plot:
c------------
       INTEGER PGOPEN
	 REAL dx,df
	CHARACTER*10 RX1,RX2,Rf1,Rf2,TA1,TA2,GX,GY,TALL1,TALL2,CUC,Rg1,Rg2
	CHARACTER*10 BX,BC
	 CHARACTER*50 psraw,psf*100,R1,R2,Rg,RA,TColor
	 CHARACTER*500 cmdp
c---------------------------
c   For Color bar:
c-----------------
       REAL clr0,clr1,clr3,clr6,clr5,clr7,clr9


c   INPUTS:
c----------
c   Get number of arguments:
      narg=iargc()
	IF (narg.lt.1) THEN
	  write(*,*)' '
	  write(*,*)'Dimensions: samples: ',npmax,
     &  ' Max. file number: ',nsmax
        write(*,*)' '
	  call usage
      ENDIF

c   Read filename list from station information
      call getarg(1,staf,ic)
c   CLEAN: remove *.list & *.dat file	 
      IF(trim(staf).EQ.'CLEAN') THEN
       cmd=' '
	 cmd='del *.list *.ps *.grd *.cpt'
       bat=systemQQ(cmd)
	 STOP 'REMOVE: *.list *.dat *.ps  *.grd *.cpt .....output file'
	ENDIF
	OPEN(1,file=staf,status='old',IOSTAT=istat)
	IF(istat.NE.0) STOP 'ERROR: STATION INFO FILE IS NOT EXIST!'
	 ns=1
       DO WHILE(.TRUE.)
	  read(1,*,iostat=istat) sta(ns),loo(ns),laa(ns),elv(ns),F(ns)
	  IF(istat.NE.0) EXIT
	  ns=ns+1
	 ENDDO
	 ns=ns-1
	CLOSE(1)
c   DEFAULTS:
c------------
      lrm=.false.
	lrtr=.false.
	ltap=.false.
	lbp=.false.
	lhp=.false.
	llp=.false.
      lrm=.false.
	lsec=.true.
	lmin=.false.
	lhour=.false.
	lday=.false.
	lcheck=.false.
	lstft=.false.
	lst=.false.
	lfix=.false.
	lflimit=.false.
	lfplot=.false.
	lnor=.false.
	ldB=.false.
	lgrp=.false.
	lbug=.false.
	lgain=.false.
      lLsac=.false.
	lsta=.false.
	GAIN=1.0
	inpdir='.\'
	oupdir='.\'
	iday1=1
	iday2=365
	f1=0.
	f2=0.
	grp1=2.0
	grp2=6.0
	TWL=10.0
	Tm=5.0
	dff=0.0
	dtt=0.0
	nfde=1
	ntde=1
	com='Z'

c   Get arguments (optional):
c----------------------------
      DO iarg=2,narg
        call getarg(iarg,par,ic)
        l=len(par)
	  IF(par(1:2).eq.'-I') THEN
	   inpdir=par(3:l)
	  ELSE IF(par(1:2).eq.'-C') THEN
         par1=par(3:l)
         read(par1,*) GAIN
	   lgain=.true.
	  ELSE IF(par(1:2).eq.'-S') THEN
         par1=par(3:l)
         read(par1,*) iday1
	  ELSE IF(par(1:2).eq.'-E') THEN
         par1=par(3:l)
         read(par1,*) iday2
	  ELSE IF(par(1:2).eq.'-T') THEN
         par1=par(3:l)
         read(par1,*) TWL
	  ELSE IF(par(1:2).eq.'-M') THEN
         par1=par(3:l)
         read(par1,*) Tm
	  ELSE IF(par(1:2).eq.'-O') THEN
	   oupdir=par(3:l)
	  ELSE IF(par(1:3).eq.'-Us') THEN
	   lsec=.true.
	  ELSE IF(par(1:3).eq.'-Um') THEN
	   lmin=.true.
	  ELSE IF(par(1:3).eq.'-Uh') THEN
	   lhour=.true.
	  ELSE IF(par(1:3).eq.'-Ud') THEN
	   lday=.true.
	  ELSE IF(par(1:2).eq.'rm') THEN
	   lrm=.true.
	  ELSE IF(par(1:3).eq.'rtr') THEN
	   lrtr=.true.
	  ELSE IF(par(1:3).eq.'tap') THEN
	   ltap=.true.
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
	  ELSE IF(par(1:4).eq.'dtt=') THEN
         par1=par(5:l)
         read(par1,*) dtt
	  ELSE IF(par(1:4).eq.'dff=') THEN
         par1=par(5:l)
         read(par1,*) dff
	  ELSE IF(par(1:3).eq.'fu=') THEN
         par1=par(4:l)
         read(par1,*) fu
	   lflimit=.true.
	  ELSE IF(par(1:3).eq.'fd=') THEN
         par1=par(4:l)
         read(par1,*) fd
	   lflimit=.true.
	  ELSE IF(par(1:5).eq.'grp1=') THEN
         par1=par(6:l)
         read(par1,*) grp1
	  ELSE IF(par(1:5).eq.'grp2=') THEN
         par1=par(6:l)
         read(par1,*) grp2
	  ELSE IF(par(1:5).eq.'Comp=') THEN
         com=par(6:l)
	  ELSE IF(par(1:5).eq.'CHECK') THEN
         lcheck=.true.
	  ELSE IF(par(1:4).eq.'stft') THEN
         lstft=.true.
	  ELSE IF(par(1:2).eq.'st') THEN
         lst=.true.
	  ELSE IF(par(1:3).eq.'FIX') THEN
         lfix=.true.
	  ELSE IF(par(1:5).eq.'FPLOT') THEN
	   lfplot=.true.
	  ELSE IF(par(1:3).eq.'nor') THEN
	   lnor=.true.
	  ELSE IF(par(1:2).eq.'dB') THEN
	   ldB=.true.
	  ELSE IF(par(1:3).eq.'grp') THEN
	   lgrp=.true.
	  ELSE IF(par(1:3).eq.'bug') THEN
	   lbug=.true.
	  ELSE IF(par(1:4).eq.'Lsac') THEN
	   lLsac=.true.
	  ELSE IF(par(1:4).eq.'Nsta') THEN
	   lsta=.true.
	  ELSE 
	   call usage
	  ENDIF
      ENDDO

c   Check inpdir exist or not 
       li=index(inpdir,' ')-1
	 IF(inpdir(li:li).ne.'\') THEN
	 li=li+1
	 inpdir(li:li)='\'
	 ENDIF

       OPEN(10,file=inpdir(1:li)//'tmp',IOSTAT=istat)
       IF(istat.NE.0) STOP 'ERROR: INPUT DIRECTORY IS NOT EXIST...'
	 CLOSE(10,STATUS='delete')

c   Check output directory exist or not
       lo=index(oupdir,' ')-1
	 IF(oupdir(lo:lo).ne.'\') THEN
	 lo=lo+1
	 oupdir(lo:lo)='\'
	 ENDIF

   10  INQUIRE(file=oupdir(1:lo)//'tmp',exist=EXT0)
       IF(EXT0.eq.LTRUE) THEN
	  ELSE
	  OPEN(10,file=oupdir(1:lo)//'tmp',status='unknown',err=11)
   	  CLOSE(10,status='delete')
	 ENDIF
	 GOTO 15
   11  cmd=' '
       cmd='mkdir '//oupdir(1:lo)
       bat=system(cmd)
       GOTO 10
       
c   Find data filename list
c-----------------------------
   15  cmd=' '
	 cmd='dir '//inpdir(1:li)
	 IF(com.eq.'*') THEN
	 cmd=trim(cmd)//' /b /ON > SACFILE.list'
	 ELSE
	 cmd=trim(cmd)//'*'//com//'.sac /b /ON > SACFILE.list'
	 ENDIF
       bat=system(cmd)

c   READ DATA & SAVE STATION INFO:
c---------------------------------
       OPEN(1,file='SACFILE.list',status='old')
c   SAVE COMMAND MODE FOR EACH PLOT:
c-----------------------------------
       OPEN(20,file=oupdir(1:lo)//'runcmd.dat',status='unknown')
	 IF(lfix.EQ.LTRUE) THEN
	 oupfall=oupdir(1:lo)//'fixall.dat'
       OPEN(21,file=oupfall,status='unknown')
	 oupf=oupdir(1:lo)//'minmax.dat'
	 OPEN(22,file=oupf,status='unknown')
	 oupf=oupdir(1:lo)//'stepmax.dat'
	 OPEN(23,file=oupf,status='unknown')
       AllMAX=-1.0E10
	 AllMIN=1.0E10
	 ENDIF
	 nfile=1
       DO WHILE(.TRUE.)
	  fn=' '
	  read(1,*,iostat=istat) fn
	  IF(istat.NE.0) EXIT
	  np=npmax
	  fn1=inpdir(1:li)//trim(fn)
	  y=0.0
	  x=0.0
        call readsac(fn1,np,dt,y,iy,iday,ih,mm,sec,stn(nfile),rd,comp)
	  IF(iday.GE.iday1.AND.iday.LE.iday2.OR.iday.LT.0) THEN
c	  print*, stn(nfile),LEN_TRIM(stn(nfile))
        num_stn=LEN_TRIM(stn(nfile))
	  cday(nfile)=iday
	  T0=0.0
	  T0=real(iday)*86400.0+real(ih)*3600.0+real(mm)*60.0+sec
	  rdist=rd
	  IF(lgrp.EQ.LTRUE.AND.rdist.LT.0.0) THEN
	  STOP 'SAC PROBLEM: DIST undefined' 
	  ENDIF
        write(*,'(/3x,a,a80)')' Reading observed seismogram :',fn

        IF(num_stn.eq.0) THEN
         IF(stn(nfile)(1:1).EQ.' ') stn(nfile)(1:4)=adjustl(fn)
         IF(stn(nfile)(1:1).EQ.'0') stn(nfile)(1:4)=adjustl(fn)
	  ENDIF


	   DO i=1,ns
	    IF(trim(stn(nfile)).EQ.sta(i)) THEN
	    slon(nfile)=loo(i)
	    slat(nfile)=laa(i)
	    selv(nfile)=elv(i)
	    Fac(nfile)=F(i)
	    EXIT
	    ENDIF
	   ENDDO
	   IF(lgain.EQ.LTRUE.OR.lgrp.EQ.LTRUE) Fac(nfile)=GAIN
	   IF(lsta.EQ.LTRUE) Fac(nfile)=GAIN
         IF(Fac(nfile).eq.0.0) STOP 'STA. Info. do not include! 
     &Check Nsta or -C option?'
	   YMAX=0.0
	   DO i=1,np
	    x(i)=real(i-1)*dt
	    y(i)=y(i)*Fac(nfile)
	    IF(YMAX.LT.abs(y(i))) YMAX=abs(y(i))
	   ENDDO

c   Determine MAX. & MIN. frequency
c   Nyquist frequency (Default)
c------------------------------
        IF(lflimit.EQ.LFALSE) THEN
	  fMAX=1.0/dt*0.5
        fMIN=0.0
	  ELSE
	  fMAX=fu
        fMIN=fd
	  ENDIF

c   DATA PROCESS:
c----------------
c   Remove mean
c--------------
        IF(lrm.EQ.LTRUE) THEN
        ymean=0.0
	  DO i=1,np
	  ymean=ymean+y(i)
	  ENDDO
	  ymean=ymean/real(np)
	  YMAX=0.0
	  DO i=1,np
	  y(i)=y(i)-ymean
	  IF(YMAX.LT.abs(y(i))) YMAX=abs(y(i))
	  ENDDO
	  ENDIF
c   Remove linear trend Y=ax+b
c-----------------------------
        IF(lrtr.EQ.LTRUE) THEN
        ALLOCATE(xr(np),yr(np),stat=ierr)       	
        IF(ierr.NE.0) STOP 'ERROR: ALLOCATING DATA ERROR...'
	  xr=0.0
	  yr=0.0
	  a=0.0
	  b=0.0
	  DO i=1,np
	   xr(i)=x(i)
	   yr(i)=y(i)
	  ENDDO
        call lsq(xr,yr,np,a,b)
	  YMAX=0.0
        DO i=1,np
	   y(i)=y(i)-(a*x(i)+b)
	   IF(YMAX.LT.abs(y(i))) YMAX=abs(y(i))
	  ENDDO
        DEALLOCATE(xr,yr)
	  ENDIF
c   5 % taper to beg/end of time-series
c--------------------------------------
        IF(ltap.EQ.LTRUE) THEN
	  mtap=nint(real(np)*0.05)
	   DO i=1,mtap
	    y(i)=y(i)*costap(i,mtap+1)
	    ii=np+1-i
	    y(ii)=y(ii)*costap(i,mtap)
	   ENDDO
	  ENDIF

c   Determine the unit of X-axis
c-------------------------------
         IF(lsec.EQ.LTRUE) uc=1.0
	   IF(lmin.EQ.LTRUE) uc=60.0
	   IF(lhour.EQ.LTRUE)uc=3600.0
	   IF(lday.EQ.LTRUE) uc=86400.0

c   Output RAW record after preprocessing
c----------------------------------------
        IF(lfix.EQ.LFALSE) THEN
	  rawf=oupdir(1:lo)//trim(fn)//'.RAW.dat'
	  OPEN(3,file=rawf,status='unknown')
	  IF(np.gt.360000.and.dtt.gt.0.0) THEN
	   np_j=10
	   ELSE
	   np_j=1
	  ENDIF
	  IF(lLsac.EQ.LTRUE) np_j=100
	  DO i=1,np,np_j
	   write(3,'(e12.6,1x,e12.6)') x(i)/uc,y(i)/YMAX
	  ENDDO
	  CLOSE(3)
	  ENDIF
c   APPLY FILTER:
c----------------
c   Bandpass filter with zero phase
c----------------------------------
        IF(lbp.EQ.LTRUE) THEN
        call IIRFILT(y(1:np),np,'BUTTER',4,'BP',f1,f2,dt,2)        
        ELSE IF(lhp.EQ.LTRUE) THEN
        call IIRFILT(y(1:np),np,'BUTTER',4,'HP',f1,f2,dt,2)	  
        ELSE IF(llp.EQ.LTRUE) THEN
        call IIRFILT(y(1:np),np,'BUTTER',4,'LP',f1,f2,dt,2)
	  ELSE
	  ENDIF
       
c   PLOT RAW SEISMOGRAM:
c----------------------- 
c   COPY rgb.txt and grfont.dat FOR PGPLOT
      write(*,'(a /)')' COPY: rgb.txt and grfont.dat FOR PGPLOT'
	   cmd='copy c:\pgplot\* .\'
c         bat=systemQQ(cmd) 
c-----------------------------------------	           
         IF(lcheck.EQ.LTRUE) THEN
         psraw='RAWCHECK.ps/cps'
         jstat=PGOPEN(trim(psraw))
         call pgsubp(1,3)
         ymax=maxval(abs(y))
         ymin=-ymax
	   xmax=real(np)*dt
	   call pgsls(1)
	   call pgsci(1)
	   call pgslw(3)
	   call pgsch(2.5)
	   call pgscf(2)
	   call pgenv(0.,xmax,ymin,ymax,2,1)
         call pglab('Time','Amplitude',trim(fn))
	   call pgslw(2)
	   call pgsci(1)
	   call pgline(np,x,y)
	   IF(lrtr.EQ.LTRUE) THEN
	   call pgscr(30,1.0,0.0,0.0)
	   call pgsci(30)
	   call pgslw(2)
	   call pgdraw(x(1),a*x(1)+b)
	   call pgmove(x(np),a*x(np)+b)
	   ENDIF
         call pgend
	   PAUSE '   CHECKING RAW SEISMOGRAM......'
         ENDIF

        ENDIF

c   START TO SHORT-TIME FOURIER TRANSFORM:
c-----------------------------------------
        IF(lstft.EQ.LTRUE) THEN
	   write(*,*) '   starting with Short-time Fourier Transform....'
	   IF(lfix.EQ.LFALSE.OR.lfplot.EQ.LTRUE) THEN
	   oupf=oupdir(1:lo)//trim(fn)//'.STFT.dat'
	   OPEN(2,file=oupf,status='unknown')
	   ENDIF
c   Truncate time window length (TWL) for STFT
c---------------------------------------------
        ITWL=int(TWL/dt)
        ITm=int(Tm/dt)
	  ISTEP=int((np-ITWL)/ITm)+1
c   Find the power number of two for Fast Fourier Transform (FFT)
c----------------------------------------------------------------
        DO i=2,20
	  n=2**i
	  IF(n.GE.ITWL) EXIT
	  ENDDO
	  nff=n
	  nff2=n/2
        nu=i
c   Create Gaussian window weighting
c-----------------------------------
        ALLOCATE(wei(ITWL))
	  wei=0.0
        call gau_taper(wei,ITWL,0.4)

        ALLOCATE(XREAL(nff),XIMAG(nff),stat=ierr)
	  ALLOCATE(yf(ITWL),stat=ierr)       	
        IF(ierr.NE.0) STOP 'ERROR: ALLOCATING OUTPUT array ERROR...'
        AMAX=-1.0E10
	  AMIN=1.0E10
        DO i=1,ISTEP
	   XREAL=0.0
	   XIMAG=0.0
	   fre=0.0
	   amp=0.0
	   pha=0.0
	   j1=1+ITm*(i-1)
	   j2=j1+ITWL-1
	   T1=real(j1)*dt
	   T2=real(j2)*dt
c   Calculate the power spectral density (PSD)
c---------------------------------------------
         yf=0.0
         yf=y(j1:j2)
	   yf=yf*yf
	   DO j=1,ITWL
	   XREAL(j)=yf(j)*wei(j)
	   ENDDO
c   Start to FFT
c---------------
         call FFT(XREAL,XIMAG,nff,nu)
	   df=1.0/(real(nff)*dt)
	   IF(df.LT.0.01) THEN 
	   nj=nint(0.01/df)
	   ELSE
	   nj=1
	   ENDIF
c   Write output data
c----------------------------------         
	   DO k=1,nff2,nj
	   fre=real(k-1)*df
	   amp=sqrt(XREAL(k)**2+XIMAG(k)**2)/real(nff)
	   pha=atan2(XIMAG(k),XREAL(k))
	   IF(fre.GE.fMIN.AND.fre.LE.fMAX) THEN
	   IF(AMAX.LT.10.0*log10(abs(amp))) AMAX=10.0*log10(abs(amp))
	   IF(AMIN.GT.10.0*log10(abs(amp))) AMIN=10.0*log10(abs(amp))
	   IF(AllMAX.LT.10.0*log10(abs(amp))) AllMAX=10.0*log10(abs(amp))
	   IF(AllMIN.GT.10.0*log10(abs(amp))) AllMIN=10.0*log10(abs(amp))
	   IF(lfix.EQ.LFALSE.OR.lfplot.EQ.LTRUE) THEN
	   write(2,99) 
     &(real(i)-0.5)*Tm,fre,10.0*log10(abs(amp))
	   ENDIF
	    IF(lfix.EQ.LTRUE) THEN
	   write(21,99) 
     &(real(i)-0.5)*Tm+T0,fre,10.0*log10(abs(amp))
	    ENDIF
	   ENDIF
	   ENDDO
   99 FORMAT(e12.6,f10.5,2(1x,e12.6))   
        IF(lfix.EQ.LTRUE) THEN
        write(23,77) (real(i)-0.5)*Tm+T0,AMAX
	  ENDIF
   77 FORMAT(e12.6,1x,f10.4)
	  ENDDO
        DEALLOCATE(XREAL,XIMAG,yf)
	  DEALLOCATE(wei)
        CLOSE(2)
        IFREQ=(fMAX-fMIN)/df

        IF(lbug) THEN
        write(*,'(a,i)') '    Total window number  :',ISTEP
        write(*,'(a,i)') '    Total freq. number   :',IFREQ
        write(*,'(a,f6.4)')  '    Sampling rate        :',dt
        write(*,'(a,f6.4)')  '    Frequency interval   :',df
	  ENDIF

c   START TO S-TRANSFORM:
c-----------------------------------------
        ELSE IF(lst.EQ.LTRUE.OR.lgrp.EQ.LTRUE) THEN
	   write(*,*) '   starting with S-Transform....'
c   For Long SAC
c---------------
        IF(lLsac.EQ.LTRUE) THEN
	  write(*,*) ' Processing ..... ',trim(fn)
          oupf=oupdir(1:lo)//trim(fn)//'.STAll.dat'
	    OPEN(2,file=oupf,status='unknown')	   
c   Consider 1-hr time-window
c----------------------------
          nh=int(3600./dt)
c   Find the power number of two for Fast Fourier Transform (FFT)
c----------------------------------------------------------------
        DO i=2,30
	  n=2**i
	  IF(n.GE.nh) EXIT
	  ENDDO
	  nff=n
	  nff2=n/2
        nu=i
        df=1.0/(dt*real(n))

        ALLOCATE(csig(n),cdum(n),stat=ierr)       	
        IF(ierr.NE.0) STOP 'ERROR: ALLOCATING COMPLEX array ERROR...'
c   Do Loop for each 1-hr data with 50% overlap
         nh2=nh/2
         mhr=np/nh2
         AMAX=-1.0E10
	   AMIN=1.0E10
	  DO mh=0,mhr-2
	  write(*,'(f5.1,a)') mh*0.5,' Hour-Step!' 
         DO nst=1,nh
	   csig(nst)=cmplx(y(mh*nh2+nst),0.0)
         ENDDO
	   DO nst=nh+1,n
	   csig(nst)=cmplx(0.0,0.0)
         ENDDO
c   Start to FFT
c---------------
         rn=1.0
	   call clogc(nu,csig,rn,dt)
c   Start frequency loop
c-----------------------
         pi=4.0*atan(1.0)
	   twpi=8.0*atan(1.0)
	   cycl=2.0
	   fact=cycl/2.0
         pp=2.0*pi*pi*fact*fact
c  limit time-freq matrix for output
c-----------------------------------
c   providing dff
         IF(dff.GT.0.0) THEN
	    IF(dff.LT.df) dff=df
	    ELSE
	    dff=df
         ENDIF
	    nfde=nint(dff/df)        
          dff=nfde*df
	    IF(lflimit.EQ.LTRUE) THEN
	    nfu=nint(fu/df)
	    nfd=nint(fd/df)
	    npf=nint((nfu-nfd)/real(nfde))+1
	    ELSE
	    nfu=nff2
	    nfd=1
          npf=nint(nff2/real(nfde))+1
	    ENDIF
	    ffre1=real(nfd)*df
c   providing dtt
         IF(dtt.GT.0.0) THEN
	    IF(dtt.LT.dt) dtt=dt
	    ELSE
	    dtt=dt
         ENDIF
	    ntde=nint(dtt/dt)        
          dtt=ntde*dt
          npt=nint(nh/real(ntde))+1
	   IF(lbug) THEN
         write(*,'(a,4f12.6)') '    dt,dtt,df,dff: ',dt,dtt,df,dff
         write(*,'(a,4i8)') '    npt,npf,ntde,nfde: ',npt,npf,ntde,nfde
	   ENDIF
         ALLOCATE(ctf(npt,npf),stat=ierr)       	
         IF(ierr.NE.0) STOP 'ERROR: ALLOCATING time-fre array ERROR...'

c   Zero mean time-series assumed
          ntn=0
          DO nst=1,nh,ntde
	    ntn=ntn+1
          ctf(ntn,1)=cmplx(1.d-20,1.d-20)
c	    print*, nst,ntde,nh
	    ENDDO

c   Remaining positive frequencies
	   nfn=1

c   write percentage %
         n10=npf*0.10
         n25=npf*0.25
	   n50=npf*0.50
	   n75=npf*0.75
	   n99=npf*0.99
	   ttime1=0.
	   ntd=1
	   ntu=nh

         DO nsf=2,nfu,nfde
         nfn=nfn+1
	   IF(nfn.gt.npf) exit
	   IF(nfn.eq.n10) write(*,*) '   done with 10% ...'
	   IF(nfn.eq.n25) write(*,*) '   25% ...'
	   IF(nfn.eq.n50) write(*,*) '   50% ...'
	   IF(nfn.eq.n75) write(*,*) '   75% ...'
	   IF(nfn.eq.n99) write(*,*) '   100% completed'
	   	   	   	   	   
	   nf1m=nsf-1
	   ppnf=-pp/real(nf1m)/real(nf1m)
c   zero negative frequencies
          DO mfr=1,n
	    cdum(mfr)=cmplx(0.0,0.0)
	    ENDDO
	    DO mfr=1,n
	    mf1=mfr-1
	    argp=ppnf*mf1*mf1
	    argn=ppnf*(n-mf1)*(n-mf1)
	    IF(argp.GT.-25.or.argn.GT.-25) THEN
	    mn=mfr+nf1m
	    IF(mn.GT.n) mn=mn-n
c   multiply with localizing Gaussian (generalized voice Gaussian)
          rr=exp(argp)+exp(argn)
	    cdum(mfr)=csig(mn)*rr
	    ENDIF
	    ENDDO
c   perform ifft to obtain temporal spectral localizations
          rn=-1.0
          call clogc(nu,cdum,rn,dt)
c   store in matrix
          ntn=0
          DO nst=ntd,ntu,ntde
	    ntn=ntn+1
 	    ctf(ntn,nfn)=2.0*cdum(nst)
	    aa=cabs(ctf(ntn,nfn))
c   Find max. & min. amplitude value in spectrogram
          amp=aa*aa
	    IF(ldB.EQ.LTRUE) amp=10.0*log10(amp)
	    IF(AMAX.LT.amp) AMAX=amp
	    IF(AMIN.GT.amp) AMIN=amp       
	    ENDDO      
	   ENDDO

      IF(lnor.EQ.LTRUE)STOP 'ERROR: CAN NOT USING NORMALIZING FOR LSAC!'

c   Write output data
c--------------------
         nfn=0
         DO nsf=nfd,nfu,nfde
	    nfn=nfn+1
	    IF(nfn.gt.npf) exit
	    IF(nfn.eq.n10) write(*,*) '   Outputing with 10% ...'
	    IF(nfn.eq.n25) write(*,*) '   25% ...'
	    IF(nfn.eq.n50) write(*,*) '   50% ...'
	    IF(nfn.eq.n75) write(*,*) '   75% ...'
	    IF(nfn.eq.n99) write(*,*) '   100% completed'
	    ntn=0
	    DO nst=ntd,ntu,ntde
	    ntn=ntn+1
	    aa=cabs(ctf(ntn,nfn))
          amp=aa*aa
	    ttime=mh*1800.+ttime1+real(ntn-1)*dtt
	    ffre=ffre1+real(nfn-1)*dff
	    IF(ldB.EQ.LTRUE) amp=10.0*log10(amp)
	    write(2,39) ttime/uc,ffre,amp
	    ENDDO
	   ENDDO 
   39 FORMAT(e12.6,f10.5,1x,e12.6)
        DEALLOCATE(ctf)
        npt=ntn
        npf=nfn
	  IF(lbug) THEN
        write(*,'(a,2(1x,e12.6))') '    XMAX & XMIN:  ',
     &mh*1800.+ntd*dt,mh*1800.+ntu*dt
        write(*,'(a,i)') '    Time-step in time-fre domain  :',npt
        write(*,'(a,i)') '    Freq-step in time-fre domain  :',npf
        write(*,'(a,2f8.4)')  '    Sampling rate & decimate dtt:',dt,dtt
        write(*,'(a,2f8.4)')  '    Freq interval & decimate dff:',df,dff
        ENDIF

        write(*,*) '   MAX.: ',AMAX,' MIN: ',AMIN
        ENDDO
        DEALLOCATE(csig,cdum)
        CLOSE(2)

	  ELSE
	    IF(lfix.EQ.LFALSE.OR.lfplot.EQ.LTRUE) THEN
	    oupf=oupdir(1:lo)//trim(fn)//'.ST.dat'
	    OPEN(2,file=oupf,status='unknown')
	    ENDIF
c   Find the power number of two for Fast Fourier Transform (FFT)
c----------------------------------------------------------------
        DO i=2,30
	  n=2**i
	  IF(n.GE.np) EXIT
	  ENDDO
	  nff=n
	  nff2=n/2
        nu=i
        df=1.0/(dt*real(n))

        ALLOCATE(csig(n),cdum(n),stat=ierr)       	
        IF(ierr.NE.0) STOP 'ERROR: ALLOCATING COMPLEX array ERROR...'
        DO nst=1,np
	  csig(nst)=cmplx(y(nst),0.0)
        ENDDO
	  DO nst=np+1,n
	  csig(nst)=cmplx(0.0,0.0)
        ENDDO
c   Start to FFT
c---------------
        rn=1.0
	  call clogc(nu,csig,rn,dt)

c   Start frequency loop
c-----------------------
        pi=4.0*atan(1.0)
	  twpi=8.0*atan(1.0)
	  cycl=2.0
	  fact=cycl/2.0
        pp=2.0*pi*pi*fact*fact
c  limit time-freq matrix for output
c-----------------------------------
c   providing dff
        IF(dff.GT.0.0) THEN
	   IF(dff.LT.df) dff=df
	   ELSE
	   dff=df
        ENDIF
	   nfde=nint(dff/df)        
         dff=nfde*df
	   IF(lflimit.EQ.LTRUE) THEN
	   nfu=nint(fu/df)
	   nfd=nint(fd/df)
	   npf=nint((nfu-nfd)/real(nfde))+1
	   ELSE
	   nfu=nff2
	   nfd=1
         npf=nint(nff2/real(nfde))+1
	   ENDIF
	   ffre1=real(nfd)*df
c   providing dtt
        IF(dtt.GT.0.0) THEN
	   IF(dtt.LT.dt) dtt=dt
	   ELSE
	   dtt=dt
        ENDIF
	   ntde=nint(dtt/dt)        
         dtt=ntde*dt
         npt=nint(np/real(ntde))+1
	  IF(lbug) THEN
        write(*,'(a,4f12.6)') '    dt,dtt,df,dff: ',dt,dtt,df,dff
        write(*,'(a,4i8)') '    npt,npf,ntde,nfde: ',npt,npf,ntde,nfde
	  ENDIF
        ALLOCATE(ctf(npt,npf),stat=ierr)       	
        IF(ierr.NE.0) STOP 'ERROR: ALLOCATING time-fre array ERROR...'
  	          
c   Zero mean time-series assumed
        ntn=0
        DO nst=1,np,ntde
	  ntn=ntn+1
        ctf(ntn,1)=cmplx(1.d-20,1.d-20)
c	  print*, nst,ntde,np
	  ENDDO

c   Remaining positive frequencies
	  nfn=1
        AMAX=-1.0E10
	  AMIN=1.0E10
c   write percentage %
        n10=npf*0.10
        n25=npf*0.25
	  n50=npf*0.50
	  n75=npf*0.75
	  n99=npf*0.99

	  IF(lgrp.EQ.LTRUE) THEN
	  grpmin=100.0
	  grpmax=0.0
	  ttime1=rdist/grp2
	  ttime2=rdist/grp1
	  ntd=nint(ttime1/dt)
	  ntu=nint(ttime2/dt)
	  write(*,'(a,f10.3)') '    SAC DIST: ',rdist
	  write(*,'(2(a,f10.3))') '    Time Window: ',ttime1,'   -',ttime2
	  ELSE
	  ttime1=0.
	  ntd=1
	  ntu=np
	  ENDIF

        DO nsf=2,nfu,nfde

         nfn=nfn+1
	   IF(nfn.gt.npf) exit
	   IF(nfn.eq.n10) write(*,*) '   done with 10% ...'
	   IF(nfn.eq.n25) write(*,*) '   25% ...'
	   IF(nfn.eq.n50) write(*,*) '   50% ...'
	   IF(nfn.eq.n75) write(*,*) '   75% ...'
	   IF(nfn.eq.n99) write(*,*) '   100% completed'
	   	   	   	   	   
	   nf1m=nsf-1
	   ppnf=-pp/real(nf1m)/real(nf1m)
c   zero negative frequencies
         DO mfr=1,n
	    cdum(mfr)=cmplx(0.0,0.0)
	   ENDDO
	   DO mfr=1,n
	    mf1=mfr-1
	    argp=ppnf*mf1*mf1
	    argn=ppnf*(n-mf1)*(n-mf1)
	    IF(argp.GT.-25.or.argn.GT.-25) THEN
	    mn=mfr+nf1m
	    IF(mn.GT.n) mn=mn-n
c   multiply with localizing Gaussian (generalized voice Gaussian)
          rr=exp(argp)+exp(argn)
	    cdum(mfr)=csig(mn)*rr
	    ENDIF
	   ENDDO
c   perform ifft to obtain temporal spectral localizations
          rn=-1.0
          call clogc(nu,cdum,rn,dt)
c   store in matrix
         ntn=0
         DO nst=ntd,ntu,ntde
	   ntn=ntn+1
 	   ctf(ntn,nfn)=2.0*cdum(nst)
	   aa=cabs(ctf(ntn,nfn))
c   Find max. & min. amplitude value in spectrogram
         amp=aa*aa
	   IF(ldB.EQ.LTRUE) amp=10.0*log10(amp)
	   IF(AMAX.LT.amp) AMAX=amp
	   IF(AMIN.GT.amp) AMIN=amp       
	   ENDDO      

	  ENDDO


c   Write output data
c   Normalized spectrogram
      AAMIN=AMIN
      IF(lnor.EQ.LTRUE) THEN
	ADIF=AMAX-AMIN
	ELSE
	ADIF=1.0
	AAMIN=0.0
	ENDIF

	  nfn=0
       DO nsf=nfd,nfu,nfde
	  nfn=nfn+1
	    IF(nfn.gt.npf) exit
	    IF(nfn.eq.n10) write(*,*) '   Outputing with 10% ...'
	    IF(nfn.eq.n25) write(*,*) '   25% ...'
	    IF(nfn.eq.n50) write(*,*) '   50% ...'
	    IF(nfn.eq.n75) write(*,*) '   75% ...'
	    IF(nfn.eq.n99) write(*,*) '   100% completed'
	  ntn=0
	   DO nst=ntd,ntu,ntde
	   ntn=ntn+1
	   aa=cabs(ctf(ntn,nfn))
         amp=aa*aa
	   ttime=ttime1+real(ntn-1)*dtt
	   ffre=ffre1+real(nfn-1)*dff
	   IF(ldB.EQ.LTRUE) amp=10.0*log10(amp)
	   IF(lgrp.EQ.LTRUE) THEN
         IF(ntn.eq.1) ttime=ttime1+dtt
	   write(2,33) 
     &ttime,ffre,(amp-AAMIN)/ADIF,rdist/ttime
	   IF(grpmin.gt.rdist/ttime)grpmin=rdist/ttime
	   IF(grpmax.lt.rdist/ttime)grpmax=rdist/ttime
	   ELSE
	   write(2,34) 
     &ttime/uc,ffre,(amp-AAMIN)/ADIF
	   ENDIF

	   ENDDO
	  ENDDO 
   33 FORMAT(e12.6,f10.5,1x,e12.6,1x,e12.6) 
   34 FORMAT(e12.6,f10.5,1x,e12.6)
        DEALLOCATE(csig,cdum)
	  IF(lgrp.EQ.LFALSE) DEALLOCATE(ctf) 
        CLOSE(2)
        npt=ntn
        npf=nfn
	  IF(lbug) THEN
        write(*,'(a,2(1x,e12.6))') '    XMAX & XMIN:  ',ntd*dt,ntu*dt
        write(*,'(a,i)') '    Time-step in time-fre domain  :',npt
        write(*,'(a,i)') '    Freq-step in time-fre domain  :',npf
        write(*,'(a,2f8.4)')  '    Sampling rate & decimate dtt:',dt,dtt
        write(*,'(a,2f8.4)')  '    Freq interval & decimate dff:',df,dff
        ENDIF
 
        ENDIF

c   ENDIF FOR Long SAC 
        ENDIF

	   IF(lgrp.EQ.LTRUE) THEN
         write(*,*) '   starting with saving dispersion curve....'
	   oupfg1=oupdir(1:lo)//trim(fn)//'.disper_sad.dat'
	   OPEN(2,file=oupfg1,status='unknown')
c   Drive dispersion curve from finding maxima value with saddle    
          ntn=0	         
         DO nst=ntd,ntu,ntde 
	    ntn=ntn+1
	    nfn=0
	     DO nsf=nfd,nfu,nfde
	       nfn=nfn+1
             ntn1=ntn-2
	       ntn2=ntn-1
	       ntn3=ntn+1
	       ntn4=ntn+2
	       nfn1=nfn-2
	       nfn2=nfn-1
	       nfn3=nfn+1
	       nfn4=nfn+2
	       IF(ntn1.lt.1)   ntn1=1
	       IF(ntn2.lt.1)   ntn2=1
	       IF(ntn3.gt.npt) ntn3=npt
	       IF(ntn4.gt.npt) ntn4=npt
	       IF(nfn1.lt.1)   nfn1=1
	       IF(nfn2.lt.1)   nfn2=1
	       IF(nfn3.gt.npf) nfn3=npf
	       IF(nfn4.gt.npf) nfn4=npf
             ampa=cabs(ctf(ntn,nfn1))
             ampb=cabs(ctf(ntn,nfn2))
             ampc=cabs(ctf(ntn,nfn3))
             ampd=cabs(ctf(ntn,nfn4))
             amp1=cabs(ctf(ntn1,nfn))
             amp2=cabs(ctf(ntn2,nfn))
             amp3=cabs(ctf(ntn3,nfn))
             amp4=cabs(ctf(ntn4,nfn))        
             aa=cabs(ctf(ntn,nfn))
	       IF(aa.gt.amp2.and.aa.gt.amp3) THEN
	         IF((amp4-amp3).le.0.and.(amp1-amp2).le.0) THEN
               amp=aa*aa
	         ttime=ttime1+real(ntn-1)*dtt
	         ffre=ffre1+real(nfn-1)*dff
	         write(2,35) ttime,ffre,rdist/ttime,(amp-AAMIN)/ADIF
	         ENDIF
	       ELSE IF(aa.gt.ampb.and.aa.gt.ampc) THEN
               amp=aa*aa
	         ttime=ttime1+real(ntn-1)*dtt
	         ffre=ffre1+real(nfn-1)*dff
	         write(2,35) ttime,ffre,rdist/ttime,(amp-AAMIN)/ADIF
	       ENDIF		        
	     ENDDO 
          ENDDO
         CLOSE(2)

	   oupfg2=oupdir(1:lo)//trim(fn)//'.disper_contf.dat'
	   OPEN(2,file=oupfg2,status='unknown')
c   Search for constant frequency maxima:
	     nfn=0
		 DO nsf=nfd,nfu,nfde
		 nfn=nfn+1
	     ffre=ffre1+real(nfn-1)*dff	
		 rrmax=-1.0E10
	      ntn=0
            DO nst=ntd,ntu,ntde 
	      ntn=ntn+1
	      ttime=ttime1+real(ntn-1)*dtt	
		  IF(ntn.eq.0) ttime=ttime1+dtt	 
             aa=cabs(ctf(ntn,nfn))
		   amp=aa*aa
		   IF(amp.gt.rrmax) THEN
	        rrmax=amp
	        rtmax=ttime
		   ENDIF
		  ENDDO
	      write(2,35) rtmax,ffre,rdist/rtmax,(rrmax-AAMIN)/ADIF		  		    
		 ENDDO          
         CLOSE(2)
         DEALLOCATE(ctf)
         ENDIF
   35 FORMAT(e12.6,f10.5,1x,f8.4,1x,e12.6)


      IF(lnor.EQ.LTRUE) THEN
	AMAX=1.0
	AMIN=0.0
	ENDIF

c   START TO CREATE COLOR BAR:
c------------------------------
	OPEN(2,file='cpt.cpt',status='unknown')
      IF(ldB) THEN
      clr5=AMIN+(AMAX-AMIN)*0.5
      clr7=AMIN+(AMAX-AMIN)*0.7
	clr9=AMIN+(AMAX-AMIN)*0.9
      WRITE(2,'(2(e16.4,a))') clr5,' 0 0 205',clr7,' 0 250 250 '
      WRITE(2,'(2(e16.4,a))') clr7,' 0 250 250',clr9,' 250 250 0 '
      WRITE(2,'(2(e16.4,a))') clr9,' 250 250 0',AMAX,' 200 0 0 '
	ELSE
      clr0=AMIN+(AMAX-AMIN)*0.01
      clr1=AMIN+(AMAX-AMIN)*0.1
	clr5=AMIN+(AMAX-AMIN)*0.5
      WRITE(2,'(2(e16.4,a))') clr0,' 0 0 205',clr1,' 0 250 250 '
      WRITE(2,'(2(e16.4,a))') clr1,' 0 250 250',clr5,' 250 250 0 '
      WRITE(2,'(2(e16.4,a))') clr5,' 250 250 0',AMAX,' 200 0 0 '
	ENDIF
      CLOSE(2)
c   START TO PLOT RESULT:
c------------------------
c   Time window range
c--------------------
       IF(lgrp.EQ.LTRUE) THEN
	 XMAX=ttime2
	 XMIN=ttime1
	 ELSE
       XMAX=maxval(x(1:np))/uc
       XMIN=minval(x(1:np))/uc
	 ENDIF
	 write(RX1,'(i10)') int(XMIN)
	 write(RX2,'(i10)') int(XMAX)
c   Nyquist frequency (Default)
c------------------------------
       IF(lflimit.EQ.LFALSE) THEN
	 fMAX=1.0/dt*0.5
       fMIN=0.0
	 ELSE
	 fMAX=fu
       fMIN=fd
	 ENDIF
	 write(Rf1,'(e10.4)') fMIN
	 write(Rf2,'(e10.4)') fMAX
c   group velocity range
c-----------------------
	 write(Rg1,'(e10.4)') grp1
	 write(Rg2,'(e10.4)') grp2       

c   Spectral amplitude 
c---------------------
       IF(lstft.EQ.LTRUE) THEN
	  write(TA1,'(e10.4)') AMIN*0.80
	  write(TA2,'(e10.4)') AMAX
c   Find grid size for plot (eg. xyz2grd -Idx/dy)
c-------------------------------------------------
        dx=Tm
	  IF(df.LT.0.01) df=0.01
	  dy=df
	  write(GX,'(e10.4)') dx
	  write(GY,'(e10.4)') dy*1.1
	  ELSE IF(lst.EQ.LTRUE) THEN
	  write(TA1,'(e10.4)') AMIN
	  write(TA2,'(e10.4)') AMAX
c   Find grid size for plot (eg. xyz2grd -Idx/dy)
c-------------------------------------------------
        dx=dtt
	  dy=dff
	  write(GX,'(e10.4)') dx
	  write(GY,'(e10.4)') dy*1.1
C   Find interval for plot (eg. -B?)
        write(BX,'(e10.4)') XMAX/4.0
	  write(BC,'(e10.4)') (AMAX-AMIN)/4.0
    	 ENDIF

        R1='-R'//trim(adjustl(RX1))//'/'//trim(adjustl(RX2))//'/-1/1'
	  R2='-R'//trim(adjustl(RX1))//'/'//trim(adjustl(RX2))//
     &'/'//trim(adjustl(Rf1))//'/'//trim(adjustl(Rf2))
	  Rg='-R'//trim(adjustl(Rf1))//'/'//trim(adjustl(Rf2))//
     &'/'//trim(adjustl(Rg1))//'/'//trim(adjustl(Rg2))
	  TColor='-T'//trim(adjustl(TA1))//'/'//trim(adjustl(TA2))

c   Output postscript name
c-------------------------
	  nfile=nfile+1
      IF(ichar(stn(nfile-1)(4:4)).eq.0) stn(nfile-1)(4:4)='0'
	IF(comp(1:1).NE.' ') THEN 
      psf=oupdir(1:lo)//trim(adjustl(stn(nfile-1)))
     &//"_"//trim(comp)//'.ps'
	ELSE
      psf=oupdir(1:lo)//trim(adjustl(stn(nfile-1)))
     &//"_"//com//'.ps'
	ENDIF
	   
        cmdp=' '
	  IF(lfix.EQ.LFALSE) THEN
	   IF(lgrp.EQ.LTRUE) THEN
	    IF(comp(1:1).NE.' ') THEN 
        psf=oupdir(1:lo)//trim(adjustl(stn(nfile-1))
     &)//"_"//trim(comp)//'_grp.ps'
	    ELSE
        psf=oupdir(1:lo)//trim(adjustl(stn(nfile-1))
     &)//"_"//com//'_grp.ps'
	    ENDIF
        cmdp='plotgrp.bat '//trim(rawf)//' '//trim(oupf)//' '//
     &  trim(R1)//' '//trim(Rg)//' '//trim(TColor)//' '//trim(GY)//' '//
     &  '0.1'//' '//trim(psf)//' '//trim(oupfg2)
	  write(20,'(a)') trim(cmdp)
c   COPY plotgrp.bat and plotscript.bat FOR GMTPLOT
      write(*,'(a)')' COPY: plotgrp.bat FOR GMTPLOT'
	   cmd='copy C:\plotscript\plotgrp.bat .\'
c         bat=systemQQ(cmd) 
         IF(lfplot.EQ.LTRUE) bat=system(cmdp)
	   ELSE
	  IF(comp(1:1).NE.' ') THEN         
      psf=oupdir(1:lo)//trim(adjustl(stn(nfile-1)))//"_"
     &//trim(comp)//'.ps'
	  ELSE
        psf=oupdir(1:lo)//trim(adjustl(stn(nfile-1)))//"_"//com//'.ps'	  
	  ENDIF
        cmdp='plotscript.bat '//trim(rawf)//' '//trim(oupf)//' '//
     &  trim(R1)//' '//trim(R2)//' '//trim(TColor)//' '//trim(GX)//' '//
     &  trim(GY)//' '//trim(psf)//' '//trim(BX)//' '//trim(BC)
	  write(20,'(a)') trim(cmdp)
c   COPY plotgrp.bat and plotscript.bat FOR GMTPLOT
      write(*,'(a)')' COPY:  plotscript.bat FOR GMTPLOT'
	   cmd='copy C:\plotscript\plotscript.bat .\'
c         bat=systemQQ(cmd) 
         IF(lfplot.EQ.LTRUE) bat=system(cmdp)
	   ENDIF
	  ELSE
	  write(str,'(I3.3)') nfile
	  IF(comp(1:1).NE.' ') THEN 
	  psf=oupdir(1:lo)//trim(adjustl(stn(nfile-1)))
     &//"_"//trim(comp)//str//'.ps'
	  ELSE
	  psf=oupdir(1:lo)//trim(adjustl(stn(nfile-1)))
     &//"_"//com//str//'.ps'
	  ENDIF
        cmdp='plotfix.bat '//trim(oupf)//' '//trim(R2)//' '
     &//trim(TColor)//' '//trim(GX)//' '//trim(GY)//' '//trim(psf)
	  write(20,'(a)') trim(cmdp)
         IF(lfplot.EQ.LTRUE) bat=system(cmdp)
	  ENDIF

c   ENDDO FOR READ SAC FILE NAME

	 ENDDO
       CLOSE(1)

	 IF(lfix.EQ.LTRUE) THEN
	 u=86400.0
	 write(22,88) 'XMIN: ',cday(1)*u,' XMAX: ',(cday(nfile)+1)*u
       write(22,88) 'fMIN: ',fMIN,' fMAX: ',fMAX
	 write(22,88) 'AMIN: ',AllMIN,' AMAX: ',AllMAX
	 write(22,88) 'dx  : ',dx,' dy  : ',dy
	 write(TALL1,'(i10)') int(AllMIN*0.80)
	 write(TALL2,'(i10)') int(AllMAX)
	 write(GX,'(e10.4)') dx/86400.
	 write(cday1,'(I3.3)') cday(1)
	 write(cday2,'(I3.3)') cday(nfile)+1
	 write(CUC,'(f10.4)') uc
	RA='-R'//trim(cday1)//'/'//trim(cday2)//
     &'/'//trim(adjustl(Rf1))//'/'//trim(adjustl(Rf2))
	TColot=' '
	TColor='-T'//trim(adjustl(TALL1))//'/'//trim(adjustl(TALL2))
	  cmdp=' '
        cmdp='plotall.bat '//trim(oupfall)//' '//trim(RA)//' '
     &//trim(TColor)//' '//trim(GX)//' '//trim(GY)//' '//'All.ps'//
     &' '//trim(CUC)
	  write(20,'(a)') trim(cmdp)
c        bat=system(cmdp)
	 CLOSE(21)
	 CLOSE(22)
	 CLOSE(23)
	 ENDIF
   88 FORMAT(2(a,1x,e10.4))
       
       CLOSE(20)
       IF(nfile.LT.1) STOP 'ERROR: NO DATA READING IN......'
       nsta=nfile-1

	  write(*,'(/a,i3.3)') '    Total file number    :',nsta

c-----------------------------------------

	END PROGRAM stft_st_grp


c--------------------------------------------------------------
      SUBROUTINE usage
	write(*,*)'------System requirements------------------------' 
      write(*,*)'Make sure that your system has GMT4.0 & gsview32 '
      write(*,*)'Set the environment variable about them'
	write(*,*)'-------------------------------------------------'
      write(*,*)'      '
      write(*,*)'USAGE:  stft_st_grp.exe filename parameter_list '
      write(*,*)'      '
      write(*,*)'# filename of station information: '
      write(*,*)'  format: STAID LON. LAT. ELV(m) Fac(?/count)'
	write(*,*)'  this file is necessary for no Staion info' // 
     & ' in SAC header'
	write(*,*)'  data size & format (less than one-day SAC file) '
	write(*,*)'  npmax=17280000 for sps:200 Hz & one-day'
	write(*,*)'  eg. XXXX.YYYY.MMDD.c.SAC'
	write(*,*)'  c is component of record' 
      write(*,*)'      '
      write(*,*)'# parameter_list: '
      write(*,*)' '
      write(*,*)'-I     : the path of input data (Default: .\)'
      write(*,*)'-C     : convert factor for instrument(eg. cm/s/count)'
	write(*,*)'-S     : Julian day of starting time in data ' //
     & ' (eg. 001 )'
	write(*,*)'-E     : Julian day of ending time in data  ' //
     & ' (eg. 365 )' 
	write(*,*)'-T     : time window length  ' //
     &  '(Default: 10 sec)'//' is as same as Gaussian window'
	write(*,*)'-M     : moving window length (unit: sec)'
      write(*,*)'         (eg. suggest 50 % overlap, Default: 5 sec)'
      write(*,*)'-O     : the path of output data (Default: .\)'
	write(*,*)'-U     : the unit of time axis for' //
     & ' Time-frequency diagram'
	write(*,*)'         s/m/h/d, s:sec m:minute h:hour d:day' //
     & ' (Default: sec)'
	write(*,*)'rm     : remove mean from input data '
	write(*,*)'rtr    : remove linear trend from input data '
	write(*,*)'tap    : apply 5% taper in begin/end of time-series '
      write(*,*)'BP     : apply bandpass filter with f1 & f2 '
      write(*,*)'LP     : apply lowpass filter with f2 corner fre.'
      write(*,*)'HP     : apply lowpass filter with f1 corner fre.'
	write(*,*)'f1,f2= : corner frequency (Hz) for butterworth filter'
	write(*,*)'dtt=   : decimate time-interval output for S-Transform'
	write(*,*)'dff=   : decimate freq-interval output for S-Transform'
	write(*,*)'fd,fu= : frequency range in Hz to limit output'
	write(*,*)'         (Default: all frequency samples)'
	write(*,*)'Comp=  : Z/N/E/* (Default:Z; * is for *.sac)'
	write(*,*)'CLEAN  : remove *.list, *.dat & *.ps output file'
	write(*,*)'CHECK  : plot raw seismogram for user checking' //
     & ' (Default: No plot RAWCHECK.ps)'
	write(*,*)'Lsac   : For Each Long One-day SAC File ' //
     & ' (Default: For Short SAC File)'
	write(*,*)'Nsta   : do not has sta info in stainfo file ' //
     & ' (Default: For Short SAC File)'
	write(*,*)'stft   : short time fourier transform' //
     & ' (Default: only waveform display)'
	write(*,*)'st     : S-Transform'
	write(*,*)'FIX    : several file only for one station with one-day
     &	 SACfile'
      write(*,*)'         (Default: several file & station free 
     &with record length less than one hour)'
	write(*,*)'FPLOT  : plot time-freq result of each file for 
     &FIX mode or single seismogram (Default: no plot)'
	write(*,*)'nor    : normalize output spectrogram to 1. for 
     &S-Transform'
	write(*,*)'dB    : dB unit (10*log(amp)) for S-Transform'
	write(*,*)'grp   : Calculate group velocity from ST spectrogram'
	write(*,*)'grp1= : Using lower group velocity '
	write(*,*)'grp2= : Using upper group velocity '
	write(*,*)'        (Default: grp1=2. & grp2=6.0)'
      write(*,*)' '
      write(*,*)'OUTPUT: ASCII file (*.RAW.dat *.STFT.dat *.ST.dat)'
      write(*,*)'        *.RAW.dat : time, ampl       '
      write(*,*)'        *.STFT.dat: time, freq, specampl       '
      write(*,*)'        *.ST.dat  : time, freq, specampl       '
      write(*,*)'        plotting with GMT et al.:'
      write(*,*)'        detail info in plotscript.bat '
      write(*,*)'grp OUTPUT: ASCII file (*.ST.dat *.disper_sad.dat 
     &*.disper_contf.dat)'
      write(*,*)'        *.ST.dat  : time, freq, specampl, grp vel     '
      write(*,*)'        *.disper_sad.dat  : time, freq, grp vel, ampl '
      write(*,*)'        *.disper_contf.dat: time, freq, grp vel, ampl '
      write(*,*)'sad is saddle maxima & contf is constant fre. maxima  '
      write(*,*)'        plotting with GMT et al.:'
      write(*,*)'        detail info in plotgrp.bat' 
	write(*,*)'FIX OUTPUT: ASCII file (*.STFT.dat fixall.dat 
     &minmax.dat)'
	write(*,*)'            *.STFT.dat: time, freq, specampl, phase   '
      write(*,*)'            plotting with GMT et al.:'
      write(*,*)'            detail info in plotfix.bat '
	write(*,*)'            fixall.dat: time, freq, specampl, phase   '
	write(*,*)'            Combine all *.STFT.dat file               '
	write(*,*)'            minmax.dat: range info for plotall.bat    '
      write(*,*)' '
      write(*,*)'AUTHOR: Weian VVN Chao, JAN. 24. 2011'
      write(*,*)' '
      write(*,*)'NOTES: Please, send bugs, comments, improvements'
      write(*,*)' .... to vvnchao@nctu.edu.tw;vvnchao@gmail.com '
	write(*,*)' '
	write(*,*)'WEBSITE: http://collab.cv.nctu.edu.tw/main.html'
      write(*,*)' '

	write(*,*) 'EXAMPLE: (Short-time Fourier Transform)'
      write(*,'(a)') ' @>stft sta.inf -IC:\data -OC:\result 
     &-C5.0E-10 -T6.0 -M3.0 rtr rm tap stft'
	write(*,*) 'EXAMPLE: single event with short SAC file'
	write(*,*) 'Using dB amplitude (10*Log(amp)) with normalizing'
	write(*,'(a)') ' @>stft_st_grp sta.inf -I.\data -O.\oup dtt=0.1
     & dff=0.01 rtr rm nor st dB Comp=Z fd=0. fu=20. FPLOT' 
	write(*,*) 'EXAMPLE: Long SAC file'
	write(*,*) 'one-day SAC file 1hr timewindow with 50% overlap'
	write(*,'(a)') ' @>stft_st_grp sta.inf -I.\data -O.\oup 
     & dtt=100. dff=0.1 rtr rm st dB  fd=0. fu=20. Comp=Z Lsac' 
	write(*,*) 'EXAMPLE: Extract Dispersion Curve'
	STOP '@>stft_st_grp sta.inf -I.\data -O.\oup rm rtr nor st grp 
     &grp1=0.5 grp2=3.5 fu=1.5 fd=0.5 Comp=* FPLOT CHECK'
       RETURN
      END SUBROUTINE usage

c---------------------------------------------------------------------
      SUBROUTINE readsac(fn,np,dt,xc,yy,iday,hh,oo,sec,stn,rd,comp)
        
        	type sacheader
        	real*4 :: sac_header_real(70)
        	integer(4) :: sac_header_integer(35)
        	logical*4 :: sac_header_logical(5)
        	character*8 :: sac_header_char(24)
        	end type sacheader
        
        	real xc(np)
           
        	real*4, allocatable :: x(:)
        
        	type(sacheader) :: sh
        	character fn*200
        
        	character stn*5
	    character comp*3
        	integer yy,hh,oo
        	real sec,rd
        
        	xc=0.0
        	
c   Loading SAC files data to array
        
        	open(31,file=fn,status='old',form='binary')
        	read(31) sh
        	nps=sh%sac_header_integer(10)
        	dt=sh%sac_header_real(1)
        
c   Record information
        
        	stn=sh%sac_header_char(1)(1:5)
        	comp=sh%sac_header_char(21)(1:5)
        	yy=sh%sac_header_integer(1)
        	iday=sh%sac_header_integer(2)
        	hh=sh%sac_header_integer(3)
        	oo=sh%sac_header_integer(4)
        	sec=sh%sac_header_integer(5)+sh%sac_header_integer(6)/1000.0
        
        	slat=sh%sac_header_real(32)
        	slon=sh%sac_header_real(33)
        	sele=sh%sac_header_real(34)
        	elat=sh%sac_header_real(36)
        	elon=sh%sac_header_real(37)
        	edep=sh%sac_header_real(39)
        
c   Distance parameter
          rd=sh%sac_header_real(51)     
        	allocate(x(nps),stat=ierr)
        	
        	if(ierr.NE.0) stop 'ERROR: ALLOCATING DATA ERROR...'
        
        	read(31) x
        	close(31)

          if(np.lt.nps) write(*,*)'WARNING: RECORD DIMENSION TOO LARGE:'
     &,nps
	    
        	if(np.gt.100) np=min(np,nps)

        	do i=1,np
        	  xc(i)=x(i)
        	end do
        	
        	deallocate(x)
        
        RETURN
      END	SUBROUTINE readsac

c-----------------------------------------------------------------
      SUBROUTINE lsq(x,y,ndata,a,b)
      implicit integer*4(I-N)
      real*8 sx,sy,sxx,sxy,det,tc
      real a,b
      real x(ndata),Y(ndata)

      SXX=0.
      SXY=0.
      SX=0.
      SY=0.

      do id=1,Ndata
        sxx=sxx+x(id)*x(id)
        SX=SX+X(ID)
        SY=SY+Y(ID)
        sxy=sxy+x(id)*y(id)
      enddo

      tc=float(ndata)
      DET=SXX*Tc-SX*SX
      A=(SXY*Tc-SX*SY)/DET
      B=(SXX*SY-SX*SXY)/DET

       RETURN
      END SUBROUTINE lsq

c--------------------------------------
c   Gaussian tapered (windowing)
      SUBROUTINE gau_taper(w,np,sigma)
       real w(np),x,sigma

c   Created windowing function
      np2=np/2
       do i=1,np
       x=(i-np2)/(sigma*np2)
	 x=-(x**2.)*0.5
	 w(i)=exp(x)
       enddo

       RETURN
      END SUBROUTINE gau_taper

c--------------------------------------
c   Cosine taper
      REAL FUNCTION costap(ma,mb)
c   ma is actual sample
c   mb is total number of samples
c   left side only
      IF(ma.GT.mb) THEN
	 costap=1.0
	 RETURN
	ENDIF
      twopi=6.28318530717958
	a=real(ma)/real(2*mb)
	costap=abs(1.0-cos(a))/2.0
	
       RETURN
	END 

c--------------------------------------------------------------
      SUBROUTINE CLOGC(n,x,sigm,dt)
 
c--- performs fft on signals with length 2**n and sampling interval
c--- of dt seconds (if in the time domain; notice that dt*df=1/2**n).
c--- the signal is stored in x. it may be complex.
c--- the spectrum is returned in x. it is almost always complex.
c--- a time-to-frequency transform is done with sign=+1. (conform
c--- the convention adopted in aki and richards - the alternative
c--- convention may be obtained by taking complex conjugates after
c--- the call to clogc).
c--- the normalization factor 1./twopi occurs in the frequency-to
c--- time transform (again aki&richards).
c--- normalization is such that physical dimensions are respected.
c--- thus, if the time signal is dimensioned in meters, the
c--- resulting spectral density in x is in meters/hz. for example,
c--- if the time signal is the unit sinc function of width dt, centered
c--- at t=0, the spectral density is dt for all values of the frequency.c
c--- array locations: if x contains the spectrum, it has the spectrum
c--- for positive frequencies in the first 2**n/2+1 elements, such that
c--- x(1) is at 0 hz, x(2) at df hertz, and x(2**n/2+1) at the nyquist,
c--- where df=1./(2**n*dt) and the nyquist is 1./(2*dt) hz.
c--- the second half of x contains the spectrum for negative frequenciesc--- such that x(2**n) is at -df, x(2**n-1) at -2*df hz etcetera.
c--- if x contains the time signal, x(1) is at time 0, x(2)
c--- at time dt etc.
c
      dimension x(1),m(25)
      complex x,wk,hold,q
c     dimension m(25)
c     complex x(262144),wk,hold,q
      if(sigm.ge.0.) then
        sign=1.
      else
        sign=-1.
      endif
      lx=2**n
      do 1 i=1,n
    1 m(i)=2**(n-i)
      do 4 l=1,n
      nblock=2**(l-1)
      lblock=lx/nblock
      lbhalf=lblock/2
      k=0
      do 4 iblock=1,nblock
      fk=k
      flx=lx
      v=sign*6.283185308*fk/flx
      wk=cmplx(cos(v),sin(v))
      istart=lblock*(iblock-1)
      do 2 i=1,lbhalf
      j=istart+i
      jh=j+lbhalf
      q=x(jh)*wk
      x(jh)=x(j)-q
      x(j)=x(j)+q
    2 continue
      do 3 i=2,n
      ii=i
      if(k.lt.m(i)) go to 4
    3 k=k-m(i)
    4 k=k+m(ii)
      k=0
      do 7 j=1,lx
      if(k.lt.j) go to 5
      hold=x(j)
      x(j)=x(k+1)
      x(k+1)=hold
    5 do 6 i=1,n
      ii=i
      if(k.lt.m(i)) go to 7
    6 k=k-m(i)
    7 k=k+m(ii)
      if(sign.gt.0.) go to 9
      flx=flx*dt
      do 8 i=1,lx
    8 x(i)=x(i)/flx
      return
    9 do 10 i=1,lx
   10 x(i)=x(i)*dt
      return
      end
