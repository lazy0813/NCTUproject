c    This program is to create horizontal/vertical envelope function.
c    henv[t]----- horizontal envelope function
c    venv[t]----- vertical envelope function
c    
c    The following parameters can be determined based on henv & venv:
c    1. signal duration, SD
c    2. skewness, SK 
c    3. kurtosis, KT               !-- modified Mar. 09. 2021
c    4. peak ground velocity, PGV
c    7. envelope noise level, A0
c    8. envelope area, AE
c    The following parameters can be determined based on original 3D data:
c    5. magnitude ratio ML/MD      !-- modified Mar. 09. 2021 
c    6. spectral amplitude ratio   !-- modified Mar. 09. 2021 
c
c    Add cos-taper                 !-- modified Mar. 09. 2021
c
c    remove cos-taper              !-- modified May  20. 2021
c    Add check in time window size (10 sec) for spectral analysis  !-- modified May 20. 2021
c    Add recalculate Tpeak and Max. PGV after T1, T2 selection     !-- modified May 20. 2021
c    Add signal-to-noise (snr)     !-- modified May 20. 2021
c    Sort the para_results.txt based on epicenter distance         !-- modified May 20. 2021
c    Add threshold of snr value: 2.7

c----------------------------------------------------------------------------------------------------
c     Nov. 10. 2018 created by vvn weian chao, assistant professor
c     Email: vvnchao@gmail.com; vvnchao@nctu.edu.tw
c     Department of Civil Engineering, National Chiao Tung University
c--------------------------------------------------------------------
c    Dec. 22. 2018: ADD Fourier-Transform Analysis
c--------------------------------------------------
c    Mar. 09. 2021: a few revised
c--------------------------------------------------
c    May  20. 2021: debug for spectral amplitude ration and add snr
c------------------------------------------------------------------



        PROGRAM calpara
         parameter(npmax=327680,nsmax=100)

c    paramters
c--------------
         real f1,f2,fac
	   logical lfilter,lfac
	   logical lhor  !-- modified Mar. 09. 2021
	   character(len=100) inp,oup
         character(len=100) par,par1
         logical LFALSE,LTRUE,EXT0
         data LFALSE,LTRUE /.false.,.true./

c    station 
c------------
         character(len=4) stn(nsmax)
	   character(len=50) stninp
	   real slo(nsmax),sla(nsmax),elv(nsmax)
	   real fz(nsmax),fn(nsmax),fe(nsmax)
         integer nsta
	   real elo,ela,dep

c    filename of waveform data
         character(len=200) fnv,fnn,fne
	   integer npv,npn,npe
	   real dt,sec
	   integer yy,iday,hh,oo
	   real wv(3,npmax)  !-- 1:Z 2:N 3:E
         real xv(npmax),xn(npmax),xe(npmax)
         real xv0(npmax),xn0(npmax),xe0(npmax)

c    FFT analysis
         real xfft(npmax),yfft(npmax),fftamp(npmax),fre(npmax)
         real LFHFR_v,LFHFR_n,LFHFR_e,LFHFR_hor

c    filter waveform data
c    Long Period (LP)
         real xvlp(npmax),xnlp(npmax),xelp(npmax)
	   real f3,f4
c    Low Frequency (LF) 
         real xvlf(npmax),xnlf(npmax),xelf(npmax)
	   real f5,f6           
c    High Frequency (HF)
         real xvhf(npmax),xnhf(npmax),xehf(npmax)
	   real f7,f8
c    Ratio between LP and HF
         real LPHF(npmax)       
c    Ratio between LF and HF
         real LFHF(npmax)

	   real henvlp(npmax),henvlf(npmax),henvhf(npmax)
	   real hrmslp(npmax),hrmslf(npmax),hrmshf(npmax)

c    horizontal & vertical envelope functions
         real henv(npmax),venv(npmax),hrms(npmax),vrms(npmax)
         integer icomp,inum
	   real envx(npmax),envy(npmax)
	   character(len=300) txtoup

c    epicenter distance & azimuth
         real*8 dtor,pi
	   real*8 delta,azim,bazim
	   real*8 xx1,xx2,yy1,yy2
	   real azi,epi

c    output parameters
         real hpgv,vpgv
	   real hSD,vSD
         real hSK,vSK
	   real hKT,vKT
	   real env0,AE

c    plot waveform data
         integer pgopen
	   character(len=300) psoup

c    STA/LTA parameter
         real stalen,ltalen,T1_theshold,T2_threshold
	   real after_sec
	   real staavg,ltaavg
         integer stanum,ltanum
         real xs(npmax),ysta(npmax),ylta(npmax),ratio(npmax)
         real T1,T1amp,T2,T2amp,Tpeak

c    Magnitude estimation    !-- modified Mar. 09. 2021
         real vel(3,npmax)   !-- 1:Z 2:N 3:E
	   real acc(3,npmax)
         ! for FFT analysis
         real x(npmax),y(npmax)
         real wa(3,npmax)

c    signal-to-noise  snr    !-- modified May 20. 2021
         real snravg,snr
	   integer navg

c    sort the results        !-- modified May 20. 2021
         character(len=132) para(nsmax),para_tmp,head
	   real snr_threshold  !-- modified May 20. 2021
	   real epi_sort(nsmax),epi_tmp,snr_tmp
 

c    command mode
c-----------------
         integer*4 bat,system
	   character(len=300) cmd
         character(len=1) str

c   INPUTS:
c---------------------------
c   Get number of arguments:
       narg=iargc()
       IF(narg.lt.1) THEN
	  print*,'cal_para.exe sta.inf -I.\inp -O.\oup f1=1. f2=5. ela= el
	1o= dep= com=H'
        stop 'ERROR: arguments?'
       ENDIF

c   DEFAULT VALUE:
c--------------------
       inp='./'
       oup='./'
       f1=0.
       f2=0.
       lfilter=.false.
	 lfac=.false.
	 lhor=.true.   !-- modified Mar. 09. 2021
       fac=1.0
       nused=0
	 ela=0.
       elo=0.
	 dep=0.
       f3=0.02
	 f4=0.1
	 f5=1.  !-- LF
	 f6=5.
	 f7=10. !-- HF
	 f8=20.
	 snr_threshold=2.7  !-- modified May 20. 2021


        pi=4.d0*datan(1.d0)
        dtor=pi/180.d0


c   Read station information: stn.ID slo sla elv fz fn fe 
       call getarg(1,par,ic)
	 stninp=trim(par)

       open(1,file=stninp,status='old')
        do i=1,nsmax
        read(1,*,iostat=istat) stn(i),slo(i),sla(i),elv(i)
	1,fz(i),fn(i),fe(i)
	  if(istat.ne.0) exit
	  enddo
       close(1)
       
       nsta=i-1
       print*,' Total number of station in list: ',nsta

c   Get arguments (optional):
c----------------------------
       DO iarg=2,narg
        call getarg(iarg,par,ic)
        l=len(par)
       IF(par(1:2).eq.'-I') THEN 
        inp=par(3:l)        
       ELSE IF(par(1:2).eq.'-O') THEN 
        oup=par(3:l)        
       ELSE IF(par(1:3).eq.'f1=') THEN
        par1=par(4:l)
        read(par1,*) f1
       ELSE IF(par(1:3).eq.'f2=') THEN
        par1=par(4:l)
        read(par1,*) f2
       ELSE IF(par(1:4).eq.'fac=') THEN
        par1=par(5:l)
        read(par1,*) fac
	  lfac=LTRUE
       ELSE IF(par(1:4).eq.'elo=') THEN
        par1=par(5:l)
        read(par1,*) elo
       ELSE IF(par(1:4).eq.'ela=') THEN
        par1=par(5:l)
        read(par1,*) ela
       ELSE IF(par(1:4).eq.'dep=') THEN
        par1=par(5:l)
        read(par1,*) dep
       ELSE IF(par(1:4).eq.'com=') THEN
        par1=par(5:l)
        if(par1.eq.'Z') lhor=.false.  !-- modified Mar. 09. 2021
        ELSE
	  print*,'cal_para.exe sta.inf -I.\inp -O.\oup f1=1. f2=5. ela= el
	1o= dep= com=H'
	  print*, 'ERROR: arguments?'
	  stop
       ENDIF 
       ENDDO
       IF(f1.ne.0.0.and.f2.ne.0.0) lfilter=LTRUE
       IF(.not.lfilter) STOP '   ERROR: OPTION f1=? f2=?'

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

c   Create the output txt file
   15  open(1,file=oup(1:lo)//'para_result.txt',status='unknown')
       write(1,'(a131)')'StaID  EPI(km) AZI(degree) PGV(cm/s)  T1(sec) T
	12(sec)  SD(sec) SK     KT    M_ratio  LFHF    A0       AE           
     1 ML     MD  SNR'

c  Start to read the three-component waveform data:
c--------------------------------------------------
        do i=1,nsta
        fnv=inp(1:li)//stn(i)//'.Z.sac'
        fnn=inp(1:li)//stn(i)//'.N.sac'
        fne=inp(1:li)//stn(i)//'.E.sac'
        INQUIRE(file=fnv,exist=EXT0)
        IF(.not.EXT0) then
	  iv=0
	  ELSE
	  iv=1
	  ENDIF
        INQUIRE(file=fnn,exist=EXT0)
        IF(.not.EXT0) then
	  in=0
	  ELSE
	  in=1
	  ENDIF
        INQUIRE(file=fne,exist=EXT0)
        IF(.not.EXT0) then
	  ie=0
	  ELSE
	  ie=1
	  ENDIF
       
	  if(iv.eq.1) then
	   npv=npmax
         call readsac(fnv,npv,dt,xv,yy,iday,hh,oo,sec)
        write(*,'(1X,a20,a30,a20,i6)') 'Reading Z-com seismogram.....:'
     1,trim(fnv),' Time-point numbers: ',npv

c   Remove DC baseline (total data length)  !-- modified May. 20 2021
         isps=1.001/dt
         iparr=npv
         call demean(xv,npv,npv)
c   5 % taper to beg/end of time-series  !-- modified Mar. 09. 2021
c--------------------------------------
c	  mtap=nint(real(npv)*0.05)
c	   DO j=1,mtap
c	    xv(j)=xv(j)*costap(j,mtap+1)
c	    ii=npv+1-j
c	    xv(ii)=xv(ii)*costap(j,mtap)
c	   ENDDO
         xvlp=0.
	   xvlp(1:npv)=xv(1:npv)
         xvlf=0.
	   xvlf(1:npv)=xv(1:npv)
         xvhf=0.
	   xvhf(1:npv)=xv(1:npv)
         xv0=xv  !-- for FFT analysis

c   apply filter f1-f2 band
        call IIRFILT(xv,npv,'BUTTER',4,'BP',f1,f2,dt,1)   
        call IIRFILT(xvlp,npv,'BUTTER',4,'BP',f3,f4,dt,1) !-- 0.02-0.1 Hz
        call IIRFILT(xvlf,npv,'BUTTER',4,'BP',f5,f6,dt,1) !-- 1.0-5.0 Hz
        call IIRFILT(xvhf,npv,'BUTTER',4,'BP',f7,f8,dt,1) !-- 10.0-20.0 Hz
 
	  endif

	  if(in.eq.1) then
	   npn=npmax
         call readsac(fnn,npn,dt,xn,yy,iday,hh,oo,sec)
        write(*,'(1X,a20,a30,a20,i6)') 'Reading N-com seismogram.....:'
     1,trim(fnn),' Time-point numbers: ',npn

c   Remove DC baseline (total data length)  !-- modified May. 20 2021
         isps=1.001/dt
         iparr=npn
         call demean(xn,npn,npn)
c   5 % taper to beg/end of time-series  !-- modified Mar. 09. 2021
c--------------------------------------
c	  mtap=nint(real(npn)*0.05)
c	   DO j=1,mtap
c	    xn(j)=xn(j)*costap(j,mtap+1)
c	    ii=npn+1-j
c	    xn(ii)=xn(ii)*costap(j,mtap)
c	   ENDDO
         xnlp=0.
	   xnlp(1:npn)=xn(1:npn)
         xnlf=0.
	   xnlf(1:npn)=xn(1:npn)
         xnhf=0.
	   xnhf(1:npn)=xn(1:npn)
         xn0=xn  !-- for FFT analysis

c   apply filter f1-f2 band
        call IIRFILT(xn,npn,'BUTTER',4,'BP',f1,f2,dt,1)
        call IIRFILT(xnlp,npn,'BUTTER',4,'BP',f3,f4,dt,1)
        call IIRFILT(xnlf,npn,'BUTTER',4,'BP',f5,f6,dt,1)
        call IIRFILT(xnhf,npn,'BUTTER',4,'BP',f7,f8,dt,1)

	  endif

	  if(ie.eq.1) then
	   npe=npmax
         call readsac(fne,npe,dt,xe,yy,iday,hh,oo,sec)
        write(*,'(1X,a20,a30,a20,i6)') 'Reading E-com seismogram.....:'
     1,trim(fne),' Time-point numbers: ',npe

c   Remove DC baseline (total data length)  !-- modified May. 20 2021
         isps=1.001/dt
         iparr=npe
         call demean(xe,npe,npe)
c   5 % taper to beg/end of time-series  !-- modified Mar. 09. 2021
c--------------------------------------
c	  mtap=nint(real(npe)*0.05)
c	   DO j=1,mtap
c	    xe(j)=xe(j)*costap(j,mtap+1)
c	    ii=npe+1-j
c	    xe(ii)=xe(ii)*costap(j,mtap)
c	   ENDDO
         xelp=0.
	   xelp(1:npe)=xe(1:npe)
         xelf=0.
	   xelf(1:npe)=xe(1:npe)
         xehf=0.
	   xehf(1:npe)=xe(1:npe)
         xe0=xe  !-- for FFT analysis

c   apply filter f1-f2 band
        call IIRFILT(xe,npe,'BUTTER',4,'BP',f1,f2,dt,1)
        call IIRFILT(xelp,npe,'BUTTER',4,'BP',f3,f4,dt,1)
        call IIRFILT(xelf,npe,'BUTTER',4,'BP',f5,f6,dt,1)
        call IIRFILT(xehf,npe,'BUTTER',4,'BP',f7,f8,dt,1)

	  endif


c--  modified Mar. 09. 2021, the user can determine to use vertical or horizontal
c   Compute horizontal/vertical root-mean-square amplitude
c   & calculate the related horizontal/vertical envelope function     
        hrms=0.
        vrms=0.
	  henv=0.
        venv=0.

         if(in.gt.0.and.ie.gt.0.) then
	    if(npe.ne.npn) stop 'ERROR: data length is not equal!!'

         if(lfac) then
         do j=1,npv
          vrms(j)=(xv(j)*xv(j))
	    vrms(j)=sqrt(vrms(j))*fac
         enddo

         do j=1,npe
          hrms(j)=0.5*(xn(j)*xn(j)+xe(j)*xe(j))
	    hrms(j)=sqrt(hrms(j))*fac
c          hrmslp(j)=0.5*(xnlp(j)*xnlp(j)+xelp(j)*xelp(j))
c	    hrmslp(j)=sqrt(hrmslp(j))*fac
c          hrmslf(j)=0.5*(xnlf(j)*xnlf(j)+xelf(j)*xelf(j))
c	    hrmslf(j)=sqrt(hrmslf(j))*fac
c          hrmshf(j)=0.5*(xnhf(j)*xnhf(j)+xehf(j)*xehf(j))
c	    hrmshf(j)=sqrt(hrmshf(j))*fac
	   enddo

	    else
         
	   do j=1,npv
          vrms(j)=(xv(j)*fz(i)*xv(j)*fz(i))
	    vrms(j)=sqrt(vrms(j))
         enddo

         do j=1,npe
          hrms(j)=0.5*(xn(j)*fn(i)*xn(j)*fn(i)+xe(j)*fe(i)*xe(j)*fe(i))
	    hrms(j)=sqrt(hrms(j))
c          hrmslp(j)=0.5*(xnlp(j)*fn(i)*xnlp(j)*fn(i)
c	1+xelp(j)*fe(i)*xelp(j)*fe(i))
c	    hrmslp(j)=sqrt(hrmslp(j))
c          hrmslf(j)=0.5*(xnlf(j)*fn(i)*xnlf(j)*fn(i)
c	1+xelf(j)*fe(i)*xelf(j)*fe(i))
c	    hrmslf(j)=sqrt(hrmslf(j))
c          hrmshf(j)=0.5*(xnhf(j)*fn(i)*xnhf(j)*fn(i)
c	1+xehf(j)*fe(i)*xehf(j)*fe(i))
c	    hrmshf(j)=sqrt(hrmshf(j))
	   enddo
         endif

        nt=npe
	  tp=1.0
	  icomp=1  !-- the number of decomposition
        envx=0.
	  envy=0.
        call envelope(hrms,nt,dt,tp,envx,envy,nt,inum,icomp) 
        call resample(envx,envy,dt,henv,nt,inum,nenv)

        nt=npv
	  tp=1.0
	  icomp=1  !-- the number of decomposition
        envx=0.
	  envy=0.
        call envelope(vrms,nt,dt,tp,envx,envy,nt,inum,icomp) 
        call resample(envx,envy,dt,venv,nt,inum,nenv)

        if(.not.lhor) henv = venv  !-- modified Mar. 09. 2021, use vertical envelope function 

c        print*, 'Record length: ', real(nenv)*dt
c    !-- modified May. 20. 2021
        print*, ' Max. value of env.: ',maxval(henv(1:nenv)),' cm/s'


c   Calculate the epicenter distance [km] & azimuth
        xx1=elo
        yy1=ela
        xx2=slo(i)
        yy2=sla(i)
        call azimth(1,yy1,xx1,yy2,xx2,delta,azim,bazim)
        epi=delta*dtor*6371. 
        azi=azim 
        print*, ' Epicenter Dist. (R,km):  ', epi,'  Azimuth: ',azi

c   Output the txt file
        if(lhor) then !-- modified Mar. 09. 2021
        txtoup=oup(1:lo)//stn(i)//'henv.txt'
        open(31,file=txtoup,status='unknown')
	  write(31,*) ' Time(sec)     RMS      ENVELOPE'
        DO j=1,npe
        write(31,'(f10.4,2(1x,e12.5))') real(j-1)*dt,hrms(j),henv(j)
        ENDDO
        close(31)
	  else
        txtoup=oup(1:lo)//stn(i)//'venv.txt'
        open(31,file=txtoup,status='unknown')
	  write(31,*) ' Time(sec)     RMS      ENVELOPE'
        DO j=1,npv
        write(31,'(f10.4,2(1x,e12.5))') real(j-1)*dt,vrms(j),venv(j)
        ENDDO
        close(31)        
	  endif
c   Find signal duration [SD]
c-------------------------------
c   Compute the STA/LTA values
        stalen=0.5
        ltalen=10.
        T1_threshold=2.2
        T2_threshold=0.05
        after_sec=20.
        stanum=int(stalen/dt)
        ltanum=int(ltalen/dt)
        jj=0
        xs=0.
	  ysta=0.
        ylta=0.
	  ratio=0.
        hpgv=0.  !-- parameter PGV
  
         do j=ltanum+1,npe-1
         if(hpgv.lt.henv(j)) then
         hpgv=henv(j)
         jmax=j
         endif

          k2=j
	    k1=k2-stanum
	    staavg=0.
	    do k=k1,k2
          staavg=staavg+henv(k)
	    enddo

          k2=j
	    k1=k2-ltanum
	    ltaave=0.
	    do k=k1,k2
          ltaavg=ltaavg+henv(k)
	    enddo
 
          staavg=staavg/real(stanum)
		ltaavg=ltaavg/real(ltanum)  
          jj=jj+1
          xs(jj)=real(j)*dt
          ysta(jj)=staavg
          ylta(jj)=ltaavg
          ratio(jj)=staavg/ltaavg
         enddo
         Tpeak=jmax*dt
c   Autamatically find T1-time point
         j1=20.*isps 
         do j=j1,jj
	    if(ratio(j).ge.T1_threshold) exit
	   enddo
         T1=real(j+ltanum)*dt
	   T1amp=henv(j+ltanum)
         print*,' T1 time-point: ',T1
c   Automatically find T2-time point
         hpgv_threshold=hpgv*T2_threshold
	   print*,' Time-point with PGV: ',Tpeak
         do j=jmax,npe
         if(henv(j).le.hpgv_threshold) then
         k1=j+1
         k2=j+after_sec*isps
         kcnt=0
         do k=k1,k2
          if(henv(k).ge.hpgv_threshold) kcnt=kcnt+1
          enddo
         if(kcnt.le.1) exit
         endif 
         enddo
         T2=real(j)*dt
         T2amp=henv(j)
c    Additional pick T2
         do l=jmax,npe
         if(henv(l).le.T1amp) then
         k1=l+1
         k2=l+after_sec*isps
         kcnt=0
         do k=k1,k2
          if(henv(k).ge.T1amp) kcnt=kcnt+1
         enddo
         if(kcnt.le.1) exit
         endif 
         enddo
         T2_additional=real(l)*dt
         
	   if(l.lt.j) then
	   T2amp=henv(l)
	   T2=T2_additional
	    else
         T2amp=henv(j)
	   endif

         print*,' T2 time-point: ',T2,T2_additional

c   Plot the horizontal envelope function for checking the data quality
   22   psoup=' '
        psoup=oup(1:lo)//stn(i)//'henv.ps/vcps'   
        istat=pgopen(psoup)
        IF(istat.le.0) STOP ' ERROR: Opening for PS file'
        pt1=0.
	  pt2=real(npe)*dt
       
	  call pgsubp(1,3)
        call pgslw(1)
        call pgscf(3)       
        call pgsch(1.5)
        call pgsci(1)      
        call pgenv(pt1,pt2,0.,hpgv,2,1)
        call pglab('Time(sec)','Amplitude(cm/s)',' ')

        call pgslw(2)
        call pgsci(1)

c         do j=1,npe-1
c         x1=t1+real(j-1)*dt
c         x2=x1+dt        
c         call pgmove(x1,hrms(j))
c         call pgdraw(x2,hrms(j+1))
c         enddo

         call pgsci(2)
         do j=1,npe-1
         x1=pt1+real(j-1)*dt
         x2=x1+dt        
         call pgmove(x1,henv(j))
         call pgdraw(x2,henv(j+1))
         enddo

c   plot T1 T2 
        call pgscr(30,0.3,0.3,0.3)
        call pgsci(30)
        call pgsls(1)
        call pgslw(5)

        call pgmove(0.,hpgv*T2_threshold)
        call pgdraw(real(npe*dt),hpgv*T2_threshold)
        call pgmove(T2,0.)
        call pgdraw(T2,hpgv)
        call pgmove(T1,0.)
        call pgdraw(T1,hpgv)


c   plot SNR ratio
        if(lhor) then
	   mtap=nint(real(npe)*0.06)
	  else         
	   mtap=nint(real(npv)*0.06)
	  endif         
	  ymax=maxval(ratio(mtap:jj-mtap))
        call pgslw(1)
        call pgscf(3)       
        call pgsch(1.5)
        call pgsci(1) 
        call pgenv(0.,real(npe*dt),0.,ymax,2,1)
        call pglab('Time (sec)','STALTA Ratio',' ')
        call pgslw(2)
        do j=mtap,jj-mtap
        call pgmove(xs(j),ratio(j))
        call pgdraw(xs(j+1),ratio(j+1))
        enddo


         call pgend

c   re-find T1 and T2 time point
        print*,' Picked T1, T2: ', T1,T2
	  write(*,*) '================================================='
   21	  write(*,'(A \)')'  Manually picking time-points T1 & T2 (y/n):'
	  read(*,*) str
	  if(str.ne.'y'.and.str.ne.'n') goto 21
	  
	  if(str.eq.'y') then
        write(*,'(A \)')'  Input T1 & T2 time-points: '
	  read(*,*) T1,T2
	   goto 22  !-- replot waveform result for checking
	   
        else

c   re-calculate the time-point of peak amplitude
	  write(*,*) '==============', stn(i),'====================='         
	   jT2=T2*isps
	   jT1=T1*isps
	   hpgv=0.0
	   snravg=0.0
	   navg=0
	   do j=jT1,jT2
	   snravg=snravg+henv(j)
	   navg=navg+1
	    if(hpgv.lt.henv(j)) then
          hpgv=henv(j)
          jmax=j
          endif
	   Tpeak=jmax*dt
	   enddo
	   snravg=snravg/real(navg)
	   snr=hpgv/snravg
         print*,' SNR                : ',snr
	   print*,' Time-point with PGV: ',Tpeak
         print*, ' Max. value of env.: ',henv(jmax),' cm/s'
 

         nused=nused+1
        endif
        
	  hSD=T2-T1
        print*,' Signal Duration (SD):   ',hSD


        jT2=T2*isps
	  jT1=T1*isps
	  T1amp=henv(jT1)
	  T2amp=henv(jT2)

c    Compute FFT amplitude for low-frequency (1-5 Hz) & high-frequency (10-20 Hz)
c    LFHFR = LF Average FFT amplitude / HF Average FFT amplitude
!-- modified Mar. 09. 2021 
c     time-point of PGV (Tpeak) as window center, +- 5 seconds
c     Thus, total window size is 10 seconds 
        jT1_w=(Tpeak-5.0)*isps
	  jT2_w=(Tpeak+5.0)*isps
        if(jT1_w.lt.jT1) jT1_w=jT1
        if(jT2_w.gt.jT2) jT2_w=jT2
        if(jT2_w-jT1_w.ne.nint(10/dt)) then
	   print*,' WARNING: time window size /= 10 sec'
        endif

c   for vertical component        
	  if(iv.eq.1) then
         jj=1
	   do j=jT1_w,jT2_w		 
	   yfft(jj)=xv0(j)
         jj=jj+1
         enddo
	   npfft=jj-1

         do j=2,16
         n=2**j
	   if(n.ge.npfft)exit
         enddo
         nu=j

         do j=npfft+1,n
	   yfft(j)=0.0
         enddo
         xfft=0.0 
         call fft(yfft,xfft,n,nu)
         df=1./(n*dt)
         yfft=yfft/real(n)
         xfft=xfft/real(n)
         yfft(n)=0.0
         xfft(n)=0.0
         do j=1,n/2-1
         fre(j)=(j-1.0)*df
         fftamp(j)=sqrt(yfft(j)*yfft(j)+xfft(j)*xfft(j))
c         print*,fre(j),fftamp(j)
         enddo
         
	   LFamp=0.
	   HFamp=0.
	   nLF=0
	   nHF=0
         do j=1,n/2-1
         f=fre(j)
	   if(f.ge.1.0 .and. f.lt.5.0) then
	   LFamp=LFamp+fftamp(j)
	   nLF=nLF+1
         else if(f.ge.10.0 .and. f.lt.20.0) then 
	   HFamp=HFamp+fftamp(j)
         nHF=nHF+1
         endif
         enddo
	   LFHFR_v=log((LFamp/real(nLF))/(HFamp/real(nHF)))
         print*,' LFHFR for vertical:   ',LFHFR_v
	  endif

c   for east component        
	  if(ie.eq.1) then
         jj=1
	   do j=jT1_w,jT2_w		 
	   yfft(jj)=xe0(j)
         jj=jj+1
         enddo
	   npfft=jj-1

         do j=2,16
         n=2**j
	   if(n.ge.npfft)exit
         enddo
         nu=j

         do j=npfft+1,n
	   yfft(j)=0.0
         enddo
         xfft=0.0 
         call fft(yfft,xfft,n,nu)
         df=1./(n*dt)
         yfft=yfft/real(n)
         xfft=xfft/real(n)
         yfft(n)=0.0
         xfft(n)=0.0
         do j=1,n/2-1
         fre(j)=(j-1.0)*df
         fftamp(j)=sqrt(yfft(j)*yfft(j)+xfft(j)*xfft(j))
c         print*,fre(j),fftamp(j)
         enddo
         
	   LFamp=0.
	   HFamp=0.
	   nLF=0
	   nHF=0
         do j=1,n/2-1
         f=fre(j)
	   if(f.ge.1.0 .and. f.lt.5.0) then
	   LFamp=LFamp+fftamp(j)
	   nLF=nLF+1
         else if(f.ge.10.0 .and. f.lt.20.0) then 
	   HFamp=HFamp+fftamp(j)
         nHF=nHF+1
         endif
         enddo
	   LFHFR_e=log((LFamp/real(nLF))/(HFamp/real(nHF)))
         print*,' Spectral ratio for E-com:   ',LFHFR_e
	  endif

c   for north component        
	  if(in.eq.1) then
         jj=1
	   do j=jT1_w,jT2_w		 
	   yfft(jj)=xn0(j)
         jj=jj+1
         enddo
	   npfft=jj-1

         do j=2,16
         n=2**j
	   if(n.ge.npfft)exit
         enddo
         nu=j

         do j=npfft+1,n
	   yfft(j)=0.0
         enddo
         xfft=0.0 
         call fft(yfft,xfft,n,nu)
         df=1./(n*dt)
         yfft=yfft/real(n)
         xfft=xfft/real(n)
         yfft(n)=0.0
         xfft(n)=0.0
         do j=1,n/2-1
         fre(j)=(j-1.0)*df
         fftamp(j)=sqrt(yfft(j)*yfft(j)+xfft(j)*xfft(j))
c         print*,fre(j),fftamp(j)
         enddo
         
	   LFamp=0.
	   HFamp=0.
	   nLF=0
	   nHF=0
         do j=1,n/2-1
         f=fre(j)
	   if(f.ge.1.0 .and. f.lt.5.0) then
	   LFamp=LFamp+fftamp(j)
	   nLF=nLF+1
         else if(f.ge.10.0 .and. f.lt.20.0) then 
	   HFamp=HFamp+fftamp(j)
         nHF=nHF+1
         endif
         enddo
	   LFHFR_n=log((LFamp/real(nLF))/(HFamp/real(nHF)))
         print*,' Spectral ratio for N-com:   ',LFHFR_n
	  endif

        if(in.eq.1.and.ie.eq.1) LFHFR_hor=(LFHFR_n+LFHFR_e)/2.0
         print*,' LFHFR for horizontal:   ',LFHFR_hor


c   Start to estimate local magnitude !-- modified Mar. 09. 2021
        if(lfac) then
	  vel(1,:)=xv0*fac  !-- 1:Z 2:N 3:E
        vel(2,:)=xn0*fac
	  vel(3,:)=xe0*fac
	  else
	  vel(1,:)=xv0*fz(i)  !-- 1:Z 2:N 3:E
        vel(2,:)=xn0*fn(i)
	  vel(3,:)=xe0*fe(i)	  
	  endif

c    start to compute acceleration data
        do nc=1,3
        acc(nc,:)=0.d0
         do j=1,npe-1
         acc(nc,j)=(vel(nc,j+1)-vel(nc,j))/dt
         enddo
        enddo  

        do k=2,16
         npt=2**k
         if(npt.ge.npe) exit
        enddo
        NNFFT=k

        do nc=1,3
         do j=npe+1,npt
         acc(nc,j)=0.0
         enddo
        enddo

c    Fourier Transform
        do nc=1,3
         x=acc(nc,:)
         y=0.0

         NFFT=npt
         call FFT(x,y,NFFT,NNFFT)
         df=1./dt/REAL(NFFT)
         NFFT2=NFFT/2
         do j=1,NFFT2
          freq=df*(j-1)
          if(j.eq.1)then
          x(1)=0.0
          y(1)=0.0
          x(NFFT)=0.0
          y(NFFT)=0.0
           else
c     Multiply Wood-Anderson Response
          call woodand_acc(freq,xr,xi,0.8)
          xr=xr*2800. !-- amplify 2800.
          xi=xi*2800.
          x1=x(j)/REAL(NFFT)
          y1=y(j)/REAL(NFFT)
          x(j)=x1*xr-y1*xi
          y(j)=-(x1*xi+y1*xr)  

          xi=-xi    !  minus sign is for inverse FT
          x1=x(NFFT+1-j)/REAL(NFFT)
          y1=y(NFFT+1-j)/REAL(NFFT)
          x(NFFT+1-j)=x1*xr-y1*xi
          y(NFFT+1-j)=-(x1*xi+y1*xr)  !  minus sign is for inverse FT
          endif
         enddo
c   Inverse Fourier Transform
         call fft(x,y,NFFT,NNFFT)
         wa(nc,:)=x*10.
        enddo

        wamaxn=maxval(abs(wa(2,1:npt)))
        wamaxe=maxval(abs(wa(3,1:npt)))
        wa_amp=(wamaxn*wamaxn+wamaxe*wamaxe)**0.5
c        print*,wa_amp
c    save the wood-anderson records
        open(31,file=oup(1:lo)//stn(i)//'.wa.txt',status='unknown')
         write(31,*) ' Time[sec]     Z-comp     N-comp    E-comp  [mm]'
         do j=1,npt
         write(31,'(f7.3,3(1x,e15.6))') dt*real(j-1),(wa(k,j),k=1,3)
         enddo
        close(31)

        R_hypo=(epi*epi+dep*dep)**0.5

c   Determine local magnitude (ML)
        if(dep.le.35.0) then  !  for shallow earthquake
         if(epi.lt.80.0) then
         wa_corr=-0.00716*R_hypo-log10(R_hypo)-0.39
          else
         wa_corr=-0.00261*R_hypo-0.831*log10(R_hypo)-1.07
         endif 
        else  !  for deep earthquake
         wa_corr=-0.00326*R_hypo-0.831*log10(R_hypo)-1.01
        endif
        xml=log10(wa_amp)-wa_corr
        print*,' ML: ',xml
c    xmD=-0.87+2.0*log10(SD)+0.0035*epi; Lee et al. (1972)
        xmD=-0.87+2.0*log10(hSD)+0.0035*epi
        print*,' MD: ',xmD
        xmratio=xml/xmD
        print*,' xmratio: ',xmratio

c   Now, SD was determined, 
c   Start to compute the skewness (SK) & kurtosis (KT)

c    SK = n/(n-1)/(n-2)*sum([(x-xmean)/sdv]**3.0); SK =0 is symmetric as normal distribution 
c    compute mean value	  
           xsum=0.
	     npt=0
		 do j=jT1,jT2
           xsum=xsum+henv(j)
	     npt=npt+1
           enddo
           xmean=xsum/real(npt)
c    compute sdv
           sdv=0.0
		 do j=jT1,jT2
           sdv=sdv+((henv(j)-xmean)*(henv(j)-xmean))
           enddo
           sdv=sqrt(sdv/real(npt-1))
c    SK value
		 xsum=0.
		 do j=jT1,jT2
           xsum=xsum+((henv(j)-xmean)/sdv)**3.0
           enddo
		 hSK=real(npt)/real(npt-1)/real(npt-2)*xsum
           print*,' Skewness (SK):   ',hSK

c    KT = m4/m2^2  KT is 0 as normal distribution
c    compute m2
           sdv=0.0
		 do j=jT1,jT2
           sdv=sdv+((henv(j)-xmean)*(henv(j)-xmean))/real(npt)
           enddo
           xm2=sdv
c    compute m4
           sdv=0.0
		 do j=jT1,jT2
           sdv=sdv+((henv(j)-xmean)**4.0)/real(npt)
           enddo
           xm4=sdv             
c    KT value
          hKT=xm4/(xm2*xm2)-3.0
          print*,' Kurtosis (KT):', hKT

c    Envelope Area (AE)
          env0=(T1amp+T2amp)/2.0
          write(*,*) ' Envelope Zero (A0): ',env0,' cm/s'
          Ae=0.0
          do j=jT1,jT2
          AE=AE+(henv(j)-env0)*dt
          enddo
        
         write(*,*) ' Envelope Area (AE): ',AE,' cm'
	  write(*,*) '==============END-', stn(i),'=================='
c    output parameters for each used stations          
         write(1,44) stn(i),epi,azi,hpgv,T1,T2,hSD,hSK,hKT,xmratio,LFHFR
	1_hor,env0,AE,xml,xmD,snr


          print*,'====================================================='

         endif

        enddo

        close(1)

   44 FORMAT(a4,1X,2f9.3,e13.3,3f8.2,2f7.2,1x,2f7.3,2(1x,e10.3),3f7.2) 



c    Sort the para_results.txt based on epicenter distance
	   open(1,file=oup(1:lo)//'para_result.txt',status='old')
          read(1,'(a132)') head
		do i=1,nused
          read(1,'(a132)') para(i)
	    read(para(i)(5:14),*) epi_sort(i)
	    enddo
	   close(1)
          do i=1,nused-1
	     do j=i+1,nused
	     if(epi_sort(i).gt.epi_sort(j)) then
	     para_tmp=para(j)
	     para(j)=para(i)
	     para(i)=para_tmp
	     epi_tmp=epi_sort(j)
	     epi_sort(j)=epi_sort(i)
	     epi_sort(i)=epi_tmp
	     endif
	     enddo
	    enddo

         nfinal=0
         open(1,file=oup(1:lo)//'para_result_sort.txt',status='unknown')
          write(1,'(a132)') head
	    do i=1,nused
          read(para(i)(126:132),*) snr_tmp
	    if(snr_tmp.ge.snr_threshold)  then 
		 write(1,'(a132)') para(i)
           nfinal=nfinal+1
          endif
	    enddo

         close(1)

        print*,' '
        print*,'-----------------Finished-----------------------------'
        print*,'Total the number of waveform data: ',nused
        print*,'The number of used station with SNR > 2.7 : ',nfinal
        print*,'To see para_result.txt in ',oup(1:lo)
        print*,'To see para_result_sort.txt in ',oup(1:lo)
        print*,'------------------------------------------------------'


	  END PROGRAM calpara

c------------------------------------------------------------
      SUBROUTINE demean(a,np,isps)
       real a(np)
       real*8 xsum
c   demean for acceleration data
        xsum=0.0
         do i=1,isps
         xsum=xsum+a(i)
         enddo
        xsum=xsum/real(isps)
         do i=1,np
         a(i)=a(i)-xsum
         enddo
       return
      END SUBROUTINE demean

c-----------------------------------------------------------------
c
c
       subroutine azimth(ellips,slat,slon,rlat,rlon,delta,azim,bazim)

c   This routine uses Euler angles to find the geocentric distance,
c   azimuth, and back azimuth for a source-reciever pair.
c
c   Input
c
c     slat  - source geographic latitude in decimal degrees
c     slon  - source longitude in decimal degrees
c     rlat  - reciever geographic latitude in decimal degrees
c     rlon  - reciever longitude in decimal degrees
c
c   Output
c
c     delta - geocentric source-reciever distance in decimal degrees of arc
c     azim  - geocentric azimuth from the source to the reciever
c     bazim - geocentric back azimuth from the reciever to the source
c
c   The distance calculated here delta is always between 0 and 180 degrees. 
c   Accordingly, the azimuth and back azimuth are defined for the minor 
c   arc between (slat,slon) and (rlat,rlon).

        implicit real*8 (a-h,o-z)
	integer ellips

        data flt/298.25d0/

	dtor=4.d0*datan(1.d0)/180.d0
	
	if(ellips.ne.0) then
          e=1.d0/flt
	else
	  e=0.d0
	endif

c   Convert to geocentric coordinates and from latitude to colatitude.

        slatra=dtor*slat
        w=dsin(slatra)
        s=((2.d0-e)*w+4.d0*e*(w**3))*e*dcos(slatra)
        scolat=1.5707963d0-slatra+s
        rlatra=dtor*rlat
        w=dsin(rlatra)
        s=((2.d0-e)*w+4.d0*e*(w**3))*e*dcos(rlatra)
        rcolat=1.5707963d0-rlatra+s

        slonra=slon*dtor
        rlonra=rlon*dtor
        c2=dcos(scolat)
        s2=dsin(scolat)
        c1=dcos(slonra)
        s1=dsin(slonra)
        slatrc=dsin(rcolat)

c   Find the azimuth and distance by rotating the source to the north pole.

        x0=slatrc*dcos(rlonra)
        y0=slatrc*dsin(rlonra)
        z0=dcos(rcolat)
        x1=c1*x0+s1*y0

        z0=dcos(rcolat)
        x1=c1*x0+s1*y0
        y1=-s1*x0+c1*y0
        z1=z0
        x2=c2*x1-s2*z1
        y2=y1
        z2=c2*z1+s2*x1
        call angles(x2,y2,z2,delta,azim)
        azim=180.d0-azim

c   Find the back azimuth by rotating the receiver to the north pole.

        c2=dcos(rcolat)
        s2=dsin(rcolat)
        c1=dcos(rlonra)
        s1=dsin(rlonra)
        slatrc=dsin(scolat)
        x0=slatrc*dcos(slonra)
        y0=slatrc*dsin(slonra)
        z0=dcos(scolat)
        x1=c1*x0+s1*y0
        y1=-s1*x0+c1*y0
        z1=z0
        x2=c2*x1-s2*z1
        y2=y1
        z2=c2*z1+s2*x1
        call angles(x2,y2,z2,delta,bazim)
        bazim=180.d0-bazim

        return
        end subroutine azimth
c
c
        subroutine angles(x,y,z,theta,phi)

c   Finds the angles theta and phi of a spherical polar coordinate
c   system from the cartesion coordinates x, y, and z.

        implicit real*8 (a-h,o-z)

        parameter(eps=1.d-14)

	  pi=4.d0*datan(1.d0)

        rtod=180.d0/pi
        arg1=dsqrt(x*x+y*y)
        theta=datan2(arg1,z)
        if(dabs(x).le.eps.and.dabs(y).le.eps) then
          phi=0.d0
        else
          phi=datan2(y,x)
        endif
        phi=phi*rtod
        theta=theta*rtod

        return
        end subroutine angles

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
        
        
c
c     Subroutine for calculate Wood-Anderson seismographs
c     freqency response at acceleration  Ts=0.8 sec
c
      subroutine woodand_acc(freq,xr,xi,damping)
      complex af
      pi=3.141592654
      w0=1.25*2.*pi
      w=2.*pi*freq
      if(damping.eq.0.0)then
        h=0.8
      else
        h=damping
      endif
      xr=-w*w+w0*w0
      xi=2.*w0*w*h
      af=cmplx(xr,xi)
      af=-1.0/af
      xr=real(af)
      xi=aimag(af)
      return
      end   