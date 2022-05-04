program fft_test
use MSFLIB 
  real x(90000),y(90000),xf(90000),yf(90000)
  real f(90000),famp(90000)
  character*64 inf,ouf
  integer i,pgopen,istat
  character(len=50) psfilename
  
  logical move

  print*,'Input filename'
  read(*,'(a)')inf
  print*,'Output filename'
  read(*,'(a)')ouf
  open(1,file=inf,status='old')
  print*,' Output PS filename'
  read(*,'(a)') psfilename
  
  xsum=0.0
  do i=1,90000
    read(1,*,iostat=iret)x(i),y(i)
    if(iret.lt.0)exit
    xsum=xsum+y(i)
  enddo
  close(1)
  np=i-1
  dt=abs(x(2)-x(1))
  xsum=xsum/real(np)
  y=y-xsum
  yf=y
  do i=2,16
    n=2**i
	if(n.ge.np)exit
  enddo
  nu=i

  do i=np+1,n
	 yf(i)=0.0
  enddo
  xf=0.0


!-- FFT
   call fft(yf,xf,n,nu)
   df=1./(n*dt)
   yf=yf/real(n)
   xf=xf/real(n)

!   y(1)=0.0
!   x(1)=0.0
   yf(n)=0.0
   xf(n)=0.0

  write(*,'(1X,a20,i7)') 'Total data point np: ',np
  write(*,'(1X,a20,f6.3)') 'Sampling interval dt: ',dt
  write(*,'(1X,a20,f7.2)') 'Nyquist freq.: ',1./dt/2.
  write(*,'(1X,a20,f6.2)') 'Sampling interval df: ',df

   open(2,file=ouf,status='unknown')
   write(2,*) '    FRE.     REAL.    IMAG.    AMP.'
   do i=1,n/2-1
   f(i)=(i-1.0)*df
   famp(i)=sqrt(yf(i)*yf(i)+xf(i)*xf(i))
   write(2,'(f10.4,3(1x,e14.8))')f(i),xf(i),yf(i),famp(i)
   enddo
   close(2)

! start to plot for psplot.f
 psfilename=trim(psfilename)//'.ps'
 call newdev(trim(psfilename),8)
 call psinit(.false.)
 call setfnt(26)
 call factor(0.7)
 xlen=4.0
 ylen=2.0
 dy0=0.8
 dx0=0.8
 x0=1.0
 y0=8.0   

 t1=x(1)
 t2=x(np)
 dx=real(nint(t2-t1))
!    plot time series data
 y2=maxval(abs(y(1:np)))
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
 x1=x(i)
 call convert_xy(xp,yp,x1,y(i),x0,y0,xlen,ylen,t1,t2,y1,y2)
 if(move) then
 call plot(xp,yp,3)
 move=.false.
 else
 call plot(xp,yp,2)
 endif
 enddo

 t1=f(1)
 t2=f(n/2-1)
 dx=real(nint(t2-t1))
!    plot spectral amplitude
 y2=maxval(abs(famp(1:np)))
 y1=minval(famp(1:np))
 dy=y2-y1
 y0=y0-ylen-dy0
 call setcolr(0.,0.,0.)
 call setlw(0.03)
 call axis(x0,y0,14hFrequency (Hz),-14,xlen,0.,0.0,dx/xlen) !-- X-axis
 call axis(x0,y0,13hSpectral Amp.,13,ylen,90.,y1,dy/ylen) !-- Y-axis
         
 call setcolr(0.,0.,0.0)
 call setlw(0.03)
 call plot(x0,y0+ylen,3)
 call plot(x0+xlen,y0+ylen,2)
 call plot(x0+xlen,y0,2)

 call setlw(0.01)
 move=.true.
 do i=1,n/2-1
 x1=f(i)
 call convert_xy(xp,yp,x1,famp(i),x0,y0,xlen,ylen,t1,t2,y1,y2)
 if(move) then
 call plot(xp,yp,3)
 move=.false.
 else
 call plot(xp,yp,2)
 endif
 enddo

 call plotnd

! start to plot for pgplot.f
!  psfilename=trim(psfilename)//'.ps/vcps'
!  write(*,*) psfilename
        
!   istat=pgopen(psfilename)
!  if(istat.ne.0) stop ' ERROR: Opening for PS file'
!        call pgsubp(2,4)
!        xmin=minval(x(1:np))
!        xmax=maxval(x(1:np))
!        ymin=minval(y(1:np))
!        ymax=maxval(y(1:np))
!        call pgslw(1)
!        call pgscf(3)
!        call pgsch(2.5)
!        call pgenv(xmin,xmax,ymin,ymax,2,0)
!        call pglab('Time','Amplitude','Time Series Plot')
!        call pgslw(2)
!        do i=1,np-1
!         call pgmove(x(i),y(i))
!         call pgdraw(x(i+1),y(i+1))
!        end do

!        xmin=minval(f(1:n/2-1))
!        xmax=maxval(f(1:n/2-1))
!        ymin=minval(famp(1:n/2-1))
!        ymax=maxval(famp(1:n/2-1))
!        call pgslw(1)
!        call pgscf(3)
!        call pgsch(2.)
!        call pgenv(xmin,5.,ymin,ymax,2,0)
!        call pgenv(xmin,xmax,ymin,ymax,2,0)
!        call pglab('Frequency','Spectral Amplitude','Spectrum Plot')
!        call pgslw(2)
!        do i=1,n/2-1
!         call pgmove(f(i),famp(i))
!         call pgdraw(f(i+1),famp(i+1))
!        end do
        
!        call pgend   
   
end	program fft_test 


SUBROUTINE FFT(XREAL,XIMAG,N,NU)
	DIMENSION XREAL(N),XIMAG(N)
	N2=N/2
	NU1=NU-1
	K=0
	DO 100 L=1,NU
102	DO 101 I=1,N2
	P=IBITR(K/2**NU1,NU)
	ARG=6.283185*P/FLOAT(N)
	C=COS(ARG)
	S=SIN(ARG)
	K1=K+1
	K1N2=K1+N2
	TREAL=XREAL(K1N2)*C+XIMAG(K1N2)*S
	TIMAG=XIMAG(K1N2)*C-XREAL(K1N2)*S
	XREAL(K1N2)=XREAL(K1)-TREAL
	XIMAG(K1N2)=XIMAG(K1)-TIMAG
	XREAL(K1)=XREAL(K1)+TREAL
	XIMAG(K1)=XIMAG(K1)+TIMAG
101	K=K+1
	K=K+N2
	IF(K.LT.N) GOTO 102
	K=0
	NU1=NU1-1
100	N2=N2/2
	DO 103 K=1,N
	I=IBITR(K-1,NU)+1
	IF(I.LE.K) GOTO 103
	TREAL=XREAL(K)
	TIMAG=XIMAG(K)
	XREAL(K)=XREAL(I)
	XIMAG(K)=XIMAG(I)
	XREAL(I)=TREAL
	XIMAG(I)=TIMAG
103	CONTINUE
	RETURN
END	SUBROUTINE FFT

FUNCTION IBITR(J,NU)
	J1=J
	IBITR=0
	DO 200 I=1,NU
	J2=J1/2
	IBITR=IBITR*2+(J1-2*J2)
200	J1=J2
	RETURN
END	FUNCTION IBITR

    subroutine convert_xy(xp,yp,x,y,x0,y0,xlen,ylen,xmin,xmax,ymin,ymax)
       xp=x0+xlen/(xmax-xmin)*(x-xmin)
       yp=y0+ylen/(ymax-ymin)*(y-ymin)
     end subroutine convert_xy