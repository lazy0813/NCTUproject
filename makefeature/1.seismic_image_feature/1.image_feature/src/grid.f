      program grid
	parameter(npmax=65536)
       real dx,dy
	 integer ngrid,nx,ny
	 real gx(npmax),gy(npmax)
	 real cx(npmax),cy(npmax)
	 real ex(npmax),ey(npmax)
	 integer ig_circle(npmax),igc_total
	 integer ig_lq(npmax),iglq_total
	 integer ig_ineq(npmax),igineq_total
	 integer ig_ofeq(npmax),igofeq_total
	 integer ig_event(npmax),ige_total   !-- ig_event is grid point of station shape
       real pi,pi2rad,theta
	 real ymax(7) 

       real lqx(7),lqy(7),ineqx(7),ineqy(7),ofeqx(7),ofeqy(7)
	 real SD,KT,SK,MLMD,SI,SRLH
	 character(len=10) stmp
c------ max. shape
       real lqxm(7),lqym(7),ineqxm(7),ineqym(7),ofeqxm(7),ofeqym(7)
	 integer ig_lq_max(npmax),iglq_total_max
	 integer ig_ineq_max(npmax),igineq_total_max
	 integer ig_ofeq_max(npmax),igofeq_total_max

       integer pgopen,istat
	 character(len=50) ps
       character(len=58) str_result
	 character(len=4) str_event

       dx=0.01
	 dy=0.01
	 nx=nint(2.0/dx)
	 ny=nint(2.0/dy)
	 ngrid=nx*ny
	 ig=0
c    create grid
       do j=1,ny
	  do i=1,nx
        ig=ig+1
	  gx(ig)=-1.0+(i-1)*dx
	  gy(ig)=1.0-(j-1)*dy
	  enddo
       enddo
       write(*,*) ' Total grid point: ', ig
c    create circle and find grid point located in circle
       cx=0.
	 cy=0.
	 pi=atan(1.0)*4.0
	 pi2rad=pi/180.0
	 do i=1,360
        theta=real(i)
	  cx(i)=cos(theta*pi2rad)
	  cy(i)=sin(theta*pi2rad)
	 enddo
	 ig=0
	 ig_circle=0
	 igc_total=0
	 do ng=1,ngrid
	  call locpt(gx(ng),gy(ng),cx,cy,360,l,m)
	  if(l.ge.0) then
        igc_total=igc_total+1
	  ig_circle(igc_total)=ng
	  endif
       enddo
       write(*,*) ' Total grid in circle: ',igc_total

c    plot circle grid

       ps='circle.ps/vcps'        
       istat=pgopen(ps)
       call pgenv(-1.0,1.0,-1.0,1.0,1,-2)
c    plot circle line
       call pgmove(cx(1),cy(1))
	 do i=2,360
       call pgdraw(cx(i),cy(i))
	 enddo
c    plot grid inside circle	 
	 do i=1,igc_total 
        ng=ig_circle(i)
        x1=gx(ng)-dx*0.7
        x2=gx(ng)+dx*0.7
        y1=gy(ng)-dy*0.7
        y2=gy(ng)+dy*0.7
	  call pgscr(30,0.5,0.5,0.5)
	  call pgsci(30)
        call pgrect(x1,x2,y1,y2)
       enddo

       call pgend

c    read input median event shape for landquake(LQ), inland earthquake(INEQ), offshore earthquake(OFEQ)
       open(1,file='input_med_shape.txt',status='old')
	 read(1,*) !-- header
c    read landquake shape
       read(1,*) stmp,SD,KT,SK,MLMD,SI,SRLH
	 lqx(1)=SD*cos(60.0*pi2rad)*(-1.0)
       lqy(1)=SD*sin(60.0*pi2rad)
	 lqx(2)=KT*cos(60.0*pi2rad)
       lqy(2)=KT*sin(60.0*pi2rad)
	 lqx(3)=SK
       lqy(3)=0.0
	 lqx(4)=MLMD*cos(60.0*pi2rad)
       lqy(4)=MLMD*sin(60.0*pi2rad)*(-1.0)
	 lqx(5)=SI*cos(60.0*pi2rad)*(-1.0)
       lqy(5)=SI*sin(60.0*pi2rad)*(-1.0)
	 lqx(6)=SRLH*(-1.0)
       lqy(6)=0.0
	 lqx(7)=lqx(1)
       lqy(7)=lqy(1)
       
	 iglq_total=0.
	 ig_lq=0.
       do i=1,igc_total
        ng=ig_circle(i)
	  call locpt(gx(ng),gy(ng),lqx,lqy,7,l,m)
	  if(l.ge.0) then
	  iglq_total=iglq_total+1
	  ig_lq(iglq_total)=ng
	  endif
       enddo
       write(*,*) ' Total LQ median shape grid: ',iglq_total
c    plot LQ shape grid

       ps='landquake_median.ps/vcps'        
       istat=pgopen(ps)
       call pgenv(-1.0,1.0,-1.0,1.0,1,-2)
c    plot circle line
       call pgmove(cx(1),cy(1))
	 do i=2,360
       call pgdraw(cx(i),cy(i))
	 enddo
c    plot grid inside circle	 
	 do i=1,iglq_total 
        ng=ig_lq(i)
        x1=gx(ng)-dx*0.7
        x2=gx(ng)+dx*0.7
        y1=gy(ng)-dy*0.7
        y2=gy(ng)+dy*0.7
	  call pgscr(30,0.5,0.5,0.5)
	  call pgsci(30)
        call pgrect(x1,x2,y1,y2)
       enddo

       call pgend

c    read inland earthquake shape
       read(1,*) stmp,SD,KT,SK,MLMD,SI,SRLH
	 ineqx(1)=SD*cos(60.0*pi2rad)*(-1.0)
       ineqy(1)=SD*sin(60.0*pi2rad)
	 ineqx(2)=KT*cos(60.0*pi2rad)
       ineqy(2)=KT*sin(60.0*pi2rad)
	 ineqx(3)=SK
       ineqy(3)=0.0
	 ineqx(4)=MLMD*cos(60.0*pi2rad)
       ineqy(4)=MLMD*sin(60.0*pi2rad)*(-1.0)
	 ineqx(5)=SI*cos(60.0*pi2rad)*(-1.0)
       ineqy(5)=SI*sin(60.0*pi2rad)*(-1.0)
	 ineqx(6)=SRLH*(-1.0)
       ineqy(6)=0.0
	 ineqx(7)=ineqx(1)
       ineqy(7)=ineqy(1)
       
	 igineq_total=0.
	 ig_ineq=0.
       do i=1,igc_total
        ng=ig_circle(i)
	  call locpt(gx(ng),gy(ng),ineqx,ineqy,7,l,m)
	  if(l.ge.0) then
	  igineq_total=igineq_total+1
	  ig_ineq(igineq_total)=ng
	  endif
       enddo
       write(*,*) ' Total INEQ median shape grid: ',igineq_total
c    plot LQ shape grid

       ps='inland_EQ_median.ps/vcps'        
       istat=pgopen(ps)
       call pgenv(-1.0,1.0,-1.0,1.0,1,-2)
c    plot circle line
       call pgmove(cx(1),cy(1))
	 do i=2,360
       call pgdraw(cx(i),cy(i))
	 enddo
c    plot grid inside circle	 
	 do i=1,igineq_total 
        ng=ig_ineq(i)
        x1=gx(ng)-dx*0.7
        x2=gx(ng)+dx*0.7
        y1=gy(ng)-dy*0.7
        y2=gy(ng)+dy*0.7
	  call pgscr(30,0.5,0.5,0.5)
	  call pgsci(30)
        call pgrect(x1,x2,y1,y2)
       enddo

       call pgend

c    read offshore earthquake shape 
       read(1,*) stmp,SD,KT,SK,MLMD,SI,SRLH
	 ofeqx(1)=SD*cos(60.0*pi2rad)*(-1.0)
       ofeqy(1)=SD*sin(60.0*pi2rad)
	 ofeqx(2)=KT*cos(60.0*pi2rad)
       ofeqy(2)=KT*sin(60.0*pi2rad)
	 ofeqx(3)=SK
       ofeqy(3)=0.0
	 ofeqx(4)=MLMD*cos(60.0*pi2rad)
       ofeqy(4)=MLMD*sin(60.0*pi2rad)*(-1.0)
	 ofeqx(5)=SI*cos(60.0*pi2rad)*(-1.0)
       ofeqy(5)=SI*sin(60.0*pi2rad)*(-1.0)
	 ofeqx(6)=SRLH*(-1.0)
       ofeqy(6)=0.0
	 ofeqx(7)=ofeqx(1)
       ofeqy(7)=ofeqy(1)
       
	 igofeq_total=0.
	 ig_ofeq=0.
       do i=1,igc_total
        ng=ig_circle(i)
	  call locpt(gx(ng),gy(ng),ofeqx,ofeqy,7,l,m)
	  if(l.ge.0) then
	  igofeq_total=igofeq_total+1
	  ig_ofeq(igofeq_total)=ng
	  endif
       enddo
       write(*,*) ' Total OFEQ median shape grid: ',igofeq_total
c    plot LQ shape grid

       ps='offshore_EQ_median.ps/vcps'        
       istat=pgopen(ps)
       call pgenv(-1.0,1.0,-1.0,1.0,1,-2)
c    plot circle line
       call pgmove(cx(1),cy(1))
	 do i=2,360
       call pgdraw(cx(i),cy(i))
	 enddo
c    plot grid inside circle	 
	 do i=1,igofeq_total 
        ng=ig_ofeq(i)
        x1=gx(ng)-dx*0.7
        x2=gx(ng)+dx*0.7
        y1=gy(ng)-dy*0.7
        y2=gy(ng)+dy*0.7
	  call pgscr(30,0.5,0.5,0.5)
	  call pgsci(30)
        call pgrect(x1,x2,y1,y2)
       enddo

       call pgend
       close(1)

c------------------------------------------------------------------------------------
c    read input maxmium event shape for landquake(LQ), inland earthquake(INEQ), offshore earthquake(OFEQ)
       open(1,file='input_max_shape.txt',status='old')
	 read(1,*) !-- header
c    read landquake shape
       read(1,*) stmp,SD,KT,SK,MLMD,SI,SRLH
	 lqxm(1)=SD*cos(60.0*pi2rad)*(-1.0)
       lqym(1)=SD*sin(60.0*pi2rad)
	 lqxm(2)=KT*cos(60.0*pi2rad)
       lqym(2)=KT*sin(60.0*pi2rad)
	 lqxm(3)=SK
       lqym(3)=0.0
	 lqxm(4)=MLMD*cos(60.0*pi2rad)
       lqym(4)=MLMD*sin(60.0*pi2rad)*(-1.0)
	 lqxm(5)=SI*cos(60.0*pi2rad)*(-1.0)
       lqym(5)=SI*sin(60.0*pi2rad)*(-1.0)
	 lqxm(6)=SRLH*(-1.0)
       lqym(6)=0.0
	 lqxm(7)=lqxm(1)
       lqym(7)=lqym(1)
       
	 iglq_total_max=0.
	 ig_lq_max=0.
       do i=1,igc_total
        ng=ig_circle(i)
	  call locpt(gx(ng),gy(ng),lqxm,lqym,7,l,m)
	  if(l.ge.0) then
	  iglq_total_max=iglq_total_max+1
	  ig_lq_max(iglq_total_max)=ng
	  endif
       enddo
       write(*,*) ' Total LQ max. shape grid: ',iglq_total_max
c    plot LQ shape grid

       ps='landquake_max.ps/vcps'        
       istat=pgopen(ps)
       call pgenv(-1.0,1.0,-1.0,1.0,1,-2)
c    plot circle line
       call pgmove(cx(1),cy(1))
	 do i=2,360
       call pgdraw(cx(i),cy(i))
	 enddo
c    plot grid inside circle	 
	 do i=1,iglq_total_max 
        ng=ig_lq_max(i)
        x1=gx(ng)-dx*0.7
        x2=gx(ng)+dx*0.7
        y1=gy(ng)-dy*0.7
        y2=gy(ng)+dy*0.7
	  call pgscr(30,0.5,0.5,0.5)
	  call pgsci(30)
        call pgrect(x1,x2,y1,y2)
       enddo

       call pgend

c    read inland earthquake shape
       read(1,*) stmp,SD,KT,SK,MLMD,SI,SRLH
	 ineqxm(1)=SD*cos(60.0*pi2rad)*(-1.0)
       ineqym(1)=SD*sin(60.0*pi2rad)
	 ineqxm(2)=KT*cos(60.0*pi2rad)
       ineqym(2)=KT*sin(60.0*pi2rad)
	 ineqxm(3)=SK
       ineqym(3)=0.0
	 ineqxm(4)=MLMD*cos(60.0*pi2rad)
       ineqym(4)=MLMD*sin(60.0*pi2rad)*(-1.0)
	 ineqxm(5)=SI*cos(60.0*pi2rad)*(-1.0)
       ineqym(5)=SI*sin(60.0*pi2rad)*(-1.0)
	 ineqxm(6)=SRLH*(-1.0)
       ineqym(6)=0.0
	 ineqxm(7)=ineqxm(1)
       ineqym(7)=ineqym(1)
       
	 igineq_total_max=0.
	 ig_ineq_max=0.
       do i=1,igc_total
        ng=ig_circle(i)
	  call locpt(gx(ng),gy(ng),ineqxm,ineqym,7,l,m)
	  if(l.ge.0) then
	  igineq_total_max=igineq_total_max+1
	  ig_ineq_max(igineq_total_max)=ng
	  endif
       enddo
       write(*,*) ' Total INEQ max. shape grid: ',igineq_total_max
c    plot LQ shape grid

       ps='inland_EQ_max.ps/vcps'        
       istat=pgopen(ps)
       call pgenv(-1.0,1.0,-1.0,1.0,1,-2)
c    plot circle line
       call pgmove(cx(1),cy(1))
	 do i=2,360
       call pgdraw(cx(i),cy(i))
	 enddo
c    plot grid inside circle	 
	 do i=1,igineq_total_max 
        ng=ig_ineq_max(i)
        x1=gx(ng)-dx*0.7
        x2=gx(ng)+dx*0.7
        y1=gy(ng)-dy*0.7
        y2=gy(ng)+dy*0.7
	  call pgscr(30,0.5,0.5,0.5)
	  call pgsci(30)
        call pgrect(x1,x2,y1,y2)
       enddo

       call pgend

c    read offshore earthquake shape 
       read(1,*) stmp,SD,KT,SK,MLMD,SI,SRLH
	 ofeqxm(1)=SD*cos(60.0*pi2rad)*(-1.0)
       ofeqym(1)=SD*sin(60.0*pi2rad)
	 ofeqxm(2)=KT*cos(60.0*pi2rad)
       ofeqym(2)=KT*sin(60.0*pi2rad)
	 ofeqxm(3)=SK
       ofeqym(3)=0.0
	 ofeqxm(4)=MLMD*cos(60.0*pi2rad)
       ofeqym(4)=MLMD*sin(60.0*pi2rad)*(-1.0)
	 ofeqxm(5)=SI*cos(60.0*pi2rad)*(-1.0)
       ofeqym(5)=SI*sin(60.0*pi2rad)*(-1.0)
	 ofeqxm(6)=SRLH*(-1.0)
       ofeqym(6)=0.0
	 ofeqxm(7)=ofeqxm(1)
       ofeqym(7)=ofeqym(1)
       
	 igofeq_total_max=0.
	 ig_ofeq_max=0.
       do i=1,igc_total
        ng=ig_circle(i)
	  call locpt(gx(ng),gy(ng),ofeqxm,ofeqym,7,l,m)
	  if(l.ge.0) then
	  igofeq_total_max=igofeq_total_max+1
	  ig_ofeq_max(igofeq_total_max)=ng
	  endif
       enddo
       write(*,*) ' Total OFEQ max. shape grid: ',igofeq_total_max
c    plot LQ shape grid

       ps='offshore_EQ_max.ps/vcps'        
       istat=pgopen(ps)
       call pgenv(-1.0,1.0,-1.0,1.0,1,-2)
c    plot circle line
       call pgmove(cx(1),cy(1))
	 do i=2,360
       call pgdraw(cx(i),cy(i))
	 enddo
c    plot grid inside circle	 
	 do i=1,igofeq_total_max 
        ng=ig_ofeq_max(i)
        x1=gx(ng)-dx*0.7
        x2=gx(ng)+dx*0.7
        y1=gy(ng)-dy*0.7
        y2=gy(ng)+dy*0.7
	  call pgscr(30,0.5,0.5,0.5)
	  call pgsci(30)
        call pgrect(x1,x2,y1,y2)
       enddo

       call pgend
       close(1)

       open(2,file='simple_typology.txt',status='unknown')

c    read input station shape
       open(1,file='input_station_shape.txt',status='old')
	 read(1,*)  !-- header
         ievent=0
	   do 
	   read(1,*,iostat=istat) SD,KT,SK,MLMD,SI,SRLH
	   if(istat.ne.0)  exit
	   ievent=ievent+1
c    normalized shape
c         ymax(1)=SD
c	   ymax(2)=KT
c	   ymax(3)=SK
c	   ymax(4)=MLMD
c	   ymax(5)=SI
c	   ymax(6)=SRLH
c	   SD=SD/maxval(ymax)
c	   KT=KT/maxval(ymax)
c	   SK=SK/maxval(ymax)
c	   MLMD=MLMD/maxval(ymax)
c	   SI=SI/maxval(ymax)
c	   SRLH=SRLH/maxval(ymax) 
c    find station shape
	   ex(1)=SD*cos(60.0*pi2rad)*(-1.0)
         ey(1)=SD*sin(60.0*pi2rad)
	   ex(2)=KT*cos(60.0*pi2rad)
         ey(2)=KT*sin(60.0*pi2rad)
	   ex(3)=SK
         ey(3)=0.0
	   ex(4)=MLMD*cos(60.0*pi2rad)
         ey(4)=MLMD*sin(60.0*pi2rad)*(-1.0)
	   ex(5)=SI*cos(60.0*pi2rad)*(-1.0)
         ey(5)=SI*sin(60.0*pi2rad)*(-1.0)
	   ex(6)=SRLH*(-1.0)
         ey(6)=0.0
	   ex(7)=ex(1)
         ey(7)=ey(1)
         ige_total=0
	   ig_event=0
c    find station shape grid in circle
          do i=1,igc_total
           ng=ig_circle(i)
	     call locpt(gx(ng),gy(ng),ex,ey,7,l,m)
	     if(l.ge.0) then
           ige_total=ige_total+1
	     ig_event(ige_total)=ng
           endif
	    enddo  
c          write(*,*) ievent, ' Total normalized station grid: ', ige_total

c-----------------------------------for median shape-----------------------
c    find station shape grid in LQ median shape
          ige_lq_total=0
	    do i=1,ige_total
           ng=ig_event(i)
	     call locpt(gx(ng),gy(ng),lqx,lqy,7,l,m)
           if(l.ge.0) then
		 ige_lq_total=ige_lq_total+1
	     endif
          enddo
c	    write(*,*) ' Total grid inside lq median shape: ',ige_lq_total
c    compute probability of LQ: best answer of plq1=1.0
          plq1=real(ige_lq_total)/real(iglq_total)
	    plq3=(real(ige_total)-real(ige_lq_total))/real(ige_total)
	    plq3=1.0-plq3
c          print*, plq1
 
c    find station shape grid in INEQ shape
          ige_ineq_total=0
	    do i=1,ige_total
           ng=ig_event(i)
	     call locpt(gx(ng),gy(ng),ineqx,ineqy,7,l,m)
           if(l.ge.0) then
		 ige_ineq_total=ige_ineq_total+1
	     endif
          enddo
c	    write(*,*) ' Total grid inside ineq median shape: ',ige_ineq_total
c    compute probability of INEQ: best answer of pineq1=1.0
          pineq1=real(ige_ineq_total)/real(igineq_total)
	    pineq3=(real(ige_total)-real(ige_ineq_total))/real(ige_total)
	    pineq3=1.0-pineq3
c          print*, pineq1

c    find station shape grid in OFEQ shape
          ige_ofeq_total=0
	    do i=1,ige_total
           ng=ig_event(i)
	     call locpt(gx(ng),gy(ng),ofeqx,ofeqy,7,l,m)
           if(l.ge.0) then
		 ige_ofeq_total=ige_ofeq_total+1
	     endif
          enddo
c	    write(*,*) ' Total grid inside ofeq median shape: ',ige_ofeq_total
c    compute probability of OFEQ: best answer of pofeq1=1.0
          pofeq1=real(ige_ofeq_total)/real(igofeq_total)
	    pofeq3=(real(ige_total)-real(ige_ofeq_total))/real(ige_total)
	    pofeq3=1.0-pofeq3
c           print*, pofeq1
c          print*,plq1,pineq1,pofeq1
c          print*,plq3,pineq3,pofeq3
c--------------------------------------for max. shape portion-------------------
c    find station shape grid in LQ max. shape
          ige_lq_total=0
	    do i=1,ige_total
           ng=ig_event(i)
	     call locpt(gx(ng),gy(ng),lqxm,lqym,7,l,m)
           if(l.ge.0) then
		 ige_lq_total=ige_lq_total+1
	     endif
          enddo
c	    write(*,*) ' Total grid inside lq max. shape: ',ige_lq_total
c    compute probability of LQ: best answer of plq2=0.0
          plq2=real(ige_total)-real(ige_lq_total)
	    plq2=1.0-plq2/real(ige_total)
c          print*, plq2
 
c    find station shape grid in INEQ shape
          ige_ineq_total=0
	    do i=1,ige_total
           ng=ig_event(i)
	     call locpt(gx(ng),gy(ng),ineqxm,ineqym,7,l,m)
           if(l.ge.0) then
		 ige_ineq_total=ige_ineq_total+1
	     endif
          enddo
c	    write(*,*) ' Total grid inside ineq max. shape: ',ige_ineq_total
c    compute probability of INEQ: best answer of pineq2=0.0
		pineq2=real(ige_total)-real(ige_ineq_total)
	    pineq2=1.0-pineq2/real(ige_total)
c          print*, pineq2
          
c    find station shape grid in OFEQ shape
          ige_ofeq_total=0
	    do i=1,ige_total
           ng=ig_event(i)
	     call locpt(gx(ng),gy(ng),ofeqxm,ofeqym,7,l,m)
           if(l.ge.0) then
		 ige_ofeq_total=ige_ofeq_total+1
	     endif
          enddo
c	    write(*,*) ' Total grid inside ofeq max. shape: ',ige_ofeq_total
c    compute probability of OFEQ: best answer of pofeq1=1.0
          pofeq2=real(ige_total)-real(ige_ofeq_total)
	    pofeq2=1.0-pofeq2/real(ige_total)
c           print*, pofeq2
c          print*,plq2,pineq2,pofeq2

c    define final probability
          plq=plq1+plq3+plq2
          pineq=pineq1+pineq3+pineq2
	    pofeq=pofeq1+pofeq3+pofeq2
c          print*,plq,pineq,pofeq

c---------------------------------------------plot portion-----------
c    plot event shape
           write(str_event,'(I4.4)') ievent
		 ps=str_event//'event.ps/vcps'        
           istat=pgopen(ps)
           call pgenv(-2.0,2.0,-2.0,2.0,1,-2)
c    plot circle line
           call pgmove(cx(1),cy(1))
	     do i=2,360
           call pgdraw(cx(i),cy(i))
	     enddo
c    plot grid inside circle	 
	     do i=1,ige_total 
            ng=ig_event(i)
             x1=gx(ng)-dx*0.7
             x2=gx(ng)+dx*0.7
             y1=gy(ng)-dy*0.7
             y2=gy(ng)+dy*0.7
	       call pgscr(30,0.5,0.5,0.5)
	       call pgsci(30)
             call pgrect(x1,x2,y1,y2)
           enddo

c    plot result information
           str_result='Event XXXX Simple Typology: XX.XX LQ XX.XX INEQ X
     1X.XX OFEQ'
           write(str_result(7:10),'(I4.4)') ievent
           write(str_result(29:33),'(F5.2)') plq
           write(str_result(38:42),'(F5.2)') pineq
           write(str_result(49:53),'(F5.2)') pofeq
           
		 if(plq.gt.pineq.and.plq.gt.pofeq) then
		 write(*,'(1X,a58,1X,a5)') str_result,'LQ'
		 write(2,'(1X,a58,1X,a5)') str_result,'LQ'
		 else if(pineq.gt.plq.and.pineq.gt.pofeq) then
		 write(*,'(1X,a58,1X,a5)') str_result,'INEQ'
		 write(2,'(1X,a58,1X,a5)') str_result,'INEQ'
	     else 
		 write(*,'(1X,a58,1X,a5)') str_result,'OFEQ'
		 write(2,'(1X,a58,1X,a5)') str_result,'OFEQ'
		 endif  
           
		 call pgsch(1.0)
	     call pgslw(3)
           call pgscf(3) 
		 call pgsci(1)
		 call pgtext(-1.0,1.2,str_result(29:58))
           call pgtext(cos(60.*pi2rad)*(-1.0),sin(60.*pi2rad)+0.1,'SD')
           call pgtext(cos(60.*pi2rad),sin(60.*pi2rad)+0.1,'KT')
           call pgtext(1.1,0.0,'SK')
           call 
     1pgtext(cos(60.*pi2rad)+0.1,sin(60.*pi2rad)*(-1.0),'M\dL\u/M\dD')
	     call 
     1pgtext(cos(60.*pi2rad)*(-1.0)-0.1,sin(60.*pi2rad)*(-1.0)-0.1,'SI') 
           call pgtext(-1.4,0.0,'SRLH')


c    plot lq shape median boundary
           call pgsci(1)
	     call pgslw(5)
           call pgmove(lqx(1),lqy(1))
	     do i=2,7
           call pgdraw(lqx(i),lqy(i))
	     enddo
c    plot ineq shape median boundary
           call pgsci(2)
	     call pgslw(5)
           call pgmove(ineqx(1),ineqy(1))
	     do i=2,7
           call pgdraw(ineqx(i),ineqy(i))
	     enddo
c    plot ofeq shape median boundary
           call pgsci(3)
	     call pgslw(5)
           call pgmove(ofeqx(1),ofeqy(1))
	     do i=2,7
           call pgdraw(ofeqx(i),ofeqy(i))
	     enddo


c    plot lq shape max. boundary
           call pgsci(1)
	     call pgslw(1)
           call pgmove(lqxm(1),lqym(1))
	     do i=2,7
           call pgdraw(lqxm(i),lqym(i))
	     enddo
c    plot ineq shape max. boundary
           call pgsci(2)
	     call pgslw(1)
           call pgmove(ineqxm(1),ineqym(1))
	     do i=2,7
           call pgdraw(ineqxm(i),ineqym(i))
	     enddo
c    plot ofeq shape max. boundary
           call pgsci(3)
	     call pgslw(1)
           call pgmove(ofeqxm(1),ofeqym(1))
	     do i=2,7
           call pgdraw(ofeqxm(i),ofeqym(i))
	     enddo



           call pgend

         enddo
       close(1)
       close(2)
	end program grid