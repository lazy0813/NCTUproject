c     NOV. 18. 2011 modified by vvn, Ph.D student
c---------------------------------------------------------------------
      SUBROUTINE readsac(fn,np,dt,xc,yy,iday,hh,oo,sec,stn,rd,comp,e,s)
        
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
          real s(3),e(3)

        	xc=0.0
        	
c   Loading SAC files data to array
        
        	open(31,file=fn,status='old',form='binary')
        	read(31) sh
        	nps=sh%sac_header_integer(10)
        	dt=sh%sac_header_real(1)
        
c   Record information
        
        	stn=sh%sac_header_char(1)
        	comp=sh%sac_header_char(21)

        
        	slat=sh%sac_header_real(32)
        	slon=sh%sac_header_real(33)
        	sele=sh%sac_header_real(34)
		s(1)=slon
		s(2)=slat		 	 				 
		s(3)=sele
									 					
          yy=sh%sac_header_integer(1)															           	
        	iday=sh%sac_header_integer(2)
        	hh=sh%sac_header_integer(3)
        	oo=sh%sac_header_integer(4)
        	sec=sh%sac_header_integer(5)+sh%sac_header_integer(6)/1000.0
        	elat=sh%sac_header_real(36)
        	elon=sh%sac_header_real(37)
        	edep=sh%sac_header_real(39)
		e(1)=elon
		e(2)=elat		 	 				 
		e(3)=edep
        
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

c---------------------------------------------------------------------
      SUBROUTINE writesac(fn,np,dt,x,yy,day,hh,mn,sec,staname,comp
     &,e,s)
        
        	type sacheader
        	real*4 :: sac_header_real(70)
        	integer(4) :: sac_header_integer(35)
        	logical*4 :: sac_header_logical(5)
        	character*8 :: sac_header_char(24)
        	end type sacheader
        
        	real x(np)

        	type(sacheader) :: sh
        	character fn*200
        	character staname*4
	    character comp*3
	    character C*1
        	integer*4 yy,hh,mn,day
        	real sec
          real s(3),e(3)

c   Loading default value for SAC file
          sh%sac_header_real=-12345.0
	    sh%sac_header_integer=-12345
	    sh%sac_header_logical=1
          sh%sac_header_logical(5)=0
		sh%sac_header_char='-12345' 

c   Record information
          sh%sac_header_integer(1)=yy															           	
        	sh%sac_header_integer(2)=day
        	sh%sac_header_integer(3)=hh
        	sh%sac_header_integer(4)=mn
        	sh%sac_header_integer(5)=int(sec)
		sh%sac_header_integer(6)=(sec-int(sec))*1000.0
		sh%sac_header_integer(7)=6
		sh%sac_header_integer(8:9)=0
        	sh%sac_header_integer(10)=np
        	sh%sac_header_integer(16)=1
        	sh%sac_header_integer(17)=5
        	sh%sac_header_integer(18)=1
        	sh%sac_header_real(1)=dt
        	sh%sac_header_real(2)=-4.630000e+02
        	sh%sac_header_real(3)=2.660000e+02
        	sh%sac_header_real(6)=0.0
        	sh%sac_header_real(7)=dt*np		
        	sh%sac_header_real(32)=s(2)
        	sh%sac_header_real(33)=s(1)
        	sh%sac_header_real(34)=s(3)
        	sh%sac_header_real(36)=e(2)
        	sh%sac_header_real(37)=e(1)
        	sh%sac_header_real(39)=e(3)
          
          C=comp(3:3)
		IF(C.eq.'Z'.or.C.eq.'z') THEN
          sh%sac_header_real(58)=0.0
          sh%sac_header_real(59)=0.0
		 ELSE IF(C.eq.'E'.or.C.eq.'e') THEN
          sh%sac_header_real(58)=90.0
          sh%sac_header_real(59)=90.0	
		 ELSE IF(C.eq.'N'.or.C.eq.'n') THEN
          sh%sac_header_real(58)=0.0
          sh%sac_header_real(59)=90.0	
		ENDIF		
				             
        	sh%sac_header_char(1)=staname
        	sh%sac_header_char(21)=comp
	 
c          print*, C,sh%sac_header_real(58:59)

        	open(31,file=fn,status='unknown',form='binary')
        	write(31) sh
	    write(31) x
	    close(31)


        RETURN
      END SUBROUTINE writesac

c---------------------------------------------------------------------
      SUBROUTINE  day2date(iday,iy,im,id)
      integer*4  iy,im,id
      integer*4  iday,itmp(12)
          
      do 12 ik=1,12
         if ( (ik.eq.1).or.(ik.eq.3).or.(ik.eq.5).or.(ik.eq.7)    
     1        .or.(ik.eq.8).or.(ik.eq.10).or.(ik.eq.12) )          
     2         itmp(ik)=31
         if ( (ik.eq.4).or.(ik.eq.6).or.(ik.eq.9).or.(ik.eq.11) ) 
     1         itmp(ik)=30
         if ( (ik.eq.2) )  then
             if ( (mod(iy,4).eq.0).and.(mod(iy,400).ne.0))  then
                   itmp(ik)=29
             else  
                   itmp(ik)=28
             endif
         endif
 12    continue
      do 99 ik=1,12
         if ((iday-itmp(ik)).le.0) goto 100
            iday=iday-itmp(ik)
 99    continue
 100   im=ik
      id=iday
       RETURN
      END SUBROUTINE day2date


