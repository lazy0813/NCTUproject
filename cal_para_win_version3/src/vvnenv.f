c-----------------------------------------------------------------
      SUBROUTINE envelope(ay,nt,dt,tp,ex,ey,n,inum,idcomp)
       real ay(nt),x(nt),y(nt),ex(n),ey(n)
c   idcomp is the number of decomposition       
       y=ay
       ex=0.
       ey=0.
       isps=nint(1./dt)
c   Envolope function determination from Tp point
c   Avoid noise before Tp
       ip1=nint(tp/dt)-isps
       ip2=nt-1
       inum=1 

        DO ip=ip1,ip2
         if(y(ip).gt.y(ip+1) .and. y(ip).gt.y(ip-1) )then
         inum=inum+1
	 ex(inum)=dt*ip
	 ey(inum)=y(ip)
         endif
        ENDDO

	IF(inum.GT.n) STOP 'ERROR: DIMENSIONAL ERROR...'
	
       IF(idcomp.gt.1) then
        Do id=1,idcomp-1
        inum1=inum
	inum=1 
	ip1=2
	ip2=inum1
	x=ex
        y=ey
	ex=0.
	ey=0.
         DO ip=ip1,ip2
          if(y(ip).gt.y(ip+1) .and. y(ip).gt.y(ip-1) )then
          inum=inum+1
	  ex(inum)=x(ip)
	  ey(inum)=y(ip)
          endif
         ENDDO
        ENDDO
       ENDIF
	
          IF(ex(1).eq.0.) THEN
	  inum=inum-1
	    DO i=1,inum
	    ex(i)=ex(i+1)
	    ey(i)=ey(i+1)
	    ENDDO
	  ENDIF

	 return
      END SUBROUTINE envelope