c------------------------------------------------------
      SUBROUTINE resample(x,y,dt,ry,nt,inum,inpt)
       real ry(nt),x(nt),y(nt)
  

       inpt=nint(x(1)/dt)-1

        DO i=1,inpt
	 ry(i)=0.
	ENDDO

   99	  DO in=1,inum
	   dx=x(in+1)-x(in)
           dy=y(in+1)-y(in)
           iterp=nint(dx/dt)-1
	   IF(iterp.eq.0) goto 99
	   s=dy/dx
           DO it=0,iterp
	    inpt=inpt+1
	    ry(inpt)=y(in)+s*it*dt
	   ENDDO
          ENDDO

        return
      END SUBROUTINE resample