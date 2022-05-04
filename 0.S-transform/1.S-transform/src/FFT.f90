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
	END                                                         
	                                                            
	FUNCTION IBITR(J,NU)                                        
	J1=J                                                        
	IBITR=0                                                     
	DO 200 I=1,NU                                               
	J2=J1/2                                                     
	IBITR=IBITR*2+(J1-2*J2)                                     
200	J1=J2                                                       
	RETURN                                                      
	END