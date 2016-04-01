C
C     ***********************
C     SIMPLE DRIVER FOR LBFGS
C     ***********************
C
C     Example of driver for LBFGS routine, using a
C     simple test problem. The solution point is at
C     X=(1,...,1) and the optimal function value of 0.
C
C                          JORGE NOCEDAL
C                        *** July 1990 ***
C
      PROGRAM SDRIVE
      PARAMETER(NDIM=2000,MSAVE=7,NWORK=NDIM*(2*MSAVE +1)+2*MSAVE)
      DOUBLE PRECISION X(NDIM),G(NDIM),DIAG(NDIM),W(NWORK)
      DOUBLE PRECISION F,EPS,XTOL,GTOL,T1,T2,STPMIN,STPMAX
      INTEGER IPRINT(2),IFLAG,ICALL,N,M,MP,LP,J
      LOGICAL DIAGCO
C
C     The driver for LBFGS must always declare LB2 as EXTERNAL
C
      EXTERNAL LB2
      COMMON /LB3/MP,LP,GTOL,STPMIN,STPMAX
C
      N=100
      M=5
      IPRINT(1)= 1
      IPRINT(2)= 0
C
C     We do not wish to provide the diagonal matrices Hk0, and
C     therefore set DIAGCO to FALSE.
C
      DIAGCO= .FALSE.
      EPS= 1.0D-5
      XTOL= 1.0D-16
      ICALL=0
      IFLAG=0
      DO 10 J=1,N,2
         X(J)=-1.2D0
         X(J+1)=1.D0
 10   CONTINUE
C
 20   CONTINUE
      F= 0.D0
      DO 30 J=1,N,2
        T1= 1.D0-X(J)
        T2= 1.D1*(X(J+1)-X(J)**2)
        G(J+1)= 2.D1*T2
        G(J)= -2.D0*(X(J)*G(J+1)+T1)
        F= F+T1**2+T2**2
 30   CONTINUE
      CALL LBFGS(N,M,X,F,G,DIAGCO,DIAG,IPRINT,EPS,XTOL,W,IFLAG)
      IF(IFLAG.LE.0) GO TO 50
      ICALL=ICALL + 1
C     We allow at most 2000 evaluations of F and G
      IF(ICALL.GT.2000) GO TO 50
      GO TO 20
  50  CONTINUE
      END
C
C     ** LAST LINE OF SIMPLE DRIVER (SDRIVE) **

