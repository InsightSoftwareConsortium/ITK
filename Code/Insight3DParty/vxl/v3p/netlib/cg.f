C
C      ________________________________________________________
C     |                                                        |
C     |   MINIMIZE A FUNCTION USING THE FLETCHER-REEVES FORM   |
C     |            OF THE CONJUGATE GRADIENT METHOD            |
C     |            WITH (OR WITHOUT) PRECONDITIONING           |
C     |                                                        |
C     |    INPUT:                                              |
C     |                                                        |
C     |         X     --ARRAY CONTAINING STARTING GUESS        |
C     |                                                        |
C     |         STEP  --STARTING GUESS FOR MINIMIZER IN DIREC- |
C     |                 TION OF NEGATIVE GRADIENT DURING FIRST |
C     |                 ITERATION (E. G. STEP=1) WHEN STEP=0,  |
C     |                 THE PROGRAM SELECTS A STARTING GUESS   |
C     |                                                        |
C     |         T     --COMPUTING TOLERANCE (ITERATIONS STOP   |
C     |                 WHEN MAX-NORM OF GRADIENT .LE. T)      |
C     |                                                        |
C     |         LIMIT --MAXIMUM NUMBER OF ITERATIONS           |
C     |                                                        |
C     |         N     --NUMBER OF UNKNOWNS                     |
C     |                                                        |
C     |         M     --NUMBER OF ITERATIONS UNTIL THE SEARCH  |
C     |                 DIRECTIONS ARE RENORMALIZED ALONG THE  |
C     |                 NEGATIVE GRADIENT (TYPICALLY, M = N)   |
C     |                                                        |
C     |         VALUE --NAME OF COST EVALUATION FUNC. ROUTINE  |
C     |                 (EXTERNAL IN MAIN PROGRAM)             |
C     |                 VALUE(X) IS VALUE OF COST AT X         |
C     |                                                        |
C     |         GRAD  --NAME OF GRADIENT EVALUATION SUBROUTINE |
C     |                 (EXTERNAL IN MAIN PROGRAM)             |
C     |                 GRAD(G,X) PUTS IN G THE GRADIENT AT X  |
C     |                                                        |
C     |         BOTH  --NAME SUBROUTINE TO EVALUATE BOTH COST  |
C     |                 AND ITS GRADIENT (EXTERNAL IN MAIN     |
C     |                 PROGRAM) BOTH(V,G,X) PUTS THE VALUE IN |
C     |                 V AND THE GRADIENT IN G FOR THE POINT X|
C     |                                                        |
C     |         PRE   --NAME OF PRECONDITIONING SUBROUTINE     |
C     |                 (EXTERNAL IN MAIN PROGRAM)             |
C     |                 PRE(Y,Z) APPLIES THE PRECONDITIONER TO |
C     |                 Z, STORING THE RESULT IN Y.            |
C     |                 IF PRECONDITIONING NOT USED SET Y = Z  |
C     |                                                        |
C     |         H     --WORK ARRAY (LENGTH AT LEAST 3N)        |
C     |                                                        |
C     |    OUTPUT:                                             |
C     |                                                        |
C     |         X     --MINIMIZER                              |
C     |                                                        |
C     |         E     --MAX-NORM OF GRADIENT                   |
C     |                                                        |
C     |         IT    --NUMBER OF ITERATIONS PERFORMED         |
C     |                                                        |
C     |         STEP  --STEP SIZE ALONG SEARCH DIRECTION FOR   |
C     |                 FINAL ITERATION                        |
C     |                                                        |
C     |    BUILTIN FUNCTIONS: DABS,DEXP,IDINT,DLOG,DSQRT,DMAX1,|
C     |                         DMIN1,DSIGN                    |
C     |    PACKAGE ROUTINES: CUB,FD,FV,FVD,INS                 |
C     |________________________________________________________|
C
      SUBROUTINE CG(X,E,IT,STEP,T,LIMIT,N,M,VALUE,GRAD,BOTH,PRE,H)
      INTEGER I,IT,J,K,L,LIMIT,M,N,NA,NB,NC,ND
      REAL*8 H(N,1),X(1),Y(50),Z(50),A1,A2,A3,A4,A5,A6,A7,A8,A,B,C,C0,C1
      REAL*8 D,D0,DA,DB,E,F,F0,F1,FA,FB,FC,G,L3,P,Q,R,S,STEP,T,V,W
      REAL*8 FV,FD,VALUE
      EXTERNAL BOTH,GRAD,PRE,VALUE
      DATA A1/.1D0/,A2/.9D0/,A3/5.D0/,A4/.2D0/,A5/10.D0/,A6/.9D0/
      DATA A7/.3D0/
      A8 = A3 + .01D0
      IT = 0
      CALL BOTH(F,H(1,3),X)
      E = 0.
      DO 10 I = 1,N
10         IF ( DABS(H(I,3)) .GT. E ) E = DABS(H(I,3))
      IF ( E .LE. T ) RETURN
      L3 = 1./DLOG(A3)
      CALL PRE(H(1,2),H(1,3))
      A = STEP
      IF ( A .GT. 0. ) GOTO 30
      DO 20 I = 1,N
20         IF ( DABS(X(I)) .GT. A ) A = DABS(X(I))
      A = .01*A/E
      IF ( A .EQ. 0. ) A = 1.
30    G = 0.
      DO 40 I = 1,N
40         G = G + H(I,2)*H(I,3)
      IF ( G .LT. 0. ) GOTO 620
50    L = 0
      DO 60 I = 1,N
60         H(I,1) = -H(I,2)
      D = -G
70    FA = FV(A,X,H,N,VALUE)
      C0 = A
      F0 = FA
      J = 2
      Y(1) = 0.
      Z(1) = F
      Y(2) = A
      Z(2) = FA
      V = A1*D
      W = A2*D
      IQ = 0
      IF ( FA .LE. F ) GOTO 80
      C = A
      B = 0.
      A = 0.
      FC = FA
      FB = F
      FA = F
      GOTO 90
80    C = 0.
      B = 0.
      FC = F
      FB = F
      IQ = 1
90    NA = 0
      NB = 0
      NC = 0
      ND = 0
      Q = (D+(F-F0)/C0)/C0
      IF ( Q .LT. 0. ) GOTO 110
      Q = A
100   ND = ND + 1
      IF ( ND .GT. 25 ) GOTO 610
      Q = A3*Q
      P = FV(Q,X,H,N,VALUE)
      CALL INS(Q,P,A,B,C,FA,FB,FC,J,Y,Z)
      IF ( P-F .LT. W*Q ) GOTO 100
      GOTO 260
110   Q = .5*D/Q
      IF ( Q .LT. .01*C0 ) Q = .01*C0
      P = FV(Q,X,H,N,VALUE)
      IF ( P .LE. F0 ) GOTO 120
      F1 = F0
      C1 = C0
      F0 = P
      C0 = Q
      GOTO 130
120   F1 = P
      C1 = Q
130   CALL INS(Q,P,A,B,C,FA,FB,FC,J,Y,Z)
135   IF ( A .EQ. 0. ) GOTO 140
      IF ( FA-F .GE. V*A ) GOTO 160
      IF ( FA-F .LT. W*A ) GOTO 210
      GOTO 280
140   Q = C0
      IF ( C1 .LT. Q ) Q = C1
150   NA = NA + 1
      IF ( NA .GT. 25 ) GOTO 630
      Q = A4*Q
      P = FV(Q,X,H,N,VALUE)
      CALL INS(Q,P,A,B,C,FA,FB,FC,J,Y,Z)
      IF ( P-F .GE. V*Q ) GOTO 150
      GOTO 250
160   IF ( C0 .GT. C1 ) GOTO 200
      IF ( F0-F .GT. V*C0 ) GOTO 180
      IF ( F0-F .GE. W*C0 ) GOTO 320
      IF ( C1 .LE. A5*C0 ) GOTO 320
      R = DLOG(C1/C0)
      S = -IDINT(R*L3+.999)
      R = .999*DEXP(R/S)
      Q = C1
170   Q = Q*R
      IF ( Q .LT. C0 ) GOTO 320
      P = FV(Q,X,H,N,VALUE)
      CALL INS(Q,P,A,B,C,FA,FB,FC,J,Y,Z)
      NA = NA + 1
      IF ( P-F .GT. V*Q ) GOTO 170
      GOTO 320
180   Q = C0
190   NA = NA + 1
      IF ( NA .GT. 25 ) GOTO 630
      Q = A4*Q
      P = FV(Q,X,H,N,VALUE)
      CALL INS(Q,P,A,B,C,FA,FB,FC,J,Y,Z)
      IF ( P-F .GE. V*Q ) GOTO 190
      GOTO 250
200   Q = A
      GOTO 190
210   IF ( C0 .LT. C1 ) GOTO 290
      IF ( F0-F .GE. V*C0 ) GOTO 230
      IF ( F0-F .GE. W*C0 ) GOTO 250
      Q = C0
220   ND = ND  + 1
      IF ( ND .GT. 25 ) GOTO 610
      Q = A3*Q
      P = FV(Q,X,H,N,VALUE)
      CALL INS(Q,P,A,B,C,FA,FB,FC,J,Y,Z)
      IF ( P-F .LT. W*Q ) GOTO 220
      GOTO 250
230   IF ( C0 .LE. A5*C1 ) GOTO 250
      R = DLOG(C0/C1)
      S = IDINT(R*L3+.999)
      R = 1.001*DEXP(R/S)
      Q = A
240   Q = Q*R
      IF ( Q .GT. C0 ) GOTO 250
      ND = ND + 1
      P = FV(Q,X,H,N,VALUE)
      CALL INS(Q,P,A,B,C,FA,FB,FC,J,Y,Z)
      IF ( P-F .LT. W*Q ) GOTO 240
250   IF ( IQ .EQ. 1 ) GOTO 320
260   IF ( B .EQ. 0. ) GOTO 280
      IF ( C .EQ. 0. ) GOTO 270
      V = C - A
      W = A - B
      R = 1./V
      S = 1./W
      P = FC - FA
      Q = FB - FA
      E = P*R + Q*S
      IF ( DSIGN(E,C-B) .NE. E ) GOTO 320
      IF ( E .EQ. 0. ) GOTO 320
      Q = (P*R)*W - (Q*S)*V
      Q = A - .5*Q/E
      P = FV(Q,X,H,N,VALUE)
      CALL INS(Q,P,A,B,C,FA,FB,FC,J,Y,Z)
      GOTO 320
270   R = 1./A
      S = 1./B
      P = R*(FA-F) - D
      Q = S*(FB-F) - D
      E = A - B
      V = (R*P-S*Q)/E
      W = (A*Q*S-B*P*R)/E
      V = W*W-3.*V*D
      IF ( V .LT. 0. ) V = 0.
      V = DSQRT(V)
      IF ( W+V .EQ. 0. ) GOTO 320
      Q = -D/(W+V)
      IF ( Q .LE. 0. ) GOTO 320
      P = FV(Q,X,H,N,VALUE)
      CALL INS(Q,P,A,B,C,FA,FB,FC,J,Y,Z)
      GOTO 320
280   IF ( IQ .EQ. 1 ) GOTO  320
      Q = (D+(F-FA)/A)/A
      IF ( Q .GE. 0. ) GOTO 320
      Q = .5*D/Q
      P = FV(Q,X,H,N,VALUE)
      CALL INS(Q,P,A,B,C,FA,FB,FC,J,Y,Z)
      GOTO 320
290   IF ( F0-F .GT. V*C0 ) GOTO 300
      IF ( F0-F .GT. W*C0 ) GOTO 320
300   Q = A
310   ND = ND + 1
      IF ( ND .GT. 25 ) GOTO 610
      Q = A3*Q
      P = FV(Q,X,H,N,VALUE)
      CALL INS(Q,P,A,B,C,FA,FB,FC,J,Y,Z)
      IF ( P-F .LT. W*Q ) GOTO 310
      GOTO 250
320   DA = FD(A,X,H,N,GRAD)
      IF ( DA .GT. A6*G ) GOTO 410
      IF ( DA .GE. 0. ) GOTO 560
      R = A
      Q = 0.
      DO 330 I = 1,J
           IF ( Y(I) .GT. A ) GOTO 370
           IF ( Y(I) .LE. Q ) GOTO 330
           IF ( Y(I) .EQ. A ) GOTO 330
           Q = Y(I)
330   CONTINUE
      IF ( A .LE. A8*Q ) GOTO 560
      Q = A
340   ND = ND + 1
      IF ( ND .GT. 25 ) GOTO 610
      Q = A3*Q
      P = FV(Q,X,H,N,VALUE)
      F1 = FA
      CALL INS(Q,P,A,B,C,FA,FB,FC,J,Y,Z)
      IF ( P .LT. F1 ) GOTO 340
      IF ( A .GT. R ) GOTO 360
      DO 350 I = 1,N
350        H(I,2) = X(I) + A*H(I,1)
      GOTO 560
360   DA = FD(A,X,H,N,GRAD)
      IF ( DA .GT. A6*G ) GOTO 410
      GOTO 560
370   Q = Y(I)
      DO 380 K = I,J
           IF ( Y(K) .LE. A ) GOTO 380
           IF ( Y(K) .LT. Q ) Q = Y(K)
380   CONTINUE
      IF ( Q .LE. A5*A ) GOTO 560
      F0 = DLOG(Q/A)
      S = IDINT(F0*L3+.999)
      F0 = 1.001*DEXP(F0/S)
      S = A
390   S = S*F0
      IF ( S .GE. Q ) GOTO 320
      P = FV(S,X,H,N,VALUE)
      F1 = FA
      CALL INS(S,P,A,B,C,FA,FB,FC,J,Y,Z)
      IF ( P .LT. F1 ) GOTO 390
      IF ( A .GT. R ) GOTO 320
      DO 400 I = 1,N
400        H(I,2) = X(I) + A*H(I,1)
      GOTO 560
410   B = 0.
      K = 1
      I = K
420   I = I + 1
      IF ( I .GT. J ) GOTO 430
      IF ( Y(I) .GE. A ) GOTO 420
      IF ( Y(I) .LT. B ) GOTO 420
      B = Y(I)
      K = I
      GOTO 420
430   FB = Z(K)
      DB = D
      IF ( B .NE. 0. ) DB = FD(B,X,H,N,GRAD)
440   W = 2.*DABS(B-A)
      CALL CUB(C,A,B,FA,FB,DA,DB)
      NC = 1
      GOTO 480
450   W = .5*W
      IF ( W .LT. DABS(C0-C) ) GOTO 550
      IF ( C0 .LT. C ) GOTO 460
      IF ( D0 .GE. D ) GOTO 470
      GOTO 550
460   IF ( D0 .GT. D ) GOTO 550
470   CALL CUB(C,C,C0,F,F0,D,D0)
      NC = NC + 1
      IF ( NC .GT. 30 ) GOTO 600
480   R = DMAX1(A,B)
      S = DMIN1(A,B)
      IF ( C .GT. R ) GOTO 490
      IF ( C .GT. S ) GOTO 500
      C = S + (S-C)
      S = .5*(A+B)
      IF ( C .GT. S ) C = S
      GOTO 500
490   C = R - (C-R)
      S = .5*(A+B)
      IF ( C .LT. S ) C = S
500   C0 = A
      F0 = FA
      D0 = DA
      CALL FVD(F,D,C,X,H,N,BOTH)
      IF ( F .LT. FA ) GOTO 510
      B = C
      FB = F
      DB = D
      GOTO 450
510   IF ( C .LT. A ) GOTO 540
      IF ( D .LT. 0. ) GOTO 530
520   B = A
      FB = FA
      DB = DA
530   A = C
      FA = F
      DA = D
      IF ( D .GT. A6*G ) GOTO 450
      GOTO 560
540   IF ( D .LT. 0. ) GOTO 520
      GOTO 530
550   C = .5*(A+B)
      NB = NB + 1
      W = DABS(B-A)
      GOTO 500
560   E = 0.
      DO 570 I = 1,N
           IF ( DABS(H(I,3)) .GT. E ) E = DABS(H(I,3))
570        X(I) = H(I,2)
      IT = IT + 1
      IF ( E .LE. T ) GOTO 660
      IF ( IT .GE. LIMIT ) GOTO 660
      F = FA
      D = DA
      A = A7*A
      CALL PRE(H(1,2),H(1,3))
      R = 0.
      DO 580 I = 1,N
580        R = R + H(I,2)*H(I,3)
      IF ( R .LT. 0. ) GOTO 620
      S = R/G
      G = R
      L = L + 1
      IF ( L .GE. M ) GOTO 50
      D = 0.
      DO 590 I = 1,N
           H(I,1) = -H(I,2) + S*H(I,1)
590        D = D + H(I,1)*H(I,3)
      GOTO 70
600   IF ( D .LT. G ) GOTO 560
      WRITE(6,*) 'UNABLE TO OBTAIN DESCENT DIRECTION'
      STOP
610   WRITE(6,*) 'THE FUNCTION DECREASES WITH NO MINIMUM'
      STOP
620   WRITE(6,*) 'PRECONDITIONER NOT POSITIVE DEFINITE'
      STOP
630   Q = Q*A3**25
      ND = 0
640   ND = ND + 1
      IF ( ND .GT. 25 ) GOTO 650
      Q = A3*Q
      P = FV(Q,X,H,N,VALUE)
      CALL INS(Q,P,A,B,C,FA,FB,FC,J,Y,Z)
      IF ( P-F .GT. V*Q ) GOTO 640
      GOTO 135
650   WRITE(6,*) 'UNABLE TO SATISFY ARMIJO CONDITION'
      RETURN
660   STEP = A
      RETURN
      END
      DOUBLE PRECISION FUNCTION FV(A,X,H,N,VALUE)
      REAL*8 H(N,1),X(1),A,VALUE
      EXTERNAL VALUE
      DO 10 I = 1 , N
10         H(I,2) = X(I) + A*H(I,1)
      FV = VALUE(H(1,2))
      RETURN
      END
      DOUBLE PRECISION FUNCTION FD(A,X,H,N,GRAD)
      REAL*8 H(N,1),X(1),A,D
      EXTERNAL GRAD
      DO 10 I = 1 , N
10         H(I,2) = X(I) + A*H(I,1)
      CALL GRAD(H(1,3),H(1,2))
      D = 0.
      DO 20 I = 1,N
20         D = D + H(I,1)*H(I,3)
      FD = D
      RETURN
      END
      SUBROUTINE FVD(V,D,A,X,H,N,BOTH)
      REAL*8 H(N,1),X(1),A,D,V
      EXTERNAL BOTH
      DO 10 I = 1 , N
10         H(I,2) = X(I) + A*H(I,1)
      CALL BOTH(V,H(1,3),H(1,2))
      D = 0.
      DO 20 I = 1,N
20         D = D + H(I,1)*H(I,3)
      RETURN
      END
      SUBROUTINE CUB(X,A,B,C,D,E,F)
      REAL*8 A,B,C,D,E,F,G,V,W,X,Y,Z
      G = B - A
      IF ( G .EQ. 0. ) GOTO 50
      V = E + F - 3*(D-C)/G
      W = V*V-E*F
      IF ( W .LT. 0. ) W = 0.
      W = DSIGN(DSQRT(W),G)
      Y = E + V
      Z = F + V
      IF ( DSIGN(Y,G) .NE. Y ) GOTO 30
      IF ( DSIGN(Z,G) .NE. Z ) GOTO 20
      IF ( Z .EQ. 0. ) GOTO 20
10    X = B - G*F/(Z+W)
      RETURN
20    IF ( C .LT. D ) X = A
      IF ( C .GE. D ) X = B
      RETURN
30    IF ( DSIGN(Z,G) .NE. Z ) GOTO 40
      IF ( DABS(E) .GT. DABS(F) ) GOTO 10
40    X = A + G*E/(Y-W)
      RETURN
50    X = A
      RETURN
      END
      SUBROUTINE INS(S,F,A,B,C,FA,FB,FC,J,Y,Z)
      REAL*8 A,B,C,F,FA,FB,FC,S,Y(1),Z(1)
      INTEGER J
      J = J + 1
      Y(J) = S
      Z(J) = F
      IF ( F .LE. FA ) GOTO 20
      IF ( F .LE. FB ) GOTO 10
      IF ( F .GT. FC ) RETURN
      C = S
      FC = F
      RETURN
10    C = B
      B = S
      FC = FB
      FB = F
      RETURN
20    C = B
      B = A
      A = S
      FC = FB
      FB = FA
      FA = F
      RETURN
      END

