      PROGRAM TRAPEZOD
C     NUMERICAL METHODS: FORTRAN Programs, (c) John H. Mathews 1994
C     To accompany the text:
C     NUMERICAL METHODS for Mathematics, Science and Engineering, 2nd Ed, 1992
C     Prentice Hall, Englewood Cliffs, New Jersey, 07632, U.S.A.
C     This free software is complements of the author.
C
C     Algorithm 7.1 (Composite Trapezoidal Rule).
C     Section 7.2, Composite Trapezoidal and Simpson's Rule, Page 365
C
      INTEGER M
      REAL A,B,Trule
      CHARACTER*60 ANS*1,DFUN,FUN
      EXTERNAL F
10    CALL INPUTS(A,B,M,DFUN,FUN)
      CALL TRAPRU(F,A,B,M,Trule)
      CALL RESULT(A,B,M,Trule,DFUN,FUN)
      WRITE(9,*)'WANT TO TRY ANOTHER INTERVAL ? <Y/N> '
      READ (9,'(A)') ANS
      IF (ANS.EQ.'Y' .OR. ANS.EQ.'y') GOTO 10
      END

      REAL FUNCTION F(X)
      REAL X
      F=X/(1+X*X)
      RETURN
      END

      SUBROUTINE PRINTFUN(DFUN,FUN)
      CHARACTER*(*) DFUN,FUN
      FUN ='X/(1+X*X)'
      DFUN='X/(1+X*X) DX'
      RETURN
      END

      SUBROUTINE TRAPRU(F,A,B,M,Trule)
      INTEGER K,M
      REAL A,B,H,Sum,Trule,X
      EXTERNAL F
      H=(B-A)/M
      Sum=0
      DO K=1,M-1
        X=A+H*K
        Sum=Sum+F(X)
      ENDDO
      Sum=H*(F(A)+F(B)+2*Sum)/2
      Trule=Sum
      RETURN
      END

      SUBROUTINE XTRAPRU(F,A,B,M,Trule)
C     This subroutine uses labeled DO loop(s).
      INTEGER K,M
      REAL A,B,H,Sum,Trule,X
      EXTERNAL F
      H=(B-A)/M
      Sum=0
      DO 10 K=1,M-1
        X=A+H*K
        Sum=Sum+F(X)
10    CONTINUE
      Sum=H*(F(A)+F(B)+2*Sum)/2
      Trule=Sum
      RETURN
      END

      SUBROUTINE INPUTS(A,B,M,DFUN,FUN)
      INTEGER I,M
      REAL A,B
      CHARACTER*(*) DFUN,FUN
      CALL PRINTFUN(DFUN,FUN)
      DO 10 I=1,18
        WRITE(9,*)' '
10    CONTINUE
      WRITE(9,*)'    THE TRAPEZOIDAL RULE IS USED TO COMPUTE AN APPROXIM
     +ATION'
      WRITE(9,*)' '
      WRITE(9,*)'FOR THE VALUE OF THE DEFINITE INTEGRAL:'
      WRITE(9,*)' '
      WRITE(9,*)' '
      WRITE(9,*)'    B'
      WRITE(9,*)'    /'
      WRITE(9,*)'    | ',DFUN
      WRITE(9,*)'    /'
      WRITE(9,*)'    A'
      WRITE(9,*)' '
      WRITE(9,*)' '
      WRITE(9,*)'ENTER THE LEFT  ENDPOINT A = '
      READ(9,*) A
      WRITE(9,*)' '
      WRITE(9,*)'ENTER THE RIGHT ENDPOINT B = '
      READ(9,*) B
      WRITE(9,*)' '
      WRITE(9,*)'  NUMBER OF SUBINTERVALS M = '
      READ(9,*) M
      WRITE(9,*)' '
      RETURN
      END

      SUBROUTINE RESULT(A,B,M,Trule,DFUN,FUN)
      INTEGER I,M
      REAL A,B,Trule
      CHARACTER*(*) DFUN,FUN
      CALL PRINTFUN(DFUN,FUN)
      DO 10 I=1,18
        WRITE(9,*)' '
10    CONTINUE
      WRITE(9,*)'            ',B
      WRITE(9,*)'               /'
      WRITE(9,*)'               |'
      WRITE(9,*)Trule,'  ~    |  ',DFUN
      WRITE(9,*)'               |'
      WRITE(9,*)'               /'
      WRITE(9,*)'           ',A
      WRITE(9,*)' '
      WRITE(9,*)' '
      WRITE(9,*)'AN APPROXIMATE VALUE FOR THE DEFINITE INTEGRAL OF'
      WRITE(9,*)' '
      WRITE(9,*)' '
      WRITE(9,*)'F(X) = ',FUN
      WRITE(9,*)' '
      WRITE(9,*)' '
      WRITE(9,*)'TAKEN OVER  [',A,'  ,',B,'  ]  WAS FOUND.'
      WRITE(9,*)' '
      WRITE(9,*)'WHEN ',M,' SUBINTERVALS ARE USED,'
      WRITE(9,*)' '
      WRITE(9,*)'THE TRAPEZOIDAL RULE APPROXIMATION IS ',Trule
      WRITE(9,*)' '
      RETURN
      END














      PROGRAM SIMPSON
C     NUMERICAL METHODS: FORTRAN Programs, (c) John H. Mathews 1994
C     To accompany the text:
C     NUMERICAL METHODS for Mathematics, Science and Engineering, 2nd Ed, 1992
C     Prentice Hall, Englewood Cliffs, New Jersey, 07632, U.S.A.
C     This free software is complements of the author.
C
C     Algorithm 7.2 (Composite Simpson Rule). 
C     Section 7.2, Composite Trapezoidal and Simpson's Rule, Page 365
C
      INTEGER M
      REAL A,B,Srule
      CHARACTER*60 ANS*1,DFUN,FUN
      EXTERNAL F
10    CALL INPUTS(A,B,M,DFUN,FUN)
      CALL SIMPRU(F,A,B,M,Srule)
      CALL RESULT(A,B,M,Srule,DFUN,FUN)
      WRITE(9,*)'WANT TO TRY ANOTHER INTERVAL ? <Y/N> '
      READ (9,'(A)') ANS
      IF (ANS.EQ.'Y' .OR. ANS.EQ.'y') GOTO 10
      END

      REAL FUNCTION F(X)
      REAL X
      F=X/(1+X*X)
      RETURN
      END

      SUBROUTINE PRINTFUN(DFUN,FUN)
      CHARACTER*(*) DFUN,FUN
      FUN ='X/(1+X*X)'
      DFUN='X/(1+X*X) DX'
      RETURN
      END

      SUBROUTINE SIMPRU(F,A,B,M,Srule)
      INTEGER K,M
      REAL A,B,H,Sum,SumEven,SumOdd,Srule,X
      EXTERNAL F
      H=(B-A)/(2*M)
      SumEven=0
      DO K=1,(M-1)
        X=A+H*2*K
        SumEven=SumEven+F(X)
      ENDDO
      SumOdd=0
      DO K=1,M
        X=A+H*(2*K-1)
        SumOdd=SumOdd+F(X)
      ENDDO
      Sum=H*(F(A)+F(B)+2*SumEven+4*SumOdd)/3
      Srule=Sum
      RETURN
      END

      SUBROUTINE XSIMPRU(F,A,B,M,Srule)
C     This subroutine uses labeled DO loop(s).
      INTEGER K,M
      REAL A,B,H,Sum,SumEven,SumOdd,Srule,X
      EXTERNAL F
      H=(B-A)/(2*M)
      SumEven=0
      DO 10 K=1,(M-1)
        X=A+H*2*K
        SumEven=SumEven+F(X)
10    CONTINUE
      SumOdd=0
      DO 20 K=1,M
        X=A+H*(2*K-1)
        SumOdd=SumOdd+F(X)
20    CONTINUE
      Sum=H*(F(A)+F(B)+2*SumEven+4*SumOdd)/3
      Srule=Sum
      RETURN
      END













      PROGRAM ROMBERG
C     NUMERICAL METHODS: FORTRAN Programs, (c) John H. Mathews 1994
C     To accompany the text:
C     NUMERICAL METHODS for Mathematics, Science and Engineering, 2nd Ed, 1992
C     Prentice Hall, Englewood Cliffs, New Jersey, 07632, U.S.A.
C     This free software is complements of the author.
C
C     Algorithm 7.3 (Recursive Trapezoidal Rule).
C     Algorithm 7.4 (Romberg Integration).
C     Section 7.3, Recursive Rules and Romberg Integration, Page 379
C
      PARAMETER(MaxN=12,Tol=1E-6)
      INTEGER J
      REAL A,B,Close,R,Rrule
      CHARACTER*56 ANS*1,DFUN,FUN
      DIMENSION R(0:MaxN,0:MaxN)
      EXTERNAL F
10    CALL INPUTS(A,B,DFUN,FUN)
      CALL ROMBRU(F,A,B,Tol,R,Rrule,Close,J)
      CALL RESULT(A,B,R,Rrule,Close,J,DFUN,FUN)
      WRITE(9,*)'WANT TO TRY ANOTHER INTERVAL ? <Y/N> '
      READ (9,'(A)') ANS
      IF (ANS.EQ.'Y' .OR. ANS.EQ.'y') GOTO 10
      END

      REAL FUNCTION F(X)
      REAL X
      F=SIN(X)
      RETURN
      END

      SUBROUTINE PRINTFUN(DFUN,FUN)
      CHARACTER*(*) DFUN,FUN
      FUN ='SIN(X)'
      DFUN='SIN(X) DX'
      RETURN
      END

      SUBROUTINE ROMBRU(F,A,B,Tol,R,Rrule,Close,J)
      PARAMETER(MaxN=12,Min=4)
      INTEGER J,K,M,P
      REAL A,B,Close,H,R,Rrule,Sum,Tol,X
      DIMENSION R(0:MaxN,0:MaxN)
      EXTERNAL F
      M=1
      H=B-A
      Close=1
      J=0
      R(0,0)=H*(F(A)+F(B))/2
      WHILE ((Close.GT.Tol .AND. J.LT.MaxN) .OR. J.LT.Min)
        J=J+1               !This is the Trapezoidal Rule:
        H=H/2
        Sum=0
        DO P=1,M
          X=A+H*(2*P-1)
          Sum=Sum+F(X)
        ENDDO
        R(J,0)=H*Sum+R(J-1,0)/2
        M=2*M             !This is the Extrapolation part:
        DO K=1,J
          R(J,K)=R(J,K-1)+(R(J,K-1)-R(J-1,K-1))/(4**K-1)
        ENDDO
        Close = ABS(R(J-1,J-1)-R(J,J))
      REPEAT
      Rrule=R(J,J)
      RETURN
      END

      SUBROUTINE XROMBRU(F,A,B,Tol,R,Rrule,Close,J)
C     This subroutine uses simulated WHILE loop(s).
      PARAMETER(MaxN=12,Min=4)
      INTEGER J,K,M,P
      REAL A,B,Close,H,R,Rrule,Sum,Tol,X
      DIMENSION R(0:MaxN,0:MaxN)
      EXTERNAL F
      M=1
      H=B-A
      Close=1
      J=0
      R(0,0)=H*(F(A)+F(B))/2
10    IF ((Close.GT.Tol .AND. J.LT.MaxN) .OR. J.LT.Min) THEN
        J=J+1                 !This is the Trapezoidal Rule:
        H=H/2
        Sum=0
        DO 20 P=1,M
          X=A+H*(2*P-1)
          Sum=Sum+F(X)
20      CONTINUE
        R(J,0)=H*Sum+R(J-1,0)/2
        M=2*M                !This is the Extrapolation part:
        DO 30 K=1,J
          R(J,K)=R(J,K-1)+(R(J,K-1)-R(J-1,K-1))/(4**K-1)
30      CONTINUE
        Close = ABS(R(J-1,J-1)-R(J,J))
        GOTO 10
      ENDIF
      Rrule=R(J,J)
      RETURN
      END

      SUBROUTINE INPUTS(A,B,DFUN,FUN)
      INTEGER I
      REAL A,B
      CHARACTER*(*) DFUN,FUN
      CALL PRINTFUN(DFUN,FUN)
      DO 10 I=1,18
        WRITE(9,*)' '
10    CONTINUE
      WRITE(9,*)'    ROMBERG INTEGRATION IS PERFORMED TO FIND AN APPROXI
     +MATION'
      WRITE(9,*)' '
      WRITE(9,*)'FOR THE VALUE OF THE DEFINITE INTEGRAL:'
      WRITE(9,*)' '
      WRITE(9,*)' '
      WRITE(9,*)'    B'
      WRITE(9,*)'    /'
      WRITE(9,*)'    | ',DFUN
      WRITE(9,*)'    /'
      WRITE(9,*)'    A'
      WRITE(9,*)' '
      WRITE(9,*)' '
      WRITE(9,*)'ENTER THE LEFT  ENDPOINT A = '
      READ(9,*) A
      WRITE(9,*)' '
      WRITE(9,*)'ENTER THE RIGHT ENDPOINT B = '
      READ(9,*) B
      WRITE(9,*)' '
      RETURN
      END

      SUBROUTINE RESULT(A,B,R,Rrule,Close,J,DFUN,FUN)
      PARAMETER(MaxN=12,Tol=1E-9)
      INTEGER I,J,K
      REAL A,B,Close,R,Rrule
      CHARACTER*(*) DFUN,FUN
      DIMENSION R(0:MaxN,0:MaxN)
      CALL PRINTFUN(DFUN,FUN)
      DO 10 I=1,18
        WRITE(9,*)' '
10    CONTINUE
      WRITE(9,*)'           ',B
      WRITE(9,*)'              /'
      WRITE(9,*)'              |'
      WRITE(9,*)Rrule,'  ~   |  ',DFUN
      WRITE(9,*)'              |'
      WRITE(9,*)'              /'
      WRITE(9,*)'           ',A
      WRITE(9,*)' '
      WRITE(9,*)'THE DIAGONAL ELEMENTS IN THE ROMBERG TABLE ARE:'
      IF (J.LE.5) THEN
        WRITE(9,*)' '
      ENDIF
      DO 20 K=0,J,2
        IF (K.LT.J) THEN
          WRITE(9,998) K,K,R(K,K),K+1,K+1,R(K+1,K+1)
        ELSE
          WRITE(9,999) K,K,R(K,K)
        ENDIF
20    CONTINUE
      IF (J.LE.7) THEN
        WRITE(9,*)' '
      ENDIF
      WRITE(9,*)'AN APPROXIMATE VALUE FOR THE DEFINITE INTEGRAL OF'
      WRITE(9,*)' '
      WRITE(9,*)'F(X) = ',FUN
      WRITE(9,*)' '
      WRITE(9,*)'TAKEN OVER  [',A,'  ,',B,'  ]  WAS FOUND.'
      WRITE(9,*)' '
      WRITE(9,*)'THE ROMBERG APPROXIMATION IS ',Rrule
      IF (J.LE.9) THEN
        WRITE(9,*)' '
      ENDIF
      WRITE(9,*)'    THE  ERROR  ESTIMATE  IS ',Close
      IF (J.LE.11) THEN
        WRITE(9,*)' '
      ENDIF
998   FORMAT(' R(',I2,',',I2,') = ',F15.7,
     +'        R(',I2,',',I2,') = ',F15.7)
999   FORMAT(' R(',I2,',',I2,') = ',F15.7)
      RETURN
      END














      PROGRAM ADAQUAD
C     NUMERICAL METHODS: FORTRAN Programs, (c) John H. Mathews 1994
C     To accompany the text:
C     NUMERICAL METHODS for Mathematics, Science and Engineering, 2nd Ed, 1992
C     Prentice Hall, Englewood Cliffs, New Jersey, 07632, U.S.A.
C     This free software is complements of the author.
C
C     Algorithm 7.5 (Adaptive Quadrature Using Simpson's Rule).
C     Section 7.4, Adaptive Quadrature, Page 389
C
      PARAMETER(MaxN=12,Tol=1E-6)
      INTEGER P,M,State
      REAL A,B,Tol0,SRmat,SRvec,Integral,ErrBdd
      REAL Close,Rrule
      INTEGER J
      CHARACTER*56 ANS*1,DFUN,FUN
      DIMENSION SRvec(1:11),SRmat(1:101,1:11)
      EXTERNAL F
10    CALL INPUTS(A,B,DFUN,FUN)
      CALL AdaptQuad(F,A,B,Tol,SRmat,Integral,ErrBdd,M,State)
      CALL RESULT(A,B,SRmat,Integral,ErrBdd,M,DFUN,FUN)
      WRITE(9,*)'WANT TO TRY ANOTHER INTERVAL ? <Y/N> '
      READ (9,'(A)') ANS
      IF (ANS.NE.'Y' .AND. ANS.NE.'y') STOP
      IF (ANS.EQ.'Y' .OR. ANS.EQ.'y') GOTO 10
      STOP
      END

      REAL FUNCTION F(X)
      REAL X
      F=SIN(X)
      RETURN
      END

      SUBROUTINE PRINTFUN(DFUN,FUN)
      CHARACTER*(*) DFUN,FUN
      FUN ='SIN(X)'
      DFUN='SIN(X) DX'
      RETURN
      END

      SUBROUTINE AdaptQuad(F,A,B,Tol,SRmat,Integral,ErrBdd,M,State)
      INTEGER M,State
      REAL A,B,Tol,SRmat,Integral,ErrBdd
      INTEGER J,K,N,Iterating,Done
      REAL Sum1,Sum2,SRvec
      DIMENSION SRmat(1:101,1:11),SRvec(1:11)
      EXTERNAL F
      Iterating = 0
      Done = 1
      CALL Srule(F,A,B,Tol,SRvec)
      DO K=1,11
        SRmat(1, K) = SRvec(K)
      ENDDO
      M = 1
      State = Iterating
      WHILE (State .EQ. Iterating)
        N = M
        DO J=N,1,-1
          CALL Refine(J,SRmat,M,State)
        ENDDO
      REPEAT
      Sum1 = 0
      Sum2 = 0
      DO J=1,M
        Sum1 = Sum1 + SRmat(J, 8)
        Sum2 = Sum2 + Abs(SRmat(J, 9))
      ENDDO
      Integral = Sum1
      ErrBdd = Sum2
      RETURN
      END

      SUBROUTINE Refine(P,SRmat,M,State)
      INTEGER P,M,State
      REAL SRmat
      INTEGER J,K,Iterating,Done
      REAL A,B,C,Err,Fa,Fb,Fc,S,S2,Tol,Tol2,Err,Check
      REAL SR0vec,SR1vec,SR2vec
      DIMENSION SRmat(1:101,1:11)
      DIMENSION SR0vec(1:11),SR1vec(1:11),SR2vec(1:11)
      EXTERNAL F
      Iterating = 0
      Done = 1
      State = Done
      DO K=1,11
        SR0vec(K) = SRmat(P, K)
      ENDDO
      A = SR0vec(1)
      C = SR0vec(2)
      B = SR0vec(3)
      Fa = SR0vec(4)
      Fc = SR0vec(5)
      Fb = SR0vec(6)
      S = SR0vec(7)
      S2 = SR0vec(8)
      Err = SR0vec(9)
      Tol = SR0vec(10)
      Check = SR0vec(11)
      IF (Check .EQ. 1) RETURN
      Tol2 = Tol / 2
      CALL Srule(F, A, C, Tol2, SR1vec)
      CALL Srule(F, C, B, Tol2, SR2vec)
      Err = ABS(SR0vec(7) - SR1vec(7) - SR2vec(7)) / 10
      IF (Err .LT. Tol) THEN
        SR0vec(11) = 1
      ENDIF
      IF (Err .LT. Tol) THEN
        DO K=1,11
          SRmat(P, K) = SR0vec(K)
        ENDDO
        SRmat(P, 8) = SR1vec(7) + SR2vec(7)
        SRmat(P, 9) = Err
      ELSE
        DO J=(M + 1),P,-1
          DO K=1,11
            SRmat(J, K) = SRmat(J - 1, K)
          ENDDO
        ENDDO
        M = M + 1
        DO K=1,11
          SRmat(P, K) = SR1vec(K)
        ENDDO
        DO K=1,11
          SRmat(P + 1, K) = SR2vec(K)
        ENDDO
        State = Iterating
      ENDIF
      RETURN
      END

      SUBROUTINE Srule(F,A,B,Tol0,SRvec)
      REAL A,B,Tol0,SRvec
      REAL C,H,Fa,Fb,Fc,S,S2,Tol1,Err,Check
      DIMENSION SRvec(1:11)
      EXTERNAL F
        H = (B - A) / 2
        C = (A + B) / 2
        Fa = F(A)
        Fc = F(C)
        Fb = F(B)
        S = H * (F(A) + 4 * F(C) + F(B)) / 3
        S2 = S
        Tol1 = Tol0
        Err = Tol0
        Check = 0
        SRvec(1) = A
        SRvec(2) = C
        SRvec(3) = B
        SRvec(4) = Fa
        SRvec(5) = Fc
        SRvec(6) = Fb
        SRvec(7) = S
        SRvec(8) = S2
        SRvec(9) = Err
        SRvec(10) = Tol1
        SRvec(11) = Check
      RETURN
      END

      SUBROUTINE INPUTS(A,B,DFUN,FUN)
      INTEGER I
      REAL A,B
      CHARACTER*(*) DFUN,FUN
      CALL PRINTFUN(DFUN,FUN)
      DO 10 I=1,18
        WRITE(9,*)' '
10    CONTINUE
      WRITE(9,*)'    ROMBERG INTEGRATION IS PERFORMED TO FIND AN APPROXI
     +MATION'
      WRITE(9,*)' '
      WRITE(9,*)'FOR THE VALUE OF THE DEFINITE INTEGRAL:'
      WRITE(9,*)' '
      WRITE(9,*)' '
      WRITE(9,*)'    B'
      WRITE(9,*)'    /'
      WRITE(9,*)'    | ',DFUN
      WRITE(9,*)'    /'
      WRITE(9,*)'    A'
      WRITE(9,*)' '
      WRITE(9,*)' '
      WRITE(9,*)'ENTER THE LEFT  ENDPOINT A = '
      READ(9,*) A
      WRITE(9,*)' '
      WRITE(9,*)'ENTER THE RIGHT ENDPOINT B = '
      READ(9,*) B
      WRITE(9,*)' '
      RETURN
      END

      SUBROUTINE RESULT(A,B,SRmat,Integral,ErrBdd,M,DFUN,FUN)
      INTEGER I,J,K,M
      REAL A,B,SRmat,Integral,ErrBdd
      CHARACTER*(*) DFUN,FUN
      DIMENSION SRmat(1:101,1:11)
      CALL PRINTFUN(DFUN,FUN)
      DO 10 I=1,18
        WRITE(9,*)' '
10    CONTINUE
      WRITE(9,*)'           ',B
      WRITE(9,*)'              /'
      WRITE(9,*)'              |'
      WRITE(9,*)Integral,'  ~   |  ',DFUN
      WRITE(9,*)'              |'
      WRITE(9,*)'              /'
      WRITE(9,*)'           ',A
      WRITE(9,*)' '
      WRITE(9,*)' '
      WRITE(9,*)'AN APPROXIMATE VALUE FOR THE DEFINITE INTEGRAL OF'
      WRITE(9,*)' '
      WRITE(9,*)'F(X) = ',FUN
      WRITE(9,*)' '
      WRITE(9,*)'TAKEN OVER  [',A,'  ,',B,'  ]  WAS FOUND.'
      WRITE(9,*)' '
      WRITE(9,*)'THE ADAVTIVE QUADRATURE APPROXIMATION IS ',Integral
      WRITE(9,*)' '
      WRITE(9,*)'                THE  ERROR  ESTIMATE  IS ',ErrBdd
      WRITE(9,*)' '
      WRITE(9,*)'     THE NUMBER OF SUBINTERVALS USED WAS ',M
      WRITE(9,*)' '
      RETURN
      END














      PROGRAM GAUSSINT
C     NUMERICAL METHODS: FORTRAN Programs, (c) John H. Mathews 1994
C     To accompany the text:
C     NUMERICAL METHODS for Mathematics, Science and Engineering, 2nd Ed, 1992
C     Prentice Hall, Englewood Cliffs, New Jersey, 07632, U.S.A.
C     This free software is complements of the author.
C
C     Algorithm 7.6 (Gauss-Legendre Quadrature).
C     Section 7.5, Gauss-Legendre Integration, Page 397
C
      PARAMETER(Tol=1E-6)
      INTEGER I,J
      REAL A,A0,B0,Close,Q,W
      CHARACTER*56 ANS*1,DFUN,FUN
      DIMENSION A(1:17,1:96),Q(1:17),W(1:17,1:96)
      EXTERNAL F
      DO 10 I=1,18
        WRITE(9,*)' '
10    CONTINUE
      WRITE(9,*)'Now loading abscissas and weights. Please wait!'
      CALL READAW(A,W)
20    CALL INPUTS(A0,B0,DFUN,FUN)
      CALL GAUSQUAD(F,A0,B0,A,W,Q,Close,J)
      CALL RESULT(A0,B0,Close,Q,J,FUN,DFUN)
      WRITE(9,*)'Wand to try another interval ? <Y/N> '
      READ (9,'(A)') ANS
      IF (ANS.EQ.'Y'.OR.ANS.EQ.'y') GOTO 20
      END

      REAL FUNCTION F(X)
      REAL X
      F=SIN(X)
      RETURN
      END

      SUBROUTINE PRINTFUN(DFUN,FUN)
      CHARACTER*(*) DFUN,FUN
      FUN ='SIN(X)'
      DFUN='SIN(X) DX'
      RETURN
      END

      SUBROUTINE GAUSQUAD(F,A0,B0,A,W,Q,Close,J)
      PARAMETER(Min=3)
      INTEGER I,J,K
      REAL A,A0,B0,Close,Mid,Sum,Q,W,Wide,X
      DIMENSION A(1:17,1:96),Q(1:17),W(1:17,1:96)
      EXTERNAL F
      Mid=(A0+B0)/2
      Wide=(B0-A0)/2
      Close=1
      J=1
      X= Mid+A(1,1)
      Q(1)= W(1,1)*F(X)*Wide
      WHILE ((Close.GT.Tol.OR.J.LT.Min).AND.J.LT.17)
        J=J+1
        Sum=0
        I=J
        IF (J.GT.10) I=12+4*(J-11)
        IF (J.GT.14) I=24+8*(J-14)
        DO K=1,I
            X=Mid+A(J,K)*Wide
            Sum=Sum+W(J,K)*F(X)
        ENDDO
        Q(J)=Sum*Wide
        Close=ABS(Q(J)-Q(J-1))
      REPEAT
      RETURN
      END

      SUBROUTINE XGAUSQUAD(F,A0,B0,A,W,Q,Close,J)
C     This subroutine uses simulated WHILE loop(s).
      PARAMETER(Min=3)
      INTEGER I,J,K
      REAL A,A0,B0,Close,Mid,Sum,Q,W,Wide,X
      DIMENSION A(1:17,1:96),Q(1:17),W(1:17,1:96)
      EXTERNAL F
      Mid=(A0+B0)/2
      Wide=(B0-A0)/2
      Close=1
      J=1
      X= Mid+A(1,1)
      Q(1)= W(1,1)*F(X)*Wide
10    IF ((Close.GT.Tol.OR.J.LT.Min).AND.J.LT.17) THEN
        J=J+1
        Sum=0
        I=J
        IF (J.GT.10) I=12+4*(J-11)
        IF (J.GT.14) I=24+8*(J-14)
        DO 20 K=1,I
            X=Mid+A(J,K)*Wide
            Sum=Sum+W(J,K)*F(X)
20      CONTINUE
        Q(J)=Sum*Wide
        Close=ABS(Q(J)-Q(J-1))
        GOTO 10
      ENDIF
      RETURN
      END

      SUBROUTINE READAW(A,W)
      INTEGER I,J,K
      REAL A,M,W
      CHARACTER*56 ANS*1,DFUN,FUN
      DIMENSION A(1:17,1:96),Q(1:17),W(1:17,1:96)
      OPEN(4,FILE='ALG7-6AW.DAT',STATUS='OLD')
      DO 50 J=1,17
        M=J
        I=J
        IF (J.GT.10) THEN
          M=12+4*(J-11)
          I=INT(M)
        ENDIF
        IF (J.GT.14) THEN
          M=24+8*(J-14)
          I=INT(M)
        ENDIF
      DO 10 K=1,INT((M+1)/2)
        READ(4,*) A(J,K)
10    CONTINUE
      DO 20 K=1,INT(M/2)
        A(J,I+1-K)=-A(J,K)
20    CONTINUE
      DO 30 K=1,INT((M+1)/2)
        READ(4,*) W(J,K)
30    CONTINUE
      DO 40 K=1,INT(M/2)
          W(J,I+1-K)=W(J,K)
40    CONTINUE
50    CONTINUE
      CLOSE(4,STATUS='KEEP')
      RETURN
      END

      SUBROUTINE INPUTS(A0,B0,DFUN,FUN)
      INTEGER I
      REAL A0,B0
      CHARACTER*(*) DFUN,FUN
      CALL PRINTFUN(DFUN,FUN)
      DO 10 I=1,18
        WRITE(9,*)' '
10    CONTINUE
      WRITE(9,*)' '
      WRITE(9,*)'          Gaussian quadrature is performed to find an'
      WRITE(9,*)' '
      WRITE(9,*)'     approximation for the value of the definite integr
     +al:'
      WRITE(9,*)' '
      WRITE(9,*)'           B              N          '
      WRITE(9,*)'           /                         '
      WRITE(9,*)'           | F(X) DX  ~  SUM w F(x ) '
      WRITE(9,*)'           /                  k   k  '
      WRITE(9,*)'           A             k=1         '
      WRITE(9,*)' '
      WRITE(9,*)'    You chose to approximate the definite integral:'
      WRITE(9,*)' '
      WRITE(9,*)' '
      WRITE(9,*)'           B'
      WRITE(9,*)'           /'
      WRITE(9,*)'           | ',DFUN
      WRITE(9,*)'           /'
      WRITE(9,*)'           A'
      WRITE(9,*)' '
      WRITE(9,*)' '
      WRITE(9,*)'ENTER the left  endpoint A = '
      READ(9,*) A0
      WRITE(9,*)' '
      WRITE(9,*)'ENTER the right endpoint B = '
      READ(9,*) B0
      WRITE(9,*)' '
      RETURN
      END

      SUBROUTINE RESULT(A0,B0,Close,Q,J,FUN,DFUN)
      INTEGER I,II,J,JJ,K,L,U,V
      REAL A0,B0,Close,Q
      CHARACTER*(*) DFUN,FUN
      DIMENSION Q(1:17)
      CALL PRINTFUN(DFUN,FUN)
      DO 10 I=1,18
        WRITE(9,*)' '
10    CONTINUE
      JJ=J
      IF (J.GT.10) THEN
        JJ=12+4*(J-11)
      ENDIF
      IF (J.GT.14) THEN
        JJ=24+8*(J-14)
      ENDIF
      WRITE(9,*)'           ',B0
      WRITE(9,*)'                   /'
      WRITE(9,*)'                   |'
      WRITE(9,*)Q(J),' ~         |  ',DFUN
      WRITE(9,*)'                   |'
      WRITE(9,*)'                   /'
      WRITE(9,*)'           ',A0
      WRITE(9,*)' '
      WRITE(9,*)' '
      WRITE(9,*)'Gaussian quadrature with',JJ,' points was used to inte
     +grate'
      WRITE(9,*)' '
      WRITE(9,*)'F(X) = ',FUN
      WRITE(9,*)' '
      WRITE(9,*)' '
      WRITE(9,*)'over the interval   [',A0,'  ,',B0,'  ].'
      WRITE(9,*)' '
      WRITE(9,*)'The values of the previous Gaussian rules are:'
      L=1
      IF (J.GT.14) THEN
        L=2
      ENDIF
      IF (J.GT.16) THEN
        L=3
      ENDIF
      DO 20 K=L,INT((J+1)/2)
      U=2*K-1
      V=2*K
      I=U
      IF (U.GT.10) THEN
        I=12+4*(U-11)
      ENDIF
      IF (U.GT.14) THEN
        I=24+8*(U-14)
      ENDIF
      II=V
      IF (V.GT.10) THEN
        II=12+4*(V-11)
      ENDIF
      IF (V.GT.14) THEN
        II=24+8*(V-14)
      ENDIF
      IF (U.LE.J.AND.V.LE.J) THEN
        WRITE(9,998) I,Q(U),II,Q(V)
      ENDIF
      IF (U.LE.J.AND.V.GT.J) THEN
        WRITE(9,999) I,Q(U)
      ENDIF
20    CONTINUE
      WRITE(9,*)' '
      WRITE(9,*)'The best Gaussian approximation is Q(',JJ,') =',Q(J)
      IF (J.LE.11) THEN
        WRITE(9,*)' '
      ENDIF
      WRITE(9,*)'                 The error estimate is  +-',Close
998   FORMAT(' Q(',I2,') = ',F15.7,
     +'        Q(',I2,') = ',F15.7)
999   FORMAT(' Q(',I2,') = ',F15.7)
      RETURN
      END














C
C     The following data must be stored in the file
C
C     ALG7-6.DAT
C
C     which contains the abscissas and weights.
C














0.0
2.0
0.577350269189626
1.0
0.774596669241483
0.0
0.555555555555556
0.888888888888889
0.861136311594053
0.339981043584856
0.347854845137454
0.652145154862546
0.906179845938664
0.538469310105683
0.0
0.236926885056189
0.478628670499366
0.568888888888889
0.932469514203152
0.661209386466265
0.238619186083197
0.171324492379170
0.360761573048139
0.467913934572691
0.949107912342759
0.741531185599394
0.405845151377397
0.0
0.129484966168870
0.279705391489277
0.381830050505119
0.417959183673469
0.960289856497536
0.796666477413627
0.525532409916329
0.183434642495650
0.101228536290376
0.222381034453374
0.313706645877887
0.362683783378362
0.968160239507626
0.836031107326636
0.613371432700590
0.324253423403809
0.0
0.081274388361574
0.180648160694857
0.260610696402935
0.312347077040003
0.330239355001260
0.973906528517172
0.865063366688985
0.679409568299024
0.433395394129247
0.148874338981631
0.066671344308688
0.149451349150581
0.219086362515982
0.269266719309996
0.295524224714735
0.981560634246719
0.904117256370475
0.769902674194305
0.587317954286617
0.367831498998180
0.125233408511469
0.047175336386512
0.106939325995318
0.160078328543346
0.203167426723066
0.233492536538355
0.249147045813403
0.989400934991650
0.944575023073233
0.865631202387832
0.755404408355003
0.617876244402644
0.458016777657227
0.281603550779259
0.095012509837637
0.027152459411754
0.062253523938648
0.095158511682493
0.124628971255534
0.149595988816577
0.169156519395003
0.182603415044924
0.189450610455068
0.993128599185095
0.963971927277914
0.912234428251326
0.839116971822219
0.746331906460151
0.636053680726515
0.510867001950827
0.373706088715420
0.227785851141645
0.076526521133497
0.017614007139152
0.040601429800387
0.062672048334109
0.083276741576705
0.101930119817240
0.118194531961518
0.131688638449177
0.142096109318382
0.149172986472604
0.152753387130726
0.995187219997021
0.974728555971309
0.938274552002733
0.886415527004401
0.820001985973903
0.740124191578554
0.648093651936976
0.545421471388840
0.433793507626045
0.315042679696163
0.191118867473616
0.064056892862606
0.012341229799987
0.028531388628934
0.044277438817420
0.059298584915437
0.073346481411080
0.086190161531953
0.097618652104114
0.107444270115966
0.115505668053726
0.121670472927803
0.125837456346828
0.127938195346752
0.997263861849482
0.985611511545268
0.964762255587506
0.934906075937740
0.896321155766052
0.849367613732570
0.794483795967942
0.732182118740290
0.663044266930215
0.587715757240762
0.506899908932229
0.421351276130635
0.331868602282128
0.239287362252137
0.144471961582796
0.048307665687738
0.007018610009470
0.016274394730906
0.025392065309262
0.034273862913021
0.042835898022227
0.050998059262376
0.058684093478536
0.065822222776362
0.072345794108849
0.078193895787070
0.083311924226947
0.087652093004404
0.091173878695764
0.093844399080805
0.095638720079275
0.096540088514728
0.998237709710559
0.990726238699457
0.977259949983774
0.957916819213792
0.932812808278677
0.902098806968874
0.865959503212260
0.824612230833312
0.778305651426519
0.727318255189927
0.671956684614180
0.612553889667980
0.549467125095128
0.483075801686179
0.413779204371605
0.341994090825758
0.268152185007254
0.192697580701371
0.116084070675255
0.038772417506051
0.004521277098533
0.010498284531153
0.016421058381908
0.022245849194167
0.027937006980023
0.033460195282548
0.038782167974472
0.043870908185673
0.048695807635072
0.053227846983937
0.057439769099392
0.061306242492929
0.064804013456601
0.067912045815234
0.070611647391287
0.072886582395804
0.074723169057968
0.076110361900626
0.077039818164248
0.077505947978425
0.998771007252426
0.993530172266351
0.984124583722827
0.970591592546247
0.952987703160431
0.931386690706554
0.905879136715570
0.876572020274248
0.843588261624394
0.807066204029443
0.767159032515740
0.724034130923815
0.677872379632664
0.628867396776514
0.577224726083973
0.523160974722233
0.466902904750958
0.408686481990717
0.348755886292161
0.287362487355456
0.224763790394689
0.161222356068892
0.097004699209463
0.032380170962869
0.003153346052306
0.007327553901276
0.011477234579235
0.015579315722944
0.019616160457356
0.023570760839324
0.027426509708357
0.031167227832798
0.034777222564770
0.038241351065831
0.041545082943465
0.044674560856694
0.047616658492490
0.050359035553854
0.052890189485194
0.055199503699984
0.057277292100403
0.059114839698396
0.060704439165894
0.062039423159893
0.063114192286254
0.063924238584648
0.064466164435950
0.064737696812684
  
