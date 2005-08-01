C     NUMERICAL METHODS: FORTRAN Programs, (c) John H. Mathews 1994
C     To accompany the text:
C     NUMERICAL METHODS for Mathematics, Science and Engineering, 2nd Ed, 1992
C     Prentice Hall, Englewood Cliffs, New Jersey, 07632, U.S.A.
C     This free software is complements of the author.
C
C     Algorithm 7.5 (Adaptive Quadrature Using Simpson's Rule).
C     Section 7.4, Adaptive Quadrature, Page 389
C

C     add missing variable F in Refine subrutine.
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
      DO WHILE (State .EQ. Iterating)
        N = M
        DO J=N,1,-1
          CALL Refine(F,J,SRmat,M,State)
        ENDDO
      ENDDO
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

      SUBROUTINE Refine(F, P,SRmat,M,State)
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


