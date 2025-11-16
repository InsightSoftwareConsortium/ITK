C     NUMERICAL METHODS: FORTRAN Programs, (c) John H. Mathews 1994
C     To accompany the text:
C     NUMERICAL METHODS for Mathematics, Science and Engineering, 2nd Ed, 1992
C     Prentice Hall, Englewood Cliffs, New Jersey, 07632, U.S.A.
C     This free software is complements of the author.
C
C     Algorithm 7.1 (Composite Trapezoidal Rule).
C     Section 7.2, Composite Trapezoidal and Simpson's Rule, Page 365
C
      SUBROUTINE TRAPRU(F,A,B,M,Trule)
      INTEGER K,M
      DOUBLE PRECISION A,B,H,Sum,Trule,X
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
      DOUBLE PRECISION A,B,H,Sum,Trule,X
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








