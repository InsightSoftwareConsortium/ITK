C
C***********************************************************************
C
      SUBROUTINE DLABAX(N, NBAND, A, X, Y)
C
C  THIS SUBROUTINE SETS Y = A*X
C  WHERE X AND Y ARE VECTORS OF LENGTH N
C  AND A IS AN  N X NBAND  SYMMETRIC BAND MATRIX
C
C  FORMAL PARAMETERS
C
      INTEGER N, NBAND
      DOUBLE PRECISION A(NBAND,1), X(1), Y(1)
C
C  LOCAL VARIABLES
C
      INTEGER I, K, L, M
      DOUBLE PRECISION ZERO(1)
C
C  FUNCTIONS CALLED
C
      INTEGER MIN0
C
C  SUBROUTINES CALLED
C
C     DCOPY
C
      ZERO(1) = 0.0D0
      CALL DCOPY(N, ZERO, 0, Y, 1)
      DO 20 K = 1, N
         Y(K) = Y(K) + A(1,K)*X(K)
         M = MIN0(N-K+1, NBAND)
         IF(M .LT. 2) GO TO 20
         DO 10 I = 2, M
            L = K + I - 1
            Y(L) = Y(L) + A(I,K)*X(K)
            Y(K) = Y(K) + A(I,K)*X(L)
   10    CONTINUE
   20 CONTINUE
      RETURN
      END
