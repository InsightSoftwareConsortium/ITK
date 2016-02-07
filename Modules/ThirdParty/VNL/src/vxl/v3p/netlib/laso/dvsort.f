C
C-------------------------------------------------------------------
C
      SUBROUTINE DVSORT(NUM, VAL, RES, IFLAG, V, NMVEC, N, VEC)
      INTEGER NUM, IFLAG, NMVEC, N
      DOUBLE PRECISION VAL(1), RES(1), V(1), VEC(NMVEC,1)
C
C  THIS SUBROUTINE SORTS THE EIGENVALUES (VAL) IN ASCENDING ORDER
C  WHILE CONCURRENTLY SWAPPING THE RESIDUALS AND VECTORS.
      INTEGER I, K, M
      DOUBLE PRECISION TEMP
      IF(NUM .LE. 1) RETURN
      DO 20 I = 2, NUM
         M = NUM - I + 1
         DO 10 K = 1, M
            IF(VAL(K) .LE. VAL(K+1)) GO TO 10
            TEMP = VAL(K)
            VAL(K) = VAL(K+1)
            VAL(K+1) = TEMP
            TEMP = RES(K)
            RES(K) = RES(K+1)
            RES(K+1) = TEMP
            CALL DSWAP(N, VEC(1,K), 1, VEC(1,K+1), 1)
            IF(IFLAG .EQ. 0) GO TO 10
            TEMP = V(K)
            V(K) = V(K+1)
            V(K+1) = TEMP
   10    CONTINUE
   20 CONTINUE
      RETURN
      END
