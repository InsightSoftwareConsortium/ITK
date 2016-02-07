*********************************************************
*
*     These routines are for testing  LSQR.
*
*********************************************************

      SUBROUTINE APROD ( MODE, M, N, X, Y, LENIW, LENRW, IW, RW )
      INTEGER            MODE, M, N, LENIW, LENRW
      INTEGER            IW(LENIW)
      DOUBLE PRECISION   X(N), Y(M), RW(LENRW)

*     ------------------------------------------------------------------
*     This is the matrix-vector product routine required by  LSQR
*     for a test matrix of the form  A = HY*D*HZ.  The quantities
*     defining D, HY, HZ are in the work array RW, followed by a
*     work array W.  These are passed to APROD1 and APROD2 in order to
*     make the code readable.
*     ------------------------------------------------------------------

      INTEGER            LOCD, LOCHY, LOCHZ, LOCW

      LOCD   = 1
      LOCHY  = LOCD  + N
      LOCHZ  = LOCHY + M
      LOCW   = LOCHZ + N

      IF (MODE .EQ. 1) CALL APROD1( M, N, X, Y,
     $   RW(LOCD), RW(LOCHY), RW(LOCHZ), RW(LOCW) )

      IF (MODE .NE. 1) CALL APROD2( M, N, X, Y,
     $   RW(LOCD), RW(LOCHY), RW(LOCHZ), RW(LOCW) )

*     End of APROD
      END

      SUBROUTINE APROD1( M, N, X, Y, D, HY, HZ, W )
      INTEGER            M, N
      DOUBLE PRECISION   X(N), Y(M), D(N), HY(M), HZ(N), W(M)

*     ------------------------------------------------------------------
*     APROD1  computes  Y = Y + A*X  for subroutine APROD,
*     where A is a test matrix of the form  A = HY*D*HZ,
*     and the latter matrices HY, D, HZ are represented by
*     input vectors with the same name.
*     ------------------------------------------------------------------

      INTEGER            I
      DOUBLE PRECISION   ZERO
      PARAMETER        ( ZERO = 0.0 )

      CALL HPROD ( N, HZ, X, W )

      DO 100 I = 1, N
         W(I)  = D(I) * W(I)
  100 CONTINUE

      DO 200 I = N + 1, M
         W(I)  = ZERO
  200 CONTINUE

      CALL HPROD ( M, HY, W, W )

      DO 600 I = 1, M
         Y(I)  = Y(I) + W(I)
  600 CONTINUE

*     End of APROD1
      END

      SUBROUTINE APROD2( M, N, X, Y, D, HY, HZ, W )
      INTEGER            M, N
      DOUBLE PRECISION   X(N), Y(M), D(N), HY(M), HZ(N), W(M)

*     ------------------------------------------------------------------
*     APROD2  computes  X = X + A(T)*Y  for subroutine APROD,
*     where  A  is a test matrix of the form  A = HY*D*HZ,
*     and the latter matrices  HY, D, HZ  are represented by
*     input vectors with the same name.
*     ------------------------------------------------------------------

      INTEGER            I

      CALL HPROD ( M, HY, Y, W )

      DO 100 I = 1, N
         W(I)  = D(I)*W(I)
  100 CONTINUE

      CALL HPROD ( N, HZ, W, W )

      DO 600 I = 1, N
         X(I)  = X(I) + W(I)
  600 CONTINUE

*     End of APROD2
      END

      SUBROUTINE HPROD ( N, HZ, X, Y )
      INTEGER            N
      DOUBLE PRECISION   HZ(N), X(N), Y(N)

*     ------------------------------------------------------------------
*     HPROD  applies a Householder transformation stored in  HZ
*     to get  Y = ( I - 2*HZ*HZ(transpose) ) * X.
*     ------------------------------------------------------------------

      INTEGER            I
      DOUBLE PRECISION   S

      S      = 0.0
      DO 100 I = 1, N
         S     = HZ(I) * X(I)  +  S
  100 CONTINUE

      S      = S + S
      DO 200 I = 1, N
         Y(I)  = X(I)  -  S * HZ(I)
  200 CONTINUE

*     End of HPROD
      END

      SUBROUTINE LSTP  ( M, N, NDUPLC, NPOWER, DAMP, X,
     $                   B, D, HY, HZ, W, ACOND, RNORM )

      INTEGER            M, N, MAXMN, NDUPLC, NPOWER
      DOUBLE PRECISION   DAMP, ACOND, RNORM
      DOUBLE PRECISION   B(M), X(N), D(N), HY(M), HZ(N), W(M)

*     ------------------------------------------------------------------
*     LSTP  generates a sparse least-squares test problem of the form
*
*                (   A    )*X = ( B )
*                ( DAMP*I )     ( 0 )
*
*     having a specified solution X.  The matrix A is constructed
*     in the form  A = HY*D*HZ,  where D is an M by N diagonal matrix,
*     and HY and HZ are Householder transformations.
*
*     The first 6 parameters are input to LSTP.  The remainder are
*     output.  LSTP is intended for use with M .GE. N.
*
*
*     Functions and subroutines
*
*     TESTPROB           APROD1, HPROD
*     BLAS               DNRM2
*     ------------------------------------------------------------------

*     Intrinsics and local variables

      INTRINSIC          COS,  SIN, SQRT
      INTEGER            I, J
      DOUBLE PRECISION   DNRM2
      DOUBLE PRECISION   ALFA, BETA, DAMPSQ, FOURPI, T
      DOUBLE PRECISION   ZERO,        ONE
      PARAMETER        ( ZERO = 0.0,  ONE = 1.0 )

*     ------------------------------------------------------------------
*     Make two vectors of norm 1.0 for the Householder transformations.
*     FOURPI  need not be exact.
*     ------------------------------------------------------------------
      DAMPSQ = DAMP**2
      FOURPI = 4.0 * 3.141592
      ALFA   = FOURPI / M
      BETA   = FOURPI / N

      DO 100 I = 1, M
         HY(I) = SIN( I * ALFA )
  100 CONTINUE

      DO 200 I = 1, N
         HZ(I) = COS( I * BETA )
  200 CONTINUE

      ALFA   = DNRM2 ( M, HY, 1 )
      BETA   = DNRM2 ( N, HZ, 1 )
      CALL DSCAL ( M, (- ONE / ALFA), HY, 1 )
      CALL DSCAL ( N, (- ONE / BETA), HZ, 1 )
*
*     ------------------------------------------------------------------
*     Set the diagonal matrix  D.  These are the singular values of  A.
*     ------------------------------------------------------------------
      DO 300 I = 1, N
         J     = (I - 1 + NDUPLC) / NDUPLC
         T     =  J * NDUPLC
         T     =  T / N
         D(I)  =  T**NPOWER
  300 CONTINUE

      ACOND  = SQRT( (D(N)**2 + DAMPSQ) / (D(1)**2 + DAMPSQ) )

*     ------------------------------------------------------------------
*     Compute the residual vector, storing it in  B.
*     It takes the form  HY*( s )
*                           ( t )
*     where  s  is obtained from  D*s = DAMP**2 * HZ * X
*     and    t  can be anything.
*     ------------------------------------------------------------------
      CALL HPROD ( N, HZ, X, B )

      DO 500 I = 1, N
         B(I)  = DAMPSQ * B(I) / D(I)
  500 CONTINUE

      T      = ONE
      DO 600 I =   N + 1, M
         J     =   I - N
         B(I)  =  (T * J) / M
         T     = - T
  600 CONTINUE

      CALL HPROD ( M, HY, B, B )

*     ------------------------------------------------------------------
*     Now compute the true  B  =  RESIDUAL  +  A*X.
*     ------------------------------------------------------------------
      RNORM  = SQRT(            DNRM2 ( M, B, 1 )**2
     $              +  DAMPSQ * DNRM2 ( N, X, 1 )**2 )
      CALL APROD1( M, N, X, B, D, HY, HZ, W )

*     End of LSTP
      END

      SUBROUTINE TEST  ( M, N, NDUPLC, NPOWER, DAMP )
      INTEGER            M, N, NDUPLC, NPOWER
      DOUBLE PRECISION   DAMP

*     ------------------------------------------------------------------
*     This is an example driver routine for running LSQR.
*     It generates a test problem, solves it, and examines the results.
*     Note that subroutine APROD must be declared EXTERNAL
*     if it is used only in the call to LSQR.
*
*
*     Functions and subroutines
*
*     TESTPROB           APROD
*     BLAS               DCOPY, DNRM2, DSCAL
*     ------------------------------------------------------------------

*     Intrinsics and local variables

      INTRINSIC          MAX, SQRT
      EXTERNAL           APROD
      INTEGER            ISTOP, ITNLIM, J, NOUT
      DOUBLE PRECISION   DNRM2

      PARAMETER        ( MAXM = 200,  MAXN = 100 )
      DOUBLE PRECISION   B(MAXM),  U(MAXM),
     $                   V(MAXN),  W(MAXN), X(MAXN),
     $                   SE(MAXN), XTRUE(MAXN)
      DOUBLE PRECISION   ATOL, BTOL, CONLIM,
     $                   ANORM, ACOND, RNORM, ARNORM,
     $                   DAMPSQ, ENORM, ETOL, XNORM

      PARAMETER        ( LENIW = 1,  LENRW = 600 )
      INTEGER            IW(LENIW)
      DOUBLE PRECISION   RW(LENRW)
      INTEGER            LOCD, LOCHY, LOCHZ, LOCW, LTOTAL

      DOUBLE PRECISION   ONE
      PARAMETER        ( ONE = 1.0 )

      CHARACTER*34       LINE
      DATA               LINE
     $                 /'----------------------------------'/


*     Set the desired solution  XTRUE.

      DO 100 J = 1, N
         XTRUE(J) = N - J
  100 CONTINUE

*     Generate the specified test problem.
*     The workspace array  IW  is not needed in this application.
*     The workspace array  RW  is used for the following vectors:
*     D(N), HY(M), HZ(N), W(MAX(M,N)).
*     The vectors  D, HY, HZ  will define the test matrix A.
*     W is needed for workspace in APROD1 and APROD2.

      LOCD   = 1
      LOCHY  = LOCD  + N
      LOCHZ  = LOCHY + M
      LOCW   = LOCHZ + N
      LTOTAL = LOCW  + MAX(M,N) - 1
      IF (LTOTAL .GT. LENRW) GO TO 900

      CALL LSTP  ( M, N, NDUPLC, NPOWER, DAMP, XTRUE,
     $             B, RW(LOCD), RW(LOCHY), RW(LOCHZ), RW(LOCW),
     $             ACOND, RNORM )

*     Solve the problem defined by APROD, DAMP and B.
*     Copy the rhs vector B into U  (LSQR will overwrite U)
*     and set the other input parameters for LSQR.

      CALL DCOPY ( M, B, 1, U, 1 )
      ATOL   = 1.0E-10
      BTOL   = ATOL
      CONLIM = 10.0 * ACOND
      ITNLIM = M + N + 50
      NOUT   = 6
      WRITE(NOUT, 1000) LINE, LINE,
     $                  M, N, NDUPLC, NPOWER, DAMP, ACOND, RNORM,
     $                  LINE, LINE

      CALL LSQR  ( M, N, APROD, DAMP,
     $             LENIW, LENRW, IW, RW,
     $             U, V, W, X, SE,
     $             ATOL, BTOL, CONLIM, ITNLIM, NOUT,
     $             ISTOP, ITN, ANORM, ACOND, RNORM, ARNORM, XNORM )

*     Examine the results.
*     We print the residual norms  RNORM  and  ARNORM  given by LSQR,
*     and then compute their true values in terms of the solution  X
*     obtained by  LSQR.  At least one of them should be small.

      DAMPSQ = DAMP**2
      WRITE(NOUT, 2000)
      WRITE(NOUT, 2100) RNORM, ARNORM, XNORM

*     Compute  U = A*X - B.
*     This is the negative of the usual residual vector.
*     It will be close to zero only if  B  is a compatible rhs
*     and  X  is close to a solution.

      CALL DCOPY ( M, B, 1, U, 1 )
      CALL DSCAL ( M, (-ONE), U, 1 )
      CALL APROD ( 1, M, N, X, U, LENIW, LENRW, IW, RW )

*     Compute  V = A(transpose)*U  +  DAMP**2 * X.
*     This will be close to zero in all cases
*     if  X  is close to a solution.

      CALL DCOPY ( N, X, 1, V, 1 )
      CALL DSCAL ( N, DAMPSQ, V, 1 )
      CALL APROD ( 2, M, N, V, U, LENIW, LENRW, IW, RW )

*     Compute the norms associated with  X, U, V.

      XNORM  = DNRM2 ( N, X, 1 )
      RNORM  = SQRT( DNRM2 ( M, U, 1 )**2  +  DAMPSQ * XNORM**2 )
      ARNORM = DNRM2 ( N, V, 1 )
      WRITE(NOUT, 2200) RNORM, ARNORM, XNORM

*     Print the solution and standard error estimates from  LSQR.

      WRITE(NOUT, 2500) (J, X(J),  J = 1, N)
      WRITE(NOUT, 2600) (J, SE(J), J = 1, N)

*     Print a clue about whether the solution looks OK.

      DO 500 J = 1, N
         W(J)  = X(J) - XTRUE(J)
  500 CONTINUE
      ENORM    = DNRM2 ( N, W, 1 ) / (ONE  +  DNRM2 ( N, XTRUE, 1 ))
      ETOL     = 1.0D-5
      IF (ENORM .LE. ETOL) WRITE(NOUT, 3000) ENORM
      IF (ENORM .GT. ETOL) WRITE(NOUT, 3100) ENORM
      RETURN

*     Not enough workspace.

  900 WRITE(NOUT, 9000) LTOTAL
      RETURN

 1000 FORMAT(1P
     $ // 1X, 2A
     $ /  ' Least-Squares Test Problem      P(', 4I5, E12.2, ' )'
     $ // ' Condition no. =', E12.4,  '     Residual function =', E17.9
     $ /  1X, 2A)
 2000 FORMAT(
     $ // 22X, ' Residual norm    Residual norm    Solution norm'
     $  / 22X, '(Abar X - bbar)   (Normal eqns)         (X)' /)
 2100 FORMAT(1P, ' Estimated by LSQR', 3E17.5)
 2200 FORMAT(1P, ' Computed from  X ', 3E17.5)
 2500 FORMAT(//' Solution  X' / 4(I6, G14.6))
 2600 FORMAT(/ ' Standard errors  SE' / 4(I6, G14.6))
 3000 FORMAT(1P / ' LSQR  appears to be successful.',
     $        '     Relative error in  X  =', E10.2)
 3100 FORMAT(1P / ' LSQR  appears to have failed.  ',
     $        '     Relative error in  X  =', E10.2)
 9000 FORMAT(/ ' XXX  Insufficient workspace.',
     $        '  The length of  RW  should be at least', I6)
*     End of TEST
      END

*     -------------
*     Main program.
*     -------------
      DOUBLE PRECISION   DAMP1, DAMP2, DAMP3, DAMP4, ZERO
*
      ZERO   = 0.0
      DAMP1  = 0.1
      DAMP2  = 0.01
      DAMP3  = 0.001
      DAMP4  = 0.0001
      CALL TEST  (  1,  1, 1, 1, ZERO  )
      CALL TEST  (  2,  1, 1, 1, ZERO  )
      CALL TEST  ( 40, 40, 4, 4, ZERO  )
      CALL TEST  ( 40, 40, 4, 4, DAMP2 )
      CALL TEST  ( 80, 40, 4, 4, DAMP2 )
      STOP

*     End of main program for testing LSQR
      END
