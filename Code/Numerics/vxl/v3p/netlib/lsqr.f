* From arpa!sol-michael.stanford.edu!mike 5 May 89 23:53:00 PDT
      SUBROUTINE LSQR  ( M, N, APROD, DAMP,
     $                   LENIW, LENRW, IW, RW,
     $                   U, V, W, X, SE,
     $                   ATOL, BTOL, CONLIM, ITNLIM, NOUT,
     $                   ISTOP, ITN, ANORM, ACOND, RNORM, ARNORM, XNORM)

      EXTERNAL           APROD
      INTEGER            M, N, LENIW, LENRW, ITNLIM, NOUT, ISTOP, ITN
      INTEGER            IW(LENIW)
      DOUBLE PRECISION   RW(LENRW), U(M), V(N), W(N), X(N), SE(N),
     $                   ATOL, BTOL, CONLIM, DAMP,
     $                   ANORM, ACOND, RNORM, ARNORM, XNORM
*-----------------------------------------------------------------------
*
*     LSQR  finds a solution x to the following problems:
*
*     1. Unsymmetric equations --    solve  A*x = b
*
*     2. Linear least squares  --    solve  A*x = b
*                                    in the least-squares sense
*
*     3. Damped least squares  --    solve  (   A    )*x = ( b )
*                                           ( damp*I )     ( 0 )
*                                    in the least-squares sense
*
*     where A is a matrix with m rows and n columns, b is an
*     m-vector, and damp is a scalar.  (All quantities are real.)
*     The matrix A is intended to be large and sparse.  It is accessed
*     by means of subroutine calls of the form
*
*                CALL APROD ( mode, m, n, x, y, LENIW, LENRW, IW, RW )
*
*     which must perform the following functions:
*
*                If MODE = 1, compute  y = y + A*x.
*                If MODE = 2, compute  x = x + A(transpose)*y.
*
*     The vectors x and y are input parameters in both cases.
*     If  mode = 1,  y should be altered without changing x.
*     If  mode = 2,  x should be altered without changing y.
*     The parameters LENIW, LENRW, IW, RW may be used for workspace
*     as described below.
*
*     The rhs vector b is input via U, and subsequently overwritten.
*
*
*     Note:  LSQR uses an iterative method to approximate the solution.
*     The number of iterations required to reach a certain accuracy
*     depends strongly on the scaling of the problem.  Poor scaling of
*     the rows or columns of A should therefore be avoided where
*     possible.
*
*     For example, in problem 1 the solution is unaltered by
*     row-scaling.  If a row of A is very small or large compared to
*     the other rows of A, the corresponding row of ( A  b ) should be
*     scaled up or down.
*
*     In problems 1 and 2, the solution x is easily recovered
*     following column-scaling.  Unless better information is known,
*     the nonzero columns of A should be scaled so that they all have
*     the same Euclidean norm (e.g., 1.0).
*
*     In problem 3, there is no freedom to re-scale if damp is
*     nonzero.  However, the value of damp should be assigned only
*     after attention has been paid to the scaling of A.
*
*     The parameter damp is intended to help regularize
*     ill-conditioned systems, by preventing the true solution from
*     being very large.  Another aid to regularization is provided by
*     the parameter ACOND, which may be used to terminate iterations
*     before the computed solution becomes very large.
*
*
*     Notation
*     --------
*
*     The following quantities are used in discussing the subroutine
*     parameters:
*
*     Abar   =  (   A    ),          bbar  =  ( b )
*               ( damp*I )                    ( 0 )
*
*     r      =  b  -  A*x,           rbar  =  bbar  -  Abar*x
*
*     rnorm  =  sqrt( norm(r)**2  +  damp**2 * norm(x)**2 )
*            =  norm( rbar )
*
*     RELPR  =  the relative precision of floating-point arithmetic
*               on the machine being used.  For example, on the IBM 370,
*               RELPR is about 1.0E-6 and 1.0D-16 in single and double
*               precision respectively.
*
*     LSQR  minimizes the function rnorm with respect to x.
*
*
*     Parameters
*     ----------
*
*     M       input      m, the number of rows in A.
*
*     N       input      n, the number of columns in A.
*
*     APROD   external   See above.
*
*     DAMP    input      The damping parameter for problem 3 above.
*                        (DAMP should be 0.0 for problems 1 and 2.)
*                        If the system A*x = b is incompatible, values
*                        of DAMP in the range 0 to sqrt(RELPR)*norm(A)
*                        will probably have a negligible effect.
*                        Larger values of DAMP will tend to decrease
*                        the norm of x and reduce the number of
*                        iterations required by LSQR.
*
*                        The work per iteration and the storage needed
*                        by LSQR are the same for all values of DAMP.
*
*     LENIW   input      The length of the workspace array IW.
*     LENRW   input      The length of the workspace array RW.
*     IW      workspace  An integer array of length LENIW.
*     RW      workspace  A real array of length LENRW.
*
*             Note:  LSQR  does not explicitly use the previous four
*             parameters, but passes them to subroutine APROD for
*             possible use as workspace.  If APROD does not need
*             IW or RW, the values LENIW = 1 or LENRW = 1 should
*             be used, and the actual parameters corresponding to
*             IW or RW  may be any convenient array of suitable type.
*
*     U(M)    input      The rhs vector b.  Beware that U is
*                        over-written by LSQR.
*
*     V(N)    workspace
*     W(N)    workspace
*
*     X(N)    output     Returns the computed solution x.
*
*     SE(N)   output     Returns standard error estimates for the
*                        components of X.  For each i, SE(i) is set
*                        to the value  rnorm * sqrt( sigma(i,i) / T ),
*                        where sigma(i,i) is an estimate of the i-th
*                        diagonal of the inverse of Abar(transpose)*Abar
*                        and  T = 1      if  m .le. n,
*                             T = m - n  if  m .gt. n  and  damp = 0,
*                             T = m      if  damp .ne. 0.
*
*     ATOL    input      An estimate of the relative error in the data
*                        defining the matrix A.  For example,
*                        if A is accurate to about 6 digits, set
*                        ATOL = 1.0E-6 .
*
*     BTOL    input      An extimate of the relative error in the data
*                        defining the rhs vector b.  For example,
*                        if b is accurate to about 6 digits, set
*                        BTOL = 1.0E-6 .
*
*     CONLIM  input      An upper limit on cond(Abar), the apparent
*                        condition number of the matrix Abar.
*                        Iterations will be terminated if a computed
*                        estimate of cond(Abar) exceeds CONLIM.
*                        This is intended to prevent certain small or
*                        zero singular values of A or Abar from
*                        coming into effect and causing unwanted growth
*                        in the computed solution.
*
*                        CONLIM and DAMP may be used separately or
*                        together to regularize ill-conditioned systems.
*
*                        Normally, CONLIM should be in the range
*                        1000 to 1/RELPR.
*                        Suggested value:
*                        CONLIM = 1/(100*RELPR)  for compatible systems,
*                        CONLIM = 1/(10*sqrt(RELPR)) for least squares.
*
*             Note:  If the user is not concerned about the parameters
*             ATOL, BTOL and CONLIM, any or all of them may be set
*             to zero.  The effect will be the same as the values
*             RELPR, RELPR and 1/RELPR respectively.
*
*     ITNLIM  input      An upper limit on the number of iterations.
*                        Suggested value:
*                        ITNLIM = n/2   for well-conditioned systems
*                                       with clustered singular values,
*                        ITNLIM = 4*n   otherwise.
*
*     NOUT    input      File number for printed output.  If positive,
*                        a summary will be printed on file NOUT.
*
*     ISTOP   output     An integer giving the reason for termination:
*
*                0       x = 0  is the exact solution.
*                        No iterations were performed.
*
*                1       The equations A*x = b are probably
*                        compatible.  Norm(A*x - b) is sufficiently
*                        small, given the values of ATOL and BTOL.
*
*                2       The system A*x = b is probably not
*                        compatible.  A least-squares solution has
*                        been obtained that is sufficiently accurate,
*                        given the value of ATOL.
*
*                3       An estimate of cond(Abar) has exceeded
*                        CONLIM.  The system A*x = b appears to be
*                        ill-conditioned.  Otherwise, there could be an
*                        error in subroutine APROD.
*
*                4       The equations A*x = b are probably
*                        compatible.  Norm(A*x - b) is as small as
*                        seems reasonable on this machine.
*
*                5       The system A*x = b is probably not
*                        compatible.  A least-squares solution has
*                        been obtained that is as accurate as seems
*                        reasonable on this machine.
*
*                6       Cond(Abar) seems to be so large that there is
*                        no point in doing further iterations,
*                        given the precision of this machine.
*                        There could be an error in subroutine APROD.
*
*                7       The iteration limit ITNLIM was reached.
*
*     ITN     output     The number of iterations performed.
*
*     ANORM   output     An estimate of the Frobenius norm of  Abar.
*                        This is the square-root of the sum of squares
*                        of the elements of Abar.
*                        If DAMP is small and if the columns of A
*                        have all been scaled to have length 1.0,
*                        ANORM should increase to roughly sqrt(n).
*                        A radically different value for ANORM may
*                        indicate an error in subroutine APROD (there
*                        may be an inconsistency between modes 1 and 2).
*
*     ACOND   output     An estimate of cond(Abar), the condition
*                        number of Abar.  A very high value of ACOND
*                        may again indicate an error in APROD.
*
*     RNORM   output     An estimate of the final value of norm(rbar),
*                        the function being minimized (see notation
*                        above).  This will be small if A*x = b has
*                        a solution.
*
*     ARNORM  output     An estimate of the final value of
*                        norm( Abar(transpose)*rbar ), the norm of
*                        the residual for the usual normal equations.
*                        This should be small in all cases.  (ARNORM
*                        will often be smaller than the true value
*                        computed from the output vector X.)
*
*     XNORM   output     An estimate of the norm of the final
*                        solution vector X.
*
*
*     Subroutines and functions used
*     ------------------------------
*
*     USER               APROD
*     BLAS               DCOPY, DNRM2, DSCAL (see Lawson et al. below)
*
*
*     Precision
*     ---------
*
*     The number of iterations required by LSQR will usually decrease
*     if the computation is performed in higher precision.  To convert
*     LSQR between single and double precision, change the words
*                        DOUBLE PRECISION
*                        DCOPY, DNRM2, DSCAL
*     to the appropriate FORTRAN and BLAS equivalents.
*     Also change 'D+' or 'E+' in the PARAMETER statement.
*
*
*     References
*     ----------
*
*     C.C. Paige and M.A. Saunders,  LSQR: An algorithm for sparse
*          linear equations and sparse least squares,
*          ACM Transactions on Mathematical Software 8, 1 (March 1982),
*          pp. 43-71.
*
*     C.C. Paige and M.A. Saunders,  Algorithm 583, LSQR: Sparse
*          linear equations and least-squares problems,
*          ACM Transactions on Mathematical Software 8, 2 (June 1982),
*          pp. 195-209.
*
*     C.L. Lawson, R.J. Hanson, D.R. Kincaid and F.T. Krogh,
*          Basic linear algebra subprograms for Fortran usage,
*          ACM Transactions on Mathematical Software 5, 3 (Sept 1979),
*          pp. 308-323 and 324-325.
*-----------------------------------------------------------------------
*
*
*     LSQR development:
*     22 Feb 1982: LSQR sent to ACM TOMS to become Algorithm 583.
*     15 Sep 1985: Final F66 version.  LSQR sent to "misc" in netlib.
*     13 Oct 1987: Bug (Robert Davies, DSIR).  Have to delete
*                     IF ( (ONE + DABS(T)) .LE. ONE ) GO TO 200
*                  from loop 200.  The test was an attempt to reduce
*                  underflows, but caused W(I) not to be updated.
*     17 Mar 1989: First F77 version.
*     04 May 1989: Bug (David Gay, AT&T).  When the second BETA is zero,
*                  RNORM = 0 and
*                  TEST2 = ARNORM / (ANORM * RNORM) overflows.
*                  Fixed by testing for RNORM = 0.
*     05 May 1989: Sent to "misc" in netlib.
*
*     Michael A. Saunders            (na.saunders @ NA-net.stanford.edu)
*     Department of Operations Research
*     Stanford University
*     Stanford, CA 94305-4022.
*-----------------------------------------------------------------------

*     Intrinsics and local variables

      INTRINSIC          ABS, MOD, SQRT
      INTEGER            I, NCONV, NSTOP
      DOUBLE PRECISION   DNRM2
      DOUBLE PRECISION   ALFA, BBNORM, BETA, BNORM,
     $                   CS, CS1, CS2, CTOL, DAMPSQ, DDNORM, DELTA,
     $                   GAMMA, GAMBAR, PHI, PHIBAR, PSI,
     $                   RES1, RES2, RHO, RHOBAR, RHBAR1, RHBAR2,
     $                   RHS, RTOL, SN, SN1, SN2,
     $                   T, TAU, TEST1, TEST2, TEST3,
     $                   THETA, T1, T2, T3, XXNORM, Z, ZBAR

      DOUBLE PRECISION   ZERO,           ONE
      PARAMETER        ( ZERO = 0.0D+0,  ONE = 1.0D+0 )

      CHARACTER*16       ENTER, EXIT
      CHARACTER*60       MSG(0:7)

      DATA               ENTER /' Enter LSQR.    '/,
     $                   EXIT  /' Exit  LSQR.    '/

      DATA               MSG
     $ / 'The exact solution is  X = 0',
     $   'Ax - b is small enough, given ATOL, BTOL',
     $   'The least-squares solution is good enough, given ATOL',
     $   'The estimate of cond(Abar) has exceeded CONLIM',
     $   'Ax - b is small enough for this machine',
     $   'The least-squares solution is good enough for this machine',
     $   'Cond(Abar) seems to be too large for this machine',
     $   'The iteration limit has been reached' /
*-----------------------------------------------------------------------


*     Initialize.

C      IF (NOUT .GT. 0)
C     $   WRITE(NOUT, 1000) ENTER, M, N, DAMP, ATOL, CONLIM, BTOL, ITNLIM
      ITN    =   0
      ISTOP  =   0
      NSTOP  =   0
      CTOL   =   ZERO
      IF (CONLIM .GT. ZERO) CTOL = ONE / CONLIM
      ANORM  =   ZERO
      ACOND  =   ZERO
      BBNORM =   ZERO
      DAMPSQ =   DAMP**2
      DDNORM =   ZERO
      RES2   =   ZERO
      XNORM  =   ZERO
      XXNORM =   ZERO
      CS2    = - ONE
      SN2    =   ZERO
      Z      =   ZERO

      DO 10  I = 1, N
         V(I)  =  ZERO
         X(I)  =  ZERO
        SE(I)  =  ZERO
   10 CONTINUE

*     Set up the first vectors U and V for the bidiagonalization.
*     These satisfy  BETA*U = b,  ALFA*V = A(transpose)*U.

      ALFA   =   ZERO
      BETA   =   DNRM2 ( M, U, 1 )

      IF (BETA .GT. ZERO) THEN
         CALL DSCAL ( M, (ONE / BETA), U, 1 )
         CALL APROD ( 2, M, N, V, U, LENIW, LENRW, IW, RW )
         ALFA   =   DNRM2 ( N, V, 1 )
      END IF

      IF (ALFA .GT. ZERO) THEN
         CALL DSCAL ( N, (ONE / ALFA), V, 1 )
         CALL DCOPY ( N, V, 1, W, 1 )
      END IF

      ARNORM =   ALFA * BETA
      IF (ARNORM .EQ. ZERO) GO TO 800

      RHOBAR =   ALFA
      PHIBAR =   BETA
      BNORM  =   BETA
      RNORM  =   BETA

C      IF (NOUT   .GT.  0  ) THEN
C         IF (DAMPSQ .EQ. ZERO) THEN
C             WRITE(NOUT, 1200)
C         ELSE
C             WRITE(NOUT, 1300)
C         END IF
C         TEST1  = ONE
C         TEST2  = ALFA / BETA
C         WRITE(NOUT, 1500) ITN, X(1), RNORM, TEST1, TEST2
C         WRITE(NOUT, 1600)
C      END IF

*     ------------------------------------------------------------------
*     Main iteration loop.
*     ------------------------------------------------------------------
  100 ITN    = ITN + 1

*     Perform the next step of the bidiagonalization to obtain the
*     next  BETA, U, ALFA, V.  These satisfy the relations
*                BETA*U  =  A*V  -  ALFA*U,
*                ALFA*V  =  A(transpose)*U  -  BETA*V.

      CALL DSCAL ( M, (- ALFA), U, 1 )
      CALL APROD ( 1, M, N, V, U, LENIW, LENRW, IW, RW )
      BETA   =   DNRM2 ( M, U, 1 )
      BBNORM =   BBNORM  +  ALFA**2  +  BETA**2  +  DAMPSQ

      IF (BETA .GT. ZERO) THEN
         CALL DSCAL ( M, (ONE / BETA), U, 1 )
         CALL DSCAL ( N, (- BETA), V, 1 )
         CALL APROD ( 2, M, N, V, U, LENIW, LENRW, IW, RW )
         ALFA   =   DNRM2 ( N, V, 1 )
         IF (ALFA .GT. ZERO) THEN
            CALL DSCAL ( N, (ONE / ALFA), V, 1 )
         END IF
      END IF

*     Use a plane rotation to eliminate the damping parameter.
*     This alters the diagonal (RHOBAR) of the lower-bidiagonal matrix.

      RHBAR2 = RHOBAR**2  +  DAMPSQ
      RHBAR1 = SQRT( RHBAR2 )
      CS1    = RHOBAR / RHBAR1
      SN1    = DAMP   / RHBAR1
      PSI    = SN1 * PHIBAR
      PHIBAR = CS1 * PHIBAR

*     Use a plane rotation to eliminate the subdiagonal element (BETA)
*     of the lower-bidiagonal matrix, giving an upper-bidiagonal matrix.

      RHO    =   SQRT( RHBAR2  +  BETA**2 )
      CS     =   RHBAR1 / RHO
      SN     =   BETA   / RHO
      THETA  =   SN * ALFA
      RHOBAR = - CS * ALFA
      PHI    =   CS * PHIBAR
      PHIBAR =   SN * PHIBAR
      TAU    =   SN * PHI

*     Update  X, W  and the standard error estimates.

      T1     =   PHI   / RHO
      T2     = - THETA / RHO
      T3     =   ONE   / RHO

      DO 200  I =  1, N
         T      =  W(I)
         X(I)   =  T1*T  +  X(I)
         W(I)   =  T2*T  +  V(I)
         T      = (T3*T)**2
         SE(I)  =  T     +  SE(I)
         DDNORM =  T     +  DDNORM
  200 CONTINUE

*     Use a plane rotation on the right to eliminate the
*     super-diagonal element (THETA) of the upper-bidiagonal matrix.
*     Then use the result to estimate  norm(X).

      DELTA  =   SN2 * RHO
      GAMBAR = - CS2 * RHO
      RHS    =   PHI    - DELTA * Z
      ZBAR   =   RHS    / GAMBAR
      XNORM  =   SQRT( XXNORM    + ZBAR **2 )
      GAMMA  =   SQRT( GAMBAR**2 + THETA**2 )
      CS2    =   GAMBAR / GAMMA
      SN2    =   THETA  / GAMMA
      Z      =   RHS    / GAMMA
      XXNORM =   XXNORM + Z**2

*     Test for convergence.
*     First, estimate the norm and condition of the matrix  Abar,
*     and the norms of  rbar  and  Abar(transpose)*rbar.

      ANORM  =   SQRT( BBNORM )
      ACOND  =   ANORM * SQRT( DDNORM )
      RES1   =   PHIBAR**2
      RES2   =   RES2  +  PSI**2
      RNORM  =   SQRT( RES1 + RES2 )
      ARNORM =   ALFA  * ABS( TAU )

*     Now use these norms to estimate certain other quantities,
*     some of which will be small near a solution.

      TEST1  =   RNORM /  BNORM
      TEST2  =   ZERO
      IF (RNORM .GT. ZERO) TEST2 = ARNORM / (ANORM * RNORM)
      TEST3  =   ONE   /  ACOND
      T1     =   TEST1 / (ONE  +  ANORM * XNORM / BNORM)
      RTOL   =   BTOL  +  ATOL *  ANORM * XNORM / BNORM

*     The following tests guard against extremely small values of
*     ATOL, BTOL  or  CTOL.  (The user may have set any or all of
*     the parameters  ATOL, BTOL, CONLIM  to zero.)
*     The effect is equivalent to the normal tests using
*     ATOL = RELPR,  BTOL = RELPR,  CONLIM = 1/RELPR.

      T3     =   ONE + TEST3
      T2     =   ONE + TEST2
      T1     =   ONE + T1
      IF (ITN .GE. ITNLIM) ISTOP = 7
      IF (T3  .LE. ONE   ) ISTOP = 6
      IF (T2  .LE. ONE   ) ISTOP = 5
      IF (T1  .LE. ONE   ) ISTOP = 4

*     Allow for tolerances set by the user.

      IF (TEST3 .LE. CTOL) ISTOP = 3
      IF (TEST2 .LE. ATOL) ISTOP = 2
      IF (TEST1 .LE. RTOL) ISTOP = 1
*     ==================================================================

*     See if it is time to print something.

      IF (NOUT  .LE.  0       ) GO TO 600
      IF (N     .LE. 40       ) GO TO 400
      IF (ITN   .LE. 10       ) GO TO 400
      IF (ITN   .GE. ITNLIM-10) GO TO 400
      IF (MOD(ITN,10) .EQ. 0  ) GO TO 400
      IF (TEST3 .LE.  2.0*CTOL) GO TO 400
      IF (TEST2 .LE. 10.0*ATOL) GO TO 400
      IF (TEST1 .LE. 10.0*RTOL) GO TO 400
      IF (ISTOP .NE.  0       ) GO TO 400
      GO TO 600

*     Print a line for this iteration.

 400  IF (1 .EQ. 1) GO TO 600
C  400 WRITE(NOUT, 1500) ITN, X(1), RNORM, TEST1, TEST2, ANORM, ACOND
C  IF (MOD(ITN,10) .EQ. 0) WRITE(NOUT, 1600)
*     ==================================================================

*     Stop if appropriate.
*     The convergence criteria are required to be met on  NCONV
*     consecutive iterations, where  NCONV  is set below.
*     Suggested value:  NCONV = 1, 2  or  3.

  600 IF (ISTOP .EQ. 0) NSTOP = 0
      IF (ISTOP .EQ. 0) GO TO 100
      NCONV  =   1
      NSTOP  =   NSTOP + 1
      IF (NSTOP .LT. NCONV  .AND.  ITN .LT. ITNLIM) ISTOP = 0
      IF (ISTOP .EQ. 0) GO TO 100
*     ------------------------------------------------------------------
*     End of iteration loop.
*     ------------------------------------------------------------------


*     Finish off the standard error estimates.

      T    =   ONE
      IF (M      .GT.   N )  T = M - N
      IF (DAMPSQ .GT. ZERO)  T = M
      T    =   RNORM / SQRT( T )

      DO 700  I = 1, N
         SE(I)  = T * SQRT( SE(I) )
  700 CONTINUE

*     Print the stopping condition.

 800  IF (1 .EQ. 1) GO TO 900
C  800 IF (NOUT .GT. 0) THEN
C         WRITE(NOUT, 2000) EXIT, ISTOP, ITN,
C     $                     EXIT, ANORM, ACOND,
C     $                     EXIT, RNORM, ARNORM,
C     $                     EXIT, BNORM, XNORM
C         WRITE(NOUT, 3000) EXIT, MSG(ISTOP)
C      END IF

  900 RETURN

*     ------------------------------------------------------------------
 1000 FORMAT(// 1P, A, '  Least-squares solution of  A*x = b'
     $    / ' The matrix  A  has', I7, ' rows   and', I7, ' columns'
     $    / ' The damping parameter is         DAMP   =', E10.2
     $    / ' ATOL   =', E10.2, 15X,        'CONLIM =', E10.2
     $    / ' BTOL   =', E10.2, 15X,        'ITNLIM =', I10)
 1200 FORMAT(// '   Itn       x(1)           Function',
     $   '     Compatible   LS        Norm A    Cond A' /)
 1300 FORMAT(// '   Itn       x(1)           Function',
     $   '     Compatible   LS     Norm Abar Cond Abar' /)
 1500 FORMAT(1P, I6, 2E17.9, 4E10.2)
 1600 FORMAT(1X)
 2000 FORMAT(/ 1P, A, 6X, 'ISTOP =', I3,   16X, 'ITN    =', I9
     $       /     A, 6X, 'ANORM =', E13.5, 6X, 'ACOND  =', E13.5
     $       /     A, 6X, 'RNORM =', E13.5, 6X, 'ARNORM =', E13.5,
     $       /     A, 6X, 'BNORM =', E13.5, 6X, 'XNORM  =', E13.5)
 3000 FORMAT( A, 6X, A )
*     ------------------------------------------------------------------
*     End of LSQR
      END
