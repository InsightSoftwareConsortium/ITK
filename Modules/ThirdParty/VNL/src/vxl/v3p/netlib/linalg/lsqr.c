/* linalg/lsqr.f -- translated by f2c (version 20050501).
   You must link the resulting object file with libf2c:
        on Microsoft Windows system, link with libf2c.lib;
        on Linux or Unix systems, link with .../path/to/libf2c.a -lm
        or, if you install libf2c.a in a standard place, with -lf2c -lm
        -- in that order, at the end of the command line, as in
                cc *.o -lf2c -lm
        Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

                http://www.netlib.org/f2c/libf2c.zip
*/

#ifdef __cplusplus
extern "C" {
#endif
#include "v3p_netlib.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__2 = 2;

/* From arpa!sol-michael.stanford.edu!mike 5 May 89 23:53:00 PDT */
/*<    >*/
/* Subroutine */ int lsqr_(integer *m, integer *n,
        int (*aprod)(v3p_netlib_integer*,
                     v3p_netlib_integer*,
                     v3p_netlib_integer*,
                     v3p_netlib_doublereal*,
                     v3p_netlib_doublereal*,
                     v3p_netlib_integer*,
                     v3p_netlib_integer*,
                     v3p_netlib_integer*,
                     v3p_netlib_doublereal*,
                     void*),
        doublereal *
        damp, integer *leniw, integer *lenrw, integer *iw, doublereal *rw,
        doublereal *u, doublereal *v, doublereal *w, doublereal *x,
        doublereal *se, doublereal *atol, doublereal *btol, doublereal *
        conlim, integer *itnlim, integer *nout, integer *istop, integer *itn,
        doublereal *anorm, doublereal *acond, doublereal *rnorm, doublereal *
        arnorm, doublereal *xnorm, void* userdata)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    integer i__;
    doublereal t, z__, t1, t2, t3, cs, sn, cs1, cs2, sn1, sn2, phi, rho, tau,
            psi, rhs, res1, res2, alfa, beta, zbar, ctol, rtol;
    extern doublereal dnrm2_(integer *, doublereal *, integer *);
    doublereal test1, test2, test3, gamma;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *,
            integer *);
    doublereal delta, theta, bnorm;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *,
            doublereal *, integer *);
    integer nconv, nstop;
    doublereal rhbar1, rhbar2, gambar, phibar, rhobar, bbnorm, ddnorm, dampsq,
             xxnorm;

/*<       EXTERNAL           APROD >*/
/*<       INTEGER            M, N, LENIW, LENRW, ITNLIM, NOUT, ISTOP, ITN >*/
/*<       INTEGER            IW(LENIW) >*/
/*<    >*/
/* ----------------------------------------------------------------------- */

/*     LSQR  finds a solution x to the following problems: */

/*     1. Unsymmetric equations --    solve  A*x = b */

/*     2. Linear least squares  --    solve  A*x = b */
/*                                    in the least-squares sense */

/*     3. Damped least squares  --    solve  (   A    )*x = ( b ) */
/*                                           ( damp*I )     ( 0 ) */
/*                                    in the least-squares sense */

/*     where A is a matrix with m rows and n columns, b is an */
/*     m-vector, and damp is a scalar.  (All quantities are real.) */
/*     The matrix A is intended to be large and sparse.  It is accessed */
/*     by means of subroutine calls of the form */

/*                CALL APROD ( mode, m, n, x, y, LENIW, LENRW, IW, RW ) */

/*     which must perform the following functions: */

/*                If MODE = 1, compute  y = y + A*x. */
/*                If MODE = 2, compute  x = x + A(transpose)*y. */

/*     The vectors x and y are input parameters in both cases. */
/*     If  mode = 1,  y should be altered without changing x. */
/*     If  mode = 2,  x should be altered without changing y. */
/*     The parameters LENIW, LENRW, IW, RW may be used for workspace */
/*     as described below. */

/*     The rhs vector b is input via U, and subsequently overwritten. */


/*     Note:  LSQR uses an iterative method to approximate the solution. */
/*     The number of iterations required to reach a certain accuracy */
/*     depends strongly on the scaling of the problem.  Poor scaling of */
/*     the rows or columns of A should therefore be avoided where */
/*     possible. */

/*     For example, in problem 1 the solution is unaltered by */
/*     row-scaling.  If a row of A is very small or large compared to */
/*     the other rows of A, the corresponding row of ( A  b ) should be */
/*     scaled up or down. */

/*     In problems 1 and 2, the solution x is easily recovered */
/*     following column-scaling.  Unless better information is known, */
/*     the nonzero columns of A should be scaled so that they all have */
/*     the same Euclidean norm (e.g., 1.0). */

/*     In problem 3, there is no freedom to re-scale if damp is */
/*     nonzero.  However, the value of damp should be assigned only */
/*     after attention has been paid to the scaling of A. */

/*     The parameter damp is intended to help regularize */
/*     ill-conditioned systems, by preventing the true solution from */
/*     being very large.  Another aid to regularization is provided by */
/*     the parameter ACOND, which may be used to terminate iterations */
/*     before the computed solution becomes very large. */


/*     Notation */
/*     -------- */

/*     The following quantities are used in discussing the subroutine */
/*     parameters: */

/*     Abar   =  (   A    ),          bbar  =  ( b ) */
/*               ( damp*I )                    ( 0 ) */

/*     r      =  b  -  A*x,           rbar  =  bbar  -  Abar*x */

/*     rnorm  =  sqrt( norm(r)**2  +  damp**2 * norm(x)**2 ) */
/*            =  norm( rbar ) */

/*     RELPR  =  the relative precision of floating-point arithmetic */
/*               on the machine being used.  For example, on the IBM 370, */
/*               RELPR is about 1.0E-6 and 1.0D-16 in single and double */
/*               precision respectively. */

/*     LSQR  minimizes the function rnorm with respect to x. */


/*     Parameters */
/*     ---------- */

/*     M       input      m, the number of rows in A. */

/*     N       input      n, the number of columns in A. */

/*     APROD   external   See above. */

/*     DAMP    input      The damping parameter for problem 3 above. */
/*                        (DAMP should be 0.0 for problems 1 and 2.) */
/*                        If the system A*x = b is incompatible, values */
/*                        of DAMP in the range 0 to sqrt(RELPR)*norm(A) */
/*                        will probably have a negligible effect. */
/*                        Larger values of DAMP will tend to decrease */
/*                        the norm of x and reduce the number of */
/*                        iterations required by LSQR. */

/*                        The work per iteration and the storage needed */
/*                        by LSQR are the same for all values of DAMP. */

/*     LENIW   input      The length of the workspace array IW. */
/*     LENRW   input      The length of the workspace array RW. */
/*     IW      workspace  An integer array of length LENIW. */
/*     RW      workspace  A real array of length LENRW. */

/*             Note:  LSQR  does not explicitly use the previous four */
/*             parameters, but passes them to subroutine APROD for */
/*             possible use as workspace.  If APROD does not need */
/*             IW or RW, the values LENIW = 1 or LENRW = 1 should */
/*             be used, and the actual parameters corresponding to */
/*             IW or RW  may be any convenient array of suitable type. */

/*     U(M)    input      The rhs vector b.  Beware that U is */
/*                        over-written by LSQR. */

/*     V(N)    workspace */
/*     W(N)    workspace */

/*     X(N)    output     Returns the computed solution x. */

/*     SE(N)   output     Returns standard error estimates for the */
/*                        components of X.  For each i, SE(i) is set */
/*                        to the value  rnorm * sqrt( sigma(i,i) / T ), */
/*                        where sigma(i,i) is an estimate of the i-th */
/*                        diagonal of the inverse of Abar(transpose)*Abar */
/*                        and  T = 1      if  m .le. n, */
/*                             T = m - n  if  m .gt. n  and  damp = 0, */
/*                             T = m      if  damp .ne. 0. */

/*     ATOL    input      An estimate of the relative error in the data */
/*                        defining the matrix A.  For example, */
/*                        if A is accurate to about 6 digits, set */
/*                        ATOL = 1.0E-6 . */

/*     BTOL    input      An extimate of the relative error in the data */
/*                        defining the rhs vector b.  For example, */
/*                        if b is accurate to about 6 digits, set */
/*                        BTOL = 1.0E-6 . */

/*     CONLIM  input      An upper limit on cond(Abar), the apparent */
/*                        condition number of the matrix Abar. */
/*                        Iterations will be terminated if a computed */
/*                        estimate of cond(Abar) exceeds CONLIM. */
/*                        This is intended to prevent certain small or */
/*                        zero singular values of A or Abar from */
/*                        coming into effect and causing unwanted growth */
/*                        in the computed solution. */

/*                        CONLIM and DAMP may be used separately or */
/*                        together to regularize ill-conditioned systems. */

/*                        Normally, CONLIM should be in the range */
/*                        1000 to 1/RELPR. */
/*                        Suggested value: */
/*                        CONLIM = 1/(100*RELPR)  for compatible systems, */
/*                        CONLIM = 1/(10*sqrt(RELPR)) for least squares. */

/*             Note:  If the user is not concerned about the parameters */
/*             ATOL, BTOL and CONLIM, any or all of them may be set */
/*             to zero.  The effect will be the same as the values */
/*             RELPR, RELPR and 1/RELPR respectively. */

/*     ITNLIM  input      An upper limit on the number of iterations. */
/*                        Suggested value: */
/*                        ITNLIM = n/2   for well-conditioned systems */
/*                                       with clustered singular values, */
/*                        ITNLIM = 4*n   otherwise. */

/*     NOUT    input      File number for printed output.  If positive, */
/*                        a summary will be printed on file NOUT. */

/*     ISTOP   output     An integer giving the reason for termination: */

/*                0       x = 0  is the exact solution. */
/*                        No iterations were performed. */

/*                1       The equations A*x = b are probably */
/*                        compatible.  Norm(A*x - b) is sufficiently */
/*                        small, given the values of ATOL and BTOL. */

/*                2       The system A*x = b is probably not */
/*                        compatible.  A least-squares solution has */
/*                        been obtained that is sufficiently accurate, */
/*                        given the value of ATOL. */

/*                3       An estimate of cond(Abar) has exceeded */
/*                        CONLIM.  The system A*x = b appears to be */
/*                        ill-conditioned.  Otherwise, there could be an */
/*                        error in subroutine APROD. */

/*                4       The equations A*x = b are probably */
/*                        compatible.  Norm(A*x - b) is as small as */
/*                        seems reasonable on this machine. */

/*                5       The system A*x = b is probably not */
/*                        compatible.  A least-squares solution has */
/*                        been obtained that is as accurate as seems */
/*                        reasonable on this machine. */

/*                6       Cond(Abar) seems to be so large that there is */
/*                        no point in doing further iterations, */
/*                        given the precision of this machine. */
/*                        There could be an error in subroutine APROD. */

/*                7       The iteration limit ITNLIM was reached. */

/*     ITN     output     The number of iterations performed. */

/*     ANORM   output     An estimate of the Frobenius norm of  Abar. */
/*                        This is the square-root of the sum of squares */
/*                        of the elements of Abar. */
/*                        If DAMP is small and if the columns of A */
/*                        have all been scaled to have length 1.0, */
/*                        ANORM should increase to roughly sqrt(n). */
/*                        A radically different value for ANORM may */
/*                        indicate an error in subroutine APROD (there */
/*                        may be an inconsistency between modes 1 and 2). */

/*     ACOND   output     An estimate of cond(Abar), the condition */
/*                        number of Abar.  A very high value of ACOND */
/*                        may again indicate an error in APROD. */

/*     RNORM   output     An estimate of the final value of norm(rbar), */
/*                        the function being minimized (see notation */
/*                        above).  This will be small if A*x = b has */
/*                        a solution. */

/*     ARNORM  output     An estimate of the final value of */
/*                        norm( Abar(transpose)*rbar ), the norm of */
/*                        the residual for the usual normal equations. */
/*                        This should be small in all cases.  (ARNORM */
/*                        will often be smaller than the true value */
/*                        computed from the output vector X.) */

/*     XNORM   output     An estimate of the norm of the final */
/*                        solution vector X. */


/*     Subroutines and functions used */
/*     ------------------------------ */

/*     USER               APROD */
/*     BLAS               DCOPY, DNRM2, DSCAL (see Lawson et al. below) */


/*     Precision */
/*     --------- */

/*     The number of iterations required by LSQR will usually decrease */
/*     if the computation is performed in higher precision.  To convert */
/*     LSQR between single and double precision, change the words */
/*                        DOUBLE PRECISION */
/*                        DCOPY, DNRM2, DSCAL */
/*     to the appropriate FORTRAN and BLAS equivalents. */
/*     Also change 'D+' or 'E+' in the PARAMETER statement. */


/*     References */
/*     ---------- */

/*     C.C. Paige and M.A. Saunders,  LSQR: An algorithm for sparse */
/*          linear equations and sparse least squares, */
/*          ACM Transactions on Mathematical Software 8, 1 (March 1982), */
/*          pp. 43-71. */

/*     C.C. Paige and M.A. Saunders,  Algorithm 583, LSQR: Sparse */
/*          linear equations and least-squares problems, */
/*          ACM Transactions on Mathematical Software 8, 2 (June 1982), */
/*          pp. 195-209. */

/*     C.L. Lawson, R.J. Hanson, D.R. Kincaid and F.T. Krogh, */
/*          Basic linear algebra subprograms for Fortran usage, */
/*          ACM Transactions on Mathematical Software 5, 3 (Sept 1979), */
/*          pp. 308-323 and 324-325. */
/* ----------------------------------------------------------------------- */


/*     LSQR development: */
/*     22 Feb 1982: LSQR sent to ACM TOMS to become Algorithm 583. */
/*     15 Sep 1985: Final F66 version.  LSQR sent to "misc" in netlib. */
/*     13 Oct 1987: Bug (Robert Davies, DSIR).  Have to delete */
/*                     IF ( (ONE + DABS(T)) .LE. ONE ) GO TO 200 */
/*                  from loop 200.  The test was an attempt to reduce */
/*                  underflows, but caused W(I) not to be updated. */
/*     17 Mar 1989: First F77 version. */
/*     04 May 1989: Bug (David Gay, AT&T).  When the second BETA is zero, */
/*                  RNORM = 0 and */
/*                  TEST2 = ARNORM / (ANORM * RNORM) overflows. */
/*                  Fixed by testing for RNORM = 0. */
/*     05 May 1989: Sent to "misc" in netlib. */

/*     Michael A. Saunders            (na.saunders @ NA-net.stanford.edu) */
/*     Department of Operations Research */
/*     Stanford University */
/*     Stanford, CA 94305-4022. */
/* ----------------------------------------------------------------------- */
/*     Intrinsics and local variables */
/*<       INTRINSIC          ABS, MOD, SQRT >*/
/*<       INTEGER            I, NCONV, NSTOP >*/
/*<       DOUBLE PRECISION   DNRM2 >*/
/*<    >*/
/*<       DOUBLE PRECISION   ZERO,           ONE >*/
/*<       PARAMETER        ( ZERO = 0.0D+0,  ONE = 1.0D+0 ) >*/
/*      CHARACTER*16       ENTER, EXIT */
/*      CHARACTER*60       MSG(0:7) */
/*      DATA               ENTER /' Enter LSQR.    '/, */
/*     $                   EXIT  /' Exit  LSQR.    '/ */
/*      DATA               MSG */
/*     $ / 'The exact solution is  X = 0', */
/*     $   'Ax - b is small enough, given ATOL, BTOL', */
/*     $   'The least-squares solution is good enough, given ATOL', */
/*     $   'The estimate of cond(Abar) has exceeded CONLIM', */
/*     $   'Ax - b is small enough for this machine', */
/*     $   'The least-squares solution is good enough for this machine', */
/*     $   'Cond(Abar) seems to be too large for this machine', */
/*     $   'The iteration limit has been reached' / */
/* ----------------------------------------------------------------------- */
/*     Initialize. */
/*      IF (NOUT .GT. 0) */
/*     $   WRITE(NOUT, 1000) ENTER, M, N, DAMP, ATOL, CONLIM, BTOL, ITNLIM */
/*<       ITN    =   0 >*/
    /* Parameter adjustments */
    --u;
    --se;
    --x;
    --w;
    --v;
    --iw;
    --rw;

    /* Function Body */
    *itn = 0;
/*<       ISTOP  =   0 >*/
    *istop = 0;
/*<       NSTOP  =   0 >*/
    nstop = 0;
/*<       CTOL   =   ZERO >*/
    ctol = 0.;
/*<       IF (CONLIM .GT. ZERO) CTOL = ONE / CONLIM >*/
    if (*conlim > 0.) {
        ctol = 1. / *conlim;
    }
/*<       ANORM  =   ZERO >*/
    *anorm = 0.;
/*<       ACOND  =   ZERO >*/
    *acond = 0.;
/*<       BBNORM =   ZERO >*/
    bbnorm = 0.;
/*<       DAMPSQ =   DAMP**2 >*/
/* Computing 2nd power */
    d__1 = *damp;
    dampsq = d__1 * d__1;
/*<       DDNORM =   ZERO >*/
    ddnorm = 0.;
/*<       RES2   =   ZERO >*/
    res2 = 0.;
/*<       XNORM  =   ZERO >*/
    *xnorm = 0.;
/*<       XXNORM =   ZERO >*/
    xxnorm = 0.;
/*<       CS2    = - ONE >*/
    cs2 = -1.;
/*<       SN2    =   ZERO >*/
    sn2 = 0.;
/*<       Z      =   ZERO >*/
    z__ = 0.;
/*<       DO 10  I = 1, N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          V(I)  =  ZERO >*/
        v[i__] = 0.;
/*<          X(I)  =  ZERO >*/
        x[i__] = 0.;
/*<         SE(I)  =  ZERO >*/
        se[i__] = 0.;
/*<    10 CONTINUE >*/
/* L10: */
    }
/*     Set up the first vectors U and V for the bidiagonalization. */
/*     These satisfy  BETA*U = b,  ALFA*V = A(transpose)*U. */
/*<       ALFA   =   ZERO >*/
    alfa = 0.;
/*<       BETA   =   DNRM2 ( M, U, 1 ) >*/
    beta = dnrm2_(m, &u[1], &c__1);
/*<       IF (BETA .GT. ZERO) THEN >*/
    if (beta > 0.) {
/*<          CALL DSCAL ( M, (ONE / BETA), U, 1 ) >*/
        d__1 = 1. / beta;
        dscal_(m, &d__1, &u[1], &c__1);
/*<          CALL APROD ( 2, M, N, V, U, LENIW, LENRW, IW, RW ) >*/
        (*aprod)(&c__2, m, n, &v[1], &u[1], leniw, lenrw, &iw[1], &rw[1],
                 userdata);
/*<          ALFA   =   DNRM2 ( N, V, 1 ) >*/
        alfa = dnrm2_(n, &v[1], &c__1);
/*<       END IF >*/
    }
/*<       IF (ALFA .GT. ZERO) THEN >*/
    if (alfa > 0.) {
/*<          CALL DSCAL ( N, (ONE / ALFA), V, 1 ) >*/
        d__1 = 1. / alfa;
        dscal_(n, &d__1, &v[1], &c__1);
/*<          CALL DCOPY ( N, V, 1, W, 1 ) >*/
        dcopy_(n, &v[1], &c__1, &w[1], &c__1);
/*<       END IF >*/
    }
/*<       ARNORM =   ALFA * BETA >*/
    *arnorm = alfa * beta;
/*<       IF (ARNORM .EQ. ZERO) GO TO 800 >*/
    if (*arnorm == 0.) {
        goto L800;
    }
/*<       RHOBAR =   ALFA >*/
    rhobar = alfa;
/*<       PHIBAR =   BETA >*/
    phibar = beta;
/*<       BNORM  =   BETA >*/
    bnorm = beta;
/*<       RNORM  =   BETA >*/
    *rnorm = beta;
/*      IF (NOUT   .GT.  0  ) THEN */
/*         IF (DAMPSQ .EQ. ZERO) THEN */
/*             WRITE(NOUT, 1200) */
/*         ELSE */
/*             WRITE(NOUT, 1300) */
/*         END IF */
/*         TEST1  = ONE */
/*         TEST2  = ALFA / BETA */
/*         WRITE(NOUT, 1500) ITN, X(1), RNORM, TEST1, TEST2 */
/*         WRITE(NOUT, 1600) */
/*      END IF */
/*     ------------------------------------------------------------------ */
/*     Main iteration loop. */
/*     ------------------------------------------------------------------ */
/*<   100 ITN    = ITN + 1 >*/
L100:
    ++(*itn);
/*     Perform the next step of the bidiagonalization to obtain the */
/*     next  BETA, U, ALFA, V.  These satisfy the relations */
/*                BETA*U  =  A*V  -  ALFA*U, */
/*                ALFA*V  =  A(transpose)*U  -  BETA*V. */
/*<       CALL DSCAL ( M, (- ALFA), U, 1 ) >*/
    d__1 = -alfa;
    dscal_(m, &d__1, &u[1], &c__1);
/*<       CALL APROD ( 1, M, N, V, U, LENIW, LENRW, IW, RW ) >*/
    (*aprod)(&c__1, m, n, &v[1], &u[1], leniw, lenrw, &iw[1], &rw[1],
             userdata);
/*<       BETA   =   DNRM2 ( M, U, 1 ) >*/
    beta = dnrm2_(m, &u[1], &c__1);
/*<       BBNORM =   BBNORM  +  ALFA**2  +  BETA**2  +  DAMPSQ >*/
/* Computing 2nd power */
    d__1 = alfa;
/* Computing 2nd power */
    d__2 = beta;
    bbnorm = bbnorm + d__1 * d__1 + d__2 * d__2 + dampsq;
/*<       IF (BETA .GT. ZERO) THEN >*/
    if (beta > 0.) {
/*<          CALL DSCAL ( M, (ONE / BETA), U, 1 ) >*/
        d__1 = 1. / beta;
        dscal_(m, &d__1, &u[1], &c__1);
/*<          CALL DSCAL ( N, (- BETA), V, 1 ) >*/
        d__1 = -beta;
        dscal_(n, &d__1, &v[1], &c__1);
/*<          CALL APROD ( 2, M, N, V, U, LENIW, LENRW, IW, RW ) >*/
        (*aprod)(&c__2, m, n, &v[1], &u[1], leniw, lenrw, &iw[1], &rw[1],
                 userdata);
/*<          ALFA   =   DNRM2 ( N, V, 1 ) >*/
        alfa = dnrm2_(n, &v[1], &c__1);
/*<          IF (ALFA .GT. ZERO) THEN >*/
        if (alfa > 0.) {
/*<             CALL DSCAL ( N, (ONE / ALFA), V, 1 ) >*/
            d__1 = 1. / alfa;
            dscal_(n, &d__1, &v[1], &c__1);
/*<          END IF >*/
        }
/*<       END IF >*/
    }
/*     Use a plane rotation to eliminate the damping parameter. */
/*     This alters the diagonal (RHOBAR) of the lower-bidiagonal matrix. */
/*<       RHBAR2 = RHOBAR**2  +  DAMPSQ >*/
/* Computing 2nd power */
    d__1 = rhobar;
    rhbar2 = d__1 * d__1 + dampsq;
/*<       RHBAR1 = SQRT( RHBAR2 ) >*/
    rhbar1 = sqrt(rhbar2);
/*<       CS1    = RHOBAR / RHBAR1 >*/
    cs1 = rhobar / rhbar1;
/*<       SN1    = DAMP   / RHBAR1 >*/
    sn1 = *damp / rhbar1;
/*<       PSI    = SN1 * PHIBAR >*/
    psi = sn1 * phibar;
/*<       PHIBAR = CS1 * PHIBAR >*/
    phibar = cs1 * phibar;
/*     Use a plane rotation to eliminate the subdiagonal element (BETA) */
/*     of the lower-bidiagonal matrix, giving an upper-bidiagonal matrix. */
/*<       RHO    =   SQRT( RHBAR2  +  BETA**2 ) >*/
/* Computing 2nd power */
    d__1 = beta;
    rho = sqrt(rhbar2 + d__1 * d__1);
/*<       CS     =   RHBAR1 / RHO >*/
    cs = rhbar1 / rho;
/*<       SN     =   BETA   / RHO >*/
    sn = beta / rho;
/*<       THETA  =   SN * ALFA >*/
    theta = sn * alfa;
/*<       RHOBAR = - CS * ALFA >*/
    rhobar = -cs * alfa;
/*<       PHI    =   CS * PHIBAR >*/
    phi = cs * phibar;
/*<       PHIBAR =   SN * PHIBAR >*/
    phibar = sn * phibar;
/*<       TAU    =   SN * PHI >*/
    tau = sn * phi;
/*     Update  X, W  and the standard error estimates. */
/*<       T1     =   PHI   / RHO >*/
    t1 = phi / rho;
/*<       T2     = - THETA / RHO >*/
    t2 = -theta / rho;
/*<       T3     =   ONE   / RHO >*/
    t3 = 1. / rho;
/*<       DO 200  I =  1, N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          T      =  W(I) >*/
        t = w[i__];
/*<          X(I)   =  T1*T  +  X(I) >*/
        x[i__] = t1 * t + x[i__];
/*<          W(I)   =  T2*T  +  V(I) >*/
        w[i__] = t2 * t + v[i__];
/*<          T      = (T3*T)**2 >*/
/* Computing 2nd power */
        d__1 = t3 * t;
        t = d__1 * d__1;
/*<          SE(I)  =  T     +  SE(I) >*/
        se[i__] = t + se[i__];
/*<          DDNORM =  T     +  DDNORM >*/
        ddnorm = t + ddnorm;
/*<   200 CONTINUE >*/
/* L200: */
    }
/*     Use a plane rotation on the right to eliminate the */
/*     super-diagonal element (THETA) of the upper-bidiagonal matrix. */
/*     Then use the result to estimate  norm(X). */
/*<       DELTA  =   SN2 * RHO >*/
    delta = sn2 * rho;
/*<       GAMBAR = - CS2 * RHO >*/
    gambar = -cs2 * rho;
/*<       RHS    =   PHI    - DELTA * Z >*/
    rhs = phi - delta * z__;
/*<       ZBAR   =   RHS    / GAMBAR >*/
    zbar = rhs / gambar;
/*<       XNORM  =   SQRT( XXNORM    + ZBAR **2 ) >*/
/* Computing 2nd power */
    d__1 = zbar;
    *xnorm = sqrt(xxnorm + d__1 * d__1);
/*<       GAMMA  =   SQRT( GAMBAR**2 + THETA**2 ) >*/
/* Computing 2nd power */
    d__1 = gambar;
/* Computing 2nd power */
    d__2 = theta;
    gamma = sqrt(d__1 * d__1 + d__2 * d__2);
/*<       CS2    =   GAMBAR / GAMMA >*/
    cs2 = gambar / gamma;
/*<       SN2    =   THETA  / GAMMA >*/
    sn2 = theta / gamma;
/*<       Z      =   RHS    / GAMMA >*/
    z__ = rhs / gamma;
/*<       XXNORM =   XXNORM + Z**2 >*/
/* Computing 2nd power */
    d__1 = z__;
    xxnorm += d__1 * d__1;
/*     Test for convergence. */
/*     First, estimate the norm and condition of the matrix  Abar, */
/*     and the norms of  rbar  and  Abar(transpose)*rbar. */
/*<       ANORM  =   SQRT( BBNORM ) >*/
    *anorm = sqrt(bbnorm);
/*<       ACOND  =   ANORM * SQRT( DDNORM ) >*/
    *acond = *anorm * sqrt(ddnorm);
/*<       RES1   =   PHIBAR**2 >*/
/* Computing 2nd power */
    d__1 = phibar;
    res1 = d__1 * d__1;
/*<       RES2   =   RES2  +  PSI**2 >*/
/* Computing 2nd power */
    d__1 = psi;
    res2 += d__1 * d__1;
/*<       RNORM  =   SQRT( RES1 + RES2 ) >*/
    *rnorm = sqrt(res1 + res2);
/*<       ARNORM =   ALFA  * ABS( TAU ) >*/
    *arnorm = alfa * abs(tau);
/*     Now use these norms to estimate certain other quantities, */
/*     some of which will be small near a solution. */
/*<       TEST1  =   RNORM /  BNORM >*/
    test1 = *rnorm / bnorm;
/*<       TEST2  =   ZERO >*/
    test2 = 0.;
/*<       IF (RNORM .GT. ZERO) TEST2 = ARNORM / (ANORM * RNORM) >*/
    if (*rnorm > 0.) {
        test2 = *arnorm / (*anorm * *rnorm);
    }
/*<       TEST3  =   ONE   /  ACOND >*/
    test3 = 1. / *acond;
/*<       T1     =   TEST1 / (ONE  +  ANORM * XNORM / BNORM) >*/
    t1 = test1 / (*anorm * *xnorm / bnorm + 1.);
/*<       RTOL   =   BTOL  +  ATOL *  ANORM * XNORM / BNORM >*/
    rtol = *btol + *atol * *anorm * *xnorm / bnorm;
/*     The following tests guard against extremely small values of */
/*     ATOL, BTOL  or  CTOL.  (The user may have set any or all of */
/*     the parameters  ATOL, BTOL, CONLIM  to zero.) */
/*     The effect is equivalent to the normal tests using */
/*     ATOL = RELPR,  BTOL = RELPR,  CONLIM = 1/RELPR. */
/*<       T3     =   ONE + TEST3 >*/
    t3 = test3 + 1.;
/*<       T2     =   ONE + TEST2 >*/
    t2 = test2 + 1.;
/*<       T1     =   ONE + T1 >*/
    t1 += 1.;
/*<       IF (ITN .GE. ITNLIM) ISTOP = 7 >*/
    if (*itn >= *itnlim) {
        *istop = 7;
    }
/*<       IF (T3  .LE. ONE   ) ISTOP = 6 >*/
    if (t3 <= 1.) {
        *istop = 6;
    }
/*<       IF (T2  .LE. ONE   ) ISTOP = 5 >*/
    if (t2 <= 1.) {
        *istop = 5;
    }
/*<       IF (T1  .LE. ONE   ) ISTOP = 4 >*/
    if (t1 <= 1.) {
        *istop = 4;
    }
/*     Allow for tolerances set by the user. */
/*<       IF (TEST3 .LE. CTOL) ISTOP = 3 >*/
    if (test3 <= ctol) {
        *istop = 3;
    }
/*<       IF (TEST2 .LE. ATOL) ISTOP = 2 >*/
    if (test2 <= *atol) {
        *istop = 2;
    }
/*<       IF (TEST1 .LE. RTOL) ISTOP = 1 >*/
    if (test1 <= rtol) {
        *istop = 1;
    }
/*     ================================================================== */
/*     See if it is time to print something. */
/*<       IF (NOUT  .LE.  0       ) GO TO 600 >*/
    if (*nout <= 0) {
        goto L600;
    }
/*<       IF (N     .LE. 40       ) GO TO 400 >*/
    if (*n <= 40) {
        goto L400;
    }
/*<       IF (ITN   .LE. 10       ) GO TO 400 >*/
    if (*itn <= 10) {
        goto L400;
    }
/*<       IF (ITN   .GE. ITNLIM-10) GO TO 400 >*/
    if (*itn >= *itnlim - 10) {
        goto L400;
    }
/*<       IF (MOD(ITN,10) .EQ. 0  ) GO TO 400 >*/
    if (*itn % 10 == 0) {
        goto L400;
    }
/*<       IF (TEST3 .LE.  2.0*CTOL) GO TO 400 >*/
    if (test3 <= ctol * (float)2.) {
        goto L400;
    }
/*<       IF (TEST2 .LE. 10.0*ATOL) GO TO 400 >*/
    if (test2 <= *atol * (float)10.) {
        goto L400;
    }
/*<       IF (TEST1 .LE. 10.0*RTOL) GO TO 400 >*/
    if (test1 <= rtol * (float)10.) {
        goto L400;
    }
/*<       IF (ISTOP .NE.  0       ) GO TO 400 >*/
    if (*istop != 0) {
        goto L400;
    }
/*<       GO TO 600 >*/
    goto L600;
/*     Print a line for this iteration. */
/*<   400 IF (1 .EQ. 1) GO TO 600 >*/
L400:
    if (TRUE_) {
        goto L600;
    }
/*  400 WRITE(NOUT, 1500) ITN, X(1), RNORM, TEST1, TEST2, ANORM, ACOND */
/*      IF (MOD(ITN,10) .EQ. 0) WRITE(NOUT, 1600) */
/*     ================================================================== */
/*     Stop if appropriate. */
/*     The convergence criteria are required to be met on  NCONV */
/*     consecutive iterations, where  NCONV  is set below. */
/*     Suggested value:  NCONV = 1, 2  or  3. */
/*<   600 IF (ISTOP .EQ. 0) NSTOP = 0 >*/
L600:
    if (*istop == 0) {
        nstop = 0;
    }
/*<       IF (ISTOP .EQ. 0) GO TO 100 >*/
    if (*istop == 0) {
        goto L100;
    }
/*<       NCONV  =   1 >*/
    nconv = 1;
/*<       NSTOP  =   NSTOP + 1 >*/
    ++nstop;
/*<       IF (NSTOP .LT. NCONV  .AND.  ITN .LT. ITNLIM) ISTOP = 0 >*/
    if (nstop < nconv && *itn < *itnlim) {
        *istop = 0;
    }
/*<       IF (ISTOP .EQ. 0) GO TO 100 >*/
    if (*istop == 0) {
        goto L100;
    }
/*     ------------------------------------------------------------------ */
/*     End of iteration loop. */
/*     ------------------------------------------------------------------ */
/*     Finish off the standard error estimates. */
/*<       T    =   ONE >*/
    t = 1.;
/*<       IF (M      .GT.   N )  T = M - N >*/
    if (*m > *n) {
        t = (doublereal) (*m - *n);
    }
/*<       IF (DAMPSQ .GT. ZERO)  T = M >*/
    if (dampsq > 0.) {
        t = (doublereal) (*m);
    }
/*<       T    =   RNORM / SQRT( T ) >*/
    t = *rnorm / sqrt(t);
/*<       DO 700  I = 1, N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          SE(I)  = T * SQRT( SE(I) ) >*/
        se[i__] = t * sqrt(se[i__]);
/*<   700 CONTINUE >*/
/* L700: */
    }
/*     Print the stopping condition. */
/*<   800 IF (1 .EQ. 1) GO TO 900 >*/
L800:
    if (TRUE_) {
        goto L900;
    }
/*  800 IF (NOUT .GT. 0) THEN */
/*         WRITE(NOUT, 2000) EXIT, ISTOP, ITN, */
/*     $                     EXIT, ANORM, ACOND, */
/*     $                     EXIT, RNORM, ARNORM, */
/*     $                     EXIT, BNORM, XNORM */
/*         WRITE(NOUT, 3000) EXIT, MSG(ISTOP) */
/*      END IF */
/*<   900 RETURN >*/
L900:
    return 0;
/*     ------------------------------------------------------------------ */
/*<  1 >*/
/* L1000: */
/*<  1 >*/
/* L1200: */
/*<  1 >*/
/* L1300: */
/*<  1500 FORMAT(1P, I6, 2E17.9, 4E10.2) >*/
/* L1500: */
/*<  1600 FORMAT(1X) >*/
/* L1600: */
/*<  2 >*/
/* L2000: */
/*<  3000 FORMAT( A, 6X, A ) >*/
/* L3000: */
/*     ------------------------------------------------------------------ */
/*     End of LSQR */
/*<       END >*/
} /* lsqr_ */

#ifdef __cplusplus
        }
#endif
