#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

/* Table of constant values */
static integer c__1 = 1;
static integer c__2 = 2;

/* From arpa!sol-michael.stanford.edu!mike 5 May 89 23:53:00 PDT */
/* Subroutine */ void lsqr_(m, n, aprod, damp, leniw, lenrw, iw, rw, u, v, w,
        x, se, atol, btol, conlim, itnlim, nout, istop, itn, anorm, acond,
        rnorm, arnorm, xnorm)
integer *m, *n;
void (*aprod) (integer*,integer*,integer*,doublereal*,doublereal*,integer*,integer*,integer*,doublereal*);
doublereal *damp;
integer *leniw, *lenrw, *iw;
doublereal *rw, *u, *v, *w, *x, *se, *atol, *btol, *conlim;
integer *itnlim, *nout, *istop, *itn;
doublereal *anorm, *acond, *rnorm, *arnorm, *xnorm;
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    static doublereal alfa, beta, zbar, ctol, rtol;
    static doublereal test1, test2, test3;
    static integer i;
    static doublereal gamma, delta, t, z;
    static doublereal theta, bnorm;
    static integer nconv, nstop;
    static doublereal t1, t2, t3, rhbar1, rhbar2, cs, gambar, sn, phibar,
            rhobar, bbnorm, ddnorm, dampsq, cs1, cs2, sn1, sn2, xxnorm, phi,
            rho, tau, psi, rhs, res1, res2;

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


/*     Note:  LSQR uses an iterative method to approximate the solution.  */
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
/*                        together to regularize ill-conditioned systems.  */

/*                        Normally, CONLIM should be in the range */
/*                        1000 to 1/RELPR. */
/*                        Suggested value: */
/*                        CONLIM = 1/(100*RELPR)  for compatible systems, */
/*                        CONLIM = 1/(10*sqrt(RELPR)) for least squares.  */

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
/*                        may be an inconsistency between modes 1 and 2).  */

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
/*     Initialize. */
/*      IF (NOUT .GT. 0) */
/*     $   WRITE(NOUT, 1000) ENTER, M, N, DAMP, ATOL, CONLIM, BTOL, ITNLIM */
    *itn = 0;
    *istop = 0;
    nstop = 0;
    ctol = 0.;
    if (*conlim > 0.) {
        ctol = 1. / *conlim;
    }
    *anorm = 0.;
    *acond = 0.;
    bbnorm = 0.;
    dampsq = *damp * *damp;
    ddnorm = 0.;
    res2 = 0.;
    *xnorm = 0.;
    xxnorm = 0.;
    cs2 = -1.;
    sn2 = 0.;
    z = 0.;
    for (i = 0; i < *n; ++i) {
        v[i] = 0.;
        x[i] = 0.;
        se[i] = 0.;
    }
/*     Set up the first vectors U and V for the bidiagonalization. */
/*     These satisfy  BETA*U = b,  ALFA*V = A(transpose)*U. */
    alfa = 0.;
    beta = dnrm2_(m, u, &c__1);
    if (beta > 0.) {
        d__1 = 1. / beta;
        dscal_(m, &d__1, u, &c__1);
        (*aprod)(&c__2, m, n, v, u, leniw, lenrw, iw, rw);
        alfa = dnrm2_(n, v, &c__1);
    }
    if (alfa > 0.) {
        d__1 = 1. / alfa;
        dscal_(n, &d__1, v, &c__1);
        dcopy_(n, v, &c__1, w, &c__1);
    }
    *arnorm = alfa * beta;
    if (*arnorm == 0.) {
        goto L800;
    }
    rhobar = alfa;
    phibar = beta;
    bnorm = beta;
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
L100:
    ++(*itn);
/*     Perform the next step of the bidiagonalization to obtain the */
/*     next  BETA, U, ALFA, V.  These satisfy the relations */
/*                BETA*U  =  A*V  -  ALFA*U, */
/*                ALFA*V  =  A(transpose)*U  -  BETA*V. */
    d__1 = -alfa;
    dscal_(m, &d__1, u, &c__1);
    (*aprod)(&c__1, m, n, v, u, leniw, lenrw, iw, rw);
    beta = dnrm2_(m, u, &c__1);
    bbnorm = bbnorm + alfa * alfa + beta * beta + dampsq;
    if (beta > 0.) {
        d__1 = 1. / beta;
        dscal_(m, &d__1, u, &c__1);
        d__1 = -beta;
        dscal_(n, &d__1, v, &c__1);
        (*aprod)(&c__2, m, n, v, u, leniw, lenrw, iw, rw);
        alfa = dnrm2_(n, v, &c__1);
        if (alfa > 0.) {
            d__1 = 1. / alfa;
            dscal_(n, &d__1, v, &c__1);
        }
    }
/*     Use a plane rotation to eliminate the damping parameter. */
/*     This alters the diagonal (RHOBAR) of the lower-bidiagonal matrix.  */
    rhbar2 = rhobar * rhobar + dampsq;
    rhbar1 = sqrt(rhbar2);
    cs1 = rhobar / rhbar1;
    sn1 = *damp / rhbar1;
    psi = sn1 * phibar;
    phibar *= cs1;
/*     Use a plane rotation to eliminate the subdiagonal element (BETA) */
/*     of the lower-bidiagonal matrix, giving an upper-bidiagonal matrix.  */
    rho = sqrt(rhbar2 + beta * beta);
    cs = rhbar1 / rho;
    sn = beta / rho;
    theta = sn * alfa;
    rhobar = -cs * alfa;
    phi = cs * phibar;
    phibar *= sn;
    tau = sn * phi;
/*     Update  X, W  and the standard error estimates. */
    t1 = phi / rho;
    t2 = -theta / rho;
    t3 = 1. / rho;
    for (i = 0; i < *n; ++i) {
        t = w[i];
        x[i] += t1 * t;
        w[i] = t2 * t + v[i];
        t *= t3; t *= t;
        se[i] += t;
        ddnorm += t;
    }
/*     Use a plane rotation on the right to eliminate the */
/*     super-diagonal element (THETA) of the upper-bidiagonal matrix. */
/*     Then use the result to estimate  norm(X). */
    delta = sn2 * rho;
    gambar = -cs2 * rho;
    rhs = phi - delta * z;
    zbar = rhs / gambar;
    *xnorm = sqrt(xxnorm + zbar * zbar);
    gamma = sqrt(gambar * gambar + theta * theta);
    cs2 = gambar / gamma;
    sn2 = theta / gamma;
    z = rhs / gamma;
    xxnorm += z * z;
/*     Test for convergence. */
/*     First, estimate the norm and condition of the matrix  Abar, */
/*     and the norms of  rbar  and  Abar(transpose)*rbar. */
    *anorm = sqrt(bbnorm);
    *acond = *anorm * sqrt(ddnorm);
    res1 = phibar * phibar;
    res2 += psi * psi;
    *rnorm = sqrt(res1 + res2);
    *arnorm = alfa * abs(tau);
/*     Now use these norms to estimate certain other quantities, */
/*     some of which will be small near a solution. */
    test1 = *rnorm / bnorm;
    test2 = 0.;
    if (*rnorm > 0.) {
        test2 = *arnorm / (*anorm * *rnorm);
    }
    test3 = 1. / *acond;
    t1 = test1 / (*anorm * *xnorm / bnorm + 1.);
    rtol = *btol + *atol * *anorm * *xnorm / bnorm;
/*     The following tests guard against extremely small values of */
/*     ATOL, BTOL  or  CTOL.  (The user may have set any or all of */
/*     the parameters  ATOL, BTOL, CONLIM  to zero.) */
/*     The effect is equivalent to the normal tests using */
/*     ATOL = RELPR,  BTOL = RELPR,  CONLIM = 1/RELPR. */
    t3 = test3 + 1.;
    t2 = test2 + 1.;
    t1 += 1.;
    if (*itn >= *itnlim) {
        *istop = 7;
    }
    if (t3 <= 1.) {
        *istop = 6;
    }
    if (t2 <= 1.) {
        *istop = 5;
    }
    if (t1 <= 1.) {
        *istop = 4;
    }
/*     Allow for tolerances set by the user. */
    if (test3 <= ctol) {
        *istop = 3;
    }
    if (test2 <= *atol) {
        *istop = 2;
    }
    if (test1 <= rtol) {
        *istop = 1;
    }
/*     ================================================================== */
/*     See if it is time to print something. */
    if (*nout <= 0) {
        goto L600;
    }
    if (*n <= 40) {
        goto L400;
    }
    if (*itn <= 10) {
        goto L400;
    }
    if (*itn >= *itnlim - 10) {
        goto L400;
    }
    if (*itn % 10 == 0) {
        goto L400;
    }
    if (test3 <= ctol * 2.f) {
        goto L400;
    }
    if (test2 <= *atol * 10.f) {
        goto L400;
    }
    if (test1 <= rtol * 10.f) {
        goto L400;
    }
    if (*istop != 0) {
        goto L400;
    }
    goto L600;
/*     Print a line for this iteration. */
L400:
    if (TRUE_) {
        goto L600;
    }
/*  400 WRITE(NOUT, 1500) ITN, X(1), RNORM, TEST1, TEST2, ANORM, ACOND */
/*  IF (MOD(ITN,10) .EQ. 0) WRITE(NOUT, 1600) */
/*     ================================================================== */
/*     Stop if appropriate. */
/*     The convergence criteria are required to be met on  NCONV */
/*     consecutive iterations, where  NCONV  is set below. */
/*     Suggested value:  NCONV = 1, 2  or  3. */
L600:
    if (*istop == 0) {
        nstop = 0;
    }
    if (*istop == 0) {
        goto L100;
    }
    nconv = 1;
    ++nstop;
    if (nstop < nconv && *itn < *itnlim) {
        *istop = 0;
    }
    if (*istop == 0) {
        goto L100;
    }
/*     ------------------------------------------------------------------ */
/*     End of iteration loop. */
/*     ------------------------------------------------------------------ */
/*     Finish off the standard error estimates. */
    t = 1.;
    if (*m > *n) {
        t = (doublereal) (*m - *n);
    }
    if (dampsq > 0.) {
        t = (doublereal) (*m);
    }
    t = *rnorm / sqrt(t);
    for (i = 0; i < *n; ++i) {
        se[i] = t * sqrt(se[i]);
    }
/*     Print the stopping condition. */
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
L900:
    return;
/*     ------------------------------------------------------------------ */
/*     ------------------------------------------------------------------ */
} /* lsqr_ */
