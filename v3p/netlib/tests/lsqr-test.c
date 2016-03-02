/* tests/lsqr-test.f -- translated by f2c (version 20050501).
   You must link the resulting object file with libf2c:
        on Microsoft Windows system, link with libf2c.lib;
        on Linux or Unix systems, link with .../path/to/libf2c.a -lm
        or, if you install libf2c.a in a standard place, with -lf2c -lm
        -- in that order, at the end of the command line, as in
                cc *.o -lf2c -lm
        Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

                http://www.netlib.org/f2c/libf2c.zip
*/

#include "v3p_netlib.h"

#include <stdio.h>

/* Table of constant values */

static integer c__1 = 1;
static integer c__600 = 600;
static doublereal c_b53 = -1.;
static integer c__2 = 2;
static integer c__40 = 40;
static integer c__4 = 4;
static integer c__80 = 80;

/* ******************************************************** */

/*     These routines are for testing  LSQR. */

/* ******************************************************** */
/*<       SUBROUTINE APROD ( MODE, M, N, X, Y, LENIW, LENRW, IW, RW ) >*/
/* Subroutine */ int aprod_(integer *mode, integer *m, integer *n, doublereal
        *x, doublereal *y, integer *leniw, integer *lenrw, integer *iw,
        doublereal *rw, void* userdata)
{
    integer locd, locw, lochy, lochz;
    extern /* Subroutine */ int aprod1_(integer *, integer *, doublereal *,
            doublereal *, doublereal *, doublereal *, doublereal *,
            doublereal *), aprod2_(integer *, integer *, doublereal *,
            doublereal *, doublereal *, doublereal *, doublereal *,
            doublereal *);
    (void)leniw;
    (void)lenrw;
    (void)userdata;

/*<       INTEGER            MODE, M, N, LENIW, LENRW >*/
/*<       INTEGER            IW(LENIW) >*/
/*<       DOUBLE PRECISION   X(N), Y(M), RW(LENRW) >*/
/*     ------------------------------------------------------------------ */
/*     This is the matrix-vector product routine required by  LSQR */
/*     for a test matrix of the form  A = HY*D*HZ.  The quantities */
/*     defining D, HY, HZ are in the work array RW, followed by a */
/*     work array W.  These are passed to APROD1 and APROD2 in order to */
/*     make the code readable. */
/*     ------------------------------------------------------------------ */
/*<       INTEGER            LOCD, LOCHY, LOCHZ, LOCW >*/
/*<       LOCD   = 1 >*/
    /* Parameter adjustments */
    --y;
    --x;
    --iw;
    --rw;

    /* Function Body */
    locd = 1;
/*<       LOCHY  = LOCD  + N >*/
    lochy = locd + *n;
/*<       LOCHZ  = LOCHY + M >*/
    lochz = lochy + *m;
/*<       LOCW   = LOCHZ + N >*/
    locw = lochz + *n;
/*<    >*/
    if (*mode == 1) {
        aprod1_(m, n, &x[1], &y[1], &rw[locd], &rw[lochy], &rw[lochz], &rw[
                locw]);
    }
/*<    >*/
    if (*mode != 1) {
        aprod2_(m, n, &x[1], &y[1], &rw[locd], &rw[lochy], &rw[lochz], &rw[
                locw]);
    }
/*     End of APROD */
/*<       END >*/
    return 0;
} /* aprod_ */

/*<       SUBROUTINE APROD1( M, N, X, Y, D, HY, HZ, W ) >*/
/* Subroutine */ int aprod1_(integer *m, integer *n, doublereal *x,
        doublereal *y, doublereal *d__, doublereal *hy, doublereal *hz,
        doublereal *w)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__;
    extern /* Subroutine */ int hprod_(integer *, doublereal *, doublereal *,
            doublereal *);

/*<       INTEGER            M, N >*/
/*<       DOUBLE PRECISION   X(N), Y(M), D(N), HY(M), HZ(N), W(M) >*/
/*     ------------------------------------------------------------------ */
/*     APROD1  computes  Y = Y + A*X  for subroutine APROD, */
/*     where A is a test matrix of the form  A = HY*D*HZ, */
/*     and the latter matrices HY, D, HZ are represented by */
/*     input vectors with the same name. */
/*     ------------------------------------------------------------------ */
/*<       INTEGER            I >*/
/*<       DOUBLE PRECISION   ZERO >*/
/*<       PARAMETER        ( ZERO = 0.0 ) >*/
/*<       CALL HPROD ( N, HZ, X, W ) >*/
    /* Parameter adjustments */
    --w;
    --hy;
    --y;
    --hz;
    --d__;
    --x;

    /* Function Body */
    hprod_(n, &hz[1], &x[1], &w[1]);
/*<       DO 100 I = 1, N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          W(I)  = D(I) * W(I) >*/
        w[i__] = d__[i__] * w[i__];
/*<   100 CONTINUE >*/
/* L100: */
    }
/*<       DO 200 I = N + 1, M >*/
    i__1 = *m;
    for (i__ = *n + 1; i__ <= i__1; ++i__) {
/*<          W(I)  = ZERO >*/
        w[i__] = 0.;
/*<   200 CONTINUE >*/
/* L200: */
    }
/*<       CALL HPROD ( M, HY, W, W ) >*/
    hprod_(m, &hy[1], &w[1], &w[1]);
/*<       DO 600 I = 1, M >*/
    i__1 = *m;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          Y(I)  = Y(I) + W(I) >*/
        y[i__] += w[i__];
/*<   600 CONTINUE >*/
/* L600: */
    }
/*     End of APROD1 */
/*<       END >*/
    return 0;
} /* aprod1_ */

/*<       SUBROUTINE APROD2( M, N, X, Y, D, HY, HZ, W ) >*/
/* Subroutine */ int aprod2_(integer *m, integer *n, doublereal *x,
        doublereal *y, doublereal *d__, doublereal *hy, doublereal *hz,
        doublereal *w)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__;
    extern /* Subroutine */ int hprod_(integer *, doublereal *, doublereal *,
            doublereal *);

/*<       INTEGER            M, N >*/
/*<       DOUBLE PRECISION   X(N), Y(M), D(N), HY(M), HZ(N), W(M) >*/
/*     ------------------------------------------------------------------ */
/*     APROD2  computes  X = X + A(T)*Y  for subroutine APROD, */
/*     where  A  is a test matrix of the form  A = HY*D*HZ, */
/*     and the latter matrices  HY, D, HZ  are represented by */
/*     input vectors with the same name. */
/*     ------------------------------------------------------------------ */
/*<       INTEGER            I >*/
/*<       CALL HPROD ( M, HY, Y, W ) >*/
    /* Parameter adjustments */
    --w;
    --hy;
    --y;
    --hz;
    --d__;
    --x;

    /* Function Body */
    hprod_(m, &hy[1], &y[1], &w[1]);
/*<       DO 100 I = 1, N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          W(I)  = D(I)*W(I) >*/
        w[i__] = d__[i__] * w[i__];
/*<   100 CONTINUE >*/
/* L100: */
    }
/*<       CALL HPROD ( N, HZ, W, W ) >*/
    hprod_(n, &hz[1], &w[1], &w[1]);
/*<       DO 600 I = 1, N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          X(I)  = X(I) + W(I) >*/
        x[i__] += w[i__];
/*<   600 CONTINUE >*/
/* L600: */
    }
/*     End of APROD2 */
/*<       END >*/
    return 0;
} /* aprod2_ */

/*<       SUBROUTINE HPROD ( N, HZ, X, Y ) >*/
/* Subroutine */ int hprod_(integer *n, doublereal *hz, doublereal *x,
        doublereal *y)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__;
    doublereal s;

/*<       INTEGER            N >*/
/*<       DOUBLE PRECISION   HZ(N), X(N), Y(N) >*/
/*     ------------------------------------------------------------------ */
/*     HPROD  applies a Householder transformation stored in  HZ */
/*     to get  Y = ( I - 2*HZ*HZ(transpose) ) * X. */
/*     ------------------------------------------------------------------ */
/*<       INTEGER            I >*/
/*<       DOUBLE PRECISION   S >*/
/*<       S      = 0.0 >*/
    /* Parameter adjustments */
    --y;
    --x;
    --hz;

    /* Function Body */
    s = (float)0.;
/*<       DO 100 I = 1, N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          S     = HZ(I) * X(I)  +  S >*/
        s = hz[i__] * x[i__] + s;
/*<   100 CONTINUE >*/
/* L100: */
    }
/*<       S      = S + S >*/
    s += s;
/*<       DO 200 I = 1, N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          Y(I)  = X(I)  -  S * HZ(I) >*/
        y[i__] = x[i__] - s * hz[i__];
/*<   200 CONTINUE >*/
/* L200: */
    }
/*     End of HPROD */
/*<       END >*/
    return 0;
} /* hprod_ */

/*<    >*/
/* Subroutine */ int lstp_(integer *m, integer *n, integer *nduplc, integer *
        npower, doublereal *damp, doublereal *x, doublereal *b, doublereal *
        d__, doublereal *hy, doublereal *hz, doublereal *w, doublereal *acond,
         doublereal *rnorm)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sin(doublereal), cos(doublereal), pow_di(doublereal *, integer *),
            sqrt(doublereal);

    /* Local variables */
    integer i__, j;
    doublereal t, alfa, beta;
    extern doublereal dnrm2_(integer *, doublereal *, integer *);
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *,
            integer *), hprod_(integer *, doublereal *, doublereal *,
            doublereal *), aprod1_(integer *, integer *, doublereal *,
            doublereal *, doublereal *, doublereal *, doublereal *,
            doublereal *);
    doublereal dampsq, fourpi;

/*<       INTEGER            M, N, MAXMN, NDUPLC, NPOWER >*/
/*<       DOUBLE PRECISION   DAMP, ACOND, RNORM >*/
/*<       DOUBLE PRECISION   B(M), X(N), D(N), HY(M), HZ(N), W(M) >*/
/*     ------------------------------------------------------------------ */
/*     LSTP  generates a sparse least-squares test problem of the form */

/*                (   A    )*X = ( B ) */
/*                ( DAMP*I )     ( 0 ) */

/*     having a specified solution X.  The matrix A is constructed */
/*     in the form  A = HY*D*HZ,  where D is an M by N diagonal matrix, */
/*     and HY and HZ are Householder transformations. */

/*     The first 6 parameters are input to LSTP.  The remainder are */
/*     output.  LSTP is intended for use with M .GE. N. */


/*     Functions and subroutines */

/*     TESTPROB           APROD1, HPROD */
/*     BLAS               DNRM2 */
/*     ------------------------------------------------------------------ */
/*     Intrinsics and local variables */
/*<       INTRINSIC          COS,  SIN, SQRT >*/
/*<       INTEGER            I, J >*/
/*<       DOUBLE PRECISION   DNRM2 >*/
/*<       DOUBLE PRECISION   ALFA, BETA, DAMPSQ, FOURPI, T >*/
/*<       DOUBLE PRECISION   ZERO,        ONE >*/
/*<       PARAMETER        ( ZERO = 0.0,  ONE = 1.0 ) >*/
/*     ------------------------------------------------------------------ */
/*     Make two vectors of norm 1.0 for the Householder transformations. */
/*     FOURPI  need not be exact. */
/*     ------------------------------------------------------------------ */
/*<       DAMPSQ = DAMP**2 >*/
    /* Parameter adjustments */
    --w;
    --hy;
    --b;
    --hz;
    --d__;
    --x;

    /* Function Body */
/* Computing 2nd power */
    d__1 = *damp;
    dampsq = d__1 * d__1;
/*<       FOURPI = 4.0 * 3.141592 >*/
    fourpi = (float)12.566368000000001;
/*<       ALFA   = FOURPI / M >*/
    alfa = fourpi / *m;
/*<       BETA   = FOURPI / N >*/
    beta = fourpi / *n;
/*<       DO 100 I = 1, M >*/
    i__1 = *m;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          HY(I) = SIN( I * ALFA ) >*/
        hy[i__] = sin(i__ * alfa);
/*<   100 CONTINUE >*/
/* L100: */
    }
/*<       DO 200 I = 1, N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          HZ(I) = COS( I * BETA ) >*/
        hz[i__] = cos(i__ * beta);
/*<   200 CONTINUE                 >*/
/* L200: */
    }
/*<       ALFA   = DNRM2 ( M, HY, 1 ) >*/
    alfa = dnrm2_(m, &hy[1], &c__1);
/*<       BETA   = DNRM2 ( N, HZ, 1 ) >*/
    beta = dnrm2_(n, &hz[1], &c__1);
/*<       CALL DSCAL ( M, (- ONE / ALFA), HY, 1 ) >*/
    d__1 = -1. / alfa;
    dscal_(m, &d__1, &hy[1], &c__1);
/*<       CALL DSCAL ( N, (- ONE / BETA), HZ, 1 ) >*/
    d__1 = -1. / beta;
    dscal_(n, &d__1, &hz[1], &c__1);

/*     ------------------------------------------------------------------ */
/*     Set the diagonal matrix  D.  These are the singular values of  A. */
/*     ------------------------------------------------------------------ */
/*<       DO 300 I = 1, N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          J     = (I - 1 + NDUPLC) / NDUPLC >*/
        j = (i__ - 1 + *nduplc) / *nduplc;
/*<          T     =  J * NDUPLC >*/
        t = (doublereal) (j * *nduplc);
/*<          T     =  T / N >*/
        t /= *n;
/*<          D(I)  =  T**NPOWER >*/
        d__[i__] = pow_di(&t, npower);
/*<   300 CONTINUE >*/
/* L300: */
    }
/*<       ACOND  = SQRT( (D(N)**2 + DAMPSQ) / (D(1)**2 + DAMPSQ) ) >*/
/* Computing 2nd power */
    d__1 = d__[*n];
/* Computing 2nd power */
    d__2 = d__[1];
    *acond = sqrt((d__1 * d__1 + dampsq) / (d__2 * d__2 + dampsq));
/*     ------------------------------------------------------------------ */
/*     Compute the residual vector, storing it in  B. */
/*     It takes the form  HY*( s ) */
/*                           ( t ) */
/*     where  s  is obtained from  D*s = DAMP**2 * HZ * X */
/*     and    t  can be anything. */
/*     ------------------------------------------------------------------ */
/*<       CALL HPROD ( N, HZ, X, B ) >*/
    hprod_(n, &hz[1], &x[1], &b[1]);
/*<       DO 500 I = 1, N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          B(I)  = DAMPSQ * B(I) / D(I) >*/
        b[i__] = dampsq * b[i__] / d__[i__];
/*<   500 CONTINUE >*/
/* L500: */
    }
/*<       T      = ONE >*/
    t = 1.;
/*<       DO 600 I =   N + 1, M >*/
    i__1 = *m;
    for (i__ = *n + 1; i__ <= i__1; ++i__) {
/*<          J     =   I - N >*/
        j = i__ - *n;
/*<          B(I)  =  (T * J) / M >*/
        b[i__] = t * j / *m;
/*<          T     = - T >*/
        t = -t;
/*<   600 CONTINUE >*/
/* L600: */
    }
/*<       CALL HPROD ( M, HY, B, B ) >*/
    hprod_(m, &hy[1], &b[1], &b[1]);
/*     ------------------------------------------------------------------ */
/*     Now compute the true  B  =  RESIDUAL  +  A*X. */
/*     ------------------------------------------------------------------ */
/*<    >*/
/* Computing 2nd power */
    d__1 = dnrm2_(m, &b[1], &c__1);
/* Computing 2nd power */
    d__2 = dnrm2_(n, &x[1], &c__1);
    *rnorm = sqrt(d__1 * d__1 + dampsq * (d__2 * d__2));
/*<       CALL APROD1( M, N, X, B, D, HY, HZ, W ) >*/
    aprod1_(m, n, &x[1], &b[1], &d__[1], &hy[1], &hz[1], &w[1]);
/*     End of LSTP */
/*<       END >*/
    return 0;
} /* lstp_ */

/*<       SUBROUTINE TEST  ( M, N, NDUPLC, NPOWER, DAMP ) >*/
/* Subroutine */ int test_(integer *m, integer *n, integer *nduplc, integer *
        npower, doublereal *damp)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    doublereal b[200];
    integer j;
    doublereal u[200], v[100], w[100], x[100], se[100];
    integer iw[1];
    doublereal rw[600];
    integer itn, locd;
    doublereal atol, btol, etol;
    integer locw;
    extern int lstp_(integer *, integer *, integer *
            , integer *, doublereal *, doublereal *, doublereal *, doublereal
            *, doublereal *, doublereal *, doublereal *, doublereal *,
            doublereal *);
    integer nout;
    extern doublereal dnrm2_(integer *, doublereal *, integer *);
    doublereal acond;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *,
            integer *), aprod_(integer *, integer *, integer *, doublereal *,
            doublereal *, integer *, integer *, integer *, doublereal *,
            void*);
    doublereal anorm;
    integer lochy;
    doublereal enorm;
    integer lochz;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *,
            doublereal *, integer *);
    doublereal rnorm;
    integer istop;
    doublereal xnorm, xtrue[100], conlim, dampsq;
    integer itnlim;
    doublereal arnorm;
    integer ltotal;

/*<       INTEGER            M, N, NDUPLC, NPOWER >*/
/*<       DOUBLE PRECISION   DAMP >*/
/*     ------------------------------------------------------------------ */
/*     This is an example driver routine for running LSQR. */
/*     It generates a test problem, solves it, and examines the results. */
/*     Note that subroutine APROD must be declared EXTERNAL */
/*     if it is used only in the call to LSQR. */


/*     Functions and subroutines */

/*     TESTPROB           APROD */
/*     BLAS               DCOPY, DNRM2, DSCAL */
/*     ------------------------------------------------------------------ */
/*     Intrinsics and local variables */
/*<       INTRINSIC          MAX, SQRT >*/
/*<       EXTERNAL           APROD >*/
/*<       INTEGER            ISTOP, ITNLIM, J, NOUT >*/
/*<       DOUBLE PRECISION   DNRM2 >*/
/*<       PARAMETER        ( MAXM = 200,  MAXN = 100 ) >*/
/*<    >*/
/*<    >*/
/*<       PARAMETER        ( LENIW = 1,  LENRW = 600 ) >*/
/*<       INTEGER            IW(LENIW) >*/
/*<       DOUBLE PRECISION   RW(LENRW) >*/
/*<       INTEGER            LOCD, LOCHY, LOCHZ, LOCW, LTOTAL >*/
/*<       DOUBLE PRECISION   ONE >*/
/*<       PARAMETER        ( ONE = 1.0 ) >*/
/*<       CHARACTER*34       LINE >*/
/*<    >*/
/*     Set the desired solution  XTRUE. */
/*<       DO 100 J = 1, N >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<          XTRUE(J) = N - J >*/
        xtrue[j - 1] = (doublereal) (*n - j);
/*<   100 CONTINUE >*/
/* L100: */
    }
/*     Generate the specified test problem. */
/*     The workspace array  IW  is not needed in this application. */
/*     The workspace array  RW  is used for the following vectors: */
/*     D(N), HY(M), HZ(N), W(MAX(M,N)). */
/*     The vectors  D, HY, HZ  will define the test matrix A. */
/*     W is needed for workspace in APROD1 and APROD2. */
/*<       LOCD   = 1 >*/
    locd = 1;
/*<       LOCHY  = LOCD  + N >*/
    lochy = locd + *n;
/*<       LOCHZ  = LOCHY + M >*/
    lochz = lochy + *m;
/*<       LOCW   = LOCHZ + N >*/
    locw = lochz + *n;
/*<       LTOTAL = LOCW  + MAX(M,N) - 1 >*/
    ltotal = locw + max(*m,*n) - 1;
/*<       IF (LTOTAL .GT. LENRW) GO TO 900 >*/
    if (ltotal > 600) {
        goto L900;
    }
/*<    >*/
    lstp_(m, n, nduplc, npower, damp, xtrue, b, &rw[locd - 1], &rw[lochy - 1],
             &rw[lochz - 1], &rw[locw - 1], &acond, &rnorm);
/*     Solve the problem defined by APROD, DAMP and B. */
/*     Copy the rhs vector B into U  (LSQR will overwrite U) */
/*     and set the other input parameters for LSQR. */
/*<       CALL DCOPY ( M, B, 1, U, 1 ) >*/
    dcopy_(m, b, &c__1, u, &c__1);
/*<       ATOL   = 1.0E-10 >*/
    atol = (float)1e-10;
/*<       BTOL   = ATOL >*/
    btol = atol;
/*<       CONLIM = 10.0 * ACOND >*/
    conlim = acond * (float)10.;
/*<       ITNLIM = M + N + 50 >*/
    itnlim = *m + *n + 50;
/*<       NOUT   = 6 >*/
    nout = 6;
/*<    >*/
    printf("\n\n --------------------------------------------------------------------\n");
    printf(" Least-Squares Test Problem      P( %ld %ld %ld %ld %12.2e )\n\n", *m,*n,*nduplc,*npower,*damp);
    printf(" Condition no. =%12.4e     Residual function =%17.9e\n", acond, rnorm);
    printf(" --------------------------------------------------------------------\n");
/*<    >*/
    lsqr_(m, n, aprod_, damp, &c__1, &c__600, iw, rw, u, v, w, x, se, &
            atol, &btol, &conlim, &itnlim, &nout, &istop, &itn, &anorm, &
            acond, &rnorm, &arnorm, &xnorm, 0);
/*     Examine the results. */
/*     We print the residual norms  RNORM  and  ARNORM  given by LSQR, */
/*     and then compute their true values in terms of the solution  X */
/*     obtained by  LSQR.  At least one of them should be small. */
/*<       DAMPSQ = DAMP**2 >*/
/* Computing 2nd power */
    d__1 = *damp;
    dampsq = d__1 * d__1;
/*<       WRITE(NOUT, 2000) >*/
/*
 2000 FORMAT(
     $ // 22X, ' Residual norm    Residual norm    Solution norm'
     $  / 22X, '(Abar X - bbar)   (Normal eqns)         (X)' /)
*/
    printf("\n\n                       Residual norm    Residual norm    Solution norm\n");
    printf("                      (Abar X - bbar)   (Normal eqns)         (X)\n");
/*<       WRITE(NOUT, 2100) RNORM, ARNORM, XNORM >*/
/*
 2100 FORMAT(1P, ' Estimated by LSQR', 3E17.5)
*/
    printf(" Estimated by LSQR%17.5e%17.5e%17.5e\n", rnorm, arnorm, xnorm);
/*     Compute  U = A*X - B. */
/*     This is the negative of the usual residual vector. */
/*     It will be close to zero only if  B  is a compatible rhs */
/*     and  X  is close to a solution. */
/*<       CALL DCOPY ( M, B, 1, U, 1 ) >*/
    dcopy_(m, b, &c__1, u, &c__1);
/*<       CALL DSCAL ( M, (-ONE), U, 1 ) >*/
    dscal_(m, &c_b53, u, &c__1);
/*<       CALL APROD ( 1, M, N, X, U, LENIW, LENRW, IW, RW ) >*/
    aprod_(&c__1, m, n, x, u, &c__1, &c__600, iw, rw, 0);
/*     Compute  V = A(transpose)*U  +  DAMP**2 * X. */
/*     This will be close to zero in all cases */
/*     if  X  is close to a solution. */
/*<       CALL DCOPY ( N, X, 1, V, 1 ) >*/
    dcopy_(n, x, &c__1, v, &c__1);
/*<       CALL DSCAL ( N, DAMPSQ, V, 1 ) >*/
    dscal_(n, &dampsq, v, &c__1);
/*<       CALL APROD ( 2, M, N, V, U, LENIW, LENRW, IW, RW ) >*/
    aprod_(&c__2, m, n, v, u, &c__1, &c__600, iw, rw, 0);
/*     Compute the norms associated with  X, U, V. */
/*<       XNORM  = DNRM2 ( N, X, 1 ) >*/
    xnorm = dnrm2_(n, x, &c__1);
/*<       RNORM  = SQRT( DNRM2 ( M, U, 1 )**2  +  DAMPSQ * XNORM**2 ) >*/
/* Computing 2nd power */
    d__1 = dnrm2_(m, u, &c__1);
/* Computing 2nd power */
    d__2 = xnorm;
    rnorm = sqrt(d__1 * d__1 + dampsq * (d__2 * d__2));
/*<       ARNORM = DNRM2 ( N, V, 1 ) >*/
    arnorm = dnrm2_(n, v, &c__1);
/*<       WRITE(NOUT, 2200) RNORM, ARNORM, XNORM >*/
/*
 2200 FORMAT(1P, ' Computed from  X ', 3E17.5)
*/
    printf(" Computed from  X %17.5e%17.5e%17.5e\n", rnorm, arnorm, xnorm);
/*     Print the solution and standard error estimates from  LSQR. */
/*<       WRITE(NOUT, 2500) (J, X(J),  J = 1, N) >*/
/*
 2500 FORMAT(//' Solution  X' / 4(I6, G14.6))
*/
    printf("\n\n Solution  X\n");
    for (j = 0; j < *n; ++j)
        printf("%6ld%14.6g\n", j+1, x[j]);
/*<       WRITE(NOUT, 2600) (J, SE(J), J = 1, N) >*/
/*
 2600 FORMAT(/ ' Standard errors  SE' / 4(I6, G14.6))
*/
    printf("\n\n Standard errors  SE\n");
    for (j = 0; j < *n; ++j)
        printf("%6ld%14.6g\n", j+1, se[j]);
    printf("\n");
/*     Print a clue about whether the solution looks OK. */
/*<       DO 500 J = 1, N >*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/*<          W(J)  = X(J) - XTRUE(J) >*/
        w[j - 1] = x[j - 1] - xtrue[j - 1];
/*<   500 CONTINUE >*/
/* L500: */
    }
/*<       ENORM    = DNRM2 ( N, W, 1 ) / (ONE  +  DNRM2 ( N, XTRUE, 1 )) >*/
    enorm = dnrm2_(n, w, &c__1) / (dnrm2_(n, xtrue, &c__1) + 1.);
/*<       ETOL     = 1.0D-5 >*/
    etol = 1e-5;
/*<       IF (ENORM .LE. ETOL) WRITE(NOUT, 3000) ENORM >*/
/*
 3000 FORMAT(1P / ' LSQR  appears to be successful.',
     $        '     Relative error in  X  =', E10.2)
*/
    if (enorm <= etol) {
        printf("\n LSQR  appears to be successful.     Relative error in  X  =%10.2e\n", enorm);
    }
/*<       IF (ENORM .GT. ETOL) WRITE(NOUT, 3100) ENORM >*/
/*
 3100 FORMAT(1P / ' LSQR  appears to have failed.  ',
     $        '     Relative error in  X  =', E10.2)
*/
    if (enorm > etol) {
        printf("\n LSQR  appears to have failed.       Relative error in  X  =%10.2e\n", enorm);
    }
/*<       RETURN >*/
    return 0;
/*     Not enough workspace. */
/*<   900 WRITE(NOUT, 9000) LTOTAL >*/
/*
 9000 FORMAT(/ ' XXX  Insufficient workspace.',
     $        '  The length of  RW  should be at least', I6)
*/
L900:
    printf("\n XXX  Insufficient workspace."
           "  The length of  RW  should be at least %ld\n", ltotal);
/*<       RETURN >*/
    return 0;
/*<  1 >*/
/*<  2 >*/
/*<  2100 FORMAT(1P, ' Estimated by LSQR', 3E17.5) >*/
/*<  2200 FORMAT(1P, ' Computed from  X ', 3E17.5)  >*/
/*<  2500 FORMAT(//' Solution  X' / 4(I6, G14.6)) >*/
/*<  2600 FORMAT(/ ' Standard errors  SE' / 4(I6, G14.6)) >*/
/*<  3 >*/
/*<  3 >*/
/*<  9 >*/
/*     End of TEST */
/*<       END >*/
} /* test_ */

/*     ------------- */
/*     Main program. */
/*     ------------- */
/*<       DOUBLE PRECISION   DAMP1, DAMP2, DAMP3, DAMP4, ZERO >*/
/* Main program */ int main()
{
    /* Local variables */
    doublereal zero;
    extern /* Subroutine */ int test_(integer *, integer *, integer *,
            integer *, doublereal *);
    doublereal damp1, damp2, damp3, damp4;


/*<       ZERO   = 0.0 >*/
    zero = (float)0.;
/*<       DAMP1  = 0.1 >*/
//    damp1 = (float).1;
/*<       DAMP2  = 0.01 >*/
    damp2 = (float).01;
/*<       DAMP3  = 0.001 >*/
//    damp3 = (float).001;
/*<       DAMP4  = 0.0001 >*/
//    damp4 = (float)1e-4;
/*<       CALL TEST  (  1,  1, 1, 1, ZERO  ) >*/
    test_(&c__1, &c__1, &c__1, &c__1, &zero);
/*<       CALL TEST  (  2,  1, 1, 1, ZERO  ) >*/
    test_(&c__2, &c__1, &c__1, &c__1, &zero);
/*<       CALL TEST  ( 40, 40, 4, 4, ZERO  ) >*/
    test_(&c__40, &c__40, &c__4, &c__4, &zero);
/*<       CALL TEST  ( 40, 40, 4, 4, DAMP2 ) >*/
    test_(&c__40, &c__40, &c__4, &c__4, &damp2);
/*<       CALL TEST  ( 80, 40, 4, 4, DAMP2 ) >*/
    test_(&c__80, &c__40, &c__4, &c__4, &damp2);
/*<       STOP >*/
/*     End of main program for testing LSQR */
/*<       END >*/
    return 0;
} /* MAIN__ */
