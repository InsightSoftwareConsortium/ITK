/* lsqr-test.f -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__600 = 600;
static doublereal c_b53 = -1.;
static integer c__2 = 2;
static integer c__40 = 40;
static integer c__4 = 4;
static integer c__80 = 80;

/*     ****************************************************** */

/*     WARNING.  Delete the following imitation BLAS routines */
/*               if a genuine BLAS library is available. */

/*     ****************************************************** */

/*      SUBROUTINE DCOPY ( N,X,INCX,Y,INCY ) */
/*      INTEGER            N,INCX,INCY */
/*      DOUBLE PRECISION   X(N),Y(N) */

/*     This may be replaced by the corresponding  BLAS  routine. */
/*     The following is a simple version for use with  LSQR. */

/*      DO 10 I = 1, N */
/*         Y(I) = X(I) */
/*   10 CONTINUE */
/*      RETURN */

/*     END OF DCOPY */
/*      END */
/*      DOUBLE PRECISION   FUNCTION DNRM2 ( N,X,INCX ) */
/*      INTEGER            N,INCX */
/*      DOUBLE PRECISION   X(N) */

/*     This may be replaced by the corresponding  BLAS  routine. */
/*     The following is a simple version for use with  LSQR. */

/*      INTEGER            I */
/*      DOUBLE PRECISION   D, DSQRT */

/*      D     = 0.0 */
/*      DO 10 I = 1, N */
/*         D    = D + X(I)**2 */
/*   10 CONTINUE */
/*      DNRM2 = DSQRT(D) */
/*      RETURN */

/*     END OF DNRM2 */
/*      END */
/*      SUBROUTINE DSCAL ( N,A,X,INCX ) */
/*      INTEGER            N,INCX */
/*      DOUBLE PRECISION   A,X(N) */

/*     This may be replaced by the corresponding  BLAS  routine. */
/*     The following is a simple version for use with  LSQR. */

/*      DO 10 I = 1, N */
/*         X(I) = A*X(I) */
/*   10 CONTINUE */
/*      RETURN */

/*     END OF DSCAL */
/*      END */
/* ******************************************************** */

/*     These routines are for testing  LSQR. */

/* ******************************************************** */
/* Subroutine */ int aprod_(mode, m, n, x, y, leniw, lenrw, iw, rw)
integer *mode, *m, *n;
doublereal *x, *y;
integer *leniw, *lenrw, *iw;
doublereal *rw;
{
    static integer locd, locw, lochy, lochz;
    extern /* Subroutine */ int aprod1_(), aprod2_();

/*     ------------------------------------------------------------------
*/
/*     This is the matrix-vector product routine required by  LSQR */
/*     for a test matrix of the form  A = HY*D*HZ.  The quantities */
/*     defining D, HY, HZ are in the work array RW, followed by a */
/*     work array W.  These are passed to APROD1 and APROD2 in order to */
/*     make the code readable. */
/*     ------------------------------------------------------------------
*/
    /* Parameter adjustments */
    --rw;
    --iw;
    --y;
    --x;

    /* Function Body */
    locd = 1;
    lochy = locd + *n;
    lochz = lochy + *m;
    locw = lochz + *n;
    if (*mode == 1) {
        aprod1_(m, n, &x[1], &y[1], &rw[locd], &rw[lochy], &rw[lochz], &rw[
                locw]);
    }
    if (*mode != 1) {
        aprod2_(m, n, &x[1], &y[1], &rw[locd], &rw[lochy], &rw[lochz], &rw[
                locw]);
    }
/*     End of APROD */
} /* aprod_ */

/* Subroutine */ int aprod1_(m, n, x, y, d, hy, hz, w)
integer *m, *n;
doublereal *x, *y, *d, *hy, *hz, *w;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i;
    extern /* Subroutine */ int hprod_();

/*     ------------------------------------------------------------------
*/
/*     APROD1  computes  Y = Y + A*X  for subroutine APROD, */
/*     where A is a test matrix of the form  A = HY*D*HZ, */
/*     and the latter matrices HY, D, HZ are represented by */
/*     input vectors with the same name. */
/*     ------------------------------------------------------------------
*/
    /* Parameter adjustments */
    --w;
    --hz;
    --hy;
    --d;
    --y;
    --x;

    /* Function Body */
    hprod_(n, &hz[1], &x[1], &w[1]);
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        w[i] = d[i] * w[i];
/* L100: */
    }
    i__1 = *m;
    for (i = *n + 1; i <= i__1; ++i) {
        w[i] = 0.;
/* L200: */
    }
    hprod_(m, &hy[1], &w[1], &w[1]);
    i__1 = *m;
    for (i = 1; i <= i__1; ++i) {
        y[i] += w[i];
/* L600: */
    }
/*     End of APROD1 */
} /* aprod1_ */

/* Subroutine */ int aprod2_(m, n, x, y, d, hy, hz, w)
integer *m, *n;
doublereal *x, *y, *d, *hy, *hz, *w;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i;
    extern /* Subroutine */ int hprod_();

/*     ------------------------------------------------------------------
*/
/*     APROD2  computes  X = X + A(T)*Y  for subroutine APROD, */
/*     where  A  is a test matrix of the form  A = HY*D*HZ, */
/*     and the latter matrices  HY, D, HZ  are represented by */
/*     input vectors with the same name. */
/*     ------------------------------------------------------------------
*/
    /* Parameter adjustments */
    --w;
    --hz;
    --hy;
    --d;
    --y;
    --x;

    /* Function Body */
    hprod_(m, &hy[1], &y[1], &w[1]);
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        w[i] = d[i] * w[i];
/* L100: */
    }
    hprod_(n, &hz[1], &w[1], &w[1]);
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        x[i] += w[i];
/* L600: */
    }
/*     End of APROD2 */
} /* aprod2_ */

/* Subroutine */ int hprod_(n, hz, x, y)
integer *n;
doublereal *hz, *x, *y;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i;
    static doublereal s;

/*     ------------------------------------------------------------------
*/
/*     HPROD  applies a Householder transformation stored in  HZ */
/*     to get  Y = ( I - 2*HZ*HZ(transpose) ) * X. */
/*     ------------------------------------------------------------------
*/
    /* Parameter adjustments */
    --y;
    --x;
    --hz;

    /* Function Body */
    s = (float)0.;
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        s = hz[i] * x[i] + s;
/* L100: */
    }
    s += s;
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        y[i] = x[i] - s * hz[i];
/* L200: */
    }
/*     End of HPROD */
} /* hprod_ */

/* Subroutine */ int lstp_(m, n, nduplc, npower, damp, x, b, d, hy, hz, w,
        acond, rnorm)
integer *m, *n, *nduplc, *npower;
doublereal *damp, *x, *b, *d, *hy, *hz, *w, *acond, *rnorm;
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sin(), cos(), pow_di(), sqrt();

    /* Local variables */
    static doublereal alfa, beta;
    extern doublereal dnrm2_();
    static integer i, j;
    static doublereal t;
    extern /* Subroutine */ int dscal_(), hprod_(), aprod1_();
    static doublereal dampsq, fourpi;

/*     ------------------------------------------------------------------
*/
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
/*     ------------------------------------------------------------------
*/
/*     Intrinsics and local variables */
/*     ------------------------------------------------------------------
*/
/*     Make two vectors of norm 1.0 for the Householder transformations.
*/
/*     FOURPI  need not be exact. */
/*     ------------------------------------------------------------------
*/
    /* Parameter adjustments */
    --w;
    --hz;
    --hy;
    --d;
    --b;
    --x;

    /* Function Body */
/* Computing 2nd power */
    d__1 = *damp;
    dampsq = d__1 * d__1;
    fourpi = (float)12.566368000000001;
    alfa = fourpi / *m;
    beta = fourpi / *n;
    i__1 = *m;
    for (i = 1; i <= i__1; ++i) {
        hy[i] = sin(i * alfa);
/* L100: */
    }
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        hz[i] = cos(i * beta);
/* L200: */
    }
    alfa = dnrm2_(m, &hy[1], &c__1);
    beta = dnrm2_(n, &hz[1], &c__1);
    d__1 = -1. / alfa;
    dscal_(m, &d__1, &hy[1], &c__1);
    d__1 = -1. / beta;
    dscal_(n, &d__1, &hz[1], &c__1);

/*     ------------------------------------------------------------------
*/
/*     Set the diagonal matrix  D.  These are the singular values of  A.
*/
/*     ------------------------------------------------------------------
*/
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        j = (i - 1 + *nduplc) / *nduplc;
        t = (doublereal) (j * *nduplc);
        t /= *n;
        d[i] = pow_di(&t, npower);
/* L300: */
    }
/* Computing 2nd power */
    d__1 = d[*n];
/* Computing 2nd power */
    d__2 = d[1];
    *acond = sqrt((d__1 * d__1 + dampsq) / (d__2 * d__2 + dampsq));
/*     ------------------------------------------------------------------
*/
/*     Compute the residual vector, storing it in  B. */
/*     It takes the form  HY*( s ) */
/*                           ( t ) */
/*     where  s  is obtained from  D*s = DAMP**2 * HZ * X */
/*     and    t  can be anything. */
/*     ------------------------------------------------------------------
*/
    hprod_(n, &hz[1], &x[1], &b[1]);
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        b[i] = dampsq * b[i] / d[i];
/* L500: */
    }
    t = 1.;
    i__1 = *m;
    for (i = *n + 1; i <= i__1; ++i) {
        j = i - *n;
        b[i] = t * j / *m;
        t = -t;
/* L600: */
    }
    hprod_(m, &hy[1], &b[1], &b[1]);
/*     ------------------------------------------------------------------
*/
/*     Now compute the true  B  =  RESIDUAL  +  A*X. */
/*     ------------------------------------------------------------------
*/
/* Computing 2nd power */
    d__1 = dnrm2_(m, &b[1], &c__1);
/* Computing 2nd power */
    d__2 = dnrm2_(n, &x[1], &c__1);
    *rnorm = sqrt(d__1 * d__1 + dampsq * (d__2 * d__2));
    aprod1_(m, n, &x[1], &b[1], &d[1], &hy[1], &hz[1], &w[1]);
/*     End of LSTP */
} /* lstp_ */

/* Subroutine */ int test_(m, n, nduplc, npower, damp)
integer *m, *n, *nduplc, *npower;
doublereal *damp;
{
    /* Initialized data */

    static char line[34+1] = "----------------------------------";

    /* Format strings */
    static char fmt_1000[] = "(1p//1x,2a/\002 Least-Squares Test Problem    \
  P(\002,4i5,e12.2,\002 )\002//\002 Condition no. =\002,e12.4,\002     Resid\
ual function =\002,e17.9/1x,2a)";
    static char fmt_2000[] = "(//22x,\002 Residual norm    Residual norm    \
Solution norm\002/22x,\002(Abar X - bbar)   (Normal eqns)         (X)\002/)";
    static char fmt_2100[] = "(1p,\002 Estimated by LSQR\002,3e17.5)";
    static char fmt_2200[] = "(1p,\002 Computed from  X \002,3e17.5)";
    static char fmt_2500[] = "(//\002 Solution  X\002/4(i6,g14.6))";
    static char fmt_2600[] = "(/\002 Standard errors  SE\002/4(i6,g14.6))";
    static char fmt_3000[] = "(1p/\002 LSQR  appears to be successful.\002\
,\002     Relative error in  X  =\002,e10.2)";
    static char fmt_3100[] = "(1p/\002 LSQR  appears to have failed.  \002\
,\002     Relative error in  X  =\002,e10.2)";
    static char fmt_9000[] = "(/\002 XXX  Insufficient workspace.\002,\002  \
The length of  RW  should be at least\002,i6)";

    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Builtin functions */
    integer s_wsfe(), do_fio(), e_wsfe();
    double sqrt();

    /* Local variables */
    static integer locd;
    static doublereal atol, btol, etol;
    static integer locw;
    extern /* Subroutine */ int lsqr_(), lstp_();
    static integer nout;
    extern doublereal dnrm2_();
    static doublereal b[200];
    static integer j;
    static doublereal acond, u[200], v[100], w[100], x[100];
    extern /* Subroutine */ int dscal_(), aprod_();
    static doublereal anorm;
    static integer lochy;
    static doublereal enorm;
    static integer lochz;
    extern /* Subroutine */ int dcopy_();
    static doublereal rnorm;
    static integer istop;
    static doublereal xnorm, xtrue[100], se[100];
    static integer iw[1];
    static doublereal rw[600], conlim, dampsq;
    static integer itnlim;
    static doublereal arnorm;
    static integer ltotal, itn;

    /* Fortran I/O blocks */
    static cilist io___34 = { 0, 0, 0, fmt_1000, 0 };
    static cilist io___46 = { 0, 0, 0, fmt_2000, 0 };
    static cilist io___47 = { 0, 0, 0, fmt_2100, 0 };
    static cilist io___48 = { 0, 0, 0, fmt_2200, 0 };
    static cilist io___49 = { 0, 0, 0, fmt_2500, 0 };
    static cilist io___50 = { 0, 0, 0, fmt_2600, 0 };
    static cilist io___53 = { 0, 0, 0, fmt_3000, 0 };
    static cilist io___54 = { 0, 0, 0, fmt_3100, 0 };
    static cilist io___55 = { 0, 0, 0, fmt_9000, 0 };


/*     ------------------------------------------------------------------
*/
/*     This is an example driver routine for running LSQR. */
/*     It generates a test problem, solves it, and examines the results.
*/
/*     Note that subroutine APROD must be declared EXTERNAL */
/*     if it is used only in the call to LSQR. */


/*     Functions and subroutines */

/*     TESTPROB           APROD */
/*     BLAS               DCOPY, DNRM2, DSCAL */
/*     ------------------------------------------------------------------
*/
/*     Intrinsics and local variables */
/*     Set the desired solution  XTRUE. */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
        xtrue[j - 1] = (doublereal) (*n - j);
/* L100: */
    }
/*     Generate the specified test problem. */
/*     The workspace array  IW  is not needed in this application. */
/*     The workspace array  RW  is used for the following vectors: */
/*     D(N), HY(M), HZ(N), W(MAX(M,N)). */
/*     The vectors  D, HY, HZ  will define the test matrix A. */
/*     W is needed for workspace in APROD1 and APROD2. */
    locd = 1;
    lochy = locd + *n;
    lochz = lochy + *m;
    locw = lochz + *n;
    ltotal = locw + max(*m,*n) - 1;
    if (ltotal > 600) {
        goto L900;
    }
    lstp_(m, n, nduplc, npower, damp, xtrue, b, &rw[locd - 1], &rw[lochy - 1],
             &rw[lochz - 1], &rw[locw - 1], &acond, &rnorm);
/*     Solve the problem defined by APROD, DAMP and B. */
/*     Copy the rhs vector B into U  (LSQR will overwrite U) */
/*     and set the other input parameters for LSQR. */
    dcopy_(m, b, &c__1, u, &c__1);
    atol = (float)1e-10;
    btol = atol;
    conlim = acond * (float)10.;
    itnlim = *m + *n + 50;
    nout = 6;
    io___34.ciunit = nout;
    s_wsfe(&io___34);
    do_fio(&c__1, line, 34L);
    do_fio(&c__1, line, 34L);
    do_fio(&c__1, (char *)&(*m), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&(*n), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&(*nduplc), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&(*npower), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&(*damp), (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&acond, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&rnorm, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, line, 34L);
    do_fio(&c__1, line, 34L);
    e_wsfe();
    lsqr_(m, n, aprod_, damp, &c__1, &c__600, iw, rw, u, v, w, x, se, &atol, &
            btol, &conlim, &itnlim, &nout, &istop, &itn, &anorm, &acond, &
            rnorm, &arnorm, &xnorm);
/*     Examine the results. */
/*     We print the residual norms  RNORM  and  ARNORM  given by LSQR, */
/*     and then compute their true values in terms of the solution  X */
/*     obtained by  LSQR.  At least one of them should be small. */
/* Computing 2nd power */
    d__1 = *damp;
    dampsq = d__1 * d__1;
    io___46.ciunit = nout;
    s_wsfe(&io___46);
    e_wsfe();
    io___47.ciunit = nout;
    s_wsfe(&io___47);
    do_fio(&c__1, (char *)&rnorm, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&arnorm, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&xnorm, (ftnlen)sizeof(doublereal));
    e_wsfe();
/*     Compute  U = A*X - B. */
/*     This is the negative of the usual residual vector. */
/*     It will be close to zero only if  B  is a compatible rhs */
/*     and  X  is close to a solution. */
    dcopy_(m, b, &c__1, u, &c__1);
    dscal_(m, &c_b53, u, &c__1);
    aprod_(&c__1, m, n, x, u, &c__1, &c__600, iw, rw);
/*     Compute  V = A(transpose)*U  +  DAMP**2 * X. */
/*     This will be close to zero in all cases */
/*     if  X  is close to a solution. */
    dcopy_(n, x, &c__1, v, &c__1);
    dscal_(n, &dampsq, v, &c__1);
    aprod_(&c__2, m, n, v, u, &c__1, &c__600, iw, rw);
/*     Compute the norms associated with  X, U, V. */
    xnorm = dnrm2_(n, x, &c__1);
/* Computing 2nd power */
    d__1 = dnrm2_(m, u, &c__1);
/* Computing 2nd power */
    d__2 = xnorm;
    rnorm = sqrt(d__1 * d__1 + dampsq * (d__2 * d__2));
    arnorm = dnrm2_(n, v, &c__1);
    io___48.ciunit = nout;
    s_wsfe(&io___48);
    do_fio(&c__1, (char *)&rnorm, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&arnorm, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&xnorm, (ftnlen)sizeof(doublereal));
    e_wsfe();
/*     Print the solution and standard error estimates from  LSQR. */
    io___49.ciunit = nout;
    s_wsfe(&io___49);
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
        do_fio(&c__1, (char *)&j, (ftnlen)sizeof(integer));
        do_fio(&c__1, (char *)&x[j - 1], (ftnlen)sizeof(doublereal));
    }
    e_wsfe();
    io___50.ciunit = nout;
    s_wsfe(&io___50);
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
        do_fio(&c__1, (char *)&j, (ftnlen)sizeof(integer));
        do_fio(&c__1, (char *)&se[j - 1], (ftnlen)sizeof(doublereal));
    }
    e_wsfe();
/*     Print a clue about whether the solution looks OK. */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
        w[j - 1] = x[j - 1] - xtrue[j - 1];
/* L500: */
    }
    enorm = dnrm2_(n, w, &c__1) / (dnrm2_(n, xtrue, &c__1) + 1.);
    etol = 1e-5;
    if (enorm <= etol) {
        io___53.ciunit = nout;
        s_wsfe(&io___53);
        do_fio(&c__1, (char *)&enorm, (ftnlen)sizeof(doublereal));
        e_wsfe();
    }
    if (enorm > etol) {
        io___54.ciunit = nout;
        s_wsfe(&io___54);
        do_fio(&c__1, (char *)&enorm, (ftnlen)sizeof(doublereal));
        e_wsfe();
    }
    return 0;
/*     Not enough workspace. */
L900:
    io___55.ciunit = nout;
    s_wsfe(&io___55);
    do_fio(&c__1, (char *)&ltotal, (ftnlen)sizeof(integer));
    e_wsfe();
    return 0;
/*     End of TEST */
} /* test_ */

/*     ------------- */
/*     Main program. */
/*     ------------- */
/* Main program */ MAIN__()
{
    /* Builtin functions */
    /* Subroutine */ int s_stop();

    /* Local variables */
    static doublereal zero;
    extern /* Subroutine */ int test_();
    static doublereal damp1, damp2, damp3, damp4;


    zero = (float)0.;
    damp1 = (float).1;
    damp2 = (float).01;
    damp3 = (float).001;
    damp4 = (float)1e-4;
    test_(&c__1, &c__1, &c__1, &c__1, &zero);
    test_(&c__2, &c__1, &c__1, &c__1, &zero);
    test_(&c__40, &c__40, &c__4, &c__4, &zero);
    test_(&c__40, &c__40, &c__4, &c__4, &damp2);
    test_(&c__80, &c__40, &c__4, &c__4, &damp2);
    s_stop("", 0L);
/*     End of main program for testing LSQR */
} /* MAIN__ */

