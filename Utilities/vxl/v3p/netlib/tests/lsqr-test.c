/* lsqr-test.f -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/
/* Peter Vanroose - May 2002 - re-inserted FORTRAN output format, now using printf() */

#include "../f2c.h"
#include "../netlib.h"
#include <stdio.h>
extern double sin(double), cos(double), sqrt(double);

/* Subroutine */ void lstp_(int*m, int*n, int*nduplc, int*npower, double*damp, double*x, double*b, double*d,
                            double*hy, double*hz, double*w, double*acond, double*rnorm);

/* Subroutine */ void aprod_(int*mode, int*m, int*n, double*x, double*y, int*leniw, int*lenrw, int*iw, double*rw);
/* Subroutine */ void hprod_(int*n, double*hz, double*x, double*y);
/* Subroutine */ void aprod1_(int*m, int*n, double*x, double*y, double*d, double*hy, double*hz, double*w);
/* Subroutine */ void aprod2_(int*m, int*n, double*x, double*y, double*d, double*hy, double*hz, double*w);

/* Subroutine */ void test_(int*m, int*n, int*nduplc, int*npower, double*damp);

/* Table of constant values */
static integer c__1 = 1;
static integer c__600 = 600;
static doublereal c_m1 = -1.;
static integer c__2 = 2;
static integer c__40 = 40;
static integer c__4 = 4;
static integer c__80 = 80;

/*     ****************************************************** */
/*                                                            */
/*     WARNING.  Delete the following imitation BLAS routines */
/*               if a genuine BLAS library is available.      */
/*                                                            */
/*     ****************************************************** */
/*                                                            */
/*      SUBROUTINE DCOPY ( N,X,INCX,Y,INCY )                  */
/*      INTEGER            N,INCX,INCY                        */
/*      DOUBLE PRECISION   X(N),Y(N)                          */
/*                                                            */
/*     This may be replaced by the corresponding BLAS routine.*/
/*     The following is a simple version for use with  LSQR.  */
/*                                                            */
/*      DO 10 I = 1, N                                        */
/*         Y(I) = X(I)                                        */
/*   10 CONTINUE                                              */
/*      RETURN                                                */
/*                                                            */
/*     END OF DCOPY                                           */
/*      END                                                   */
/*      DOUBLE PRECISION   FUNCTION DNRM2 ( N,X,INCX )        */
/*      INTEGER            N,INCX                             */
/*      DOUBLE PRECISION   X(N)                               */
/*                                                            */
/*     This may be replaced by the corresponding BLAS routine.*/
/*     The following is a simple version for use with  LSQR.  */
/*                                                            */
/*      INTEGER            I                                  */
/*      DOUBLE PRECISION   D, DSQRT                           */
/*                                                            */
/*      D     = 0.0                                           */
/*      DO 10 I = 1, N                                        */
/*         D    = D + X(I)**2                                 */
/*   10 CONTINUE                                              */
/*      DNRM2 = DSQRT(D)                                      */
/*      RETURN                                                */
/*                                                            */
/*     END OF DNRM2                                           */
/*      END                                                   */
/*      SUBROUTINE DSCAL ( N,A,X,INCX )                       */
/*      INTEGER            N,INCX                             */
/*      DOUBLE PRECISION   A,X(N)                             */
/*                                                            */
/*     This may be replaced by the corresponding BLAS routine.*/
/*     The following is a simple version for use with  LSQR.  */
/*                                                            */
/*      DO 10 I = 1, N                                        */
/*         X(I) = A*X(I)                                      */
/*   10 CONTINUE                                              */
/*      RETURN                                                */
/*                                                            */
/*     END OF DSCAL                                           */
/*      END                                                   */

/* ******************************************************** */
/*                                                          */
/*     These routines are for testing  LSQR.                */
/*                                                          */
/* ******************************************************** */

/* Subroutine */ void aprod_(mode, m, n, x, y, leniw, lenrw, iw, rw)
integer *mode, *m, *n;
doublereal *x, *y;
integer *leniw, *lenrw, *iw; /* these three parameters are unused */
doublereal *rw;
{
/*     ------------------------------------------------------------------ */
/*     This is the matrix-vector product routine required by  LSQR        */
/*     for a test matrix of the form  A = HY*D*HZ.  The quantities        */
/*     defining D, HY, HZ are in the work array RW, followed by a         */
/*     work array W.  These are passed to APROD1 and APROD2 in order to   */
/*     make the code readable.                                            */
/*     ------------------------------------------------------------------ */

    if (*mode == 1)
        aprod1_(m, n, x, y, rw, rw + *n, rw + *n + *m, rw + *n + *m + *n);
    else
        aprod2_(m, n, x, y, rw, rw + *n, rw + *n + *m, rw + *n + *m + *n);
} /* aprod_ */

/* Subroutine */ void aprod1_(m, n, x, y, d, hy, hz, w)
integer *m, *n;
doublereal *x, *y, *d, *hy, *hz, *w;
{
    /* Local variables */
    static integer i;

/*     ------------------------------------------------------------------ */
/*     APROD1  computes  Y = Y + A*X  for subroutine APROD,               */
/*     where A is a test matrix of the form  A = HY*D*HZ,                 */
/*     and the latter matrices HY, D, HZ are represented by               */
/*     input vectors with the same name.                                  */
/*     ------------------------------------------------------------------ */

    hprod_(n, hz, x, w);
    for (i =  0; i < *n; ++i) w[i] = d[i] * w[i];
    for (i = *n; i < *m; ++i) w[i] = 0.;
    hprod_(m, hy, w, w);
    for (i =  0; i < *m; ++i) y[i] += w[i];
} /* aprod1_ */

/* Subroutine */ void aprod2_(m, n, x, y, d, hy, hz, w)
integer *m, *n;
doublereal *x, *y, *d, *hy, *hz, *w;
{
    /* Local variables */
    static integer i;

/*     ------------------------------------------------------------------ */
/*     APROD2  computes  X = X + A(T)*Y  for subroutine APROD,            */
/*     where  A  is a test matrix of the form  A = HY*D*HZ,               */
/*     and the latter matrices  HY, D, HZ  are represented by             */
/*     input vectors with the same name.                                  */
/*     ------------------------------------------------------------------ */

    hprod_(m, hy, y, w);
    for (i = 0; i < *n; ++i) w[i] = d[i] * w[i];
    hprod_(n, hz, w, w);
    for (i = 0; i < *n; ++i) x[i] += w[i];
} /* aprod2_ */

/* Subroutine */ void hprod_(n, hz, x, y)
integer *n;
doublereal *hz, *x, *y;
{
    /* Local variables */
    static integer i;
    static doublereal s;

/*     ------------------------------------------------------------------ */
/*     HPROD  applies a Householder transformation stored in  HZ          */
/*     to get  Y = ( I - 2*HZ*HZ(transpose) ) * X.                        */
/*     ------------------------------------------------------------------ */

    s = 0.0;
    for (i = 0; i < *n; ++i)
        s += hz[i] * x[i];
    s += s;
    for (i = 0; i < *n; ++i)
        y[i] = x[i] - s * hz[i];
} /* hprod_ */

/* Subroutine */ void lstp_(m, n, nduplc, npower, damp, x, b, d, hy, hz, w, acond, rnorm)
integer *m, *n, *nduplc, *npower;
doublereal *damp, *x, *b, *d, *hy, *hz, *w, *acond, *rnorm;
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Local variables */
    static doublereal alfa, beta;
    static integer i, j;
    static doublereal t;
    static doublereal dampsq, fourpi;

/*     ------------------------------------------------------------------ */
/*     LSTP  generates a sparse least-squares test problem of the form    */
/*                                                                        */
/*                (   A    )*X = ( B )                                    */
/*                ( DAMP*I )     ( 0 )                                    */
/*                                                                        */
/*     having a specified solution X.  The matrix A is constructed        */
/*     in the form  A = HY*D*HZ,  where D is an M by N diagonal matrix,   */
/*     and HY and HZ are Householder transformations.                     */
/*                                                                        */
/*     The first 6 parameters are input to LSTP.  The remainder are       */
/*     output.  LSTP is intended for use with M .GE. N.                   */
/*                                                                        */
/*                                                                        */
/*     Functions and subroutines                                          */
/*                                                                        */
/*     TESTPROB           APROD1, HPROD                                   */
/*     BLAS               DNRM2                                           */
/*     ------------------------------------------------------------------ */
/*     Intrinsics and local variables                                     */
/*     ------------------------------------------------------------------ */
/*     Make two vectors of norm 1.0 for the Householder transformations.  */
/*     FOURPI  need not be exact.                                         */
/*     ------------------------------------------------------------------ */

    dampsq = *damp * *damp;
    fourpi = 12.566368000000001f;
    alfa = fourpi / *m;
    beta = fourpi / *n;
    for (i = 0; i < *m; ++i)
        hy[i] = sin((i+1) * alfa);
    for (i = 0; i < *n; ++i)
        hz[i] = cos((i+1) * beta);
    alfa = dnrm2_(m, hy, &c__1);
    beta = dnrm2_(n, hz, &c__1);
    d__1 = -1./alfa; dscal_(m, &d__1, hy, &c__1);
    d__1 = -1./beta; dscal_(n, &d__1, hz, &c__1);

/*     ------------------------------------------------------------------ */
/*     Set the diagonal matrix  D.  These are the singular values of  A.  */
/*     ------------------------------------------------------------------ */
    for (i = 0; i < *n; ++i) {
        j = (i + *nduplc) / *nduplc;
        t = (doublereal) (j * *nduplc);
        t /= *n;
        d[i] = pow_di(&t, npower);
    }
    *acond = sqrt((d[*n-1] * d[*n-1] + dampsq) / (d[0] * d[0] + dampsq));
/*     ------------------------------------------------------------------ */
/*     Compute the residual vector, storing it in  B.                     */
/*     It takes the form  HY*( s )                                        */
/*                           ( t )                                        */
/*     where  s  is obtained from  D*s = DAMP**2 * HZ * X                 */
/*     and    t  can be anything.                                         */
/*     ------------------------------------------------------------------ */
    hprod_(n, hz, x, b);
    for (i = 0; i < *n; ++i)
        b[i] = dampsq * b[i] / d[i];
    t = 1.;
    for (i = *n; i < *m; ++i) {
        j = i + 1 - *n;
        b[i] = t * j / *m;
        t = -t;
    }
    hprod_(m, hy, b, b);
/*     ------------------------------------------------------------------ */
/*     Now compute the true  B  =  RESIDUAL  +  A*X.                      */
/*     ------------------------------------------------------------------ */
    d__1 = dnrm2_(m, b, &c__1);
    d__2 = dnrm2_(n, x, &c__1);
    *rnorm = sqrt(d__1 * d__1 + dampsq * (d__2 * d__2));
    aprod1_(m, n, x, b, d, hy, hz, w);
} /* lstp_ */

/* Subroutine */ void test_(m, n, nduplc, npower, damp)
integer *m, *n, *nduplc, *npower;
doublereal *damp;
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    static doublereal atol, btol;
    static integer nout;
    static doublereal b[200];
    static integer j;
    static doublereal acond, u[200], v[100], w[100], x[100];
    static doublereal anorm;
    static doublereal enorm;
    static doublereal rnorm;
    static integer istop;
    static doublereal xnorm, xtrue[100], se[100];
    static integer iw[1];
    static doublereal rw[600], conlim, dampsq;
    static integer itnlim;
    static doublereal arnorm;
    static integer ltotal, itn;

/*     ------------------------------------------------------------------ */
/*     This is an example driver routine for running LSQR.                */
/*     It generates a test problem, solves it, and examines the results.  */
/*     Note that subroutine APROD must be declared EXTERNAL               */
/*     if it is used only in the call to LSQR.                            */

/*     Functions and subroutines                                          */
/*                                                                        */
/*     TESTPROB           APROD                                           */
/*     BLAS               DCOPY, DNRM2, DSCAL                             */
/*     ------------------------------------------------------------------ */

/*     Set the desired solution  XTRUE. */
    for (j = 0; j < *n; ++j) {
        xtrue[j] = (doublereal) (*n - j - 1);
    }
/*     Generate the specified test problem.                               */
/*     The workspace array  IW  is not needed in this application.        */
/*     The workspace array  RW  is used for the following vectors:        */
/*     D(N), HY(M), HZ(N), W(MAX(M,N)).                                   */
/*     The vectors  D, HY, HZ  will define the test matrix A.             */
/*     W is needed for workspace in APROD1 and APROD2.                    */
    ltotal = *n + *m + *n + max(*m,*n);
    if (ltotal > 600) {
        /* Not enough workspace. */
        printf("\n XXX  Insufficient workspace.  The length of  RW  should be at least %d\n", ltotal);
        return;
    }
    lstp_(m, n, nduplc, npower, damp, xtrue, b, rw, rw + *n, rw + *n + *m, rw + *n + *m + *n, &acond, &rnorm);
/*     Solve the problem defined by APROD, DAMP and B.                    */
/*     Copy the rhs vector B into U  (LSQR will overwrite U)              */
/*     and set the other input parameters for LSQR.                       */
    dcopy_(m, b, &c__1, u, &c__1);
    atol = 1e-10f;
    btol = atol;
    conlim = acond * 10.f;
    itnlim = *m + *n + 50;
    printf("\n\n --------------------------------------------------------------------\n");
    printf(" Least-Squares Test Problem      P( %d %d %d %d %12.2e )\n\n", *m,*n,*nduplc,*npower,*damp);
    printf(" Condition no. =%12.4e     Residual function =%17.9e\n", acond, rnorm);
    printf(" --------------------------------------------------------------------\n");
    lsqr_(m, n, aprod_, damp, &c__1, &c__600, iw, rw, u, v, w, x, se, &atol, &btol, &conlim,
          &itnlim, &nout, &istop, &itn, &anorm, &acond, &rnorm, &arnorm, &xnorm);
/*     Examine the results.                                               */
/*     We print the residual norms  RNORM  and  ARNORM  given by LSQR,    */
/*     and then compute their true values in terms of the solution  X     */
/*     obtained by  LSQR.  At least one of them should be small.          */
    dampsq = *damp * *damp;
    printf("\n\n                       Residual norm    Residual norm    Solution norm\n");
    printf("                      (Abar X - bbar)   (Normal eqns)         (X)\n");
    printf(" Estimated by LSQR%17.5e%17.5e%17.5e\n", rnorm, arnorm, xnorm);
/*     Compute  U = A*X - B.                                              */
/*     This is the negative of the usual residual vector.                 */
/*     It will be close to zero only if  B  is a compatible rhs           */
/*     and  X  is close to a solution.                                    */
    dcopy_(m, b, &c__1, u, &c__1);
    dscal_(m, &c_m1, u, &c__1);
    aprod_(&c__1, m, n, x, u, &c__1, &c__600, iw, rw);
/*     Compute  V = A(transpose)*U  +  DAMP**2 * X.                       */
/*     This will be close to zero in all cases                            */
/*     if  X  is close to a solution.                                     */
    dcopy_(n, x, &c__1, v, &c__1);
    dscal_(n, &dampsq, v, &c__1);
    aprod_(&c__2, m, n, v, u, &c__1, &c__600, iw, rw);
/*     Compute the norms associated with  X, U, V. */
    xnorm = dnrm2_(n, x, &c__1);
    d__1 = dnrm2_(m, u, &c__1);
    rnorm = sqrt(d__1 * d__1 + dampsq * xnorm * xnorm);
    arnorm = dnrm2_(n, v, &c__1);
    printf(" Computed from  X %17.5e%17.5e%17.5e\n", rnorm, arnorm, xnorm);
/*     Print the solution and standard error estimates from  LSQR. */
    printf("\n\n Solution  X\n");
    for (j = 0; j < *n; ++j)
        printf("%6d%14.6g\n", j+1, x[j]);
    printf("\n\n Standard errors  SE\n");
    for (j = 0; j < *n; ++j)
        printf("%6d%14.6g\n", j+1, se[j]);
    printf("\n");
/*     Print a clue about whether the solution looks OK. */
    for (j = 0; j < *n; ++j)
        w[j] = x[j] - xtrue[j];
    enorm = dnrm2_(n, w, &c__1) / (dnrm2_(n, xtrue, &c__1) + 1.);
    if (enorm <= 1e-5)
        printf("\n LSQR  appears to be successful.     Relative error in  X  =%10.2e\n", enorm);
    else
        printf("\n LSQR  appears to have failed.       Relative error in  X  =%10.2e\n", enorm);
} /* test_ */

/*     ------------- */
/*     Main program. */
/*     ------------- */
int main(void)
{
    static doublereal zero = 0.f;
/*  static doublereal damp1 = .1f; */
    static doublereal damp2 = .01f;
/*  static doublereal damp3 = .001f; */
/*  static doublereal damp4 = 1e-4f; */
    test_(&c__1, &c__1, &c__1, &c__1, &zero);
    test_(&c__2, &c__1, &c__1, &c__1, &zero);
    test_(&c__40, &c__40, &c__4, &c__4, &zero);
    test_(&c__40, &c__40, &c__4, &c__4, &damp2);
    test_(&c__80, &c__40, &c__4, &c__4, &damp2);
    return 0;
} /* End of main program for testing LSQR */
