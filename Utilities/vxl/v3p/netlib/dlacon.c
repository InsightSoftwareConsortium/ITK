#include "f2c.h"
#include "netlib.h"

/* Table of constant values */
static integer c__1 = 1;
static doublereal c_b11 = 1.;

/* Subroutine */ void dlacon_(n, v, x, isgn, est, kase)
integer *n;
doublereal *v, *x;
integer *isgn;
doublereal *est;
integer *kase;
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    static integer iter;
    static doublereal temp;
    static integer jump, i, j;
    static integer jlast;
    static doublereal altsgn, estold;


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     February 29, 1992 */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  DLACON estimates the 1-norm of a square, real matrix A.               */
/*  Reverse communication is used for evaluating matrix-vector products.  */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  N      (input) INTEGER                                                */
/*         The order of the matrix.  N >= 1.                              */
/*                                                                        */
/*  V      (workspace) DOUBLE PRECISION array, dimension (N)              */
/*         On the final return, V = A*W,  where  EST = norm(V)/norm(W)    */
/*         (W is not returned).                                           */
/*                                                                        */
/*  X      (input/output) DOUBLE PRECISION array, dimension (N)           */
/*         On an intermediate return, X should be overwritten by          */
/*               A * X,   if KASE=1,                                      */
/*               A' * X,  if KASE=2,                                      */
/*         and DLACON must be re-called with all the other parameters     */
/*         unchanged.                                                     */
/*                                                                        */
/*  ISGN   (workspace) INTEGER array, dimension (N)                       */
/*                                                                        */
/*  EST    (output) DOUBLE PRECISION                                      */
/*         An estimate (a lower bound) for norm(A).                       */
/*                                                                        */
/*  KASE   (input/output) INTEGER                                         */
/*         On the initial call to DLACON, KASE should be 0.               */
/*         On an intermediate return, KASE will be 1 or 2, indicating     */
/*         whether X should be overwritten by A * X  or A' * X.           */
/*         On the final return from DLACON, KASE will again be 0.         */
/*                                                                        */
/*  Further Details                                                       */
/*  ======= =======                                                       */
/*                                                                        */
/*  Contributed by Nick Higham, University of Manchester.                 */
/*  Originally named SONEST, dated March 16, 1988.                        */
/*                                                                        */
/*  Reference: N.J. Higham, "FORTRAN codes for estimating the one-norm of */
/*  a real or complex matrix, with applications to condition estimation", */
/*  ACM Trans. Math. Soft., vol. 14, no. 4, pp. 381-396, December 1988.   */
/*                                                                        */
/*  ===================================================================== */

    if (*kase == 0) {
        for (i = 0; i < *n; ++i) {
            x[i] = 1. / (doublereal) (*n);
        }
        *kase = 1;
        jump = 1;
        return;
    }

    switch ((int)jump) {
        case 1:  goto L20;
        case 2:  goto L40;
        case 3:  goto L70;
        case 4:  goto L110;
        case 5:  goto L140;
    }

/*     ................ ENTRY   (JUMP = 1) */
/*     FIRST ITERATION.  X HAS BEEN OVERWRITTEN BY A*X. */

L20:
    if (*n == 1) {
        v[0] = x[0];
        *est = abs(v[0]);
        *kase = 0;
        return;
    }
    *est = dasum_(n, x, &c__1);

    for (i = 0; i < *n; ++i) {
        x[i] = d_sign(&c_b11, &x[i]);
        isgn[i] = i_dnnt(&x[i]);
    }
    *kase = 2;
    jump = 2;
    return;

/*     ................ ENTRY   (JUMP = 2) */
/*     FIRST ITERATION.  X HAS BEEN OVERWRITTEN BY TRANSPOSE(A)*X. */

L40:
    j = idamax_(n, x, &c__1) - 1;
    iter = 2;

/*     MAIN LOOP - ITERATIONS 2,3,...,ITMAX. */

L50:
    for (i = 0; i < *n; ++i) {
        x[i] = 0.;
    }
    x[j] = 1.;
    *kase = 1;
    jump = 3;
    return;

/*     ................ ENTRY   (JUMP = 3) */
/*     X HAS BEEN OVERWRITTEN BY A*X. */

L70:
    dcopy_(n, x, &c__1, v, &c__1);
    estold = *est;
    *est = dasum_(n, v, &c__1);
    for (i = 0; i < *n; ++i) {
        d__1 = d_sign(&c_b11, &x[i]);
        if (i_dnnt(&d__1) != isgn[i]) {
            goto L90;
        }
    }
/*     REPEATED SIGN VECTOR DETECTED, HENCE ALGORITHM HAS CONVERGED. */
    goto L120;

L90:
/*     TEST FOR CYCLING. */
    if (*est <= estold) {
        goto L120;
    }

    for (i = 0; i < *n; ++i) {
        x[i] = d_sign(&c_b11, &x[i]);
        isgn[i] = i_dnnt(&x[i]);
    }
    *kase = 2;
    jump = 4;
    return;

/*     ................ ENTRY   (JUMP = 4) */
/*     X HAS BEEN OVERWRITTEN BY TRANSPOSE(A)*X. */

L110:
    jlast = j;
    j = idamax_(n, x, &c__1) - 1;
    if (x[jlast] != abs(x[j]) && iter < 5) {
        ++iter;
        goto L50;
    }

/*     ITERATION COMPLETE.  FINAL STAGE. */

L120:
    altsgn = 1.;
    for (i = 0; i < *n; ++i) {
        x[i] = altsgn * ((doublereal) i / (doublereal) (*n - 1) + 1.);
        altsgn = -altsgn;
    }
    *kase = 1;
    jump = 5;
    return;

/*     ................ ENTRY   (JUMP = 5) */
/*     X HAS BEEN OVERWRITTEN BY A*X. */

L140:
    temp = dasum_(n, x, &c__1) / (doublereal) (*n * 3) * 2.;
    if (temp > *est) {
        dcopy_(n, x, &c__1, v, &c__1);
        *est = temp;
    }

    *kase = 0;

} /* dlacon_ */
