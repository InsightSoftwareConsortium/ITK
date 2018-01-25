/* lapack/double/dlacon.f -- translated by f2c (version 20050501).
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
#include <stdio.h> /* fprintf */
#include "v3p_netlib.h"

/* Table of constant values */

static integer c__1 = 1;
static doublereal c_b11 = 1.;

/*<       SUBROUTINE DLACON( N, V, X, ISGN, EST, KASE ) >*/
/* Subroutine */ int dlacon_(integer *n, doublereal *v, doublereal *x,
        integer *isgn, doublereal *est, integer *kase)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *);
    integer i_dnnt(doublereal *);

    /* Local variables */
    static integer i__, j, iter;
    static doublereal temp;
    static integer jump;
    extern doublereal dasum_(integer *, doublereal *, integer *);
    static integer jlast;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *,
            doublereal *, integer *);
    extern integer idamax_(integer *, doublereal *, integer *);
    static doublereal altsgn, estold;

    fprintf(stderr,
            "WARNING: dlacon_ has not been converted for thread safety "
            "because the vnl test suite does not manage to call it "
            "through dgges.  Please send the case for which you get this "
            "message to the vxl-users mailing list:\n"
            "https://lists.sourceforge.net/lists/listinfo/vxl-users\n\n");

/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     February 29, 1992 */

/*     .. Scalar Arguments .. */
/*<       INTEGER            KASE, N >*/
/*<       DOUBLE PRECISION   EST >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       INTEGER            ISGN( * ) >*/
/*<       DOUBLE PRECISION   V( * ), X( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DLACON estimates the 1-norm of a square, real matrix A. */
/*  Reverse communication is used for evaluating matrix-vector products. */

/*  Arguments */
/*  ========= */

/*  N      (input) INTEGER */
/*         The order of the matrix.  N >= 1. */

/*  V      (workspace) DOUBLE PRECISION array, dimension (N) */
/*         On the final return, V = A*W,  where  EST = norm(V)/norm(W) */
/*         (W is not returned). */

/*  X      (input/output) DOUBLE PRECISION array, dimension (N) */
/*         On an intermediate return, X should be overwritten by */
/*               A * X,   if KASE=1, */
/*               A' * X,  if KASE=2, */
/*         and DLACON must be re-called with all the other parameters */
/*         unchanged. */

/*  ISGN   (workspace) INTEGER array, dimension (N) */

/*  EST    (output) DOUBLE PRECISION */
/*         An estimate (a lower bound) for norm(A). */

/*  KASE   (input/output) INTEGER */
/*         On the initial call to DLACON, KASE should be 0. */
/*         On an intermediate return, KASE will be 1 or 2, indicating */
/*         whether X should be overwritten by A * X  or A' * X. */
/*         On the final return from DLACON, KASE will again be 0. */

/*  Further Details */
/*  ======= ======= */

/*  Contributed by Nick Higham, University of Manchester. */
/*  Originally named SONEST, dated March 16, 1988. */

/*  Reference: N.J. Higham, "FORTRAN codes for estimating the one-norm of */
/*  a real or complex matrix, with applications to condition estimation", */
/*  ACM Trans. Math. Soft., vol. 14, no. 4, pp. 381-396, December 1988. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       INTEGER            ITMAX >*/
/*<       PARAMETER          ( ITMAX = 5 ) >*/
/*<       DOUBLE PRECISION   ZERO, ONE, TWO >*/
/*<       PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TWO = 2.0D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       INTEGER            I, ITER, J, JLAST, JUMP >*/
/*<       DOUBLE PRECISION   ALTSGN, ESTOLD, TEMP >*/
/*     .. */
/*     .. External Functions .. */
/*<       INTEGER            IDAMAX >*/
/*<       DOUBLE PRECISION   DASUM >*/
/*<       EXTERNAL           IDAMAX, DASUM >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           DCOPY >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, DBLE, NINT, SIGN >*/
/*     .. */
/*     .. Save statement .. */
/*<       SAVE >*/
/*     .. */
/*     .. Executable Statements .. */

/*<       IF( KASE.EQ.0 ) THEN >*/
    /* Parameter adjustments */
    --isgn;
    --x;
    --v;

    /* Function Body */
    if (*kase == 0) {
/*<          DO 10 I = 1, N >*/
        i__1 = *n;
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             X( I ) = ONE / DBLE( N ) >*/
            x[i__] = 1. / (doublereal) (*n);
/*<    10    CONTINUE >*/
/* L10: */
        }
/*<          KASE = 1 >*/
        *kase = 1;
/*<          JUMP = 1 >*/
        jump = 1;
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*<       GO TO ( 20, 40, 70, 110, 140 )JUMP >*/
    switch (jump) {
        case 1:  goto L20;
        case 2:  goto L40;
        case 3:  goto L70;
        case 4:  goto L110;
        case 5:  goto L140;
    }

/*     ................ ENTRY   (JUMP = 1) */
/*     FIRST ITERATION.  X HAS BEEN OVERWRITTEN BY A*X. */

/*<    20 CONTINUE >*/
L20:
/*<       IF( N.EQ.1 ) THEN >*/
    if (*n == 1) {
/*<          V( 1 ) = X( 1 ) >*/
        v[1] = x[1];
/*<          EST = ABS( V( 1 ) ) >*/
        *est = abs(v[1]);
/*        ... QUIT */
/*<          GO TO 150 >*/
        goto L150;
/*<       END IF >*/
    }
/*<       EST = DASUM( N, X, 1 ) >*/
    *est = dasum_(n, &x[1], &c__1);

/*<       DO 30 I = 1, N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          X( I ) = SIGN( ONE, X( I ) ) >*/
        x[i__] = d_sign(&c_b11, &x[i__]);
/*<          ISGN( I ) = NINT( X( I ) ) >*/
        isgn[i__] = i_dnnt(&x[i__]);
/*<    30 CONTINUE >*/
/* L30: */
    }
/*<       KASE = 2 >*/
    *kase = 2;
/*<       JUMP = 2 >*/
    jump = 2;
/*<       RETURN >*/
    return 0;

/*     ................ ENTRY   (JUMP = 2) */
/*     FIRST ITERATION.  X HAS BEEN OVERWRITTEN BY TRANSPOSE(A)*X. */

/*<    40 CONTINUE >*/
L40:
/*<       J = IDAMAX( N, X, 1 ) >*/
    j = idamax_(n, &x[1], &c__1);
/*<       ITER = 2 >*/
    iter = 2;

/*     MAIN LOOP - ITERATIONS 2,3,...,ITMAX. */

/*<    50 CONTINUE >*/
L50:
/*<       DO 60 I = 1, N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          X( I ) = ZERO >*/
        x[i__] = 0.;
/*<    60 CONTINUE >*/
/* L60: */
    }
/*<       X( J ) = ONE >*/
    x[j] = 1.;
/*<       KASE = 1 >*/
    *kase = 1;
/*<       JUMP = 3 >*/
    jump = 3;
/*<       RETURN >*/
    return 0;

/*     ................ ENTRY   (JUMP = 3) */
/*     X HAS BEEN OVERWRITTEN BY A*X. */

/*<    70 CONTINUE >*/
L70:
/*<       CALL DCOPY( N, X, 1, V, 1 ) >*/
    dcopy_(n, &x[1], &c__1, &v[1], &c__1);
/*<       ESTOLD = EST >*/
    estold = *est;
/*<       EST = DASUM( N, V, 1 ) >*/
    *est = dasum_(n, &v[1], &c__1);
/*<       DO 80 I = 1, N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<    >*/
        d__1 = d_sign(&c_b11, &x[i__]);
        if (i_dnnt(&d__1) != isgn[i__]) {
            goto L90;
        }
/*<    80 CONTINUE >*/
/* L80: */
    }
/*     REPEATED SIGN VECTOR DETECTED, HENCE ALGORITHM HAS CONVERGED. */
/*<       GO TO 120 >*/
    goto L120;

/*<    90 CONTINUE >*/
L90:
/*     TEST FOR CYCLING. */
/*<    >*/
    if (*est <= estold) {
        goto L120;
    }

/*<       DO 100 I = 1, N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          X( I ) = SIGN( ONE, X( I ) ) >*/
        x[i__] = d_sign(&c_b11, &x[i__]);
/*<          ISGN( I ) = NINT( X( I ) ) >*/
        isgn[i__] = i_dnnt(&x[i__]);
/*<   100 CONTINUE >*/
/* L100: */
    }
/*<       KASE = 2 >*/
    *kase = 2;
/*<       JUMP = 4 >*/
    jump = 4;
/*<       RETURN >*/
    return 0;

/*     ................ ENTRY   (JUMP = 4) */
/*     X HAS BEEN OVERWRITTEN BY TRANSPOSE(A)*X. */

/*<   110 CONTINUE >*/
L110:
/*<       JLAST = J >*/
    jlast = j;
/*<       J = IDAMAX( N, X, 1 ) >*/
    j = idamax_(n, &x[1], &c__1);
/*<       IF( ( X( JLAST ).NE.ABS( X( J ) ) ) .AND. ( ITER.LT.ITMAX ) ) THEN >*/
    if (x[jlast] != (d__1 = x[j], abs(d__1)) && iter < 5) {
/*<          ITER = ITER + 1 >*/
        ++iter;
/*<          GO TO 50 >*/
        goto L50;
/*<       END IF >*/
    }

/*     ITERATION COMPLETE.  FINAL STAGE. */

/*<   120 CONTINUE >*/
L120:
/*<       ALTSGN = ONE >*/
    altsgn = 1.;
/*<       DO 130 I = 1, N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          X( I ) = ALTSGN*( ONE+DBLE( I-1 ) / DBLE( N-1 ) ) >*/
        x[i__] = altsgn * ((doublereal) (i__ - 1) / (doublereal) (*n - 1) +
                1.);
/*<          ALTSGN = -ALTSGN >*/
        altsgn = -altsgn;
/*<   130 CONTINUE >*/
/* L130: */
    }
/*<       KASE = 1 >*/
    *kase = 1;
/*<       JUMP = 5 >*/
    jump = 5;
/*<       RETURN >*/
    return 0;

/*     ................ ENTRY   (JUMP = 5) */
/*     X HAS BEEN OVERWRITTEN BY A*X. */

/*<   140 CONTINUE >*/
L140:
/*<       TEMP = TWO*( DASUM( N, X, 1 ) / DBLE( 3*N ) ) >*/
    temp = dasum_(n, &x[1], &c__1) / (doublereal) (*n * 3) * 2.;
/*<       IF( TEMP.GT.EST ) THEN >*/
    if (temp > *est) {
/*<          CALL DCOPY( N, X, 1, V, 1 ) >*/
        dcopy_(n, &x[1], &c__1, &v[1], &c__1);
/*<          EST = TEMP >*/
        *est = temp;
/*<       END IF >*/
    }

/*<   150 CONTINUE >*/
L150:
/*<       KASE = 0 >*/
    *kase = 0;
/*<       RETURN >*/
    return 0;

/*     End of DLACON */

/*<       END >*/
} /* dlacon_ */

#ifdef __cplusplus
        }
#endif
