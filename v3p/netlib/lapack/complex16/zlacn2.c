/* lapack/complex16/zlacn2.f -- translated by f2c (version 20090411).
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

/*<       SUBROUTINE ZLACN2( N, V, X, EST, KASE, ISAVE ) >*/
/* Subroutine */ int zlacn2_(integer *n, doublecomplex *v, doublecomplex *x,
        doublereal *est, integer *kase, integer *isave)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1, d__2;
    doublecomplex z__1;

    /* Builtin functions */
    double z_abs(doublecomplex *), d_imag(doublecomplex *);

    /* Local variables */
    integer i__;
    doublereal temp, absxi;
    integer jlast;
    extern /* Subroutine */ int zcopy_(integer *, doublecomplex *, integer *,
            doublecomplex *, integer *);
    extern integer izmax1_(integer *, doublecomplex *, integer *);
    extern doublereal dzsum1_(integer *, doublecomplex *, integer *), dlamch_(
            char *, ftnlen);
    doublereal safmin, altsgn, estold;


/*  -- LAPACK auxiliary routine (version 3.2) -- */
/*  -- LAPACK is a software package provided by Univ. of Tennessee,    -- */
/*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..-- */
/*     November 2006 */

/*     .. Scalar Arguments .. */
/*<       INTEGER            KASE, N >*/
/*<       DOUBLE PRECISION   EST >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       INTEGER            ISAVE( 3 ) >*/
/*<       COMPLEX*16         V( * ), X( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZLACN2 estimates the 1-norm of a square, complex matrix A. */
/*  Reverse communication is used for evaluating matrix-vector products. */

/*  Arguments */
/*  ========= */

/*  N      (input) INTEGER */
/*         The order of the matrix.  N >= 1. */

/*  V      (workspace) COMPLEX*16 array, dimension (N) */
/*         On the final return, V = A*W,  where  EST = norm(V)/norm(W) */
/*         (W is not returned). */

/*  X      (input/output) COMPLEX*16 array, dimension (N) */
/*         On an intermediate return, X should be overwritten by */
/*               A * X,   if KASE=1, */
/*               A' * X,  if KASE=2, */
/*         where A' is the conjugate transpose of A, and ZLACN2 must be */
/*         re-called with all the other parameters unchanged. */

/*  EST    (input/output) DOUBLE PRECISION */
/*         On entry with KASE = 1 or 2 and ISAVE(1) = 3, EST should be */
/*         unchanged from the previous call to ZLACN2. */
/*         On exit, EST is an estimate (a lower bound) for norm(A). */

/*  KASE   (input/output) INTEGER */
/*         On the initial call to ZLACN2, KASE should be 0. */
/*         On an intermediate return, KASE will be 1 or 2, indicating */
/*         whether X should be overwritten by A * X  or A' * X. */
/*         On the final return from ZLACN2, KASE will again be 0. */

/*  ISAVE  (input/output) INTEGER array, dimension (3) */
/*         ISAVE is used to save variables between calls to ZLACN2 */

/*  Further Details */
/*  ======= ======= */

/*  Contributed by Nick Higham, University of Manchester. */
/*  Originally named CONEST, dated March 16, 1988. */

/*  Reference: N.J. Higham, "FORTRAN codes for estimating the one-norm of */
/*  a real or complex matrix, with applications to condition estimation", */
/*  ACM Trans. Math. Soft., vol. 14, no. 4, pp. 381-396, December 1988. */

/*  Last modified:  April, 1999 */

/*  This is a thread safe version of ZLACON, which uses the array ISAVE */
/*  in place of a SAVE statement, as follows: */

/*     ZLACON     ZLACN2 */
/*      JUMP     ISAVE(1) */
/*      J        ISAVE(2) */
/*      ITER     ISAVE(3) */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       INTEGER              ITMAX >*/
/*<       PARAMETER          ( ITMAX = 5 ) >*/
/*<       DOUBLE PRECISION     ONE,         TWO >*/
/*<       PARAMETER          ( ONE = 1.0D0, TWO = 2.0D0 ) >*/
/*<       COMPLEX*16           CZERO, CONE >*/
/*<    >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       INTEGER            I, JLAST >*/
/*<       DOUBLE PRECISION   ABSXI, ALTSGN, ESTOLD, SAFMIN, TEMP >*/
/*     .. */
/*     .. External Functions .. */
/*<       INTEGER            IZMAX1 >*/
/*<       DOUBLE PRECISION   DLAMCH, DZSUM1 >*/
/*<       EXTERNAL           IZMAX1, DLAMCH, DZSUM1 >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           ZCOPY >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, DBLE, DCMPLX, DIMAG >*/
/*     .. */
/*     .. Executable Statements .. */

/*<       SAFMIN = DLAMCH( 'Safe minimum' ) >*/
    /* Parameter adjustments */
    --isave;
    --x;
    --v;

    /* Function Body */
    safmin = dlamch_("Safe minimum", (ftnlen)12);
/*<       IF( KASE.EQ.0 ) THEN >*/
    if (*kase == 0) {
/*<          DO 10 I = 1, N >*/
        i__1 = *n;
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             X( I ) = DCMPLX( ONE / DBLE( N ) ) >*/
            i__2 = i__;
            d__1 = 1. / (doublereal) (*n);
            z__1.r = d__1, z__1.i = 0.;
            x[i__2].r = z__1.r, x[i__2].i = z__1.i;
/*<    10    CONTINUE >*/
/* L10: */
        }
/*<          KASE = 1 >*/
        *kase = 1;
/*<          ISAVE( 1 ) = 1 >*/
        isave[1] = 1;
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*<       GO TO ( 20, 40, 70, 90, 120 )ISAVE( 1 ) >*/
    switch (isave[1]) {
        case 1:  goto L20;
        case 2:  goto L40;
        case 3:  goto L70;
        case 4:  goto L90;
        case 5:  goto L120;
    }

/*     ................ ENTRY   (ISAVE( 1 ) = 1) */
/*     FIRST ITERATION.  X HAS BEEN OVERWRITTEN BY A*X. */

/*<    20 CONTINUE >*/
L20:
/*<       IF( N.EQ.1 ) THEN >*/
    if (*n == 1) {
/*<          V( 1 ) = X( 1 ) >*/
        v[1].r = x[1].r, v[1].i = x[1].i;
/*<          EST = ABS( V( 1 ) ) >*/
        *est = z_abs(&v[1]);
/*        ... QUIT */
/*<          GO TO 130 >*/
        goto L130;
/*<       END IF >*/
    }
/*<       EST = DZSUM1( N, X, 1 ) >*/
    *est = dzsum1_(n, &x[1], &c__1);

/*<       DO 30 I = 1, N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          ABSXI = ABS( X( I ) ) >*/
        absxi = z_abs(&x[i__]);
/*<          IF( ABSXI.GT.SAFMIN ) THEN >*/
        if (absxi > safmin) {
/*<    >*/
            i__2 = i__;
            i__3 = i__;
            d__1 = x[i__3].r / absxi;
            d__2 = d_imag(&x[i__]) / absxi;
            z__1.r = d__1, z__1.i = d__2;
            x[i__2].r = z__1.r, x[i__2].i = z__1.i;
/*<          ELSE >*/
        } else {
/*<             X( I ) = CONE >*/
            i__2 = i__;
            x[i__2].r = 1., x[i__2].i = 0.;
/*<          END IF >*/
        }
/*<    30 CONTINUE >*/
/* L30: */
    }
/*<       KASE = 2 >*/
    *kase = 2;
/*<       ISAVE( 1 ) = 2 >*/
    isave[1] = 2;
/*<       RETURN >*/
    return 0;

/*     ................ ENTRY   (ISAVE( 1 ) = 2) */
/*     FIRST ITERATION.  X HAS BEEN OVERWRITTEN BY CTRANS(A)*X. */

/*<    40 CONTINUE >*/
L40:
/*<       ISAVE( 2 ) = IZMAX1( N, X, 1 ) >*/
    isave[2] = izmax1_(n, &x[1], &c__1);
/*<       ISAVE( 3 ) = 2 >*/
    isave[3] = 2;

/*     MAIN LOOP - ITERATIONS 2,3,...,ITMAX. */

/*<    50 CONTINUE >*/
L50:
/*<       DO 60 I = 1, N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          X( I ) = CZERO >*/
        i__2 = i__;
        x[i__2].r = 0., x[i__2].i = 0.;
/*<    60 CONTINUE >*/
/* L60: */
    }
/*<       X( ISAVE( 2 ) ) = CONE >*/
    i__1 = isave[2];
    x[i__1].r = 1., x[i__1].i = 0.;
/*<       KASE = 1 >*/
    *kase = 1;
/*<       ISAVE( 1 ) = 3 >*/
    isave[1] = 3;
/*<       RETURN >*/
    return 0;

/*     ................ ENTRY   (ISAVE( 1 ) = 3) */
/*     X HAS BEEN OVERWRITTEN BY A*X. */

/*<    70 CONTINUE >*/
L70:
/*<       CALL ZCOPY( N, X, 1, V, 1 ) >*/
    zcopy_(n, &x[1], &c__1, &v[1], &c__1);
/*<       ESTOLD = EST >*/
    estold = *est;
/*<       EST = DZSUM1( N, V, 1 ) >*/
    *est = dzsum1_(n, &v[1], &c__1);

/*     TEST FOR CYCLING. */
/*<    >*/
    if (*est <= estold) {
        goto L100;
    }

/*<       DO 80 I = 1, N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          ABSXI = ABS( X( I ) ) >*/
        absxi = z_abs(&x[i__]);
/*<          IF( ABSXI.GT.SAFMIN ) THEN >*/
        if (absxi > safmin) {
/*<    >*/
            i__2 = i__;
            i__3 = i__;
            d__1 = x[i__3].r / absxi;
            d__2 = d_imag(&x[i__]) / absxi;
            z__1.r = d__1, z__1.i = d__2;
            x[i__2].r = z__1.r, x[i__2].i = z__1.i;
/*<          ELSE >*/
        } else {
/*<             X( I ) = CONE >*/
            i__2 = i__;
            x[i__2].r = 1., x[i__2].i = 0.;
/*<          END IF >*/
        }
/*<    80 CONTINUE >*/
/* L80: */
    }
/*<       KASE = 2 >*/
    *kase = 2;
/*<       ISAVE( 1 ) = 4 >*/
    isave[1] = 4;
/*<       RETURN >*/
    return 0;

/*     ................ ENTRY   (ISAVE( 1 ) = 4) */
/*     X HAS BEEN OVERWRITTEN BY CTRANS(A)*X. */

/*<    90 CONTINUE >*/
L90:
/*<       JLAST = ISAVE( 2 ) >*/
    jlast = isave[2];
/*<       ISAVE( 2 ) = IZMAX1( N, X, 1 ) >*/
    isave[2] = izmax1_(n, &x[1], &c__1);
/*<    >*/
    if (z_abs(&x[jlast]) != z_abs(&x[isave[2]]) && isave[3] < 5) {
/*<          ISAVE( 3 ) = ISAVE( 3 ) + 1 >*/
        ++isave[3];
/*<          GO TO 50 >*/
        goto L50;
/*<       END IF >*/
    }

/*     ITERATION COMPLETE.  FINAL STAGE. */

/*<   100 CONTINUE >*/
L100:
/*<       ALTSGN = ONE >*/
    altsgn = 1.;
/*<       DO 110 I = 1, N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          X( I ) = DCMPLX( ALTSGN*( ONE+DBLE( I-1 ) / DBLE( N-1 ) ) ) >*/
        i__2 = i__;
        d__1 = altsgn * ((doublereal) (i__ - 1) / (doublereal) (*n - 1) + 1.);
        z__1.r = d__1, z__1.i = 0.;
        x[i__2].r = z__1.r, x[i__2].i = z__1.i;
/*<          ALTSGN = -ALTSGN >*/
        altsgn = -altsgn;
/*<   110 CONTINUE >*/
/* L110: */
    }
/*<       KASE = 1 >*/
    *kase = 1;
/*<       ISAVE( 1 ) = 5 >*/
    isave[1] = 5;
/*<       RETURN >*/
    return 0;

/*     ................ ENTRY   (ISAVE( 1 ) = 5) */
/*     X HAS BEEN OVERWRITTEN BY A*X. */

/*<   120 CONTINUE >*/
L120:
/*<       TEMP = TWO*( DZSUM1( N, X, 1 ) / DBLE( 3*N ) ) >*/
    temp = dzsum1_(n, &x[1], &c__1) / (doublereal) (*n * 3) * 2.;
/*<       IF( TEMP.GT.EST ) THEN >*/
    if (temp > *est) {
/*<          CALL ZCOPY( N, X, 1, V, 1 ) >*/
        zcopy_(n, &x[1], &c__1, &v[1], &c__1);
/*<          EST = TEMP >*/
        *est = temp;
/*<       END IF >*/
    }

/*<   130 CONTINUE >*/
L130:
/*<       KASE = 0 >*/
    *kase = 0;
/*<       RETURN >*/
    return 0;

/*     End of ZLACN2 */

/*<       END >*/
} /* zlacn2_ */

#ifdef __cplusplus
        }
#endif
