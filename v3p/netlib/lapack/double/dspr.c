/* dspr.f -- translated by f2c (version 20060506).
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

/*<       SUBROUTINE DSPR  ( UPLO, N, ALPHA, X, INCX, AP ) >*/
/* Subroutine */ int dspr_(char *uplo, integer *n, doublereal *alpha,
        doublereal *x, integer *incx, doublereal *ap, ftnlen uplo_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    integer i__, j, k, kk, ix, jx, kx=0, info;
    doublereal temp;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);
    (void)uplo_len;

/*     .. Scalar Arguments .. */
/*<       DOUBLE PRECISION   ALPHA >*/
/*<       INTEGER            INCX, N >*/
/*<       CHARACTER*1        UPLO >*/
/*     .. Array Arguments .. */
/*<       DOUBLE PRECISION   AP( * ), X( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DSPR    performs the symmetric rank 1 operation */

/*     A := alpha*x*x' + A, */

/*  where alpha is a real scalar, x is an n element vector and A is an */
/*  n by n symmetric matrix, supplied in packed form. */

/*  Parameters */
/*  ========== */

/*  UPLO   - CHARACTER*1. */
/*           On entry, UPLO specifies whether the upper or lower */
/*           triangular part of the matrix A is supplied in the packed */
/*           array AP as follows: */

/*              UPLO = 'U' or 'u'   The upper triangular part of A is */
/*                                  supplied in AP. */

/*              UPLO = 'L' or 'l'   The lower triangular part of A is */
/*                                  supplied in AP. */

/*           Unchanged on exit. */

/*  N      - INTEGER. */
/*           On entry, N specifies the order of the matrix A. */
/*           N must be at least zero. */
/*           Unchanged on exit. */

/*  ALPHA  - DOUBLE PRECISION. */
/*           On entry, ALPHA specifies the scalar alpha. */
/*           Unchanged on exit. */

/*  X      - DOUBLE PRECISION array of dimension at least */
/*           ( 1 + ( n - 1 )*abs( INCX ) ). */
/*           Before entry, the incremented array X must contain the n */
/*           element vector x. */
/*           Unchanged on exit. */

/*  INCX   - INTEGER. */
/*           On entry, INCX specifies the increment for the elements of */
/*           X. INCX must not be zero. */
/*           Unchanged on exit. */

/*  AP     - DOUBLE PRECISION array of DIMENSION at least */
/*           ( ( n*( n + 1 ) )/2 ). */
/*           Before entry with  UPLO = 'U' or 'u', the array AP must */
/*           contain the upper triangular part of the symmetric matrix */
/*           packed sequentially, column by column, so that AP( 1 ) */
/*           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 1, 2 ) */
/*           and a( 2, 2 ) respectively, and so on. On exit, the array */
/*           AP is overwritten by the upper triangular part of the */
/*           updated matrix. */
/*           Before entry with UPLO = 'L' or 'l', the array AP must */
/*           contain the lower triangular part of the symmetric matrix */
/*           packed sequentially, column by column, so that AP( 1 ) */
/*           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 2, 1 ) */
/*           and a( 3, 1 ) respectively, and so on. On exit, the array */
/*           AP is overwritten by the lower triangular part of the */
/*           updated matrix. */


/*  Level 2 Blas routine. */

/*  -- Written on 22-October-1986. */
/*     Jack Dongarra, Argonne National Lab. */
/*     Jeremy Du Croz, Nag Central Office. */
/*     Sven Hammarling, Nag Central Office. */
/*     Richard Hanson, Sandia National Labs. */


/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO >*/
/*<       PARAMETER        ( ZERO = 0.0D+0 ) >*/
/*     .. Local Scalars .. */
/*<       DOUBLE PRECISION   TEMP >*/
/*<       INTEGER            I, INFO, IX, J, JX, K, KK, KX >*/
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       EXTERNAL           LSAME >*/
/*     .. External Subroutines .. */
/*<       EXTERNAL           XERBLA >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Test the input parameters. */

/*<       INFO = 0 >*/
    /* Parameter adjustments */
    --ap;
    --x;

    /* Function Body */
    info = 0;
/*<    >*/
    if (! lsame_(uplo, "U", (ftnlen)1, (ftnlen)1) && ! lsame_(uplo, "L", (
            ftnlen)1, (ftnlen)1)) {
/*<          INFO = 1 >*/
        info = 1;
/*<       ELSE IF( N.LT.0 )THEN >*/
    } else if (*n < 0) {
/*<          INFO = 2 >*/
        info = 2;
/*<       ELSE IF( INCX.EQ.0 )THEN >*/
    } else if (*incx == 0) {
/*<          INFO = 5 >*/
        info = 5;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 )THEN >*/
    if (info != 0) {
/*<          CALL XERBLA( 'DSPR  ', INFO ) >*/
        xerbla_("DSPR  ", &info, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible. */

/*<    >*/
    if (*n == 0 || *alpha == 0.) {
        return 0;
    }

/*     Set the start point in X if the increment is not unity. */

/*<       IF( INCX.LE.0 )THEN >*/
    if (*incx <= 0) {
/*<          KX = 1 - ( N - 1 )*INCX >*/
        kx = 1 - (*n - 1) * *incx;
/*<       ELSE IF( INCX.NE.1 )THEN >*/
    } else if (*incx != 1) {
/*<          KX = 1 >*/
        kx = 1;
/*<       END IF >*/
    }

/*     Start the operations. In this version the elements of the array AP */
/*     are accessed sequentially with one pass through AP. */

/*<       KK = 1 >*/
    kk = 1;
/*<       IF( LSAME( UPLO, 'U' ) )THEN >*/
    if (lsame_(uplo, "U", (ftnlen)1, (ftnlen)1)) {

/*        Form  A  when upper triangle is stored in AP. */

/*<          IF( INCX.EQ.1 )THEN >*/
        if (*incx == 1) {
/*<             DO 20, J = 1, N >*/
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
/*<                IF( X( J ).NE.ZERO )THEN >*/
                if (x[j] != 0.) {
/*<                   TEMP = ALPHA*X( J ) >*/
                    temp = *alpha * x[j];
/*<                   K    = KK >*/
                    k = kk;
/*<                   DO 10, I = 1, J >*/
                    i__2 = j;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      AP( K ) = AP( K ) + X( I )*TEMP >*/
                        ap[k] += x[i__] * temp;
/*<                      K       = K       + 1 >*/
                        ++k;
/*<    10             CONTINUE >*/
/* L10: */
                    }
/*<                END IF >*/
                }
/*<                KK = KK + J >*/
                kk += j;
/*<    20       CONTINUE >*/
/* L20: */
            }
/*<          ELSE >*/
        } else {
/*<             JX = KX >*/
            jx = kx;
/*<             DO 40, J = 1, N >*/
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
/*<                IF( X( JX ).NE.ZERO )THEN >*/
                if (x[jx] != 0.) {
/*<                   TEMP = ALPHA*X( JX ) >*/
                    temp = *alpha * x[jx];
/*<                   IX   = KX >*/
                    ix = kx;
/*<                   DO 30, K = KK, KK + J - 1 >*/
                    i__2 = kk + j - 1;
                    for (k = kk; k <= i__2; ++k) {
/*<                      AP( K ) = AP( K ) + X( IX )*TEMP >*/
                        ap[k] += x[ix] * temp;
/*<                      IX      = IX      + INCX >*/
                        ix += *incx;
/*<    30             CONTINUE >*/
/* L30: */
                    }
/*<                END IF >*/
                }
/*<                JX = JX + INCX >*/
                jx += *incx;
/*<                KK = KK + J >*/
                kk += j;
/*<    40       CONTINUE >*/
/* L40: */
            }
/*<          END IF >*/
        }
/*<       ELSE >*/
    } else {

/*        Form  A  when lower triangle is stored in AP. */

/*<          IF( INCX.EQ.1 )THEN >*/
        if (*incx == 1) {
/*<             DO 60, J = 1, N >*/
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
/*<                IF( X( J ).NE.ZERO )THEN >*/
                if (x[j] != 0.) {
/*<                   TEMP = ALPHA*X( J ) >*/
                    temp = *alpha * x[j];
/*<                   K    = KK >*/
                    k = kk;
/*<                   DO 50, I = J, N >*/
                    i__2 = *n;
                    for (i__ = j; i__ <= i__2; ++i__) {
/*<                      AP( K ) = AP( K ) + X( I )*TEMP >*/
                        ap[k] += x[i__] * temp;
/*<                      K       = K       + 1 >*/
                        ++k;
/*<    50             CONTINUE >*/
/* L50: */
                    }
/*<                END IF >*/
                }
/*<                KK = KK + N - J + 1 >*/
                kk = kk + *n - j + 1;
/*<    60       CONTINUE >*/
/* L60: */
            }
/*<          ELSE >*/
        } else {
/*<             JX = KX >*/
            jx = kx;
/*<             DO 80, J = 1, N >*/
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
/*<                IF( X( JX ).NE.ZERO )THEN >*/
                if (x[jx] != 0.) {
/*<                   TEMP = ALPHA*X( JX ) >*/
                    temp = *alpha * x[jx];
/*<                   IX   = JX >*/
                    ix = jx;
/*<                   DO 70, K = KK, KK + N - J >*/
                    i__2 = kk + *n - j;
                    for (k = kk; k <= i__2; ++k) {
/*<                      AP( K ) = AP( K ) + X( IX )*TEMP >*/
                        ap[k] += x[ix] * temp;
/*<                      IX      = IX      + INCX >*/
                        ix += *incx;
/*<    70             CONTINUE >*/
/* L70: */
                    }
/*<                END IF >*/
                }
/*<                JX = JX + INCX >*/
                jx += *incx;
/*<                KK = KK + N - J + 1 >*/
                kk = kk + *n - j + 1;
/*<    80       CONTINUE >*/
/* L80: */
            }
/*<          END IF >*/
        }
/*<       END IF >*/
    }

/*<       RETURN >*/
    return 0;

/*     End of DSPR  . */

/*<       END >*/
} /* dspr_ */

#ifdef __cplusplus
        }
#endif
