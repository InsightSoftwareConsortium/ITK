/* blas/dtrmv.f -- translated by f2c (version 20050501).
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

/*<       SUBROUTINE DTRMV ( UPLO, TRANS, DIAG, N, A, LDA, X, INCX ) >*/
/* Subroutine */ int dtrmv_(char *uplo, char *trans, char *diag, integer *n,
        doublereal *a, integer *lda, doublereal *x, integer *incx, ftnlen
        uplo_len, ftnlen trans_len, ftnlen diag_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;

    /* Local variables */
    integer i__, j, ix, jx, kx=0, info;
    doublereal temp;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);
    logical nounit;
    (void)uplo_len;
    (void)trans_len;
    (void)diag_len;

/*     .. Scalar Arguments .. */
/*<       INTEGER            INCX, LDA, N >*/
/*<       CHARACTER*1        DIAG, TRANS, UPLO >*/
/*     .. Array Arguments .. */
/*<       DOUBLE PRECISION   A( LDA, * ), X( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DTRMV  performs one of the matrix-vector operations */

/*     x := A*x,   or   x := A'*x, */

/*  where x is an n element vector and  A is an n by n unit, or non-unit, */
/*  upper or lower triangular matrix. */

/*  Parameters */
/*  ========== */

/*  UPLO   - CHARACTER*1. */
/*           On entry, UPLO specifies whether the matrix is an upper or */
/*           lower triangular matrix as follows: */

/*              UPLO = 'U' or 'u'   A is an upper triangular matrix. */

/*              UPLO = 'L' or 'l'   A is a lower triangular matrix. */

/*           Unchanged on exit. */

/*  TRANS  - CHARACTER*1. */
/*           On entry, TRANS specifies the operation to be performed as */
/*           follows: */

/*              TRANS = 'N' or 'n'   x := A*x. */

/*              TRANS = 'T' or 't'   x := A'*x. */

/*              TRANS = 'C' or 'c'   x := A'*x. */

/*           Unchanged on exit. */

/*  DIAG   - CHARACTER*1. */
/*           On entry, DIAG specifies whether or not A is unit */
/*           triangular as follows: */

/*              DIAG = 'U' or 'u'   A is assumed to be unit triangular. */

/*              DIAG = 'N' or 'n'   A is not assumed to be unit */
/*                                  triangular. */

/*           Unchanged on exit. */

/*  N      - INTEGER. */
/*           On entry, N specifies the order of the matrix A. */
/*           N must be at least zero. */
/*           Unchanged on exit. */

/*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ). */
/*           Before entry with  UPLO = 'U' or 'u', the leading n by n */
/*           upper triangular part of the array A must contain the upper */
/*           triangular matrix and the strictly lower triangular part of */
/*           A is not referenced. */
/*           Before entry with UPLO = 'L' or 'l', the leading n by n */
/*           lower triangular part of the array A must contain the lower */
/*           triangular matrix and the strictly upper triangular part of */
/*           A is not referenced. */
/*           Note that when  DIAG = 'U' or 'u', the diagonal elements of */
/*           A are not referenced either, but are assumed to be unity. */
/*           Unchanged on exit. */

/*  LDA    - INTEGER. */
/*           On entry, LDA specifies the first dimension of A as declared */
/*           in the calling (sub) program. LDA must be at least */
/*           max( 1, n ). */
/*           Unchanged on exit. */

/*  X      - DOUBLE PRECISION array of dimension at least */
/*           ( 1 + ( n - 1 )*abs( INCX ) ). */
/*           Before entry, the incremented array X must contain the n */
/*           element vector x. On exit, X is overwritten with the */
/*           transformed vector x. */

/*  INCX   - INTEGER. */
/*           On entry, INCX specifies the increment for the elements of */
/*           X. INCX must not be zero. */
/*           Unchanged on exit. */


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
/*<       INTEGER            I, INFO, IX, J, JX, KX >*/
/*<       LOGICAL            NOUNIT >*/
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       EXTERNAL           LSAME >*/
/*     .. External Subroutines .. */
/*<       EXTERNAL           XERBLA >*/
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          MAX >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Test the input parameters. */

/*<       INFO = 0 >*/
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --x;

    /* Function Body */
    info = 0;
/*<    >*/
    if (! lsame_(uplo, "U", (ftnlen)1, (ftnlen)1) && ! lsame_(uplo, "L", (
            ftnlen)1, (ftnlen)1)) {
/*<          INFO = 1 >*/
        info = 1;
/*<    >*/
    } else if (! lsame_(trans, "N", (ftnlen)1, (ftnlen)1) && ! lsame_(trans,
            "T", (ftnlen)1, (ftnlen)1) && ! lsame_(trans, "C", (ftnlen)1, (
            ftnlen)1)) {
/*<          INFO = 2 >*/
        info = 2;
/*<    >*/
    } else if (! lsame_(diag, "U", (ftnlen)1, (ftnlen)1) && ! lsame_(diag,
            "N", (ftnlen)1, (ftnlen)1)) {
/*<          INFO = 3 >*/
        info = 3;
/*<       ELSE IF( N.LT.0 )THEN >*/
    } else if (*n < 0) {
/*<          INFO = 4 >*/
        info = 4;
/*<       ELSE IF( LDA.LT.MAX( 1, N ) )THEN >*/
    } else if (*lda < max(1,*n)) {
/*<          INFO = 6 >*/
        info = 6;
/*<       ELSE IF( INCX.EQ.0 )THEN >*/
    } else if (*incx == 0) {
/*<          INFO = 8 >*/
        info = 8;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 )THEN >*/
    if (info != 0) {
/*<          CALL XERBLA( 'DTRMV ', INFO ) >*/
        xerbla_("DTRMV ", &info, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible. */

/*<    >*/
    if (*n == 0) {
        return 0;
    }

/*<       NOUNIT = LSAME( DIAG, 'N' ) >*/
    nounit = lsame_(diag, "N", (ftnlen)1, (ftnlen)1);

/*     Set up the start point in X if the increment is not unity. This */
/*     will be  ( N - 1 )*INCX  too small for descending loops. */

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

/*     Start the operations. In this version the elements of A are */
/*     accessed sequentially with one pass through A. */

/*<       IF( LSAME( TRANS, 'N' ) )THEN >*/
    if (lsame_(trans, "N", (ftnlen)1, (ftnlen)1)) {

/*        Form  x := A*x. */

/*<          IF( LSAME( UPLO, 'U' ) )THEN >*/
        if (lsame_(uplo, "U", (ftnlen)1, (ftnlen)1)) {
/*<             IF( INCX.EQ.1 )THEN >*/
            if (*incx == 1) {
/*<                DO 20, J = 1, N >*/
                i__1 = *n;
                for (j = 1; j <= i__1; ++j) {
/*<                   IF( X( J ).NE.ZERO )THEN >*/
                    if (x[j] != 0.) {
/*<                      TEMP = X( J ) >*/
                        temp = x[j];
/*<                      DO 10, I = 1, J - 1 >*/
                        i__2 = j - 1;
                        for (i__ = 1; i__ <= i__2; ++i__) {
/*<                         X( I ) = X( I ) + TEMP*A( I, J ) >*/
                            x[i__] += temp * a[i__ + j * a_dim1];
/*<    10                CONTINUE >*/
/* L10: */
                        }
/*<    >*/
                        if (nounit) {
                            x[j] *= a[j + j * a_dim1];
                        }
/*<                   END IF >*/
                    }
/*<    20          CONTINUE >*/
/* L20: */
                }
/*<             ELSE >*/
            } else {
/*<                JX = KX >*/
                jx = kx;
/*<                DO 40, J = 1, N >*/
                i__1 = *n;
                for (j = 1; j <= i__1; ++j) {
/*<                   IF( X( JX ).NE.ZERO )THEN >*/
                    if (x[jx] != 0.) {
/*<                      TEMP = X( JX ) >*/
                        temp = x[jx];
/*<                      IX   = KX >*/
                        ix = kx;
/*<                      DO 30, I = 1, J - 1 >*/
                        i__2 = j - 1;
                        for (i__ = 1; i__ <= i__2; ++i__) {
/*<                         X( IX ) = X( IX ) + TEMP*A( I, J ) >*/
                            x[ix] += temp * a[i__ + j * a_dim1];
/*<                         IX      = IX      + INCX >*/
                            ix += *incx;
/*<    30                CONTINUE >*/
/* L30: */
                        }
/*<    >*/
                        if (nounit) {
                            x[jx] *= a[j + j * a_dim1];
                        }
/*<                   END IF >*/
                    }
/*<                   JX = JX + INCX >*/
                    jx += *incx;
/*<    40          CONTINUE >*/
/* L40: */
                }
/*<             END IF >*/
            }
/*<          ELSE >*/
        } else {
/*<             IF( INCX.EQ.1 )THEN >*/
            if (*incx == 1) {
/*<                DO 60, J = N, 1, -1 >*/
                for (j = *n; j >= 1; --j) {
/*<                   IF( X( J ).NE.ZERO )THEN >*/
                    if (x[j] != 0.) {
/*<                      TEMP = X( J ) >*/
                        temp = x[j];
/*<                      DO 50, I = N, J + 1, -1 >*/
                        i__1 = j + 1;
                        for (i__ = *n; i__ >= i__1; --i__) {
/*<                         X( I ) = X( I ) + TEMP*A( I, J ) >*/
                            x[i__] += temp * a[i__ + j * a_dim1];
/*<    50                CONTINUE >*/
/* L50: */
                        }
/*<    >*/
                        if (nounit) {
                            x[j] *= a[j + j * a_dim1];
                        }
/*<                   END IF >*/
                    }
/*<    60          CONTINUE >*/
/* L60: */
                }
/*<             ELSE >*/
            } else {
/*<                KX = KX + ( N - 1 )*INCX >*/
                kx += (*n - 1) * *incx;
/*<                JX = KX >*/
                jx = kx;
/*<                DO 80, J = N, 1, -1 >*/
                for (j = *n; j >= 1; --j) {
/*<                   IF( X( JX ).NE.ZERO )THEN >*/
                    if (x[jx] != 0.) {
/*<                      TEMP = X( JX ) >*/
                        temp = x[jx];
/*<                      IX   = KX >*/
                        ix = kx;
/*<                      DO 70, I = N, J + 1, -1 >*/
                        i__1 = j + 1;
                        for (i__ = *n; i__ >= i__1; --i__) {
/*<                         X( IX ) = X( IX ) + TEMP*A( I, J ) >*/
                            x[ix] += temp * a[i__ + j * a_dim1];
/*<                         IX      = IX      - INCX >*/
                            ix -= *incx;
/*<    70                CONTINUE >*/
/* L70: */
                        }
/*<    >*/
                        if (nounit) {
                            x[jx] *= a[j + j * a_dim1];
                        }
/*<                   END IF >*/
                    }
/*<                   JX = JX - INCX >*/
                    jx -= *incx;
/*<    80          CONTINUE >*/
/* L80: */
                }
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       ELSE >*/
    } else {

/*        Form  x := A'*x. */

/*<          IF( LSAME( UPLO, 'U' ) )THEN >*/
        if (lsame_(uplo, "U", (ftnlen)1, (ftnlen)1)) {
/*<             IF( INCX.EQ.1 )THEN >*/
            if (*incx == 1) {
/*<                DO 100, J = N, 1, -1 >*/
                for (j = *n; j >= 1; --j) {
/*<                   TEMP = X( J ) >*/
                    temp = x[j];
/*<    >*/
                    if (nounit) {
                        temp *= a[j + j * a_dim1];
                    }
/*<                   DO 90, I = J - 1, 1, -1 >*/
                    for (i__ = j - 1; i__ >= 1; --i__) {
/*<                      TEMP = TEMP + A( I, J )*X( I ) >*/
                        temp += a[i__ + j * a_dim1] * x[i__];
/*<    90             CONTINUE >*/
/* L90: */
                    }
/*<                   X( J ) = TEMP >*/
                    x[j] = temp;
/*<   100          CONTINUE >*/
/* L100: */
                }
/*<             ELSE >*/
            } else {
/*<                JX = KX + ( N - 1 )*INCX >*/
                jx = kx + (*n - 1) * *incx;
/*<                DO 120, J = N, 1, -1 >*/
                for (j = *n; j >= 1; --j) {
/*<                   TEMP = X( JX ) >*/
                    temp = x[jx];
/*<                   IX   = JX >*/
                    ix = jx;
/*<    >*/
                    if (nounit) {
                        temp *= a[j + j * a_dim1];
                    }
/*<                   DO 110, I = J - 1, 1, -1 >*/
                    for (i__ = j - 1; i__ >= 1; --i__) {
/*<                      IX   = IX   - INCX >*/
                        ix -= *incx;
/*<                      TEMP = TEMP + A( I, J )*X( IX ) >*/
                        temp += a[i__ + j * a_dim1] * x[ix];
/*<   110             CONTINUE >*/
/* L110: */
                    }
/*<                   X( JX ) = TEMP >*/
                    x[jx] = temp;
/*<                   JX      = JX   - INCX >*/
                    jx -= *incx;
/*<   120          CONTINUE >*/
/* L120: */
                }
/*<             END IF >*/
            }
/*<          ELSE >*/
        } else {
/*<             IF( INCX.EQ.1 )THEN >*/
            if (*incx == 1) {
/*<                DO 140, J = 1, N >*/
                i__1 = *n;
                for (j = 1; j <= i__1; ++j) {
/*<                   TEMP = X( J ) >*/
                    temp = x[j];
/*<    >*/
                    if (nounit) {
                        temp *= a[j + j * a_dim1];
                    }
/*<                   DO 130, I = J + 1, N >*/
                    i__2 = *n;
                    for (i__ = j + 1; i__ <= i__2; ++i__) {
/*<                      TEMP = TEMP + A( I, J )*X( I ) >*/
                        temp += a[i__ + j * a_dim1] * x[i__];
/*<   130             CONTINUE >*/
/* L130: */
                    }
/*<                   X( J ) = TEMP >*/
                    x[j] = temp;
/*<   140          CONTINUE >*/
/* L140: */
                }
/*<             ELSE >*/
            } else {
/*<                JX = KX >*/
                jx = kx;
/*<                DO 160, J = 1, N >*/
                i__1 = *n;
                for (j = 1; j <= i__1; ++j) {
/*<                   TEMP = X( JX ) >*/
                    temp = x[jx];
/*<                   IX   = JX >*/
                    ix = jx;
/*<    >*/
                    if (nounit) {
                        temp *= a[j + j * a_dim1];
                    }
/*<                   DO 150, I = J + 1, N >*/
                    i__2 = *n;
                    for (i__ = j + 1; i__ <= i__2; ++i__) {
/*<                      IX   = IX   + INCX >*/
                        ix += *incx;
/*<                      TEMP = TEMP + A( I, J )*X( IX ) >*/
                        temp += a[i__ + j * a_dim1] * x[ix];
/*<   150             CONTINUE >*/
/* L150: */
                    }
/*<                   X( JX ) = TEMP >*/
                    x[jx] = temp;
/*<                   JX      = JX   + INCX >*/
                    jx += *incx;
/*<   160          CONTINUE >*/
/* L160: */
                }
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       END IF >*/
    }

/*<       RETURN >*/
    return 0;

/*     End of DTRMV . */

/*<       END >*/
} /* dtrmv_ */

#ifdef __cplusplus
        }
#endif
