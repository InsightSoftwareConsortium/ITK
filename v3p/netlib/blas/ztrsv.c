/* blas/ztrsv.f -- translated by f2c (version 20050501).
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

/*<       SUBROUTINE ZTRSV ( UPLO, TRANS, DIAG, N, A, LDA, X, INCX ) >*/
/* Subroutine */ int ztrsv_(char *uplo, char *trans, char *diag, integer *n,
        doublecomplex *a, integer *lda, doublecomplex *x, integer *incx,
        ftnlen uplo_len, ftnlen trans_len, ftnlen diag_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4, i__5;
    doublecomplex z__1, z__2, z__3;

    /* Builtin functions */
    void z_div(doublecomplex *, doublecomplex *, doublecomplex *), d_cnjg(
            doublecomplex *, doublecomplex *);

    /* Local variables */
    integer i__, j, ix, jx, kx=0, info;
    doublecomplex temp;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);
    logical noconj, nounit;
    (void)uplo_len;
    (void)trans_len;
    (void)diag_len;

/*     .. Scalar Arguments .. */
/*<       INTEGER            INCX, LDA, N >*/
/*<       CHARACTER*1        DIAG, TRANS, UPLO >*/
/*     .. Array Arguments .. */
/*<       COMPLEX*16         A( LDA, * ), X( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZTRSV  solves one of the systems of equations */

/*     A*x = b,   or   A'*x = b,   or   conjg( A' )*x = b, */

/*  where b and x are n element vectors and A is an n by n unit, or */
/*  non-unit, upper or lower triangular matrix. */

/*  No test for singularity or near-singularity is included in this */
/*  routine. Such tests must be performed before calling this routine. */

/*  Parameters */
/*  ========== */

/*  UPLO   - CHARACTER*1. */
/*           On entry, UPLO specifies whether the matrix is an upper or */
/*           lower triangular matrix as follows: */

/*              UPLO = 'U' or 'u'   A is an upper triangular matrix. */

/*              UPLO = 'L' or 'l'   A is a lower triangular matrix. */

/*           Unchanged on exit. */

/*  TRANS  - CHARACTER*1. */
/*           On entry, TRANS specifies the equations to be solved as */
/*           follows: */

/*              TRANS = 'N' or 'n'   A*x = b. */

/*              TRANS = 'T' or 't'   A'*x = b. */

/*              TRANS = 'C' or 'c'   conjg( A' )*x = b. */

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

/*  A      - COMPLEX*16       array of DIMENSION ( LDA, n ). */
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

/*  X      - COMPLEX*16       array of dimension at least */
/*           ( 1 + ( n - 1 )*abs( INCX ) ). */
/*           Before entry, the incremented array X must contain the n */
/*           element right-hand side vector b. On exit, X is overwritten */
/*           with the solution vector x. */

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
/*<       COMPLEX*16         ZERO >*/
/*<       PARAMETER        ( ZERO = ( 0.0D+0, 0.0D+0 ) ) >*/
/*     .. Local Scalars .. */
/*<       COMPLEX*16         TEMP >*/
/*<       INTEGER            I, INFO, IX, J, JX, KX >*/
/*<       LOGICAL            NOCONJ, NOUNIT >*/
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       EXTERNAL           LSAME >*/
/*     .. External Subroutines .. */
/*<       EXTERNAL           XERBLA >*/
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          DCONJG, MAX >*/
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
/*<          CALL XERBLA( 'ZTRSV ', INFO ) >*/
        xerbla_("ZTRSV ", &info, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible. */

/*<    >*/
    if (*n == 0) {
        return 0;
    }

/*<       NOCONJ = LSAME( TRANS, 'T' ) >*/
    noconj = lsame_(trans, "T", (ftnlen)1, (ftnlen)1);
/*<       NOUNIT = LSAME( DIAG , 'N' ) >*/
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

/*        Form  x := inv( A )*x. */

/*<          IF( LSAME( UPLO, 'U' ) )THEN >*/
        if (lsame_(uplo, "U", (ftnlen)1, (ftnlen)1)) {
/*<             IF( INCX.EQ.1 )THEN >*/
            if (*incx == 1) {
/*<                DO 20, J = N, 1, -1 >*/
                for (j = *n; j >= 1; --j) {
/*<                   IF( X( J ).NE.ZERO )THEN >*/
                    i__1 = j;
                    if (x[i__1].r != 0. || x[i__1].i != 0.) {
/*<    >*/
                        if (nounit) {
                            i__1 = j;
                            z_div(&z__1, &x[j], &a[j + j * a_dim1]);
                            x[i__1].r = z__1.r, x[i__1].i = z__1.i;
                        }
/*<                      TEMP = X( J ) >*/
                        i__1 = j;
                        temp.r = x[i__1].r, temp.i = x[i__1].i;
/*<                      DO 10, I = J - 1, 1, -1 >*/
                        for (i__ = j - 1; i__ >= 1; --i__) {
/*<                         X( I ) = X( I ) - TEMP*A( I, J ) >*/
                            i__1 = i__;
                            i__2 = i__;
                            i__3 = i__ + j * a_dim1;
                            z__2.r = temp.r * a[i__3].r - temp.i * a[i__3].i,
                                    z__2.i = temp.r * a[i__3].i + temp.i * a[
                                    i__3].r;
                            z__1.r = x[i__2].r - z__2.r, z__1.i = x[i__2].i -
                                    z__2.i;
                            x[i__1].r = z__1.r, x[i__1].i = z__1.i;
/*<    10                CONTINUE >*/
/* L10: */
                        }
/*<                   END IF >*/
                    }
/*<    20          CONTINUE >*/
/* L20: */
                }
/*<             ELSE >*/
            } else {
/*<                JX = KX + ( N - 1 )*INCX >*/
                jx = kx + (*n - 1) * *incx;
/*<                DO 40, J = N, 1, -1 >*/
                for (j = *n; j >= 1; --j) {
/*<                   IF( X( JX ).NE.ZERO )THEN >*/
                    i__1 = jx;
                    if (x[i__1].r != 0. || x[i__1].i != 0.) {
/*<    >*/
                        if (nounit) {
                            i__1 = jx;
                            z_div(&z__1, &x[jx], &a[j + j * a_dim1]);
                            x[i__1].r = z__1.r, x[i__1].i = z__1.i;
                        }
/*<                      TEMP = X( JX ) >*/
                        i__1 = jx;
                        temp.r = x[i__1].r, temp.i = x[i__1].i;
/*<                      IX   = JX >*/
                        ix = jx;
/*<                      DO 30, I = J - 1, 1, -1 >*/
                        for (i__ = j - 1; i__ >= 1; --i__) {
/*<                         IX      = IX      - INCX >*/
                            ix -= *incx;
/*<                         X( IX ) = X( IX ) - TEMP*A( I, J ) >*/
                            i__1 = ix;
                            i__2 = ix;
                            i__3 = i__ + j * a_dim1;
                            z__2.r = temp.r * a[i__3].r - temp.i * a[i__3].i,
                                    z__2.i = temp.r * a[i__3].i + temp.i * a[
                                    i__3].r;
                            z__1.r = x[i__2].r - z__2.r, z__1.i = x[i__2].i -
                                    z__2.i;
                            x[i__1].r = z__1.r, x[i__1].i = z__1.i;
/*<    30                CONTINUE >*/
/* L30: */
                        }
/*<                   END IF >*/
                    }
/*<                   JX = JX - INCX >*/
                    jx -= *incx;
/*<    40          CONTINUE >*/
/* L40: */
                }
/*<             END IF >*/
            }
/*<          ELSE >*/
        } else {
/*<             IF( INCX.EQ.1 )THEN >*/
            if (*incx == 1) {
/*<                DO 60, J = 1, N >*/
                i__1 = *n;
                for (j = 1; j <= i__1; ++j) {
/*<                   IF( X( J ).NE.ZERO )THEN >*/
                    i__2 = j;
                    if (x[i__2].r != 0. || x[i__2].i != 0.) {
/*<    >*/
                        if (nounit) {
                            i__2 = j;
                            z_div(&z__1, &x[j], &a[j + j * a_dim1]);
                            x[i__2].r = z__1.r, x[i__2].i = z__1.i;
                        }
/*<                      TEMP = X( J ) >*/
                        i__2 = j;
                        temp.r = x[i__2].r, temp.i = x[i__2].i;
/*<                      DO 50, I = J + 1, N >*/
                        i__2 = *n;
                        for (i__ = j + 1; i__ <= i__2; ++i__) {
/*<                         X( I ) = X( I ) - TEMP*A( I, J ) >*/
                            i__3 = i__;
                            i__4 = i__;
                            i__5 = i__ + j * a_dim1;
                            z__2.r = temp.r * a[i__5].r - temp.i * a[i__5].i,
                                    z__2.i = temp.r * a[i__5].i + temp.i * a[
                                    i__5].r;
                            z__1.r = x[i__4].r - z__2.r, z__1.i = x[i__4].i -
                                    z__2.i;
                            x[i__3].r = z__1.r, x[i__3].i = z__1.i;
/*<    50                CONTINUE >*/
/* L50: */
                        }
/*<                   END IF >*/
                    }
/*<    60          CONTINUE >*/
/* L60: */
                }
/*<             ELSE >*/
            } else {
/*<                JX = KX >*/
                jx = kx;
/*<                DO 80, J = 1, N >*/
                i__1 = *n;
                for (j = 1; j <= i__1; ++j) {
/*<                   IF( X( JX ).NE.ZERO )THEN >*/
                    i__2 = jx;
                    if (x[i__2].r != 0. || x[i__2].i != 0.) {
/*<    >*/
                        if (nounit) {
                            i__2 = jx;
                            z_div(&z__1, &x[jx], &a[j + j * a_dim1]);
                            x[i__2].r = z__1.r, x[i__2].i = z__1.i;
                        }
/*<                      TEMP = X( JX ) >*/
                        i__2 = jx;
                        temp.r = x[i__2].r, temp.i = x[i__2].i;
/*<                      IX   = JX >*/
                        ix = jx;
/*<                      DO 70, I = J + 1, N >*/
                        i__2 = *n;
                        for (i__ = j + 1; i__ <= i__2; ++i__) {
/*<                         IX      = IX      + INCX >*/
                            ix += *incx;
/*<                         X( IX ) = X( IX ) - TEMP*A( I, J ) >*/
                            i__3 = ix;
                            i__4 = ix;
                            i__5 = i__ + j * a_dim1;
                            z__2.r = temp.r * a[i__5].r - temp.i * a[i__5].i,
                                    z__2.i = temp.r * a[i__5].i + temp.i * a[
                                    i__5].r;
                            z__1.r = x[i__4].r - z__2.r, z__1.i = x[i__4].i -
                                    z__2.i;
                            x[i__3].r = z__1.r, x[i__3].i = z__1.i;
/*<    70                CONTINUE >*/
/* L70: */
                        }
/*<                   END IF >*/
                    }
/*<                   JX = JX + INCX >*/
                    jx += *incx;
/*<    80          CONTINUE >*/
/* L80: */
                }
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       ELSE >*/
    } else {

/*        Form  x := inv( A' )*x  or  x := inv( conjg( A' ) )*x. */

/*<          IF( LSAME( UPLO, 'U' ) )THEN >*/
        if (lsame_(uplo, "U", (ftnlen)1, (ftnlen)1)) {
/*<             IF( INCX.EQ.1 )THEN >*/
            if (*incx == 1) {
/*<                DO 110, J = 1, N >*/
                i__1 = *n;
                for (j = 1; j <= i__1; ++j) {
/*<                   TEMP = X( J ) >*/
                    i__2 = j;
                    temp.r = x[i__2].r, temp.i = x[i__2].i;
/*<                   IF( NOCONJ )THEN >*/
                    if (noconj) {
/*<                      DO 90, I = 1, J - 1 >*/
                        i__2 = j - 1;
                        for (i__ = 1; i__ <= i__2; ++i__) {
/*<                         TEMP = TEMP - A( I, J )*X( I ) >*/
                            i__3 = i__ + j * a_dim1;
                            i__4 = i__;
                            z__2.r = a[i__3].r * x[i__4].r - a[i__3].i * x[
                                    i__4].i, z__2.i = a[i__3].r * x[i__4].i +
                                    a[i__3].i * x[i__4].r;
                            z__1.r = temp.r - z__2.r, z__1.i = temp.i -
                                    z__2.i;
                            temp.r = z__1.r, temp.i = z__1.i;
/*<    90                CONTINUE >*/
/* L90: */
                        }
/*<    >*/
                        if (nounit) {
                            z_div(&z__1, &temp, &a[j + j * a_dim1]);
                            temp.r = z__1.r, temp.i = z__1.i;
                        }
/*<                   ELSE >*/
                    } else {
/*<                      DO 100, I = 1, J - 1 >*/
                        i__2 = j - 1;
                        for (i__ = 1; i__ <= i__2; ++i__) {
/*<                         TEMP = TEMP - DCONJG( A( I, J ) )*X( I ) >*/
                            d_cnjg(&z__3, &a[i__ + j * a_dim1]);
                            i__3 = i__;
                            z__2.r = z__3.r * x[i__3].r - z__3.i * x[i__3].i,
                                    z__2.i = z__3.r * x[i__3].i + z__3.i * x[
                                    i__3].r;
                            z__1.r = temp.r - z__2.r, z__1.i = temp.i -
                                    z__2.i;
                            temp.r = z__1.r, temp.i = z__1.i;
/*<   100                CONTINUE >*/
/* L100: */
                        }
/*<    >*/
                        if (nounit) {
                            d_cnjg(&z__2, &a[j + j * a_dim1]);
                            z_div(&z__1, &temp, &z__2);
                            temp.r = z__1.r, temp.i = z__1.i;
                        }
/*<                   END IF >*/
                    }
/*<                   X( J ) = TEMP >*/
                    i__2 = j;
                    x[i__2].r = temp.r, x[i__2].i = temp.i;
/*<   110          CONTINUE >*/
/* L110: */
                }
/*<             ELSE >*/
            } else {
/*<                JX = KX >*/
                jx = kx;
/*<                DO 140, J = 1, N >*/
                i__1 = *n;
                for (j = 1; j <= i__1; ++j) {
/*<                   IX   = KX >*/
                    ix = kx;
/*<                   TEMP = X( JX ) >*/
                    i__2 = jx;
                    temp.r = x[i__2].r, temp.i = x[i__2].i;
/*<                   IF( NOCONJ )THEN >*/
                    if (noconj) {
/*<                      DO 120, I = 1, J - 1 >*/
                        i__2 = j - 1;
                        for (i__ = 1; i__ <= i__2; ++i__) {
/*<                         TEMP = TEMP - A( I, J )*X( IX ) >*/
                            i__3 = i__ + j * a_dim1;
                            i__4 = ix;
                            z__2.r = a[i__3].r * x[i__4].r - a[i__3].i * x[
                                    i__4].i, z__2.i = a[i__3].r * x[i__4].i +
                                    a[i__3].i * x[i__4].r;
                            z__1.r = temp.r - z__2.r, z__1.i = temp.i -
                                    z__2.i;
                            temp.r = z__1.r, temp.i = z__1.i;
/*<                         IX   = IX   + INCX >*/
                            ix += *incx;
/*<   120                CONTINUE >*/
/* L120: */
                        }
/*<    >*/
                        if (nounit) {
                            z_div(&z__1, &temp, &a[j + j * a_dim1]);
                            temp.r = z__1.r, temp.i = z__1.i;
                        }
/*<                   ELSE >*/
                    } else {
/*<                      DO 130, I = 1, J - 1 >*/
                        i__2 = j - 1;
                        for (i__ = 1; i__ <= i__2; ++i__) {
/*<                         TEMP = TEMP - DCONJG( A( I, J ) )*X( IX ) >*/
                            d_cnjg(&z__3, &a[i__ + j * a_dim1]);
                            i__3 = ix;
                            z__2.r = z__3.r * x[i__3].r - z__3.i * x[i__3].i,
                                    z__2.i = z__3.r * x[i__3].i + z__3.i * x[
                                    i__3].r;
                            z__1.r = temp.r - z__2.r, z__1.i = temp.i -
                                    z__2.i;
                            temp.r = z__1.r, temp.i = z__1.i;
/*<                         IX   = IX   + INCX >*/
                            ix += *incx;
/*<   130                CONTINUE >*/
/* L130: */
                        }
/*<    >*/
                        if (nounit) {
                            d_cnjg(&z__2, &a[j + j * a_dim1]);
                            z_div(&z__1, &temp, &z__2);
                            temp.r = z__1.r, temp.i = z__1.i;
                        }
/*<                   END IF >*/
                    }
/*<                   X( JX ) = TEMP >*/
                    i__2 = jx;
                    x[i__2].r = temp.r, x[i__2].i = temp.i;
/*<                   JX      = JX   + INCX >*/
                    jx += *incx;
/*<   140          CONTINUE >*/
/* L140: */
                }
/*<             END IF >*/
            }
/*<          ELSE >*/
        } else {
/*<             IF( INCX.EQ.1 )THEN >*/
            if (*incx == 1) {
/*<                DO 170, J = N, 1, -1 >*/
                for (j = *n; j >= 1; --j) {
/*<                   TEMP = X( J ) >*/
                    i__1 = j;
                    temp.r = x[i__1].r, temp.i = x[i__1].i;
/*<                   IF( NOCONJ )THEN >*/
                    if (noconj) {
/*<                      DO 150, I = N, J + 1, -1 >*/
                        i__1 = j + 1;
                        for (i__ = *n; i__ >= i__1; --i__) {
/*<                         TEMP = TEMP - A( I, J )*X( I ) >*/
                            i__2 = i__ + j * a_dim1;
                            i__3 = i__;
                            z__2.r = a[i__2].r * x[i__3].r - a[i__2].i * x[
                                    i__3].i, z__2.i = a[i__2].r * x[i__3].i +
                                    a[i__2].i * x[i__3].r;
                            z__1.r = temp.r - z__2.r, z__1.i = temp.i -
                                    z__2.i;
                            temp.r = z__1.r, temp.i = z__1.i;
/*<   150                CONTINUE >*/
/* L150: */
                        }
/*<    >*/
                        if (nounit) {
                            z_div(&z__1, &temp, &a[j + j * a_dim1]);
                            temp.r = z__1.r, temp.i = z__1.i;
                        }
/*<                   ELSE >*/
                    } else {
/*<                      DO 160, I = N, J + 1, -1 >*/
                        i__1 = j + 1;
                        for (i__ = *n; i__ >= i__1; --i__) {
/*<                         TEMP = TEMP - DCONJG( A( I, J ) )*X( I ) >*/
                            d_cnjg(&z__3, &a[i__ + j * a_dim1]);
                            i__2 = i__;
                            z__2.r = z__3.r * x[i__2].r - z__3.i * x[i__2].i,
                                    z__2.i = z__3.r * x[i__2].i + z__3.i * x[
                                    i__2].r;
                            z__1.r = temp.r - z__2.r, z__1.i = temp.i -
                                    z__2.i;
                            temp.r = z__1.r, temp.i = z__1.i;
/*<   160                CONTINUE >*/
/* L160: */
                        }
/*<    >*/
                        if (nounit) {
                            d_cnjg(&z__2, &a[j + j * a_dim1]);
                            z_div(&z__1, &temp, &z__2);
                            temp.r = z__1.r, temp.i = z__1.i;
                        }
/*<                   END IF >*/
                    }
/*<                   X( J ) = TEMP >*/
                    i__1 = j;
                    x[i__1].r = temp.r, x[i__1].i = temp.i;
/*<   170          CONTINUE >*/
/* L170: */
                }
/*<             ELSE >*/
            } else {
/*<                KX = KX + ( N - 1 )*INCX >*/
                kx += (*n - 1) * *incx;
/*<                JX = KX >*/
                jx = kx;
/*<                DO 200, J = N, 1, -1 >*/
                for (j = *n; j >= 1; --j) {
/*<                   IX   = KX >*/
                    ix = kx;
/*<                   TEMP = X( JX ) >*/
                    i__1 = jx;
                    temp.r = x[i__1].r, temp.i = x[i__1].i;
/*<                   IF( NOCONJ )THEN >*/
                    if (noconj) {
/*<                      DO 180, I = N, J + 1, -1 >*/
                        i__1 = j + 1;
                        for (i__ = *n; i__ >= i__1; --i__) {
/*<                         TEMP = TEMP - A( I, J )*X( IX ) >*/
                            i__2 = i__ + j * a_dim1;
                            i__3 = ix;
                            z__2.r = a[i__2].r * x[i__3].r - a[i__2].i * x[
                                    i__3].i, z__2.i = a[i__2].r * x[i__3].i +
                                    a[i__2].i * x[i__3].r;
                            z__1.r = temp.r - z__2.r, z__1.i = temp.i -
                                    z__2.i;
                            temp.r = z__1.r, temp.i = z__1.i;
/*<                         IX   = IX   - INCX >*/
                            ix -= *incx;
/*<   180                CONTINUE >*/
/* L180: */
                        }
/*<    >*/
                        if (nounit) {
                            z_div(&z__1, &temp, &a[j + j * a_dim1]);
                            temp.r = z__1.r, temp.i = z__1.i;
                        }
/*<                   ELSE >*/
                    } else {
/*<                      DO 190, I = N, J + 1, -1 >*/
                        i__1 = j + 1;
                        for (i__ = *n; i__ >= i__1; --i__) {
/*<                         TEMP = TEMP - DCONJG( A( I, J ) )*X( IX ) >*/
                            d_cnjg(&z__3, &a[i__ + j * a_dim1]);
                            i__2 = ix;
                            z__2.r = z__3.r * x[i__2].r - z__3.i * x[i__2].i,
                                    z__2.i = z__3.r * x[i__2].i + z__3.i * x[
                                    i__2].r;
                            z__1.r = temp.r - z__2.r, z__1.i = temp.i -
                                    z__2.i;
                            temp.r = z__1.r, temp.i = z__1.i;
/*<                         IX   = IX   - INCX >*/
                            ix -= *incx;
/*<   190                CONTINUE >*/
/* L190: */
                        }
/*<    >*/
                        if (nounit) {
                            d_cnjg(&z__2, &a[j + j * a_dim1]);
                            z_div(&z__1, &temp, &z__2);
                            temp.r = z__1.r, temp.i = z__1.i;
                        }
/*<                   END IF >*/
                    }
/*<                   X( JX ) = TEMP >*/
                    i__1 = jx;
                    x[i__1].r = temp.r, x[i__1].i = temp.i;
/*<                   JX      = JX   - INCX >*/
                    jx -= *incx;
/*<   200          CONTINUE >*/
/* L200: */
                }
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       END IF >*/
    }

/*<       RETURN >*/
    return 0;

/*     End of ZTRSV . */

/*<       END >*/
} /* ztrsv_ */

#ifdef __cplusplus
        }
#endif
