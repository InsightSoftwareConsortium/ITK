/* blas/zgemv.f -- translated by f2c (version 20050501).
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

/*<    >*/
/* Subroutine */ int zgemv_(char *trans, integer *m, integer *n,
        doublecomplex *alpha, doublecomplex *a, integer *lda, doublecomplex *
        x, integer *incx, doublecomplex *beta, doublecomplex *y, integer *
        incy, ftnlen trans_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4, i__5;
    doublecomplex z__1, z__2, z__3;

    /* Builtin functions */
    void d_cnjg(doublecomplex *, doublecomplex *);

    /* Local variables */
    integer i__, j, ix, iy, jx, jy, kx, ky, info;
    doublecomplex temp;
    integer lenx, leny;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);
    logical noconj;
    (void)trans_len;

/*     .. Scalar Arguments .. */
/*<       COMPLEX*16         ALPHA, BETA >*/
/*<       INTEGER            INCX, INCY, LDA, M, N >*/
/*<       CHARACTER*1        TRANS >*/
/*     .. Array Arguments .. */
/*<       COMPLEX*16         A( LDA, * ), X( * ), Y( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZGEMV  performs one of the matrix-vector operations */

/*     y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,   or */

/*     y := alpha*conjg( A' )*x + beta*y, */

/*  where alpha and beta are scalars, x and y are vectors and A is an */
/*  m by n matrix. */

/*  Parameters */
/*  ========== */

/*  TRANS  - CHARACTER*1. */
/*           On entry, TRANS specifies the operation to be performed as */
/*           follows: */

/*              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y. */

/*              TRANS = 'T' or 't'   y := alpha*A'*x + beta*y. */

/*              TRANS = 'C' or 'c'   y := alpha*conjg( A' )*x + beta*y. */

/*           Unchanged on exit. */

/*  M      - INTEGER. */
/*           On entry, M specifies the number of rows of the matrix A. */
/*           M must be at least zero. */
/*           Unchanged on exit. */

/*  N      - INTEGER. */
/*           On entry, N specifies the number of columns of the matrix A. */
/*           N must be at least zero. */
/*           Unchanged on exit. */

/*  ALPHA  - COMPLEX*16      . */
/*           On entry, ALPHA specifies the scalar alpha. */
/*           Unchanged on exit. */

/*  A      - COMPLEX*16       array of DIMENSION ( LDA, n ). */
/*           Before entry, the leading m by n part of the array A must */
/*           contain the matrix of coefficients. */
/*           Unchanged on exit. */

/*  LDA    - INTEGER. */
/*           On entry, LDA specifies the first dimension of A as declared */
/*           in the calling (sub) program. LDA must be at least */
/*           max( 1, m ). */
/*           Unchanged on exit. */

/*  X      - COMPLEX*16       array of DIMENSION at least */
/*           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n' */
/*           and at least */
/*           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise. */
/*           Before entry, the incremented array X must contain the */
/*           vector x. */
/*           Unchanged on exit. */

/*  INCX   - INTEGER. */
/*           On entry, INCX specifies the increment for the elements of */
/*           X. INCX must not be zero. */
/*           Unchanged on exit. */

/*  BETA   - COMPLEX*16      . */
/*           On entry, BETA specifies the scalar beta. When BETA is */
/*           supplied as zero then Y need not be set on input. */
/*           Unchanged on exit. */

/*  Y      - COMPLEX*16       array of DIMENSION at least */
/*           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n' */
/*           and at least */
/*           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise. */
/*           Before entry with BETA non-zero, the incremented array Y */
/*           must contain the vector y. On exit, Y is overwritten by the */
/*           updated vector y. */

/*  INCY   - INTEGER. */
/*           On entry, INCY specifies the increment for the elements of */
/*           Y. INCY must not be zero. */
/*           Unchanged on exit. */


/*  Level 2 Blas routine. */

/*  -- Written on 22-October-1986. */
/*     Jack Dongarra, Argonne National Lab. */
/*     Jeremy Du Croz, Nag Central Office. */
/*     Sven Hammarling, Nag Central Office. */
/*     Richard Hanson, Sandia National Labs. */


/*     .. Parameters .. */
/*<       COMPLEX*16         ONE >*/
/*<       PARAMETER        ( ONE  = ( 1.0D+0, 0.0D+0 ) ) >*/
/*<       COMPLEX*16         ZERO >*/
/*<       PARAMETER        ( ZERO = ( 0.0D+0, 0.0D+0 ) ) >*/
/*     .. Local Scalars .. */
/*<       COMPLEX*16         TEMP >*/
/*<       INTEGER            I, INFO, IX, IY, J, JX, JY, KX, KY, LENX, LENY >*/
/*<       LOGICAL            NOCONJ >*/
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
    --y;

    /* Function Body */
    info = 0;
/*<    >*/
    if (! lsame_(trans, "N", (ftnlen)1, (ftnlen)1) && ! lsame_(trans, "T", (
            ftnlen)1, (ftnlen)1) && ! lsame_(trans, "C", (ftnlen)1, (ftnlen)1)
            ) {
/*<          INFO = 1 >*/
        info = 1;
/*<       ELSE IF( M.LT.0 )THEN >*/
    } else if (*m < 0) {
/*<          INFO = 2 >*/
        info = 2;
/*<       ELSE IF( N.LT.0 )THEN >*/
    } else if (*n < 0) {
/*<          INFO = 3 >*/
        info = 3;
/*<       ELSE IF( LDA.LT.MAX( 1, M ) )THEN >*/
    } else if (*lda < max(1,*m)) {
/*<          INFO = 6 >*/
        info = 6;
/*<       ELSE IF( INCX.EQ.0 )THEN >*/
    } else if (*incx == 0) {
/*<          INFO = 8 >*/
        info = 8;
/*<       ELSE IF( INCY.EQ.0 )THEN >*/
    } else if (*incy == 0) {
/*<          INFO = 11 >*/
        info = 11;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 )THEN >*/
    if (info != 0) {
/*<          CALL XERBLA( 'ZGEMV ', INFO ) >*/
        xerbla_("ZGEMV ", &info, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible. */

/*<    >*/
    if (*m == 0 || *n == 0 || (alpha->r == 0. && alpha->i == 0. && (beta->r ==
            1. && beta->i == 0.))) {
        return 0;
    }

/*<       NOCONJ = LSAME( TRANS, 'T' ) >*/
    noconj = lsame_(trans, "T", (ftnlen)1, (ftnlen)1);

/*     Set  LENX  and  LENY, the lengths of the vectors x and y, and set */
/*     up the start points in  X  and  Y. */

/*<       IF( LSAME( TRANS, 'N' ) )THEN >*/
    if (lsame_(trans, "N", (ftnlen)1, (ftnlen)1)) {
/*<          LENX = N >*/
        lenx = *n;
/*<          LENY = M >*/
        leny = *m;
/*<       ELSE >*/
    } else {
/*<          LENX = M >*/
        lenx = *m;
/*<          LENY = N >*/
        leny = *n;
/*<       END IF >*/
    }
/*<       IF( INCX.GT.0 )THEN >*/
    if (*incx > 0) {
/*<          KX = 1 >*/
        kx = 1;
/*<       ELSE >*/
    } else {
/*<          KX = 1 - ( LENX - 1 )*INCX >*/
        kx = 1 - (lenx - 1) * *incx;
/*<       END IF >*/
    }
/*<       IF( INCY.GT.0 )THEN >*/
    if (*incy > 0) {
/*<          KY = 1 >*/
        ky = 1;
/*<       ELSE >*/
    } else {
/*<          KY = 1 - ( LENY - 1 )*INCY >*/
        ky = 1 - (leny - 1) * *incy;
/*<       END IF >*/
    }

/*     Start the operations. In this version the elements of A are */
/*     accessed sequentially with one pass through A. */

/*     First form  y := beta*y. */

/*<       IF( BETA.NE.ONE )THEN >*/
    if (beta->r != 1. || beta->i != 0.) {
/*<          IF( INCY.EQ.1 )THEN >*/
        if (*incy == 1) {
/*<             IF( BETA.EQ.ZERO )THEN >*/
            if (beta->r == 0. && beta->i == 0.) {
/*<                DO 10, I = 1, LENY >*/
                i__1 = leny;
                for (i__ = 1; i__ <= i__1; ++i__) {
/*<                   Y( I ) = ZERO >*/
                    i__2 = i__;
                    y[i__2].r = 0., y[i__2].i = 0.;
/*<    10          CONTINUE >*/
/* L10: */
                }
/*<             ELSE >*/
            } else {
/*<                DO 20, I = 1, LENY >*/
                i__1 = leny;
                for (i__ = 1; i__ <= i__1; ++i__) {
/*<                   Y( I ) = BETA*Y( I ) >*/
                    i__2 = i__;
                    i__3 = i__;
                    z__1.r = beta->r * y[i__3].r - beta->i * y[i__3].i,
                            z__1.i = beta->r * y[i__3].i + beta->i * y[i__3]
                            .r;
                    y[i__2].r = z__1.r, y[i__2].i = z__1.i;
/*<    20          CONTINUE >*/
/* L20: */
                }
/*<             END IF >*/
            }
/*<          ELSE >*/
        } else {
/*<             IY = KY >*/
            iy = ky;
/*<             IF( BETA.EQ.ZERO )THEN >*/
            if (beta->r == 0. && beta->i == 0.) {
/*<                DO 30, I = 1, LENY >*/
                i__1 = leny;
                for (i__ = 1; i__ <= i__1; ++i__) {
/*<                   Y( IY ) = ZERO >*/
                    i__2 = iy;
                    y[i__2].r = 0., y[i__2].i = 0.;
/*<                   IY      = IY   + INCY >*/
                    iy += *incy;
/*<    30          CONTINUE >*/
/* L30: */
                }
/*<             ELSE >*/
            } else {
/*<                DO 40, I = 1, LENY >*/
                i__1 = leny;
                for (i__ = 1; i__ <= i__1; ++i__) {
/*<                   Y( IY ) = BETA*Y( IY ) >*/
                    i__2 = iy;
                    i__3 = iy;
                    z__1.r = beta->r * y[i__3].r - beta->i * y[i__3].i,
                            z__1.i = beta->r * y[i__3].i + beta->i * y[i__3]
                            .r;
                    y[i__2].r = z__1.r, y[i__2].i = z__1.i;
/*<                   IY      = IY           + INCY >*/
                    iy += *incy;
/*<    40          CONTINUE >*/
/* L40: */
                }
/*<             END IF >*/
            }
/*<          END IF >*/
        }
/*<       END IF >*/
    }
/*<    >*/
    if (alpha->r == 0. && alpha->i == 0.) {
        return 0;
    }
/*<       IF( LSAME( TRANS, 'N' ) )THEN >*/
    if (lsame_(trans, "N", (ftnlen)1, (ftnlen)1)) {

/*        Form  y := alpha*A*x + y. */

/*<          JX = KX >*/
        jx = kx;
/*<          IF( INCY.EQ.1 )THEN >*/
        if (*incy == 1) {
/*<             DO 60, J = 1, N >*/
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
/*<                IF( X( JX ).NE.ZERO )THEN >*/
                i__2 = jx;
                if (x[i__2].r != 0. || x[i__2].i != 0.) {
/*<                   TEMP = ALPHA*X( JX ) >*/
                    i__2 = jx;
                    z__1.r = alpha->r * x[i__2].r - alpha->i * x[i__2].i,
                            z__1.i = alpha->r * x[i__2].i + alpha->i * x[i__2]
                            .r;
                    temp.r = z__1.r, temp.i = z__1.i;
/*<                   DO 50, I = 1, M >*/
                    i__2 = *m;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      Y( I ) = Y( I ) + TEMP*A( I, J ) >*/
                        i__3 = i__;
                        i__4 = i__;
                        i__5 = i__ + j * a_dim1;
                        z__2.r = temp.r * a[i__5].r - temp.i * a[i__5].i,
                                z__2.i = temp.r * a[i__5].i + temp.i * a[i__5]
                                .r;
                        z__1.r = y[i__4].r + z__2.r, z__1.i = y[i__4].i +
                                z__2.i;
                        y[i__3].r = z__1.r, y[i__3].i = z__1.i;
/*<    50             CONTINUE >*/
/* L50: */
                    }
/*<                END IF >*/
                }
/*<                JX = JX + INCX >*/
                jx += *incx;
/*<    60       CONTINUE >*/
/* L60: */
            }
/*<          ELSE >*/
        } else {
/*<             DO 80, J = 1, N >*/
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
/*<                IF( X( JX ).NE.ZERO )THEN >*/
                i__2 = jx;
                if (x[i__2].r != 0. || x[i__2].i != 0.) {
/*<                   TEMP = ALPHA*X( JX ) >*/
                    i__2 = jx;
                    z__1.r = alpha->r * x[i__2].r - alpha->i * x[i__2].i,
                            z__1.i = alpha->r * x[i__2].i + alpha->i * x[i__2]
                            .r;
                    temp.r = z__1.r, temp.i = z__1.i;
/*<                   IY   = KY >*/
                    iy = ky;
/*<                   DO 70, I = 1, M >*/
                    i__2 = *m;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      Y( IY ) = Y( IY ) + TEMP*A( I, J ) >*/
                        i__3 = iy;
                        i__4 = iy;
                        i__5 = i__ + j * a_dim1;
                        z__2.r = temp.r * a[i__5].r - temp.i * a[i__5].i,
                                z__2.i = temp.r * a[i__5].i + temp.i * a[i__5]
                                .r;
                        z__1.r = y[i__4].r + z__2.r, z__1.i = y[i__4].i +
                                z__2.i;
                        y[i__3].r = z__1.r, y[i__3].i = z__1.i;
/*<                      IY      = IY      + INCY >*/
                        iy += *incy;
/*<    70             CONTINUE >*/
/* L70: */
                    }
/*<                END IF >*/
                }
/*<                JX = JX + INCX >*/
                jx += *incx;
/*<    80       CONTINUE >*/
/* L80: */
            }
/*<          END IF >*/
        }
/*<       ELSE >*/
    } else {

/*        Form  y := alpha*A'*x + y  or  y := alpha*conjg( A' )*x + y. */

/*<          JY = KY >*/
        jy = ky;
/*<          IF( INCX.EQ.1 )THEN >*/
        if (*incx == 1) {
/*<             DO 110, J = 1, N >*/
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
/*<                TEMP = ZERO >*/
                temp.r = 0., temp.i = 0.;
/*<                IF( NOCONJ )THEN >*/
                if (noconj) {
/*<                   DO 90, I = 1, M >*/
                    i__2 = *m;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      TEMP = TEMP + A( I, J )*X( I ) >*/
                        i__3 = i__ + j * a_dim1;
                        i__4 = i__;
                        z__2.r = a[i__3].r * x[i__4].r - a[i__3].i * x[i__4]
                                .i, z__2.i = a[i__3].r * x[i__4].i + a[i__3]
                                .i * x[i__4].r;
                        z__1.r = temp.r + z__2.r, z__1.i = temp.i + z__2.i;
                        temp.r = z__1.r, temp.i = z__1.i;
/*<    90             CONTINUE >*/
/* L90: */
                    }
/*<                ELSE >*/
                } else {
/*<                   DO 100, I = 1, M >*/
                    i__2 = *m;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      TEMP = TEMP + DCONJG( A( I, J ) )*X( I ) >*/
                        d_cnjg(&z__3, &a[i__ + j * a_dim1]);
                        i__3 = i__;
                        z__2.r = z__3.r * x[i__3].r - z__3.i * x[i__3].i,
                                z__2.i = z__3.r * x[i__3].i + z__3.i * x[i__3]
                                .r;
                        z__1.r = temp.r + z__2.r, z__1.i = temp.i + z__2.i;
                        temp.r = z__1.r, temp.i = z__1.i;
/*<   100             CONTINUE >*/
/* L100: */
                    }
/*<                END IF >*/
                }
/*<                Y( JY ) = Y( JY ) + ALPHA*TEMP >*/
                i__2 = jy;
                i__3 = jy;
                z__2.r = alpha->r * temp.r - alpha->i * temp.i, z__2.i =
                        alpha->r * temp.i + alpha->i * temp.r;
                z__1.r = y[i__3].r + z__2.r, z__1.i = y[i__3].i + z__2.i;
                y[i__2].r = z__1.r, y[i__2].i = z__1.i;
/*<                JY      = JY      + INCY >*/
                jy += *incy;
/*<   110       CONTINUE >*/
/* L110: */
            }
/*<          ELSE >*/
        } else {
/*<             DO 140, J = 1, N >*/
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
/*<                TEMP = ZERO >*/
                temp.r = 0., temp.i = 0.;
/*<                IX   = KX >*/
                ix = kx;
/*<                IF( NOCONJ )THEN >*/
                if (noconj) {
/*<                   DO 120, I = 1, M >*/
                    i__2 = *m;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      TEMP = TEMP + A( I, J )*X( IX ) >*/
                        i__3 = i__ + j * a_dim1;
                        i__4 = ix;
                        z__2.r = a[i__3].r * x[i__4].r - a[i__3].i * x[i__4]
                                .i, z__2.i = a[i__3].r * x[i__4].i + a[i__3]
                                .i * x[i__4].r;
                        z__1.r = temp.r + z__2.r, z__1.i = temp.i + z__2.i;
                        temp.r = z__1.r, temp.i = z__1.i;
/*<                      IX   = IX   + INCX >*/
                        ix += *incx;
/*<   120             CONTINUE >*/
/* L120: */
                    }
/*<                ELSE >*/
                } else {
/*<                   DO 130, I = 1, M >*/
                    i__2 = *m;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      TEMP = TEMP + DCONJG( A( I, J ) )*X( IX ) >*/
                        d_cnjg(&z__3, &a[i__ + j * a_dim1]);
                        i__3 = ix;
                        z__2.r = z__3.r * x[i__3].r - z__3.i * x[i__3].i,
                                z__2.i = z__3.r * x[i__3].i + z__3.i * x[i__3]
                                .r;
                        z__1.r = temp.r + z__2.r, z__1.i = temp.i + z__2.i;
                        temp.r = z__1.r, temp.i = z__1.i;
/*<                      IX   = IX   + INCX >*/
                        ix += *incx;
/*<   130             CONTINUE >*/
/* L130: */
                    }
/*<                END IF >*/
                }
/*<                Y( JY ) = Y( JY ) + ALPHA*TEMP >*/
                i__2 = jy;
                i__3 = jy;
                z__2.r = alpha->r * temp.r - alpha->i * temp.i, z__2.i =
                        alpha->r * temp.i + alpha->i * temp.r;
                z__1.r = y[i__3].r + z__2.r, z__1.i = y[i__3].i + z__2.i;
                y[i__2].r = z__1.r, y[i__2].i = z__1.i;
/*<                JY      = JY      + INCY >*/
                jy += *incy;
/*<   140       CONTINUE >*/
/* L140: */
            }
/*<          END IF >*/
        }
/*<       END IF >*/
    }

/*<       RETURN >*/
    return 0;

/*     End of ZGEMV . */

/*<       END >*/
} /* zgemv_ */

#ifdef __cplusplus
        }
#endif
