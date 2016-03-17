/* blas/sgemv.f -- translated by f2c (version 20050501).
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
/* Subroutine */ int sgemv_(char *trans, integer *m, integer *n, real *alpha,
        real *a, integer *lda, real *x, integer *incx, real *beta, real *y,
        integer *incy, ftnlen trans_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;

    /* Local variables */
    integer i__, j, ix, iy, jx, jy, kx, ky, info;
    real temp;
    integer lenx, leny;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);
    (void)trans_len;

/*     .. Scalar Arguments .. */
/*<       REAL               ALPHA, BETA >*/
/*<       INTEGER            INCX, INCY, LDA, M, N >*/
/*<       CHARACTER*1        TRANS >*/
/*     .. Array Arguments .. */
/*<       REAL               A( LDA, * ), X( * ), Y( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  SGEMV  performs one of the matrix-vector operations */

/*     y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y, */

/*  where alpha and beta are scalars, x and y are vectors and A is an */
/*  m by n matrix. */

/*  Parameters */
/*  ========== */

/*  TRANS  - CHARACTER*1. */
/*           On entry, TRANS specifies the operation to be performed as */
/*           follows: */

/*              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y. */

/*              TRANS = 'T' or 't'   y := alpha*A'*x + beta*y. */

/*              TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y. */

/*           Unchanged on exit. */

/*  M      - INTEGER. */
/*           On entry, M specifies the number of rows of the matrix A. */
/*           M must be at least zero. */
/*           Unchanged on exit. */

/*  N      - INTEGER. */
/*           On entry, N specifies the number of columns of the matrix A. */
/*           N must be at least zero. */
/*           Unchanged on exit. */

/*  ALPHA  - REAL            . */
/*           On entry, ALPHA specifies the scalar alpha. */
/*           Unchanged on exit. */

/*  A      - REAL             array of DIMENSION ( LDA, n ). */
/*           Before entry, the leading m by n part of the array A must */
/*           contain the matrix of coefficients. */
/*           Unchanged on exit. */

/*  LDA    - INTEGER. */
/*           On entry, LDA specifies the first dimension of A as declared */
/*           in the calling (sub) program. LDA must be at least */
/*           max( 1, m ). */
/*           Unchanged on exit. */

/*  X      - REAL             array of DIMENSION at least */
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

/*  BETA   - REAL            . */
/*           On entry, BETA specifies the scalar beta. When BETA is */
/*           supplied as zero then Y need not be set on input. */
/*           Unchanged on exit. */

/*  Y      - REAL             array of DIMENSION at least */
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
/*<       REAL               ONE         , ZERO >*/
/*<       PARAMETER        ( ONE = 1.0E+0, ZERO = 0.0E+0 ) >*/
/*     .. Local Scalars .. */
/*<       REAL               TEMP >*/
/*<       INTEGER            I, INFO, IX, IY, J, JX, JY, KX, KY, LENX, LENY >*/
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
/*<          CALL XERBLA( 'SGEMV ', INFO ) >*/
        xerbla_("SGEMV ", &info, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible. */

/*<    >*/
    if (*m == 0 || *n == 0 || (*alpha == (float)0. && *beta == (float)1.)) {
        return 0;
    }

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
    if (*beta != (float)1.) {
/*<          IF( INCY.EQ.1 )THEN >*/
        if (*incy == 1) {
/*<             IF( BETA.EQ.ZERO )THEN >*/
            if (*beta == (float)0.) {
/*<                DO 10, I = 1, LENY >*/
                i__1 = leny;
                for (i__ = 1; i__ <= i__1; ++i__) {
/*<                   Y( I ) = ZERO >*/
                    y[i__] = (float)0.;
/*<    10          CONTINUE >*/
/* L10: */
                }
/*<             ELSE >*/
            } else {
/*<                DO 20, I = 1, LENY >*/
                i__1 = leny;
                for (i__ = 1; i__ <= i__1; ++i__) {
/*<                   Y( I ) = BETA*Y( I ) >*/
                    y[i__] = *beta * y[i__];
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
            if (*beta == (float)0.) {
/*<                DO 30, I = 1, LENY >*/
                i__1 = leny;
                for (i__ = 1; i__ <= i__1; ++i__) {
/*<                   Y( IY ) = ZERO >*/
                    y[iy] = (float)0.;
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
                    y[iy] = *beta * y[iy];
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
    if (*alpha == (float)0.) {
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
                if (x[jx] != (float)0.) {
/*<                   TEMP = ALPHA*X( JX ) >*/
                    temp = *alpha * x[jx];
/*<                   DO 50, I = 1, M >*/
                    i__2 = *m;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      Y( I ) = Y( I ) + TEMP*A( I, J ) >*/
                        y[i__] += temp * a[i__ + j * a_dim1];
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
                if (x[jx] != (float)0.) {
/*<                   TEMP = ALPHA*X( JX ) >*/
                    temp = *alpha * x[jx];
/*<                   IY   = KY >*/
                    iy = ky;
/*<                   DO 70, I = 1, M >*/
                    i__2 = *m;
                    for (i__ = 1; i__ <= i__2; ++i__) {
/*<                      Y( IY ) = Y( IY ) + TEMP*A( I, J ) >*/
                        y[iy] += temp * a[i__ + j * a_dim1];
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

/*        Form  y := alpha*A'*x + y. */

/*<          JY = KY >*/
        jy = ky;
/*<          IF( INCX.EQ.1 )THEN >*/
        if (*incx == 1) {
/*<             DO 100, J = 1, N >*/
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
/*<                TEMP = ZERO >*/
                temp = (float)0.;
/*<                DO 90, I = 1, M >*/
                i__2 = *m;
                for (i__ = 1; i__ <= i__2; ++i__) {
/*<                   TEMP = TEMP + A( I, J )*X( I ) >*/
                    temp += a[i__ + j * a_dim1] * x[i__];
/*<    90          CONTINUE >*/
/* L90: */
                }
/*<                Y( JY ) = Y( JY ) + ALPHA*TEMP >*/
                y[jy] += *alpha * temp;
/*<                JY      = JY      + INCY >*/
                jy += *incy;
/*<   100       CONTINUE >*/
/* L100: */
            }
/*<          ELSE >*/
        } else {
/*<             DO 120, J = 1, N >*/
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
/*<                TEMP = ZERO >*/
                temp = (float)0.;
/*<                IX   = KX >*/
                ix = kx;
/*<                DO 110, I = 1, M >*/
                i__2 = *m;
                for (i__ = 1; i__ <= i__2; ++i__) {
/*<                   TEMP = TEMP + A( I, J )*X( IX ) >*/
                    temp += a[i__ + j * a_dim1] * x[ix];
/*<                   IX   = IX   + INCX >*/
                    ix += *incx;
/*<   110          CONTINUE >*/
/* L110: */
                }
/*<                Y( JY ) = Y( JY ) + ALPHA*TEMP >*/
                y[jy] += *alpha * temp;
/*<                JY      = JY      + INCY >*/
                jy += *incy;
/*<   120       CONTINUE >*/
/* L120: */
            }
/*<          END IF >*/
        }
/*<       END IF >*/
    }

/*<       RETURN >*/
    return 0;

/*     End of SGEMV . */

/*<       END >*/
} /* sgemv_ */

#ifdef __cplusplus
        }
#endif
