/* blas/zgeru.f -- translated by f2c (version 20090411).
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

/*<       SUBROUTINE ZGERU(M,N,ALPHA,X,INCX,Y,INCY,A,LDA) >*/
/* Subroutine */ int zgeru_(integer *m, integer *n, doublecomplex *alpha,
        doublecomplex *x, integer *incx, doublecomplex *y, integer *incy,
        doublecomplex *a, integer *lda)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4, i__5;
    doublecomplex z__1, z__2;

    /* Local variables */
    integer i__, j, ix, jy, kx, info;
    doublecomplex temp;
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);

/*     .. Scalar Arguments .. */
/*<       DOUBLE COMPLEX ALPHA >*/
/*<       INTEGER INCX,INCY,LDA,M,N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       DOUBLE COMPLEX A(LDA,*),X(*),Y(*) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZGERU  performs the rank 1 operation */

/*     A := alpha*x*y' + A, */

/*  where alpha is a scalar, x is an m element vector, y is an n element */
/*  vector and A is an m by n matrix. */

/*  Arguments */
/*  ========== */

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

/*  X      - COMPLEX*16       array of dimension at least */
/*           ( 1 + ( m - 1 )*abs( INCX ) ). */
/*           Before entry, the incremented array X must contain the m */
/*           element vector x. */
/*           Unchanged on exit. */

/*  INCX   - INTEGER. */
/*           On entry, INCX specifies the increment for the elements of */
/*           X. INCX must not be zero. */
/*           Unchanged on exit. */

/*  Y      - COMPLEX*16       array of dimension at least */
/*           ( 1 + ( n - 1 )*abs( INCY ) ). */
/*           Before entry, the incremented array Y must contain the n */
/*           element vector y. */
/*           Unchanged on exit. */

/*  INCY   - INTEGER. */
/*           On entry, INCY specifies the increment for the elements of */
/*           Y. INCY must not be zero. */
/*           Unchanged on exit. */

/*  A      - COMPLEX*16       array of DIMENSION ( LDA, n ). */
/*           Before entry, the leading m by n part of the array A must */
/*           contain the matrix of coefficients. On exit, A is */
/*           overwritten by the updated matrix. */

/*  LDA    - INTEGER. */
/*           On entry, LDA specifies the first dimension of A as declared */
/*           in the calling (sub) program. LDA must be at least */
/*           max( 1, m ). */
/*           Unchanged on exit. */


/*  Level 2 Blas routine. */

/*  -- Written on 22-October-1986. */
/*     Jack Dongarra, Argonne National Lab. */
/*     Jeremy Du Croz, Nag Central Office. */
/*     Sven Hammarling, Nag Central Office. */
/*     Richard Hanson, Sandia National Labs. */


/*     .. Parameters .. */
/*<       DOUBLE COMPLEX ZERO >*/
/*<       PARAMETER (ZERO= (0.0D+0,0.0D+0)) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       DOUBLE COMPLEX TEMP >*/
/*<       INTEGER I,INFO,IX,J,JY,KX >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL XERBLA >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC MAX >*/
/*     .. */

/*     Test the input parameters. */

/*<       INFO = 0 >*/
    /* Parameter adjustments */
    --x;
    --y;
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;

    /* Function Body */
    info = 0;
/*<       IF (M.LT.0) THEN >*/
    if (*m < 0) {
/*<           INFO = 1 >*/
        info = 1;
/*<       ELSE IF (N.LT.0) THEN >*/
    } else if (*n < 0) {
/*<           INFO = 2 >*/
        info = 2;
/*<       ELSE IF (INCX.EQ.0) THEN >*/
    } else if (*incx == 0) {
/*<           INFO = 5 >*/
        info = 5;
/*<       ELSE IF (INCY.EQ.0) THEN >*/
    } else if (*incy == 0) {
/*<           INFO = 7 >*/
        info = 7;
/*<       ELSE IF (LDA.LT.MAX(1,M)) THEN >*/
    } else if (*lda < max(1,*m)) {
/*<           INFO = 9 >*/
        info = 9;
/*<       END IF >*/
    }
/*<       IF (INFO.NE.0) THEN >*/
    if (info != 0) {
/*<           CALL XERBLA('ZGERU ',INFO) >*/
        xerbla_("ZGERU ", &info, (ftnlen)6);
/*<           RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible. */

/*<       IF ((M.EQ.0) .OR. (N.EQ.0) .OR. (ALPHA.EQ.ZERO)) RETURN >*/
    if (*m == 0 || *n == 0 || (alpha->r == 0. && alpha->i == 0.)) {
        return 0;
    }

/*     Start the operations. In this version the elements of A are */
/*     accessed sequentially with one pass through A. */

/*<       IF (INCY.GT.0) THEN >*/
    if (*incy > 0) {
/*<           JY = 1 >*/
        jy = 1;
/*<       ELSE >*/
    } else {
/*<           JY = 1 - (N-1)*INCY >*/
        jy = 1 - (*n - 1) * *incy;
/*<       END IF >*/
    }
/*<       IF (INCX.EQ.1) THEN >*/
    if (*incx == 1) {
/*<           DO 20 J = 1,N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<               IF (Y(JY).NE.ZERO) THEN >*/
            i__2 = jy;
            if (y[i__2].r != 0. || y[i__2].i != 0.) {
/*<                   TEMP = ALPHA*Y(JY) >*/
                i__2 = jy;
                z__1.r = alpha->r * y[i__2].r - alpha->i * y[i__2].i, z__1.i =
                         alpha->r * y[i__2].i + alpha->i * y[i__2].r;
                temp.r = z__1.r, temp.i = z__1.i;
/*<                   DO 10 I = 1,M >*/
                i__2 = *m;
                for (i__ = 1; i__ <= i__2; ++i__) {
/*<                       A(I,J) = A(I,J) + X(I)*TEMP >*/
                    i__3 = i__ + j * a_dim1;
                    i__4 = i__ + j * a_dim1;
                    i__5 = i__;
                    z__2.r = x[i__5].r * temp.r - x[i__5].i * temp.i, z__2.i =
                             x[i__5].r * temp.i + x[i__5].i * temp.r;
                    z__1.r = a[i__4].r + z__2.r, z__1.i = a[i__4].i + z__2.i;
                    a[i__3].r = z__1.r, a[i__3].i = z__1.i;
/*<    10             CONTINUE >*/
/* L10: */
                }
/*<               END IF >*/
            }
/*<               JY = JY + INCY >*/
            jy += *incy;
/*<    20     CONTINUE >*/
/* L20: */
        }
/*<       ELSE >*/
    } else {
/*<           IF (INCX.GT.0) THEN >*/
        if (*incx > 0) {
/*<               KX = 1 >*/
            kx = 1;
/*<           ELSE >*/
        } else {
/*<               KX = 1 - (M-1)*INCX >*/
            kx = 1 - (*m - 1) * *incx;
/*<           END IF >*/
        }
/*<           DO 40 J = 1,N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<               IF (Y(JY).NE.ZERO) THEN >*/
            i__2 = jy;
            if (y[i__2].r != 0. || y[i__2].i != 0.) {
/*<                   TEMP = ALPHA*Y(JY) >*/
                i__2 = jy;
                z__1.r = alpha->r * y[i__2].r - alpha->i * y[i__2].i, z__1.i =
                         alpha->r * y[i__2].i + alpha->i * y[i__2].r;
                temp.r = z__1.r, temp.i = z__1.i;
/*<                   IX = KX >*/
                ix = kx;
/*<                   DO 30 I = 1,M >*/
                i__2 = *m;
                for (i__ = 1; i__ <= i__2; ++i__) {
/*<                       A(I,J) = A(I,J) + X(IX)*TEMP >*/
                    i__3 = i__ + j * a_dim1;
                    i__4 = i__ + j * a_dim1;
                    i__5 = ix;
                    z__2.r = x[i__5].r * temp.r - x[i__5].i * temp.i, z__2.i =
                             x[i__5].r * temp.i + x[i__5].i * temp.r;
                    z__1.r = a[i__4].r + z__2.r, z__1.i = a[i__4].i + z__2.i;
                    a[i__3].r = z__1.r, a[i__3].i = z__1.i;
/*<                       IX = IX + INCX >*/
                    ix += *incx;
/*<    30             CONTINUE >*/
/* L30: */
                }
/*<               END IF >*/
            }
/*<               JY = JY + INCY >*/
            jy += *incy;
/*<    40     CONTINUE >*/
/* L40: */
        }
/*<       END IF >*/
    }

/*<       RETURN >*/
    return 0;

/*     End of ZGERU . */

/*<       END >*/
} /* zgeru_ */

#ifdef __cplusplus
        }
#endif
