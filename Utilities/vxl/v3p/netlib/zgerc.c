#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Subroutine */ void zgerc_(m, n, alpha, x, incx, y, incy, a, lda)
const integer *m, *n;
doublecomplex *alpha, *x;
const integer *incx;
doublecomplex *y;
const integer *incy;
doublecomplex *a;
const integer *lda;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer info;
    static doublecomplex temp;
    static integer i, j, ix, jy, kx;

/*  ===================================================================== */
/*                                                                        */
/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  ZGERC  performs the rank 1 operation                                  */
/*                                                                        */
/*     A := alpha*x*conjg( y' ) + A,                                      */
/*                                                                        */
/*  where alpha is a scalar, x is an m element vector, y is an n element  */
/*  vector and A is an m by n matrix.                                     */
/*                                                                        */
/*  Parameters                                                            */
/*  ==========                                                            */
/*                                                                        */
/*  M      - INTEGER.                                                     */
/*           On entry, M specifies the number of rows of the matrix A.    */
/*           M must be at least zero.                                     */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  N      - INTEGER.                                                     */
/*           On entry, N specifies the number of columns of the matrix A. */
/*           N must be at least zero.                                     */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  ALPHA  - COMPLEX*16      .                                            */
/*           On entry, ALPHA specifies the scalar alpha.                  */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  X      - COMPLEX*16       array of dimension at least                 */
/*           ( 1 + ( m - 1 )*abs( INCX ) ).                               */
/*           Before entry, the incremented array X must contain the m     */
/*           element vector x.                                            */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  INCX   - INTEGER.                                                     */
/*           On entry, INCX specifies the increment for the elements of   */
/*           X. INCX must not be zero.                                    */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  Y      - COMPLEX*16       array of dimension at least                 */
/*           ( 1 + ( n - 1 )*abs( INCY ) ).                               */
/*           Before entry, the incremented array Y must contain the n     */
/*           element vector y.                                            */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  INCY   - INTEGER.                                                     */
/*           On entry, INCY specifies the increment for the elements of   */
/*           Y. INCY must not be zero.                                    */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  A      - COMPLEX*16       array of DIMENSION ( LDA, n ).              */
/*           Before entry, the leading m by n part of the array A must    */
/*           contain the matrix of coefficients. On exit, A is            */
/*           overwritten by the updated matrix.                           */
/*                                                                        */
/*  LDA    - INTEGER.                                                     */
/*           On entry, LDA specifies the first dimension of A as declared */
/*           in the calling (sub) program. LDA must be at least           */
/*           max( 1, m ).                                                 */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  ===================================================================== */

/*  Level 2 Blas routine. */

/*  -- Written on 22-October-1986. */
/*     Jack Dongarra, Argonne National Lab. */
/*     Jeremy Du Croz, Nag Central Office. */
/*     Sven Hammarling, Nag Central Office. */
/*     Richard Hanson, Sandia National Labs. */

    info = 0;
    if (*m < 0) {
        info = 1;
    } else if (*n < 0) {
        info = 2;
    } else if (*incx == 0) {
        info = 5;
    } else if (*incy == 0) {
        info = 7;
    } else if (*lda < max(1,*m)) {
        info = 9;
    }
    if (info != 0) {
        xerbla_("ZGERC ", &info);
        return;
    }

/*     Quick return if possible. */
    if (*m == 0 || *n == 0 || (alpha->r == 0. && alpha->i == 0.)) {
        return;
    }

/*     Start the operations. In this version the elements of A are */
/*     accessed sequentially with one pass through A. */

    if (*incy > 0) {
        jy = 0;
    } else {
        jy = (1 - *n) * *incy;
    }
    if (*incx == 1) {
        for (j = 0; j < *n; ++j) {
            if (y[jy].r != 0. || y[jy].i != 0.) {
                temp.r = alpha->r * y[jy].r + alpha->i * y[jy].i,
                temp.i = alpha->i * y[jy].r - alpha->r * y[jy].i;
                for (i = 0; i < *m; ++i) {
                    i__1 = i + j * *lda; /* index [i,j] */
                    a[i__1].r += x[i].r * temp.r - x[i].i * temp.i,
                    a[i__1].i += x[i].r * temp.i + x[i].i * temp.r;
                }
            }
            jy += *incy;
        }
    } else {
        if (*incx > 0) {
            kx = 0;
        } else {
            kx = (1 - *m) * *incx;
        }
        for (j = 0; j < *n; ++j) {
            if (y[jy].r != 0. || y[jy].i != 0.) {
                temp.r = alpha->r * y[jy].r + alpha->i * y[jy].i,
                temp.i = alpha->i * y[jy].r - alpha->r * y[jy].i;
                ix = kx;
                for (i = 0; i < *m; ++i) {
                    i__1 = i + j * *lda; /* index [i,j] */
                    a[i__1].r += x[ix].r * temp.r - x[ix].i * temp.i,
                    a[i__1].i += x[ix].r * temp.i + x[ix].i * temp.r;
                    ix += *incx;
                }
            }
            jy += *incy;
        }
    }
} /* zgerc_ */
