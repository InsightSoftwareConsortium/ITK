#include "f2c.h"
#include "netlib.h"

/* Subroutine */ void sger_(const integer *m, const integer *n, real *alpha,
                            real *x, const integer *incx, real *y, const integer *incy,
                            real *a, const integer *lda)
{
    /* Local variables */
    static integer info;
    static real temp;
    static integer i, j, ix, jy, kx;

/*
    Purpose
    =======

    SGER   performs the rank 1 operation

       A := alpha*x*y' + A,

    where alpha is a scalar, x is an m element vector, y is an n element

    vector and A is an m by n matrix.

    Parameters
    ==========

    M      - INTEGER.
             On entry, M specifies the number of rows of the matrix A.
             M must be at least zero.
             Unchanged on exit.

    N      - INTEGER.
             On entry, N specifies the number of columns of the matrix A.

             N must be at least zero.
             Unchanged on exit.

    ALPHA  - SINGLE PRECISION.
             On entry, ALPHA specifies the scalar alpha.
             Unchanged on exit.

    X      - SINGLE PRECISION array of dimension at least
             ( 1 + ( m - 1 )*abs( INCX ) ).
             Before entry, the incremented array X must contain the m
             element vector x.
             Unchanged on exit.

    INCX   - INTEGER.
             On entry, INCX specifies the increment for the elements of
             X. INCX must not be zero.
             Unchanged on exit.

    Y      - SINGLE PRECISION array of dimension at least
             ( 1 + ( n - 1 )*abs( INCY ) ).
             Before entry, the incremented array Y must contain the n
             element vector y.
             Unchanged on exit.

    INCY   - INTEGER.
             On entry, INCY specifies the increment for the elements of
             Y. INCY must not be zero.
             Unchanged on exit.

    A      - SINGLE PRECISION array of DIMENSION ( LDA, n ).
             Before entry, the leading m by n part of the array A must
             contain the matrix of coefficients. On exit, A is
             overwritten by the updated matrix.

    LDA    - INTEGER.
             On entry, LDA specifies the first dimension of A as declared

             in the calling (sub) program. LDA must be at least
             max( 1, m ).
             Unchanged on exit.


    Level 2 Blas routine.

    -- Written on 22-October-1986.
       Jack Dongarra, Argonne National Lab.
       Jeremy Du Croz, Nag Central Office.
       Sven Hammarling, Nag Central Office.
       Richard Hanson, Sandia National Labs.
*/

/*     Test the input parameters. */

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
        xerbla_("SGER  ", &info);
        return;
    }

/*     Quick return if possible. */

    if (*m == 0 || *n == 0 || *alpha == 0.) {
        return;
    }

/*     Start the operations. In this version the elements of A are
       accessed sequentially with one pass through A. */

    if (*incy > 0) {
        jy = 0;
    } else {
        jy = (1 - *n) * *incy;
    }
    if (*incx == 1) {
        for (j = 0; j < *n; ++j) {
            if (y[jy] != 0.) {
                temp = *alpha * y[jy];
                for (i = 0; i < *m; ++i) {
                    a[i + j* *lda] += x[i] * temp;
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
            if (y[jy] != 0.) {
                temp = *alpha * y[jy];
                ix = kx;
                for (i = 0; i < *m; ++i) {
                    a[i + j* *lda] += x[ix] * temp;
                    ix += *incx;
                }
            }
            jy += *incy;
        }
    }
} /* sger_ */
