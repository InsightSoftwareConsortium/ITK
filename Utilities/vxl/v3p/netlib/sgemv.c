#include "f2c.h"
#include "netlib.h"

/* Subroutine */ void sgemv_(const char *trans, const integer *m, const integer *n, real *alpha,
                             real *a, const integer *lda, real *x, const integer *incx,
                             real *beta, real *y, const integer *incy)
{
    /* Local variables */
    static integer info;
    static real temp;
    static integer lenx, leny, i, j;
    static integer ix, iy, jx, jy, kx, ky;

/*
    Purpose
    =======

    SGEMV  performs one of the matrix-vector operations

       y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,

    where alpha and beta are scalars, x and y are vectors and A is an
    m by n matrix.

    Parameters
    ==========

    TRANS  - CHARACTER*1.
             On entry, TRANS specifies the operation to be performed as
             follows:

                TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.

                TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.

                TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.

             Unchanged on exit.

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

    A      - SINGLE PRECISION array of DIMENSION ( LDA, n ).
             Before entry, the leading m by n part of the array A must
             contain the matrix of coefficients.
             Unchanged on exit.

    LDA    - INTEGER.
             On entry, LDA specifies the first dimension of A as declared

             in the calling (sub) program. LDA must be at least
             max( 1, m ).
             Unchanged on exit.

    X      - SINGLE PRECISION array of DIMENSION at least
             ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
             and at least
             ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
             Before entry, the incremented array X must contain the
             vector x.
             Unchanged on exit.

    INCX   - INTEGER.
             On entry, INCX specifies the increment for the elements of
             X. INCX must not be zero.
             Unchanged on exit.

    BETA   - SINGLE PRECISION.
             On entry, BETA specifies the scalar beta. When BETA is
             supplied as zero then Y need not be set on input.
             Unchanged on exit.

    Y      - SINGLE PRECISION array of DIMENSION at least
             ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
             and at least
             ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
             Before entry with BETA non-zero, the incremented array Y
             must contain the vector y. On exit, Y is overwritten by the

             updated vector y.

    INCY   - INTEGER.
             On entry, INCY specifies the increment for the elements of
             Y. INCY must not be zero.
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
    if (! lsame_(trans, "N") && ! lsame_(trans, "T") && ! lsame_(trans, "C")) {
        info = 1;
    } else if (*m < 0) {
        info = 2;
    } else if (*n < 0) {
        info = 3;
    } else if (*lda < max(1,*m)) {
        info = 6;
    } else if (*incx == 0) {
        info = 8;
    } else if (*incy == 0) {
        info = 11;
    }
    if (info != 0) {
        xerbla_("SGEMV ", &info);
        return;
    }

/*     Quick return if possible. */

    if (*m == 0 || *n == 0 || (*alpha == 0. && *beta == 1.)) {
        return;
    }

/*     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
       up the start points in  X  and  Y. */

    if (lsame_(trans, "N")) { /* no transpose */
        lenx = *n;
        leny = *m;
    } else { /* transpose */
        lenx = *m;
        leny = *n;
    }
    if (*incx > 0) {
        kx = 0;
    } else {
        kx = - (lenx - 1) * *incx;
    }
    if (*incy > 0) {
        ky = 0;
    } else {
        ky = - (leny - 1) * *incy;
    }

/*     Start the operations. In this version the elements of A are
       accessed sequentially with one pass through A.

       First form  y := beta*y. */

    if (*beta != 1.) {
        if (*incy == 1) {
            if (*beta == 0.) {
                for (i = 0; i < leny; ++i) {
                    y[i] = 0.;
                }
            } else {
                for (i = 0; i < leny; ++i) {
                    y[i] *= *beta;
                }
            }
        } else {
            iy = ky;
            if (*beta == 0.) {
                for (i = 0; i < leny; ++i) {
                    y[iy] = 0.;
                    iy += *incy;
                }
            } else {
                for (i = 0; i < leny; ++i) {
                    y[iy] *= *beta;
                    iy += *incy;
                }
            }
        }
    }
    if (*alpha == 0.) {
        return;
    }
    if (lsame_(trans, "N")) { /* no transpose */

/*        Form  y := alpha*A*x + y. */

        jx = kx;
        if (*incy == 1) {
            for (j = 0; j < *n; ++j) {
                if (x[jx] != 0.) {
                    temp = *alpha * x[jx];
                    for (i = 0; i < *m; ++i) {
                        y[i] += temp * a[i + j* *lda];
                    }
                }
                jx += *incx;
            }
        } else {
            for (j = 0; j < *n; ++j) {
                if (x[jx] != 0.) {
                    temp = *alpha * x[jx];
                    iy = ky;
                    for (i = 0; i < *m; ++i) {
                        y[iy] += temp * a[i + j* *lda];
                        iy += *incy;
                    }
                }
                jx += *incx;
            }
        }
    } else { /* transpose */

/*        Form  y := alpha*A'*x + y. */

        jy = ky;
        if (*incx == 1) {
            for (j = 0; j < *n; ++j) {
                temp = 0.;
                for (i = 0; i < *m; ++i) {
                    temp += a[i + j* *lda] * x[i];
                }
                y[jy] += *alpha * temp;
                jy += *incy;
            }
        } else {
            for (j = 0; j < *n; ++j) {
                temp = 0.;
                ix = kx;
                for (i = 0; i < *m; ++i) {
                    temp += a[i + j* *lda] * x[ix];
                    ix += *incx;
                }
                y[jy] += *alpha * temp;
                jy += *incy;
            }
        }
    }
} /* sgemv_ */
