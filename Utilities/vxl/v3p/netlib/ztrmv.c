#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Subroutine */ void ztrmv_(uplo, trans, diag, n, a, lda, x, incx)
const char *uplo, *trans, *diag;
const integer *n;
doublecomplex *a;
const integer *lda;
doublecomplex *x;
const integer *incx;
{
    /* System generated locals */
    integer i__1;
    doublecomplex z__1;

    /* Local variables */
    static integer info;
    static doublecomplex temp;
    static integer i, j;
    static integer ix, jx, kx;
    static logical noconj, nounit;

/************************************************************************/
/*                                                                      */
/*  Purpose                                                             */
/*  =======                                                             */
/*                                                                      */
/*  ZTRMV  performs one of the matrix-vector operations                 */
/*                                                                      */
/*     x := A*x,   or   x := A'*x,   or   x := conjg( A' )*x,           */
/*                                                                      */
/*  where x is an n element vector and  A is an n by n unit, or         */
/*  non-unit, upper or lower triangular matrix.                         */
/*                                                                      */
/*  Parameters                                                          */
/*  ==========                                                          */
/*                                                                      */
/*  UPLO   - CHARACTER*1.                                               */
/*           On entry, UPLO specifies whether the matrix is an upper or */
/*           lower triangular matrix as follows:                        */
/*                                                                      */
/*              UPLO = 'U' or 'u'   A is an upper triangular matrix.    */
/*                                                                      */
/*              UPLO = 'L' or 'l'   A is a lower triangular matrix.     */
/*                                                                      */
/*           Unchanged on exit.                                         */
/*                                                                      */
/*  TRANS  - CHARACTER*1.                                               */
/*           On entry, TRANS specifies the operation to be performed as */
/*           follows:                                                   */
/*                                                                      */
/*              TRANS = 'N' or 'n'   x := A*x.                          */
/*                                                                      */
/*              TRANS = 'T' or 't'   x := A'*x.                         */
/*                                                                      */
/*              TRANS = 'C' or 'c'   x := conjg( A' )*x.                */
/*                                                                      */
/*           Unchanged on exit.                                         */
/*                                                                      */
/*  DIAG   - CHARACTER*1.                                               */
/*           On entry, DIAG specifies whether or not A is unit          */
/*           triangular as follows:                                     */
/*                                                                      */
/*              DIAG = 'U' or 'u'   A is assumed to be unit triangular. */
/*                                                                      */
/*              DIAG = 'N' or 'n'   A is not assumed to be unit         */
/*                                  triangular.                         */
/*                                                                      */
/*           Unchanged on exit.                                         */
/*                                                                      */
/*  N      - INTEGER.                                                   */
/*           On entry, N specifies the order of the matrix A.           */
/*           N must be at least zero.                                   */
/*           Unchanged on exit.                                         */
/*                                                                      */
/*  A      - COMPLEX*16       array of DIMENSION ( LDA, n ).            */
/*           Before entry with  UPLO = 'U' or 'u', the leading n by n   */
/*           upper triangular part of the array A must contain the upper*/
/*           triangular matrix and the strictly lower triangular part of*/
/*           A is not referenced.                                       */
/*           Before entry with UPLO = 'L' or 'l', the leading n by n    */
/*           lower triangular part of the array A must contain the lower*/
/*           triangular matrix and the strictly upper triangular part of*/
/*           A is not referenced.                                       */
/*           Note that when  DIAG = 'U' or 'u', the diagonal elements of*/
/*           A are not referenced either, but are assumed to be unity.  */
/*           Unchanged on exit.                                         */
/*                                                                      */
/*  LDA    - INTEGER.                                                   */
/*           On entry, LDA specifies the first dimension of A as        */
/*           declared in the calling (sub) program. LDA must be at      */
/*           least max( 1, n ).                                         */
/*           Unchanged on exit.                                         */
/*                                                                      */
/*  X      - COMPLEX*16       array of dimension at least               */
/*           ( 1 + ( n - 1 )*abs( INCX ) ).                             */
/*           Before entry, the incremented array X must contain the n   */
/*           element vector x. On exit, X is overwritten with the       */
/*           transformed vector x.                                       */
/*                                                                      */
/*  INCX   - INTEGER.                                                   */
/*           On entry, INCX specifies the increment for the elements of */
/*           X. INCX must not be zero.                                  */
/*           Unchanged on exit.                                         */
/*                                                                      */
/************************************************************************/

/*  Level 2 Blas routine. */

/*  -- Written on 22-October-1986. */
/*     Jack Dongarra, Argonne National Lab. */
/*     Jeremy Du Croz, Nag Central Office. */
/*     Sven Hammarling, Nag Central Office. */
/*     Richard Hanson, Sandia National Labs. */

    info = 0;
    if (! lsame_(uplo, "U") && ! lsame_(uplo, "L")) {
        info = 1;
    } else if (! lsame_(trans, "N") && ! lsame_(trans, "T") && ! lsame_(trans, "C")) {
        info = 2;
    } else if (! lsame_(diag, "U") && ! lsame_(diag, "N")) {
        info = 3;
    } else if (*n < 0) {
        info = 4;
    } else if (*lda < max(1,*n)) {
        info = 6;
    } else if (*incx == 0) {
        info = 8;
    }
    if (info != 0) {
        xerbla_("ZTRMV ", &info);
        return;
    }

/*     Quick return if possible. */

    if (*n == 0) {
        return;
    }

    noconj = lsame_(trans, "T");
    nounit = lsame_(diag, "N");

/*     Set up the start point in X if the increment is not unity. This */
/*     will be  ( N - 1 )*INCX  too small for descending loops. */

    if (*incx <= 0) {
        kx = (1 - *n) * *incx;
    } else if (*incx != 1) {
        kx = 0;
    }

/*     Start the operations. In this version the elements of A are */
/*     accessed sequentially with one pass through A. */

    if (lsame_(trans, "N")) {

/*        Form  x := A*x. */

        if (lsame_(uplo, "U")) {
            if (*incx == 1) {
                for (j = 0; j < *n; ++j) {

                    if (x[j].r != 0. || x[j].i != 0.) {

                        temp.r = x[j].r, temp.i = x[j].i;
                        for (i = 0; i < j; ++i) {
                            i__1 = i + j * *lda;
                            x[i].r += temp.r * a[i__1].r - temp.i * a[i__1].i,
                            x[i].i += temp.r * a[i__1].i + temp.i * a[i__1].r;
                        }
                        if (nounit) {
                            i__1 = j + j * *lda;
                            z__1.r = x[j].r * a[i__1].r - x[j].i * a[i__1].i,
                            z__1.i = x[j].r * a[i__1].i + x[j].i * a[i__1].r;
                            x[j].r = z__1.r, x[j].i = z__1.i;
                        }
                    }
                }
            } else {
                jx = kx;
                for (j = 0; j < *n; ++j) {
                    if (x[jx].r != 0. || x[jx].i != 0.) {
                        temp.r = x[jx].r, temp.i = x[jx].i;
                        ix = kx;
                        for (i = 0; i < j; ++i) {
                            i__1 = i + j * *lda;
                            x[ix].r += temp.r * a[i__1].r - temp.i * a[i__1].i,
                            x[ix].i += temp.r * a[i__1].i + temp.i * a[i__1].r;
                            ix += *incx;
                        }
                        if (nounit) {
                            i__1 = j + j * *lda;
                            z__1.r = x[jx].r * a[i__1].r - x[jx].i * a[i__1].i,
                            z__1.i = x[jx].r * a[i__1].i + x[jx].i * a[i__1].r;
                            x[jx].r = z__1.r, x[jx].i = z__1.i;
                        }
                    }
                    jx += *incx;
                }
            }
        } else {
            if (*incx == 1) {
                for (j = *n - 1; j >= 0; --j) {

                    if (x[j].r != 0. || x[j].i != 0.) {

                        temp.r = x[j].r, temp.i = x[j].i;
                        for (i = *n - 1; i > j; --i) {
                            i__1 = i + j * *lda;
                            x[i].r += temp.r * a[i__1].r - temp.i * a[i__1].i,
                            x[i].i += temp.r * a[i__1].i + temp.i * a[i__1].r;
                        }
                        if (nounit) {
                            i__1 = j + j * *lda;
                            z__1.r = x[j].r * a[i__1].r - x[j].i * a[i__1].i,
                            z__1.i = x[j].r * a[i__1].i + x[j].i * a[i__1].r;
                            x[j].r = z__1.r, x[j].i = z__1.i;
                        }
                    }
                }
            } else {
                kx += (*n - 1) * *incx;
                jx = kx;
                for (j = *n - 1; j >= 0; --j) {
                    if (x[jx].r != 0. || x[jx].i != 0.) {
                        temp.r = x[jx].r, temp.i = x[jx].i;
                        ix = kx;
                        for (i = *n - 1; i > j; --i) {
                            i__1 = i + j * *lda;
                            x[ix].r += temp.r * a[i__1].r - temp.i * a[i__1].i,
                            x[ix].i += temp.r * a[i__1].i + temp.i * a[i__1].r;
                            ix -= *incx;
                        }
                        if (nounit) {
                            i__1 = j + j * *lda;
                            x[jx].r = x[jx].r * a[i__1].r - x[jx].i * a[i__1].i,
                            x[jx].i = x[jx].r * a[i__1].i + x[jx].i * a[i__1].r;
                        }
                    }
                    jx -= *incx;
                }
            }
        }
    } else {

/*        Form  x := A'*x  or  x := conjg( A' )*x. */

        if (lsame_(uplo, "U")) {
            if (*incx == 1) {
                for (j = *n - 1; j >= 0; --j) {

                    temp.r = x[j].r, temp.i = x[j].i;
                    if (noconj) {
                        if (nounit) {
                            i__1 = j + j * *lda;
                            z__1.r = temp.r * a[i__1].r - temp.i * a[i__1].i,
                            z__1.i = temp.i * a[i__1].r + temp.r * a[i__1].i;
                            temp.r = z__1.r, temp.i = z__1.i;
                        }
                        for (i = j - 1; i >= 0; --i) {
                            i__1 = i + j * *lda;
                            temp.r += a[i__1].r * x[i].r - a[i__1].i * x[i].i,
                            temp.i += a[i__1].r * x[i].i + a[i__1].i * x[i].r;
                        }
                    } else {
                        if (nounit) {
                            i__1 = j + j * *lda;
                            z__1.r = temp.r * a[i__1].r + temp.i * a[i__1].i,
                            z__1.i = temp.i * a[i__1].r - temp.r * a[i__1].i;
                            temp.r = z__1.r, temp.i = z__1.i;
                        }
                        for (i = j - 1; i >= 0; --i) {
                            i__1 = i + j * *lda;
                            temp.r += a[i__1].r * x[i].r + a[i__1].i * x[i].i,
                            temp.i += a[i__1].r * x[i].i - a[i__1].i * x[i].r;
                        }
                    }

                    x[j].r = temp.r, x[j].i = temp.i;
                }
            } else {
                jx = kx + (*n - 1) * *incx;
                for (j = *n - 1; j >= 0; --j) {
                    temp.r = x[jx].r, temp.i = x[jx].i;
                    ix = jx;
                    if (noconj) {
                        if (nounit) {
                            i__1 = j + j * *lda;
                            z__1.r = temp.r * a[i__1].r - temp.i * a[i__1].i,
                            z__1.i = temp.i * a[i__1].r + temp.r * a[i__1].i;
                            temp.r = z__1.r, temp.i = z__1.i;
                        }
                        for (i = j - 1; i >= 0; --i) {
                            ix -= *incx;
                            i__1 = i + j * *lda;
                            temp.r += a[i__1].r * x[ix].r - a[i__1].i * x[ix].i,
                            temp.i += a[i__1].r * x[ix].i + a[i__1].i * x[ix].r;
                        }
                    } else {
                        if (nounit) {
                            i__1 = j + j * *lda;
                            z__1.r = temp.r * a[i__1].r + temp.i * a[i__1].i,
                            z__1.i = temp.i * a[i__1].r - temp.r * a[i__1].i;
                            temp.r = z__1.r, temp.i = z__1.i;
                        }
                        for (i = j - 1; i >= 0; --i) {
                            ix -= *incx;
                            i__1 = i + j * *lda;
                            temp.r += a[i__1].r * x[ix].r + a[i__1].i * x[ix].i,
                            temp.i += a[i__1].r * x[ix].i - a[i__1].i * x[ix].r;
                        }
                    }
                    x[jx].r = temp.r, x[jx].i = temp.i;
                    jx -= *incx;
                }
            }
        } else {
            if (*incx == 1) {
                for (j = 0; j < *n; ++j) {
                    temp.r = x[j].r, temp.i = x[j].i;
                    if (noconj) {
                        if (nounit) {
                            i__1 = j + j * *lda;
                            z__1.r = temp.r * a[i__1].r - temp.i * a[i__1].i,
                            z__1.i = temp.i * a[i__1].r + temp.r * a[i__1].i;
                            temp.r = z__1.r, temp.i = z__1.i;
                        }
                        for (i = j + 1; i < *n; ++i) {
                            i__1 = i + j * *lda;
                            temp.r += a[i__1].r * x[i].r - a[i__1].i * x[i].i,
                            temp.i += a[i__1].r * x[i].i + a[i__1].i * x[i].r;
                        }
                    } else {
                        if (nounit) {
                            i__1 = j + j * *lda;
                            z__1.r = temp.r * a[i__1].r + temp.i * a[i__1].i,
                            z__1.i = temp.i * a[i__1].r - temp.r * a[i__1].i;
                            temp.r = z__1.r, temp.i = z__1.i;
                        }
                        for (i = j + 1; i < *n; ++i) {
                            i__1 = i + j * *lda;
                            temp.r += a[i__1].r * x[i].r + a[i__1].i * x[i].i,
                            temp.i += a[i__1].r * x[i].i - a[i__1].i * x[i].r;
                        }
                    }

                    x[j].r = temp.r, x[j].i = temp.i;
                }
            } else {
                jx = kx;
                for (j = 0; j < *n; ++j) {
                    temp.r = x[jx].r, temp.i = x[jx].i;
                    ix = jx;
                    if (noconj) {
                        if (nounit) {
                            i__1 = j + j * *lda;
                            z__1.r = temp.r * a[i__1].r - temp.i * a[i__1].i,
                            z__1.i = temp.i * a[i__1].r + temp.r * a[i__1].i;
                            temp.r = z__1.r, temp.i = z__1.i;
                        }
                        for (i = j + 1; i < *n; ++i) {
                            ix += *incx;
                            i__1 = i + j * *lda;
                            temp.r += a[i__1].r * x[ix].r - a[i__1].i * x[ix].i,
                            temp.i += a[i__1].r * x[ix].i + a[i__1].i * x[ix].r;
                        }
                    } else {
                        if (nounit) {
                            i__1 = j + j * *lda;
                            z__1.r = temp.r * a[i__1].r + temp.i * a[i__1].i,
                            z__1.i = temp.i * a[i__1].r - temp.r * a[i__1].i;
                            temp.r = z__1.r, temp.i = z__1.i;
                        }
                        for (i = j + 1; i < *n; ++i) {
                            ix += *incx;
                            i__1 = i + j * *lda;
                            temp.r += a[i__1].r * x[ix].r + a[i__1].i * x[ix].i,
                            temp.i += a[i__1].r * x[ix].i - a[i__1].i * x[ix].r;
                        }
                    }
                    x[jx].r = temp.r, x[jx].i = temp.i;
                    jx += *incx;
                }
            }
        }
    }
} /* ztrmv_ */
