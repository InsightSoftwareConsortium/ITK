#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Subroutine */ void ztrmm_(side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb)
const char *side, *uplo, *transa, *diag;
const integer *m, *n;
doublecomplex *alpha, *a;
const integer *lda;
doublecomplex *b;
const integer *ldb;
{
    /* System generated locals */
    integer i__1, i__2;
    doublecomplex z__1;

    /* Local variables */
    static integer info;
    static doublecomplex temp;
    static integer i, j, k;
    static logical lside;
    static integer nrowa;
    static logical upper;
    static logical noconj, nounit;

/**************************************************************************/
/*                                                                        */
/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  ZTRMM  performs one of the matrix-matrix operations                   */
/*                                                                        */
/*     B := alpha*op( A )*B,   or   B := alpha*B*op( A )                  */
/*                                                                        */
/*  where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or */
/*  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of */
/*                                                                        */
/*     op( A ) = A   or   op( A ) = A'   or   op( A ) = conjg( A' ).      */
/*                                                                        */
/*  Parameters                                                            */
/*  ==========                                                            */
/*                                                                        */
/*  SIDE   - CHARACTER*1.                                                 */
/*           On entry,  SIDE specifies whether  op( A ) multiplies B from */
/*           the left or right as follows:                                */
/*                                                                        */
/*              SIDE = 'L' or 'l'   B := alpha*op( A )*B.                 */
/*                                                                        */
/*              SIDE = 'R' or 'r'   B := alpha*B*op( A ).                 */
/*                                                                        */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  UPLO   - CHARACTER*1.                                                 */
/*           On entry, UPLO specifies whether the matrix A is an upper or */
/*           lower triangular matrix as follows:                          */
/*                                                                        */
/*              UPLO = 'U' or 'u'   A is an upper triangular matrix.      */
/*                                                                        */
/*              UPLO = 'L' or 'l'   A is a lower triangular matrix.       */
/*                                                                        */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  TRANSA - CHARACTER*1.                                                 */
/*           On entry, TRANSA specifies the form of op( A ) to be used in */
/*           the matrix multiplication as follows:                        */
/*                                                                        */
/*              TRANSA = 'N' or 'n'   op( A ) = A.                        */
/*                                                                        */
/*              TRANSA = 'T' or 't'   op( A ) = A'.                       */
/*                                                                        */
/*              TRANSA = 'C' or 'c'   op( A ) = conjg( A' ).              */
/*                                                                        */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  DIAG   - CHARACTER*1.                                                 */
/*           On entry, DIAG specifies whether or not A is unit triangular */
/*           as follows:                                                  */
/*                                                                        */
/*              DIAG = 'U' or 'u'   A is assumed to be unit triangular.   */
/*                                                                        */
/*              DIAG = 'N' or 'n'   A is not assumed to be unit           */
/*                                  triangular.                           */
/*                                                                        */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  M      - INTEGER.                                                     */
/*           On entry, M specifies the number of rows of B. M must be at  */
/*           least zero.                                                  */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  N      - INTEGER.                                                     */
/*           On entry, N specifies the number of columns of B.  N must be */
/*           at least zero.                                               */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  ALPHA  - COMPLEX*16      .                                            */
/*           On entry,  ALPHA specifies the scalar  alpha. When  alpha is */
/*           zero then  A is not referenced and  B need not be set before */
/*           entry.                                                       */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  A      - COMPLEX*16       array of DIMENSION ( LDA, k ), where k is m */
/*           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'. */
/*           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k */
/*           upper triangular part of the array  A must contain the upper */
/*           triangular matrix  and the strictly lower triangular part of */
/*           A is not referenced.                                         */
/*           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k */
/*           lower triangular part of the array  A must contain the lower */
/*           triangular matrix  and the strictly upper triangular part of */
/*           A is not referenced.                                         */
/*           Note that when  DIAG = 'U' or 'u',  the diagonal elements of */
/*           A  are not referenced either,  but are assumed to be  unity. */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  LDA    - INTEGER.                                                     */
/*           On entry, LDA specifies the first dimension of A as declared */
/*           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then */
/*           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r' */
/*           then LDA must be at least max( 1, n ).                       */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  B      - COMPLEX*16       array of DIMENSION ( LDB, n ).              */
/*           Before entry,  the leading  m by n part of the array  B must */
/*           contain the matrix  B,  and  on exit  is overwritten  by the */
/*           transformed matrix.                                          */
/*                                                                        */
/*  LDB    - INTEGER.                                                     */
/*           On entry, LDB specifies the first dimension of B as declared */
/*           in  the  calling  (sub)  program.   LDB  must  be  at  least */
/*           max( 1, m ).                                                 */
/*           Unchanged on exit.                                           */
/*                                                                        */
/**************************************************************************/

/*  Level 3 Blas routine. */

/*  -- Written on 8-February-1989. */
/*     Jack Dongarra, Argonne National Laboratory. */
/*     Iain Duff, AERE Harwell. */
/*     Jeremy Du Croz, Numerical Algorithms Group Ltd. */
/*     Sven Hammarling, Numerical Algorithms Group Ltd. */

    lside = lsame_(side, "L");
    if (lside) {
        nrowa = *m;
    } else {
        nrowa = *n;
    }
    noconj = lsame_(transa, "T");
    nounit = lsame_(diag, "N");
    upper = lsame_(uplo, "U");

    info = 0;
    if (! lside && ! lsame_(side, "R")) {
        info = 1;
    } else if (! upper && ! lsame_(uplo, "L")) {
        info = 2;
    } else if (! lsame_(transa, "N") && ! lsame_(transa, "T") && ! lsame_(transa, "C")) {
        info = 3;
    } else if (! lsame_(diag, "U") && ! lsame_(diag, "N")) {
        info = 4;
    } else if (*m < 0) {
        info = 5;
    } else if (*n < 0) {
        info = 6;
    } else if (*lda < max(1,nrowa)) {
        info = 9;
    } else if (*ldb < max(1,*m)) {
        info = 11;
    }
    if (info != 0) {
        xerbla_("ZTRMM ", &info);
        return;
    }

/*     Quick return if possible. */

    if (*n == 0) {
        return;
    }

/*     And when  alpha.eq.zero. */

    if (alpha->r == 0. && alpha->i == 0.) {
        for (j = 0; j < *n; ++j) {
            for (i = 0; i < *m; ++i) {
                i__2 = i + j * *ldb;
                b[i__2].r = 0., b[i__2].i = 0.;
            }
        }
        return;
    }

/*     Start the operations. */

    if (lside) {
        if (lsame_(transa, "N")) {

/*           Form  B := alpha*A*B. */

            if (upper) {
                for (j = 0; j < *n; ++j) {
                    for (k = 0; k < *m; ++k) {
                        i__2 = k + j * *ldb;
                        if (b[i__2].r != 0. || b[i__2].i != 0.) {
                            temp.r = alpha->r * b[i__2].r - alpha->i * b[i__2].i,
                            temp.i = alpha->r * b[i__2].i + alpha->i * b[i__2].r;
                            for (i = 0; i < k; ++i) {
                                i__1 = i + k * *lda;
                                i__2 = i + j * *ldb;
                                b[i__2].r += temp.r * a[i__1].r - temp.i * a[i__1].i,
                                b[i__2].i += temp.r * a[i__1].i + temp.i * a[i__1].r;
                            }
                            if (nounit) {
                                i__1 = k + k * *lda;
                                z__1.r = temp.r * a[i__1].r - temp.i * a[i__1].i,
                                z__1.i = temp.r * a[i__1].i + temp.i * a[i__1].r;
                                temp.r = z__1.r, temp.i = z__1.i;
                            }
                            i__2 = k + j * *ldb;
                            b[i__2].r = temp.r, b[i__2].i = temp.i;
                        }
                    }
                }
            } else {
                for (j = 0; j < *n; ++j) {
                    for (k = *m - 1; k >= 0; --k) {
                        i__2 = k + j * *ldb;
                        if (b[i__2].r != 0. || b[i__2].i != 0.) {
                            temp.r = alpha->r * b[i__2].r - alpha->i * b[i__2].i,
                            temp.i = alpha->r * b[i__2].i + alpha->i * b[i__2].r;
                            b[i__2].r = temp.r, b[i__2].i = temp.i;
                            if (nounit) {
                                i__1 = k + k * *lda;
                                z__1.r = b[i__2].r * a[i__1].r - b[i__2].i * a[i__1].i,
                                z__1.i = b[i__2].r * a[i__1].i + b[i__2].i * a[i__1].r;
                                b[i__2].r = z__1.r, b[i__2].i = z__1.i;
                            }
                            for (i = k + 1; i < *m; ++i) {
                                i__1 = i + k * *lda;
                                i__2 = i + j * *ldb;
                                b[i__2].r += temp.r * a[i__1].r - temp.i * a[i__1].i,
                                b[i__2].i += temp.r * a[i__1].i + temp.i * a[i__1].r;
                            }
                        }
                    }
                }
            }
        } else {

/*           Form  B := alpha*B*A'   or   B := alpha*B*conjg( A' ). */

            if (upper) {
                for (j = 0; j < *n; ++j) {
                    for (i = *m - 1; i >= 0; --i) {
                        i__2 = i + j * *ldb;
                        temp.r = b[i__2].r, temp.i = b[i__2].i;
                        if (noconj) {
                            if (nounit) {
                                i__1 = i + i * *lda;
                                z__1.r = temp.r * a[i__1].r - temp.i * a[i__1].i,
                                z__1.i = temp.r * a[i__1].i + temp.i * a[i__1].r;
                                temp.r = z__1.r, temp.i = z__1.i;
                            }
                            for (k = 0; k < i; ++k) {
                                i__1 = k + i * *lda;
                                i__2 = k + j * *ldb;
                                temp.r += a[i__1].r * b[i__2].r - a[i__1].i * b[i__2].i,
                                temp.i += a[i__1].r * b[i__2].i + a[i__1].i * b[i__2].r;
                            }
                        } else {
                            if (nounit) {
                                i__1 = i + i * *lda;
                                z__1.r = temp.r * a[i__1].r + temp.i * a[i__1].i,
                                z__1.i = temp.i * a[i__1].r - temp.r * a[i__1].i;
                                temp.r = z__1.r, temp.i = z__1.i;
                            }
                            for (k = 0; k < i; ++k) {
                                i__1 = k + i * *lda;
                                i__2 = k + j * *ldb;
                                temp.r += a[i__1].r * b[i__2].r + a[i__1].i * b[i__2].i,
                                temp.i += a[i__1].r * b[i__2].i - a[i__1].i * b[i__2].r;
                            }
                        }
                        i__2 = i + j * *ldb;
                        b[i__2].r = alpha->r * temp.r - alpha->i * temp.i,
                        b[i__2].i = alpha->r * temp.i + alpha->i * temp.r;
                    }
                }
            } else {
                for (j = 0; j < *n; ++j) {
                    for (i = 0; i < *m; ++i) {
                        i__2 = i + j * *ldb;
                        temp.r = b[i__2].r, temp.i = b[i__2].i;
                        if (noconj) {
                            if (nounit) {
                                i__1 = i + i * *lda;
                                z__1.r = temp.r * a[i__1].r - temp.i * a[i__1].i,
                                z__1.i = temp.r * a[i__1].i + temp.i * a[i__1].r;
                                temp.r = z__1.r, temp.i = z__1.i;
                            }
                            for (k = i + 1; k < *m; ++k) {
                                i__1 = k + i * *lda;
                                i__2 = k + j * *ldb;
                                temp.r += a[i__1].r * b[i__2].r - a[i__1].i * b[i__2].i,
                                temp.i += a[i__1].r * b[i__2].i + a[i__1].i * b[i__2].r;
                            }
                        } else {
                            if (nounit) {
                                i__1 = i + i * *lda;
                                z__1.r = temp.r * a[i__1].r + temp.i * a[i__1].i,
                                z__1.i = temp.i * a[i__1].r - temp.r * a[i__1].i;
                                temp.r = z__1.r, temp.i = z__1.i;
                            }
                            for (k = i + 1; k < *m; ++k) {
                                i__1 = k + i * *lda;
                                i__2 = k + j * *ldb;
                                temp.r += a[i__1].r * b[i__2].r + a[i__1].i * b[i__2].i,
                                temp.i += a[i__1].r * b[i__2].i - a[i__1].i * b[i__2].r;
                            }
                        }
                        i__2 = i + j * *ldb;
                        b[i__2].r = alpha->r * temp.r - alpha->i * temp.i,
                        b[i__2].i = alpha->r * temp.i + alpha->i * temp.r;
                    }
                }
            }
        }
    } else {
        if (lsame_(transa, "N")) {

/*           Form  B := alpha*B*A. */

            if (upper) {
                for (j = *n - 1; j >= 0; --j) {
                    temp.r = alpha->r, temp.i = alpha->i;
                    if (nounit) {
                        i__1 = j + j * *lda;
                        z__1.r = temp.r * a[i__1].r - temp.i * a[i__1].i,
                        z__1.i = temp.r * a[i__1].i + temp.i * a[i__1].r;
                        temp.r = z__1.r, temp.i = z__1.i;
                    }
                    for (i = 0; i < *m; ++i) {
                        i__2 = i + j * *ldb;
                        z__1.r = temp.r * b[i__2].r - temp.i * b[i__2].i,
                        z__1.i = temp.r * b[i__2].i + temp.i * b[i__2].r;
                        b[i__2].r = z__1.r, b[i__2].i = z__1.i;
                    }
                    for (k = 0; k < j; ++k) {
                        i__1 = k + j * *lda;
                        if (a[i__1].r != 0. || a[i__1].i != 0.) {
                            temp.r = alpha->r * a[i__1].r - alpha->i * a[i__1].i,
                            temp.i = alpha->r * a[i__1].i + alpha->i * a[i__1].r;
                            for (i = 0; i < *m; ++i) {
                                i__2 = i + k * *ldb;
                                z__1.r = temp.r * b[i__2].r - temp.i * b[i__2].i,
                                z__1.i = temp.r * b[i__2].i + temp.i * b[i__2].r;
                                i__2 = i + j * *ldb;
                                b[i__2].r += z__1.r, b[i__2].i += z__1.i;
                            }
                        }
                    }
                }
            } else {
                for (j = 0; j < *n; ++j) {
                    temp.r = alpha->r, temp.i = alpha->i;
                    if (nounit) {
                        i__1 = j + j * *lda;
                        z__1.r = temp.r * a[i__1].r - temp.i * a[i__1].i,
                        z__1.i = temp.r * a[i__1].i + temp.i * a[i__1].r;
                        temp.r = z__1.r, temp.i = z__1.i;
                    }
                    for (i = 0; i < *m; ++i) {
                        i__2 = i + j * *ldb;
                        z__1.r = temp.r * b[i__2].r - temp.i * b[i__2].i,
                        z__1.i = temp.r * b[i__2].i + temp.i * b[i__2].r;
                        b[i__2].r = z__1.r, b[i__2].i = z__1.i;
                    }
                    for (k = j + 1; k < *n; ++k) {
                        i__1 = k + j * *lda;
                        if (a[i__1].r != 0. || a[i__1].i != 0.) {
                            temp.r = alpha->r * a[i__1].r - alpha->i * a[i__1].i,
                            temp.i = alpha->r * a[i__1].i + alpha->i * a[i__1].r;
                            for (i = 0; i < *m; ++i) {
                                i__2 = i + k * *ldb;
                                z__1.r = temp.r * b[i__2].r - temp.i * b[i__2].i,
                                z__1.i = temp.r * b[i__2].i + temp.i * b[i__2].r;
                                i__2 = i + j * *ldb;
                                b[i__2].r += z__1.r, b[i__2].i += z__1.i;
                            }
                        }
                    }
                }
            }
        } else {

/*           Form  B := alpha*B*A'   or   B := alpha*B*conjg( A' ). */

            if (upper) {
                for (k = 0; k < *n; ++k) {
                    for (j = 0; j < k; ++j) {
                        i__1 = j + k * *lda;
                        if (a[i__1].r != 0. || a[i__1].i != 0.) {
                            if (noconj) {
                                temp.r = alpha->r * a[i__1].r - alpha->i * a[i__1].i,
                                temp.i = alpha->i * a[i__1].r + alpha->r * a[i__1].i;
                            } else {
                                temp.r = alpha->r * a[i__1].r + alpha->i * a[i__1].i,
                                temp.i = alpha->i * a[i__1].r - alpha->r * a[i__1].i;
                            }
                            for (i = 0; i < *m; ++i) {
                                i__2 = i + k * *ldb;
                                z__1.r = temp.r * b[i__2].r - temp.i * b[i__2].i,
                                z__1.i = temp.r * b[i__2].i + temp.i * b[i__2].r;
                                i__2 = i + j * *ldb;
                                b[i__2].r += z__1.r, b[i__2].i += z__1.i;
                            }
                        }
                    }
                    temp.r = alpha->r, temp.i = alpha->i;
                    if (nounit) {
                        if (noconj) {
                            i__1 = k + k * *lda;
                            z__1.r = temp.r * a[i__1].r - temp.i * a[i__1].i,
                            z__1.i = temp.r * a[i__1].i + temp.i * a[i__1].r;
                            temp.r = z__1.r, temp.i = z__1.i;
                        } else {
                            i__1 = k + k * *lda;
                            z__1.r = temp.r * a[i__1].r + temp.i * a[i__1].i,
                            z__1.i = temp.i * a[i__1].r - temp.r * a[i__1].i;
                            temp.r = z__1.r, temp.i = z__1.i;
                        }
                    }
                    if (temp.r != 1. || temp.i != 0.) {
                        for (i = 0; i < *m; ++i) {
                            i__2 = i + k * *ldb;
                            z__1.r = temp.r * b[i__2].r - temp.i * b[i__2].i,
                            z__1.i = temp.r * b[i__2].i + temp.i * b[i__2].r;
                            b[i__2].r = z__1.r, b[i__2].i = z__1.i;
                        }
                    }
                }
            } else {
                for (k = *n - 1; k >= 0; --k) {
                    for (j = k + 1; j < *n; ++j) {
                        i__1 = j + k * *lda;
                        if (a[i__1].r != 0. || a[i__1].i != 0.) {
                            if (noconj) {
                                temp.r = alpha->r * a[i__1].r - alpha->i * a[i__1].i,
                                temp.i = alpha->r * a[i__1].i + alpha->i * a[i__1].r;
                            } else {
                                temp.r = alpha->r * a[i__1].r + alpha->i * a[i__1].i,
                                temp.i = alpha->i * a[i__1].r - alpha->r * a[i__1].i;
                            }
                            for (i = 0; i < *m; ++i) {
                                i__2 = i + k * *ldb;
                                z__1.r = temp.r * b[i__2].r - temp.i * b[i__2].i,
                                z__1.i = temp.r * b[i__2].i + temp.i * b[i__2].r;
                                i__2 = i + j * *ldb;
                                b[i__2].r += z__1.r, b[i__2].i += z__1.i;
                            }
                        }
                    }
                    temp.r = alpha->r, temp.i = alpha->i;
                    if (nounit) {
                        if (noconj) {
                            i__1 = k + k * *lda;
                            z__1.r = temp.r * a[i__1].r - temp.i * a[i__1].i,
                            z__1.i = temp.i * a[i__1].r + temp.r * a[i__1].i;
                            temp.r = z__1.r, temp.i = z__1.i;
                        } else {
                            i__1 = k + k * *lda;
                            z__1.r = temp.r * a[i__1].r + temp.i * a[i__1].i,
                            z__1.i = temp.i * a[i__1].r - temp.r * a[i__1].i;
                            temp.r = z__1.r, temp.i = z__1.i;
                        }
                    }
                    if (temp.r != 1. || temp.i != 0.) {
                        for (i = 0; i < *m; ++i) {
                            i__2 = i + k * *ldb;
                            z__1.r = temp.r * b[i__2].r - temp.i * b[i__2].i,
                            z__1.i = temp.r * b[i__2].i + temp.i * b[i__2].r;
                            b[i__2].r = z__1.r, b[i__2].i = z__1.i;
                        }
                    }
                }
            }
        }
    }
} /* ztrmm_ */
