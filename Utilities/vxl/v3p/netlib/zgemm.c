#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Subroutine */ void zgemm_(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
const char *transa, *transb;
const integer *m, *n, *k;
doublecomplex *alpha, *a;
const integer *lda;
doublecomplex *b;
const integer *ldb;
doublecomplex *beta, *c;
const integer *ldc;
{
    /* System generated locals */
    integer i__1, i__2;
    doublecomplex z__1;

    /* Local variables */
    static integer info;
    static logical nota, notb;
    static doublecomplex temp;
    static integer i, j, l;
    static logical conja, conjb;
/*  static integer ncola; */
    static integer nrowa, nrowb;

/*  ===================================================================== */
/*                                                                        */
/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  ZGEMM  performs one of the matrix-matrix operations                   */
/*                                                                        */
/*     C := alpha*op( A )*op( B ) + beta*C,                               */
/*                                                                        */
/*  where  op( X ) is one of                                              */
/*                                                                        */
/*     op( X ) = X   or   op( X ) = X'   or   op( X ) = conjg( X' ),      */
/*                                                                        */
/*  alpha and beta are scalars, and A, B and C are matrices, with op( A ) */
/*  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix. */
/*                                                                        */
/*  Parameters                                                            */
/*  ==========                                                            */
/*                                                                        */
/*  TRANSA - CHARACTER*1.                                                 */
/*           On entry, TRANSA specifies the form of op( A ) to be used in */
/*           the matrix multiplication as follows:                        */
/*                                                                        */
/*              TRANSA = 'N' or 'n',  op( A ) = A.                        */
/*                                                                        */
/*              TRANSA = 'T' or 't',  op( A ) = A'.                       */
/*                                                                        */
/*              TRANSA = 'C' or 'c',  op( A ) = conjg( A' ).              */
/*                                                                        */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  TRANSB - CHARACTER*1.                                                 */
/*           On entry, TRANSB specifies the form of op( B ) to be used in */
/*           the matrix multiplication as follows:                        */
/*                                                                        */
/*              TRANSB = 'N' or 'n',  op( B ) = B.                        */
/*                                                                        */
/*              TRANSB = 'T' or 't',  op( B ) = B'.                       */
/*                                                                        */
/*              TRANSB = 'C' or 'c',  op( B ) = conjg( B' ).              */
/*                                                                        */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  M      - INTEGER.                                                     */
/*           On entry,  M  specifies  the number  of rows  of the  matrix */
/*           op( A )  and of the  matrix  C.  M  must  be at least  zero. */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  N      - INTEGER.                                                     */
/*           On entry,  N  specifies the number  of columns of the matrix */
/*           op( B ) and the number of columns of the matrix C. N must be */
/*           at least zero.                                               */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  K      - INTEGER.                                                     */
/*           On entry,  K  specifies  the number of columns of the matrix */
/*           op( A ) and the number of rows of the matrix op( B ). K must */
/*           be at least  zero.                                           */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  ALPHA  - COMPLEX*16      .                                            */
/*           On entry, ALPHA specifies the scalar alpha.                  */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  A      - COMPLEX*16       array of DIMENSION ( LDA, ka ), where ka is */
/*           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.         */
/*           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k */
/*           part of the array  A  must contain the matrix  A,  otherwise */
/*           the leading  k by m  part of the array  A  must contain  the */
/*           matrix A.                                                    */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  LDA    - INTEGER.                                                     */
/*           On entry, LDA specifies the first dimension of A as declared */
/*           in the calling (sub) program. When  TRANSA = 'N' or 'n' then */
/*           LDA must be at least  max( 1, m ), otherwise  LDA must be at */
/*           least  max( 1, k ).                                          */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  B      - COMPLEX*16       array of DIMENSION ( LDB, kb ), where kb is */
/*           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.         */
/*           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n */
/*           part of the array  B  must contain the matrix  B,  otherwise */
/*           the leading  n by k  part of the array  B  must contain  the */
/*           matrix B.                                                    */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  LDB    - INTEGER.                                                     */
/*           On entry, LDB specifies the first dimension of B as declared */
/*           in the calling (sub) program. When  TRANSB = 'N' or 'n' then */
/*           LDB must be at least  max( 1, k ), otherwise  LDB must be at */
/*           least  max( 1, n ).                                          */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  BETA   - COMPLEX*16      .                                            */
/*           On entry,  BETA  specifies the scalar  beta.  When  BETA  is */
/*           supplied as zero then C need not be set on input.            */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  C      - COMPLEX*16       array of DIMENSION ( LDC, n ).              */
/*           Before entry, the leading  m by n  part of the array  C must */
/*           contain the matrix  C,  except when  beta  is zero, in which */
/*           case C need not be set on entry.                             */
/*           On exit, the array  C  is overwritten by the  m by n  matrix */
/*           ( alpha*op( A )*op( B ) + beta*C ).                          */
/*                                                                        */
/*  LDC    - INTEGER.                                                     */
/*           On entry, LDC specifies the first dimension of C as declared */
/*           in  the  calling  (sub)  program.   LDC  must  be  at  least */
/*           max( 1, m ).                                                 */
/*           Unchanged on exit.                                           */
/*                                                                        */
/*  ===================================================================== */

/*  Level 3 Blas routine. */

/*  -- Written on 8-February-1989. */
/*     Jack Dongarra, Argonne National Laboratory. */
/*     Iain Duff, AERE Harwell. */
/*     Jeremy Du Croz, Numerical Algorithms Group Ltd. */
/*     Sven Hammarling, Numerical Algorithms Group Ltd. */

/*     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not */
/*     conjugated or transposed, set  CONJA and CONJB  as true if  A  and */
/*     B  respectively are to be  transposed but  not conjugated  and set */
/*     NROWA, NCOLA and  NROWB  as the number of rows and  columns  of  A */
/*     and the number of rows of  B  respectively. */

    nota = lsame_(transa, "N");
    notb = lsame_(transb, "N");
    conja = lsame_(transa, "C");
    conjb = lsame_(transb, "C");
    if (nota) {
        nrowa = *m;
/*      ncola = *k; */
    } else {
        nrowa = *k;
/*      ncola = *m; */
    }
    if (notb) {
        nrowb = *k;
    } else {
        nrowb = *n;
    }

/*     Test the input parameters. */

    info = 0;
    if (! nota && ! conja && ! lsame_(transa, "T")) {
        info = 1;
    } else if (! notb && ! conjb && ! lsame_(transb, "T")) {
        info = 2;
    } else if (*m < 0) {
        info = 3;
    } else if (*n < 0) {
        info = 4;
    } else if (*k < 0) {
        info = 5;
    } else if (*lda < max(1,nrowa)) {
        info = 8;
    } else if (*ldb < max(1,nrowb)) {
        info = 10;
    } else if (*ldc < max(1,*m)) {
        info = 13;
    }
    if (info != 0) {
        xerbla_("ZGEMM ", &info);
        return;
    }

/*     Quick return if possible. */

    if (*m == 0 || *n == 0 ||
        (((alpha->r == 0. && alpha->i == 0.) || *k == 0) && (beta->r == 1. && beta->i == 0.))) {
        return;
    }

/*     And when  alpha.eq.zero. */

    if (alpha->r == 0. && alpha->i == 0.) {
        if (beta->r == 0. && beta->i == 0.) {
            for (j = 0; j < *n; ++j) {
                for (i = 0; i < *m; ++i) {
                    i__1 = i + j * *ldc;
                    c[i__1].r = 0., c[i__1].i = 0.;
                }
            }
        } else {
            for (j = 0; j < *n; ++j) {
                for (i = 0; i < *m; ++i) {
                    i__1 = i + j * *ldc;
                    z__1.r = beta->r * c[i__1].r - beta->i * c[i__1].i,
                    z__1.i = beta->r * c[i__1].i + beta->i * c[i__1].r;
                    c[i__1].r = z__1.r, c[i__1].i = z__1.i;
                }
            }
        }
        return;
    }

/*     Start the operations. */

    if (notb) {
        if (nota) {

/*           Form  C := alpha*A*B + beta*C. */

            for (j = 0; j < *n; ++j) {
                if (beta->r == 0. && beta->i == 0.) {
                    for (i = 0; i < *m; ++i) {
                        i__1 = i + j * *ldc;
                        c[i__1].r = 0., c[i__1].i = 0.;
                    }
                } else if (beta->r != 1. || beta->i != 0.) {
                    for (i = 0; i < *m; ++i) {
                        i__1 = i + j * *ldc;
                        z__1.r = beta->r * c[i__1].r - beta->i * c[i__1].i,
                        z__1.i = beta->r * c[i__1].i + beta->i * c[i__1].r;
                        c[i__1].r = z__1.r, c[i__1].i = z__1.i;
                    }
                }
                for (l = 0; l < *k; ++l) {
                    i__1 = l + j * *ldb;
                    if (b[i__1].r != 0. || b[i__1].i != 0.) {
                        temp.r = alpha->r * b[i__1].r - alpha->i * b[i__1].i,
                        temp.i = alpha->r * b[i__1].i + alpha->i * b[i__1].r;
                        for (i = 0; i < *m; ++i) {
                            i__1 = i + j * *ldc;
                            i__2 = i + l * *lda;
                            c[i__1].r += temp.r * a[i__2].r - temp.i * a[i__2].i,
                            c[i__1].i += temp.r * a[i__2].i + temp.i * a[i__2].r;
                        }
                    }
                }
            }
        } else if (conja) {

/*           Form  C := alpha*conjg( A' )*B + beta*C. */

            for (j = 0; j < *n; ++j) {
                for (i = 0; i < *m; ++i) {
                    temp.r = 0., temp.i = 0.;
                    for (l = 0; l < *k; ++l) {
                        i__1 = l + i * *lda;
                        i__2 = l + j * *ldb;
                        temp.r += a[i__1].r * b[i__2].r + a[i__1].i * b[i__2].i,
                        temp.i += a[i__1].r * b[i__2].i - a[i__1].i * b[i__2].r;
                    }
                    if (beta->r == 0. && beta->i == 0.) {
                        i__1 = i + j * *ldc;
                        c[i__1].r = alpha->r * temp.r - alpha->i * temp.i,
                        c[i__1].i = alpha->r * temp.i + alpha->i * temp.r;
                    } else {
                        i__1 = i + j * *ldc;
                        z__1.r = beta->r * c[i__1].r - beta->i * c[i__1].i,
                        z__1.i = beta->r * c[i__1].i + beta->i * c[i__1].r;
                        c[i__1].r = z__1.r + alpha->r * temp.r - alpha->i * temp.i,
                        c[i__1].i = z__1.i + alpha->r * temp.i + alpha->i * temp.r;
                    }
                }
            }
        } else {

/*           Form  C := alpha*A'*B + beta*C */

            for (j = 0; j < *n; ++j) {
                for (i = 0; i < *m; ++i) {
                    temp.r = 0., temp.i = 0.;
                    for (l = 0; l < *k; ++l) {
                        i__1 = l + i * *lda;
                        i__2 = l + j * *ldb;
                        temp.r += a[i__1].r * b[i__2].r - a[i__1].i * b[i__2].i,
                        temp.i += a[i__1].r * b[i__2].i + a[i__1].i * b[i__2].r;
                    }
                    if (beta->r == 0. && beta->i == 0.) {
                        i__1 = i + j * *ldc;
                        c[i__1].r = alpha->r * temp.r - alpha->i * temp.i,
                        c[i__1].i = alpha->r * temp.i + alpha->i * temp.r;
                    } else {
                        i__1 = i + j * *ldc;
                        z__1.r = beta->r * c[i__1].r - beta->i * c[i__1].i,
                        z__1.i = beta->r * c[i__1].i + beta->i * c[i__1].r;
                        c[i__1].r = z__1.r + alpha->r * temp.r - alpha->i * temp.i,
                        c[i__1].i = z__1.i + alpha->r * temp.i + alpha->i * temp.r;
                    }
                }
            }
        }
    } else if (nota) {
        if (conjb) {

/*           Form  C := alpha*A*conjg( B' ) + beta*C. */

            for (j = 0; j < *n; ++j) {
                if (beta->r == 0. && beta->i == 0.) {
                    for (i = 0; i < *m; ++i) {
                        i__1 = i + j * *ldc;
                        c[i__1].r = 0., c[i__1].i = 0.;
                    }
                } else if (beta->r != 1. || beta->i != 0.) {
                    for (i = 0; i < *m; ++i) {
                        i__1 = i + j * *ldc;
                        z__1.r = beta->r * c[i__1].r - beta->i * c[i__1].i,
                        z__1.i = beta->r * c[i__1].i + beta->i * c[i__1].r;
                        c[i__1].r = z__1.r, c[i__1].i = z__1.i;
                    }
                }
                for (l = 0; l < *k; ++l) {
                    i__1 = j + l * *ldb;
                    if (b[i__1].r != 0. || b[i__1].i != 0.) {
                        i__1 = j + l * *ldb;
                        temp.r = alpha->r * b[i__1].r + alpha->i * b[i__1].i,
                        temp.i = alpha->i * b[i__1].r - alpha->r * b[i__1].i;
                        for (i = 0; i < *m; ++i) {
                            i__1 = i + j * *ldc;
                            i__2 = i + l * *lda;
                            c[i__1].r += temp.r * a[i__2].r - temp.i * a[i__2].i,
                            c[i__1].i += temp.r * a[i__2].i + temp.i * a[i__2].r;
                        }
                    }
                }
            }
        } else {

/*           Form  C := alpha*A*B'          + beta*C */

            for (j = 0; j < *n; ++j) {
                if (beta->r == 0. && beta->i == 0.) {
                    for (i = 0; i < *m; ++i) {
                        i__1 = i + j * *ldc;
                        c[i__1].r = 0., c[i__1].i = 0.;
                    }
                } else if (beta->r != 1. || beta->i != 0.) {
                    for (i = 0; i < *m; ++i) {
                        i__1 = i + j * *ldc;
                        z__1.r = beta->r * c[i__1].r - beta->i * c[i__1].i,
                        z__1.i = beta->r * c[i__1].i + beta->i * c[i__1].r;
                        c[i__1].r = z__1.r, c[i__1].i = z__1.i;
                    }
                }
                for (l = 0; l < *k; ++l) {
                    i__1 = j + l * *ldb;
                    if (b[i__1].r != 0. || b[i__1].i != 0.) {
                        i__1 = j + l * *ldb;
                        temp.r = alpha->r * b[i__1].r - alpha->i * b[i__1].i,
                        temp.i = alpha->r * b[i__1].i + alpha->i * b[i__1].r;
                        for (i = 0; i < *m; ++i) {
                            i__1 = i + j * *ldc;
                            i__2 = i + l * *lda;
                            c[i__1].r += temp.r * a[i__2].r - temp.i * a[i__2].i,
                            c[i__1].i += temp.r * a[i__2].i + temp.i * a[i__2].r;
                        }
                    }
                }
            }
        }
    } else if (conja) {
        if (conjb) {

/*           Form  C := alpha*conjg( A' )*conjg( B' ) + beta*C. */

            for (j = 0; j < *n; ++j) {
                for (i = 0; i < *m; ++i) {
                    temp.r = 0., temp.i = 0.;
                    for (l = 0; l < *k; ++l) {
                        i__1 = l + i * *lda;
                        i__2 = j + l * *ldb;
                        temp.r +=   a[i__1].r * b[i__2].r - a[i__1].i * b[i__2].i,
                        temp.i += - a[i__1].r * b[i__2].i - a[i__1].i * b[i__2].r;
                    }
                    if (beta->r == 0. && beta->i == 0.) {
                        i__1 = i + j * *ldc;
                        c[i__1].r = alpha->r * temp.r - alpha->i * temp.i,
                        c[i__1].i = alpha->r * temp.i + alpha->i * temp.r;
                    } else {
                        i__1 = i + j * *ldc;
                        z__1.r = beta->r * c[i__1].r - beta->i * c[i__1].i,
                        z__1.i = beta->r * c[i__1].i + beta->i * c[i__1].r;
                        c[i__1].r = z__1.r + alpha->r * temp.r - alpha->i * temp.i,
                        c[i__1].i = z__1.i + alpha->r * temp.i + alpha->i * temp.r;
                    }
                }
            }
        } else {

/*           Form  C := alpha*conjg( A' )*B' + beta*C */

            for (j = 0; j < *n; ++j) {
                for (i = 0; i < *m; ++i) {
                    temp.r = 0., temp.i = 0.;
                    for (l = 0; l < *k; ++l) {
                        i__1 = l + i * *lda;
                        i__2 = j + l * *ldb;
                        temp.r += a[i__1].r * b[i__2].r + a[i__1].i * b[i__2].i,
                        temp.i += a[i__1].r * b[i__2].i - a[i__1].i * b[i__2].r;
                    }
                    if (beta->r == 0. && beta->i == 0.) {
                        i__1 = i + j * *ldc;
                        c[i__1].r = alpha->r * temp.r - alpha->i * temp.i,
                        c[i__1].i = alpha->r * temp.i + alpha->i * temp.r;
                    } else {
                        i__1 = i + j * *ldc;
                        z__1.r = beta->r * c[i__1].r - beta->i * c[i__1].i,
                        z__1.i = beta->r * c[i__1].i + beta->i * c[i__1].r;
                        c[i__1].r = z__1.r + alpha->r * temp.r - alpha->i * temp.i,
                        c[i__1].i = z__1.i + alpha->r * temp.i + alpha->i * temp.r;
                    }
                }
            }
        }
    } else {
        if (conjb) {

/*           Form  C := alpha*A'*conjg( B' ) + beta*C */

            for (j = 0; j < *n; ++j) {
                for (i = 0; i < *m; ++i) {
                    temp.r = 0., temp.i = 0.;
                    for (l = 0; l < *k; ++l) {
                        i__1 = l + i * *lda;
                        i__2 = j + l * *ldb;
                        temp.r += a[i__1].r * b[i__2].r + a[i__1].i * b[i__2].i,
                        temp.i += a[i__1].i * b[i__2].r - a[i__1].r * b[i__2].i;
                    }
                    if (beta->r == 0. && beta->i == 0.) {
                        i__1 = i + j * *ldc;
                        c[i__1].r = alpha->r * temp.r - alpha->i * temp.i,
                        c[i__1].i = alpha->r * temp.i + alpha->i * temp.r;
                    } else {
                        i__1 = i + j * *ldc;
                        z__1.r = beta->r * c[i__1].r - beta->i * c[i__1].i,
                        z__1.i = beta->r * c[i__1].i + beta->i * c[i__1].r;
                        c[i__1].r = z__1.r + alpha->r * temp.r - alpha->i * temp.i,
                        c[i__1].i = z__1.i + alpha->r * temp.i + alpha->i * temp.r;
                    }
                }
            }
        } else {

/*           Form  C := alpha*A'*B' + beta*C */

            for (j = 0; j < *n; ++j) {
                for (i = 0; i < *m; ++i) {
                    temp.r = 0., temp.i = 0.;
                    for (l = 0; l < *k; ++l) {
                        i__1 = l + i * *lda;
                        i__2 = j + l * *ldb;
                        temp.r += a[i__1].r * b[i__2].r - a[i__1].i * b[i__2].i,
                        temp.i += a[i__1].r * b[i__2].i + a[i__1].i * b[i__2].r;
                    }
                    if (beta->r == 0. && beta->i == 0.) {
                        i__1 = i + j * *ldc;
                        c[i__1].r = alpha->r * temp.r - alpha->i * temp.i,
                        c[i__1].i = alpha->r * temp.i + alpha->i * temp.r;
                    } else {
                        i__1 = i + j * *ldc;
                        z__1.r = alpha->r * temp.r - alpha->i * temp.i,
                        z__1.i = alpha->r * temp.i + alpha->i * temp.r;
                        z__1.r += beta->r * c[i__1].r - beta->i * c[i__1].i,
                        z__1.i += beta->r * c[i__1].i + beta->i * c[i__1].r;
                        c[i__1].r = z__1.r, c[i__1].i = z__1.i;
                    }
                }
            }
        }
    }
} /* zgemm_ */
