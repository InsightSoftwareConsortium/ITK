#include "f2c.h"
#include "netlib.h"

/* Subroutine */ void slacpy_(const char *uplo, const integer *m, const integer *n,
                              real *a, const integer *lda, real *b, const integer *ldb)
{
    /* Local variables */
    static integer i, j;

/*  -- LAPACK auxiliary routine (version 2.0) --                    */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,  */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     February 29, 1992                                            */

/*  Purpose                                                              */
/*  =======                                                              */
/*                                                                       */
/*  SLACPY copies all or part of a two-dimensional matrix A to another   */
/*  matrix B.                                                            */
/*                                                                       */
/*  Arguments                                                            */
/*  =========                                                            */
/*                                                                       */
/*  UPLO    (input) CHARACTER*1                                          */
/*          Specifies the part of the matrix A to be copied to B.        */
/*          = 'U':      Upper triangular part                            */
/*          = 'L':      Lower triangular part                            */
/*          Otherwise:  All of the matrix A                              */
/*                                                                       */
/*  M       (input) INTEGER                                              */
/*          The number of rows of the matrix A.  M >= 0.                 */
/*                                                                       */
/*  N       (input) INTEGER                                              */
/*          The number of columns of the matrix A.  N >= 0.              */
/*                                                                       */
/*  A       (input) REAL array, dimension (LDA,N)                        */
/*          The m by n matrix A.  If UPLO = 'U', only the upper triangle */
/*          or trapezoid is accessed; if UPLO = 'L', only the lower      */
/*          triangle or trapezoid is accessed.                           */
/*                                                                       */
/*  LDA     (input) INTEGER                                              */
/*          The leading dimension of the array A.  LDA >= max(1,M).      */
/*                                                                       */
/*  B       (output) REAL array, dimension (LDB,N)                       */
/*          On exit, B = A in the locations specified by UPLO.           */
/*                                                                       */
/*  LDB     (input) INTEGER                                              */
/*          The leading dimension of the array B.  LDB >= max(1,M).      */
/*                                                                       */
/* ===================================================================== */

    if (lsame_(uplo, "U")) {
        for (j = 0; j < *n; ++j) {
            for (i = 0; i <= j && i < *m; ++i) {
                b[i + j * *ldb] = a[i + j * *lda];
            }
        }
    } else if (lsame_(uplo, "L")) {
        for (j = 0; j < *n; ++j) {
            for (i = j; i < *m; ++i) {
                b[i + j * *ldb] = a[i + j * *lda];
            }
        }
    } else {
        for (j = 0; j < *n; ++j) {
            for (i = 0; i < *m; ++i) {
                b[i + j * *ldb] = a[i + j * *lda];
            }
        }
    }
} /* slacpy_ */
