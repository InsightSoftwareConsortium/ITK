#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Subroutine */ void zlacpy_(uplo, m, n, a, lda, b, ldb)
const char *uplo;
const integer *m, *n;
doublecomplex *a;
const integer *lda;
doublecomplex *b;
const integer *ldb;
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i, j;


/*  -- LAPACK auxiliary routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     February 29, 1992 */

/*  ===================================================================== */
/*                                                                        */
/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  ZLACPY copies all or part of a two-dimensional matrix A to another    */
/*  matrix B.                                                             */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  UPLO    (input) CHARACTER*1                                           */
/*          Specifies the part of the matrix A to be copied to B.         */
/*          = 'U':      Upper triangular part                             */
/*          = 'L':      Lower triangular part                             */
/*          Otherwise:  All of the matrix A                               */
/*                                                                        */
/*  M       (input) INTEGER                                               */
/*          The number of rows of the matrix A.  M >= 0.                  */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The number of columns of the matrix A.  N >= 0.               */
/*                                                                        */
/*  A       (input) COMPLEX*16 array, dimension (LDA,N)                   */
/*          The m by n matrix A.  If UPLO = 'U', only the upper trapezium */
/*          is accessed; if UPLO = 'L', only the lower trapezium is       */
/*          accessed.                                                     */
/*                                                                        */
/*  LDA     (input) INTEGER                                               */
/*          The leading dimension of the array A.  LDA >= max(1,M).       */
/*                                                                        */
/*  B       (output) COMPLEX*16 array, dimension (LDB,N)                  */
/*          On exit, B = A in the locations specified by UPLO.            */
/*                                                                        */
/*  LDB     (input) INTEGER                                               */
/*          The leading dimension of the array B.  LDB >= max(1,M).       */
/*                                                                        */
/*  ===================================================================== */

    if (lsame_(uplo, "U")) {
        for (j = 0; j < *n; ++j) {
            for (i = 0; i <= j && i < *m; ++i) {
                i__1 = i + j * *ldb;
                i__2 = i + j * *lda;
                b[i__1].r = a[i__2].r, b[i__1].i = a[i__2].i;
            }
        }

    } else if (lsame_(uplo, "L")) {
        for (j = 0; j < *n; ++j) {
            for (i = j; i < *m; ++i) {
                i__1 = i + j * *ldb;
                i__2 = i + j * *lda;
                b[i__1].r = a[i__2].r, b[i__1].i = a[i__2].i;
            }
        }

    } else {
        for (j = 0; j < *n; ++j) {
            for (i = 0; i < *m; ++i) {
                i__1 = i + j * *ldb;
                i__2 = i + j * *lda;
                b[i__1].r = a[i__2].r, b[i__1].i = a[i__2].i;
            }
        }
    }
} /* zlacpy_ */
