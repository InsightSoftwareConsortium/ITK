#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Subroutine */ void zlaset_(uplo, m, n, alpha, beta, a, lda)
const char *uplo;
const integer *m, *n;
doublecomplex *alpha, *beta, *a;
const integer *lda;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i, j;

/*  -- LAPACK auxiliary routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     October 31, 1992 */

/*  ===================================================================== */
/*                                                                        */
/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  ZLASET initializes a 2-D array A to BETA on the diagonal and          */
/*  ALPHA on the offdiagonals.                                            */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  UPLO    (input) CHARACTER*1                                           */
/*          Specifies the part of the matrix A to be set.                 */
/*          = 'U':      Upper triangular part is set. The lower triangle  */
/*                      is unchanged.                                     */
/*          = 'L':      Lower triangular part is set. The upper triangle  */
/*                      is unchanged.                                     */
/*          Otherwise:  All of the matrix A is set.                       */
/*                                                                        */
/*  M       (input) INTEGER                                               */
/*          On entry, M specifies the number of rows of A.                */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          On entry, N specifies the number of columns of A.             */
/*                                                                        */
/*  ALPHA   (input) COMPLEX*16                                            */
/*          All the offdiagonal array elements are set to ALPHA.          */
/*                                                                        */
/*  BETA    (input) COMPLEX*16                                            */
/*          All the diagonal array elements are set to BETA.              */
/*                                                                        */
/*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)            */
/*          On entry, the m by n matrix A.                                */
/*          On exit, A(i,j) = ALPHA, 1 <= i <= m, 1 <= j <= n, i.ne.j;    */
/*                   A(i,i) = BETA , 1 <= i <= min(m,n)                   */
/*                                                                        */
/*  LDA     (input) INTEGER                                               */
/*          The leading dimension of the array A.  LDA >= max(1,M).       */
/*                                                                        */
/*  ===================================================================== */

    if (lsame_(uplo, "U")) {

/*        Set the diagonal to BETA and the strictly upper triangular */
/*        part of the array to ALPHA. */

        for (j = 1; j < *n; ++j) {
            for (i = 0; i < j && i < *m; ++i) {
                i__1 = i + j * *lda;
                a[i__1].r = alpha->r, a[i__1].i = alpha->i;
            }
        }
        for (j = 0; j < *n && j < *m; ++j) {
            i__1 = j + j * *lda;
            a[i__1].r = beta->r, a[i__1].i = beta->i;
        }

    } else if (lsame_(uplo, "L")) {

/*        Set the diagonal to BETA and the strictly lower triangular */
/*        part of the array to ALPHA. */

        for (j = 0; j < *m && j < *n; ++j) {
            for (i = j + 1; i < *m; ++i) {
                i__1 = i + j * *lda;
                a[i__1].r = alpha->r, a[i__1].i = alpha->i;
            }
            i__1 = j + j * *lda;
            a[i__1].r = beta->r, a[i__1].i = beta->i;
        }

    } else {

/*        Set the array to BETA on the diagonal and ALPHA on the */
/*        offdiagonal. */

        for (j = 0; j < *n; ++j) {
            for (i = 0; i < *m; ++i) {
                i__1 = i + j * *lda;
                a[i__1].r = alpha->r, a[i__1].i = alpha->i;
            }
        }
        for (j = 0; j < *m && j < *n; ++j) {
            i__1 = j + j * *lda;
            a[i__1].r = beta->r, a[i__1].i = beta->i;
        }
    }
} /* zlaset_ */
