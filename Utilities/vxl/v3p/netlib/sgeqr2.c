#include "f2c.h"
#include "netlib.h"

/* Table of constant values */
static integer c__1 = 1;

/* Subroutine */ void sgeqr2_(integer *m, integer *n, real *a, integer *lda,
        real *tau, real *work, integer *info)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i, ip1;
    static real aii;

/*  -- LAPACK routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     February 29, 1992 */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  SGEQR2 computes a QR factorization of a real m by n matrix A:         */
/*  A = Q * R.                                                            */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  M       (input) INTEGER                                               */
/*          The number of rows of the matrix A.  M >= 0.                  */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The number of columns of the matrix A.  N >= 0.               */
/*                                                                        */
/*  A       (input/output) REAL array, dimension (LDA,N)                  */
/*          On entry, the m by n matrix A.                                */
/*          On exit, the elements on and above the diagonal of the array  */
/*          contain the min(m,n) by n upper trapezoidal matrix R (R is    */
/*          upper triangular if m >= n); the elements below the diagonal, */
/*          with the array TAU, represent the orthogonal matrix Q as a    */
/*          product of elementary reflectors (see Further Details).       */
/*                                                                        */
/*  LDA     (input) INTEGER                                               */
/*          The leading dimension of the array A.  LDA >= max(1,M).       */
/*                                                                        */
/*  TAU     (output) REAL array, dimension (min(M,N))                     */
/*          The scalar factors of the elementary reflectors (see Further  */
/*          Details).                                                     */
/*                                                                        */
/*  WORK    (workspace) REAL array, dimension (N)                         */
/*                                                                        */
/*  INFO    (output) INTEGER                                              */
/*          = 0: successful exit                                          */
/*          < 0: if INFO = -i, the i-th argument had an illegal value     */
/*                                                                        */
/*  Further Details                                                       */
/*  ===============                                                       */
/*                                                                        */
/*  The matrix Q is represented as a product of elementary reflectors     */
/*                                                                        */
/*     Q = H(1) H(2) . . . H(k), where k = min(m,n).                      */
/*                                                                        */
/*  Each H(i) has the form                                                */
/*                                                                        */
/*     H(i) = I - tau * v * v'                                            */
/*                                                                        */
/*  where tau is a real scalar, and v is a real vector with               */
/*  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),  */
/*  and tau in TAU(i).                                                    */
/*                                                                        */
/*  ===================================================================== */

/*     Test the input arguments */

    *info = 0;
    if (*m < 0) {
        *info = -1;
    } else if (*n < 0) {
        *info = -2;
    } else if (*lda < max(1,*m)) {
        *info = -4;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("SGEQR2", &i__1);
        return;
    }

    for (i = 0; i < *m && i < *n; ++i) {
        ip1 = i + 1;

/*        Generate elementary reflector H(i) to annihilate A(i+1:m,i) */

        i__1 = *m - i;
        slarfg_(&i__1, &a[i + i * *lda], &a[min(ip1,*m-1) + i * *lda], &c__1, &tau[i]);
        if (ip1 < *n) {

/*           Apply H(i) to A(i:m,i+1:n) from the left */

            aii = a[i + i * *lda];
            a[i + i * *lda] = 1.f;
            i__1 = *m - i;
            i__2 = *n - ip1;
            slarf_("Left", &i__1, &i__2, &a[i + i * *lda], &c__1, &tau[i], &a[i + ip1 * *lda], lda, work);
            a[i + i * *lda] = aii;
        }
    }
} /* sgeqr2_ */
