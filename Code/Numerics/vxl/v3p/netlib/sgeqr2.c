/* sgeqr2.f -- translated by f2c (version of 4 June 1993  1:43:59).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* Subroutine */ int sgeqr2_(integer *m, integer *n, real *a, integer *lda,
        real *tau, real *work, integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;

    /* Local variables */
    static integer i, k;
    extern /* Subroutine */ int slarf_(char *, integer *, integer *, real *,
            integer *, real *, real *, integer *, real *, ftnlen), xerbla_(
            char *, integer *, ftnlen), slarfg_(integer *, real *, real *,
            integer *, real *);
    static real aii;


/*  -- LAPACK routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     February 29, 1992 */

/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Array Arguments .. */
/*     .. */

/*  Purpose */
/*  ======= */

/*  SGEQR2 computes a QR factorization of a real m by n matrix A: */
/*  A = Q * R. */

/*  Arguments */
/*  ========= */

/*  M       (input) INTEGER */
/*          The number of rows of the matrix A.  M >= 0. */

/*  N       (input) INTEGER */
/*          The number of columns of the matrix A.  N >= 0. */

/*  A       (input/output) REAL array, dimension (LDA,N) */
/*          On entry, the m by n matrix A. */
/*          On exit, the elements on and above the diagonal of the array
*/
/*          contain the min(m,n) by n upper trapezoidal matrix R (R is */
/*          upper triangular if m >= n); the elements below the diagonal,
*/
/*          with the array TAU, represent the orthogonal matrix Q as a */
/*          product of elementary reflectors (see Further Details). */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A.  LDA >= max(1,M). */

/*  TAU     (output) REAL array, dimension (min(M,N)) */
/*          The scalar factors of the elementary reflectors (see Further
*/
/*          Details). */

/*  WORK    (workspace) REAL array, dimension (N) */

/*  INFO    (output) INTEGER */
/*          = 0: successful exit */
/*          < 0: if INFO = -i, the i-th argument had an illegal value */

/*  Further Details */
/*  =============== */

/*  The matrix Q is represented as a product of elementary reflectors */

/*     Q = H(1) H(2) . . . H(k), where k = min(m,n). */

/*  Each H(i) has the form */

/*     H(i) = I - tau * v * v' */

/*  where tau is a real scalar, and v is a real vector with */
/*  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
*/
/*  and tau in TAU(i). */

/*  =====================================================================
*/

/*     .. Parameters .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. External Subroutines .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Executable Statements .. */

/*     Test the input arguments */

    /* Parameter adjustments */
    --work;
    --tau;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
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
        xerbla_("SGEQR2", &i__1, 6L);
        return 0;
    }

    k = min(*m,*n);

    i__1 = k;
    for (i = 1; i <= i__1; ++i) {

/*        Generate elementary reflector H(i) to annihilate A(i+1:m,i)
*/

        i__2 = *m - i + 1;
/* Computing MIN */
        i__3 = i + 1;
        slarfg_(&i__2, &a[i + i * a_dim1], &a[min(i__3,*m) + i * a_dim1], &
                c__1, &tau[i]);
        if (i < *n) {

/*           Apply H(i) to A(i:m,i+1:n) from the left */

            aii = a[i + i * a_dim1];
            a[i + i * a_dim1] = 1.f;
            i__2 = *m - i + 1;
            i__3 = *n - i;
            slarf_("Left", &i__2, &i__3, &a[i + i * a_dim1], &c__1, &tau[i], &
                    a[i + (i + 1) * a_dim1], lda, &work[1], 4L);
            a[i + i * a_dim1] = aii;
        }
/* L10: */
    }
    return 0;

/*     End of SGEQR2 */

} /* sgeqr2_ */

