/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* Subroutine */ int zung2r_(m, n, k, a, lda, tau, work, info)
integer *m, *n, *k;
doublecomplex *a;
integer *lda;
doublecomplex *tau, *work;
integer *info;
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    doublecomplex z__1;

    /* Local variables */
    static integer i, j, l;
    extern /* Subroutine */ int zscal_(), zlarf_(), xerbla_();


/*  -- LAPACK routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Array Arguments .. */
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZUNG2R generates an m by n complex matrix Q with orthonormal columns,
*/
/*  which is defined as the first n columns of a product of k elementary
*/
/*  reflectors of order m */

/*        Q  =  H(1) H(2) . . . H(k) */

/*  as returned by ZGEQRF. */

/*  Arguments */
/*  ========= */

/*  M       (input) INTEGER */
/*          The number of rows of the matrix Q. M >= 0. */

/*  N       (input) INTEGER */
/*          The number of columns of the matrix Q. M >= N >= 0. */

/*  K       (input) INTEGER */
/*          The number of elementary reflectors whose product defines the
*/
/*          matrix Q. N >= K >= 0. */

/*  A       (input/output) COMPLEX*16 array, dimension (LDA,N) */
/*          On entry, the i-th column must contain the vector which */
/*          defines the elementary reflector H(i), for i = 1,2,...,k, as
*/
/*          returned by ZGEQRF in the first k columns of its array */
/*          argument A. */
/*          On exit, the m by n matrix Q. */

/*  LDA     (input) INTEGER */
/*          The first dimension of the array A. LDA >= max(1,M). */

/*  TAU     (input) COMPLEX*16 array, dimension (K) */
/*          TAU(i) must contain the scalar factor of the elementary */
/*          reflector H(i), as returned by ZGEQRF. */

/*  WORK    (workspace) COMPLEX*16 array, dimension (N) */

/*  INFO    (output) INTEGER */
/*          = 0: successful exit */
/*          < 0: if INFO = -i, the i-th argument has an illegal value */

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
    } else if (*n < 0 || *n > *m) {
        *info = -2;
    } else if (*k < 0 || *k > *n) {
        *info = -3;
    } else if (*lda < max(1,*m)) {
        *info = -5;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("ZUNG2R", &i__1, 6L);
        return 0;
    }

/*     Quick return if possible */

    if (*n <= 0) {
        return 0;
    }

/*     Initialise columns k+1:n to columns of the unit matrix */

    i__1 = *n;
    for (j = *k + 1; j <= i__1; ++j) {
        i__2 = *m;
        for (l = 1; l <= i__2; ++l) {
            i__3 = l + j * a_dim1;
            a[i__3].r = 0., a[i__3].i = 0.;
/* L10: */
        }
        i__2 = j + j * a_dim1;
        a[i__2].r = 1., a[i__2].i = 0.;
/* L20: */
    }

    for (i = *k; i >= 1; --i) {

/*        Apply H(i) to A(i:m,i:n) from the left */

        if (i < *n) {
            i__1 = i + i * a_dim1;
            a[i__1].r = 1., a[i__1].i = 0.;
            i__1 = *m - i + 1;
            i__2 = *n - i;
            zlarf_("Left", &i__1, &i__2, &a[i + i * a_dim1], &c__1, &tau[i], &
                    a[i + (i + 1) * a_dim1], lda, &work[1], 4L);
        }
        if (i < *m) {
            i__1 = *m - i;
            i__2 = i;
            z__1.r = -tau[i__2].r, z__1.i = -tau[i__2].i;
            zscal_(&i__1, &z__1, &a[i + 1 + i * a_dim1], &c__1);
        }
        i__1 = i + i * a_dim1;
        i__2 = i;
        z__1.r = 1. - tau[i__2].r, z__1.i = 0. - tau[i__2].i;
        a[i__1].r = z__1.r, a[i__1].i = z__1.i;

/*        Set A(1:i-1,i) to zero */

        i__1 = i - 1;
        for (l = 1; l <= i__1; ++l) {
            i__2 = l + i * a_dim1;
            a[i__2].r = 0., a[i__2].i = 0.;
/* L30: */
        }
/* L40: */
    }
    return 0;

/*     End of ZUNG2R */

} /* zung2r_ */

