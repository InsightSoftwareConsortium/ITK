/* sorg2r.f -- translated by f2c (version of 4 June 1993  1:43:59).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* Subroutine */ int sorg2r_(integer *m, integer *n, integer *k, real *a, 
	integer *lda, real *tau, real *work, integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;
    real r__1;

    /* Local variables */
    static integer i, j, l;
    extern /* Subroutine */ int sscal_(integer *, real *, real *, integer *), 
	    slarf_(char *, integer *, integer *, real *, integer *, real *, 
	    real *, integer *, real *, ftnlen), xerbla_(char *, integer *, 
	    ftnlen);


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

/*  SORG2R generates an m by n real matrix Q with orthonormal columns, */
/*  which is defined as the first n columns of a product of k elementary 
*/
/*  reflectors of order m */

/*        Q  =  H(1) H(2) . . . H(k) */

/*  as returned by SGEQRF. */

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

/*  A       (input/output) REAL array, dimension (LDA,N) */
/*          On entry, the i-th column must contain the vector which */
/*          defines the elementary reflector H(i), for i = 1,2,...,k, as 
*/
/*          returned by SGEQRF in the first k columns of its array */
/*          argument A. */
/*          On exit, the m-by-n matrix Q. */

/*  LDA     (input) INTEGER */
/*          The first dimension of the array A. LDA >= max(1,M). */

/*  TAU     (input) REAL array, dimension (K) */
/*          TAU(i) must contain the scalar factor of the elementary */
/*          reflector H(i), as returned by SGEQRF. */

/*  WORK    (workspace) REAL array, dimension (N) */

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
	xerbla_("SORG2R", &i__1, 6L);
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
	    a[l + j * a_dim1] = 0.f;
/* L10: */
	}
	a[j + j * a_dim1] = 1.f;
/* L20: */
    }

    for (i = *k; i >= 1; --i) {

/*        Apply H(i) to A(i:m,i:n) from the left */

	if (i < *n) {
	    a[i + i * a_dim1] = 1.f;
	    i__1 = *m - i + 1;
	    i__2 = *n - i;
	    slarf_("Left", &i__1, &i__2, &a[i + i * a_dim1], &c__1, &tau[i], &
		    a[i + (i + 1) * a_dim1], lda, &work[1], 4L);
	}
	if (i < *m) {
	    i__1 = *m - i;
	    r__1 = -tau[i];
	    sscal_(&i__1, &r__1, &a[i + 1 + i * a_dim1], &c__1);
	}
	a[i + i * a_dim1] = 1.f - tau[i];

/*        Set A(1:i-1,i) to zero */

	i__1 = i - 1;
	for (l = 1; l <= i__1; ++l) {
	    a[l + i * a_dim1] = 0.f;
/* L30: */
	}
/* L40: */
    }
    return 0;

/*     End of SORG2R */

} /* sorg2r_ */

