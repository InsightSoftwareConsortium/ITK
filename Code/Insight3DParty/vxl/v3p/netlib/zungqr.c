/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c_n1 = -1;
static integer c__3 = 3;
static integer c__2 = 2;

/* Subroutine */ int zungqr_(m, n, k, a, lda, tau, work, lwork, info)
integer *m, *n, *k;
doublecomplex *a;
integer *lda;
doublecomplex *tau, *work;
integer *lwork, *info;
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4;

    /* Local variables */
    static integer i, j, l, nbmin, iinfo, ib, nb, ki, kk;
    extern /* Subroutine */ int zung2r_();
    static integer nx;
    extern /* Subroutine */ int xerbla_();
    extern integer ilaenv_();
    extern /* Subroutine */ int zlarfb_();
    static integer ldwork;
    extern /* Subroutine */ int zlarft_();
    static integer iws;


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

/*  ZUNGQR generates an M-by-N complex matrix Q with orthonormal columns, 
*/
/*  which is defined as the first N columns of a product of K elementary 
*/
/*  reflectors of order M */

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
/*          On exit, the M-by-N matrix Q. */

/*  LDA     (input) INTEGER */
/*          The first dimension of the array A. LDA >= max(1,M). */

/*  TAU     (input) COMPLEX*16 array, dimension (K) */
/*          TAU(i) must contain the scalar factor of the elementary */
/*          reflector H(i), as returned by ZGEQRF. */

/*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK) */
/*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK. */

/*  LWORK   (input) INTEGER */
/*          The dimension of the array WORK. LWORK >= max(1,N). */
/*          For optimum performance LWORK >= N*NB, where NB is the */
/*          optimal blocksize. */

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit */
/*          < 0:  if INFO = -i, the i-th argument has an illegal value */

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
/*     .. External Functions .. */
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
    } else if (*lwork < max(1,*n)) {
	*info = -8;
    }
    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("ZUNGQR", &i__1, 6L);
	return 0;
    }

/*     Quick return if possible */

    if (*n <= 0) {
	work[1].r = 1., work[1].i = 0.;
	return 0;
    }

/*     Determine the block size. */

    nb = ilaenv_(&c__1, "ZUNGQR", " ", m, n, k, &c_n1, 6L, 1L);
    nbmin = 2;
    nx = 0;
    iws = *n;
    if (nb > 1 && nb < *k) {

/*        Determine when to cross over from blocked to unblocked code.
 */

/* Computing MAX */
	i__1 = 0, i__2 = ilaenv_(&c__3, "ZUNGQR", " ", m, n, k, &c_n1, 6L, 1L)
		;
	nx = max(i__1,i__2);
	if (nx < *k) {

/*           Determine if workspace is large enough for blocked co
de. */

	    ldwork = *n;
	    iws = ldwork * nb;
	    if (*lwork < iws) {

/*              Not enough workspace to use optimal NB:  reduc
e NB and */
/*              determine the minimum value of NB. */

		nb = *lwork / ldwork;
/* Computing MAX */
		i__1 = 2, i__2 = ilaenv_(&c__2, "ZUNGQR", " ", m, n, k, &c_n1,
			 6L, 1L);
		nbmin = max(i__1,i__2);
	    }
	}
    }

    if (nb >= nbmin && nb < *k && nx < *k) {

/*        Use blocked code after the last block. */
/*        The first kk columns are handled by the block method. */

	ki = (*k - nx - 1) / nb * nb;
/* Computing MIN */
	i__1 = *k, i__2 = ki + nb;
	kk = min(i__1,i__2);

/*        Set A(1:kk,kk+1:n) to zero. */

	i__1 = *n;
	for (j = kk + 1; j <= i__1; ++j) {
	    i__2 = kk;
	    for (i = 1; i <= i__2; ++i) {
		i__3 = i + j * a_dim1;
		a[i__3].r = 0., a[i__3].i = 0.;
/* L10: */
	    }
/* L20: */
	}
    } else {
	kk = 0;
    }

/*     Use unblocked code for the last or only block. */

    if (kk < *n) {
	i__1 = *m - kk;
	i__2 = *n - kk;
	i__3 = *k - kk;
	zung2r_(&i__1, &i__2, &i__3, &a[kk + 1 + (kk + 1) * a_dim1], lda, &
		tau[kk + 1], &work[1], &iinfo);
    }

    if (kk > 0) {

/*        Use blocked code */

	i__1 = -nb;
	for (i = ki + 1; i__1 < 0 ? i >= 1 : i <= 1; i += i__1) {
/* Computing MIN */
	    i__2 = nb, i__3 = *k - i + 1;
	    ib = min(i__2,i__3);
	    if (i + ib <= *n) {

/*              Form the triangular factor of the block reflec
tor */
/*              H = H(i) H(i+1) . . . H(i+ib-1) */

		i__2 = *m - i + 1;
		zlarft_("Forward", "Columnwise", &i__2, &ib, &a[i + i * 
			a_dim1], lda, &tau[i], &work[1], &ldwork, 7L, 10L);

/*              Apply H to A(i:m,i+ib:n) from the left */

		i__2 = *m - i + 1;
		i__3 = *n - i - ib + 1;
		zlarfb_("Left", "No transpose", "Forward", "Columnwise", &
			i__2, &i__3, &ib, &a[i + i * a_dim1], lda, &work[1], &
			ldwork, &a[i + (i + ib) * a_dim1], lda, &work[ib + 1],
			 &ldwork, 4L, 12L, 7L, 10L);
	    }

/*           Apply H to rows i:m of current block */

	    i__2 = *m - i + 1;
	    zung2r_(&i__2, &ib, &ib, &a[i + i * a_dim1], lda, &tau[i], &work[
		    1], &iinfo);

/*           Set rows 1:i-1 of current block to zero */

	    i__2 = i + ib - 1;
	    for (j = i; j <= i__2; ++j) {
		i__3 = i - 1;
		for (l = 1; l <= i__3; ++l) {
		    i__4 = l + j * a_dim1;
		    a[i__4].r = 0., a[i__4].i = 0.;
/* L30: */
		}
/* L40: */
	    }
/* L50: */
	}
    }

    work[1].r = (doublereal) iws, work[1].i = 0.;
    return 0;

/*     End of ZUNGQR */

} /* zungqr_ */

