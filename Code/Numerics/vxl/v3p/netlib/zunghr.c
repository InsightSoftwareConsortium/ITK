/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int zunghr_(n, ilo, ihi, a, lda, tau, work, lwork, info)
integer *n, *ilo, *ihi;
doublecomplex *a;
integer *lda;
doublecomplex *tau, *work;
integer *lwork, *info;
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4;

    /* Local variables */
    static integer i, j, iinfo, nh;
    extern /* Subroutine */ int xerbla_(), zungqr_();


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

/*  ZUNGHR generates a complex unitary matrix Q which is defined as the */
/*  product of IHI-ILO elementary reflectors of order N, as returned by */
/*  ZGEHRD: */

/*  Q = H(ilo) H(ilo+1) . . . H(ihi-1). */

/*  Arguments */
/*  ========= */

/*  N       (input) INTEGER */
/*          The order of the matrix Q. N >= 0. */

/*  ILO     (input) INTEGER */
/*  IHI     (input) INTEGER */
/*          ILO and IHI must have the same values as in the previous call
*/
/*          of ZGEHRD. Q is equal to the unit matrix except in the */
/*          submatrix Q(ilo+1:ihi,ilo+1:ihi). */
/*          1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0. */

/*  A       (input/output) COMPLEX*16 array, dimension (LDA,N) */
/*          On entry, the vectors which define the elementary reflectors,
*/
/*          as returned by ZGEHRD. */
/*          On exit, the N-by-N unitary matrix Q. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A. LDA >= max(1,N). */

/*  TAU     (input) COMPLEX*16 array, dimension (N-1) */
/*          TAU(i) must contain the scalar factor of the elementary */
/*          reflector H(i), as returned by ZGEHRD. */

/*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK) */
/*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK. */

/*  LWORK   (input) INTEGER */
/*          The dimension of the array WORK. LWORK >= IHI-ILO. */
/*          For optimum performance LWORK >= (IHI-ILO)*NB, where NB is */
/*          the optimal blocksize. */

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value */

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
    if (*n < 0) {
        *info = -1;
    } else if (*ilo < 1 || *ilo > max(1,*n)) {
        *info = -2;
    } else if (*ihi < min(*ilo,*n) || *ihi > *n) {
        *info = -3;
    } else if (*lda < max(1,*n)) {
        *info = -5;
    } else /* if(complicated condition) */ {
/* Computing MAX */
        i__1 = 1, i__2 = *ihi - *ilo;
        if (*lwork < max(i__1,i__2)) {
            *info = -8;
        }
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("ZUNGHR", &i__1, 6L);
        return 0;
    }

/*     Quick return if possible */

    if (*n == 0) {
        work[1].r = 1., work[1].i = 0.;
        return 0;
    }

/*     Shift the vectors which define the elementary reflectors one */
/*     column to the right, and set the first ilo and the last n-ihi */
/*     rows and columns to those of the unit matrix */

    i__1 = *ilo + 1;
    for (j = *ihi; j >= i__1; --j) {
        i__2 = j - 1;
        for (i = 1; i <= i__2; ++i) {
            i__3 = i + j * a_dim1;
            a[i__3].r = 0., a[i__3].i = 0.;
/* L10: */
        }
        i__2 = *ihi;
        for (i = j + 1; i <= i__2; ++i) {
            i__3 = i + j * a_dim1;
            i__4 = i + (j - 1) * a_dim1;
            a[i__3].r = a[i__4].r, a[i__3].i = a[i__4].i;
/* L20: */
        }
        i__2 = *n;
        for (i = *ihi + 1; i <= i__2; ++i) {
            i__3 = i + j * a_dim1;
            a[i__3].r = 0., a[i__3].i = 0.;
/* L30: */
        }
/* L40: */
    }
    i__1 = *ilo;
    for (j = 1; j <= i__1; ++j) {
        i__2 = *n;
        for (i = 1; i <= i__2; ++i) {
            i__3 = i + j * a_dim1;
            a[i__3].r = 0., a[i__3].i = 0.;
/* L50: */
        }
        i__2 = j + j * a_dim1;
        a[i__2].r = 1., a[i__2].i = 0.;
/* L60: */
    }
    i__1 = *n;
    for (j = *ihi + 1; j <= i__1; ++j) {
        i__2 = *n;
        for (i = 1; i <= i__2; ++i) {
            i__3 = i + j * a_dim1;
            a[i__3].r = 0., a[i__3].i = 0.;
/* L70: */
        }
        i__2 = j + j * a_dim1;
        a[i__2].r = 1., a[i__2].i = 0.;
/* L80: */
    }

    nh = *ihi - *ilo;
    if (nh > 0) {

/*        Generate Q(ilo+1:ihi,ilo+1:ihi) */

        zungqr_(&nh, &nh, &nh, &a[*ilo + 1 + (*ilo + 1) * a_dim1], lda, &tau[*
                ilo], &work[1], lwork, &iinfo);
    }
    return 0;

/*     End of ZUNGHR */

} /* zunghr_ */

