#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Table of constant values */
static integer c__1 = 1;

/* Subroutine */ void zung2r_(m, n, k, a, lda, tau, work, info)
const integer *m, *n, *k;
doublecomplex *a;
const integer *lda;
const doublecomplex *tau;
doublecomplex *work;
integer *info;
{
    /* System generated locals */
    integer i__1, i__2;
    doublecomplex z__1;

    /* Local variables */
    static integer i, j, l;

/*  -- LAPACK routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*  ===================================================================== */
/*                                                                        */
/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  ZUNG2R generates an m by n complex matrix Q with orthonormal columns, */
/*  which is defined as the first n columns of a product of k elementary  */
/*  reflectors of order m                                                 */
/*                                                                        */
/*        Q  =  H(1) H(2) . . . H(k)                                      */
/*                                                                        */
/*  as returned by ZGEQRF.                                                */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  M       (input) INTEGER                                               */
/*          The number of rows of the matrix Q. M >= 0.                   */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The number of columns of the matrix Q. M >= N >= 0.           */
/*                                                                        */
/*  K       (input) INTEGER                                               */
/*          The number of elementary reflectors whose product defines the */
/*          matrix Q. N >= K >= 0.                                        */
/*                                                                        */
/*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)            */
/*          On entry, the i-th column must contain the vector which       */
/*          defines the elementary reflector H(i), for i = 1,2,...,k, as  */
/*          returned by ZGEQRF in the first k columns of its array        */
/*          argument A.                                                   */
/*          On exit, the m by n matrix Q.                                 */
/*                                                                        */
/*  LDA     (input) INTEGER                                               */
/*          The first dimension of the array A. LDA >= max(1,M).          */
/*                                                                        */
/*  TAU     (input) COMPLEX*16 array, dimension (K)                       */
/*          TAU(i) must contain the scalar factor of the elementary       */
/*          reflector H(i), as returned by ZGEQRF.                        */
/*                                                                        */
/*  WORK    (workspace) COMPLEX*16 array, dimension (N)                   */
/*                                                                        */
/*  INFO    (output) INTEGER                                              */
/*          = 0: successful exit                                          */
/*          < 0: if INFO = -i, the i-th argument has an illegal value     */
/*                                                                        */
/*  ===================================================================== */

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
        xerbla_("ZUNG2R", &i__1);
        return;
    }

/*     Quick return if possible */

    if (*n <= 0) {
        return;
    }

/*     Initialise columns k+1:n to columns of the unit matrix */

    for (j = *k; j < *n; ++j) {
        for (l = 0; l < *m; ++l) {
            i__1 = l + j * *lda;
            a[i__1].r = 0., a[i__1].i = 0.;
        }
        i__1 = j + j * *lda;
        a[i__1].r = 1., a[i__1].i = 0.;
    }

    for (i = *k - 1; i >= 0; --i) {

/*        Apply H(i) to A(i:m,i:n) from the left */

        if (i+1 < *n) {
            i__1 = i + i * *lda;
            a[i__1].r = 1., a[i__1].i = 0.;
            i__1 = *m - i;
            i__2 = *n - i - 1;
            zlarf_("Left", &i__1, &i__2, &a[i + i * *lda], &c__1, &tau[i], &a[i + (i+1) * *lda], lda, work);
        }
        if (i+1 < *m) {
            i__1 = *m - i - 1;
            z__1.r = -tau[i].r, z__1.i = -tau[i].i;
            zscal_(&i__1, &z__1, &a[i + 1 + i * *lda], &c__1);
        }
        i__1 = i + i * *lda;
        a[i__1].r = 1. - tau[i].r,
        a[i__1].i = 0. - tau[i].i;

/*        Set A(1:i-1,i) to zero */

        for (l = 0; l < i; ++l) {
            i__1 = l + i * *lda;
            a[i__1].r = 0., a[i__1].i = 0.;
        }
    }
} /* zung2r_ */
