#include "f2c.h"
#include "netlib.h"

/* Table of constant values */
static integer c__1 = 1;

/* Subroutine */ void dorg2r_(integer *m, integer *n, integer *k, doublereal *a,
         integer *lda, doublereal *tau, doublereal *work, integer *info)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1;

    /* Local variables */
    static integer i, j, l;

/*  -- LAPACK routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     February 29, 1992 */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  DORG2R generates an m by n real matrix Q with orthonormal columns,    */
/*  which is defined as the first n columns of a product of k elementary  */
/*  reflectors of order m                                                 */
/*                                                                        */
/*        Q  =  H(1) H(2) . . . H(k)                                      */
/*                                                                        */
/*  as returned by DGEQRF.                                                */
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
/*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)      */
/*          On entry, the i-th column must contain the vector which       */
/*          defines the elementary reflector H(i), for i = 1,2,...,k, as  */
/*          returned by DGEQRF in the first k columns of its array        */
/*          argument A.                                                   */
/*          On exit, the m-by-n matrix Q.                                 */
/*                                                                        */
/*  LDA     (input) INTEGER                                               */
/*          The first dimension of the array A. LDA >= max(1,M).          */
/*                                                                        */
/*  TAU     (input) DOUBLE PRECISION array, dimension (K)                 */
/*          TAU(i) must contain the scalar factor of the elementary       */
/*          reflector H(i), as returned by DGEQRF.                        */
/*                                                                        */
/*  WORK    (workspace) DOUBLE PRECISION array, dimension (N)             */
/*                                                                        */
/*  INFO    (output) INTEGER                                              */
/*          = 0: successful exit                                          */
/*          < 0: if INFO = -i, the i-th argument has an illegal value     */
/*                                                                        */
/*  ===================================================================== */

/*     Test the input arguments */

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
        xerbla_("DORG2R", &i__1);
        return;
    }

/*     Quick return if possible */

    if (*n <= 0) {
        return;
    }

/*     Initialise columns k+1:n to columns of the unit matrix */

    for (j = *k; j < *n; ++j) {
        for (l = 0; l < *m; ++l) {
            a[l + j * *lda] = 0.;
        }
        a[j + j * *lda] = 1.;
    }

    for (i = *k - 1; i >= 0; --i) {

/*        Apply H(i) to A(i:m,i:n) from the left */

        if (i < *n - 1) {
            a[i + i * *lda] = 1.;
            i__1 = *m - i;
            i__2 = *n - i - 1;
            dlarf_("Left", &i__1, &i__2, &a[i + i * *lda], &c__1, &tau[i], &a[i + (i + 1) * *lda], lda, work);
        }
        if (i < *m - 1) {
            i__1 = *m - i - 1;
            d__1 = -tau[i];
            dscal_(&i__1, &d__1, &a[i + 1 + i * *lda], &c__1);
        }
        a[i + i * *lda] = 1. - tau[i];

/*        Set A(1:i-1,i) to zero */

        for (l = 0; l < i-1; ++l) {
            a[l + i * *lda] = 0.;
        }
    }
} /* dorg2r_ */
