#include "f2c.h"
#include "netlib.h"

/* Subroutine */ void dorgr2_(m, n, k, a, lda, tau, work, info)
integer *m, *n, *k;
doublereal *a;
integer *lda;
doublereal *tau, *work;
integer *info;
{
    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Local variables */
    static integer i, j, l;
    static integer ii, ij;

/*  -- LAPACK routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     February 29, 1992 */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  DORGR2 generates an m by n real matrix Q with orthonormal rows,       */
/*  which is defined as the last m rows of a product of k elementary      */
/*  reflectors of order n                                                 */
/*                                                                        */
/*        Q  =  H(1) H(2) . . . H(k)                                      */
/*                                                                        */
/*  as returned by DGERQF.                                                */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  M       (input) INTEGER                                               */
/*          The number of rows of the matrix Q. M >= 0.                   */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The number of columns of the matrix Q. N >= M.                */
/*                                                                        */
/*  K       (input) INTEGER                                               */
/*          The number of elementary reflectors whose product defines the */
/*          matrix Q. M >= K >= 0.                                        */
/*                                                                        */
/*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)      */
/*          On entry, the (m-k+i)-th row must contain the vector which    */
/*          defines the elementary reflector H(i), for i = 1,2,...,k, as  */
/*          returned by DGERQF in the last k rows of its array argument   */
/*          A.                                                            */
/*          On exit, the m by n matrix Q.                                 */
/*                                                                        */
/*  LDA     (input) INTEGER                                               */
/*          The first dimension of the array A. LDA >= max(1,M).          */
/*                                                                        */
/*  TAU     (input) DOUBLE PRECISION array, dimension (K)                 */
/*          TAU(i) must contain the scalar factor of the elementary       */
/*          reflector H(i), as returned by DGERQF.                        */
/*                                                                        */
/*  WORK    (workspace) DOUBLE PRECISION array, dimension (M)             */
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
    } else if (*n < *m) {
        *info = -2;
    } else if (*k < 0 || *k > *m) {
        *info = -3;
    } else if (*lda < max(1,*m)) {
        *info = -5;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("DORGR2", &i__1);
        return;
    }

/*     Quick return if possible */

    if (*m <= 0) {
        return;
    }

    if (*k < *m) {

/*        Initialise rows 1:m-k to rows of the unit matrix */

        for (j = 0; j < *n; ++j) {
            for (l = 0; l < *m - *k; ++l) {
                a[l + j * *lda] = 0.;
            }
            if (j >= *n - *m && j < *n - *k) {
                a[*m - *n + j + j * *lda] = 1.;
            }
        }
    }

    for (i = 0; i < *k; ++i) {
        ii = *m - *k + i;
        ij = *n - *k + i;

/*        Apply H(i) to A(1:m-k+i,1:n-k+i) from the right */

        a[ii + ij * *lda] = 1.;
        i__1 = ij + 1;
        dlarf_("Right", &ii, &i__1, &a[ii], lda, &tau[i], a, lda, work);
        d__1 = -tau[i];
        dscal_(&ij, &d__1, &a[ii], lda);
        a[ii + ij * *lda] = 1. - tau[i];

/*        Set A(m-k+i,n-k+i+1:n) to zero */

        for (l = ij + 1; l < *n; ++l) {
            a[ii + l * *lda] = 0.;
        }
    }
} /* dorgr2_ */
