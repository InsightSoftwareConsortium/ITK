#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, Oct 2003: manual optimisation and clean-up */

/* Subroutine */ void dgerq2_(const integer *m, const integer *n, doublereal *a, const integer *lda,
                              doublereal *tau, doublereal *work, integer *info)
{
    /* Local variables */
    static integer i, k, k1, k2, k3;
    static doublereal aii;

/*  -- LAPACK routine (version 2.0) --                                    */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,        */
/*     Courant Institute, Argonne National Lab, and Rice University       */
/*     February 29, 1992                                                  */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  DGERQ2 computes an RQ factorization of a real m by n matrix A:        */
/*  A = R * Q.                                                            */
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
/*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)      */
/*          On entry, the m by n matrix A.                                */
/*          On exit, if m <= n, the upper triangle of the subarray        */
/*          A(1:m,n-m+1:n) contains the m by m upper triangular matrix R; */
/*          if m >= n, the elements on and above the (m-n)-th subdiagonal */
/*          contain the m by n upper trapezoidal matrix R; the remaining  */
/*          elements, with the array TAU, represent the orthogonal matrix */
/*          Q as a product of elementary reflectors (see Further          */
/*          Details).                                                     */
/*                                                                        */
/*  LDA     (input) INTEGER                                               */
/*          The leading dimension of the array A.  LDA >= max(1,M).       */
/*                                                                        */
/*  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))         */
/*          The scalar factors of the elementary reflectors (see Further  */
/*          Details).                                                     */
/*                                                                        */
/*  WORK    (workspace) DOUBLE PRECISION array, dimension (M)             */
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
/*  v(n-k+i+1:n) = 0 and v(n-k+i) = 1; v(1:n-k+i-1) is stored on exit in  */
/*  A(m-k+i,1:n-k+i-1), and tau in TAU(i).                                */
/*                                                                        */
/*  ===================================================================== */

    /* Test the input arguments */

    *info = 0;
    if (*m < 0)                *info = 1;
    else if (*n < 0)           *info = 2;
    else if (*lda < max(1,*m)) *info = 4;
    if (*info != 0) {
        xerbla_("DGERQ2", info);
        *info = -(*info);
        return;
    }

    k = min(*m,*n);
    k1 = *m;
    k2 = *n + 1;
    k3 = *m + *n * *lda;

    for (i = k-1; i >= 0; --i)
    {
        --k1; --k2; k3 -= *lda + 1;

        /* Generate elementary reflector H(i) to annihilate A(m-k+i,1:n-k+i-1) */
        dlarfg_(&k2, &a[k3], &a[k1], lda, &tau[i]);

        /* Apply H(i) to A(1:m-k+i-1,1:n-k+i) from the right */
        aii = a[k3]; a[k3] = 1.;
        dlarf_("Right", &k1, &k2, &a[k1], lda, &tau[i], a, lda, work);
        a[k3] = aii;
    }
} /* dgerq2_ */
