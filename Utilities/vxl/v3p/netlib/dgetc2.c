#include "f2c.h"
#include "netlib.h"

/* Table of constant values */
static integer c__1 = 1;
static doublereal c_b10 = -1.;

/* Subroutine */ void dgetc2_(n, a, lda, ipiv, jpiv, info)
integer *n;
doublereal *a;
integer *lda, *ipiv, *jpiv, *info;
{
    /* Local variables */
    static doublereal smin, xmax;
    static integer i, j;
    static integer ip, jp;
    static doublereal bignum, smlnum, eps;
    static integer ipv, jpv;


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     June 30, 1999 */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  DGETC2 computes an LU factorization with complete pivoting of the     */
/*  n-by-n matrix A. The factorization has the form A = P * L * U * Q,    */
/*  where P and Q are permutation matrices, L is lower triangular with    */
/*  unit diagonal elements and U is upper triangular.                     */
/*                                                                        */
/*  This is the Level 2 BLAS algorithm.                                   */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The order of the matrix A. N >= 0.                            */
/*                                                                        */
/*  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)     */
/*          On entry, the n-by-n matrix A to be factored.                 */
/*          On exit, the factors L and U from the factorization           */
/*          A = P*L*U*Q; the unit diagonal elements of L are not stored.  */
/*          If U(k, k) appears to be less than SMIN, U(k, k) is given the */
/*          value of SMIN, i.e., giving a nonsingular perturbed system.   */
/*                                                                        */
/*  LDA     (input) INTEGER                                               */
/*          The leading dimension of the array A.  LDA >= max(1,N).       */
/*                                                                        */
/*  IPIV    (output) INTEGER array, dimension(N).                         */
/*          The pivot indices; for 1 <= i <= N, row i of the              */
/*          matrix has been interchanged with row IPIV(i).                */
/*                                                                        */
/*  JPIV    (output) INTEGER array, dimension(N).                         */
/*          The pivot indices; for 1 <= j <= N, column j of the           */
/*          matrix has been interchanged with column JPIV(j).             */
/*                                                                        */
/*  INFO    (output) INTEGER                                              */
/*           = 0: successful exit                                         */
/*           > 0: if INFO = k, U(k, k) is likely to produce owerflow if   */
/*                we try to solve for x in Ax = b. So U is perturbed to   */
/*                avoid the overflow.                                     */
/*                                                                        */
/*  Further Details                                                       */
/*  ===============                                                       */
/*                                                                        */
/*  Based on contributions by                                             */
/*     Bo Kagstrom and Peter Poromaa, Department of Computing Science,    */
/*     Umea University, S-901 87 Umea, Sweden.                            */
/*                                                                        */
/*  ===================================================================== */

/*     Set constants to control overflow */

    *info = 0;
    eps = dlamch_("P");
    smlnum = dlamch_("S") / eps;
    bignum = 1. / smlnum;
    dlabad_(&smlnum, &bignum);

/*     Factorize A using complete pivoting. */
/*     Set pivots less than SMIN to SMIN. */

    for (i = 0; i < *n - 1; ++i) {

/*        Find max element in matrix A */

        xmax = 0.;
        for (ip = i; ip < *n; ++ip) {
            for (jp = i; jp < *n; ++jp) {
                if (abs(a[ip + jp * *lda]) >= xmax) {
                    xmax = abs(a[ip + jp * *lda]);
                    ipv = ip;
                    jpv = jp;
                }
            }
        }
        if (i == 0) {
            smin = max(eps*xmax, smlnum);
        }

/*        Swap rows */

        if (ipv != i) {
            dswap_(n, &a[ipv], lda, &a[i], lda);
        }
        ipiv[i] = ipv+1;

/*        Swap columns */

        if (jpv != i) {
            dswap_(n, &a[jpv * *lda], &c__1, &a[i * *lda], &c__1);
        }
        jpiv[i] = jpv+1;

/*        Check for singularity */

        if (abs(a[i + i * *lda]) < smin) {
            *info = i + 1;
            a[i + i * *lda] = smin;
        }
        for (j = i + 1; j < *n; ++j) {
            a[j + i * *lda] /= a[i + i * *lda];
        }
        j = *n - i - 1;
        dger_(&j, &j, &c_b10, &a[i + 1 + i * *lda], &c__1,
              &a[i + (i + 1) * *lda], lda, &a[i + 1 + (i + 1) * *lda], lda);
    }

    if (abs(a[*n-1 + (*n-1) * *lda]) < smin) {
        *info = *n;
        a[*n-1 + (*n-1) * *lda] = smin;
    }
} /* dgetc2_ */
