#include "f2c.h"
#include "netlib.h"

/* Table of constant values */
static integer c__1 = 1;
static integer c_n1 = -1;

/* Subroutine */ void dgesc2_(n, a, lda, rhs, ipiv, jpiv, scale)
integer *n;
doublereal *a;
integer *lda;
doublereal *rhs;
integer *ipiv, *jpiv;
doublereal *scale;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static doublereal temp;
    static integer i, j;
    static doublereal bignum;
    static doublereal smlnum, eps;

/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     June 30, 1999 */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  DGESC2 solves a system of linear equations                            */
/*                                                                        */
/*            A * X = scale* RHS                                          */
/*                                                                        */
/*  with a general N-by-N matrix A using the LU factorization with        */
/*  complete pivoting computed by DGETC2.                                 */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The order of the matrix A.                                    */
/*                                                                        */
/*  A       (input) DOUBLE PRECISION array, dimension (LDA,N)             */
/*          On entry, the  LU part of the factorization of the n-by-n     */
/*          matrix A computed by DGETC2:  A = P * L * U * Q               */
/*                                                                        */
/*  LDA     (input) INTEGER                                               */
/*          The leading dimension of the array A.  LDA >= max(1, N).      */
/*                                                                        */
/*  RHS     (input/output) DOUBLE PRECISION array, dimension (N).         */
/*          On entry, the right hand side vector b.                       */
/*          On exit, the solution vector X.                               */
/*                                                                        */
/*  IPIV    (iput) INTEGER array, dimension (N).                          */
/*          The pivot indices; for 1 <= i <= N, row i of the              */
/*          matrix has been interchanged with row IPIV(i).                */
/*                                                                        */
/*  JPIV    (iput) INTEGER array, dimension (N).                          */
/*          The pivot indices; for 1 <= j <= N, column j of the           */
/*          matrix has been interchanged with column JPIV(j).             */
/*                                                                        */
/*  SCALE    (output) DOUBLE PRECISION                                    */
/*           On exit, SCALE contains the scale factor. SCALE is chosen    */
/*           0 <= SCALE <= 1 to prevent owerflow in the solution.         */
/*                                                                        */
/*  Further Details                                                       */
/*  ===============                                                       */
/*                                                                        */
/*  Based on contributions by                                             */
/*     Bo Kagstrom and Peter Poromaa, Department of Computing Science,    */
/*     Umea University, S-901 87 Umea, Sweden.                            */
/*                                                                        */
/*  ===================================================================== */

/*      Set constant to control owerflow */

    eps = dlamch_("P");
    smlnum = dlamch_("S") / eps;
    bignum = 1. / smlnum;
    dlabad_(&smlnum, &bignum);

/*     Apply permutations IPIV to RHS */

    i__1 = *n - 1;
    dlaswp_(&c__1, rhs, lda, &c__1, &i__1, ipiv, &c__1);

/*     Solve for L part */

    for (i = 0; i < *n - 1; ++i) {
        for (j = i + 1; j < *n; ++j) {
            rhs[j] -= a[j + i * *lda] * rhs[i];
        }
    }

/*     Solve for U part */

    *scale = 1.;

/*     Check for scaling */

    i = idamax_(n, rhs, &c__1) - 1;
    if (smlnum * 2. * abs(rhs[i]) > abs(a[*n-1 + (*n-1) * *lda])) {
        temp = .5 / abs(rhs[i]);
        dscal_(n, &temp, rhs, &c__1);
        *scale *= temp;
    }

    for (i = *n-1; i >= 0; --i) {
        temp = 1. / a[i + i * *lda];
        rhs[i] *= temp;
        for (j = i + 1; j < *n; ++j) {
            rhs[i] -= rhs[j] * (a[i + j * *lda] * temp);
        }
    }

/*     Apply permutations JPIV to the solution (RHS) */

    i__1 = *n - 1;
    dlaswp_(&c__1, rhs, lda, &c__1, &i__1, jpiv, &c_n1);

} /* dgesc2_ */
