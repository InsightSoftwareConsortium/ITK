#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

/* Table of constant values */
static integer c__1 = 1;
static integer c_n1 = -1;
static doublereal c_b23 = 1.;
static doublereal c_b37 = -1.;

/* Subroutine */ void dlatdf_(ijob, n, z, ldz, rhs, rdsum, rdscal, ipiv, jpiv)
integer *ijob, *n;
doublereal *z;
integer *ldz;
doublereal *rhs, *rdsum, *rdscal;
integer *ipiv, *jpiv;
{
    /* System generated locals */
    integer nm1;

    /* Local variables */
    static integer info;
    static doublereal temp, work[32];
    static integer i, j, k;
    static doublereal pmone;
    static doublereal sminu;
    static integer iwork[8];
    static doublereal splus;
    static doublereal bm, bp;
    static doublereal xm[8], xp[8];

/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     June 30, 1999 */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  DLATDF uses the LU factorization of the n-by-n matrix Z computed by   */
/*  DGETC2 and computes a contribution to the reciprocal Dif-estimate     */
/*  by solving Z * x = b for x, and choosing the r.h.s. b such that       */
/*  the norm of x is as large as possible. On entry RHS = b holds the     */
/*  contribution from earlier solved sub-systems, and on return RHS = x.  */
/*                                                                        */
/*  The factorization of Z returned by DGETC2 has the form Z = P*L*U*Q,   */
/*  where P and Q are permutation matrices. L is lower triangular with    */
/*  unit diagonal elements and U is upper triangular.                     */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  IJOB    (input) INTEGER                                               */
/*          IJOB = 2: First compute an approximative null-vector e        */
/*              of Z using DGECON, e is normalized and solve for          */
/*              Zx = +-e - f with the sign giving the greater value       */
/*              of 2-norm(x). About 5 times as expensive as Default.      */
/*          IJOB .ne. 2: Local look ahead strategy where all entries of   */
/*              the r.h.s. b is choosen as either +1 or -1 (Default).     */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The number of columns of the matrix Z.                        */
/*                                                                        */
/*  Z       (input) DOUBLE PRECISION array, dimension (LDZ, N)            */
/*          On entry, the LU part of the factorization of the n-by-n      */
/*          matrix Z computed by DGETC2:  Z = P * L * U * Q               */
/*                                                                        */
/*  LDZ     (input) INTEGER                                               */
/*          The leading dimension of the array Z.  LDA >= max(1, N).      */
/*                                                                        */
/*  RHS     (input/output) DOUBLE PRECISION array, dimension N.           */
/*          On entry, RHS contains contributions from other subsystems.   */
/*          On exit, RHS contains the solution of the subsystem with      */
/*          entries acoording to the value of IJOB (see above).           */
/*                                                                        */
/*  RDSUM   (input/output) DOUBLE PRECISION                               */
/*          On entry, the sum of squares of computed contributions to     */
/*          the Dif-estimate under computation by DTGSYL, where the       */
/*          scaling factor RDSCAL (see below) has been factored out.      */
/*          On exit, the corresponding sum of squares updated with the    */
/*          contributions from the current sub-system.                    */
/*          If TRANS = 'T' RDSUM is not touched.                          */
/*          NOTE: RDSUM only makes sense when DTGSY2 is called by STGSYL. */
/*                                                                        */
/*  RDSCAL  (input/output) DOUBLE PRECISION                               */
/*          On entry, scaling factor used to prevent overflow in RDSUM.   */
/*          On exit, RDSCAL is updated w.r.t. the current contributions   */
/*          in RDSUM.                                                     */
/*          If TRANS = 'T', RDSCAL is not touched.                        */
/*          NOTE: RDSCAL only makes sense when DTGSY2 is called by        */
/*                DTGSYL.                                                 */
/*                                                                        */
/*  IPIV    (input) INTEGER array, dimension (N).                         */
/*          The pivot indices; for 1 <= i <= N, row i of the              */
/*          matrix has been interchanged with row IPIV(i).                */
/*                                                                        */
/*  JPIV    (input) INTEGER array, dimension (N).                         */
/*          The pivot indices; for 1 <= j <= N, column j of the           */
/*          matrix has been interchanged with column JPIV(j).             */
/*                                                                        */
/*  Further Details                                                       */
/*  ===============                                                       */
/*                                                                        */
/*  Based on contributions by                                             */
/*     Bo Kagstrom and Peter Poromaa, Department of Computing Science,    */
/*     Umea University, S-901 87 Umea, Sweden.                            */
/*                                                                        */
/*  This routine is a further developed implementation of algorithm       */
/*  BSOLVE in [1] using complete pivoting in the LU factorization.        */
/*                                                                        */
/*  [1] Bo Kagstrom and Lars Westin,                                      */
/*      Generalized Schur Methods with Condition Estimators for           */
/*      Solving the Generalized Sylvester Equation, IEEE Transactions     */
/*      on Automatic Control, Vol. 34, No. 7, July 1989, pp 745-751.      */
/*                                                                        */
/*  [2] Peter Poromaa,                                                    */
/*      On Efficient and Robust Estimators for the Separation             */
/*      between two Regular Matrix Pairs with Applications in             */
/*      Condition Estimation. Report IMINF-95.05, Departement of          */
/*      Computing Science, Umea University, S-901 87 Umea, Sweden, 1995.  */
/*                                                                        */
/*  ===================================================================== */

    if (*ijob != 2) {

/*        Apply permutations IPIV to RHS */

        nm1 = *n - 1;
        dlaswp_(&c__1, rhs, ldz, &c__1, &nm1, ipiv, &c__1);

/*        Solve for L-part choosing RHS either to +1 or -1. */

        pmone = -1.;

        for (j = 0; j < *n-1; ++j) {
            bp = rhs[j] + 1.;
            bm = rhs[j] - 1.;
            splus = 1.;

/*           Look-ahead for L-part RHS(1:N-1) = + or -1, SPLUS and */
/*           SMIN computed more efficiently than in BSOLVE [1]. */

            nm1 = *n - j - 1;
            splus += ddot_(&nm1, &z[j+1 + j * *ldz], &c__1, &z[j+1 + j * *ldz], &c__1);
            sminu  = ddot_(&nm1, &z[j+1 + j * *ldz], &c__1, &rhs[j+1], &c__1);
            splus *= rhs[j];
            if (splus > sminu) {
                rhs[j] = bp;
            } else if (sminu > splus) {
                rhs[j] = bm;
            } else {

/*              In this case the updating sums are equal and we can */
/*              choose RHS(J) +1 or -1. The first time this happens */
/*              we choose -1, thereafter +1. This is a simple way to */
/*              get good estimates of matrices like Byers well-known */
/*              example (see [1]). (Not done in BSOLVE.) */

                rhs[j] += pmone;
                pmone = 1.;
            }

/*           Compute the remaining r.h.s. */

            temp = -rhs[j];
            daxpy_(&nm1, &temp, &z[j+1 + j * *ldz], &c__1, &rhs[j+1], &c__1);
        }

/*        Solve for U-part, look-ahead for RHS(N) = +-1. This is not done */
/*        in BSOLVE and will hopefully give us a better estimate because */
/*        any ill-conditioning of the original matrix is transfered to U */
/*        and not to L. U(N, N) is an approximation to sigma_min(LU). */

        nm1 = *n - 1;
        dcopy_(&nm1, rhs, &c__1, xp, &c__1);
        xp[nm1] = rhs[nm1] + 1.;
        rhs[nm1] += -1.;
        splus = 0.;
        sminu = 0.;
        for (i = *n-1; i >= 0; --i) {
            temp = 1. / z[i + i * *ldz];
            xp[i] *= temp;
            rhs[i] *= temp;
            for (k = i+1; k < *n; ++k) {
                xp[i] -= xp[k] * (z[i + k * *ldz] * temp);
                rhs[i] -= rhs[k] * (z[i + k * *ldz] * temp);
            }
            splus += abs(xp[i]);
            sminu += abs(rhs[i]);
        }
        if (splus > sminu) {
            dcopy_(n, xp, &c__1, rhs, &c__1);
        }

/*        Apply the permutations JPIV to the computed solution (RHS) */

        dlaswp_(&c__1, rhs, ldz, &c__1, &nm1, jpiv, &c_n1);

/*        Compute the sum of squares */

        dlassq_(n, rhs, &c__1, rdscal, rdsum);

    } else {

/*        IJOB = 2, Compute approximate nullvector XM of Z */

        dgecon_("I", n, z, ldz, &c_b23, &temp, work, iwork, &info);
        dcopy_(n, &work[*n], &c__1, xm, &c__1);

/*        Compute RHS */

        nm1 = *n - 1;
        dlaswp_(&c__1, xm, ldz, &c__1, &nm1, ipiv, &c_n1);
        temp = 1. / sqrt(ddot_(n, xm, &c__1, xm, &c__1));
        dscal_(n, &temp, xm, &c__1);
        dcopy_(n, xm, &c__1, xp, &c__1);
        daxpy_(n, &c_b23, rhs, &c__1, xp, &c__1);
        daxpy_(n, &c_b37, xm, &c__1, rhs, &c__1);
        dgesc2_(n, z, ldz, rhs, ipiv, jpiv, &temp);
        dgesc2_(n, z, ldz, xp, ipiv, jpiv, &temp);
        if (dasum_(n, xp, &c__1) > dasum_(n, rhs, &c__1)) {
            dcopy_(n, xp, &c__1, rhs, &c__1);
        }

/*        Compute the sum of squares */

        dlassq_(n, rhs, &c__1, rdscal, rdsum);
    }
} /* dlatdf_ */
