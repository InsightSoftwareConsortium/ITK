#include "f2c.h"
#include "netlib.h"

/* Subroutine */ void dormr2_(char *side, char *trans, integer *m, integer *n,
        integer *k, doublereal *a, integer *lda, doublereal *tau, doublereal *c, integer *ldc, doublereal *work, integer *info)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static logical left;
    static integer i;
    static integer i1, i2, i3, mi, ni, nq;
    static logical notran;
    static doublereal aii;

/*  -- LAPACK routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     February 29, 1992 */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  DORMR2 overwrites the general real m by n matrix C with               */
/*                                                                        */
/*        Q * C  if SIDE = 'L' and TRANS = 'N', or                        */
/*                                                                        */
/*        Q'* C  if SIDE = 'L' and TRANS = 'T', or                        */
/*                                                                        */
/*        C * Q  if SIDE = 'R' and TRANS = 'N', or                        */
/*                                                                        */
/*        C * Q' if SIDE = 'R' and TRANS = 'T',                           */
/*                                                                        */
/*  where Q is a real orthogonal matrix defined as the product of k       */
/*  elementary reflectors                                                 */
/*                                                                        */
/*        Q = H(1) H(2) . . . H(k)                                        */
/*                                                                        */
/*  as returned by DGERQF. Q is of order m if SIDE = 'L' and of order n   */
/*  if SIDE = 'R'.                                                        */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  SIDE    (input) CHARACTER*1                                           */
/*          = 'L': apply Q or Q' from the Left                            */
/*          = 'R': apply Q or Q' from the Right                           */
/*                                                                        */
/*  TRANS   (input) CHARACTER*1                                           */
/*          = 'N': apply Q  (No transpose)                                */
/*          = 'T': apply Q' (Transpose)                                   */
/*                                                                        */
/*  M       (input) INTEGER                                               */
/*          The number of rows of the matrix C. M >= 0.                   */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The number of columns of the matrix C. N >= 0.                */
/*                                                                        */
/*  K       (input) INTEGER                                               */
/*          The number of elementary reflectors whose product defines     */
/*          the matrix Q.                                                 */
/*          If SIDE = 'L', M >= K >= 0;                                   */
/*          if SIDE = 'R', N >= K >= 0.                                   */
/*                                                                        */
/*  A       (input) DOUBLE PRECISION array, dimension                     */
/*                               (LDA,M) if SIDE = 'L',                   */
/*                               (LDA,N) if SIDE = 'R'                    */
/*          The i-th row must contain the vector which defines the        */
/*          elementary reflector H(i), for i = 1,2,...,k, as returned by  */
/*          DGERQF in the last k rows of its array argument A.            */
/*          A is modified by the routine but restored on exit.            */
/*                                                                        */
/*  LDA     (input) INTEGER                                               */
/*          The leading dimension of the array A. LDA >= max(1,K).        */
/*                                                                        */
/*  TAU     (input) DOUBLE PRECISION array, dimension (K)                 */
/*          TAU(i) must contain the scalar factor of the elementary       */
/*          reflector H(i), as returned by DGERQF.                        */
/*                                                                        */
/*  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)      */
/*          On entry, the m by n matrix C.                                */
/*          On exit, C is overwritten by Q*C or Q'*C or C*Q' or C*Q.      */
/*                                                                        */
/*  LDC     (input) INTEGER                                               */
/*          The leading dimension of the array C. LDC >= max(1,M).        */
/*                                                                        */
/*  WORK    (workspace) DOUBLE PRECISION array, dimension                 */
/*                                   (N) if SIDE = 'L',                   */
/*                                   (M) if SIDE = 'R'                    */
/*                                                                        */
/*  INFO    (output) INTEGER                                              */
/*          = 0: successful exit                                          */
/*          < 0: if INFO = -i, the i-th argument had an illegal value     */
/*                                                                        */
/*  ===================================================================== */

/*     Test the input arguments */

    *info = 0;
    left = lsame_(side, "L");
    notran = lsame_(trans, "N");

/*     NQ is the order of Q */

    if (left) {
        nq = *m;
    } else {
        nq = *n;
    }
    if (! left && ! lsame_(side, "R")) {
        *info = -1;
    } else if (! notran && ! lsame_(trans, "T")) {
        *info = -2;
    } else if (*m < 0) {
        *info = -3;
    } else if (*n < 0) {
        *info = -4;
    } else if (*k < 0 || *k > nq) {
        *info = -5;
    } else if (*lda < max(1,*k)) {
        *info = -7;
    } else if (*ldc < max(1,*m)) {
        *info = -10;
    }
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("DORMR2", &i__1);
        return;
    }

/*     Quick return if possible */

    if (*m == 0 || *n == 0 || *k == 0) {
        return;
    }

    if ( (left && !notran) || (!left && notran) ) {
        i1 = 0;
        i2 = *k - 1;
        i3 = 1;
    } else {
        i1 = *k - 1;
        i2 = 0;
        i3 = -1;
    }

    if (left) {
        ni = *n;
    } else {
        mi = *m;
    }

    for (i = i1; i3 < 0 ? i >= i2 : i <= i2; i += i3) {
        if (left) {

/*           H(i) is applied to C(1:m-k+i,1:n) */

            mi = *m - *k + i + 1;
        } else {

/*           H(i) is applied to C(1:m,1:n-k+i) */

            ni = *n - *k + i + 1;
        }

/*        Apply H(i) */

        aii = a[i + (nq - *k + i) * *lda];
        a[i + (nq - *k + i) * *lda] = 1.;
        dlarf_(side, &mi, &ni, &a[i], lda, &tau[i], c, ldc, work);
        a[i + (nq - *k + i) * *lda] = aii;
    }
} /* dormr2_ */
