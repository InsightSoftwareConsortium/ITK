#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

/* Table of constant values */
static integer c__16 = 16;
static doublereal c_b3 = 0.;
static integer c__0 = 0;
static integer c__1 = 1;
static integer c__4 = 4;
static integer c__2 = 2;
static doublereal c_b38 = 1.;
static doublereal c_b44 = -1.;

/* Subroutine */ void dtgex2_(wantq, wantz, n, a, lda, b, ldb, q, ldq, z, ldz, j1, n1, n2, work, lwork, info)
logical *wantq, *wantz;
integer *n;
doublereal *a;
integer *lda;
doublereal *b;
integer *ldb;
doublereal *q;
integer *ldq;
doublereal *z;
integer *ldz, *j1, *n1, *n2;
doublereal *work;
integer *lwork, *info;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static logical weak;
    static doublereal ddum;
    static integer idum;
    static doublereal taul[4], dsum;
    static doublereal taur[4], scpy[16] /* was [4][4] */, tcpy[16] /* was [4][4] */, f, g;
    static integer i, m;
    static doublereal s[16] /* was [4][4] */, t[16] /* was [4][4] */;
    static doublereal scale, bqra21, brqa21;
    static doublereal licop[16] /* was [4][4] */;
    static integer linfo;
    static doublereal ircop[16] /* was [4][4] */;
    static doublereal dnorm;
    static integer iwork[4];
    static doublereal be[2], ai[2];
    static doublereal ar[2], sa, sb, li[16] /* was [4][4] */;
    static doublereal dscale, ir[16] /* was [4][4] */, ss, ws;
    static logical dtrong;
    static doublereal thresh, smlnum, eps;

/*  -- LAPACK auxiliary routine (version 3.0) --                          */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,        */
/*     Courant Institute, Argonne National Lab, and Rice University       */
/*     June 30, 1999                                                      */
/*                                                                        */
/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  DTGEX2 swaps adjacent diagonal blocks (A11, B11) and (A22, B22)       */
/*  of size 1-by-1 or 2-by-2 in an upper (quasi) triangular matrix pair   */
/*  (A, B) by an orthogonal equivalence transformation.                   */
/*                                                                        */
/*  (A, B) must be in generalized real Schur canonical form (as returned  */
/*  by DGGES), i.e. A is block upper triangular with 1-by-1 and 2-by-2    */
/*  diagonal blocks. B is upper triangular.                               */
/*                                                                        */
/*  Optionally, the matrices Q and Z of generalized Schur vectors are     */
/*  updated.                                                              */
/*                                                                        */
/*         Q(in) * A(in) * Z(in)' = Q(out) * A(out) * Z(out)'             */
/*         Q(in) * B(in) * Z(in)' = Q(out) * B(out) * Z(out)'             */
/*                                                                        */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  WANTQ   (input) LOGICAL                                               */
/*          .TRUE. : update the left transformation matrix Q;             */
/*          .FALSE.: do not update Q.                                     */
/*                                                                        */
/*  WANTZ   (input) LOGICAL                                               */
/*          .TRUE. : update the right transformation matrix Z;            */
/*          .FALSE.: do not update Z.                                     */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The order of the matrices A and B. N >= 0.                    */
/*                                                                        */
/*  A      (input/output) DOUBLE PRECISION arrays, dimensions (LDA,N)     */
/*          On entry, the matrix A in the pair (A, B).                    */
/*          On exit, the updated matrix A.                                */
/*                                                                        */
/*  LDA     (input)  INTEGER                                              */
/*          The leading dimension of the array A. LDA >= max(1,N).        */
/*                                                                        */
/*  B      (input/output) DOUBLE PRECISION arrays, dimensions (LDB,N)     */
/*          On entry, the matrix B in the pair (A, B).                    */
/*          On exit, the updated matrix B.                                */
/*                                                                        */
/*  LDB     (input)  INTEGER                                              */
/*          The leading dimension of the array B. LDB >= max(1,N).        */
/*                                                                        */
/*  Q       (input/output) DOUBLE PRECISION array, dimension (LDZ,N)      */
/*          On entry, if WANTQ = .TRUE., the orthogonal matrix Q.         */
/*          On exit, the updated matrix Q.                                */
/*          Not referenced if WANTQ = .FALSE..                            */
/*                                                                        */
/*  LDQ     (input) INTEGER                                               */
/*          The leading dimension of the array Q. LDQ >= 1.               */
/*          If WANTQ = .TRUE., LDQ >= N.                                  */
/*                                                                        */
/*  Z       (input/output) DOUBLE PRECISION array, dimension (LDZ,N)      */
/*          On entry, if WANTZ =.TRUE., the orthogonal matrix Z.          */
/*          On exit, the updated matrix Z.                                */
/*          Not referenced if WANTZ = .FALSE..                            */
/*                                                                        */
/*  LDZ     (input) INTEGER                                               */
/*          The leading dimension of the array Z. LDZ >= 1.               */
/*          If WANTZ = .TRUE., LDZ >= N.                                  */
/*                                                                        */
/*  J1      (input) INTEGER                                               */
/*          The index to the first block (A11, B11). 1 <= J1 <= N.        */
/*                                                                        */
/*  N1      (input) INTEGER                                               */
/*          The order of the first block (A11, B11). N1 = 0, 1 or 2.      */
/*                                                                        */
/*  N2      (input) INTEGER                                               */
/*          The order of the second block (A22, B22). N2 = 0, 1 or 2.     */
/*                                                                        */
/*  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK).        */
/*                                                                        */
/*  LWORK   (input) INTEGER                                               */
/*          The dimension of the array WORK.                              */
/*          LWORK >=  MAX( N*(N2+N1), (N2+N1)*(N2+N1)*2 )                 */
/*                                                                        */
/*  INFO    (output) INTEGER                                              */
/*            =0: Successful exit                                         */
/*            >0: If INFO = 1, the transformed matrix (A, B) would be     */
/*                too far from generalized Schur form; the blocks are     */
/*                not swapped and (A, B) and (Q, Z) are unchanged.        */
/*                The problem of swapping is too ill-conditioned.         */
/*            <0: If INFO = -16: LWORK is too small. Appropriate value    */
/*                for LWORK is returned in WORK(1).                       */
/*                                                                        */
/*  Further Details                                                       */
/*  ===============                                                       */
/*                                                                        */
/*  Based on contributions by                                             */
/*     Bo Kagstrom and Peter Poromaa, Department of Computing Science,    */
/*     Umea University, S-901 87 Umea, Sweden.                            */
/*                                                                        */
/*  In the current code both weak and strong stability tests are          */
/*  performed. The user can omit the strong stability test by changing    */
/*  the internal logical parameter WANDS to .FALSE.. See ref. [2] for     */
/*  details.                                                              */
/*                                                                        */
/*  [1] B. Kagstrom; A Direct Method for Reordering Eigenvalues in the    */
/*      Generalized Real Schur Form of a Regular Matrix Pair (A, B), in   */
/*      M.S. Moonen et al (eds), Linear Algebra for Large Scale and       */
/*      Real-Time Applications, Kluwer Academic Publ. 1993, pp 195-218.   */
/*                                                                        */
/*  [2] B. Kagstrom and P. Poromaa; Computing Eigenspaces with Specified  */
/*      Eigenvalues of a Regular Matrix Pair (A, B) and Condition         */
/*      Estimation: Theory, Algorithms and Software,                      */
/*      Report UMINF - 94.04, Department of Computing Science, Umea       */
/*      University, S-901 87 Umea, Sweden, 1994. Also as LAPACK Working   */
/*      Note 87. To appear in Numerical Algorithms, 1996.                 */
/*                                                                        */
/*  ===================================================================== */

    *info = 0;

/*     Quick return if possible */

    if (*n <= 1 || *n1 <= 0 || *n2 <= 0) {
        return;
    }
    if (*n1 > *n || *j1 + *n1 > *n) {
        return;
    }
    m = *n1 + *n2;
    if (*lwork < max(*n * m, m * m * 2)) {
        *info = -16;
        work[0] = (doublereal) max(*n * m, m * m * 2);
        return;
    }

    /* Parameter adjustments */
    *j1 -= 1;

    weak = FALSE_;
    dtrong = FALSE_;

/*     Make a local copy of selected block */

    dcopy_(&c__16, &c_b3, &c__0, li, &c__1);
    dcopy_(&c__16, &c_b3, &c__0, ir, &c__1);
    dlacpy_("Full", &m, &m, &a[*j1 + *j1 * *lda], lda, s, &c__4);
    dlacpy_("Full", &m, &m, &b[*j1 + *j1 * *ldb], ldb, t, &c__4);

/*     Compute threshold for testing acceptance of swapping. */

    eps = dlamch_("P");
    smlnum = dlamch_("S") / eps;
    dscale = 0.;
    dsum = 1.;
    dlacpy_("Full", &m, &m, s, &c__4, work, &m);
    i__1 = m * m;
    dlassq_(&i__1, work, &c__1, &dscale, &dsum);
    dlacpy_("Full", &m, &m, t, &c__4, work, &m);
    i__1 = m * m;
    dlassq_(&i__1, work, &c__1, &dscale, &dsum);
    dnorm = dscale * sqrt(dsum);
    thresh = max(eps * 10. * dnorm, smlnum);

    if (m == 2) {

/*        CASE 1: Swap 1-by-1 and 1-by-1 blocks. */

/*        Compute orthogonal QL and RQ that swap 1-by-1 and 1-by-1 blocks */
/*        using Givens rotations and perform the swap tentatively. */

        f = s[5] * t[0] - t[5] * s[0];
        g = s[5] * t[4] - t[5] * s[4];
        sb = abs(t[5]);
        sa = abs(s[5]);
        dlartg_(&f, &g, &ir[4], ir, &ddum);
        ir[1] = -ir[4];
        ir[5] = ir[0];
        drot_(&c__2, s, &c__1, &s[4], &c__1, ir, &ir[1]);
        drot_(&c__2, t, &c__1, &t[4], &c__1, ir, &ir[1]);
        if (sa >= sb) {
            dlartg_(s, &s[1], li, &li[1], &ddum);
        } else {
            dlartg_(t, &t[1], li, &li[1], &ddum);
        }
        drot_(&c__2, s, &c__4, &s[1], &c__4, li, &li[1]);
        drot_(&c__2, t, &c__4, &t[1], &c__4, li, &li[1]);
        li[5] = li[0];
        li[4] = -li[1];

/*        Weak stability test: */
/*           |S21| + |T21| <= O(EPS * F-norm((S, T))) */

        ws = abs(s[1]) + abs(t[1]);
        weak = ws <= thresh;
        if (! weak) {
            goto L70;
        }

        if (TRUE_) {

/*           Strong stability test: */
/*             F-norm((A-QL'*S*QR, B-QL'*T*QR)) <= O(EPS*F-norm((A,B))) */

            dlacpy_("Full", &m, &m, &a[*j1 + *j1 * *lda], lda, &work[m*m], &m);
            dgemm_("N", "N", &m, &m, &m, &c_b38, li, &c__4, s, &c__4, &c_b3, work, &m);
            dgemm_("N", "T", &m, &m, &m, &c_b44, work, &m, ir, &c__4, &c_b38, &work[m*m], &m);
            dscale = 0.;
            dsum = 1.;
            i__1 = m * m;
            dlassq_(&i__1, &work[m*m], &c__1, &dscale, &dsum);

            dlacpy_("Full", &m, &m, &b[*j1 + *j1 * *ldb], ldb, &work[m*m], &m);
            dgemm_("N", "N", &m, &m, &m, &c_b38, li, &c__4, t, &c__4, &c_b3, work, &m);
            dgemm_("N", "T", &m, &m, &m, &c_b44, work, &m, ir, &c__4, &c_b38, &work[m*m], &m);
            i__1 = m * m;
            dlassq_(&i__1, &work[m*m], &c__1, &dscale, &dsum);
            ss = dscale * sqrt(dsum);
            dtrong = ss <= thresh;
            if (! dtrong) {
                goto L70;
            }
        }

/*        Update (A(J1:J1+M-1, M+J1:N), B(J1:J1+M-1, M+J1:N)) and */
/*               (A(1:J1-1, J1:J1+M), B(1:J1-1, J1:J1+M)). */

        i__1 = *j1 + 2;
        drot_(&i__1, &a[*j1 * *lda], &c__1, &a[(*j1+1) * *lda], &c__1, ir, &ir[1]);
        drot_(&i__1, &b[*j1 * *ldb], &c__1, &b[(*j1+1) * *ldb], &c__1, ir, &ir[1]);
        i__1 = *n - *j1;
        drot_(&i__1, &a[*j1 + *j1 * *lda], lda, &a[(*j1+1) + *j1 * *lda], lda, li, &li[1]);
        drot_(&i__1, &b[*j1 + *j1 * *ldb], ldb, &b[(*j1+1) + *j1 * *ldb], ldb, li, &li[1]);

/*        Set  N1-by-N2 (2,1) - blocks to ZERO. */

        a[*j1+1 + *j1 * *lda] = 0.;
        b[*j1+1 + *j1 * *ldb] = 0.;

/*        Accumulate transformations into Q and Z if requested. */

        if (*wantz) {
            drot_(n, &z[*j1 * *ldz], &c__1, &z[(*j1+1) * *ldz], &c__1, ir, &ir[1]);
        }
        if (*wantq) {
            drot_(n, &q[*j1 * *ldq], &c__1, &q[(*j1+1) * *ldq], &c__1, li, &li[1]);
        }
    } else {

/*        CASE 2: Swap 1-by-1 and 2-by-2 blocks, or 2-by-2 */
/*                and 2-by-2 blocks. */

/*        Solve the generalized Sylvester equation */
/*                 S11 * R - L * S22 = SCALE * S12 */
/*                 T11 * R - L * T22 = SCALE * T12 */
/*        for R and L. Solutions in LI and IR. */

        dlacpy_("Full", n1, n2, &t[4 * *n1], &c__4, li, &c__4);
        dlacpy_("Full", n1, n2, &s[4 * *n1], &c__4, &ir[*n2 + 4 * *n1], &c__4);
        dtgsy2_("N", &c__0, n1, n2, s, &c__4, &s[5 * *n1],
                &c__4, &ir[*n2 + 4 * *n1], &c__4, t, &c__4,
                &t[5 * *n1], &c__4, li, &c__4, &scale,
                &dsum, &dscale, iwork, &idum, &linfo);

/*        Compute orthogonal matrix QL: */

/*                    QL' * LI = [ TL ] */
/*                               [ 0  ] */
/*        where */
/*                    LI =  [      -L              ] */
/*                          [ SCALE * identity(N2) ] */

        for (i = 0; i < *n2; ++i) {
            dscal_(n1, &c_b44, &li[4*i], &c__1);
            li[*n1 + 5*i] = scale;
        }
        dgeqr2_(&m, n2, li, &c__4, taul, work, &linfo);
        if (linfo != 0) {
            goto L70;
        }
        dorg2r_(&m, &m, n2, li, &c__4, taul, work, &linfo);
        if (linfo != 0) {
            goto L70;
        }

/*        Compute orthogonal matrix RQ: */

/*                    IR * RQ' =   [ 0  TR], */

/*         where IR = [ SCALE * identity(N1), R ] */

        for (i = 0; i < *n1; ++i) {
            ir[*n2 + 5*i] = scale;
        }
        dgerq2_(n1, &m, &ir[*n2], &c__4, taur, work, &linfo);
        if (linfo != 0) {
            goto L70;
        }
        dorgr2_(&m, &m, n1, ir, &c__4, taur, work, &linfo);
        if (linfo != 0) {
            goto L70;
        }

/*        Perform the swapping tentatively: */

        dgemm_("T", "N", &m, &m, &m, &c_b38, li, &c__4, s, &c__4, &c_b3, work, &m);
        dgemm_("N", "T", &m, &m, &m, &c_b38, work, &m, ir, &c__4, &c_b3, s, &c__4);
        dgemm_("T", "N", &m, &m, &m, &c_b38, li, &c__4, t, &c__4, &c_b3, work, &m);
        dgemm_("N", "T", &m, &m, &m, &c_b38, work, &m, ir, &c__4, &c_b3, t, &c__4);
        dlacpy_("F", &m, &m, s, &c__4, scpy, &c__4);
        dlacpy_("F", &m, &m, t, &c__4, tcpy, &c__4);
        dlacpy_("F", &m, &m, ir, &c__4, ircop, &c__4);
        dlacpy_("F", &m, &m, li, &c__4, licop, &c__4);

/*        Triangularize the B-part by an RQ factorization. */
/*        Apply transformation (from left) to A-part, giving S. */

        dgerq2_(&m, &m, t, &c__4, taur, work, &linfo);
        if (linfo != 0) {
            goto L70;
        }
        dormr2_("R", "T", &m, &m, &m, t, &c__4, taur, s, &c__4, work, &linfo);
        if (linfo != 0) {
            goto L70;
        }
        dormr2_("L", "N", &m, &m, &m, t, &c__4, taur, ir, &c__4, work, &linfo);
        if (linfo != 0) {
            goto L70;
        }

/*        Compute F-norm(S21) in BRQA21. (T21 is 0.) */

        dscale = 0.;
        dsum = 1.;
        for (i = 0; i < *n2; ++i) {
            dlassq_(n1, &s[*n2 + 4*i], &c__1, &dscale, &dsum);
        }
        brqa21 = dscale * sqrt(dsum);

/*        Triangularize the B-part by a QR factorization. */
/*        Apply transformation (from right) to A-part, giving S. */

        dgeqr2_(&m, &m, tcpy, &c__4, taul, work, &linfo);
        if (linfo != 0) {
            goto L70;
        }
        dorm2r_("L", "T", &m, &m, &m, tcpy, &c__4, taul, scpy, &c__4, work, info);
        dorm2r_("R", "N", &m, &m, &m, tcpy, &c__4, taul, licop, &c__4, work, info);
        if (linfo != 0) {
            goto L70;
        }

/*        Compute F-norm(S21) in BQRA21. (T21 is 0.) */

        dscale = 0.;
        dsum = 1.;
        for (i = 0; i < *n2; ++i) {
            dlassq_(n1, &scpy[*n2 + 4*i], &c__1, &dscale, &dsum);
        }
        bqra21 = dscale * sqrt(dsum);

/*        Decide which method to use. */
/*          Weak stability test: */
/*             F-norm(S21) <= O(EPS * F-norm((S, T))) */

        if (bqra21 <= brqa21 && bqra21 <= thresh) {
            dlacpy_("F", &m, &m, scpy, &c__4, s, &c__4);
            dlacpy_("F", &m, &m, tcpy, &c__4, t, &c__4);
            dlacpy_("F", &m, &m, ircop, &c__4, ir, &c__4);
            dlacpy_("F", &m, &m, licop, &c__4, li, &c__4);
        } else if (brqa21 >= thresh) {
            goto L70;
        }

/*        Set lower triangle of B-part to zero */

        for (i = 1; i < m; ++i) {
            i__1 = m - i;
            dcopy_(&i__1, &c_b3, &c__0, &t[5*i-4], &c__1);
        }

        if (TRUE_) {

/*           Strong stability test: */
/*              F-norm((A-QL*S*QR', B-QL*T*QR')) <= O(EPS*F-norm((A,B))) */

            dlacpy_("Full", &m, &m, &a[*j1 + *j1 * *lda], lda, &work[m*m], &m);
            dgemm_("N", "N", &m, &m, &m, &c_b38, li, &c__4, s, &c__4, &c_b3, work, &m);
            dgemm_("N", "N", &m, &m, &m, &c_b44, work, &m, ir, &c__4, &c_b38, &work[m*m], &m);
            dscale = 0.;
            dsum = 1.;
            i__1 = m * m;
            dlassq_(&i__1, &work[m*m], &c__1, &dscale, &dsum);

            dlacpy_("Full", &m, &m, &b[*j1 + *j1 * *ldb], ldb, &work[m*m], &m);
            dgemm_("N", "N", &m, &m, &m, &c_b38, li, &c__4, t, &c__4, &c_b3, work, &m);
            dgemm_("N", "N", &m, &m, &m, &c_b44, work, &m, ir, &c__4, &c_b38, &work[m*m], &m);
            i__1 = m * m;
            dlassq_(&i__1, &work[m*m], &c__1, &dscale, &dsum);
            ss = dscale * sqrt(dsum);
            dtrong = ss <= thresh;
            if (! dtrong) {
                goto L70;
            }
        }

/*        If the swap is accepted ("weakly" and "strongly"), apply the */
/*        transformations and set N1-by-N2 (2,1)-block to zero. */

        for (i = 0; i < *n2; ++i) {
            dcopy_(n1, &c_b3, &c__0, &s[*n2 + 4*i], &c__1);
        }

/*        copy back M-by-M diagonal block starting at index J1 of (A, B) */

        dlacpy_("F", &m, &m, s, &c__4, &a[*j1 + *j1 * *lda], lda);
        dlacpy_("F", &m, &m, t, &c__4, &b[*j1 + *j1 * *ldb], ldb);
        dcopy_(&c__16, &c_b3, &c__0, t, &c__1);

/*        Standardize existing 2-by-2 blocks. */

        i__1 = m * m;
        dcopy_(&i__1, &c_b3, &c__0, work, &c__1);
        work[0] = 1.;
        t[0] = 1.;
        idum = *lwork - m * m - 2;
        if (*n2 > 1) {
            dlagv2_(&a[*j1 + *j1 * *lda], lda, &b[*j1 + *j1 * *ldb], ldb,
                    ar, ai, be, work, &work[1], t, &t[1]);
            work[m]  = -work[1];
            work[m+1] = work[0];
            t[5 * *n2 - 5] = t[0];
            t[4] = -t[1];
        }
        work[m*m-1] = 1.;
        t[5*m-5] = 1.;

        if (*n1 > 1) {
            dlagv2_(&a[(*j1 + *n2) * (*lda+1)], lda, &b[(*j1 + *n2) * (*ldb+1)], ldb,
                    taur, taul, &work[m*m], &work[*n2 * (m+1)], &work[*n2 * (m+1) + 1], &t[5 * *n2], &t[5*m-9]);
            work[m * m - 1] =  work[*n2 * m + *n2];
            work[m * m - 2] = -work[*n2 * m + *n2 + 1];
            t[5*m - 5] =  t[5 * *n2];
            t[5*m - 6] = -t[5*m - 9];
        }
        dgemm_("T", "N", n2, n1, n2, &c_b38, work, &m, &a[*j1 + (*j1 + * n2) * *lda], lda, &c_b3, &work[m*m], n2);
        dlacpy_("Full", n2, n1, &work[m*m], n2, &a[*j1 + (*j1 + *n2) * *lda], lda);
        dgemm_("T", "N", n2, n1, n2, &c_b38, work, &m, &b[*j1 + (*j1 + * n2) * *ldb], ldb, &c_b3, &work[m*m], n2);
        dlacpy_("Full", n2, n1, &work[m*m], n2, &b[*j1 + (*j1 + *n2) * *ldb], ldb);
        dgemm_("N", "N", &m, &m, &m, &c_b38, li, &c__4, work, &m, &c_b3, &work[m*m], &m);
        dlacpy_("Full", &m, &m, &work[m*m], &m, li, &c__4);
        dgemm_("N", "N", n2, n1, n1, &c_b38, &a[*j1 + (*j1 + *n2) * *lda], lda, &t[5 * *n2], &c__4, &c_b3, work, n2);
        dlacpy_("Full", n2, n1, work, n2, &a[*j1 + (*j1 + *n2) * *lda], lda);
        dgemm_("N", "N", n2, n1, n1, &c_b38, &b[*j1 + (*j1 + *n2) * *ldb], lda, &t[5 * *n2], &c__4, &c_b3, work, n2);
        dlacpy_("Full", n2, n1, work, n2, &b[*j1 + (*j1 + *n2) * *ldb], ldb);
        dgemm_("T", "N", &m, &m, &m, &c_b38, ir, &c__4, t, &c__4, &c_b3, work, &m);
        dlacpy_("Full", &m, &m, work, &m, ir, &c__4);

/*        Accumulate transformations into Q and Z if requested. */

        if (*wantq) {
            dgemm_("N", "N", n, &m, &m, &c_b38, &q[*j1 * *ldq], ldq, li, &c__4, &c_b3, work, n);
            dlacpy_("Full", n, &m, work, n, &q[*j1 * *ldq], ldq);
        }

        if (*wantz) {
            dgemm_("N", "N", n, &m, &m, &c_b38, &z[*j1 * *ldz], ldz, ir, &c__4, &c_b3, work, n);
            dlacpy_("Full", n, &m, work, n, &z[*j1 * *ldz], ldz);
        }

/*        Update (A(J1:J1+M-1, M+J1:N), B(J1:J1+M-1, M+J1:N)) and */
/*                (A(1:J1-1, J1:J1+M), B(1:J1-1, J1:J1+M)). */

        i = *j1 + m;
        if (i < *n) {
            i__1 = *n - i;
            dgemm_("T", "N", &m, &i__1, &m, &c_b38, li, &c__4, &a[*j1 + i * *lda], lda, &c_b3, work, &m);
            dlacpy_("Full", &m, &i__1, work, &m, &a[*j1 + i * *lda], lda);
            dgemm_("T", "N", &m, &i__1, &m, &c_b38, li, &c__4, &b[*j1 + i * *ldb], lda, &c_b3, work, &m);
            dlacpy_("Full", &m, &i__1, work, &m, &b[*j1 + i * *ldb], lda);
        }
        if (*j1 > 0) {
            dgemm_("N", "N", j1, &m, &m, &c_b38, &a[*j1 * *lda], lda, ir, &c__4, &c_b3, work, j1);
            dlacpy_("Full", j1, &m, work, j1, &a[*j1 * *lda], lda);
            dgemm_("N", "N", j1, &m, &m, &c_b38, &b[*j1 * *ldb], ldb, ir, &c__4, &c_b3, work, j1);
            dlacpy_("Full", j1, &m, work, j1, &b[*j1 * *ldb], ldb);
        }
    }

/*     Exit with INFO = 0 if swap was successfully performed. */

    *info = 0;
    /* Parameter adjustments */
    *j1 += 1;
    return;

/*     Exit with INFO = 1 if swap was rejected. */

L70:
    *info = 1;
    /* Parameter adjustments */
    *j1 += 1;

} /* dtgex2_ */
