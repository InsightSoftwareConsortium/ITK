/* lapack/double/dtgex2.f -- translated by f2c (version 20050501).
   You must link the resulting object file with libf2c:
        on Microsoft Windows system, link with libf2c.lib;
        on Linux or Unix systems, link with .../path/to/libf2c.a -lm
        or, if you install libf2c.a in a standard place, with -lf2c -lm
        -- in that order, at the end of the command line, as in
                cc *.o -lf2c -lm
        Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

                http://www.netlib.org/f2c/libf2c.zip
*/

#ifdef __cplusplus
extern "C" {
#endif
#include "v3p_netlib.h"

/* Table of constant values */

static integer c__16 = 16;
static doublereal c_b3 = 0.;
static integer c__0 = 0;
static integer c__1 = 1;
static integer c__4 = 4;
static integer c__2 = 2;
static doublereal c_b38 = 1.;
static doublereal c_b44 = -1.;

/*<    >*/
/* Subroutine */ int dtgex2_(logical *wantq, logical *wantz, integer *n,
        doublereal *a, integer *lda, doublereal *b, integer *ldb, doublereal *
        q, integer *ldq, doublereal *z__, integer *ldz, integer *j1, integer *
        n1, integer *n2, doublereal *work, integer *lwork, integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, q_dim1, q_offset, z_dim1,
            z_offset, i__1, i__2;
    doublereal d__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    doublereal f, g;
    integer i__, m;
    doublereal s[16]    /* was [4][4] */, t[16] /* was [4][4] */, be[2], ai[2]
            , ar[2], sa, sb, li[16]     /* was [4][4] */, ir[16]        /*
            was [4][4] */, ss, ws, eps;
    logical weak;
    doublereal ddum;
    integer idum;
    doublereal taul[4], dsum;
    extern /* Subroutine */ int drot_(integer *, doublereal *, integer *,
            doublereal *, integer *, doublereal *, doublereal *);
    doublereal taur[4], scpy[16]        /* was [4][4] */, tcpy[16]      /*
            was [4][4] */;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *,
            integer *);
    doublereal scale, bqra21, brqa21;
    extern /* Subroutine */ int dgemm_(char *, char *, integer *, integer *,
            integer *, doublereal *, doublereal *, integer *, doublereal *,
            integer *, doublereal *, doublereal *, integer *, ftnlen, ftnlen);
    doublereal licop[16]        /* was [4][4] */;
    integer linfo;
    doublereal ircop[16]        /* was [4][4] */;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *,
            doublereal *, integer *);
    doublereal dnorm;
    integer iwork[4];
    extern /* Subroutine */ int dlagv2_(doublereal *, integer *, doublereal *,
             integer *, doublereal *, doublereal *, doublereal *, doublereal *
            , doublereal *, doublereal *, doublereal *), dgeqr2_(integer *,
            integer *, doublereal *, integer *, doublereal *, doublereal *,
            integer *), dgerq2_(integer *, integer *, doublereal *, integer *,
             doublereal *, doublereal *, integer *), dorg2r_(integer *,
            integer *, integer *, doublereal *, integer *, doublereal *,
            doublereal *, integer *), dorgr2_(integer *, integer *, integer *,
             doublereal *, integer *, doublereal *, doublereal *, integer *),
            dorm2r_(char *, char *, integer *, integer *, integer *,
            doublereal *, integer *, doublereal *, doublereal *, integer *,
            doublereal *, integer *, ftnlen, ftnlen), dormr2_(char *, char *,
            integer *, integer *, integer *, doublereal *, integer *,
            doublereal *, doublereal *, integer *, doublereal *, integer *,
            ftnlen, ftnlen), dtgsy2_(char *, integer *, integer *, integer *,
            doublereal *, integer *, doublereal *, integer *, doublereal *,
            integer *, doublereal *, integer *, doublereal *, integer *,
            doublereal *, integer *, doublereal *, doublereal *, doublereal *,
             integer *, integer *, integer *, ftnlen);
    extern doublereal dlamch_(char *, ftnlen);
    doublereal dscale;
    extern /* Subroutine */ int dlacpy_(char *, integer *, integer *,
            doublereal *, integer *, doublereal *, integer *, ftnlen),
            dlartg_(doublereal *, doublereal *, doublereal *, doublereal *,
            doublereal *), dlassq_(integer *, doublereal *, integer *,
            doublereal *, doublereal *);
    logical dtrong;
    doublereal thresh, smlnum;


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     June 30, 1999 */

/*     .. Scalar Arguments .. */
/*<       LOGICAL            WANTQ, WANTZ >*/
/*<       INTEGER            INFO, J1, LDA, LDB, LDQ, LDZ, LWORK, N, N1, N2 >*/
/*     .. */
/*     .. Array Arguments .. */
/*<    >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DTGEX2 swaps adjacent diagonal blocks (A11, B11) and (A22, B22) */
/*  of size 1-by-1 or 2-by-2 in an upper (quasi) triangular matrix pair */
/*  (A, B) by an orthogonal equivalence transformation. */

/*  (A, B) must be in generalized real Schur canonical form (as returned */
/*  by DGGES), i.e. A is block upper triangular with 1-by-1 and 2-by-2 */
/*  diagonal blocks. B is upper triangular. */

/*  Optionally, the matrices Q and Z of generalized Schur vectors are */
/*  updated. */

/*         Q(in) * A(in) * Z(in)' = Q(out) * A(out) * Z(out)' */
/*         Q(in) * B(in) * Z(in)' = Q(out) * B(out) * Z(out)' */


/*  Arguments */
/*  ========= */

/*  WANTQ   (input) LOGICAL */
/*          .TRUE. : update the left transformation matrix Q; */
/*          .FALSE.: do not update Q. */

/*  WANTZ   (input) LOGICAL */
/*          .TRUE. : update the right transformation matrix Z; */
/*          .FALSE.: do not update Z. */

/*  N       (input) INTEGER */
/*          The order of the matrices A and B. N >= 0. */

/*  A      (input/output) DOUBLE PRECISION arrays, dimensions (LDA,N) */
/*          On entry, the matrix A in the pair (A, B). */
/*          On exit, the updated matrix A. */

/*  LDA     (input)  INTEGER */
/*          The leading dimension of the array A. LDA >= max(1,N). */

/*  B      (input/output) DOUBLE PRECISION arrays, dimensions (LDB,N) */
/*          On entry, the matrix B in the pair (A, B). */
/*          On exit, the updated matrix B. */

/*  LDB     (input)  INTEGER */
/*          The leading dimension of the array B. LDB >= max(1,N). */

/*  Q       (input/output) DOUBLE PRECISION array, dimension (LDZ,N) */
/*          On entry, if WANTQ = .TRUE., the orthogonal matrix Q. */
/*          On exit, the updated matrix Q. */
/*          Not referenced if WANTQ = .FALSE.. */

/*  LDQ     (input) INTEGER */
/*          The leading dimension of the array Q. LDQ >= 1. */
/*          If WANTQ = .TRUE., LDQ >= N. */

/*  Z       (input/output) DOUBLE PRECISION array, dimension (LDZ,N) */
/*          On entry, if WANTZ =.TRUE., the orthogonal matrix Z. */
/*          On exit, the updated matrix Z. */
/*          Not referenced if WANTZ = .FALSE.. */

/*  LDZ     (input) INTEGER */
/*          The leading dimension of the array Z. LDZ >= 1. */
/*          If WANTZ = .TRUE., LDZ >= N. */

/*  J1      (input) INTEGER */
/*          The index to the first block (A11, B11). 1 <= J1 <= N. */

/*  N1      (input) INTEGER */
/*          The order of the first block (A11, B11). N1 = 0, 1 or 2. */

/*  N2      (input) INTEGER */
/*          The order of the second block (A22, B22). N2 = 0, 1 or 2. */

/*  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK). */

/*  LWORK   (input) INTEGER */
/*          The dimension of the array WORK. */
/*          LWORK >=  MAX( N*(N2+N1), (N2+N1)*(N2+N1)*2 ) */

/*  INFO    (output) INTEGER */
/*            =0: Successful exit */
/*            >0: If INFO = 1, the transformed matrix (A, B) would be */
/*                too far from generalized Schur form; the blocks are */
/*                not swapped and (A, B) and (Q, Z) are unchanged. */
/*                The problem of swapping is too ill-conditioned. */
/*            <0: If INFO = -16: LWORK is too small. Appropriate value */
/*                for LWORK is returned in WORK(1). */

/*  Further Details */
/*  =============== */

/*  Based on contributions by */
/*     Bo Kagstrom and Peter Poromaa, Department of Computing Science, */
/*     Umea University, S-901 87 Umea, Sweden. */

/*  In the current code both weak and strong stability tests are */
/*  performed. The user can omit the strong stability test by changing */
/*  the internal logical parameter WANDS to .FALSE.. See ref. [2] for */
/*  details. */

/*  [1] B. Kagstrom; A Direct Method for Reordering Eigenvalues in the */
/*      Generalized Real Schur Form of a Regular Matrix Pair (A, B), in */
/*      M.S. Moonen et al (eds), Linear Algebra for Large Scale and */
/*      Real-Time Applications, Kluwer Academic Publ. 1993, pp 195-218. */

/*  [2] B. Kagstrom and P. Poromaa; Computing Eigenspaces with Specified */
/*      Eigenvalues of a Regular Matrix Pair (A, B) and Condition */
/*      Estimation: Theory, Algorithms and Software, */
/*      Report UMINF - 94.04, Department of Computing Science, Umea */
/*      University, S-901 87 Umea, Sweden, 1994. Also as LAPACK Working */
/*      Note 87. To appear in Numerical Algorithms, 1996. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO, ONE >*/
/*<       PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 ) >*/
/*<       DOUBLE PRECISION   TEN >*/
/*<       PARAMETER          ( TEN = 1.0D+01 ) >*/
/*<       INTEGER            LDST >*/
/*<       PARAMETER          ( LDST = 4 ) >*/
/*<       LOGICAL            WANDS >*/
/*<       PARAMETER          ( WANDS = .TRUE. ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            DTRONG, WEAK >*/
/*<       INTEGER            I, IDUM, LINFO, M >*/
/*<    >*/
/*     .. */
/*     .. Local Arrays .. */
/*<       INTEGER            IWORK( LDST ) >*/
/*<    >*/
/*     .. */
/*     .. External Functions .. */
/*<       DOUBLE PRECISION   DLAMCH >*/
/*<       EXTERNAL           DLAMCH >*/
/*     .. */
/*     .. External Subroutines .. */
/*<    >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, MAX, SQRT >*/
/*     .. */
/*     .. Executable Statements .. */

/*<       INFO = 0 >*/
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1;
    b -= b_offset;
    q_dim1 = *ldq;
    q_offset = 1 + q_dim1;
    q -= q_offset;
    z_dim1 = *ldz;
    z_offset = 1 + z_dim1;
    z__ -= z_offset;
    --work;

    /* Function Body */
    *info = 0;

/*     Quick return if possible */

/*<    >*/
    if (*n <= 1 || *n1 <= 0 || *n2 <= 0) {
        return 0;
    }
/*<    >*/
    if (*n1 > *n || *j1 + *n1 > *n) {
        return 0;
    }
/*<       M = N1 + N2 >*/
    m = *n1 + *n2;
/*<       IF( LWORK.LT.MAX( N*M, M*M*2 ) ) THEN >*/
/* Computing MAX */
    i__1 = *n * m, i__2 = m * m << 1;
    if (*lwork < max(i__1,i__2)) {
/*<          INFO = -16 >*/
        *info = -16;
/*<          WORK( 1 ) = MAX( N*M, M*M*2 ) >*/
/* Computing MAX */
        i__1 = *n * m, i__2 = m * m << 1;
        work[1] = (doublereal) max(i__1,i__2);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*<       WEAK = .FALSE. >*/
//    weak = FALSE_;
/*<       DTRONG = .FALSE. >*/
//    dtrong = FALSE_;

/*     Make a local copy of selected block */

/*<       CALL DCOPY( LDST*LDST, ZERO, 0, LI, 1 ) >*/
    dcopy_(&c__16, &c_b3, &c__0, li, &c__1);
/*<       CALL DCOPY( LDST*LDST, ZERO, 0, IR, 1 ) >*/
    dcopy_(&c__16, &c_b3, &c__0, ir, &c__1);
/*<       CALL DLACPY( 'Full', M, M, A( J1, J1 ), LDA, S, LDST ) >*/
    dlacpy_("Full", &m, &m, &a[*j1 + *j1 * a_dim1], lda, s, &c__4, (ftnlen)4);
/*<       CALL DLACPY( 'Full', M, M, B( J1, J1 ), LDB, T, LDST ) >*/
    dlacpy_("Full", &m, &m, &b[*j1 + *j1 * b_dim1], ldb, t, &c__4, (ftnlen)4);

/*     Compute threshold for testing acceptance of swapping. */

/*<       EPS = DLAMCH( 'P' ) >*/
    eps = dlamch_("P", (ftnlen)1);
/*<       SMLNUM = DLAMCH( 'S' ) / EPS >*/
    smlnum = dlamch_("S", (ftnlen)1) / eps;
/*<       DSCALE = ZERO >*/
    dscale = 0.;
/*<       DSUM = ONE >*/
    dsum = 1.;
/*<       CALL DLACPY( 'Full', M, M, S, LDST, WORK, M ) >*/
    dlacpy_("Full", &m, &m, s, &c__4, &work[1], &m, (ftnlen)4);
/*<       CALL DLASSQ( M*M, WORK, 1, DSCALE, DSUM ) >*/
    i__1 = m * m;
    dlassq_(&i__1, &work[1], &c__1, &dscale, &dsum);
/*<       CALL DLACPY( 'Full', M, M, T, LDST, WORK, M ) >*/
    dlacpy_("Full", &m, &m, t, &c__4, &work[1], &m, (ftnlen)4);
/*<       CALL DLASSQ( M*M, WORK, 1, DSCALE, DSUM ) >*/
    i__1 = m * m;
    dlassq_(&i__1, &work[1], &c__1, &dscale, &dsum);
/*<       DNORM = DSCALE*SQRT( DSUM ) >*/
    dnorm = dscale * sqrt(dsum);
/*<       THRESH = MAX( TEN*EPS*DNORM, SMLNUM ) >*/
/* Computing MAX */
    d__1 = eps * 10. * dnorm;
    thresh = max(d__1,smlnum);

/*<       IF( M.EQ.2 ) THEN >*/
    if (m == 2) {

/*        CASE 1: Swap 1-by-1 and 1-by-1 blocks. */

/*        Compute orthogonal QL and RQ that swap 1-by-1 and 1-by-1 blocks */
/*        using Givens rotations and perform the swap tentatively. */

/*<          F = S( 2, 2 )*T( 1, 1 ) - T( 2, 2 )*S( 1, 1 ) >*/
        f = s[5] * t[0] - t[5] * s[0];
/*<          G = S( 2, 2 )*T( 1, 2 ) - T( 2, 2 )*S( 1, 2 ) >*/
        g = s[5] * t[4] - t[5] * s[4];
/*<          SB = ABS( T( 2, 2 ) ) >*/
        sb = abs(t[5]);
/*<          SA = ABS( S( 2, 2 ) ) >*/
        sa = abs(s[5]);
/*<          CALL DLARTG( F, G, IR( 1, 2 ), IR( 1, 1 ), DDUM ) >*/
        dlartg_(&f, &g, &ir[4], ir, &ddum);
/*<          IR( 2, 1 ) = -IR( 1, 2 ) >*/
        ir[1] = -ir[4];
/*<          IR( 2, 2 ) = IR( 1, 1 ) >*/
        ir[5] = ir[0];
/*<    >*/
        drot_(&c__2, s, &c__1, &s[4], &c__1, ir, &ir[1]);
/*<    >*/
        drot_(&c__2, t, &c__1, &t[4], &c__1, ir, &ir[1]);
/*<          IF( SA.GE.SB ) THEN >*/
        if (sa >= sb) {
/*<    >*/
            dlartg_(s, &s[1], li, &li[1], &ddum);
/*<          ELSE >*/
        } else {
/*<    >*/
            dlartg_(t, &t[1], li, &li[1], &ddum);
/*<          END IF >*/
        }
/*<    >*/
        drot_(&c__2, s, &c__4, &s[1], &c__4, li, &li[1]);
/*<    >*/
        drot_(&c__2, t, &c__4, &t[1], &c__4, li, &li[1]);
/*<          LI( 2, 2 ) = LI( 1, 1 ) >*/
        li[5] = li[0];
/*<          LI( 1, 2 ) = -LI( 2, 1 ) >*/
        li[4] = -li[1];

/*        Weak stability test: */
/*           |S21| + |T21| <= O(EPS * F-norm((S, T))) */

/*<          WS = ABS( S( 2, 1 ) ) + ABS( T( 2, 1 ) ) >*/
        ws = abs(s[1]) + abs(t[1]);
/*<          WEAK = WS.LE.THRESH >*/
        weak = ws <= thresh;
/*<    >*/
        if (! weak) {
            goto L70;
        }

/*<          IF( WANDS ) THEN >*/
        if (TRUE_) {

/*           Strong stability test: */
/*             F-norm((A-QL'*S*QR, B-QL'*T*QR)) <= O(EPS*F-norm((A,B))) */

/*<    >*/
            dlacpy_("Full", &m, &m, &a[*j1 + *j1 * a_dim1], lda, &work[m * m
                    + 1], &m, (ftnlen)4);
/*<    >*/
            dgemm_("N", "N", &m, &m, &m, &c_b38, li, &c__4, s, &c__4, &c_b3, &
                    work[1], &m, (ftnlen)1, (ftnlen)1);
/*<    >*/
            dgemm_("N", "T", &m, &m, &m, &c_b44, &work[1], &m, ir, &c__4, &
                    c_b38, &work[m * m + 1], &m, (ftnlen)1, (ftnlen)1);
/*<             DSCALE = ZERO >*/
            dscale = 0.;
/*<             DSUM = ONE >*/
            dsum = 1.;
/*<             CALL DLASSQ( M*M, WORK( M*M+1 ), 1, DSCALE, DSUM ) >*/
            i__1 = m * m;
            dlassq_(&i__1, &work[m * m + 1], &c__1, &dscale, &dsum);

/*<    >*/
            dlacpy_("Full", &m, &m, &b[*j1 + *j1 * b_dim1], ldb, &work[m * m
                    + 1], &m, (ftnlen)4);
/*<    >*/
            dgemm_("N", "N", &m, &m, &m, &c_b38, li, &c__4, t, &c__4, &c_b3, &
                    work[1], &m, (ftnlen)1, (ftnlen)1);
/*<    >*/
            dgemm_("N", "T", &m, &m, &m, &c_b44, &work[1], &m, ir, &c__4, &
                    c_b38, &work[m * m + 1], &m, (ftnlen)1, (ftnlen)1);
/*<             CALL DLASSQ( M*M, WORK( M*M+1 ), 1, DSCALE, DSUM ) >*/
            i__1 = m * m;
            dlassq_(&i__1, &work[m * m + 1], &c__1, &dscale, &dsum);
/*<             SS = DSCALE*SQRT( DSUM ) >*/
            ss = dscale * sqrt(dsum);
/*<             DTRONG = SS.LE.THRESH >*/
            dtrong = ss <= thresh;
/*<    >*/
            if (! dtrong) {
                goto L70;
            }
/*<          END IF >*/
        }

/*        Update (A(J1:J1+M-1, M+J1:N), B(J1:J1+M-1, M+J1:N)) and */
/*               (A(1:J1-1, J1:J1+M), B(1:J1-1, J1:J1+M)). */

/*<    >*/
        i__1 = *j1 + 1;
        drot_(&i__1, &a[*j1 * a_dim1 + 1], &c__1, &a[(*j1 + 1) * a_dim1 + 1],
                &c__1, ir, &ir[1]);
/*<    >*/
        i__1 = *j1 + 1;
        drot_(&i__1, &b[*j1 * b_dim1 + 1], &c__1, &b[(*j1 + 1) * b_dim1 + 1],
                &c__1, ir, &ir[1]);
/*<    >*/
        i__1 = *n - *j1 + 1;
        drot_(&i__1, &a[*j1 + *j1 * a_dim1], lda, &a[*j1 + 1 + *j1 * a_dim1],
                lda, li, &li[1]);
/*<    >*/
        i__1 = *n - *j1 + 1;
        drot_(&i__1, &b[*j1 + *j1 * b_dim1], ldb, &b[*j1 + 1 + *j1 * b_dim1],
                ldb, li, &li[1]);

/*        Set  N1-by-N2 (2,1) - blocks to ZERO. */

/*<          A( J1+1, J1 ) = ZERO >*/
        a[*j1 + 1 + *j1 * a_dim1] = 0.;
/*<          B( J1+1, J1 ) = ZERO >*/
        b[*j1 + 1 + *j1 * b_dim1] = 0.;

/*        Accumulate transformations into Q and Z if requested. */

/*<    >*/
        if (*wantz) {
            drot_(n, &z__[*j1 * z_dim1 + 1], &c__1, &z__[(*j1 + 1) * z_dim1 +
                    1], &c__1, ir, &ir[1]);
        }
/*<    >*/
        if (*wantq) {
            drot_(n, &q[*j1 * q_dim1 + 1], &c__1, &q[(*j1 + 1) * q_dim1 + 1],
                    &c__1, li, &li[1]);
        }

/*        Exit with INFO = 0 if swap was successfully performed. */

/*<          RETURN >*/
        return 0;

/*<       ELSE >*/
    } else {

/*        CASE 2: Swap 1-by-1 and 2-by-2 blocks, or 2-by-2 */
/*                and 2-by-2 blocks. */

/*        Solve the generalized Sylvester equation */
/*                 S11 * R - L * S22 = SCALE * S12 */
/*                 T11 * R - L * T22 = SCALE * T12 */
/*        for R and L. Solutions in LI and IR. */

/*<          CALL DLACPY( 'Full', N1, N2, T( 1, N1+1 ), LDST, LI, LDST ) >*/
        dlacpy_("Full", n1, n2, &t[((*n1 + 1) << 2) - 4], &c__4, li, &c__4, (
                ftnlen)4);
/*<    >*/
        dlacpy_("Full", n1, n2, &s[((*n1 + 1) << 2) - 4], &c__4, &ir[*n2 + 1 + (
                (*n1 + 1) << 2) - 5], &c__4, (ftnlen)4);
/*<    >*/
        dtgsy2_("N", &c__0, n1, n2, s, &c__4, &s[(*n1 + 1) + ((*n1 + 1) << 2) - 5]
                , &c__4, &ir[*n2 + 1 + ((*n1 + 1) << 2) - 5], &c__4, t, &c__4, &
                t[(*n1 + 1) + ((*n1 + 1) << 2) - 5], &c__4, li, &c__4, &scale, &
                dsum, &dscale, iwork, &idum, &linfo, (ftnlen)1);

/*        Compute orthogonal matrix QL: */

/*                    QL' * LI = [ TL ] */
/*                               [ 0  ] */
/*        where */
/*                    LI =  [      -L              ] */
/*                          [ SCALE * identity(N2) ] */

/*<          DO 10 I = 1, N2 >*/
        i__1 = *n2;
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             CALL DSCAL( N1, -ONE, LI( 1, I ), 1 ) >*/
            dscal_(n1, &c_b44, &li[(i__ << 2) - 4], &c__1);
/*<             LI( N1+I, I ) = SCALE >*/
            li[*n1 + i__ + (i__ << 2) - 5] = scale;
/*<    10    CONTINUE >*/
/* L10: */
        }
/*<          CALL DGEQR2( M, N2, LI, LDST, TAUL, WORK, LINFO ) >*/
        dgeqr2_(&m, n2, li, &c__4, taul, &work[1], &linfo);
/*<    >*/
        if (linfo != 0) {
            goto L70;
        }
/*<          CALL DORG2R( M, M, N2, LI, LDST, TAUL, WORK, LINFO ) >*/
        dorg2r_(&m, &m, n2, li, &c__4, taul, &work[1], &linfo);
/*<    >*/
        if (linfo != 0) {
            goto L70;
        }

/*        Compute orthogonal matrix RQ: */

/*                    IR * RQ' =   [ 0  TR], */

/*         where IR = [ SCALE * identity(N1), R ] */

/*<          DO 20 I = 1, N1 >*/
        i__1 = *n1;
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             IR( N2+I, I ) = SCALE >*/
            ir[*n2 + i__ + (i__ << 2) - 5] = scale;
/*<    20    CONTINUE >*/
/* L20: */
        }
/*<          CALL DGERQ2( N1, M, IR( N2+1, 1 ), LDST, TAUR, WORK, LINFO ) >*/
        dgerq2_(n1, &m, &ir[*n2], &c__4, taur, &work[1], &linfo);
/*<    >*/
        if (linfo != 0) {
            goto L70;
        }
/*<          CALL DORGR2( M, M, N1, IR, LDST, TAUR, WORK, LINFO ) >*/
        dorgr2_(&m, &m, n1, ir, &c__4, taur, &work[1], &linfo);
/*<    >*/
        if (linfo != 0) {
            goto L70;
        }

/*        Perform the swapping tentatively: */

/*<    >*/
        dgemm_("T", "N", &m, &m, &m, &c_b38, li, &c__4, s, &c__4, &c_b3, &
                work[1], &m, (ftnlen)1, (ftnlen)1);
/*<    >*/
        dgemm_("N", "T", &m, &m, &m, &c_b38, &work[1], &m, ir, &c__4, &c_b3,
                s, &c__4, (ftnlen)1, (ftnlen)1);
/*<    >*/
        dgemm_("T", "N", &m, &m, &m, &c_b38, li, &c__4, t, &c__4, &c_b3, &
                work[1], &m, (ftnlen)1, (ftnlen)1);
/*<    >*/
        dgemm_("N", "T", &m, &m, &m, &c_b38, &work[1], &m, ir, &c__4, &c_b3,
                t, &c__4, (ftnlen)1, (ftnlen)1);
/*<          CALL DLACPY( 'F', M, M, S, LDST, SCPY, LDST ) >*/
        dlacpy_("F", &m, &m, s, &c__4, scpy, &c__4, (ftnlen)1);
/*<          CALL DLACPY( 'F', M, M, T, LDST, TCPY, LDST ) >*/
        dlacpy_("F", &m, &m, t, &c__4, tcpy, &c__4, (ftnlen)1);
/*<          CALL DLACPY( 'F', M, M, IR, LDST, IRCOP, LDST ) >*/
        dlacpy_("F", &m, &m, ir, &c__4, ircop, &c__4, (ftnlen)1);
/*<          CALL DLACPY( 'F', M, M, LI, LDST, LICOP, LDST ) >*/
        dlacpy_("F", &m, &m, li, &c__4, licop, &c__4, (ftnlen)1);

/*        Triangularize the B-part by an RQ factorization. */
/*        Apply transformation (from left) to A-part, giving S. */

/*<          CALL DGERQ2( M, M, T, LDST, TAUR, WORK, LINFO ) >*/
        dgerq2_(&m, &m, t, &c__4, taur, &work[1], &linfo);
/*<    >*/
        if (linfo != 0) {
            goto L70;
        }
/*<    >*/
        dormr2_("R", "T", &m, &m, &m, t, &c__4, taur, s, &c__4, &work[1], &
                linfo, (ftnlen)1, (ftnlen)1);
/*<    >*/
        if (linfo != 0) {
            goto L70;
        }
/*<    >*/
        dormr2_("L", "N", &m, &m, &m, t, &c__4, taur, ir, &c__4, &work[1], &
                linfo, (ftnlen)1, (ftnlen)1);
/*<    >*/
        if (linfo != 0) {
            goto L70;
        }

/*        Compute F-norm(S21) in BRQA21. (T21 is 0.) */

/*<          DSCALE = ZERO >*/
        dscale = 0.;
/*<          DSUM = ONE >*/
        dsum = 1.;
/*<          DO 30 I = 1, N2 >*/
        i__1 = *n2;
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             CALL DLASSQ( N1, S( N2+1, I ), 1, DSCALE, DSUM ) >*/
            dlassq_(n1, &s[*n2 + 1 + (i__ << 2) - 5], &c__1, &dscale, &dsum);
/*<    30    CONTINUE >*/
/* L30: */
        }
/*<          BRQA21 = DSCALE*SQRT( DSUM ) >*/
        brqa21 = dscale * sqrt(dsum);

/*        Triangularize the B-part by a QR factorization. */
/*        Apply transformation (from right) to A-part, giving S. */

/*<          CALL DGEQR2( M, M, TCPY, LDST, TAUL, WORK, LINFO ) >*/
        dgeqr2_(&m, &m, tcpy, &c__4, taul, &work[1], &linfo);
/*<    >*/
        if (linfo != 0) {
            goto L70;
        }
/*<    >*/
        dorm2r_("L", "T", &m, &m, &m, tcpy, &c__4, taul, scpy, &c__4, &work[1]
                , info, (ftnlen)1, (ftnlen)1);
/*<    >*/
        dorm2r_("R", "N", &m, &m, &m, tcpy, &c__4, taul, licop, &c__4, &work[
                1], info, (ftnlen)1, (ftnlen)1);
/*<    >*/
        if (linfo != 0) {
            goto L70;
        }

/*        Compute F-norm(S21) in BQRA21. (T21 is 0.) */

/*<          DSCALE = ZERO >*/
        dscale = 0.;
/*<          DSUM = ONE >*/
        dsum = 1.;
/*<          DO 40 I = 1, N2 >*/
        i__1 = *n2;
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             CALL DLASSQ( N1, SCPY( N2+1, I ), 1, DSCALE, DSUM ) >*/
            dlassq_(n1, &scpy[*n2 + 1 + (i__ << 2) - 5], &c__1, &dscale, &
                    dsum);
/*<    40    CONTINUE >*/
/* L40: */
        }
/*<          BQRA21 = DSCALE*SQRT( DSUM ) >*/
        bqra21 = dscale * sqrt(dsum);

/*        Decide which method to use. */
/*          Weak stability test: */
/*             F-norm(S21) <= O(EPS * F-norm((S, T))) */

/*<          IF( BQRA21.LE.BRQA21 .AND. BQRA21.LE.THRESH ) THEN >*/
        if (bqra21 <= brqa21 && bqra21 <= thresh) {
/*<             CALL DLACPY( 'F', M, M, SCPY, LDST, S, LDST ) >*/
            dlacpy_("F", &m, &m, scpy, &c__4, s, &c__4, (ftnlen)1);
/*<             CALL DLACPY( 'F', M, M, TCPY, LDST, T, LDST ) >*/
            dlacpy_("F", &m, &m, tcpy, &c__4, t, &c__4, (ftnlen)1);
/*<             CALL DLACPY( 'F', M, M, IRCOP, LDST, IR, LDST ) >*/
            dlacpy_("F", &m, &m, ircop, &c__4, ir, &c__4, (ftnlen)1);
/*<             CALL DLACPY( 'F', M, M, LICOP, LDST, LI, LDST ) >*/
            dlacpy_("F", &m, &m, licop, &c__4, li, &c__4, (ftnlen)1);
/*<          ELSE IF( BRQA21.GE.THRESH ) THEN >*/
        } else if (brqa21 >= thresh) {
/*<             GO TO 70 >*/
            goto L70;
/*<          END IF >*/
        }

/*        Set lower triangle of B-part to zero */

/*<          DO 50 I = 2, M >*/
        i__1 = m;
        for (i__ = 2; i__ <= i__1; ++i__) {
/*<             CALL DCOPY( M-I+1, ZERO, 0, T( I, I-1 ), 1 ) >*/
            i__2 = m - i__ + 1;
            dcopy_(&i__2, &c_b3, &c__0, &t[i__ + ((i__ - 1) << 2) - 5], &c__1);
/*<    50    CONTINUE >*/
/* L50: */
        }

/*<          IF( WANDS ) THEN >*/
        if (TRUE_) {

/*           Strong stability test: */
/*              F-norm((A-QL*S*QR', B-QL*T*QR')) <= O(EPS*F-norm((A,B))) */

/*<    >*/
            dlacpy_("Full", &m, &m, &a[*j1 + *j1 * a_dim1], lda, &work[m * m
                    + 1], &m, (ftnlen)4);
/*<    >*/
            dgemm_("N", "N", &m, &m, &m, &c_b38, li, &c__4, s, &c__4, &c_b3, &
                    work[1], &m, (ftnlen)1, (ftnlen)1);
/*<    >*/
            dgemm_("N", "N", &m, &m, &m, &c_b44, &work[1], &m, ir, &c__4, &
                    c_b38, &work[m * m + 1], &m, (ftnlen)1, (ftnlen)1);
/*<             DSCALE = ZERO >*/
            dscale = 0.;
/*<             DSUM = ONE >*/
            dsum = 1.;
/*<             CALL DLASSQ( M*M, WORK( M*M+1 ), 1, DSCALE, DSUM ) >*/
            i__1 = m * m;
            dlassq_(&i__1, &work[m * m + 1], &c__1, &dscale, &dsum);

/*<    >*/
            dlacpy_("Full", &m, &m, &b[*j1 + *j1 * b_dim1], ldb, &work[m * m
                    + 1], &m, (ftnlen)4);
/*<    >*/
            dgemm_("N", "N", &m, &m, &m, &c_b38, li, &c__4, t, &c__4, &c_b3, &
                    work[1], &m, (ftnlen)1, (ftnlen)1);
/*<    >*/
            dgemm_("N", "N", &m, &m, &m, &c_b44, &work[1], &m, ir, &c__4, &
                    c_b38, &work[m * m + 1], &m, (ftnlen)1, (ftnlen)1);
/*<             CALL DLASSQ( M*M, WORK( M*M+1 ), 1, DSCALE, DSUM ) >*/
            i__1 = m * m;
            dlassq_(&i__1, &work[m * m + 1], &c__1, &dscale, &dsum);
/*<             SS = DSCALE*SQRT( DSUM ) >*/
            ss = dscale * sqrt(dsum);
/*<             DTRONG = ( SS.LE.THRESH ) >*/
            dtrong = ss <= thresh;
/*<    >*/
            if (! dtrong) {
                goto L70;
            }

/*<          END IF >*/
        }

/*        If the swap is accepted ("weakly" and "strongly"), apply the */
/*        transformations and set N1-by-N2 (2,1)-block to zero. */

/*<          DO 60 I = 1, N2 >*/
        i__1 = *n2;
        for (i__ = 1; i__ <= i__1; ++i__) {
/*<             CALL DCOPY( N1, ZERO, 0, S( N2+1, I ), 1 ) >*/
            dcopy_(n1, &c_b3, &c__0, &s[*n2 + 1 + (i__ << 2) - 5], &c__1);
/*<    60    CONTINUE >*/
/* L60: */
        }

/*        copy back M-by-M diagonal block starting at index J1 of (A, B) */

/*<          CALL DLACPY( 'F', M, M, S, LDST, A( J1, J1 ), LDA ) >*/
        dlacpy_("F", &m, &m, s, &c__4, &a[*j1 + *j1 * a_dim1], lda, (ftnlen)1)
                ;
/*<          CALL DLACPY( 'F', M, M, T, LDST, B( J1, J1 ), LDB ) >*/
        dlacpy_("F", &m, &m, t, &c__4, &b[*j1 + *j1 * b_dim1], ldb, (ftnlen)1)
                ;
/*<          CALL DCOPY( LDST*LDST, ZERO, 0, T, 1 ) >*/
        dcopy_(&c__16, &c_b3, &c__0, t, &c__1);

/*        Standardize existing 2-by-2 blocks. */

/*<          CALL DCOPY( M*M, ZERO, 0, WORK, 1 ) >*/
        i__1 = m * m;
        dcopy_(&i__1, &c_b3, &c__0, &work[1], &c__1);
/*<          WORK( 1 ) = ONE >*/
        work[1] = 1.;
/*<          T( 1, 1 ) = ONE >*/
        t[0] = 1.;
/*<          IDUM = LWORK - M*M - 2 >*/
        idum = *lwork - m * m - 2;
/*<          IF( N2.GT.1 ) THEN >*/
        if (*n2 > 1) {
/*<    >*/
            dlagv2_(&a[*j1 + *j1 * a_dim1], lda, &b[*j1 + *j1 * b_dim1], ldb,
                    ar, ai, be, &work[1], &work[2], t, &t[1]);
/*<             WORK( M+1 ) = -WORK( 2 ) >*/
            work[m + 1] = -work[2];
/*<             WORK( M+2 ) = WORK( 1 ) >*/
            work[m + 2] = work[1];
/*<             T( N2, N2 ) = T( 1, 1 ) >*/
            t[*n2 + (*n2 << 2) - 5] = t[0];
/*<             T( 1, 2 ) = -T( 2, 1 ) >*/
            t[4] = -t[1];
/*<          END IF >*/
        }
/*<          WORK( M*M ) = ONE >*/
        work[m * m] = 1.;
/*<          T( M, M ) = ONE >*/
        t[m + (m << 2) - 5] = 1.;

/*<          IF( N1.GT.1 ) THEN >*/
        if (*n1 > 1) {
/*<    >*/
            dlagv2_(&a[*j1 + *n2 + (*j1 + *n2) * a_dim1], lda, &b[*j1 + *n2 +
                    (*j1 + *n2) * b_dim1], ldb, taur, taul, &work[m * m + 1],
                    &work[*n2 * m + *n2 + 1], &work[*n2 * m + *n2 + 2], &t[*
                    n2 + 1 + ((*n2 + 1) << 2) - 5], &t[m + ((m - 1) << 2) - 5]);
/*<             WORK( M*M ) = WORK( N2*M+N2+1 ) >*/
            work[m * m] = work[*n2 * m + *n2 + 1];
/*<             WORK( M*M-1 ) = -WORK( N2*M+N2+2 ) >*/
            work[m * m - 1] = -work[*n2 * m + *n2 + 2];
/*<             T( M, M ) = T( N2+1, N2+1 ) >*/
            t[m + (m << 2) - 5] = t[*n2 + 1 + ((*n2 + 1) << 2) - 5];
/*<             T( M-1, M ) = -T( M, M-1 ) >*/
            t[m - 1 + (m << 2) - 5] = -t[m + ((m - 1) << 2) - 5];
/*<          END IF >*/
        }
/*<    >*/
        dgemm_("T", "N", n2, n1, n2, &c_b38, &work[1], &m, &a[*j1 + (*j1 + *
                n2) * a_dim1], lda, &c_b3, &work[m * m + 1], n2, (ftnlen)1, (
                ftnlen)1);
/*<    >*/
        dlacpy_("Full", n2, n1, &work[m * m + 1], n2, &a[*j1 + (*j1 + *n2) *
                a_dim1], lda, (ftnlen)4);
/*<    >*/
        dgemm_("T", "N", n2, n1, n2, &c_b38, &work[1], &m, &b[*j1 + (*j1 + *
                n2) * b_dim1], ldb, &c_b3, &work[m * m + 1], n2, (ftnlen)1, (
                ftnlen)1);
/*<    >*/
        dlacpy_("Full", n2, n1, &work[m * m + 1], n2, &b[*j1 + (*j1 + *n2) *
                b_dim1], ldb, (ftnlen)4);
/*<    >*/
        dgemm_("N", "N", &m, &m, &m, &c_b38, li, &c__4, &work[1], &m, &c_b3, &
                work[m * m + 1], &m, (ftnlen)1, (ftnlen)1);
/*<          CALL DLACPY( 'Full', M, M, WORK( M*M+1 ), M, LI, LDST ) >*/
        dlacpy_("Full", &m, &m, &work[m * m + 1], &m, li, &c__4, (ftnlen)4);
/*<    >*/
        dgemm_("N", "N", n2, n1, n1, &c_b38, &a[*j1 + (*j1 + *n2) * a_dim1],
                lda, &t[*n2 + 1 + ((*n2 + 1) << 2) - 5], &c__4, &c_b3, &work[1],
                 n2, (ftnlen)1, (ftnlen)1);
/*<          CALL DLACPY( 'Full', N2, N1, WORK, N2, A( J1, J1+N2 ), LDA ) >*/
        dlacpy_("Full", n2, n1, &work[1], n2, &a[*j1 + (*j1 + *n2) * a_dim1],
                lda, (ftnlen)4);
/*<    >*/
        dgemm_("N", "N", n2, n1, n1, &c_b38, &b[*j1 + (*j1 + *n2) * b_dim1],
                lda, &t[*n2 + 1 + ((*n2 + 1) << 2) - 5], &c__4, &c_b3, &work[1],
                 n2, (ftnlen)1, (ftnlen)1);
/*<          CALL DLACPY( 'Full', N2, N1, WORK, N2, B( J1, J1+N2 ), LDB ) >*/
        dlacpy_("Full", n2, n1, &work[1], n2, &b[*j1 + (*j1 + *n2) * b_dim1],
                ldb, (ftnlen)4);
/*<    >*/
        dgemm_("T", "N", &m, &m, &m, &c_b38, ir, &c__4, t, &c__4, &c_b3, &
                work[1], &m, (ftnlen)1, (ftnlen)1);
/*<          CALL DLACPY( 'Full', M, M, WORK, M, IR, LDST ) >*/
        dlacpy_("Full", &m, &m, &work[1], &m, ir, &c__4, (ftnlen)4);

/*        Accumulate transformations into Q and Z if requested. */

/*<          IF( WANTQ ) THEN >*/
        if (*wantq) {
/*<    >*/
            dgemm_("N", "N", n, &m, &m, &c_b38, &q[*j1 * q_dim1 + 1], ldq, li,
                     &c__4, &c_b3, &work[1], n, (ftnlen)1, (ftnlen)1);
/*<             CALL DLACPY( 'Full', N, M, WORK, N, Q( 1, J1 ), LDQ ) >*/
            dlacpy_("Full", n, &m, &work[1], n, &q[*j1 * q_dim1 + 1], ldq, (
                    ftnlen)4);

/*<          END IF >*/
        }

/*<          IF( WANTZ ) THEN >*/
        if (*wantz) {
/*<    >*/
            dgemm_("N", "N", n, &m, &m, &c_b38, &z__[*j1 * z_dim1 + 1], ldz,
                    ir, &c__4, &c_b3, &work[1], n, (ftnlen)1, (ftnlen)1);
/*<             CALL DLACPY( 'Full', N, M, WORK, N, Z( 1, J1 ), LDZ ) >*/
            dlacpy_("Full", n, &m, &work[1], n, &z__[*j1 * z_dim1 + 1], ldz, (
                    ftnlen)4);

/*<          END IF >*/
        }

/*        Update (A(J1:J1+M-1, M+J1:N), B(J1:J1+M-1, M+J1:N)) and */
/*                (A(1:J1-1, J1:J1+M), B(1:J1-1, J1:J1+M)). */

/*<          I = J1 + M >*/
        i__ = *j1 + m;
/*<          IF( I.LE.N ) THEN >*/
        if (i__ <= *n) {
/*<    >*/
            i__1 = *n - i__ + 1;
            dgemm_("T", "N", &m, &i__1, &m, &c_b38, li, &c__4, &a[*j1 + i__ *
                    a_dim1], lda, &c_b3, &work[1], &m, (ftnlen)1, (ftnlen)1);
/*<             CALL DLACPY( 'Full', M, N-I+1, WORK, M, A( J1, I ), LDA ) >*/
            i__1 = *n - i__ + 1;
            dlacpy_("Full", &m, &i__1, &work[1], &m, &a[*j1 + i__ * a_dim1],
                    lda, (ftnlen)4);
/*<    >*/
            i__1 = *n - i__ + 1;
            dgemm_("T", "N", &m, &i__1, &m, &c_b38, li, &c__4, &b[*j1 + i__ *
                    b_dim1], lda, &c_b3, &work[1], &m, (ftnlen)1, (ftnlen)1);
/*<             CALL DLACPY( 'Full', M, N-I+1, WORK, M, B( J1, I ), LDA ) >*/
            i__1 = *n - i__ + 1;
            dlacpy_("Full", &m, &i__1, &work[1], &m, &b[*j1 + i__ * b_dim1],
                    lda, (ftnlen)4);
/*<          END IF >*/
        }
/*<          I = J1 - 1 >*/
        i__ = *j1 - 1;
/*<          IF( I.GT.0 ) THEN >*/
        if (i__ > 0) {
/*<    >*/
            dgemm_("N", "N", &i__, &m, &m, &c_b38, &a[*j1 * a_dim1 + 1], lda,
                    ir, &c__4, &c_b3, &work[1], &i__, (ftnlen)1, (ftnlen)1);
/*<             CALL DLACPY( 'Full', I, M, WORK, I, A( 1, J1 ), LDA ) >*/
            dlacpy_("Full", &i__, &m, &work[1], &i__, &a[*j1 * a_dim1 + 1],
                    lda, (ftnlen)4);
/*<    >*/
            dgemm_("N", "N", &i__, &m, &m, &c_b38, &b[*j1 * b_dim1 + 1], ldb,
                    ir, &c__4, &c_b3, &work[1], &i__, (ftnlen)1, (ftnlen)1);
/*<             CALL DLACPY( 'Full', I, M, WORK, I, B( 1, J1 ), LDB ) >*/
            dlacpy_("Full", &i__, &m, &work[1], &i__, &b[*j1 * b_dim1 + 1],
                    ldb, (ftnlen)4);
/*<          END IF >*/
        }

/*        Exit with INFO = 0 if swap was successfully performed. */

/*<          RETURN >*/
        return 0;

/*<       END IF >*/
    }

/*     Exit with INFO = 1 if swap was rejected. */

/*<    70 CONTINUE >*/
L70:

/*<       INFO = 1 >*/
    *info = 1;
/*<       RETURN >*/
    return 0;

/*     End of DTGEX2 */

/*<       END >*/
} /* dtgex2_ */

#ifdef __cplusplus
        }
#endif
