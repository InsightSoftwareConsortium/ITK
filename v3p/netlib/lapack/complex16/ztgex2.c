/* lapack/complex16/ztgex2.f -- translated by f2c (version 20090411).
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

static integer c__2 = 2;
static integer c__1 = 1;

/*<    >*/
/* Subroutine */ int ztgex2_(logical *wantq, logical *wantz, integer *n,
        doublecomplex *a, integer *lda, doublecomplex *b, integer *ldb,
        doublecomplex *q, integer *ldq, doublecomplex *z__, integer *ldz,
        integer *j1, integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, q_dim1, q_offset, z_dim1,
            z_offset, i__1, i__2, i__3;
    doublereal d__1;
    doublecomplex z__1, z__2, z__3;

    /* Builtin functions */
    double sqrt(doublereal), z_abs(doublecomplex *);
    void d_cnjg(doublecomplex *, doublecomplex *);

    /* Local variables */
    doublecomplex f, g;
    integer i__, m;
    doublecomplex s[4]        /* was [2][2] */, t[4]        /* was [2][2] */;
    doublereal cq, sa, sb, cz;
    doublecomplex sq;
    doublereal ss, ws;
    doublecomplex sz;
    doublereal eps, sum;
    logical weak;
    doublecomplex cdum, work[8];
    extern /* Subroutine */ int zrot_(integer *, doublecomplex *, integer *,
            doublecomplex *, integer *, doublereal *, doublecomplex *);
    doublereal scale;
    extern doublereal dlamch_(char *, ftnlen);
    logical dtrong;
    doublereal thresh;
    extern /* Subroutine */ int zlacpy_(char *, integer *, integer *,
            doublecomplex *, integer *, doublecomplex *, integer *, ftnlen),
            zlartg_(doublecomplex *, doublecomplex *, doublereal *,
            doublecomplex *, doublecomplex *);
    doublereal smlnum;
    extern /* Subroutine */ int zlassq_(integer *, doublecomplex *, integer *,
             doublereal *, doublereal *);


/*  -- LAPACK auxiliary routine (version 3.2.2) -- */
/*  -- LAPACK is a software package provided by Univ. of Tennessee,    -- */
/*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..-- */
/*     June 2010 */

/*     .. Scalar Arguments .. */
/*<       LOGICAL            WANTQ, WANTZ >*/
/*<       INTEGER            INFO, J1, LDA, LDB, LDQ, LDZ, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<    >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZTGEX2 swaps adjacent diagonal 1 by 1 blocks (A11,B11) and (A22,B22) */
/*  in an upper triangular matrix pair (A, B) by an unitary equivalence */
/*  transformation. */

/*  (A, B) must be in generalized Schur canonical form, that is, A and */
/*  B are both upper triangular. */

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

/*  A       (input/output) COMPLEX*16 arrays, dimensions (LDA,N) */
/*          On entry, the matrix A in the pair (A, B). */
/*          On exit, the updated matrix A. */

/*  LDA     (input)  INTEGER */
/*          The leading dimension of the array A. LDA >= max(1,N). */

/*  B       (input/output) COMPLEX*16 arrays, dimensions (LDB,N) */
/*          On entry, the matrix B in the pair (A, B). */
/*          On exit, the updated matrix B. */

/*  LDB     (input)  INTEGER */
/*          The leading dimension of the array B. LDB >= max(1,N). */

/*  Q       (input/output) COMPLEX*16 array, dimension (LDZ,N) */
/*          If WANTQ = .TRUE, on entry, the unitary matrix Q. On exit, */
/*          the updated matrix Q. */
/*          Not referenced if WANTQ = .FALSE.. */

/*  LDQ     (input) INTEGER */
/*          The leading dimension of the array Q. LDQ >= 1; */
/*          If WANTQ = .TRUE., LDQ >= N. */

/*  Z       (input/output) COMPLEX*16 array, dimension (LDZ,N) */
/*          If WANTZ = .TRUE, on entry, the unitary matrix Z. On exit, */
/*          the updated matrix Z. */
/*          Not referenced if WANTZ = .FALSE.. */

/*  LDZ     (input) INTEGER */
/*          The leading dimension of the array Z. LDZ >= 1; */
/*          If WANTZ = .TRUE., LDZ >= N. */

/*  J1      (input) INTEGER */
/*          The index to the first block (A11, B11). */

/*  INFO    (output) INTEGER */
/*           =0:  Successful exit. */
/*           =1:  The transformed matrix pair (A, B) would be too far */
/*                from generalized Schur form; the problem is ill- */
/*                conditioned. */


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
/*      Estimation: Theory, Algorithms and Software, Report UMINF-94.04, */
/*      Department of Computing Science, Umea University, S-901 87 Umea, */
/*      Sweden, 1994. Also as LAPACK Working Note 87. To appear in */
/*      Numerical Algorithms, 1996. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       COMPLEX*16         CZERO, CONE >*/
/*<    >*/
/*<       DOUBLE PRECISION   TWENTY >*/
/*<       PARAMETER          ( TWENTY = 2.0D+1 ) >*/
/*<       INTEGER            LDST >*/
/*<       PARAMETER          ( LDST = 2 ) >*/
/*<       LOGICAL            WANDS >*/
/*<       PARAMETER          ( WANDS = .TRUE. ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            DTRONG, WEAK >*/
/*<       INTEGER            I, M >*/
/*<    >*/
/*<       COMPLEX*16         CDUM, F, G, SQ, SZ >*/
/*     .. */
/*     .. Local Arrays .. */
/*<       COMPLEX*16         S( LDST, LDST ), T( LDST, LDST ), WORK( 8 ) >*/
/*     .. */
/*     .. External Functions .. */
/*<       DOUBLE PRECISION   DLAMCH >*/
/*<       EXTERNAL           DLAMCH >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           ZLACPY, ZLARTG, ZLASSQ, ZROT >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, DBLE, DCONJG, MAX, SQRT >*/
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

    /* Function Body */
    *info = 0;

/*     Quick return if possible */

/*<    >*/
    if (*n <= 1) {
        return 0;
    }

/*<       M = LDST >*/
    m = 2;
/*<       WEAK = .FALSE. >*/
//    weak = FALSE_;
/*<       DTRONG = .FALSE. >*/
//    dtrong = FALSE_;

/*     Make a local copy of selected block in (A, B) */

/*<       CALL ZLACPY( 'Full', M, M, A( J1, J1 ), LDA, S, LDST ) >*/
    zlacpy_("Full", &m, &m, &a[*j1 + *j1 * a_dim1], lda, s, &c__2, (ftnlen)4);
/*<       CALL ZLACPY( 'Full', M, M, B( J1, J1 ), LDB, T, LDST ) >*/
    zlacpy_("Full", &m, &m, &b[*j1 + *j1 * b_dim1], ldb, t, &c__2, (ftnlen)4);

/*     Compute the threshold for testing the acceptance of swapping. */

/*<       EPS = DLAMCH( 'P' ) >*/
    eps = dlamch_("P", (ftnlen)1);
/*<       SMLNUM = DLAMCH( 'S' ) / EPS >*/
    smlnum = dlamch_("S", (ftnlen)1) / eps;
/*<       SCALE = DBLE( CZERO ) >*/
    scale = 0.;
/*<       SUM = DBLE( CONE ) >*/
    sum = 1.;
/*<       CALL ZLACPY( 'Full', M, M, S, LDST, WORK, M ) >*/
    zlacpy_("Full", &m, &m, s, &c__2, work, &m, (ftnlen)4);
/*<       CALL ZLACPY( 'Full', M, M, T, LDST, WORK( M*M+1 ), M ) >*/
    zlacpy_("Full", &m, &m, t, &c__2, &work[m * m], &m, (ftnlen)4);
/*<       CALL ZLASSQ( 2*M*M, WORK, 1, SCALE, SUM ) >*/
    i__1 = (m << 1) * m;
    zlassq_(&i__1, work, &c__1, &scale, &sum);
/*<       SA = SCALE*SQRT( SUM ) >*/
    sa = scale * sqrt(sum);

/*     THRES has been changed from */
/*        THRESH = MAX( TEN*EPS*SA, SMLNUM ) */
/*     to */
/*        THRESH = MAX( TWENTY*EPS*SA, SMLNUM ) */
/*     on 04/01/10. */
/*     "Bug" reported by Ondra Kamenik, confirmed by Julie Langou, fixed by */
/*     Jim Demmel and Guillaume Revy. See forum post 1783. */

/*<       THRESH = MAX( TWENTY*EPS*SA, SMLNUM ) >*/
/* Computing MAX */
    d__1 = eps * 20. * sa;
    thresh = max(d__1,smlnum);

/*     Compute unitary QL and RQ that swap 1-by-1 and 1-by-1 blocks */
/*     using Givens rotations and perform the swap tentatively. */

/*<       F = S( 2, 2 )*T( 1, 1 ) - T( 2, 2 )*S( 1, 1 ) >*/
    z__2.r = s[3].r * t[0].r - s[3].i * t[0].i, z__2.i = s[3].r * t[0].i + s[
            3].i * t[0].r;
    z__3.r = t[3].r * s[0].r - t[3].i * s[0].i, z__3.i = t[3].r * s[0].i + t[
            3].i * s[0].r;
    z__1.r = z__2.r - z__3.r, z__1.i = z__2.i - z__3.i;
    f.r = z__1.r, f.i = z__1.i;
/*<       G = S( 2, 2 )*T( 1, 2 ) - T( 2, 2 )*S( 1, 2 ) >*/
    z__2.r = s[3].r * t[2].r - s[3].i * t[2].i, z__2.i = s[3].r * t[2].i + s[
            3].i * t[2].r;
    z__3.r = t[3].r * s[2].r - t[3].i * s[2].i, z__3.i = t[3].r * s[2].i + t[
            3].i * s[2].r;
    z__1.r = z__2.r - z__3.r, z__1.i = z__2.i - z__3.i;
    g.r = z__1.r, g.i = z__1.i;
/*<       SA = ABS( S( 2, 2 ) ) >*/
    sa = z_abs(&s[3]);
/*<       SB = ABS( T( 2, 2 ) ) >*/
    sb = z_abs(&t[3]);
/*<       CALL ZLARTG( G, F, CZ, SZ, CDUM ) >*/
    zlartg_(&g, &f, &cz, &sz, &cdum);
/*<       SZ = -SZ >*/
    z__1.r = -sz.r, z__1.i = -sz.i;
    sz.r = z__1.r, sz.i = z__1.i;
/*<       CALL ZROT( 2, S( 1, 1 ), 1, S( 1, 2 ), 1, CZ, DCONJG( SZ ) ) >*/
    d_cnjg(&z__1, &sz);
    zrot_(&c__2, s, &c__1, &s[2], &c__1, &cz, &z__1);
/*<       CALL ZROT( 2, T( 1, 1 ), 1, T( 1, 2 ), 1, CZ, DCONJG( SZ ) ) >*/
    d_cnjg(&z__1, &sz);
    zrot_(&c__2, t, &c__1, &t[2], &c__1, &cz, &z__1);
/*<       IF( SA.GE.SB ) THEN >*/
    if (sa >= sb) {
/*<          CALL ZLARTG( S( 1, 1 ), S( 2, 1 ), CQ, SQ, CDUM ) >*/
        zlartg_(s, &s[1], &cq, &sq, &cdum);
/*<       ELSE >*/
    } else {
/*<          CALL ZLARTG( T( 1, 1 ), T( 2, 1 ), CQ, SQ, CDUM ) >*/
        zlartg_(t, &t[1], &cq, &sq, &cdum);
/*<       END IF >*/
    }
/*<       CALL ZROT( 2, S( 1, 1 ), LDST, S( 2, 1 ), LDST, CQ, SQ ) >*/
    zrot_(&c__2, s, &c__2, &s[1], &c__2, &cq, &sq);
/*<       CALL ZROT( 2, T( 1, 1 ), LDST, T( 2, 1 ), LDST, CQ, SQ ) >*/
    zrot_(&c__2, t, &c__2, &t[1], &c__2, &cq, &sq);

/*     Weak stability test: |S21| + |T21| <= O(EPS F-norm((S, T))) */

/*<       WS = ABS( S( 2, 1 ) ) + ABS( T( 2, 1 ) ) >*/
    ws = z_abs(&s[1]) + z_abs(&t[1]);
/*<       WEAK = WS.LE.THRESH >*/
    weak = ws <= thresh;
/*<    >*/
    if (! weak) {
        goto L20;
    }

/*<       IF( WANDS ) THEN >*/
    if (TRUE_) {

/*        Strong stability test: */
/*           F-norm((A-QL'*S*QR, B-QL'*T*QR)) <= O(EPS*F-norm((A, B))) */

/*<          CALL ZLACPY( 'Full', M, M, S, LDST, WORK, M ) >*/
        zlacpy_("Full", &m, &m, s, &c__2, work, &m, (ftnlen)4);
/*<          CALL ZLACPY( 'Full', M, M, T, LDST, WORK( M*M+1 ), M ) >*/
        zlacpy_("Full", &m, &m, t, &c__2, &work[m * m], &m, (ftnlen)4);
/*<          CALL ZROT( 2, WORK, 1, WORK( 3 ), 1, CZ, -DCONJG( SZ ) ) >*/
        d_cnjg(&z__2, &sz);
        z__1.r = -z__2.r, z__1.i = -z__2.i;
        zrot_(&c__2, work, &c__1, &work[2], &c__1, &cz, &z__1);
/*<          CALL ZROT( 2, WORK( 5 ), 1, WORK( 7 ), 1, CZ, -DCONJG( SZ ) ) >*/
        d_cnjg(&z__2, &sz);
        z__1.r = -z__2.r, z__1.i = -z__2.i;
        zrot_(&c__2, &work[4], &c__1, &work[6], &c__1, &cz, &z__1);
/*<          CALL ZROT( 2, WORK, 2, WORK( 2 ), 2, CQ, -SQ ) >*/
        z__1.r = -sq.r, z__1.i = -sq.i;
        zrot_(&c__2, work, &c__2, &work[1], &c__2, &cq, &z__1);
/*<          CALL ZROT( 2, WORK( 5 ), 2, WORK( 6 ), 2, CQ, -SQ ) >*/
        z__1.r = -sq.r, z__1.i = -sq.i;
        zrot_(&c__2, &work[4], &c__2, &work[5], &c__2, &cq, &z__1);
/*<          DO 10 I = 1, 2 >*/
        for (i__ = 1; i__ <= 2; ++i__) {
/*<             WORK( I ) = WORK( I ) - A( J1+I-1, J1 ) >*/
            i__1 = i__ - 1;
            i__2 = i__ - 1;
            i__3 = *j1 + i__ - 1 + *j1 * a_dim1;
            z__1.r = work[i__2].r - a[i__3].r, z__1.i = work[i__2].i - a[i__3]
                    .i;
            work[i__1].r = z__1.r, work[i__1].i = z__1.i;
/*<             WORK( I+2 ) = WORK( I+2 ) - A( J1+I-1, J1+1 ) >*/
            i__1 = i__ + 1;
            i__2 = i__ + 1;
            i__3 = *j1 + i__ - 1 + (*j1 + 1) * a_dim1;
            z__1.r = work[i__2].r - a[i__3].r, z__1.i = work[i__2].i - a[i__3]
                    .i;
            work[i__1].r = z__1.r, work[i__1].i = z__1.i;
/*<             WORK( I+4 ) = WORK( I+4 ) - B( J1+I-1, J1 ) >*/
            i__1 = i__ + 3;
            i__2 = i__ + 3;
            i__3 = *j1 + i__ - 1 + *j1 * b_dim1;
            z__1.r = work[i__2].r - b[i__3].r, z__1.i = work[i__2].i - b[i__3]
                    .i;
            work[i__1].r = z__1.r, work[i__1].i = z__1.i;
/*<             WORK( I+6 ) = WORK( I+6 ) - B( J1+I-1, J1+1 ) >*/
            i__1 = i__ + 5;
            i__2 = i__ + 5;
            i__3 = *j1 + i__ - 1 + (*j1 + 1) * b_dim1;
            z__1.r = work[i__2].r - b[i__3].r, z__1.i = work[i__2].i - b[i__3]
                    .i;
            work[i__1].r = z__1.r, work[i__1].i = z__1.i;
/*<    10    CONTINUE >*/
/* L10: */
        }
/*<          SCALE = DBLE( CZERO ) >*/
        scale = 0.;
/*<          SUM = DBLE( CONE ) >*/
        sum = 1.;
/*<          CALL ZLASSQ( 2*M*M, WORK, 1, SCALE, SUM ) >*/
        i__1 = (m << 1) * m;
        zlassq_(&i__1, work, &c__1, &scale, &sum);
/*<          SS = SCALE*SQRT( SUM ) >*/
        ss = scale * sqrt(sum);
/*<          DTRONG = SS.LE.THRESH >*/
        dtrong = ss <= thresh;
/*<    >*/
        if (! dtrong) {
            goto L20;
        }
/*<       END IF >*/
    }

/*     If the swap is accepted ("weakly" and "strongly"), apply the */
/*     equivalence transformations to the original matrix pair (A,B) */

/*<    >*/
    i__1 = *j1 + 1;
    d_cnjg(&z__1, &sz);
    zrot_(&i__1, &a[*j1 * a_dim1 + 1], &c__1, &a[(*j1 + 1) * a_dim1 + 1], &
            c__1, &cz, &z__1);
/*<    >*/
    i__1 = *j1 + 1;
    d_cnjg(&z__1, &sz);
    zrot_(&i__1, &b[*j1 * b_dim1 + 1], &c__1, &b[(*j1 + 1) * b_dim1 + 1], &
            c__1, &cz, &z__1);
/*<       CALL ZROT( N-J1+1, A( J1, J1 ), LDA, A( J1+1, J1 ), LDA, CQ, SQ ) >*/
    i__1 = *n - *j1 + 1;
    zrot_(&i__1, &a[*j1 + *j1 * a_dim1], lda, &a[*j1 + 1 + *j1 * a_dim1], lda,
             &cq, &sq);
/*<       CALL ZROT( N-J1+1, B( J1, J1 ), LDB, B( J1+1, J1 ), LDB, CQ, SQ ) >*/
    i__1 = *n - *j1 + 1;
    zrot_(&i__1, &b[*j1 + *j1 * b_dim1], ldb, &b[*j1 + 1 + *j1 * b_dim1], ldb,
             &cq, &sq);

/*     Set  N1 by N2 (2,1) blocks to 0 */

/*<       A( J1+1, J1 ) = CZERO >*/
    i__1 = *j1 + 1 + *j1 * a_dim1;
    a[i__1].r = 0., a[i__1].i = 0.;
/*<       B( J1+1, J1 ) = CZERO >*/
    i__1 = *j1 + 1 + *j1 * b_dim1;
    b[i__1].r = 0., b[i__1].i = 0.;

/*     Accumulate transformations into Q and Z if requested. */

/*<    >*/
    if (*wantz) {
        d_cnjg(&z__1, &sz);
        zrot_(n, &z__[*j1 * z_dim1 + 1], &c__1, &z__[(*j1 + 1) * z_dim1 + 1],
                &c__1, &cz, &z__1);
    }
/*<    >*/
    if (*wantq) {
        d_cnjg(&z__1, &sq);
        zrot_(n, &q[*j1 * q_dim1 + 1], &c__1, &q[(*j1 + 1) * q_dim1 + 1], &
                c__1, &cq, &z__1);
    }

/*     Exit with INFO = 0 if swap was successfully performed. */

/*<       RETURN >*/
    return 0;

/*     Exit with INFO = 1 if swap was rejected. */

/*<    20 CONTINUE >*/
L20:
/*<       INFO = 1 >*/
    *info = 1;
/*<       RETURN >*/
    return 0;

/*     End of ZTGEX2 */

/*<       END >*/
} /* ztgex2_ */

#ifdef __cplusplus
        }
#endif
