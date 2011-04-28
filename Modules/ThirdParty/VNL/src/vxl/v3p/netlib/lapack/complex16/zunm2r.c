/* lapack/complex16/zunm2r.f -- translated by f2c (version 20090411).
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

static integer c__1 = 1;

/*<    >*/
/* Subroutine */ int zunm2r_(char *side, char *trans, integer *m, integer *n,
        integer *k, doublecomplex *a, integer *lda, doublecomplex *tau,
        doublecomplex *c__, integer *ldc, doublecomplex *work, integer *info,
        ftnlen side_len, ftnlen trans_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, c_dim1, c_offset, i__1, i__2, i__3;
    doublecomplex z__1;

    /* Builtin functions */
    void d_cnjg(doublecomplex *, doublecomplex *);

    /* Local variables */
    integer i__, i1, i2, i3, ic, jc, mi, ni, nq;
    doublecomplex aii;
    logical left;
    doublecomplex taui;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    extern /* Subroutine */ int zlarf_(char *, integer *, integer *,
            doublecomplex *, integer *, doublecomplex *, doublecomplex *,
            integer *, doublecomplex *, ftnlen), xerbla_(char *, integer *,
            ftnlen);
    logical notran;
    (void)side_len;
    (void)trans_len;

/*  -- LAPACK routine (version 3.2) -- */
/*  -- LAPACK is a software package provided by Univ. of Tennessee,    -- */
/*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..-- */
/*     November 2006 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          SIDE, TRANS >*/
/*<       INTEGER            INFO, K, LDA, LDC, M, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       COMPLEX*16         A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZUNM2R overwrites the general complex m-by-n matrix C with */

/*        Q * C  if SIDE = 'L' and TRANS = 'N', or */

/*        Q'* C  if SIDE = 'L' and TRANS = 'C', or */

/*        C * Q  if SIDE = 'R' and TRANS = 'N', or */

/*        C * Q' if SIDE = 'R' and TRANS = 'C', */

/*  where Q is a complex unitary matrix defined as the product of k */
/*  elementary reflectors */

/*        Q = H(1) H(2) . . . H(k) */

/*  as returned by ZGEQRF. Q is of order m if SIDE = 'L' and of order n */
/*  if SIDE = 'R'. */

/*  Arguments */
/*  ========= */

/*  SIDE    (input) CHARACTER*1 */
/*          = 'L': apply Q or Q' from the Left */
/*          = 'R': apply Q or Q' from the Right */

/*  TRANS   (input) CHARACTER*1 */
/*          = 'N': apply Q  (No transpose) */
/*          = 'C': apply Q' (Conjugate transpose) */

/*  M       (input) INTEGER */
/*          The number of rows of the matrix C. M >= 0. */

/*  N       (input) INTEGER */
/*          The number of columns of the matrix C. N >= 0. */

/*  K       (input) INTEGER */
/*          The number of elementary reflectors whose product defines */
/*          the matrix Q. */
/*          If SIDE = 'L', M >= K >= 0; */
/*          if SIDE = 'R', N >= K >= 0. */

/*  A       (input) COMPLEX*16 array, dimension (LDA,K) */
/*          The i-th column must contain the vector which defines the */
/*          elementary reflector H(i), for i = 1,2,...,k, as returned by */
/*          ZGEQRF in the first k columns of its array argument A. */
/*          A is modified by the routine but restored on exit. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A. */
/*          If SIDE = 'L', LDA >= max(1,M); */
/*          if SIDE = 'R', LDA >= max(1,N). */

/*  TAU     (input) COMPLEX*16 array, dimension (K) */
/*          TAU(i) must contain the scalar factor of the elementary */
/*          reflector H(i), as returned by ZGEQRF. */

/*  C       (input/output) COMPLEX*16 array, dimension (LDC,N) */
/*          On entry, the m-by-n matrix C. */
/*          On exit, C is overwritten by Q*C or Q'*C or C*Q' or C*Q. */

/*  LDC     (input) INTEGER */
/*          The leading dimension of the array C. LDC >= max(1,M). */

/*  WORK    (workspace) COMPLEX*16 array, dimension */
/*                                   (N) if SIDE = 'L', */
/*                                   (M) if SIDE = 'R' */

/*  INFO    (output) INTEGER */
/*          = 0: successful exit */
/*          < 0: if INFO = -i, the i-th argument had an illegal value */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       COMPLEX*16         ONE >*/
/*<       PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ) ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            LEFT, NOTRAN >*/
/*<       INTEGER            I, I1, I2, I3, IC, JC, MI, NI, NQ >*/
/*<       COMPLEX*16         AII, TAUI >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       EXTERNAL           LSAME >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           XERBLA, ZLARF >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          DCONJG, MAX >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Test the input arguments */

/*<       INFO = 0 >*/
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --tau;
    c_dim1 = *ldc;
    c_offset = 1 + c_dim1;
    c__ -= c_offset;
    --work;

    /* Function Body */
    *info = 0;
/*<       LEFT = LSAME( SIDE, 'L' ) >*/
    left = lsame_(side, "L", (ftnlen)1, (ftnlen)1);
/*<       NOTRAN = LSAME( TRANS, 'N' ) >*/
    notran = lsame_(trans, "N", (ftnlen)1, (ftnlen)1);

/*     NQ is the order of Q */

/*<       IF( LEFT ) THEN >*/
    if (left) {
/*<          NQ = M >*/
        nq = *m;
/*<       ELSE >*/
    } else {
/*<          NQ = N >*/
        nq = *n;
/*<       END IF >*/
    }
/*<       IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN >*/
    if (! left && ! lsame_(side, "R", (ftnlen)1, (ftnlen)1)) {
/*<          INFO = -1 >*/
        *info = -1;
/*<       ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'C' ) ) THEN >*/
    } else if (! notran && ! lsame_(trans, "C", (ftnlen)1, (ftnlen)1)) {
/*<          INFO = -2 >*/
        *info = -2;
/*<       ELSE IF( M.LT.0 ) THEN >*/
    } else if (*m < 0) {
/*<          INFO = -3 >*/
        *info = -3;
/*<       ELSE IF( N.LT.0 ) THEN >*/
    } else if (*n < 0) {
/*<          INFO = -4 >*/
        *info = -4;
/*<       ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN >*/
    } else if (*k < 0 || *k > nq) {
/*<          INFO = -5 >*/
        *info = -5;
/*<       ELSE IF( LDA.LT.MAX( 1, NQ ) ) THEN >*/
    } else if (*lda < max(1,nq)) {
/*<          INFO = -7 >*/
        *info = -7;
/*<       ELSE IF( LDC.LT.MAX( 1, M ) ) THEN >*/
    } else if (*ldc < max(1,*m)) {
/*<          INFO = -10 >*/
        *info = -10;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'ZUNM2R', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("ZUNM2R", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible */

/*<    >*/
    if (*m == 0 || *n == 0 || *k == 0) {
        return 0;
    }

/*<       IF( ( LEFT .AND. .NOT.NOTRAN .OR. .NOT.LEFT .AND. NOTRAN ) ) THEN >*/
    if ((left && ! notran) || (! left && notran)) {
/*<          I1 = 1 >*/
        i1 = 1;
/*<          I2 = K >*/
        i2 = *k;
/*<          I3 = 1 >*/
        i3 = 1;
/*<       ELSE >*/
    } else {
/*<          I1 = K >*/
        i1 = *k;
/*<          I2 = 1 >*/
        i2 = 1;
/*<          I3 = -1 >*/
        i3 = -1;
/*<       END IF >*/
    }

/*<       IF( LEFT ) THEN >*/
    if (left) {
/*<          NI = N >*/
        ni = *n;
/*<          JC = 1 >*/
        jc = 1;
/*<       ELSE >*/
    } else {
/*<          MI = M >*/
        mi = *m;
/*<          IC = 1 >*/
        ic = 1;
/*<       END IF >*/
    }

/*<       DO 10 I = I1, I2, I3 >*/
    i__1 = i2;
    i__2 = i3;
    for (i__ = i1; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
/*<          IF( LEFT ) THEN >*/
        if (left) {

/*           H(i) or H(i)' is applied to C(i:m,1:n) */

/*<             MI = M - I + 1 >*/
            mi = *m - i__ + 1;
/*<             IC = I >*/
            ic = i__;
/*<          ELSE >*/
        } else {

/*           H(i) or H(i)' is applied to C(1:m,i:n) */

/*<             NI = N - I + 1 >*/
            ni = *n - i__ + 1;
/*<             JC = I >*/
            jc = i__;
/*<          END IF >*/
        }

/*        Apply H(i) or H(i)' */

/*<          IF( NOTRAN ) THEN >*/
        if (notran) {
/*<             TAUI = TAU( I ) >*/
            i__3 = i__;
            taui.r = tau[i__3].r, taui.i = tau[i__3].i;
/*<          ELSE >*/
        } else {
/*<             TAUI = DCONJG( TAU( I ) ) >*/
            d_cnjg(&z__1, &tau[i__]);
            taui.r = z__1.r, taui.i = z__1.i;
/*<          END IF >*/
        }
/*<          AII = A( I, I ) >*/
        i__3 = i__ + i__ * a_dim1;
        aii.r = a[i__3].r, aii.i = a[i__3].i;
/*<          A( I, I ) = ONE >*/
        i__3 = i__ + i__ * a_dim1;
        a[i__3].r = 1., a[i__3].i = 0.;
/*<    >*/
        zlarf_(side, &mi, &ni, &a[i__ + i__ * a_dim1], &c__1, &taui, &c__[ic
                + jc * c_dim1], ldc, &work[1], (ftnlen)1);
/*<          A( I, I ) = AII >*/
        i__3 = i__ + i__ * a_dim1;
        a[i__3].r = aii.r, a[i__3].i = aii.i;
/*<    10 CONTINUE >*/
/* L10: */
    }
/*<       RETURN >*/
    return 0;

/*     End of ZUNM2R */

/*<       END >*/
} /* zunm2r_ */

#ifdef __cplusplus
        }
#endif
