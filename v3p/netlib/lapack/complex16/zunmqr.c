/* lapack/complex16/zunmqr.f -- translated by f2c (version 20090411).
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
static integer c_n1 = -1;
static integer c__2 = 2;
static integer c__65 = 65;

/*<    >*/
/* Subroutine */ int zunmqr_(char *side, char *trans, integer *m, integer *n,
        integer *k, doublecomplex *a, integer *lda, doublecomplex *tau,
        doublecomplex *c__, integer *ldc, doublecomplex *work, integer *lwork,
         integer *info, ftnlen side_len, ftnlen trans_len)
{
    /* System generated locals */
    address a__1[2];
    integer a_dim1, a_offset, c_dim1, c_offset, i__1, i__2, i__3[2], i__4,
            i__5;
    char ch__1[2];

    /* Builtin functions */
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);

    /* Local variables */
    integer i__;
    doublecomplex t[4160]        /* was [65][64] */;
    integer i1, i2, i3, ib, ic, jc, nb, mi, ni, nq, nw, iws;
    logical left;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    integer nbmin, iinfo;
    extern /* Subroutine */ int zunm2r_(char *, char *, integer *, integer *,
            integer *, doublecomplex *, integer *, doublecomplex *,
            doublecomplex *, integer *, doublecomplex *, integer *, ftnlen,
            ftnlen), xerbla_(char *, integer *, ftnlen);
    extern integer ilaenv_(integer *, char *, char *, integer *, integer *,
            integer *, integer *, ftnlen, ftnlen);
    extern /* Subroutine */ int zlarfb_(char *, char *, char *, char *,
            integer *, integer *, integer *, doublecomplex *, integer *,
            doublecomplex *, integer *, doublecomplex *, integer *,
            doublecomplex *, integer *, ftnlen, ftnlen, ftnlen, ftnlen);
    logical notran;
    integer ldwork;
    extern /* Subroutine */ int zlarft_(char *, char *, integer *, integer *,
            doublecomplex *, integer *, doublecomplex *, doublecomplex *,
            integer *, ftnlen, ftnlen);
    integer lwkopt;
    logical lquery;
    (void)side_len;
    (void)trans_len;

/*  -- LAPACK routine (version 3.2) -- */
/*  -- LAPACK is a software package provided by Univ. of Tennessee,    -- */
/*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..-- */
/*     November 2006 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          SIDE, TRANS >*/
/*<       INTEGER            INFO, K, LDA, LDC, LWORK, M, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       COMPLEX*16         A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZUNMQR overwrites the general complex M-by-N matrix C with */

/*                  SIDE = 'L'     SIDE = 'R' */
/*  TRANS = 'N':      Q * C          C * Q */
/*  TRANS = 'C':      Q**H * C       C * Q**H */

/*  where Q is a complex unitary matrix defined as the product of k */
/*  elementary reflectors */

/*        Q = H(1) H(2) . . . H(k) */

/*  as returned by ZGEQRF. Q is of order M if SIDE = 'L' and of order N */
/*  if SIDE = 'R'. */

/*  Arguments */
/*  ========= */

/*  SIDE    (input) CHARACTER*1 */
/*          = 'L': apply Q or Q**H from the Left; */
/*          = 'R': apply Q or Q**H from the Right. */

/*  TRANS   (input) CHARACTER*1 */
/*          = 'N':  No transpose, apply Q; */
/*          = 'C':  Conjugate transpose, apply Q**H. */

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
/*          On entry, the M-by-N matrix C. */
/*          On exit, C is overwritten by Q*C or Q**H*C or C*Q**H or C*Q. */

/*  LDC     (input) INTEGER */
/*          The leading dimension of the array C. LDC >= max(1,M). */

/*  WORK    (workspace/output) COMPLEX*16 array, dimension (MAX(1,LWORK)) */
/*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK. */

/*  LWORK   (input) INTEGER */
/*          The dimension of the array WORK. */
/*          If SIDE = 'L', LWORK >= max(1,N); */
/*          if SIDE = 'R', LWORK >= max(1,M). */
/*          For optimum performance LWORK >= N*NB if SIDE = 'L', and */
/*          LWORK >= M*NB if SIDE = 'R', where NB is the optimal */
/*          blocksize. */

/*          If LWORK = -1, then a workspace query is assumed; the routine */
/*          only calculates the optimal size of the WORK array, returns */
/*          this value as the first entry of the WORK array, and no error */
/*          message related to LWORK is issued by XERBLA. */

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       INTEGER            NBMAX, LDT >*/
/*<       PARAMETER          ( NBMAX = 64, LDT = NBMAX+1 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            LEFT, LQUERY, NOTRAN >*/
/*<    >*/
/*     .. */
/*     .. Local Arrays .. */
/*<       COMPLEX*16         T( LDT, NBMAX ) >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       INTEGER            ILAENV >*/
/*<       EXTERNAL           LSAME, ILAENV >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           XERBLA, ZLARFB, ZLARFT, ZUNM2R >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          MAX, MIN >*/
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
/*<       LQUERY = ( LWORK.EQ.-1 ) >*/
    lquery = *lwork == -1;

/*     NQ is the order of Q and NW is the minimum dimension of WORK */

/*<       IF( LEFT ) THEN >*/
    if (left) {
/*<          NQ = M >*/
        nq = *m;
/*<          NW = N >*/
        nw = *n;
/*<       ELSE >*/
    } else {
/*<          NQ = N >*/
        nq = *n;
/*<          NW = M >*/
        nw = *m;
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
/*<       ELSE IF( LWORK.LT.MAX( 1, NW ) .AND. .NOT.LQUERY ) THEN >*/
    } else if (*lwork < max(1,nw) && ! lquery) {
/*<          INFO = -12 >*/
        *info = -12;
/*<       END IF >*/
    }

/*<       IF( INFO.EQ.0 ) THEN >*/
    if (*info == 0) {

/*        Determine the block size.  NB may be at most NBMAX, where NBMAX */
/*        is used to define the local array T. */

/*<    >*/
/* Computing MIN */
/* Writing concatenation */
        i__3[0] = 1, a__1[0] = side;
        i__3[1] = 1, a__1[1] = trans;
        s_cat(ch__1, a__1, i__3, &c__2, (ftnlen)2);
        i__1 = 64, i__2 = ilaenv_(&c__1, "ZUNMQR", ch__1, m, n, k, &c_n1, (
                ftnlen)6, (ftnlen)2);
        nb = min(i__1,i__2);
/*<          LWKOPT = MAX( 1, NW )*NB >*/
        lwkopt = max(1,nw) * nb;
/*<          WORK( 1 ) = LWKOPT >*/
        work[1].r = (doublereal) lwkopt, work[1].i = 0.;
/*<       END IF >*/
    }

/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'ZUNMQR', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("ZUNMQR", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       ELSE IF( LQUERY ) THEN >*/
    } else if (lquery) {
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible */

/*<       IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 ) THEN >*/
    if (*m == 0 || *n == 0 || *k == 0) {
/*<          WORK( 1 ) = 1 >*/
        work[1].r = 1., work[1].i = 0.;
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*<       NBMIN = 2 >*/
    nbmin = 2;
/*<       LDWORK = NW >*/
    ldwork = nw;
/*<       IF( NB.GT.1 .AND. NB.LT.K ) THEN >*/
    if (nb > 1 && nb < *k) {
/*<          IWS = NW*NB >*/
        iws = nw * nb;
/*<          IF( LWORK.LT.IWS ) THEN >*/
        if (*lwork < iws) {
/*<             NB = LWORK / LDWORK >*/
            nb = *lwork / ldwork;
/*<    >*/
/* Computing MAX */
/* Writing concatenation */
            i__3[0] = 1, a__1[0] = side;
            i__3[1] = 1, a__1[1] = trans;
            s_cat(ch__1, a__1, i__3, &c__2, (ftnlen)2);
            i__1 = 2, i__2 = ilaenv_(&c__2, "ZUNMQR", ch__1, m, n, k, &c_n1, (
                    ftnlen)6, (ftnlen)2);
            nbmin = max(i__1,i__2);
/*<          END IF >*/
        }
/*<       ELSE >*/
    } else {
/*<          IWS = NW >*/
//        iws = nw;
/*<       END IF >*/
    }

/*<       IF( NB.LT.NBMIN .OR. NB.GE.K ) THEN >*/
    if (nb < nbmin || nb >= *k) {

/*        Use unblocked code */

/*<    >*/
        zunm2r_(side, trans, m, n, k, &a[a_offset], lda, &tau[1], &c__[
                c_offset], ldc, &work[1], &iinfo, (ftnlen)1, (ftnlen)1);
/*<       ELSE >*/
    } else {

/*        Use blocked code */

/*<    >*/
        if ((left && ! notran) || (! left && notran)) {
/*<             I1 = 1 >*/
            i1 = 1;
/*<             I2 = K >*/
            i2 = *k;
/*<             I3 = NB >*/
            i3 = nb;
/*<          ELSE >*/
        } else {
/*<             I1 = ( ( K-1 ) / NB )*NB + 1 >*/
            i1 = (*k - 1) / nb * nb + 1;
/*<             I2 = 1 >*/
            i2 = 1;
/*<             I3 = -NB >*/
            i3 = -nb;
/*<          END IF >*/
        }

/*<          IF( LEFT ) THEN >*/
        if (left) {
/*<             NI = N >*/
            ni = *n;
/*<             JC = 1 >*/
            jc = 1;
/*<          ELSE >*/
        } else {
/*<             MI = M >*/
            mi = *m;
/*<             IC = 1 >*/
            ic = 1;
/*<          END IF >*/
        }

/*<          DO 10 I = I1, I2, I3 >*/
        i__1 = i2;
        i__2 = i3;
        for (i__ = i1; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
/*<             IB = MIN( NB, K-I+1 ) >*/
/* Computing MIN */
            i__4 = nb, i__5 = *k - i__ + 1;
            ib = min(i__4,i__5);

/*           Form the triangular factor of the block reflector */
/*           H = H(i) H(i+1) . . . H(i+ib-1) */

/*<    >*/
            i__4 = nq - i__ + 1;
            zlarft_("Forward", "Columnwise", &i__4, &ib, &a[i__ + i__ *
                    a_dim1], lda, &tau[i__], t, &c__65, (ftnlen)7, (ftnlen)10)
                    ;
/*<             IF( LEFT ) THEN >*/
            if (left) {

/*              H or H' is applied to C(i:m,1:n) */

/*<                MI = M - I + 1 >*/
                mi = *m - i__ + 1;
/*<                IC = I >*/
                ic = i__;
/*<             ELSE >*/
            } else {

/*              H or H' is applied to C(1:m,i:n) */

/*<                NI = N - I + 1 >*/
                ni = *n - i__ + 1;
/*<                JC = I >*/
                jc = i__;
/*<             END IF >*/
            }

/*           Apply H or H' */

/*<    >*/
            zlarfb_(side, trans, "Forward", "Columnwise", &mi, &ni, &ib, &a[
                    i__ + i__ * a_dim1], lda, t, &c__65, &c__[ic + jc *
                    c_dim1], ldc, &work[1], &ldwork, (ftnlen)1, (ftnlen)1, (
                    ftnlen)7, (ftnlen)10);
/*<    10    CONTINUE >*/
/* L10: */
        }
/*<       END IF >*/
    }
/*<       WORK( 1 ) = LWKOPT >*/
    work[1].r = (doublereal) lwkopt, work[1].i = 0.;
/*<       RETURN >*/
    return 0;

/*     End of ZUNMQR */

/*<       END >*/
} /* zunmqr_ */

#ifdef __cplusplus
        }
#endif
