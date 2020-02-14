/* lapack/complex16/ztrexc.f -- translated by f2c (version 20090411).
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

/*<       SUBROUTINE ZTREXC( COMPQ, N, T, LDT, Q, LDQ, IFST, ILST, INFO ) >*/
/* Subroutine */ int ztrexc_(char *compq, integer *n, doublecomplex *t,
        integer *ldt, doublecomplex *q, integer *ldq, integer *ifst, integer *
        ilst, integer *info, ftnlen compq_len)
{
    /* System generated locals */
    integer q_dim1, q_offset, t_dim1, t_offset, i__1, i__2, i__3;
    doublecomplex z__1;

    /* Builtin functions */
    void d_cnjg(doublecomplex *, doublecomplex *);

    /* Local variables */
    integer k, m1, m2, m3;
    doublereal cs;
    doublecomplex t11, t22, sn, temp;
    extern /* Subroutine */ int zrot_(integer *, doublecomplex *, integer *,
            doublecomplex *, integer *, doublereal *, doublecomplex *);
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    logical wantq;
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen), zlartg_(
            doublecomplex *, doublecomplex *, doublereal *, doublecomplex *,
            doublecomplex *);
     (void)compq_len;

/*  -- LAPACK routine (version 3.2) -- */
/*  -- LAPACK is a software package provided by Univ. of Tennessee,    -- */
/*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..-- */
/*     November 2006 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          COMPQ >*/
/*<       INTEGER            IFST, ILST, INFO, LDQ, LDT, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       COMPLEX*16         Q( LDQ, * ), T( LDT, * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZTREXC reorders the Schur factorization of a complex matrix */
/*  A = Q*T*Q**H, so that the diagonal element of T with row index IFST */
/*  is moved to row ILST. */

/*  The Schur form T is reordered by a unitary similarity transformation */
/*  Z**H*T*Z, and optionally the matrix Q of Schur vectors is updated by */
/*  postmultplying it with Z. */

/*  Arguments */
/*  ========= */

/*  COMPQ   (input) CHARACTER*1 */
/*          = 'V':  update the matrix Q of Schur vectors; */
/*          = 'N':  do not update Q. */

/*  N       (input) INTEGER */
/*          The order of the matrix T. N >= 0. */

/*  T       (input/output) COMPLEX*16 array, dimension (LDT,N) */
/*          On entry, the upper triangular matrix T. */
/*          On exit, the reordered upper triangular matrix. */

/*  LDT     (input) INTEGER */
/*          The leading dimension of the array T. LDT >= max(1,N). */

/*  Q       (input/output) COMPLEX*16 array, dimension (LDQ,N) */
/*          On entry, if COMPQ = 'V', the matrix Q of Schur vectors. */
/*          On exit, if COMPQ = 'V', Q has been postmultiplied by the */
/*          unitary transformation matrix Z which reorders T. */
/*          If COMPQ = 'N', Q is not referenced. */

/*  LDQ     (input) INTEGER */
/*          The leading dimension of the array Q.  LDQ >= max(1,N). */

/*  IFST    (input) INTEGER */
/*  ILST    (input) INTEGER */
/*          Specify the reordering of the diagonal elements of T: */
/*          The element with row index IFST is moved to row ILST by a */
/*          sequence of transpositions between adjacent elements. */
/*          1 <= IFST <= N; 1 <= ILST <= N. */

/*  INFO    (output) INTEGER */
/*          = 0:  successful exit */
/*          < 0:  if INFO = -i, the i-th argument had an illegal value */

/*  ===================================================================== */

/*     .. Local Scalars .. */
/*<       LOGICAL            WANTQ >*/
/*<       INTEGER            K, M1, M2, M3 >*/
/*<       DOUBLE PRECISION   CS >*/
/*<       COMPLEX*16         SN, T11, T22, TEMP >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       EXTERNAL           LSAME >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           XERBLA, ZLARTG, ZROT >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          DCONJG, MAX >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Decode and test the input parameters. */

/*<       INFO = 0 >*/
    /* Parameter adjustments */
    t_dim1 = *ldt;
    t_offset = 1 + t_dim1;
    t -= t_offset;
    q_dim1 = *ldq;
    q_offset = 1 + q_dim1;
    q -= q_offset;

    /* Function Body */
    *info = 0;
/*<       WANTQ = LSAME( COMPQ, 'V' ) >*/
    wantq = lsame_(compq, "V", (ftnlen)1, (ftnlen)1);
/*<       IF( .NOT.LSAME( COMPQ, 'N' ) .AND. .NOT.WANTQ ) THEN >*/
    if (! lsame_(compq, "N", (ftnlen)1, (ftnlen)1) && ! wantq) {
/*<          INFO = -1 >*/
        *info = -1;
/*<       ELSE IF( N.LT.0 ) THEN >*/
    } else if (*n < 0) {
/*<          INFO = -2 >*/
        *info = -2;
/*<       ELSE IF( LDT.LT.MAX( 1, N ) ) THEN >*/
    } else if (*ldt < max(1,*n)) {
/*<          INFO = -4 >*/
        *info = -4;
/*<       ELSE IF( LDQ.LT.1 .OR. ( WANTQ .AND. LDQ.LT.MAX( 1, N ) ) ) THEN >*/
    } else if (*ldq < 1 || (wantq && *ldq < max(1,*n))) {
/*<          INFO = -6 >*/
        *info = -6;
/*<       ELSE IF( IFST.LT.1 .OR. IFST.GT.N ) THEN >*/
    } else if (*ifst < 1 || *ifst > *n) {
/*<          INFO = -7 >*/
        *info = -7;
/*<       ELSE IF( ILST.LT.1 .OR. ILST.GT.N ) THEN >*/
    } else if (*ilst < 1 || *ilst > *n) {
/*<          INFO = -8 >*/
        *info = -8;
/*<       END IF >*/
    }
/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'ZTREXC', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("ZTREXC", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible */

/*<    >*/
    if (*n == 1 || *ifst == *ilst) {
        return 0;
    }

/*<       IF( IFST.LT.ILST ) THEN >*/
    if (*ifst < *ilst) {

/*        Move the IFST-th diagonal element forward down the diagonal. */

/*<          M1 = 0 >*/
        m1 = 0;
/*<          M2 = -1 >*/
        m2 = -1;
/*<          M3 = 1 >*/
        m3 = 1;
/*<       ELSE >*/
    } else {

/*        Move the IFST-th diagonal element backward up the diagonal. */

/*<          M1 = -1 >*/
        m1 = -1;
/*<          M2 = 0 >*/
        m2 = 0;
/*<          M3 = -1 >*/
        m3 = -1;
/*<       END IF >*/
    }

/*<       DO 10 K = IFST + M1, ILST + M2, M3 >*/
    i__1 = *ilst + m2;
    i__2 = m3;
    for (k = *ifst + m1; i__2 < 0 ? k >= i__1 : k <= i__1; k += i__2) {

/*        Interchange the k-th and (k+1)-th diagonal elements. */

/*<          T11 = T( K, K ) >*/
        i__3 = k + k * t_dim1;
        t11.r = t[i__3].r, t11.i = t[i__3].i;
/*<          T22 = T( K+1, K+1 ) >*/
        i__3 = k + 1 + (k + 1) * t_dim1;
        t22.r = t[i__3].r, t22.i = t[i__3].i;

/*        Determine the transformation to perform the interchange. */

/*<          CALL ZLARTG( T( K, K+1 ), T22-T11, CS, SN, TEMP ) >*/
        z__1.r = t22.r - t11.r, z__1.i = t22.i - t11.i;
        zlartg_(&t[k + (k + 1) * t_dim1], &z__1, &cs, &sn, &temp);

/*        Apply transformation to the matrix T. */

/*<    >*/
        if (k + 2 <= *n) {
            i__3 = *n - k - 1;
            zrot_(&i__3, &t[k + (k + 2) * t_dim1], ldt, &t[k + 1 + (k + 2) *
                    t_dim1], ldt, &cs, &sn);
        }
/*<    >*/
        i__3 = k - 1;
        d_cnjg(&z__1, &sn);
        zrot_(&i__3, &t[k * t_dim1 + 1], &c__1, &t[(k + 1) * t_dim1 + 1], &
                c__1, &cs, &z__1);

/*<          T( K, K ) = T22 >*/
        i__3 = k + k * t_dim1;
        t[i__3].r = t22.r, t[i__3].i = t22.i;
/*<          T( K+1, K+1 ) = T11 >*/
        i__3 = k + 1 + (k + 1) * t_dim1;
        t[i__3].r = t11.r, t[i__3].i = t11.i;

/*<          IF( WANTQ ) THEN >*/
        if (wantq) {

/*           Accumulate transformation in the matrix Q. */

/*<    >*/
            d_cnjg(&z__1, &sn);
            zrot_(n, &q[k * q_dim1 + 1], &c__1, &q[(k + 1) * q_dim1 + 1], &
                    c__1, &cs, &z__1);
/*<          END IF >*/
        }

/*<    10 CONTINUE >*/
/* L10: */
    }

/*<       RETURN >*/
    return 0;

/*     End of ZTREXC */

/*<       END >*/
} /* ztrexc_ */

#ifdef __cplusplus
        }
#endif
