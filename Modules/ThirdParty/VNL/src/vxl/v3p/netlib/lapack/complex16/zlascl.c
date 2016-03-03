/* lapack/complex16/zlascl.f -- translated by f2c (version 20050501).
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

/*<       SUBROUTINE ZLASCL( TYPE, KL, KU, CFROM, CTO, M, N, A, LDA, INFO ) >*/
/* Subroutine */ int zlascl_(char *type__, integer *kl, integer *ku,
        doublereal *cfrom, doublereal *cto, integer *m, integer *n,
        doublecomplex *a, integer *lda, integer *info, ftnlen type_len)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4, i__5;
    doublecomplex z__1;

    /* Local variables */
    integer i__, j, k1, k2, k3, k4;
    doublereal mul, cto1;
    logical done;
    doublereal ctoc;
    extern logical lsame_(const char *, const char *, ftnlen, ftnlen);
    integer itype;
    doublereal cfrom1;
    extern doublereal dlamch_(char *, ftnlen);
    doublereal cfromc;
    extern /* Subroutine */ int xerbla_(char *, integer *, ftnlen);
    doublereal bignum, smlnum;
    (void)type_len;

/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     February 29, 1992 */

/*     .. Scalar Arguments .. */
/*<       CHARACTER          TYPE >*/
/*<       INTEGER            INFO, KL, KU, LDA, M, N >*/
/*<       DOUBLE PRECISION   CFROM, CTO >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       COMPLEX*16         A( LDA, * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZLASCL multiplies the M by N complex matrix A by the real scalar */
/*  CTO/CFROM.  This is done without over/underflow as long as the final */
/*  result CTO*A(I,J)/CFROM does not over/underflow. TYPE specifies that */
/*  A may be full, upper triangular, lower triangular, upper Hessenberg, */
/*  or banded. */

/*  Arguments */
/*  ========= */

/*  TYPE    (input) CHARACTER*1 */
/*          TYPE indices the storage type of the input matrix. */
/*          = 'G':  A is a full matrix. */
/*          = 'L':  A is a lower triangular matrix. */
/*          = 'U':  A is an upper triangular matrix. */
/*          = 'H':  A is an upper Hessenberg matrix. */
/*          = 'B':  A is a symmetric band matrix with lower bandwidth KL */
/*                  and upper bandwidth KU and with the only the lower */
/*                  half stored. */
/*          = 'Q':  A is a symmetric band matrix with lower bandwidth KL */
/*                  and upper bandwidth KU and with the only the upper */
/*                  half stored. */
/*          = 'Z':  A is a band matrix with lower bandwidth KL and upper */
/*                  bandwidth KU. */

/*  KL      (input) INTEGER */
/*          The lower bandwidth of A.  Referenced only if TYPE = 'B', */
/*          'Q' or 'Z'. */

/*  KU      (input) INTEGER */
/*          The upper bandwidth of A.  Referenced only if TYPE = 'B', */
/*          'Q' or 'Z'. */

/*  CFROM   (input) DOUBLE PRECISION */
/*  CTO     (input) DOUBLE PRECISION */
/*          The matrix A is multiplied by CTO/CFROM. A(I,J) is computed */
/*          without over/underflow if the final result CTO*A(I,J)/CFROM */
/*          can be represented without over/underflow.  CFROM must be */
/*          nonzero. */

/*  M       (input) INTEGER */
/*          The number of rows of the matrix A.  M >= 0. */

/*  N       (input) INTEGER */
/*          The number of columns of the matrix A.  N >= 0. */

/*  A       (input/output) COMPLEX*16 array, dimension (LDA,M) */
/*          The matrix to be multiplied by CTO/CFROM.  See TYPE for the */
/*          storage type. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A.  LDA >= max(1,M). */

/*  INFO    (output) INTEGER */
/*          0  - successful exit */
/*          <0 - if INFO = -i, the i-th argument had an illegal value. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO, ONE >*/
/*<       PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       LOGICAL            DONE >*/
/*<       INTEGER            I, ITYPE, J, K1, K2, K3, K4 >*/
/*<       DOUBLE PRECISION   BIGNUM, CFROM1, CFROMC, CTO1, CTOC, MUL, SMLNUM >*/
/*     .. */
/*     .. External Functions .. */
/*<       LOGICAL            LSAME >*/
/*<       DOUBLE PRECISION   DLAMCH >*/
/*<       EXTERNAL           LSAME, DLAMCH >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, MAX, MIN >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           XERBLA >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Test the input arguments */

/*<       INFO = 0 >*/
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;

    /* Function Body */
    *info = 0;

/*<       IF( LSAME( TYPE, 'G' ) ) THEN >*/
    if (lsame_(type__, "G", (ftnlen)1, (ftnlen)1)) {
/*<          ITYPE = 0 >*/
        itype = 0;
/*<       ELSE IF( LSAME( TYPE, 'L' ) ) THEN >*/
    } else if (lsame_(type__, "L", (ftnlen)1, (ftnlen)1)) {
/*<          ITYPE = 1 >*/
        itype = 1;
/*<       ELSE IF( LSAME( TYPE, 'U' ) ) THEN >*/
    } else if (lsame_(type__, "U", (ftnlen)1, (ftnlen)1)) {
/*<          ITYPE = 2 >*/
        itype = 2;
/*<       ELSE IF( LSAME( TYPE, 'H' ) ) THEN >*/
    } else if (lsame_(type__, "H", (ftnlen)1, (ftnlen)1)) {
/*<          ITYPE = 3 >*/
        itype = 3;
/*<       ELSE IF( LSAME( TYPE, 'B' ) ) THEN >*/
    } else if (lsame_(type__, "B", (ftnlen)1, (ftnlen)1)) {
/*<          ITYPE = 4 >*/
        itype = 4;
/*<       ELSE IF( LSAME( TYPE, 'Q' ) ) THEN >*/
    } else if (lsame_(type__, "Q", (ftnlen)1, (ftnlen)1)) {
/*<          ITYPE = 5 >*/
        itype = 5;
/*<       ELSE IF( LSAME( TYPE, 'Z' ) ) THEN >*/
    } else if (lsame_(type__, "Z", (ftnlen)1, (ftnlen)1)) {
/*<          ITYPE = 6 >*/
        itype = 6;
/*<       ELSE >*/
    } else {
/*<          ITYPE = -1 >*/
        itype = -1;
/*<       END IF >*/
    }

/*<       IF( ITYPE.EQ.-1 ) THEN >*/
    if (itype == -1) {
/*<          INFO = -1 >*/
        *info = -1;
/*<       ELSE IF( CFROM.EQ.ZERO ) THEN >*/
    } else if (*cfrom == 0.) {
/*<          INFO = -4 >*/
        *info = -4;
/*<       ELSE IF( M.LT.0 ) THEN >*/
    } else if (*m < 0) {
/*<          INFO = -6 >*/
        *info = -6;
/*<    >*/
    } else if (*n < 0 || (itype == 4 && *n != *m) || (itype == 5 && *n != *m)) {
/*<          INFO = -7 >*/
        *info = -7;
/*<       ELSE IF( ITYPE.LE.3 .AND. LDA.LT.MAX( 1, M ) ) THEN >*/
    } else if (itype <= 3 && *lda < max(1,*m)) {
/*<          INFO = -9 >*/
        *info = -9;
/*<       ELSE IF( ITYPE.GE.4 ) THEN >*/
    } else if (itype >= 4) {
/*<          IF( KL.LT.0 .OR. KL.GT.MAX( M-1, 0 ) ) THEN >*/
/* Computing MAX */
        i__1 = *m - 1;
        if (*kl < 0 || *kl > max(i__1,0)) {
/*<             INFO = -2 >*/
            *info = -2;
/*<    >*/
        } else /* if(complicated condition) */ {
/* Computing MAX */
            i__1 = *n - 1;
            if (*ku < 0 || *ku > max(i__1,0) || ((itype == 4 || itype == 5) &&
                    *kl != *ku)) {
/*<             INFO = -3 >*/
                *info = -3;
/*<    >*/
            } else if ((itype == 4 && *lda < *kl + 1) || (itype == 5 && *lda < *
                    ku + 1) || (itype == 6 && *lda < (*kl << 1) + *ku + 1)) {
/*<             INFO = -9 >*/
                *info = -9;
/*<          END IF >*/
            }
        }
/*<       END IF >*/
    }

/*<       IF( INFO.NE.0 ) THEN >*/
    if (*info != 0) {
/*<          CALL XERBLA( 'ZLASCL', -INFO ) >*/
        i__1 = -(*info);
        xerbla_("ZLASCL", &i__1, (ftnlen)6);
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*     Quick return if possible */

/*<    >*/
    if (*n == 0 || *m == 0) {
        return 0;
    }

/*     Get machine parameters */

/*<       SMLNUM = DLAMCH( 'S' ) >*/
    smlnum = dlamch_("S", (ftnlen)1);
/*<       BIGNUM = ONE / SMLNUM >*/
    bignum = 1. / smlnum;

/*<       CFROMC = CFROM >*/
    cfromc = *cfrom;
/*<       CTOC = CTO >*/
    ctoc = *cto;

/*<    10 CONTINUE >*/
L10:
/*<       CFROM1 = CFROMC*SMLNUM >*/
    cfrom1 = cfromc * smlnum;
/*<       CTO1 = CTOC / BIGNUM >*/
    cto1 = ctoc / bignum;
/*<       IF( ABS( CFROM1 ).GT.ABS( CTOC ) .AND. CTOC.NE.ZERO ) THEN >*/
    if (abs(cfrom1) > abs(ctoc) && ctoc != 0.) {
/*<          MUL = SMLNUM >*/
        mul = smlnum;
/*<          DONE = .FALSE. >*/
        done = FALSE_;
/*<          CFROMC = CFROM1 >*/
        cfromc = cfrom1;
/*<       ELSE IF( ABS( CTO1 ).GT.ABS( CFROMC ) ) THEN >*/
    } else if (abs(cto1) > abs(cfromc)) {
/*<          MUL = BIGNUM >*/
        mul = bignum;
/*<          DONE = .FALSE. >*/
        done = FALSE_;
/*<          CTOC = CTO1 >*/
        ctoc = cto1;
/*<       ELSE >*/
    } else {
/*<          MUL = CTOC / CFROMC >*/
        mul = ctoc / cfromc;
/*<          DONE = .TRUE. >*/
        done = TRUE_;
/*<       END IF >*/
    }

/*<       IF( ITYPE.EQ.0 ) THEN >*/
    if (itype == 0) {

/*        Full matrix */

/*<          DO 30 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<             DO 20 I = 1, M >*/
            i__2 = *m;
            for (i__ = 1; i__ <= i__2; ++i__) {
/*<                A( I, J ) = A( I, J )*MUL >*/
                i__3 = i__ + j * a_dim1;
                i__4 = i__ + j * a_dim1;
                z__1.r = mul * a[i__4].r, z__1.i = mul * a[i__4].i;
                a[i__3].r = z__1.r, a[i__3].i = z__1.i;
/*<    20       CONTINUE >*/
/* L20: */
            }
/*<    30    CONTINUE >*/
/* L30: */
        }

/*<       ELSE IF( ITYPE.EQ.1 ) THEN >*/
    } else if (itype == 1) {

/*        Lower triangular matrix */

/*<          DO 50 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<             DO 40 I = J, M >*/
            i__2 = *m;
            for (i__ = j; i__ <= i__2; ++i__) {
/*<                A( I, J ) = A( I, J )*MUL >*/
                i__3 = i__ + j * a_dim1;
                i__4 = i__ + j * a_dim1;
                z__1.r = mul * a[i__4].r, z__1.i = mul * a[i__4].i;
                a[i__3].r = z__1.r, a[i__3].i = z__1.i;
/*<    40       CONTINUE >*/
/* L40: */
            }
/*<    50    CONTINUE >*/
/* L50: */
        }

/*<       ELSE IF( ITYPE.EQ.2 ) THEN >*/
    } else if (itype == 2) {

/*        Upper triangular matrix */

/*<          DO 70 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<             DO 60 I = 1, MIN( J, M ) >*/
            i__2 = min(j,*m);
            for (i__ = 1; i__ <= i__2; ++i__) {
/*<                A( I, J ) = A( I, J )*MUL >*/
                i__3 = i__ + j * a_dim1;
                i__4 = i__ + j * a_dim1;
                z__1.r = mul * a[i__4].r, z__1.i = mul * a[i__4].i;
                a[i__3].r = z__1.r, a[i__3].i = z__1.i;
/*<    60       CONTINUE >*/
/* L60: */
            }
/*<    70    CONTINUE >*/
/* L70: */
        }

/*<       ELSE IF( ITYPE.EQ.3 ) THEN >*/
    } else if (itype == 3) {

/*        Upper Hessenberg matrix */

/*<          DO 90 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<             DO 80 I = 1, MIN( J+1, M ) >*/
/* Computing MIN */
            i__3 = j + 1;
            i__2 = min(i__3,*m);
            for (i__ = 1; i__ <= i__2; ++i__) {
/*<                A( I, J ) = A( I, J )*MUL >*/
                i__3 = i__ + j * a_dim1;
                i__4 = i__ + j * a_dim1;
                z__1.r = mul * a[i__4].r, z__1.i = mul * a[i__4].i;
                a[i__3].r = z__1.r, a[i__3].i = z__1.i;
/*<    80       CONTINUE >*/
/* L80: */
            }
/*<    90    CONTINUE >*/
/* L90: */
        }

/*<       ELSE IF( ITYPE.EQ.4 ) THEN >*/
    } else if (itype == 4) {

/*        Lower half of a symmetric band matrix */

/*<          K3 = KL + 1 >*/
        k3 = *kl + 1;
/*<          K4 = N + 1 >*/
        k4 = *n + 1;
/*<          DO 110 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<             DO 100 I = 1, MIN( K3, K4-J ) >*/
/* Computing MIN */
            i__3 = k3, i__4 = k4 - j;
            i__2 = min(i__3,i__4);
            for (i__ = 1; i__ <= i__2; ++i__) {
/*<                A( I, J ) = A( I, J )*MUL >*/
                i__3 = i__ + j * a_dim1;
                i__4 = i__ + j * a_dim1;
                z__1.r = mul * a[i__4].r, z__1.i = mul * a[i__4].i;
                a[i__3].r = z__1.r, a[i__3].i = z__1.i;
/*<   100       CONTINUE >*/
/* L100: */
            }
/*<   110    CONTINUE >*/
/* L110: */
        }

/*<       ELSE IF( ITYPE.EQ.5 ) THEN >*/
    } else if (itype == 5) {

/*        Upper half of a symmetric band matrix */

/*<          K1 = KU + 2 >*/
        k1 = *ku + 2;
/*<          K3 = KU + 1 >*/
        k3 = *ku + 1;
/*<          DO 130 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<             DO 120 I = MAX( K1-J, 1 ), K3 >*/
/* Computing MAX */
            i__2 = k1 - j;
            i__3 = k3;
            for (i__ = max(i__2,1); i__ <= i__3; ++i__) {
/*<                A( I, J ) = A( I, J )*MUL >*/
                i__2 = i__ + j * a_dim1;
                i__4 = i__ + j * a_dim1;
                z__1.r = mul * a[i__4].r, z__1.i = mul * a[i__4].i;
                a[i__2].r = z__1.r, a[i__2].i = z__1.i;
/*<   120       CONTINUE >*/
/* L120: */
            }
/*<   130    CONTINUE >*/
/* L130: */
        }

/*<       ELSE IF( ITYPE.EQ.6 ) THEN >*/
    } else if (itype == 6) {

/*        Band matrix */

/*<          K1 = KL + KU + 2 >*/
        k1 = *kl + *ku + 2;
/*<          K2 = KL + 1 >*/
        k2 = *kl + 1;
/*<          K3 = 2*KL + KU + 1 >*/
        k3 = (*kl << 1) + *ku + 1;
/*<          K4 = KL + KU + 1 + M >*/
        k4 = *kl + *ku + 1 + *m;
/*<          DO 150 J = 1, N >*/
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
/*<             DO 140 I = MAX( K1-J, K2 ), MIN( K3, K4-J ) >*/
/* Computing MAX */
            i__3 = k1 - j;
/* Computing MIN */
            i__4 = k3, i__5 = k4 - j;
            i__2 = min(i__4,i__5);
            for (i__ = max(i__3,k2); i__ <= i__2; ++i__) {
/*<                A( I, J ) = A( I, J )*MUL >*/
                i__3 = i__ + j * a_dim1;
                i__4 = i__ + j * a_dim1;
                z__1.r = mul * a[i__4].r, z__1.i = mul * a[i__4].i;
                a[i__3].r = z__1.r, a[i__3].i = z__1.i;
/*<   140       CONTINUE >*/
/* L140: */
            }
/*<   150    CONTINUE >*/
/* L150: */
        }

/*<       END IF >*/
    }

/*<    >*/
    if (! done) {
        goto L10;
    }

/*<       RETURN >*/
    return 0;

/*     End of ZLASCL */

/*<       END >*/
} /* zlascl_ */

#ifdef __cplusplus
        }
#endif
