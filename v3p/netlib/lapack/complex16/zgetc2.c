/* lapack/complex16/zgetc2.f -- translated by f2c (version 20090411).
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
static doublecomplex c_b10 = {-1.,-0.};

/*<       SUBROUTINE ZGETC2( N, A, LDA, IPIV, JPIV, INFO ) >*/
/* Subroutine */ int zgetc2_(integer *n, doublecomplex *a, integer *lda,
        integer *ipiv, integer *jpiv, integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    doublereal d__1;
    doublecomplex z__1;

    /* Builtin functions */
    double z_abs(doublecomplex *);
    void z_div(doublecomplex *, doublecomplex *, doublecomplex *);

    /* Local variables */
    integer i__, j, ip, jp;
    doublereal eps;
    integer ipv, jpv;
    doublereal smin, xmax;
    extern /* Subroutine */ int zgeru_(integer *, integer *, doublecomplex *,
            doublecomplex *, integer *, doublecomplex *, integer *,
            doublecomplex *, integer *), zswap_(integer *, doublecomplex *,
            integer *, doublecomplex *, integer *), dlabad_(doublereal *,
            doublereal *);
    extern doublereal dlamch_(char *, ftnlen);
    doublereal bignum, smlnum;


/*  -- LAPACK auxiliary routine (version 3.2) -- */
/*  -- LAPACK is a software package provided by Univ. of Tennessee,    -- */
/*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..-- */
/*     November 2006 */

/*     .. Scalar Arguments .. */
/*<       INTEGER            INFO, LDA, N >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       INTEGER            IPIV( * ), JPIV( * ) >*/
/*<       COMPLEX*16         A( LDA, * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZGETC2 computes an LU factorization, using complete pivoting, of the */
/*  n-by-n matrix A. The factorization has the form A = P * L * U * Q, */
/*  where P and Q are permutation matrices, L is lower triangular with */
/*  unit diagonal elements and U is upper triangular. */

/*  This is a level 1 BLAS version of the algorithm. */

/*  Arguments */
/*  ========= */

/*  N       (input) INTEGER */
/*          The order of the matrix A. N >= 0. */

/*  A       (input/output) COMPLEX*16 array, dimension (LDA, N) */
/*          On entry, the n-by-n matrix to be factored. */
/*          On exit, the factors L and U from the factorization */
/*          A = P*L*U*Q; the unit diagonal elements of L are not stored. */
/*          If U(k, k) appears to be less than SMIN, U(k, k) is given the */
/*          value of SMIN, giving a nonsingular perturbed system. */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A.  LDA >= max(1, N). */

/*  IPIV    (output) INTEGER array, dimension (N). */
/*          The pivot indices; for 1 <= i <= N, row i of the */
/*          matrix has been interchanged with row IPIV(i). */

/*  JPIV    (output) INTEGER array, dimension (N). */
/*          The pivot indices; for 1 <= j <= N, column j of the */
/*          matrix has been interchanged with column JPIV(j). */

/*  INFO    (output) INTEGER */
/*           = 0: successful exit */
/*           > 0: if INFO = k, U(k, k) is likely to produce overflow if */
/*                one tries to solve for x in Ax = b. So U is perturbed */
/*                to avoid the overflow. */

/*  Further Details */
/*  =============== */

/*  Based on contributions by */
/*     Bo Kagstrom and Peter Poromaa, Department of Computing Science, */
/*     Umea University, S-901 87 Umea, Sweden. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO, ONE >*/
/*<       PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       INTEGER            I, IP, IPV, J, JP, JPV >*/
/*<       DOUBLE PRECISION   BIGNUM, EPS, SMIN, SMLNUM, XMAX >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           ZGERU, ZSWAP >*/
/*     .. */
/*     .. External Functions .. */
/*<       DOUBLE PRECISION   DLAMCH >*/
/*<       EXTERNAL           DLAMCH >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, DCMPLX, MAX >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Set constants to control overflow */

/*<       INFO = 0 >*/
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --ipiv;
    --jpiv;

    /* Function Body */
    *info = 0;
/*<       EPS = DLAMCH( 'P' ) >*/
    eps = dlamch_("P", (ftnlen)1);
/*<       SMLNUM = DLAMCH( 'S' ) / EPS >*/
    smlnum = dlamch_("S", (ftnlen)1) / eps;
/*<       BIGNUM = ONE / SMLNUM >*/
    bignum = 1. / smlnum;
/*<       CALL DLABAD( SMLNUM, BIGNUM ) >*/
    dlabad_(&smlnum, &bignum);

/*     Factorize A using complete pivoting. */
/*     Set pivots less than SMIN to SMIN */

/*<       DO 40 I = 1, N - 1 >*/
    i__1 = *n - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Find max element in matrix A */

/*<          XMAX = ZERO >*/
        xmax = 0.;
/*<          DO 20 IP = I, N >*/
        i__2 = *n;
        for (ip = i__; ip <= i__2; ++ip) {
/*<             DO 10 JP = I, N >*/
            i__3 = *n;
            for (jp = i__; jp <= i__3; ++jp) {
/*<                IF( ABS( A( IP, JP ) ).GE.XMAX ) THEN >*/
                if (z_abs(&a[ip + jp * a_dim1]) >= xmax) {
/*<                   XMAX = ABS( A( IP, JP ) ) >*/
                    xmax = z_abs(&a[ip + jp * a_dim1]);
/*<                   IPV = IP >*/
                    ipv = ip;
/*<                   JPV = JP >*/
                    jpv = jp;
/*<                END IF >*/
                }
/*<    10       CONTINUE >*/
/* L10: */
            }
/*<    20    CONTINUE >*/
/* L20: */
        }
/*<    >*/
        if (i__ == 1) {
/* Computing MAX */
            d__1 = eps * xmax;
            smin = max(d__1,smlnum);
        }

/*        Swap rows */

/*<    >*/
        if (ipv != i__) {
            zswap_(n, &a[ipv + a_dim1], lda, &a[i__ + a_dim1], lda);
        }
/*<          IPIV( I ) = IPV >*/
        ipiv[i__] = ipv;

/*        Swap columns */

/*<    >*/
        if (jpv != i__) {
            zswap_(n, &a[jpv * a_dim1 + 1], &c__1, &a[i__ * a_dim1 + 1], &
                    c__1);
        }
/*<          JPIV( I ) = JPV >*/
        jpiv[i__] = jpv;

/*        Check for singularity */

/*<          IF( ABS( A( I, I ) ).LT.SMIN ) THEN >*/
        if (z_abs(&a[i__ + i__ * a_dim1]) < smin) {
/*<             INFO = I >*/
            *info = i__;
/*<             A( I, I ) = DCMPLX( SMIN, ZERO ) >*/
            i__2 = i__ + i__ * a_dim1;
            z__1.r = smin, z__1.i = 0.;
            a[i__2].r = z__1.r, a[i__2].i = z__1.i;
/*<          END IF >*/
        }
/*<          DO 30 J = I + 1, N >*/
        i__2 = *n;
        for (j = i__ + 1; j <= i__2; ++j) {
/*<             A( J, I ) = A( J, I ) / A( I, I ) >*/
            i__3 = j + i__ * a_dim1;
            z_div(&z__1, &a[j + i__ * a_dim1], &a[i__ + i__ * a_dim1]);
            a[i__3].r = z__1.r, a[i__3].i = z__1.i;
/*<    30    CONTINUE >*/
/* L30: */
        }
/*<    >*/
        i__2 = *n - i__;
        i__3 = *n - i__;
        zgeru_(&i__2, &i__3, &c_b10, &a[i__ + 1 + i__ * a_dim1], &c__1, &a[
                i__ + (i__ + 1) * a_dim1], lda, &a[i__ + 1 + (i__ + 1) *
                a_dim1], lda);
/*<    40 CONTINUE >*/
/* L40: */
    }

/*<       IF( ABS( A( N, N ) ).LT.SMIN ) THEN >*/
    if (z_abs(&a[*n + *n * a_dim1]) < smin) {
/*<          INFO = N >*/
        *info = *n;
/*<          A( N, N ) = DCMPLX( SMIN, ZERO ) >*/
        i__1 = *n + *n * a_dim1;
        z__1.r = smin, z__1.i = 0.;
        a[i__1].r = z__1.r, a[i__1].i = z__1.i;
/*<       END IF >*/
    }
/*<       RETURN >*/
    return 0;

/*     End of ZGETC2 */

/*<       END >*/
} /* zgetc2_ */

#ifdef __cplusplus
        }
#endif
