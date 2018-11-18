/* lapack/complex16/zgesc2.f -- translated by f2c (version 20090411).
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
static doublecomplex c_b13 = {1.,0.};
static integer c_n1 = -1;

/*<       SUBROUTINE ZGESC2( N, A, LDA, RHS, IPIV, JPIV, SCALE ) >*/
/* Subroutine */ int zgesc2_(integer *n, doublecomplex *a, integer *lda,
        doublecomplex *rhs, integer *ipiv, integer *jpiv, doublereal *scale)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4, i__5, i__6;
    doublereal d__1;
    doublecomplex z__1, z__2, z__3;

    /* Builtin functions */
    double z_abs(doublecomplex *);
    void z_div(doublecomplex *, doublecomplex *, doublecomplex *);

    /* Local variables */
    integer i__, j;
    doublereal eps;
    doublecomplex temp;
    extern /* Subroutine */ int zscal_(integer *, doublecomplex *,
            doublecomplex *, integer *), dlabad_(doublereal *, doublereal *);
    extern doublereal dlamch_(char *, ftnlen);
    doublereal bignum;
    extern integer izamax_(integer *, doublecomplex *, integer *);
    doublereal smlnum;
    extern /* Subroutine */ int zlaswp_(integer *, doublecomplex *, integer *,
             integer *, integer *, integer *, integer *);


/*  -- LAPACK auxiliary routine (version 3.2) -- */
/*  -- LAPACK is a software package provided by Univ. of Tennessee,    -- */
/*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..-- */
/*     November 2006 */

/*     .. Scalar Arguments .. */
/*<       INTEGER            LDA, N >*/
/*<       DOUBLE PRECISION   SCALE >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       INTEGER            IPIV( * ), JPIV( * ) >*/
/*<       COMPLEX*16         A( LDA, * ), RHS( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZGESC2 solves a system of linear equations */

/*            A * X = scale* RHS */

/*  with a general N-by-N matrix A using the LU factorization with */
/*  complete pivoting computed by ZGETC2. */


/*  Arguments */
/*  ========= */

/*  N       (input) INTEGER */
/*          The number of columns of the matrix A. */

/*  A       (input) COMPLEX*16 array, dimension (LDA, N) */
/*          On entry, the  LU part of the factorization of the n-by-n */
/*          matrix A computed by ZGETC2:  A = P * L * U * Q */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A.  LDA >= max(1, N). */

/*  RHS     (input/output) COMPLEX*16 array, dimension N. */
/*          On entry, the right hand side vector b. */
/*          On exit, the solution vector X. */

/*  IPIV    (input) INTEGER array, dimension (N). */
/*          The pivot indices; for 1 <= i <= N, row i of the */
/*          matrix has been interchanged with row IPIV(i). */

/*  JPIV    (input) INTEGER array, dimension (N). */
/*          The pivot indices; for 1 <= j <= N, column j of the */
/*          matrix has been interchanged with column JPIV(j). */

/*  SCALE    (output) DOUBLE PRECISION */
/*           On exit, SCALE contains the scale factor. SCALE is chosen */
/*           0 <= SCALE <= 1 to prevent owerflow in the solution. */

/*  Further Details */
/*  =============== */

/*  Based on contributions by */
/*     Bo Kagstrom and Peter Poromaa, Department of Computing Science, */
/*     Umea University, S-901 87 Umea, Sweden. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ZERO, ONE, TWO >*/
/*<       PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TWO = 2.0D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       INTEGER            I, J >*/
/*<       DOUBLE PRECISION   BIGNUM, EPS, SMLNUM >*/
/*<       COMPLEX*16         TEMP >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           ZLASWP, ZSCAL >*/
/*     .. */
/*     .. External Functions .. */
/*<       INTEGER            IZAMAX >*/
/*<       DOUBLE PRECISION   DLAMCH >*/
/*<       EXTERNAL           IZAMAX, DLAMCH >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, DBLE, DCMPLX >*/
/*     .. */
/*     .. Executable Statements .. */

/*     Set constant to control overflow */

/*<       EPS = DLAMCH( 'P' ) >*/
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --rhs;
    --ipiv;
    --jpiv;

    /* Function Body */
    eps = dlamch_("P", (ftnlen)1);
/*<       SMLNUM = DLAMCH( 'S' ) / EPS >*/
    smlnum = dlamch_("S", (ftnlen)1) / eps;
/*<       BIGNUM = ONE / SMLNUM >*/
    bignum = 1. / smlnum;
/*<       CALL DLABAD( SMLNUM, BIGNUM ) >*/
    dlabad_(&smlnum, &bignum);

/*     Apply permutations IPIV to RHS */

/*<       CALL ZLASWP( 1, RHS, LDA, 1, N-1, IPIV, 1 ) >*/
    i__1 = *n - 1;
    zlaswp_(&c__1, &rhs[1], lda, &c__1, &i__1, &ipiv[1], &c__1);

/*     Solve for L part */

/*<       DO 20 I = 1, N - 1 >*/
    i__1 = *n - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          DO 10 J = I + 1, N >*/
        i__2 = *n;
        for (j = i__ + 1; j <= i__2; ++j) {
/*<             RHS( J ) = RHS( J ) - A( J, I )*RHS( I ) >*/
            i__3 = j;
            i__4 = j;
            i__5 = j + i__ * a_dim1;
            i__6 = i__;
            z__2.r = a[i__5].r * rhs[i__6].r - a[i__5].i * rhs[i__6].i,
                    z__2.i = a[i__5].r * rhs[i__6].i + a[i__5].i * rhs[i__6]
                    .r;
            z__1.r = rhs[i__4].r - z__2.r, z__1.i = rhs[i__4].i - z__2.i;
            rhs[i__3].r = z__1.r, rhs[i__3].i = z__1.i;
/*<    10    CONTINUE >*/
/* L10: */
        }
/*<    20 CONTINUE >*/
/* L20: */
    }

/*     Solve for U part */

/*<       SCALE = ONE >*/
    *scale = 1.;

/*     Check for scaling */

/*<       I = IZAMAX( N, RHS, 1 ) >*/
    i__ = izamax_(n, &rhs[1], &c__1);
/*<       IF( TWO*SMLNUM*ABS( RHS( I ) ).GT.ABS( A( N, N ) ) ) THEN >*/
    if (smlnum * 2. * z_abs(&rhs[i__]) > z_abs(&a[*n + *n * a_dim1])) {
/*<          TEMP = DCMPLX( ONE / TWO, ZERO ) / ABS( RHS( I ) ) >*/
        d__1 = z_abs(&rhs[i__]);
        z__1.r = .5 / d__1, z__1.i = 0. / d__1;
        temp.r = z__1.r, temp.i = z__1.i;
/*<          CALL ZSCAL( N, TEMP, RHS( 1 ), 1 ) >*/
        zscal_(n, &temp, &rhs[1], &c__1);
/*<          SCALE = SCALE*DBLE( TEMP ) >*/
        *scale *= temp.r;
/*<       END IF >*/
    }
/*<       DO 40 I = N, 1, -1 >*/
    for (i__ = *n; i__ >= 1; --i__) {
/*<          TEMP = DCMPLX( ONE, ZERO ) / A( I, I ) >*/
        z_div(&z__1, &c_b13, &a[i__ + i__ * a_dim1]);
        temp.r = z__1.r, temp.i = z__1.i;
/*<          RHS( I ) = RHS( I )*TEMP >*/
        i__1 = i__;
        i__2 = i__;
        z__1.r = rhs[i__2].r * temp.r - rhs[i__2].i * temp.i, z__1.i = rhs[
                i__2].r * temp.i + rhs[i__2].i * temp.r;
        rhs[i__1].r = z__1.r, rhs[i__1].i = z__1.i;
/*<          DO 30 J = I + 1, N >*/
        i__1 = *n;
        for (j = i__ + 1; j <= i__1; ++j) {
/*<             RHS( I ) = RHS( I ) - RHS( J )*( A( I, J )*TEMP ) >*/
            i__2 = i__;
            i__3 = i__;
            i__4 = j;
            i__5 = i__ + j * a_dim1;
            z__3.r = a[i__5].r * temp.r - a[i__5].i * temp.i, z__3.i = a[i__5]
                    .r * temp.i + a[i__5].i * temp.r;
            z__2.r = rhs[i__4].r * z__3.r - rhs[i__4].i * z__3.i, z__2.i =
                    rhs[i__4].r * z__3.i + rhs[i__4].i * z__3.r;
            z__1.r = rhs[i__3].r - z__2.r, z__1.i = rhs[i__3].i - z__2.i;
            rhs[i__2].r = z__1.r, rhs[i__2].i = z__1.i;
/*<    30    CONTINUE >*/
/* L30: */
        }
/*<    40 CONTINUE >*/
/* L40: */
    }

/*     Apply permutations JPIV to the solution (RHS) */

/*<       CALL ZLASWP( 1, RHS, LDA, 1, N-1, JPIV, -1 ) >*/
    i__1 = *n - 1;
    zlaswp_(&c__1, &rhs[1], lda, &c__1, &i__1, &jpiv[1], &c_n1);
/*<       RETURN >*/
    return 0;

/*     End of ZGESC2 */

/*<       END >*/
} /* zgesc2_ */

#ifdef __cplusplus
        }
#endif
