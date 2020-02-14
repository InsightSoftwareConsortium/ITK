/* lapack/double/dgesc2.f -- translated by f2c (version 20050501).
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

/*<       SUBROUTINE DGESC2( N, A, LDA, RHS, IPIV, JPIV, SCALE ) >*/
/* Subroutine */ int dgesc2_(integer *n, doublereal *a, integer *lda,
        doublereal *rhs, integer *ipiv, integer *jpiv, doublereal *scale)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;
    doublereal d__1, d__2;

    /* Local variables */
    integer i__, j;
    doublereal eps, temp;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *,
            integer *), dlabad_(doublereal *, doublereal *);
    extern doublereal dlamch_(char *, ftnlen);
    extern integer idamax_(integer *, doublereal *, integer *);
    doublereal bignum;
    extern /* Subroutine */ int dlaswp_(integer *, doublereal *, integer *,
            integer *, integer *, integer *, integer *);
    doublereal smlnum;


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     June 30, 1999 */

/*     .. Scalar Arguments .. */
/*<       INTEGER            LDA, N >*/
/*<       DOUBLE PRECISION   SCALE >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       INTEGER            IPIV( * ), JPIV( * ) >*/
/*<       DOUBLE PRECISION   A( LDA, * ), RHS( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DGESC2 solves a system of linear equations */

/*            A * X = scale* RHS */

/*  with a general N-by-N matrix A using the LU factorization with */
/*  complete pivoting computed by DGETC2. */

/*  Arguments */
/*  ========= */

/*  N       (input) INTEGER */
/*          The order of the matrix A. */

/*  A       (input) DOUBLE PRECISION array, dimension (LDA,N) */
/*          On entry, the  LU part of the factorization of the n-by-n */
/*          matrix A computed by DGETC2:  A = P * L * U * Q */

/*  LDA     (input) INTEGER */
/*          The leading dimension of the array A.  LDA >= max(1, N). */

/*  RHS     (input/output) DOUBLE PRECISION array, dimension (N). */
/*          On entry, the right hand side vector b. */
/*          On exit, the solution vector X. */

/*  IPIV    (iput) INTEGER array, dimension (N). */
/*          The pivot indices; for 1 <= i <= N, row i of the */
/*          matrix has been interchanged with row IPIV(i). */

/*  JPIV    (iput) INTEGER array, dimension (N). */
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
/*<       DOUBLE PRECISION   ONE, TWO >*/
/*<       PARAMETER          ( ONE = 1.0D+0, TWO = 2.0D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       INTEGER            I, J >*/
/*<       DOUBLE PRECISION   BIGNUM, EPS, SMLNUM, TEMP >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           DLASWP, DSCAL >*/
/*     .. */
/*     .. External Functions .. */
/*<       INTEGER            IDAMAX >*/
/*<       DOUBLE PRECISION   DLAMCH >*/
/*<       EXTERNAL           IDAMAX, DLAMCH >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS >*/
/*     .. */
/*     .. Executable Statements .. */

/*      Set constant to control owerflow */

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

/*<       CALL DLASWP( 1, RHS, LDA, 1, N-1, IPIV, 1 ) >*/
    i__1 = *n - 1;
    dlaswp_(&c__1, &rhs[1], lda, &c__1, &i__1, &ipiv[1], &c__1);

/*     Solve for L part */

/*<       DO 20 I = 1, N - 1 >*/
    i__1 = *n - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<          DO 10 J = I + 1, N >*/
        i__2 = *n;
        for (j = i__ + 1; j <= i__2; ++j) {
/*<             RHS( J ) = RHS( J ) - A( J, I )*RHS( I ) >*/
            rhs[j] -= a[j + i__ * a_dim1] * rhs[i__];
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

/*<       I = IDAMAX( N, RHS, 1 ) >*/
    i__ = idamax_(n, &rhs[1], &c__1);
/*<       IF( TWO*SMLNUM*ABS( RHS( I ) ).GT.ABS( A( N, N ) ) ) THEN >*/
    if (smlnum * 2. * (d__1 = rhs[i__], abs(d__1)) > (d__2 = a[*n + *n *
            a_dim1], abs(d__2))) {
/*<          TEMP = ( ONE / TWO ) / ABS( RHS( I ) ) >*/
        temp = .5 / (d__1 = rhs[i__], abs(d__1));
/*<          CALL DSCAL( N, TEMP, RHS( 1 ), 1 ) >*/
        dscal_(n, &temp, &rhs[1], &c__1);
/*<          SCALE = SCALE*TEMP >*/
        *scale *= temp;
/*<       END IF >*/
    }

/*<       DO 40 I = N, 1, -1 >*/
    for (i__ = *n; i__ >= 1; --i__) {
/*<          TEMP = ONE / A( I, I ) >*/
        temp = 1. / a[i__ + i__ * a_dim1];
/*<          RHS( I ) = RHS( I )*TEMP >*/
        rhs[i__] *= temp;
/*<          DO 30 J = I + 1, N >*/
        i__1 = *n;
        for (j = i__ + 1; j <= i__1; ++j) {
/*<             RHS( I ) = RHS( I ) - RHS( J )*( A( I, J )*TEMP ) >*/
            rhs[i__] -= rhs[j] * (a[i__ + j * a_dim1] * temp);
/*<    30    CONTINUE >*/
/* L30: */
        }
/*<    40 CONTINUE >*/
/* L40: */
    }

/*     Apply permutations JPIV to the solution (RHS) */

/*<       CALL DLASWP( 1, RHS, LDA, 1, N-1, JPIV, -1 ) >*/
    i__1 = *n - 1;
    dlaswp_(&c__1, &rhs[1], lda, &c__1, &i__1, &jpiv[1], &c_n1);
/*<       RETURN >*/
    return 0;

/*     End of DGESC2 */

/*<       END >*/
} /* dgesc2_ */

#ifdef __cplusplus
        }
#endif
