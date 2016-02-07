/* lapack/single/slarfg.f -- translated by f2c (version 20050501).
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

/*<       SUBROUTINE SLARFG( N, ALPHA, X, INCX, TAU ) >*/
/* Subroutine */ int slarfg_(integer *n, real *alpha, real *x, integer *incx,
        real *tau)
{
    /* System generated locals */
    integer i__1;
    real r__1;

    /* Builtin functions */
    double r_sign(real *, real *);

    /* Local variables */
    integer j, knt;
    real beta;
    extern doublereal snrm2_(integer *, real *, integer *);
    extern /* Subroutine */ int sscal_(integer *, real *, real *, integer *);
    real xnorm;
    extern doublereal slapy2_(real *, real *), slamch_(char *, ftnlen);
    real safmin, rsafmn;


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*     .. Scalar Arguments .. */
/*<       INTEGER            INCX, N >*/
/*<       REAL               ALPHA, TAU >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       REAL               X( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  SLARFG generates a real elementary reflector H of order n, such */
/*  that */

/*        H * ( alpha ) = ( beta ),   H' * H = I. */
/*            (   x   )   (   0  ) */

/*  where alpha and beta are scalars, and x is an (n-1)-element real */
/*  vector. H is represented in the form */

/*        H = I - tau * ( 1 ) * ( 1 v' ) , */
/*                      ( v ) */

/*  where tau is a real scalar and v is a real (n-1)-element */
/*  vector. */

/*  If the elements of x are all zero, then tau = 0 and H is taken to be */
/*  the unit matrix. */

/*  Otherwise  1 <= tau <= 2. */

/*  Arguments */
/*  ========= */

/*  N       (input) INTEGER */
/*          The order of the elementary reflector. */

/*  ALPHA   (input/output) REAL */
/*          On entry, the value alpha. */
/*          On exit, it is overwritten with the value beta. */

/*  X       (input/output) REAL array, dimension */
/*                         (1+(N-2)*abs(INCX)) */
/*          On entry, the vector x. */
/*          On exit, it is overwritten with the vector v. */

/*  INCX    (input) INTEGER */
/*          The increment between elements of X. INCX > 0. */

/*  TAU     (output) REAL */
/*          The value tau. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       REAL               ONE, ZERO >*/
/*<       PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       INTEGER            J, KNT >*/
/*<       REAL               BETA, RSAFMN, SAFMIN, XNORM >*/
/*     .. */
/*     .. External Functions .. */
/*<       REAL               SLAMCH, SLAPY2, SNRM2 >*/
/*<       EXTERNAL           SLAMCH, SLAPY2, SNRM2 >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, SIGN >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           SSCAL >*/
/*     .. */
/*     .. Executable Statements .. */

/*<       IF( N.LE.1 ) THEN >*/
    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n <= 1) {
/*<          TAU = ZERO >*/
        *tau = (float)0.;
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*<       XNORM = SNRM2( N-1, X, INCX ) >*/
    i__1 = *n - 1;
    xnorm = snrm2_(&i__1, &x[1], incx);

/*<       IF( XNORM.EQ.ZERO ) THEN >*/
    if (xnorm == (float)0.) {

/*        H  =  I */

/*<          TAU = ZERO >*/
        *tau = (float)0.;
/*<       ELSE >*/
    } else {

/*        general case */

/*<          BETA = -SIGN( SLAPY2( ALPHA, XNORM ), ALPHA ) >*/
        r__1 = slapy2_(alpha, &xnorm);
        beta = -r_sign(&r__1, alpha);
/*<          SAFMIN = SLAMCH( 'S' ) / SLAMCH( 'E' ) >*/
        safmin = slamch_("S", (ftnlen)1) / slamch_("E", (ftnlen)1);
/*<          IF( ABS( BETA ).LT.SAFMIN ) THEN >*/
        if (dabs(beta) < safmin) {

/*           XNORM, BETA may be inaccurate; scale X and recompute them */

/*<             RSAFMN = ONE / SAFMIN >*/
            rsafmn = (float)1. / safmin;
/*<             KNT = 0 >*/
            knt = 0;
/*<    10       CONTINUE >*/
L10:
/*<             KNT = KNT + 1 >*/
            ++knt;
/*<             CALL SSCAL( N-1, RSAFMN, X, INCX ) >*/
            i__1 = *n - 1;
            sscal_(&i__1, &rsafmn, &x[1], incx);
/*<             BETA = BETA*RSAFMN >*/
            beta *= rsafmn;
/*<             ALPHA = ALPHA*RSAFMN >*/
            *alpha *= rsafmn;
/*<    >*/
            if (dabs(beta) < safmin) {
                goto L10;
            }

/*           New BETA is at most 1, at least SAFMIN */

/*<             XNORM = SNRM2( N-1, X, INCX ) >*/
            i__1 = *n - 1;
            xnorm = snrm2_(&i__1, &x[1], incx);
/*<             BETA = -SIGN( SLAPY2( ALPHA, XNORM ), ALPHA ) >*/
            r__1 = slapy2_(alpha, &xnorm);
            beta = -r_sign(&r__1, alpha);
/*<             TAU = ( BETA-ALPHA ) / BETA >*/
            *tau = (beta - *alpha) / beta;
/*<             CALL SSCAL( N-1, ONE / ( ALPHA-BETA ), X, INCX ) >*/
            i__1 = *n - 1;
            r__1 = (float)1. / (*alpha - beta);
            sscal_(&i__1, &r__1, &x[1], incx);

/*           If ALPHA is subnormal, it may lose relative accuracy */

/*<             ALPHA = BETA >*/
            *alpha = beta;
/*<             DO 20 J = 1, KNT >*/
            i__1 = knt;
            for (j = 1; j <= i__1; ++j) {
/*<                ALPHA = ALPHA*SAFMIN >*/
                *alpha *= safmin;
/*<    20       CONTINUE >*/
/* L20: */
            }
/*<          ELSE >*/
        } else {
/*<             TAU = ( BETA-ALPHA ) / BETA >*/
            *tau = (beta - *alpha) / beta;
/*<             CALL SSCAL( N-1, ONE / ( ALPHA-BETA ), X, INCX ) >*/
            i__1 = *n - 1;
            r__1 = (float)1. / (*alpha - beta);
            sscal_(&i__1, &r__1, &x[1], incx);
/*<             ALPHA = BETA >*/
            *alpha = beta;
/*<          END IF >*/
        }
/*<       END IF >*/
    }

/*<       RETURN >*/
    return 0;

/*     End of SLARFG */

/*<       END >*/
} /* slarfg_ */

#ifdef __cplusplus
        }
#endif
