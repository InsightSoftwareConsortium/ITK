/* lapack/double/dlarfg.f -- translated by f2c (version 20050501).
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

/*<       SUBROUTINE DLARFG( N, ALPHA, X, INCX, TAU ) >*/
/* Subroutine */ int dlarfg_(integer *n, doublereal *alpha, doublereal *x,
        integer *incx, doublereal *tau)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *);

    /* Local variables */
    integer j, knt;
    doublereal beta;
    extern doublereal dnrm2_(integer *, doublereal *, integer *);
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *,
            integer *);
    doublereal xnorm;
    extern doublereal dlapy2_(doublereal *, doublereal *), dlamch_(char *,
            ftnlen);
    doublereal safmin, rsafmn;


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*     .. Scalar Arguments .. */
/*<       INTEGER            INCX, N >*/
/*<       DOUBLE PRECISION   ALPHA, TAU >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       DOUBLE PRECISION   X( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  DLARFG generates a real elementary reflector H of order n, such */
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

/*  ALPHA   (input/output) DOUBLE PRECISION */
/*          On entry, the value alpha. */
/*          On exit, it is overwritten with the value beta. */

/*  X       (input/output) DOUBLE PRECISION array, dimension */
/*                         (1+(N-2)*abs(INCX)) */
/*          On entry, the vector x. */
/*          On exit, it is overwritten with the vector v. */

/*  INCX    (input) INTEGER */
/*          The increment between elements of X. INCX > 0. */

/*  TAU     (output) DOUBLE PRECISION */
/*          The value tau. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ONE, ZERO >*/
/*<       PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       INTEGER            J, KNT >*/
/*<       DOUBLE PRECISION   BETA, RSAFMN, SAFMIN, XNORM >*/
/*     .. */
/*     .. External Functions .. */
/*<       DOUBLE PRECISION   DLAMCH, DLAPY2, DNRM2 >*/
/*<       EXTERNAL           DLAMCH, DLAPY2, DNRM2 >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, SIGN >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           DSCAL >*/
/*     .. */
/*     .. Executable Statements .. */

/*<       IF( N.LE.1 ) THEN >*/
    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n <= 1) {
/*<          TAU = ZERO >*/
        *tau = 0.;
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*<       XNORM = DNRM2( N-1, X, INCX ) >*/
    i__1 = *n - 1;
    xnorm = dnrm2_(&i__1, &x[1], incx);

/*<       IF( XNORM.EQ.ZERO ) THEN >*/
    if (xnorm == 0.) {

/*        H  =  I */

/*<          TAU = ZERO >*/
        *tau = 0.;
/*<       ELSE >*/
    } else {

/*        general case */

/*<          BETA = -SIGN( DLAPY2( ALPHA, XNORM ), ALPHA ) >*/
        d__1 = dlapy2_(alpha, &xnorm);
        beta = -d_sign(&d__1, alpha);
/*<          SAFMIN = DLAMCH( 'S' ) / DLAMCH( 'E' ) >*/
        safmin = dlamch_("S", (ftnlen)1) / dlamch_("E", (ftnlen)1);
/*<          IF( ABS( BETA ).LT.SAFMIN ) THEN >*/
        if (abs(beta) < safmin) {

/*           XNORM, BETA may be inaccurate; scale X and recompute them */

/*<             RSAFMN = ONE / SAFMIN >*/
            rsafmn = 1. / safmin;
/*<             KNT = 0 >*/
            knt = 0;
/*<    10       CONTINUE >*/
L10:
/*<             KNT = KNT + 1 >*/
            ++knt;
/*<             CALL DSCAL( N-1, RSAFMN, X, INCX ) >*/
            i__1 = *n - 1;
            dscal_(&i__1, &rsafmn, &x[1], incx);
/*<             BETA = BETA*RSAFMN >*/
            beta *= rsafmn;
/*<             ALPHA = ALPHA*RSAFMN >*/
            *alpha *= rsafmn;
/*<    >*/
            if (abs(beta) < safmin) {
                goto L10;
            }

/*           New BETA is at most 1, at least SAFMIN */

/*<             XNORM = DNRM2( N-1, X, INCX ) >*/
            i__1 = *n - 1;
            xnorm = dnrm2_(&i__1, &x[1], incx);
/*<             BETA = -SIGN( DLAPY2( ALPHA, XNORM ), ALPHA ) >*/
            d__1 = dlapy2_(alpha, &xnorm);
            beta = -d_sign(&d__1, alpha);
/*<             TAU = ( BETA-ALPHA ) / BETA >*/
            *tau = (beta - *alpha) / beta;
/*<             CALL DSCAL( N-1, ONE / ( ALPHA-BETA ), X, INCX ) >*/
            i__1 = *n - 1;
            d__1 = 1. / (*alpha - beta);
            dscal_(&i__1, &d__1, &x[1], incx);

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
/*<             CALL DSCAL( N-1, ONE / ( ALPHA-BETA ), X, INCX ) >*/
            i__1 = *n - 1;
            d__1 = 1. / (*alpha - beta);
            dscal_(&i__1, &d__1, &x[1], incx);
/*<             ALPHA = BETA >*/
            *alpha = beta;
/*<          END IF >*/
        }
/*<       END IF >*/
    }

/*<       RETURN >*/
    return 0;

/*     End of DLARFG */

/*<       END >*/
} /* dlarfg_ */

#ifdef __cplusplus
        }
#endif
