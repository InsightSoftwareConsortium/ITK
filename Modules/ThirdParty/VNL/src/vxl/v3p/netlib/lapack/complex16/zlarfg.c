/* lapack/complex16/zlarfg.f -- translated by f2c (version 20050501).
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

static doublecomplex c_b5 = {1.,0.};

/*<       SUBROUTINE ZLARFG( N, ALPHA, X, INCX, TAU ) >*/
/* Subroutine */ int zlarfg_(integer *n, doublecomplex *alpha, doublecomplex *
        x, integer *incx, doublecomplex *tau)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;
    doublecomplex z__1, z__2;

    /* Builtin functions */
    double d_imag(doublecomplex *), d_sign(doublereal *, doublereal *);

    /* Local variables */
    integer j, knt;
    doublereal beta, alphi, alphr;
    extern /* Subroutine */ int zscal_(integer *, doublecomplex *,
            doublecomplex *, integer *);
    doublereal xnorm;
    extern doublereal dlapy3_(doublereal *, doublereal *, doublereal *),
            dznrm2_(integer *, doublecomplex *, integer *), dlamch_(char *,
            ftnlen);
    doublereal safmin;
    extern /* Subroutine */ int zdscal_(integer *, doublereal *,
            doublecomplex *, integer *);
    doublereal rsafmn;
    extern /* Double Complex */ VOID zladiv_(doublecomplex *, doublecomplex *,
             doublecomplex *);


/*  -- LAPACK auxiliary routine (version 3.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*     .. Scalar Arguments .. */
/*<       INTEGER            INCX, N >*/
/*<       COMPLEX*16         ALPHA, TAU >*/
/*     .. */
/*     .. Array Arguments .. */
/*<       COMPLEX*16         X( * ) >*/
/*     .. */

/*  Purpose */
/*  ======= */

/*  ZLARFG generates a complex elementary reflector H of order n, such */
/*  that */

/*        H' * ( alpha ) = ( beta ),   H' * H = I. */
/*             (   x   )   (   0  ) */

/*  where alpha and beta are scalars, with beta real, and x is an */
/*  (n-1)-element complex vector. H is represented in the form */

/*        H = I - tau * ( 1 ) * ( 1 v' ) , */
/*                      ( v ) */

/*  where tau is a complex scalar and v is a complex (n-1)-element */
/*  vector. Note that H is not hermitian. */

/*  If the elements of x are all zero and alpha is real, then tau = 0 */
/*  and H is taken to be the unit matrix. */

/*  Otherwise  1 <= real(tau) <= 2  and  abs(tau-1) <= 1 . */

/*  Arguments */
/*  ========= */

/*  N       (input) INTEGER */
/*          The order of the elementary reflector. */

/*  ALPHA   (input/output) COMPLEX*16 */
/*          On entry, the value alpha. */
/*          On exit, it is overwritten with the value beta. */

/*  X       (input/output) COMPLEX*16 array, dimension */
/*                         (1+(N-2)*abs(INCX)) */
/*          On entry, the vector x. */
/*          On exit, it is overwritten with the vector v. */

/*  INCX    (input) INTEGER */
/*          The increment between elements of X. INCX > 0. */

/*  TAU     (output) COMPLEX*16 */
/*          The value tau. */

/*  ===================================================================== */

/*     .. Parameters .. */
/*<       DOUBLE PRECISION   ONE, ZERO >*/
/*<       PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 ) >*/
/*     .. */
/*     .. Local Scalars .. */
/*<       INTEGER            J, KNT >*/
/*<       DOUBLE PRECISION   ALPHI, ALPHR, BETA, RSAFMN, SAFMIN, XNORM >*/
/*     .. */
/*     .. External Functions .. */
/*<       DOUBLE PRECISION   DLAMCH, DLAPY3, DZNRM2 >*/
/*<       COMPLEX*16         ZLADIV >*/
/*<       EXTERNAL           DLAMCH, DLAPY3, DZNRM2, ZLADIV >*/
/*     .. */
/*     .. Intrinsic Functions .. */
/*<       INTRINSIC          ABS, DBLE, DCMPLX, DIMAG, SIGN >*/
/*     .. */
/*     .. External Subroutines .. */
/*<       EXTERNAL           ZDSCAL, ZSCAL >*/
/*     .. */
/*     .. Executable Statements .. */

/*<       IF( N.LE.0 ) THEN >*/
    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n <= 0) {
/*<          TAU = ZERO >*/
        tau->r = 0., tau->i = 0.;
/*<          RETURN >*/
        return 0;
/*<       END IF >*/
    }

/*<       XNORM = DZNRM2( N-1, X, INCX ) >*/
    i__1 = *n - 1;
    xnorm = dznrm2_(&i__1, &x[1], incx);
/*<       ALPHR = DBLE( ALPHA ) >*/
    alphr = alpha->r;
/*<       ALPHI = DIMAG( ALPHA ) >*/
    alphi = d_imag(alpha);

/*<       IF( XNORM.EQ.ZERO .AND. ALPHI.EQ.ZERO ) THEN >*/
    if (xnorm == 0. && alphi == 0.) {

/*        H  =  I */

/*<          TAU = ZERO >*/
        tau->r = 0., tau->i = 0.;
/*<       ELSE >*/
    } else {

/*        general case */

/*<          BETA = -SIGN( DLAPY3( ALPHR, ALPHI, XNORM ), ALPHR ) >*/
        d__1 = dlapy3_(&alphr, &alphi, &xnorm);
        beta = -d_sign(&d__1, &alphr);
/*<          SAFMIN = DLAMCH( 'S' ) / DLAMCH( 'E' ) >*/
        safmin = dlamch_("S", (ftnlen)1) / dlamch_("E", (ftnlen)1);
/*<          RSAFMN = ONE / SAFMIN >*/
        rsafmn = 1. / safmin;

/*<          IF( ABS( BETA ).LT.SAFMIN ) THEN >*/
        if (abs(beta) < safmin) {

/*           XNORM, BETA may be inaccurate; scale X and recompute them */

/*<             KNT = 0 >*/
            knt = 0;
/*<    10       CONTINUE >*/
L10:
/*<             KNT = KNT + 1 >*/
            ++knt;
/*<             CALL ZDSCAL( N-1, RSAFMN, X, INCX ) >*/
            i__1 = *n - 1;
            zdscal_(&i__1, &rsafmn, &x[1], incx);
/*<             BETA = BETA*RSAFMN >*/
            beta *= rsafmn;
/*<             ALPHI = ALPHI*RSAFMN >*/
            alphi *= rsafmn;
/*<             ALPHR = ALPHR*RSAFMN >*/
            alphr *= rsafmn;
/*<    >*/
            if (abs(beta) < safmin) {
                goto L10;
            }

/*           New BETA is at most 1, at least SAFMIN */

/*<             XNORM = DZNRM2( N-1, X, INCX ) >*/
            i__1 = *n - 1;
            xnorm = dznrm2_(&i__1, &x[1], incx);
/*<             ALPHA = DCMPLX( ALPHR, ALPHI ) >*/
            z__1.r = alphr, z__1.i = alphi;
            alpha->r = z__1.r, alpha->i = z__1.i;
/*<             BETA = -SIGN( DLAPY3( ALPHR, ALPHI, XNORM ), ALPHR ) >*/
            d__1 = dlapy3_(&alphr, &alphi, &xnorm);
            beta = -d_sign(&d__1, &alphr);
/*<             TAU = DCMPLX( ( BETA-ALPHR ) / BETA, -ALPHI / BETA ) >*/
            d__1 = (beta - alphr) / beta;
            d__2 = -alphi / beta;
            z__1.r = d__1, z__1.i = d__2;
            tau->r = z__1.r, tau->i = z__1.i;
/*<             ALPHA = ZLADIV( DCMPLX( ONE ), ALPHA-BETA ) >*/
            z__2.r = alpha->r - beta, z__2.i = alpha->i;
            zladiv_(&z__1, &c_b5, &z__2);
            alpha->r = z__1.r, alpha->i = z__1.i;
/*<             CALL ZSCAL( N-1, ALPHA, X, INCX ) >*/
            i__1 = *n - 1;
            zscal_(&i__1, alpha, &x[1], incx);

/*           If ALPHA is subnormal, it may lose relative accuracy */

/*<             ALPHA = BETA >*/
            alpha->r = beta, alpha->i = 0.;
/*<             DO 20 J = 1, KNT >*/
            i__1 = knt;
            for (j = 1; j <= i__1; ++j) {
/*<                ALPHA = ALPHA*SAFMIN >*/
                z__1.r = safmin * alpha->r, z__1.i = safmin * alpha->i;
                alpha->r = z__1.r, alpha->i = z__1.i;
/*<    20       CONTINUE >*/
/* L20: */
            }
/*<          ELSE >*/
        } else {
/*<             TAU = DCMPLX( ( BETA-ALPHR ) / BETA, -ALPHI / BETA ) >*/
            d__1 = (beta - alphr) / beta;
            d__2 = -alphi / beta;
            z__1.r = d__1, z__1.i = d__2;
            tau->r = z__1.r, tau->i = z__1.i;
/*<             ALPHA = ZLADIV( DCMPLX( ONE ), ALPHA-BETA ) >*/
            z__2.r = alpha->r - beta, z__2.i = alpha->i;
            zladiv_(&z__1, &c_b5, &z__2);
            alpha->r = z__1.r, alpha->i = z__1.i;
/*<             CALL ZSCAL( N-1, ALPHA, X, INCX ) >*/
            i__1 = *n - 1;
            zscal_(&i__1, alpha, &x[1], incx);
/*<             ALPHA = BETA >*/
            alpha->r = beta, alpha->i = 0.;
/*<          END IF >*/
        }
/*<       END IF >*/
    }

/*<       RETURN >*/
    return 0;

/*     End of ZLARFG */

/*<       END >*/
} /* zlarfg_ */

#ifdef __cplusplus
        }
#endif
