/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublecomplex c_b5 = {1.,0.};

/* Subroutine */ int zlarfg_(n, alpha, x, incx, tau)
integer *n;
doublecomplex *alpha, *x;
integer *incx;
doublecomplex *tau;
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;
    doublecomplex z__1, z__2;

    /* Builtin functions */
    double d_imag(), d_sign();

    /* Local variables */
    static doublereal beta;
    static integer j;
    static doublereal alphi, alphr;
    extern /* Subroutine */ int zscal_();
    static doublereal xnorm;
    extern doublereal dlapy3_(), dznrm2_(), dlamch_();
    static doublereal safmin;
    extern /* Subroutine */ int zdscal_();
    static doublereal rsafmn;
    extern /* Double Complex */ void zladiv_();
    static integer knt;


/*  -- LAPACK auxiliary routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Array Arguments .. */
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

/*  ===================================================================== 
*/

/*     .. Parameters .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. External Functions .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. External Subroutines .. */
/*     .. */
/*     .. Executable Statements .. */

    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n <= 0) {
	tau->r = 0., tau->i = 0.;
	return 0;
    }

    i__1 = *n - 1;
    xnorm = dznrm2_(&i__1, &x[1], incx);
    alphr = alpha->r;
    alphi = d_imag(alpha);

    if (xnorm == 0. && alphi == 0.) {

/*        H  =  I */

	tau->r = 0., tau->i = 0.;
    } else {

/*        general case */

	d__1 = dlapy3_(&alphr, &alphi, &xnorm);
	beta = -d_sign(&d__1, &alphr);
	safmin = dlamch_("S", 1L) / dlamch_("E", 1L);
	rsafmn = 1. / safmin;

	if (abs(beta) < safmin) {

/*           XNORM, BETA may be inaccurate; scale X and recompute 
them */

	    knt = 0;
L10:
	    ++knt;
	    i__1 = *n - 1;
	    zdscal_(&i__1, &rsafmn, &x[1], incx);
	    beta *= rsafmn;
	    alphi *= rsafmn;
	    alphr *= rsafmn;
	    if (abs(beta) < safmin) {
		goto L10;
	    }

/*           New BETA is at most 1, at least SAFMIN */

	    i__1 = *n - 1;
	    xnorm = dznrm2_(&i__1, &x[1], incx);
	    z__1.r = alphr, z__1.i = alphi;
	    alpha->r = z__1.r, alpha->i = z__1.i;
	    d__1 = dlapy3_(&alphr, &alphi, &xnorm);
	    beta = -d_sign(&d__1, &alphr);
	    d__1 = (beta - alphr) / beta;
	    d__2 = -alphi / beta;
	    z__1.r = d__1, z__1.i = d__2;
	    tau->r = z__1.r, tau->i = z__1.i;
	    z__2.r = alpha->r - beta, z__2.i = alpha->i;
	    zladiv_(&z__1, &c_b5, &z__2);
	    alpha->r = z__1.r, alpha->i = z__1.i;
	    i__1 = *n - 1;
	    zscal_(&i__1, alpha, &x[1], incx);

/*           If ALPHA is subnormal, it may lose relative accuracy 
*/

	    alpha->r = beta, alpha->i = 0.;
	    i__1 = knt;
	    for (j = 1; j <= i__1; ++j) {
		z__1.r = safmin * alpha->r, z__1.i = safmin * alpha->i;
		alpha->r = z__1.r, alpha->i = z__1.i;
/* L20: */
	    }
	} else {
	    d__1 = (beta - alphr) / beta;
	    d__2 = -alphi / beta;
	    z__1.r = d__1, z__1.i = d__2;
	    tau->r = z__1.r, tau->i = z__1.i;
	    z__2.r = alpha->r - beta, z__2.i = alpha->i;
	    zladiv_(&z__1, &c_b5, &z__2);
	    alpha->r = z__1.r, alpha->i = z__1.i;
	    i__1 = *n - 1;
	    zscal_(&i__1, alpha, &x[1], incx);
	    alpha->r = beta, alpha->i = 0.;
	}
    }

    return 0;

/*     End of ZLARFG */

} /* zlarfg_ */

