#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Table of constant values */
static doublecomplex c_b5 = {1.,0.};

/* Subroutine */ void zlarfg_(n, alpha, x, incx, tau)
const integer *n;
doublecomplex *alpha, *x;
const integer *incx;
doublecomplex *tau;
{
    /* System generated locals */
    integer i__1;
    doublereal d__1;
    doublecomplex z__1;

    /* Local variables */
    static doublereal beta;
    static integer j;
    static doublereal alphi, alphr;
    static doublereal xnorm;
    static doublereal safmin;
    static doublereal rsafmn;
    static integer knt;

/*  -- LAPACK auxiliary routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     September 30, 1994 */

/*  ===================================================================== */
/*                                                                        */
/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  ZLARFG generates a complex elementary reflector H of order n, such    */
/*  that                                                                  */
/*                                                                        */
/*        H' * ( alpha ) = ( beta ),   H' * H = I.                        */
/*             (   x   )   (   0  )                                       */
/*                                                                        */
/*  where alpha and beta are scalars, with beta real, and x is an         */
/*  (n-1)-element complex vector. H is represented in the form            */
/*                                                                        */
/*        H = I - tau * ( 1 ) * ( 1 v' ) ,                                */
/*                      ( v )                                             */
/*                                                                        */
/*  where tau is a complex scalar and v is a complex (n-1)-element        */
/*  vector. Note that H is not hermitian.                                 */
/*                                                                        */
/*  If the elements of x are all zero and alpha is real, then tau = 0     */
/*  and H is taken to be the unit matrix.                                 */
/*                                                                        */
/*  Otherwise  1 <= real(tau) <= 2  and  abs(tau-1) <= 1 .                */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The order of the elementary reflector.                        */
/*                                                                        */
/*  ALPHA   (input/output) COMPLEX*16                                     */
/*          On entry, the value alpha.                                    */
/*          On exit, it is overwritten with the value beta.               */
/*                                                                        */
/*  X       (input/output) COMPLEX*16 array, dimension                    */
/*                         (1+(N-2)*abs(INCX))                            */
/*          On entry, the vector x.                                       */
/*          On exit, it is overwritten with the vector v.                 */
/*                                                                        */
/*  INCX    (input) INTEGER                                               */
/*          The increment between elements of X. INCX > 0.                */
/*                                                                        */
/*  TAU     (output) COMPLEX*16                                           */
/*          The value tau.                                                */
/*                                                                        */
/*  ===================================================================== */

    if (*n <= 0) {
        tau->r = 0., tau->i = 0.;
        return;
    }

    i__1 = *n - 1;
    xnorm = dznrm2_(&i__1, x, incx);
    alphr = alpha->r;
    alphi = alpha->i;

    if (xnorm == 0. && alphi == 0.) {

/*        H  =  I */

        tau->r = 0., tau->i = 0.;
    } else {

/*        general case */

        d__1 = dlapy3_(&alphr, &alphi, &xnorm);
        beta = -d_sign(&d__1, &alphr);
        safmin = dlamch_("S") / dlamch_("E");
        rsafmn = 1. / safmin;

        if (abs(beta) < safmin) {

/*           XNORM, BETA may be inaccurate; scale X and recompute them */

            knt = 0;
L10:
            ++knt;
            i__1 = *n - 1;
            zdscal_(&i__1, &rsafmn, x, incx);
            beta *= rsafmn;
            alphi *= rsafmn;
            alphr *= rsafmn;
            if (abs(beta) < safmin) {
                goto L10;
            }

/*           New BETA is at most 1, at least SAFMIN */

            i__1 = *n - 1;
            xnorm = dznrm2_(&i__1, x, incx);
            alpha->r = alphr, alpha->i = alphi;
            d__1 = dlapy3_(&alphr, &alphi, &xnorm);
            beta = -d_sign(&d__1, &alphr);
            tau->r = (beta - alphr) / beta, tau->i = -alphi / beta;
            z__1.r = alpha->r - beta, z__1.i = alpha->i;
            zladiv_(alpha, &c_b5, &z__1);
            i__1 = *n - 1;
            zscal_(&i__1, alpha, x, incx);

/*           If ALPHA is subnormal, it may lose relative accuracy */

            alpha->r = beta, alpha->i = 0.;
            for (j = 1; j <= knt; ++j) {
                alpha->r *= safmin, alpha->i *= safmin;
            }
        } else {
            tau->r = (beta - alphr) / beta, tau->i = -alphi / beta;
            z__1.r = alpha->r - beta, z__1.i = alpha->i;
            zladiv_(alpha, &c_b5, &z__1);
            i__1 = *n - 1;
            zscal_(&i__1, alpha, x, incx);
            alpha->r = beta, alpha->i = 0.;
        }
    }
} /* zlarfg_ */
