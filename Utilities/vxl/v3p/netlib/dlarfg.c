#include "f2c.h"
#include "netlib.h"

/* Subroutine */ void dlarfg_(const integer *n, doublereal *alpha, doublereal *x, const integer *incx, doublereal *tau)
{
    /* System generated locals */
    const integer nm1 = *n - 1;
    doublereal d__1;

    /* Local variables */
    static doublereal beta;
    static integer j;
    static doublereal xnorm;
    static doublereal safmin, rsafmn;
    static integer knt;

/*  -- LAPACK auxiliary routine (version 2.0) --                          */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,        */
/*     Courant Institute, Argonne National Lab, and Rice University       */
/*     September 30, 1994                                                 */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  DLARFG generates a real elementary reflector H of order n, such       */
/*  that                                                                  */
/*                                                                        */
/*        H * ( alpha ) = ( beta ),   H' * H = I.                         */
/*            (   x   )   (   0  )                                        */
/*                                                                        */
/*  where alpha and beta are scalars, and x is an (n-1)-element real      */
/*  vector. H is represented in the form                                  */
/*                                                                        */
/*        H = I - tau * ( 1 ) * ( 1 v' ) ,                                */
/*                      ( v )                                             */
/*                                                                        */
/*  where tau is a real scalar and v is a real (n-1)-element              */
/*  vector.                                                               */
/*                                                                        */
/*  If the elements of x are all zero, then tau = 0 and H is taken to be  */
/*  the unit matrix.                                                      */
/*                                                                        */
/*  Otherwise  1 <= tau <= 2.                                             */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  N       (input) INTEGER                                               */
/*          The order of the elementary reflector.                        */
/*                                                                        */
/*  ALPHA   (input/output) DOUBLE PRECISION                               */
/*          On entry, the value alpha.                                    */
/*          On exit, it is overwritten with the value beta.               */
/*                                                                        */
/*  X       (input/output) DOUBLE PRECISION array, dimension              */
/*                         (1+(N-2)*abs(INCX))                            */
/*          On entry, the vector x.                                       */
/*          On exit, it is overwritten with the vector v.                 */
/*                                                                        */
/*  INCX    (input) INTEGER                                               */
/*          The increment between elements of X. INCX > 0.                */
/*                                                                        */
/*  TAU     (output) DOUBLE PRECISION                                     */
/*          The value tau.                                                */
/*                                                                        */
/*  ===================================================================== */

    if (*n <= 1) {
        *tau = 0.;
        return;
    }

    xnorm = dnrm2_(&nm1, x, incx);

    if (xnorm == 0.) {

/*        H  =  I */

        *tau = 0.;
    } else {

/*        general case */

        d__1 = dlapy2_(alpha, &xnorm);
        beta = -d_sign(&d__1, alpha);
        safmin = dlamch_("S") / dlamch_("E");
        if (abs(beta) < safmin) {

/*           XNORM, BETA may be inaccurate; scale X and recompute them */

            rsafmn = 1. / safmin;
            knt = 0;
            do {
                ++knt;
                dscal_(&nm1, &rsafmn, x, incx);
                beta *= rsafmn;
                *alpha *= rsafmn;
            } while (abs(beta) < safmin);

/*           New BETA is at most 1, at least SAFMIN */

            xnorm = dnrm2_(&nm1, x, incx);
            d__1 = dlapy2_(alpha, &xnorm);
            beta = -d_sign(&d__1, alpha);
            *tau = (beta - *alpha) / beta;
            d__1 = 1. / (*alpha - beta);
            dscal_(&nm1, &d__1, x, incx);

/*           If ALPHA is subnormal, it may lose relative accuracy */

            *alpha = beta;
            for (j = 0; j < knt; ++j) {
                *alpha *= safmin;
            }
        } else {
            *tau = (beta - *alpha) / beta;
            d__1 = 1. / (*alpha - beta);
            dscal_(&nm1, &d__1, x, incx);
            *alpha = beta;
        }
    }
} /* dlarfg_ */
