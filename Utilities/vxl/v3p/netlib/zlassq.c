#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Subroutine */ void zlassq_(n, x, incx, scale, sumsq)
const integer *n;
const doublecomplex *x;
const integer *incx;
doublereal *scale, *sumsq;
{
    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Local variables */
    static doublereal temp1;
    static integer ix;

/*  -- LAPACK auxiliary routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     October 31, 1992 */

/* ===================================================================== */
/*                                                                       */
/*  Purpose                                                              */
/*  =======                                                              */
/*                                                                       */
/*  ZLASSQ returns the values scl and ssq such that                      */
/*                                                                       */
/*     ( scl**2 )*ssq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq,  */
/*                                                                       */
/*  where x( i ) = abs( X( 1 + ( i - 1 )*INCX ) ). The value of sumsq is */
/*  assumed to be at least unity and the value of ssq will then satisfy  */
/*                                                                       */
/*     1.0 .le. ssq .le. ( sumsq + 2*n ).                                */
/*                                                                       */
/*  scale is assumed to be non-negative and scl returns the value        */
/*                                                                       */
/*     scl = max( scale, abs( real( x( i ) ) ), abs( aimag( x( i ) ) ) ),*/
/*            i                                                          */
/*                                                                       */
/*  scale and sumsq must be supplied in SCALE and SUMSQ respectively.    */
/*  SCALE and SUMSQ are overwritten by scl and ssq respectively.         */
/*                                                                       */
/*  The routine makes only one pass through the vector X.                */
/*                                                                       */
/*  Arguments                                                            */
/*  =========                                                            */
/*                                                                       */
/*  N       (input) INTEGER                                              */
/*          The number of elements to be used from the vector X.         */
/*                                                                       */
/*  X       (input) DOUBLE PRECISION                                     */
/*          The vector x as described above.                             */
/*             x( i )  = X( 1 + ( i - 1 )*INCX ), 1 <= i <= n.           */
/*                                                                       */
/*  INCX    (input) INTEGER                                              */
/*          The increment between successive values of the vector X.     */
/*          INCX > 0.                                                    */
/*                                                                       */
/*  SCALE   (input/output) DOUBLE PRECISION                              */
/*          On entry, the value  scale  in the equation above.           */
/*          On exit, SCALE is overwritten with the value  scl .          */
/*                                                                       */
/*  SUMSQ   (input/output) DOUBLE PRECISION                              */
/*          On entry, the value  sumsq  in the equation above.           */
/*          On exit, SUMSQ is overwritten with the value  ssq .          */
/*                                                                       */
/* ===================================================================== */

    if (*n > 0) {
        i__1 = (*n - 1) * *incx;
        for (ix = 0; *incx < 0 ? ix >= i__1 : ix <= i__1; ix += *incx) {
            if (x[ix].r != 0.) {
                temp1 = abs(x[ix].r);
                if (*scale < temp1) {
                    d__1 = *scale / temp1;
                    *sumsq = *sumsq * (d__1 * d__1) + 1;
                    *scale = temp1;
                } else {
                    d__1 = temp1 / *scale;
                    *sumsq += d__1 * d__1;
                }
            }
            if (x[ix].i != 0.) {
                temp1 = abs(x[ix].i);
                if (*scale < temp1) {
                    d__1 = *scale / temp1;
                    *sumsq = *sumsq * (d__1 * d__1) + 1;
                    *scale = temp1;
                } else {
                    d__1 = temp1 / *scale;
                    *sumsq += d__1 * d__1;
                }
            }
        }
    }
} /* zlassq_ */
