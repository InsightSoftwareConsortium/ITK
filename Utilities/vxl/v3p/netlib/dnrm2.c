#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

#ifdef KR_headers
doublereal dnrm2_(n, x, incx)
const integer *n;
const doublereal *x;
const integer *incx;
#else
doublereal dnrm2_(const integer* n, const doublereal* x, const integer* incx)
#endif
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    static doublereal norm, scale, absxi;
    static integer ix;
    static doublereal ssq;

/*  DNRM2 returns the euclidean norm of a vector via the function       */
/*  name, so that                                                       */
/*                                                                      */
/*     DNRM2 := sqrt( x'*x )                                            */
/*                                                                      */
/*  -- This version written on 25-October-1982.                         */
/*     Modified on 14-October-1993 to inline the call to DLASSQ.        */
/*     Sven Hammarling, Nag Ltd.                                        */

    if (*n < 1 || *incx < 1) {
        norm = 0.;
    } else if (*n == 1) {
        norm = abs(x[0]);
    } else {
        scale = 0.;
        ssq = 1.;
/*        The following loop is equivalent to this call to the LAPACK */
/*        auxiliary routine: */
/*        CALL DLASSQ( N, X, INCX, SCALE, SSQ ) */

        for (ix = 0; ix < *n * *incx; ix += *incx) {
            if (x[ix] != 0.) {
                absxi = abs(x[ix]);
                if (scale < absxi) {
                    d__1 = scale / absxi;
                    ssq = ssq * d__1 * d__1 + 1.;
                    scale = absxi;
                } else {
                    d__1 = absxi / scale;
                    ssq += d__1 * d__1;
                }
            }
        }
        norm = scale * sqrt(ssq);
    }

    return norm;

} /* dnrm2_ */
