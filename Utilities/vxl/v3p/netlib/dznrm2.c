#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

doublereal dznrm2_(n, x, incx)
const integer *n;
const doublecomplex *x;
const integer *incx;
{
    /* Local variables */
    static doublereal temp, norm, scale;
    static integer ix;
    static doublereal ssq;

/*  DZNRM2 returns the euclidean norm of a vector via the function */
/*  name, so that                                                  */
/*                                                                 */
/*     DZNRM2 := sqrt( conjg( x' )*x )                             */


/*  -- This version written on 25-October-1982. */
/*     Modified on 14-October-1993 to inline the call to ZLASSQ. */
/*     Sven Hammarling, Nag Ltd. */

    if (*n < 1 || *incx < 1) {
        norm = 0.;
    } else {
        scale = 0.;
        ssq = 1.;
/*        The following loop is equivalent to this call to the LAPACK */
/*        auxiliary routine: */
/*        CALL ZLASSQ( N, X, INCX, SCALE, SSQ ) */

        for (ix = 0; ix < *n * *incx; ix += *incx) {
            if (x[ix].r != 0.) {
                temp = abs(x[ix].r);
                if (scale < temp) {
                    scale /= temp;
                    ssq = ssq * (scale * scale) + 1.;
                    scale = temp;
                } else {
                    temp /= scale;
                    ssq += temp * temp;
                }
            }
            if (x[ix].i != 0.) {
                temp = abs(x[ix].i);
                if (scale < temp) {
                    scale /= temp;
                    ssq = ssq * (scale * scale) + 1.;
                    scale = temp;
                } else {
                    temp /= scale;
                    ssq += temp * temp;
                }
            }
        }
        norm = scale * sqrt(ssq);
    }

    return norm;
} /* dznrm2_ */
