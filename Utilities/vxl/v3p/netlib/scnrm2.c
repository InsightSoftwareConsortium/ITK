#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

real scnrm2_(n, x, incx)
const integer *n;
const complex *x;
const integer *incx;
{
    /* Local variables */
    static real temp, norm, scale;
    static integer ix;
    static real ssq;

/*  SCNRM2 returns the euclidean norm of a vector via the function */
/*  name, so that                                                  */
/*                                                                 */
/*     SCNRM2 := sqrt( conjg( x' )*x )                             */


/*  -- This version written on 25-October-1982. */
/*     Modified on 14-October-1993 to inline the call to CLASSQ. */
/*     Sven Hammarling, Nag Ltd. */

    if (*n < 1 || *incx < 1) {
        norm = 0.f;
    } else {
        scale = 0.f;
        ssq = 1.f;
/*        The following loop is equivalent to this call to the LAPACK */
/*        auxiliary routine: */
/*        CALL CLASSQ( N, X, INCX, SCALE, SSQ ) */

        for (ix = 0; ix < *n * *incx; ix += *incx) {
            if (x[ix].r != 0.f) {
                temp = abs(x[ix].r);
                if (scale < temp) {
                    scale /= temp;
                    ssq = ssq * (scale * scale) + 1.f;
                    scale = temp;
                } else {
                    temp /= scale;
                    ssq += temp * temp;
                }
            }
            if (x[ix].i != 0.f) {
                temp = abs(x[ix].i);
                if (scale < temp) {
                    scale /= temp;
                    ssq = ssq * (scale * scale) + 1.f;
                    scale = temp;
                } else {
                    temp /= scale;
                    ssq += temp * temp;
                }
            }
        }
        norm = scale * sqrtf(ssq);
    }

    return norm;
} /* scnrm2_ */
