#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

doublereal dlapy3_(x, y, z)
const doublereal *x, *y, *z;
{
    /* Local variables */
    static doublereal xabs, yabs, zabs, w;

/*  -- LAPACK auxiliary routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     October 31, 1992 */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  DLAPY3 returns sqrt(x**2+y**2+z**2), taking care not to cause         */
/*  unnecessary overflow.                                                 */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  X       (input) DOUBLE PRECISION                                      */
/*  Y       (input) DOUBLE PRECISION                                      */
/*  Z       (input) DOUBLE PRECISION                                      */
/*          X, Y and Z specify the values x, y and z.                     */
/*                                                                        */
/*  ===================================================================== */

    xabs = abs(*x);
    yabs = abs(*y);
    zabs = abs(*z);
    w = max(max(xabs,yabs),zabs);
    if (w == 0.) {
        return 0;
    } else {
        xabs /= w; yabs /= w; zabs /= w;
        return w * sqrt(xabs * xabs + yabs * yabs + zabs * zabs);
    }
} /* dlapy3_ */
