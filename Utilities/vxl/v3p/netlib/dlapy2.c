#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

doublereal dlapy2_(const doublereal *x, const doublereal *y)
{
    /* Local variables */
    static doublereal xabs, yabs, w, z;

/*  -- LAPACK auxiliary routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     October 31, 1992 */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  DLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary  */
/*  overflow.                                                             */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  X       (input) DOUBLE PRECISION                                      */
/*  Y       (input) DOUBLE PRECISION                                      */
/*          X and Y specify the values x and y.                           */
/*                                                                        */
/*  ===================================================================== */

    xabs = abs(*x);
    yabs = abs(*y);
    w = max(xabs,yabs);
    z = min(xabs,yabs);
    if (z == 0.) {
        return w;
    } else {
        z /= w;
        return w * sqrt(z * z + 1.);
    }
} /* dlapy2_ */
