#include "f2c.h"
#include "netlib.h"
extern double sqrt(double); /* #include <math.h> */

real slapy2_(const real *x, const real *y)
{
    /* Local variables */
    static real xabs, yabs, w, z;

/*  -- LAPACK auxiliary routine (version 2.0) -- */
/*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd., */
/*     Courant Institute, Argonne National Lab, and Rice University */
/*     October 31, 1992 */

/*  Purpose                                                               */
/*  =======                                                               */
/*                                                                        */
/*  SLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary  */
/*  overflow.                                                             */
/*                                                                        */
/*  Arguments                                                             */
/*  =========                                                             */
/*                                                                        */
/*  X       (input) REAL                                                  */
/*  Y       (input) REAL                                                  */
/*          X and Y specify the values x and y.                           */
/*                                                                        */
/*  ===================================================================== */

    xabs = abs(*x);
    yabs = abs(*y);
    w = max(xabs,yabs);
    z = min(xabs,yabs);
    if (z == 0.f) {
        return w;
    } else {
        z /= w;
        return w * sqrtf(z * z + 1.f);
    }
} /* slapy2_ */
