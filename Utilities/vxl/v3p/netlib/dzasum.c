#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

doublereal dzasum_(n, zx, incx)
const integer *n;
const doublecomplex *zx;
const integer *incx;
{
    /* Local variables */
    static integer i;
    static doublereal stemp;
    static integer ix;

/*     takes the sum of the absolute values (1-norm). */
/*     jack dongarra, 3/11/78. */
/*     modified 3/93 to return if incx .le. 0. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

    stemp = 0.;
    if (*n <= 0 || *incx <= 0) {
        return stemp;
    }
    if (*incx == 1) {
        for (i = 0; i < *n; ++i) {
            stemp += abs(zx[i].r) + abs(zx[i].i);
        }
    }
    else {
        ix = 0;
        for (i = 0; i < *n; ++i) {
            stemp += abs(zx[ix].r) + abs(zx[ix].i);
            ix += *incx;
        }
    }
    return stemp;
} /* dzasum_ */

