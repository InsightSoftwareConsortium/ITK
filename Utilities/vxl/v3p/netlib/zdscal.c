#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Subroutine */ void zdscal_(n, da, zx, incx)
const integer *n;
const doublereal *da;
doublecomplex *zx;
const integer *incx;
{
    /* Local variables */
    static integer i, ix;

/*     scales a vector by a constant. */
/*     jack dongarra, 3/11/78. */
/*     modified 3/93 to return if incx .le. 0. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

    if (*n <= 0 || *incx <= 0) {
        return;
    }
    if (*incx == 1) {
        for (i = 0; i < *n; ++i) {
            zx[i].r *= *da, zx[i].i *= *da;
        }
    }
    else {
        ix = 0;
        for (i = 0; i < *n; ++i) {
            zx[ix].r *= *da, zx[ix].i *= *da;
            ix += *incx;
        }
    }
} /* zdscal_ */
