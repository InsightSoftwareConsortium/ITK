#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

integer izamax_(n, zx, incx)
const integer *n;
const doublecomplex *zx;
const integer *incx;
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    static doublereal smax, temp;
    static integer i;
    static integer ix;

/*     finds the index of element having max. absolute value. */
/*     jack dongarra, 1/15/85. */
/*     modified 3/93 to return if incx .le. 0. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

    if (*n < 1 || *incx <= 0) {
        return 0;
    }
    if (*n == 1) {
        return 1;
    }
    ret_val = 1;
    if (*incx == 1) {
        smax = abs(zx[0].r) + abs(zx[0].i);
        for (i = 1; i < *n; ++i) {
            temp = abs(zx[i].r) + abs(zx[i].i);
            if (temp > smax) { ret_val = i+1; smax = temp; }
        }
    }
    else {
        smax = abs(zx[0].r) + abs(zx[0].i);
        for (i = 1, ix = *incx; i < *n; ++i, ix += *incx) {
            temp = abs(zx[ix].r) + abs(zx[ix].i);
            if (temp > smax) { ret_val = i+1; smax = temp; }
        }
    }
    return ret_val;
} /* izamax_ */

