#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Subroutine */ void ccopy_(n, cx, incx, cy, incy)
const integer *n;
const complex *cx;
const integer *incx;
complex *cy;
const integer *incy;
{
    /* Local variables */
    static integer i, ix, iy;

/*     copies a vector, x, to a vector, y. */
/*     jack dongarra, linpack, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

    if (*n <= 0) {
        return;
    }
    if (*incx == 1 && *incy == 1) {
        for (i = 0; i < *n; ++i) {
            cy[i].r = cx[i].r, cy[i].i = cx[i].i;
        }
    }
    else {
        ix = 0; iy = 0;
        if (*incx < 0) {
            ix = (1-(*n)) * *incx;
        }
        if (*incy < 0) {
            iy = (1-(*n)) * *incy;
        }
        for (i = 0; i < *n; ++i) {
            cy[iy].r = cx[ix].r, cy[iy].i = cx[ix].i;
            ix += *incx; iy += *incy;
        }
    }
} /* ccopy_ */
