#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Subroutine */ void zcopy_(n, zx, incx, zy, incy)
const integer *n;
const doublecomplex *zx;
const integer *incx;
doublecomplex *zy;
const integer *incy;
{
    /* Local variables */
    static integer i, ix, iy;

/*     copies a vector, x, to a vector, y. */
/*     jack dongarra, linpack, 4/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

    if (*n <= 0) {
        return;
    }
    if (*incx == 1 && *incy == 1) {
        for (i = 0; i < *n; ++i) {
            zy[i].r = zx[i].r, zy[i].i = zx[i].i;
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
            zy[iy].r = zx[ix].r, zy[iy].i = zx[ix].i;
            ix += *incx; iy += *incy;
        }
    }
} /* zcopy_ */
