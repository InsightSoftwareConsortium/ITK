#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */
/*                               and moved out of zsvdc.c to separate file */

/* Subroutine */ void zswap_(n, zx, incx, zy, incy)
const integer *n;
doublecomplex *zx;
const integer *incx;
doublecomplex *zy;
const integer *incy;
{
    /* Local variables */
    static integer i;
    static doublecomplex ztemp;
    static integer ix, iy;

/*     interchanges two vectors. */
/*     jack dongarra, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

    if (*n <= 0) {
        return;
    }
    if (*incx == 1 && *incy == 1) {
        for (i = 0; i < *n; ++i) {
            ztemp.r = zx[i].r, ztemp.i = zx[i].i;
            zx[i].r = zy[i].r, zx[i].i = zy[i].i;
            zy[i].r = ztemp.r, zy[i].i = ztemp.i;
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
            ztemp.r = zx[ix].r, ztemp.i = zx[ix].i;
            zx[ix].r = zy[iy].r, zx[ix].i = zy[iy].i;
            zy[iy].r = ztemp.r, zy[iy].i = ztemp.i;
            ix += *incx; iy += *incy;
        }
    }
} /* zswap_ */
