#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Subroutine */ void cswap_(n, cx, incx, cy, incy)
const integer *n;
complex *cx;
const integer *incx;
complex *cy;
const integer *incy;
{
    /* Local variables */
    static integer i;
    static complex ctemp;
    static integer ix, iy;

/*     interchanges two vectors. */
/*     jack dongarra, linpack, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

    if (*n <= 0) {
        return;
    }
    if (*incx == 1 && *incy == 1) {
        for (i = 0; i < *n; ++i) {
            ctemp.r = cx[i].r, ctemp.i = cx[i].i;
            cx[i].r = cy[i].r, cx[i].i = cy[i].i;
            cy[i].r = ctemp.r, cy[i].i = ctemp.i;
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
            ctemp.r = cx[ix].r, ctemp.i = cx[ix].i;
            cx[ix].r = cy[iy].r, cx[ix].i = cy[iy].i;
            cy[iy].r = ctemp.r, cy[iy].i = ctemp.i;
            ix += *incx; iy += *incy;
        }
    }
} /* cswap_ */
