#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Double Complex */ void zdotu_(ret_val, n, zx, incx, zy, incy)
doublecomplex *ret_val;
const integer *n;
const doublecomplex *zx;
const integer *incx;
const doublecomplex *zy;
const integer *incy;
{
    /* Local variables */
    static integer i;
    static integer ix, iy;

/*     forms the dot product of two vectors. */
/*     jack dongarra, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

    ret_val->r = 0., ret_val->i = 0.;
    if (*n <= 0) {
        return ;
    }
    if (*incx == 1 && *incy == 1) {
        for (i = 0; i < *n; ++i) {
            ret_val->r += zx[i].r * zy[i].r - zx[i].i * zy[i].i,
            ret_val->i += zx[i].r * zy[i].i + zx[i].i * zy[i].r;
        }
    }
    else
    {
        ix = 0; iy = 0;
        if (*incx < 0) {
            ix = (1-(*n)) * *incx;
        }
        if (*incy < 0) {
            iy = (1-(*n)) * *incy;
        }
        for (i = 0; i < *n; ++i) {
            ret_val->r += zx[ix].r * zy[iy].r - zx[ix].i * zy[iy].i,
            ret_val->i += zx[ix].r * zy[iy].i + zx[ix].i * zy[iy].r;
            ix += *incx; iy += *incy;
        }
    }

    return;
} /* zdotu_ */

