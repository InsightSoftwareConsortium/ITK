#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Subroutine */ void csrot_(n, cx, incx, cy, incy, c, s)
const integer *n;
complex *cx;
const integer *incx;
complex *cy;
const integer *incy;
const real *c, *s;
{
    /* Local variables */
    static integer i;
    static complex ctemp;
    static integer ix, iy;

/*     applies a plane rotation, where the cos and sin (c and s) are real */
/*     and the vectors cx and cy are complex. */
/*     jack dongarra, linpack, 3/11/78. */

    if (*n <= 0) {
        return;
    }
    if (*incx == 1 && *incy == 1) {
        for (i = 0; i < *n; ++i) {
            ctemp.r = *c * cx[i].r + *s * cy[i].r,
            ctemp.i = *c * cx[i].i + *s * cy[i].i;
            cy[i].r = *c * cy[i].r - *s * cx[i].r,
            cy[i].i = *c * cy[i].i - *s * cx[i].i;
            cx[i].r = ctemp.r, cx[i].i = ctemp.i;
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
            ctemp.r = *c * cx[ix].r + *s * cy[iy].r,
            ctemp.i = *c * cx[ix].i + *s * cy[iy].i;
            cy[iy].r = *c * cy[iy].r - *s * cx[ix].r,
            cy[iy].i = *c * cy[iy].i - *s * cx[ix].i;
            cx[ix].r = ctemp.r, cx[ix].i = ctemp.i;
            ix += *incx; iy += *incy;
        }
    }
} /* csrot_ */
