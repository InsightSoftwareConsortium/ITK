#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */
/*                               and moved out of zsvdc.c to separate file */

/* Subroutine */ void zdrot_(n, zx, incx, zy, incy, c, s)
const integer *n;
doublecomplex *zx;
const integer *incx;
doublecomplex *zy;
const integer *incy;
const doublereal *c, *s;
{
    /* Local variables */
    static integer i;
    static doublecomplex ztemp;
    static integer ix, iy;

/*     applies a plane rotation, where the cos and sin (c and s) are */
/*     double precision and the vectors zx and zy are double complex. */
/*     jack dongarra, linpack, 3/11/78. */

    if (*n <= 0) {
        return;
    }
    if (*incx == 1 && *incy == 1) {
        for (i = 0; i < *n; ++i) {
            ztemp.r = *c * zx[i].r + *s * zy[i].r,
            ztemp.i = *c * zx[i].i + *s * zy[i].i;
            zy[i].r = *c * zy[i].r - *s * zx[i].r,
            zy[i].i = *c * zy[i].i - *s * zx[i].i;
            zx[i].r = ztemp.r, zx[i].i = ztemp.i;
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
            ztemp.r = *c * zx[ix].r + *s * zy[iy].r,
            ztemp.i = *c * zx[ix].i + *s * zy[iy].i;
            zy[iy].r = *c * zy[iy].r - *s * zx[ix].r,
            zy[iy].i = *c * zy[iy].i - *s * zx[ix].i;
            zx[ix].r = ztemp.r, zx[ix].i = ztemp.i;
            ix += *incx; iy += *incy;
        }
    }
} /* zdrot_ */
