#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */
/*                               and moved out of zsvdc.c to separate file */

/* Subroutine */ void zaxpy_(n, za, zx, incx, zy, incy)
const integer *n;
const doublecomplex *za, *zx;
const integer *incx;
doublecomplex *zy;
const integer *incy;
{
    /* System generated locals */
    doublecomplex z__1;

    /* Local variables */
    static integer i, ix, iy;

/*     constant times a vector plus a vector. */
/*     jack dongarra, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

    if (*n <= 0) {
        return;
    }
    if (za->r == 0. && za->i == 0.) {
        return;
    }
    if (*incx == 1 && *incy == 1) {
        for (i = 0; i < *n; ++i) {
            z__1.r = za->r * zx[i].r - za->i * zx[i].i,
            z__1.i = za->r * zx[i].i + za->i * zx[i].r;
            zy[i].r += z__1.r, zy[i].i += z__1.i;
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
            z__1.r = za->r * zx[ix].r - za->i * zx[ix].i,
            z__1.i = za->r * zx[ix].i + za->i * zx[ix].r;
            zy[iy].r += z__1.r, zy[iy].i += z__1.i;
            ix += *incx; iy += *incy;
        }
    }
} /* zaxpy_ */
