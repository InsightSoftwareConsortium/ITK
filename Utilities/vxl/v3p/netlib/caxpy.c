#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */


/* Subroutine */ void caxpy_(n, ca, cx, incx, cy, incy)
const integer *n;
const complex *ca, *cx;
const integer *incx;
complex *cy;
const integer *incy;
{
    /* System generated locals */
    complex q__1;

    /* Local variables */
    static integer i, ix, iy;

/*     constant times a vector plus a vector. */
/*     jack dongarra, linpack, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

    if (*n <= 0) {
        return;
    }
    if (ca->r == 0.f && ca->i == 0.f) {
        return;
    }
    if (*incx == 1 && *incy == 1) {
        for (i = 0; i < *n; ++i) {
            q__1.r = ca->r * cx[i].r - ca->i * cx[i].i,
            q__1.i = ca->r * cx[i].i + ca->i * cx[i].r;
            cy[i].r += q__1.r, cy[i].i += q__1.i;
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
            q__1.r = ca->r * cx[ix].r - ca->i * cx[ix].i,
            q__1.i = ca->r * cx[ix].i + ca->i * cx[ix].r;
            cy[iy].r += q__1.r, cy[iy].i += q__1.i;
            ix += *incx; iy += *incy;
        }
    }
} /* caxpy_ */
