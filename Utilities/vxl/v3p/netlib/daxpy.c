#include "f2c.h"
#include "netlib.h"
#ifndef KR_headers
void daxpy_(const integer *n, const doublereal *da, const doublereal *dx, const integer *incx, doublereal *dy, const integer *incy)
#else
/* Subroutine */ void daxpy_(n, da, dx, incx, dy, incy)
const integer *n;
const doublereal *da, *dx;
const integer *incx;
doublereal *dy;
const integer *incy;
#endif
{
    /* Local variables */
    static integer i, ix, iy;

/*     constant times a vector plus a vector. */
/*     uses unrolled loops for increments equal to one. */
/*     jack dongarra, linpack, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

    if (*n <= 0) {
        return;
    }
    if (*da == 0.) {
        return;
    }
    if (*incx == 1 && *incy == 1) {
        for (i = 0; i < *n; ++i) {
            dy[i] += *da * dx[i];
        }
    }
    else {
        ix = 0;
        iy = 0;
        if (*incx < 0) {
            ix = (1-(*n)) * *incx;
        }
        if (*incy < 0) {
            iy = (1-(*n)) * *incy;
        }
        for (i = 0; i < *n; ++i) {
            dy[iy] += *da * dx[ix];
            ix += *incx; iy += *incy;
        }
    }
    return;
} /* daxpy_ */
