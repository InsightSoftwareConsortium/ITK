#include "f2c.h"
#include "netlib.h"
#ifdef KR_headers
/* Subroutine */ void dcopy_(n, dx, incx, dy, incy)
const integer *n;
const doublereal *dx;
const integer *incx;
doublereal *dy;
const integer *incy;
#else
void dcopy_(const integer *n, const doublereal *dx, const integer *incx, doublereal *dy, const integer *incy)
#endif
{
    /* Local variables */
    static integer i, ix, iy;

/*     copies a vector, x, to a vector, y. */
/*     uses unrolled loops for increments equal to 1. */
/*     jack dongarra, linpack, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

    if (*n <= 0) {
        return;
    }
    if (*incx == 1 && *incy == 1) {
        for (i = 0; i < *n; ++i) {
            dy[i] = dx[i];
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
            dy[iy] = dx[ix];
            ix += *incx; iy += *incy;
        }
    }
    return;
} /* dcopy_ */
