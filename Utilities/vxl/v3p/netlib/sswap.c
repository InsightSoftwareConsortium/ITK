#include "f2c.h"
#include "netlib.h"

/* Subroutine */ void sswap_(n, sx, incx, sy, incy)
const integer *n;
real *sx;
const integer *incx;
real *sy;
const integer *incy;
{
    /* Local variables */
    static integer i, m;
    static real stemp;
    static integer ix, iy;

/*     interchanges two vectors. */
/*     uses unrolled loops for increments equal to 1. */
/*     jack dongarra, linpack, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

    if (*n <= 0) {
        return;
    }
    if (*incx == 1 && *incy == 1) {
        m = *n % 3;
        for (i = 0; i < m; ++i) {
            stemp = sx[i];
            sx[i] = sy[i];
            sy[i] = stemp;
        }
        for (i = m; i < *n; i += 3) {
            stemp = sx[i];
            sx[i] = sy[i];
            sy[i] = stemp;
            stemp = sx[i + 1];
            sx[i + 1] = sy[i + 1];
            sy[i + 1] = stemp;
            stemp = sx[i + 2];
            sx[i + 2] = sy[i + 2];
            sy[i + 2] = stemp;
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
            stemp = sx[ix];
            sx[ix] = sy[iy];
            sy[iy] = stemp;
            ix += *incx; iy += *incy;
        }
    }
} /* sswap_ */
