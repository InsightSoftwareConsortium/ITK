#include "f2c.h"
#include "netlib.h"

/* Subroutine */ void scopy_(n, sx, incx, sy, incy)
const integer *n;
const real *sx;
const integer *incx;
real *sy;
const integer *incy;
{
    /* Local variables */
    static integer i, m, ix, iy;

/*     copies a vector, x, to a vector, y. */
/*     uses unrolled loops for increments equal to 1. */
/*     jack dongarra, linpack, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

    if (*n <= 0) {
        return;
    }
    if (*incx == 1 && *incy == 1) {
        m = *n % 7;
        for (i = 0; i < m; ++i) {
            sy[i] = sx[i];
        }
        for (i = m; i < *n; i += 7) {
            sy[i] = sx[i];
            sy[i + 1] = sx[i + 1];
            sy[i + 2] = sx[i + 2];
            sy[i + 3] = sx[i + 3];
            sy[i + 4] = sx[i + 4];
            sy[i + 5] = sx[i + 5];
            sy[i + 6] = sx[i + 6];
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
            sy[iy] = sx[ix];
            ix += *incx; iy += *incy;
        }
    }
} /* scopy_ */
