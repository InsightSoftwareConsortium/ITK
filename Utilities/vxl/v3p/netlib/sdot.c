#include "f2c.h"
#include "netlib.h"

real sdot_(n, sx, incx, sy, incy)
const integer *n;
const real *sx;
const integer *incx;
const real *sy;
const integer *incy;
{
    /* Local variables */
    static integer i, m;
    static real stemp;
    static integer ix, iy;

/*     forms the dot product of two vectors. */
/*     uses unrolled loops for increments equal to one. */
/*     jack dongarra, linpack, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

    stemp = 0.f;
    if (*n <= 0) {
        return stemp;
    }
    if (*incx == 1 && *incy == 1) {
        m = *n % 5;
        for (i = 0; i < m; ++i) {
            stemp += sx[i] * sy[i];
        }
        for (i = m; i < *n; i += 5) {
            stemp += sx[i] * sy[i] +
                     sx[i + 1] * sy[i + 1] +
                     sx[i + 2] * sy[i + 2] +
                     sx[i + 3] * sy[i + 3] +
                     sx[i + 4] * sy[i + 4];
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
            stemp += sx[ix] * sy[iy];
            ix += *incx; iy += *incy;
        }
    }
    return stemp;
} /* sdot_ */
