#include "f2c.h"
#include "netlib.h"

/* Subroutine */ void saxpy_(n, sa, sx, incx, sy, incy)
const integer *n;
const real *sa, *sx;
const integer *incx;
real *sy;
const integer *incy;
{
    /* Local variables */
    static integer i, m, ix, iy;

/*     constant times a vector plus a vector. */
/*     uses unrolled loop for increments equal to one. */
/*     jack dongarra, linpack, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

    if (*n <= 0) {
        return;
    }
    if (*sa == 0.f) {
        return;
    }
    if (*incx == 1 && *incy == 1) {
        m = *n % 4;
        for (i = 0; i < m; ++i) {
            sy[i] += *sa * sx[i];
        }
        for (i = m; i < *n; i += 4) {
            sy[i] += *sa * sx[i];
            sy[i + 1] += *sa * sx[i + 1];
            sy[i + 2] += *sa * sx[i + 2];
            sy[i + 3] += *sa * sx[i + 3];
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
            sy[iy] += *sa * sx[ix];
            ix += *incx; iy += *incy;
        }
    }
} /* saxpy_ */
