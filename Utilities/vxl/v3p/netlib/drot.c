#include "f2c.h"
#include "netlib.h"
#ifdef KR_headers
/* Subroutine */ void drot_(n, dx, incx, dy, incy, c, s)
const integer *n;
doublereal *dx;
const integer *incx;
doublereal *dy;
const integer *incy;
const doublereal *c, *s;
#else
void drot_(const integer *n, doublereal *dx, const integer *incx, doublereal *dy, const integer *incy,
           const doublereal *c, const doublereal*s)
#endif
{
    /* Local variables */
    static integer i;
    static doublereal dtemp;
    static integer ix, iy;

/*     applies a plane rotation. */
/*     jack dongarra, linpack, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

    if (*n <= 0) {
        return;
    }
    if (*incx == 1 && *incy == 1) {
        for (i = 0; i < *n; ++i) {
            dtemp = *c * dx[i] + *s * dy[i];
            dy[i] = *c * dy[i] - *s * dx[i];
            dx[i] = dtemp;
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
            dtemp  = *c * dx[ix] + *s * dy[iy];
            dy[iy] = *c * dy[iy] - *s * dx[ix];
            dx[ix] = dtemp;
            ix += *incx; iy += *incy;
        }
    }
} /* drot_ */
