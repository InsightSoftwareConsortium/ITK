#include "f2c.h"
#include "netlib.h"

/* Modified by Peter Vanroose, June 2001: manual optimisation and clean-up */

/* Subroutine */ void cscal_(n, ca, cx, incx)
const integer *n;
const complex *ca;
complex *cx;
const integer *incx;
{
    /* System generated locals */
    complex q__1;

    /* Local variables */
    static integer i, ix;

/*     scales a vector by a constant. */
/*     jack dongarra, linpack,  3/11/78. */
/*     modified 3/93 to return if incx .le. 0. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

    if (*n <= 0 || *incx <= 0) {
        return;
    }

    if (*incx == 1) {
        for (i = 0; i < *n; ++i) {
            q__1.r = ca->r * cx[i].r - ca->i * cx[i].i,
            q__1.i = ca->r * cx[i].i + ca->i * cx[i].r;
            cx[i].r = q__1.r, cx[i].i = q__1.i;
        }
    }
    else {
        for (i = ix = 0; i < *n; ++i, ix += *incx) {
            q__1.r = ca->r * cx[ix].r - ca->i * cx[ix].i,
            q__1.i = ca->r * cx[ix].i + ca->i * cx[ix].r;
            cx[ix].r = q__1.r, cx[ix].i = q__1.i;
        }
    }
    return;
} /* cscal_ */

