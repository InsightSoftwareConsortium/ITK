/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int zcopy_(n, zx, incx, zy, incy)
integer *n;
doublecomplex *zx;
integer *incx;
doublecomplex *zy;
integer *incy;
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Local variables */
    static integer i, ix, iy;


/*     copies a vector, x, to a vector, y. */
/*     jack dongarra, linpack, 4/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */


    /* Parameter adjustments */
    --zy;
    --zx;

    /* Function Body */
    if (*n <= 0) {
        return 0;
    }
    if (*incx == 1 && *incy == 1) {
        goto L20;
    }

/*        code for unequal increments or equal increments */
/*          not equal to 1 */

    ix = 1;
    iy = 1;
    if (*incx < 0) {
        ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
        iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        i__2 = iy;
        i__3 = ix;
        zy[i__2].r = zx[i__3].r, zy[i__2].i = zx[i__3].i;
        ix += *incx;
        iy += *incy;
/* L10: */
    }
    return 0;

/*        code for both increments equal to 1 */

L20:
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        i__2 = i;
        i__3 = i;
        zy[i__2].r = zx[i__3].r, zy[i__2].i = zx[i__3].i;
/* L30: */
    }
    return 0;
} /* zcopy_ */

