/* srot.f -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int srot_(n, sx, incx, sy, incy, c, s)
integer *n;
real *sx;
integer *incx;
real *sy;
integer *incy;
real *c, *s;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i;
    static real stemp;
    static integer ix, iy;


/*     applies a plane rotation. */
/*     jack dongarra, linpack, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */


    /* Parameter adjustments */
    --sy;
    --sx;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }

/*       code for unequal increments or equal increments not equal */
/*         to 1 */

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
	stemp = *c * sx[ix] + *s * sy[iy];
	sy[iy] = *c * sy[iy] - *s * sx[ix];
	sx[ix] = stemp;
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    return 0;

/*       code for both increments equal to 1 */

L20:
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	stemp = *c * sx[i] + *s * sy[i];
	sy[i] = *c * sy[i] - *s * sx[i];
	sx[i] = stemp;
/* L30: */
    }
    return 0;
} /* srot_ */

