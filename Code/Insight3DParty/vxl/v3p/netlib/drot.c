/* drot.f -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"
#ifdef KR_headers
/* Subroutine */ int drot_(n, dx, incx, dy, incy, c, s)
integer *n;
doublereal *dx;
integer *incx;
doublereal *dy;
integer *incy;
doublereal *c, *s;
#else
int drot_(
integer *n,
doublereal *dx,
integer *incx,
doublereal *dy,
integer *incy,
doublereal *c, doublereal*s)
#endif
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i;
    static doublereal dtemp;
    static integer ix, iy;


/*     applies a plane rotation. */
/*     jack dongarra, linpack, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */


    /* Parameter adjustments */
    --dy;
    --dx;

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
	dtemp = *c * dx[ix] + *s * dy[iy];
	dy[iy] = *c * dy[iy] - *s * dx[ix];
	dx[ix] = dtemp;
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    return 0;

/*       code for both increments equal to 1 */

L20:
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	dtemp = *c * dx[i] + *s * dy[i];
	dy[i] = *c * dy[i] - *s * dx[i];
	dx[i] = dtemp;
/* L30: */
    }
    return 0;
} /* drot_ */

