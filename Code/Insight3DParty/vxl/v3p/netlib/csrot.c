/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int csrot_(n, cx, incx, cy, incy, c, s)
integer *n;
complex *cx;
integer *incx;
complex *cy;
integer *incy;
real *c, *s;
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    complex q__1, q__2, q__3;

    /* Local variables */
    static integer i;
    static complex ctemp;
    static integer ix, iy;


/*     applies a plane rotation, where the cos and sin (c and s) are real 
*/
/*     and the vectors cx and cy are complex. */
/*     jack dongarra, linpack, 3/11/78. */


    /* Parameter adjustments */
    --cy;
    --cx;

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
	i__2 = ix;
	q__2.r = *c * cx[i__2].r, q__2.i = *c * cx[i__2].i;
	i__3 = iy;
	q__3.r = *s * cy[i__3].r, q__3.i = *s * cy[i__3].i;
	q__1.r = q__2.r + q__3.r, q__1.i = q__2.i + q__3.i;
	ctemp.r = q__1.r, ctemp.i = q__1.i;
	i__2 = iy;
	i__3 = iy;
	q__2.r = *c * cy[i__3].r, q__2.i = *c * cy[i__3].i;
	i__4 = ix;
	q__3.r = *s * cx[i__4].r, q__3.i = *s * cx[i__4].i;
	q__1.r = q__2.r - q__3.r, q__1.i = q__2.i - q__3.i;
	cy[i__2].r = q__1.r, cy[i__2].i = q__1.i;
	i__2 = ix;
	cx[i__2].r = ctemp.r, cx[i__2].i = ctemp.i;
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    return 0;

/*       code for both increments equal to 1 */

L20:
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	i__2 = i;
	q__2.r = *c * cx[i__2].r, q__2.i = *c * cx[i__2].i;
	i__3 = i;
	q__3.r = *s * cy[i__3].r, q__3.i = *s * cy[i__3].i;
	q__1.r = q__2.r + q__3.r, q__1.i = q__2.i + q__3.i;
	ctemp.r = q__1.r, ctemp.i = q__1.i;
	i__2 = i;
	i__3 = i;
	q__2.r = *c * cy[i__3].r, q__2.i = *c * cy[i__3].i;
	i__4 = i;
	q__3.r = *s * cx[i__4].r, q__3.i = *s * cx[i__4].i;
	q__1.r = q__2.r - q__3.r, q__1.i = q__2.i - q__3.i;
	cy[i__2].r = q__1.r, cy[i__2].i = q__1.i;
	i__2 = i;
	cx[i__2].r = ctemp.r, cx[i__2].i = ctemp.i;
/* L30: */
    }
    return 0;
} /* csrot_ */

