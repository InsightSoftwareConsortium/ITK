/*  -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Double Complex */ void zdotu_( ret_val, n, zx, incx, zy, incy)
doublecomplex * ret_val;
integer *n;
doublecomplex *zx;
integer *incx;
doublecomplex *zy;
integer *incy;
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublecomplex z__1, z__2;

    /* Local variables */
    static integer i;
    static doublecomplex ztemp;
    static integer ix, iy;


/*     forms the dot product of two vectors. */
/*     jack dongarra, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

    /* Parameter adjustments */
    --zy;
    --zx;

    /* Function Body */
    ztemp.r = 0., ztemp.i = 0.;
     ret_val->r = 0.,  ret_val->i = 0.;
    if (*n <= 0) {
	return ;
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
	i__2 = ix;
	i__3 = iy;
	z__2.r = zx[i__2].r * zy[i__3].r - zx[i__2].i * zy[i__3].i, z__2.i = 
		zx[i__2].r * zy[i__3].i + zx[i__2].i * zy[i__3].r;
	z__1.r = ztemp.r + z__2.r, z__1.i = ztemp.i + z__2.i;
	ztemp.r = z__1.r, ztemp.i = z__1.i;
	ix += *incx;
	iy += *incy;
/* L10: */
    }
     ret_val->r = ztemp.r,  ret_val->i = ztemp.i;
    return ;

/*        code for both increments equal to 1 */

L20:
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	i__2 = i;
	i__3 = i;
	z__2.r = zx[i__2].r * zy[i__3].r - zx[i__2].i * zy[i__3].i, z__2.i = 
		zx[i__2].r * zy[i__3].i + zx[i__2].i * zy[i__3].r;
	z__1.r = ztemp.r + z__2.r, z__1.i = ztemp.i + z__2.i;
	ztemp.r = z__1.r, ztemp.i = z__1.i;
/* L30: */
    }
     ret_val->r = ztemp.r,  ret_val->i = ztemp.i;
    return ;
} /* zdotu_ */

