/* cdotc.f -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
        -lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Complex */ int cdotc_( ret_val, n, cx, incx, cy, incy)
complex * ret_val;
integer *n;
complex *cx;
integer *incx;
complex *cy;
integer *incy;
{
    /* System generated locals */
    integer i__1, i__2;
    complex q__1, q__2, q__3;

    /* Builtin functions */
    void r_cnjg();

    /* Local variables */
    static integer i;
    static complex ctemp;
    static integer ix, iy;


/*     forms the dot product of two vectors, conjugating the first */
/*     vector. */
/*     jack dongarra, linpack,  3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */


    /* Parameter adjustments */
    --cy;
    --cx;

    /* Function Body */
    ctemp.r = (float)0., ctemp.i = (float)0.;
     ret_val->r = (float)0.,  ret_val->i = (float)0.;
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
        r_cnjg(&q__3, &cx[ix]);
        i__2 = iy;
        q__2.r = q__3.r * cy[i__2].r - q__3.i * cy[i__2].i, q__2.i = q__3.r *
                cy[i__2].i + q__3.i * cy[i__2].r;
        q__1.r = ctemp.r + q__2.r, q__1.i = ctemp.i + q__2.i;
        ctemp.r = q__1.r, ctemp.i = q__1.i;
        ix += *incx;
        iy += *incy;
/* L10: */
    }
     ret_val->r = ctemp.r,  ret_val->i = ctemp.i;
    return 0;

/*        code for both increments equal to 1 */

L20:
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        r_cnjg(&q__3, &cx[i]);
        i__2 = i;
        q__2.r = q__3.r * cy[i__2].r - q__3.i * cy[i__2].i, q__2.i = q__3.r *
                cy[i__2].i + q__3.i * cy[i__2].r;
        q__1.r = ctemp.r + q__2.r, q__1.i = ctemp.i + q__2.i;
        ctemp.r = q__1.r, ctemp.i = q__1.i;
/* L30: */
    }
     ret_val->r = ctemp.r,  ret_val->i = ctemp.i;
    return 0;
} /* cdotc_ */

