/* blas/cswap.f -- translated by f2c (version 20050501).
   You must link the resulting object file with libf2c:
        on Microsoft Windows system, link with libf2c.lib;
        on Linux or Unix systems, link with .../path/to/libf2c.a -lm
        or, if you install libf2c.a in a standard place, with -lf2c -lm
        -- in that order, at the end of the command line, as in
                cc *.o -lf2c -lm
        Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

                http://www.netlib.org/f2c/libf2c.zip
*/

#ifdef __cplusplus
extern "C" {
#endif
#include "v3p_netlib.h"

/*<       subroutine  cswap (n,cx,incx,cy,incy) >*/
/* Subroutine */ int cswap_(integer *n, complex *cx, integer *incx, complex *
        cy, integer *incy)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Local variables */
    integer i__, ix, iy;
    complex ctemp;


/*     interchanges two vectors. */
/*     jack dongarra, linpack, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

/*<       complex cx(*),cy(*),ctemp >*/
/*<       integer i,incx,incy,ix,iy,n >*/

/*<       if(n.le.0)return >*/
    /* Parameter adjustments */
    --cy;
    --cx;

    /* Function Body */
    if (*n <= 0) {
        return 0;
    }
/*<       if(incx.eq.1.and.incy.eq.1)go to 20 >*/
    if (*incx == 1 && *incy == 1) {
        goto L20;
    }

/*       code for unequal increments or equal increments not equal */
/*         to 1 */

/*<       ix = 1 >*/
    ix = 1;
/*<       iy = 1 >*/
    iy = 1;
/*<       if(incx.lt.0)ix = (-n+1)*incx + 1 >*/
    if (*incx < 0) {
        ix = (-(*n) + 1) * *incx + 1;
    }
/*<       if(incy.lt.0)iy = (-n+1)*incy + 1 >*/
    if (*incy < 0) {
        iy = (-(*n) + 1) * *incy + 1;
    }
/*<       do 10 i = 1,n >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         ctemp = cx(ix) >*/
        i__2 = ix;
        ctemp.r = cx[i__2].r, ctemp.i = cx[i__2].i;
/*<         cx(ix) = cy(iy) >*/
        i__2 = ix;
        i__3 = iy;
        cx[i__2].r = cy[i__3].r, cx[i__2].i = cy[i__3].i;
/*<         cy(iy) = ctemp >*/
        i__2 = iy;
        cy[i__2].r = ctemp.r, cy[i__2].i = ctemp.i;
/*<         ix = ix + incx >*/
        ix += *incx;
/*<         iy = iy + incy >*/
        iy += *incy;
/*<    10 continue >*/
/* L10: */
    }
/*<       return >*/
    return 0;

/*       code for both increments equal to 1 */
/*<    20 do 30 i = 1,n >*/
L20:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         ctemp = cx(i) >*/
        i__2 = i__;
        ctemp.r = cx[i__2].r, ctemp.i = cx[i__2].i;
/*<         cx(i) = cy(i) >*/
        i__2 = i__;
        i__3 = i__;
        cx[i__2].r = cy[i__3].r, cx[i__2].i = cy[i__3].i;
/*<         cy(i) = ctemp >*/
        i__2 = i__;
        cy[i__2].r = ctemp.r, cy[i__2].i = ctemp.i;
/*<    30 continue >*/
/* L30: */
    }
/*<       return >*/
    return 0;
/*<       end >*/
} /* cswap_ */

#ifdef __cplusplus
        }
#endif
