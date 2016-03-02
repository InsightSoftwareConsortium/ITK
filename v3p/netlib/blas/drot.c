/* blas/drot.f -- translated by f2c (version 20050501).
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

/*<       subroutine  drot (n,dx,incx,dy,incy,c,s) >*/
/* Subroutine */ int drot_(integer *n, doublereal *dx, integer *incx,
        doublereal *dy, integer *incy, doublereal *c__, doublereal *s)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__, ix, iy;
    doublereal dtemp;


/*     applies a plane rotation. */
/*     jack dongarra, linpack, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

/*<       double precision dx(*),dy(*),dtemp,c,s >*/
/*<       integer i,incx,incy,ix,iy,n >*/

/*<       if(n.le.0)return >*/
    /* Parameter adjustments */
    --dy;
    --dx;

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
/*<         dtemp = c*dx(ix) + s*dy(iy) >*/
        dtemp = *c__ * dx[ix] + *s * dy[iy];
/*<         dy(iy) = c*dy(iy) - s*dx(ix) >*/
        dy[iy] = *c__ * dy[iy] - *s * dx[ix];
/*<         dx(ix) = dtemp >*/
        dx[ix] = dtemp;
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
/*<         dtemp = c*dx(i) + s*dy(i) >*/
        dtemp = *c__ * dx[i__] + *s * dy[i__];
/*<         dy(i) = c*dy(i) - s*dx(i) >*/
        dy[i__] = *c__ * dy[i__] - *s * dx[i__];
/*<         dx(i) = dtemp >*/
        dx[i__] = dtemp;
/*<    30 continue >*/
/* L30: */
    }
/*<       return >*/
    return 0;
/*<       end >*/
} /* drot_ */

#ifdef __cplusplus
        }
#endif
