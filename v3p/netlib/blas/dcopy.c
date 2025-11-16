/* blas/dcopy.f -- translated by f2c (version 20050501).
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

/*<       subroutine  dcopy(n,dx,incx,dy,incy) >*/
/* Subroutine */ int dcopy_(integer *n, doublereal *dx, integer *incx,
        doublereal *dy, integer *incy)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__, m, ix, iy, mp1;


/*     copies a vector, x, to a vector, y. */
/*     uses unrolled loops for increments equal to one. */
/*     jack dongarra, linpack, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

/*<       double precision dx(*),dy(*) >*/
/*<       integer i,incx,incy,ix,iy,m,mp1,n >*/

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

/*        code for unequal increments or equal increments */
/*          not equal to 1 */

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
/*<         dy(iy) = dx(ix) >*/
        dy[iy] = dx[ix];
/*<         ix = ix + incx >*/
        ix += *incx;
/*<         iy = iy + incy >*/
        iy += *incy;
/*<    10 continue >*/
/* L10: */
    }
/*<       return >*/
    return 0;

/*        code for both increments equal to 1 */


/*        clean-up loop */

/*<    20 m = mod(n,7) >*/
L20:
    m = *n % 7;
/*<       if( m .eq. 0 ) go to 40 >*/
    if (m == 0) {
        goto L40;
    }
/*<       do 30 i = 1,m >*/
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         dy(i) = dx(i) >*/
        dy[i__] = dx[i__];
/*<    30 continue >*/
/* L30: */
    }
/*<       if( n .lt. 7 ) return >*/
    if (*n < 7) {
        return 0;
    }
/*<    40 mp1 = m + 1 >*/
L40:
    mp1 = m + 1;
/*<       do 50 i = mp1,n,7 >*/
    i__1 = *n;
    for (i__ = mp1; i__ <= i__1; i__ += 7) {
/*<         dy(i) = dx(i) >*/
        dy[i__] = dx[i__];
/*<         dy(i + 1) = dx(i + 1) >*/
        dy[i__ + 1] = dx[i__ + 1];
/*<         dy(i + 2) = dx(i + 2) >*/
        dy[i__ + 2] = dx[i__ + 2];
/*<         dy(i + 3) = dx(i + 3) >*/
        dy[i__ + 3] = dx[i__ + 3];
/*<         dy(i + 4) = dx(i + 4) >*/
        dy[i__ + 4] = dx[i__ + 4];
/*<         dy(i + 5) = dx(i + 5) >*/
        dy[i__ + 5] = dx[i__ + 5];
/*<         dy(i + 6) = dx(i + 6) >*/
        dy[i__ + 6] = dx[i__ + 6];
/*<    50 continue >*/
/* L50: */
    }
/*<       return >*/
    return 0;
/*<       end >*/
} /* dcopy_ */

#ifdef __cplusplus
        }
#endif
