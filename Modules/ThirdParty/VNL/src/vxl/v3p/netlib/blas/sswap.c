/* blas/sswap.f -- translated by f2c (version 20050501).
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

/*<       subroutine sswap (n,sx,incx,sy,incy) >*/
/* Subroutine */ int sswap_(integer *n, real *sx, integer *incx, real *sy,
        integer *incy)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__, m, ix, iy, mp1;
    real stemp;


/*     interchanges two vectors. */
/*     uses unrolled loops for increments equal to 1. */
/*     jack dongarra, linpack, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

/*<       real sx(*),sy(*),stemp >*/
/*<       integer i,incx,incy,ix,iy,m,mp1,n >*/

/*<       if(n.le.0)return >*/
    /* Parameter adjustments */
    --sy;
    --sx;

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
/*<         stemp = sx(ix) >*/
        stemp = sx[ix];
/*<         sx(ix) = sy(iy) >*/
        sx[ix] = sy[iy];
/*<         sy(iy) = stemp >*/
        sy[iy] = stemp;
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


/*       clean-up loop */

/*<    20 m = mod(n,3) >*/
L20:
    m = *n % 3;
/*<       if( m .eq. 0 ) go to 40 >*/
    if (m == 0) {
        goto L40;
    }
/*<       do 30 i = 1,m >*/
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         stemp = sx(i) >*/
        stemp = sx[i__];
/*<         sx(i) = sy(i) >*/
        sx[i__] = sy[i__];
/*<         sy(i) = stemp >*/
        sy[i__] = stemp;
/*<    30 continue >*/
/* L30: */
    }
/*<       if( n .lt. 3 ) return >*/
    if (*n < 3) {
        return 0;
    }
/*<    40 mp1 = m + 1 >*/
L40:
    mp1 = m + 1;
/*<       do 50 i = mp1,n,3 >*/
    i__1 = *n;
    for (i__ = mp1; i__ <= i__1; i__ += 3) {
/*<         stemp = sx(i) >*/
        stemp = sx[i__];
/*<         sx(i) = sy(i) >*/
        sx[i__] = sy[i__];
/*<         sy(i) = stemp >*/
        sy[i__] = stemp;
/*<         stemp = sx(i + 1) >*/
        stemp = sx[i__ + 1];
/*<         sx(i + 1) = sy(i + 1) >*/
        sx[i__ + 1] = sy[i__ + 1];
/*<         sy(i + 1) = stemp >*/
        sy[i__ + 1] = stemp;
/*<         stemp = sx(i + 2) >*/
        stemp = sx[i__ + 2];
/*<         sx(i + 2) = sy(i + 2) >*/
        sx[i__ + 2] = sy[i__ + 2];
/*<         sy(i + 2) = stemp >*/
        sy[i__ + 2] = stemp;
/*<    50 continue >*/
/* L50: */
    }
/*<       return >*/
    return 0;
/*<       end >*/
} /* sswap_ */

#ifdef __cplusplus
        }
#endif
