/* blas/zdrot.f -- translated by f2c (version 20050501).
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

/*<       subroutine  zdrot (n,zx,incx,zy,incy,c,s) >*/
/* Subroutine */ int zdrot_(integer *n, doublecomplex *zx, integer *incx,
        doublecomplex *zy, integer *incy, doublereal *c__, doublereal *s)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    doublecomplex z__1, z__2, z__3;

    /* Local variables */
    integer i__, ix, iy;
    doublecomplex ztemp;


/*     applies a plane rotation, where the cos and sin (c and s) are */
/*     double precision and the vectors zx and zy are double complex. */
/*     jack dongarra, linpack, 3/11/78. */

/*<       double complex zx(1),zy(1),ztemp >*/
/*<       double precision c,s >*/
/*<       integer i,incx,incy,ix,iy,n >*/

/*<       if(n.le.0)return >*/
    /* Parameter adjustments */
    --zy;
    --zx;

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
/*<         ztemp = c*zx(ix) + s*zy(iy) >*/
        i__2 = ix;
        z__2.r = *c__ * zx[i__2].r, z__2.i = *c__ * zx[i__2].i;
        i__3 = iy;
        z__3.r = *s * zy[i__3].r, z__3.i = *s * zy[i__3].i;
        z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
        ztemp.r = z__1.r, ztemp.i = z__1.i;
/*<         zy(iy) = c*zy(iy) - s*zx(ix) >*/
        i__2 = iy;
        i__3 = iy;
        z__2.r = *c__ * zy[i__3].r, z__2.i = *c__ * zy[i__3].i;
        i__4 = ix;
        z__3.r = *s * zx[i__4].r, z__3.i = *s * zx[i__4].i;
        z__1.r = z__2.r - z__3.r, z__1.i = z__2.i - z__3.i;
        zy[i__2].r = z__1.r, zy[i__2].i = z__1.i;
/*<         zx(ix) = ztemp >*/
        i__2 = ix;
        zx[i__2].r = ztemp.r, zx[i__2].i = ztemp.i;
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
/*<         ztemp = c*zx(i) + s*zy(i) >*/
        i__2 = i__;
        z__2.r = *c__ * zx[i__2].r, z__2.i = *c__ * zx[i__2].i;
        i__3 = i__;
        z__3.r = *s * zy[i__3].r, z__3.i = *s * zy[i__3].i;
        z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
        ztemp.r = z__1.r, ztemp.i = z__1.i;
/*<         zy(i) = c*zy(i) - s*zx(i) >*/
        i__2 = i__;
        i__3 = i__;
        z__2.r = *c__ * zy[i__3].r, z__2.i = *c__ * zy[i__3].i;
        i__4 = i__;
        z__3.r = *s * zx[i__4].r, z__3.i = *s * zx[i__4].i;
        z__1.r = z__2.r - z__3.r, z__1.i = z__2.i - z__3.i;
        zy[i__2].r = z__1.r, zy[i__2].i = z__1.i;
/*<         zx(i) = ztemp >*/
        i__2 = i__;
        zx[i__2].r = ztemp.r, zx[i__2].i = ztemp.i;
/*<    30 continue >*/
/* L30: */
    }
/*<       return >*/
    return 0;
/*<       end >*/
} /* zdrot_ */

#ifdef __cplusplus
        }
#endif
