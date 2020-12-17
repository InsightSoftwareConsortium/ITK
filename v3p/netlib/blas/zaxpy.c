/* blas/zaxpy.f -- translated by f2c (version 20050501).
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

/*<       subroutine zaxpy(n,za,zx,incx,zy,incy) >*/
/* Subroutine */ int zaxpy_(integer *n, doublecomplex *za, doublecomplex *zx,
        integer *incx, doublecomplex *zy, integer *incy)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    doublecomplex z__1, z__2;

    /* Local variables */
    integer i__, ix, iy;
    extern doublereal dcabs1_(doublecomplex *);


/*     constant times a vector plus a vector. */
/*     jack dongarra, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

/*<       double complex zx(*),zy(*),za >*/
/*<       integer i,incx,incy,ix,iy,n >*/
/*<       double precision dcabs1 >*/
/*<       if(n.le.0)return >*/
    /* Parameter adjustments */
    --zy;
    --zx;

    /* Function Body */
    if (*n <= 0) {
        return 0;
    }
/*<       if (dcabs1(za) .eq. 0.0d0) return >*/
    if (dcabs1_(za) == 0.) {
        return 0;
    }
/*<       if (incx.eq.1.and.incy.eq.1)go to 20 >*/
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
/*<         zy(iy) = zy(iy) + za*zx(ix) >*/
        i__2 = iy;
        i__3 = iy;
        i__4 = ix;
        z__2.r = za->r * zx[i__4].r - za->i * zx[i__4].i, z__2.i = za->r * zx[
                i__4].i + za->i * zx[i__4].r;
        z__1.r = zy[i__3].r + z__2.r, z__1.i = zy[i__3].i + z__2.i;
        zy[i__2].r = z__1.r, zy[i__2].i = z__1.i;
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

/*<    20 do 30 i = 1,n >*/
L20:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         zy(i) = zy(i) + za*zx(i) >*/
        i__2 = i__;
        i__3 = i__;
        i__4 = i__;
        z__2.r = za->r * zx[i__4].r - za->i * zx[i__4].i, z__2.i = za->r * zx[
                i__4].i + za->i * zx[i__4].r;
        z__1.r = zy[i__3].r + z__2.r, z__1.i = zy[i__3].i + z__2.i;
        zy[i__2].r = z__1.r, zy[i__2].i = z__1.i;
/*<    30 continue >*/
/* L30: */
    }
/*<       return >*/
    return 0;
/*<       end >*/
} /* zaxpy_ */

#ifdef __cplusplus
        }
#endif
