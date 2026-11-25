/* blas/caxpy.f -- translated by f2c (version 20050501).
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

/*<       subroutine caxpy(n,ca,cx,incx,cy,incy) >*/
/* Subroutine */ int caxpy_(integer *n, complex *ca, complex *cx, integer *
        incx, complex *cy, integer *incy)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    real r__1, r__2;
    complex q__1, q__2;

    /* Builtin functions */
    double r_imag(complex *);

    /* Local variables */
    integer i__, ix, iy;


/*     constant times a vector plus a vector. */
/*     jack dongarra, linpack, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

/*<       complex cx(*),cy(*),ca >*/
/*<       integer i,incx,incy,ix,iy,n >*/

/*<       if(n.le.0)return >*/
    /* Parameter adjustments */
    --cy;
    --cx;

    /* Function Body */
    if (*n <= 0) {
        return 0;
    }
/*<       if (abs(real(ca)) + abs(aimag(ca)) .eq. 0.0 ) return >*/
    if ((r__1 = ca->r, dabs(r__1)) + (r__2 = r_imag(ca), dabs(r__2)) == 0.f) {
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
/*<         cy(iy) = cy(iy) + ca*cx(ix) >*/
        i__2 = iy;
        i__3 = iy;
        i__4 = ix;
        q__2.r = ca->r * cx[i__4].r - ca->i * cx[i__4].i, q__2.i = ca->r * cx[
                i__4].i + ca->i * cx[i__4].r;
        q__1.r = cy[i__3].r + q__2.r, q__1.i = cy[i__3].i + q__2.i;
        cy[i__2].r = q__1.r, cy[i__2].i = q__1.i;
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
/*<         cy(i) = cy(i) + ca*cx(i) >*/
        i__2 = i__;
        i__3 = i__;
        i__4 = i__;
        q__2.r = ca->r * cx[i__4].r - ca->i * cx[i__4].i, q__2.i = ca->r * cx[
                i__4].i + ca->i * cx[i__4].r;
        q__1.r = cy[i__3].r + q__2.r, q__1.i = cy[i__3].i + q__2.i;
        cy[i__2].r = q__1.r, cy[i__2].i = q__1.i;
/*<    30 continue >*/
/* L30: */
    }
/*<       return >*/
    return 0;
/*<       end >*/
} /* caxpy_ */

#ifdef __cplusplus
        }
#endif
