/* blas/cdotc.f -- translated by f2c (version 20050501).
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

/*<       complex function cdotc(n,cx,incx,cy,incy) >*/
/* Complex */ VOID cdotc_(complex * ret_val, integer *n, complex *cx, integer
        *incx, complex *cy, integer *incy)
{
    /* System generated locals */
    integer i__1, i__2;
    complex q__1, q__2, q__3;

    /* Builtin functions */
    void r_cnjg(complex *, complex *);

    /* Local variables */
    integer i__, ix, iy;
    complex ctemp;


/*     forms the dot product of two vectors, conjugating the first */
/*     vector. */
/*     jack dongarra, linpack,  3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

/*<       complex cx(*),cy(*),ctemp >*/
/*<       integer i,incx,incy,ix,iy,n >*/

/*<       ctemp = (0.0,0.0) >*/
    /* Parameter adjustments */
    --cy;
    --cx;

    /* Function Body */
    ctemp.r = (float)0., ctemp.i = (float)0.;
/*<       cdotc = (0.0,0.0) >*/
     ret_val->r = (float)0.,  ret_val->i = (float)0.;
/*<       if(n.le.0)return >*/
    if (*n <= 0) {
        return ;
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
/*<         ctemp = ctemp + conjg(cx(ix))*cy(iy) >*/
        r_cnjg(&q__3, &cx[ix]);
        i__2 = iy;
        q__2.r = q__3.r * cy[i__2].r - q__3.i * cy[i__2].i, q__2.i = q__3.r *
                cy[i__2].i + q__3.i * cy[i__2].r;
        q__1.r = ctemp.r + q__2.r, q__1.i = ctemp.i + q__2.i;
        ctemp.r = q__1.r, ctemp.i = q__1.i;
/*<         ix = ix + incx >*/
        ix += *incx;
/*<         iy = iy + incy >*/
        iy += *incy;
/*<    10 continue >*/
/* L10: */
    }
/*<       cdotc = ctemp >*/
     ret_val->r = ctemp.r,  ret_val->i = ctemp.i;
/*<       return >*/
    return ;

/*        code for both increments equal to 1 */

/*<    20 do 30 i = 1,n >*/
L20:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         ctemp = ctemp + conjg(cx(i))*cy(i) >*/
        r_cnjg(&q__3, &cx[i__]);
        i__2 = i__;
        q__2.r = q__3.r * cy[i__2].r - q__3.i * cy[i__2].i, q__2.i = q__3.r *
                cy[i__2].i + q__3.i * cy[i__2].r;
        q__1.r = ctemp.r + q__2.r, q__1.i = ctemp.i + q__2.i;
        ctemp.r = q__1.r, ctemp.i = q__1.i;
/*<    30 continue >*/
/* L30: */
    }
/*<       cdotc = ctemp >*/
     ret_val->r = ctemp.r,  ret_val->i = ctemp.i;
/*<       return >*/
    return ;
/*<       end >*/
} /* cdotc_ */

#ifdef __cplusplus
        }
#endif
