/* blas/cscal.f -- translated by f2c (version 20050501).
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

/*<       subroutine  cscal(n,ca,cx,incx) >*/
/* Subroutine */ int cscal_(integer *n, complex *ca, complex *cx, integer *
        incx)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    complex q__1;

    /* Local variables */
    integer i__, nincx;


/*     scales a vector by a constant. */
/*     jack dongarra, linpack,  3/11/78. */
/*     modified 3/93 to return if incx .le. 0. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

/*<       complex ca,cx(*) >*/
/*<       integer i,incx,n,nincx >*/

/*<       if( n.le.0 .or. incx.le.0 )return >*/
    /* Parameter adjustments */
    --cx;

    /* Function Body */
    if (*n <= 0 || *incx <= 0) {
        return 0;
    }
/*<       if(incx.eq.1)go to 20 >*/
    if (*incx == 1) {
        goto L20;
    }

/*        code for increment not equal to 1 */

/*<       nincx = n*incx >*/
    nincx = *n * *incx;
/*<       do 10 i = 1,nincx,incx >*/
    i__1 = nincx;
    i__2 = *incx;
    for (i__ = 1; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
/*<         cx(i) = ca*cx(i) >*/
        i__3 = i__;
        i__4 = i__;
        q__1.r = ca->r * cx[i__4].r - ca->i * cx[i__4].i, q__1.i = ca->r * cx[
                i__4].i + ca->i * cx[i__4].r;
        cx[i__3].r = q__1.r, cx[i__3].i = q__1.i;
/*<    10 continue >*/
/* L10: */
    }
/*<       return >*/
    return 0;

/*        code for increment equal to 1 */

/*<    20 do 30 i = 1,n >*/
L20:
    i__2 = *n;
    for (i__ = 1; i__ <= i__2; ++i__) {
/*<         cx(i) = ca*cx(i) >*/
        i__1 = i__;
        i__3 = i__;
        q__1.r = ca->r * cx[i__3].r - ca->i * cx[i__3].i, q__1.i = ca->r * cx[
                i__3].i + ca->i * cx[i__3].r;
        cx[i__1].r = q__1.r, cx[i__1].i = q__1.i;
/*<    30 continue >*/
/* L30: */
    }
/*<       return >*/
    return 0;
/*<       end >*/
} /* cscal_ */

#ifdef __cplusplus
        }
#endif
