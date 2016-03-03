/* blas/dasum.f -- translated by f2c (version 20050501).
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

/*<       double precision function dasum(n,dx,incx) >*/
doublereal dasum_(integer *n, doublereal *dx, integer *incx)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal ret_val, d__1, d__2, d__3, d__4, d__5, d__6;

    /* Local variables */
    integer i__, m, mp1;
    doublereal dtemp;
    integer nincx;


/*     takes the sum of the absolute values. */
/*     jack dongarra, linpack, 3/11/78. */
/*     modified 3/93 to return if incx .le. 0. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

/*<       double precision dx(*),dtemp >*/
/*<       integer i,incx,m,mp1,n,nincx >*/

/*<       dasum = 0.0d0 >*/
    /* Parameter adjustments */
    --dx;

    /* Function Body */
    ret_val = 0.;
/*<       dtemp = 0.0d0 >*/
    dtemp = 0.;
/*<       if( n.le.0 .or. incx.le.0 )return >*/
    if (*n <= 0 || *incx <= 0) {
        return ret_val;
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
/*<         dtemp = dtemp + dabs(dx(i)) >*/
        dtemp += (d__1 = dx[i__], abs(d__1));
/*<    10 continue >*/
/* L10: */
    }
/*<       dasum = dtemp >*/
    ret_val = dtemp;
/*<       return >*/
    return ret_val;

/*        code for increment equal to 1 */


/*        clean-up loop */

/*<    20 m = mod(n,6) >*/
L20:
    m = *n % 6;
/*<       if( m .eq. 0 ) go to 40 >*/
    if (m == 0) {
        goto L40;
    }
/*<       do 30 i = 1,m >*/
    i__2 = m;
    for (i__ = 1; i__ <= i__2; ++i__) {
/*<         dtemp = dtemp + dabs(dx(i)) >*/
        dtemp += (d__1 = dx[i__], abs(d__1));
/*<    30 continue >*/
/* L30: */
    }
/*<       if( n .lt. 6 ) go to 60 >*/
    if (*n < 6) {
        goto L60;
    }
/*<    40 mp1 = m + 1 >*/
L40:
    mp1 = m + 1;
/*<       do 50 i = mp1,n,6 >*/
    i__2 = *n;
    for (i__ = mp1; i__ <= i__2; i__ += 6) {
/*<    >*/
        dtemp = dtemp + (d__1 = dx[i__], abs(d__1)) + (d__2 = dx[i__ + 1],
                abs(d__2)) + (d__3 = dx[i__ + 2], abs(d__3)) + (d__4 = dx[i__
                + 3], abs(d__4)) + (d__5 = dx[i__ + 4], abs(d__5)) + (d__6 =
                dx[i__ + 5], abs(d__6));
/*<    50 continue >*/
/* L50: */
    }
/*<    60 dasum = dtemp >*/
L60:
    ret_val = dtemp;
/*<       return >*/
    return ret_val;
/*<       end >*/
} /* dasum_ */

#ifdef __cplusplus
        }
#endif
