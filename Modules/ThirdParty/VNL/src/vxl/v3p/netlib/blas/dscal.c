/* blas/dscal.f -- translated by f2c (version 20050501).
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

/*<       subroutine  dscal(n,da,dx,incx) >*/
/* Subroutine */ int dscal_(integer *n, doublereal *da, doublereal *dx,
        integer *incx)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    integer i__, m, mp1, nincx;


/*     scales a vector by a constant. */
/*     uses unrolled loops for increment equal to one. */
/*     jack dongarra, linpack, 3/11/78. */
/*     modified 3/93 to return if incx .le. 0. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

/*<       double precision da,dx(*) >*/
/*<       integer i,incx,m,mp1,n,nincx >*/

/*<       if( n.le.0 .or. incx.le.0 )return >*/
    /* Parameter adjustments */
    --dx;

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
/*<         dx(i) = da*dx(i) >*/
        dx[i__] = *da * dx[i__];
/*<    10 continue >*/
/* L10: */
    }
/*<       return >*/
    return 0;

/*        code for increment equal to 1 */


/*        clean-up loop */

/*<    20 m = mod(n,5) >*/
L20:
    m = *n % 5;
/*<       if( m .eq. 0 ) go to 40 >*/
    if (m == 0) {
        goto L40;
    }
/*<       do 30 i = 1,m >*/
    i__2 = m;
    for (i__ = 1; i__ <= i__2; ++i__) {
/*<         dx(i) = da*dx(i) >*/
        dx[i__] = *da * dx[i__];
/*<    30 continue >*/
/* L30: */
    }
/*<       if( n .lt. 5 ) return >*/
    if (*n < 5) {
        return 0;
    }
/*<    40 mp1 = m + 1 >*/
L40:
    mp1 = m + 1;
/*<       do 50 i = mp1,n,5 >*/
    i__2 = *n;
    for (i__ = mp1; i__ <= i__2; i__ += 5) {
/*<         dx(i) = da*dx(i) >*/
        dx[i__] = *da * dx[i__];
/*<         dx(i + 1) = da*dx(i + 1) >*/
        dx[i__ + 1] = *da * dx[i__ + 1];
/*<         dx(i + 2) = da*dx(i + 2) >*/
        dx[i__ + 2] = *da * dx[i__ + 2];
/*<         dx(i + 3) = da*dx(i + 3) >*/
        dx[i__ + 3] = *da * dx[i__ + 3];
/*<         dx(i + 4) = da*dx(i + 4) >*/
        dx[i__ + 4] = *da * dx[i__ + 4];
/*<    50 continue >*/
/* L50: */
    }
/*<       return >*/
    return 0;
/*<       end >*/
} /* dscal_ */

#ifdef __cplusplus
        }
#endif
