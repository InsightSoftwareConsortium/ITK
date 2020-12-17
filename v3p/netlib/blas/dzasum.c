/* blas/dzasum.f -- translated by f2c (version 20050501).
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

/*<       double precision function dzasum(n,zx,incx) >*/
doublereal dzasum_(integer *n, doublecomplex *zx, integer *incx)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val;

    /* Local variables */
    integer i__, ix;
    doublereal stemp;
    extern doublereal dcabs1_(doublecomplex *);


/*     takes the sum of the absolute values. */
/*     jack dongarra, 3/11/78. */
/*     modified 3/93 to return if incx .le. 0. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

/*<       double complex zx(*) >*/
/*<       double precision stemp,dcabs1 >*/
/*<       integer i,incx,ix,n >*/

/*<       dzasum = 0.0d0 >*/
    /* Parameter adjustments */
    --zx;

    /* Function Body */
    ret_val = 0.;
/*<       stemp = 0.0d0 >*/
    stemp = 0.;
/*<       if( n.le.0 .or. incx.le.0 )return >*/
    if (*n <= 0 || *incx <= 0) {
        return ret_val;
    }
/*<       if(incx.eq.1)go to 20 >*/
    if (*incx == 1) {
        goto L20;
    }

/*        code for increment not equal to 1 */

/*<       ix = 1 >*/
    ix = 1;
/*<       do 10 i = 1,n >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         stemp = stemp + dcabs1(zx(ix)) >*/
        stemp += dcabs1_(&zx[ix]);
/*<         ix = ix + incx >*/
        ix += *incx;
/*<    10 continue >*/
/* L10: */
    }
/*<       dzasum = stemp >*/
    ret_val = stemp;
/*<       return >*/
    return ret_val;

/*        code for increment equal to 1 */

/*<    20 do 30 i = 1,n >*/
L20:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<         stemp = stemp + dcabs1(zx(i)) >*/
        stemp += dcabs1_(&zx[i__]);
/*<    30 continue >*/
/* L30: */
    }
/*<       dzasum = stemp >*/
    ret_val = stemp;
/*<       return >*/
    return ret_val;
/*<       end >*/
} /* dzasum_ */

#ifdef __cplusplus
        }
#endif
