/* blas/izamax.f -- translated by f2c (version 20050501).
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

/*<       integer function izamax(n,zx,incx) >*/
integer izamax_(integer *n, doublecomplex *zx, integer *incx)
{
    /* System generated locals */
    integer ret_val, i__1;

    /* Local variables */
    integer i__, ix;
    doublereal smax;
    extern doublereal dcabs1_(doublecomplex *);


/*     finds the index of element having max. absolute value. */
/*     jack dongarra, 1/15/85. */
/*     modified 3/93 to return if incx .le. 0. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

/*<       double complex zx(*) >*/
/*<       double precision smax >*/
/*<       integer i,incx,ix,n >*/
/*<       double precision dcabs1 >*/

/*<       izamax = 0 >*/
    /* Parameter adjustments */
    --zx;

    /* Function Body */
    ret_val = 0;
/*<       if( n.lt.1 .or. incx.le.0 )return >*/
    if (*n < 1 || *incx <= 0) {
        return ret_val;
    }
/*<       izamax = 1 >*/
    ret_val = 1;
/*<       if(n.eq.1)return >*/
    if (*n == 1) {
        return ret_val;
    }
/*<       if(incx.eq.1)go to 20 >*/
    if (*incx == 1) {
        goto L20;
    }

/*        code for increment not equal to 1 */

/*<       ix = 1 >*/
    ix = 1;
/*<       smax = dcabs1(zx(1)) >*/
    smax = dcabs1_(&zx[1]);
/*<       ix = ix + incx >*/
    ix += *incx;
/*<       do 10 i = 2,n >*/
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
/*<          if(dcabs1(zx(ix)).le.smax) go to 5 >*/
        if (dcabs1_(&zx[ix]) <= smax) {
            goto L5;
        }
/*<          izamax = i >*/
        ret_val = i__;
/*<          smax = dcabs1(zx(ix)) >*/
        smax = dcabs1_(&zx[ix]);
/*<     5    ix = ix + incx >*/
L5:
        ix += *incx;
/*<    10 continue >*/
/* L10: */
    }
/*<       return >*/
    return ret_val;

/*        code for increment equal to 1 */

/*<    20 smax = dcabs1(zx(1)) >*/
L20:
    smax = dcabs1_(&zx[1]);
/*<       do 30 i = 2,n >*/
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
/*<          if(dcabs1(zx(i)).le.smax) go to 30 >*/
        if (dcabs1_(&zx[i__]) <= smax) {
            goto L30;
        }
/*<          izamax = i >*/
        ret_val = i__;
/*<          smax = dcabs1(zx(i)) >*/
        smax = dcabs1_(&zx[i__]);
/*<    30 continue >*/
L30:
        ;
    }
/*<       return >*/
    return ret_val;
/*<       end >*/
} /* izamax_ */

#ifdef __cplusplus
        }
#endif
