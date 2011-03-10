/* blas/isamax.f -- translated by f2c (version 20050501).
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

/*<       integer function isamax(n,sx,incx) >*/
integer isamax_(integer *n, real *sx, integer *incx)
{
    /* System generated locals */
    integer ret_val, i__1;
    real r__1;

    /* Local variables */
    integer i__, ix;
    real smax;


/*     finds the index of element having max. absolute value. */
/*     jack dongarra, linpack, 3/11/78. */
/*     modified 3/93 to return if incx .le. 0. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

/*<       real sx(*),smax >*/
/*<       integer i,incx,ix,n >*/

/*<       isamax = 0 >*/
    /* Parameter adjustments */
    --sx;

    /* Function Body */
    ret_val = 0;
/*<       if( n.lt.1 .or. incx.le.0 ) return >*/
    if (*n < 1 || *incx <= 0) {
        return ret_val;
    }
/*<       isamax = 1 >*/
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
/*<       smax = abs(sx(1)) >*/
    smax = dabs(sx[1]);
/*<       ix = ix + incx >*/
    ix += *incx;
/*<       do 10 i = 2,n >*/
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
/*<          if(abs(sx(ix)).le.smax) go to 5 >*/
        if ((r__1 = sx[ix], dabs(r__1)) <= smax) {
            goto L5;
        }
/*<          isamax = i >*/
        ret_val = i__;
/*<          smax = abs(sx(ix)) >*/
        smax = (r__1 = sx[ix], dabs(r__1));
/*<     5    ix = ix + incx >*/
L5:
        ix += *incx;
/*<    10 continue >*/
/* L10: */
    }
/*<       return >*/
    return ret_val;

/*        code for increment equal to 1 */

/*<    20 smax = abs(sx(1)) >*/
L20:
    smax = dabs(sx[1]);
/*<       do 30 i = 2,n >*/
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
/*<          if(abs(sx(i)).le.smax) go to 30 >*/
        if ((r__1 = sx[i__], dabs(r__1)) <= smax) {
            goto L30;
        }
/*<          isamax = i >*/
        ret_val = i__;
/*<          smax = abs(sx(i)) >*/
        smax = (r__1 = sx[i__], dabs(r__1));
/*<    30 continue >*/
L30:
        ;
    }
/*<       return >*/
    return ret_val;
/*<       end >*/
} /* isamax_ */

#ifdef __cplusplus
        }
#endif
