#include "f2c.h"
#include "netlib.h"

integer idamax_(const integer *n, const doublereal *dx, const integer *incx)
{
    /* System generated locals */
    integer ret_val = 1;

    /* Local variables */
    static doublereal maxv;
    static integer i, ix;

/*     finds the index of element having max. absolute value.           */
/*     jack dongarra, linpack, 3/11/78.                                 */
/*     modified 3/93 to return if incx .le. 0.                          */
/*     modified 12/3/93, array(1) declarations changed to array(*)      */

    if (*n < 1 || *incx <= 0) {
        return 0;
    }
    if (*n == 1) {
        return 1;
    }
    maxv = abs(dx[0]);
/*        code for increment equal to 1 */
    if (*incx == 1) {
        for (i = 1; i < *n; ++i)
            if (abs(dx[i]) > maxv) {
                ret_val = i+1;
                maxv = abs(dx[i]);
            }
    }
/*        code for increment not equal to 1 */
    else {
        ix = *incx;
        for (i = 1; i < *n; ++i, ix += *incx)
            if (abs(dx[ix]) > maxv) {
                ret_val = i+1;
                maxv = abs(dx[ix]);
            }
    }
    return ret_val;
} /* idamax_ */
