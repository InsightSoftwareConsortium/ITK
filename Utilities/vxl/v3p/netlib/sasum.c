#include "f2c.h"
#include "netlib.h"

real sasum_(n, sx, incx)
const integer *n;
const real *sx;
const integer *incx;
{
    /* Local variables */
    static integer i, m, nincx;
    static real stemp;

/*     takes the sum of the absolute values.                            */
/*     uses unrolled loops for increment equal to one.                  */
/*     jack dongarra, linpack, 3/11/78.                                 */
/*     modified 3/93 to return if incx .le. 0.                          */
/*     modified 12/3/93, array(1) declarations changed to array(*)      */

    stemp = 0.f;
    if (*n <= 0 || *incx <= 0) {
        return stemp;
    }
/*        code for increment equal to 1 */
    if (*incx == 1) {
        m = *n % 6;
        for (i = 0; i < m; ++i) {
            stemp += abs(sx[i]);
        }
        for (i = m; i < *n; i += 6) {
            stemp += abs(sx[i]) + abs(sx[i+1]) + abs(sx[i+2]) + abs(sx[i+3]) + abs(sx[i+4]) + abs(sx[i+5]);
        }
    }
/*        code for increment not equal to 1 */
    else {
        nincx = *n * *incx;
        for (i = 0; i < nincx; i += *incx) {
            stemp += abs(sx[i]);
        }
    }
    return stemp;
} /* sasum_ */
